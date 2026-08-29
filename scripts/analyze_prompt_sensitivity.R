suppressPackageStartupMessages({
  library(lme4)
})

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1) stop("Usage: Rscript tools/analyze_prompt_sensitivity.R <P1/P2 summary.csv> [output_dir] [bootstrap_n]")

root <- normalizePath(".")
variant_path <- normalizePath(args[[1]])
out_dir <- if (length(args) >= 2) args[[2]] else file.path(dirname(variant_path), "analysis")
n_boot <- if (length(args) >= 3) as.integer(args[[3]]) else 5000L
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

p0_path <- file.path(root, "results/paper_revision_final/01_frozen_ets/ets_observations_frozen_450.csv")
p0 <- read.csv(p0_path, stringsAsFactors = FALSE, check.names = FALSE)
p0 <- data.frame(
  model = p0$model, prompt_variant = "P0", unit_id = p0$unit_id,
  level = p0$level, level_num = p0$level_num, label = p0$probe_label,
  support = p0$probe_support, status = p0$probe_status, stringsAsFactors = FALSE
)

v <- read.csv(variant_path, stringsAsFactors = FALSE, check.names = FALSE)
v <- v[v$status == "ok", c("model", "prompt_variant", "unit_id", "level", "level_num", "label", "support", "status")]
d <- rbind(p0, v)
d$model <- factor(d$model, levels = c("deepseek", "qwen_llamacpp", "seed"))
d$prompt_variant <- factor(d$prompt_variant, levels = c("P0", "P1", "P2"))
d$level <- factor(d$level, levels = paste0("B", 0:4))
d$level_num <- as.numeric(d$level_num)
d$support <- as.integer(d$support)

expected <- 30L * 5L * 3L * 3L
if (nrow(d) != expected) stop(sprintf("Expected %d rows after merging P0/P1/P2; found %d", expected, nrow(d)))

rates <- aggregate(support ~ model + prompt_variant + level + level_num, d, function(x) c(n=length(x), support=sum(x), rate=mean(x)))
rates <- data.frame(rates[1:4], rates$support, row.names = NULL, check.names = FALSE)
write.csv(rates, file.path(out_dir, "prompt_level_support_rates.csv"), row.names = FALSE)

fit_signature <- function(z) {
  fit <- glm(support ~ level_num, data = z, family = binomial())
  b <- coef(fit)
  c(tau = -unname(b[[1]]) / unname(b[[2]]), k = unname(b[[2]]))
}
signatures <- do.call(rbind, lapply(levels(d$model), function(m) {
  do.call(rbind, lapply(levels(d$prompt_variant), function(p) {
    s <- fit_signature(d[d$model == m & d$prompt_variant == p, ])
    data.frame(model=m, prompt_variant=p, tau=s[["tau"]], k=s[["k"]])
  }))
}))
write.csv(signatures, file.path(out_dir, "prompt_ets_point_estimates.csv"), row.names = FALSE)

set.seed(829)
units <- unique(as.character(d$unit_id))
boot_rows <- vector("list", n_boot)
for (b in seq_len(n_boot)) {
  sampled <- sample(units, length(units), replace = TRUE)
  db <- do.call(rbind, lapply(seq_along(sampled), function(i) d[d$unit_id == sampled[[i]], ]))
  values <- list()
  failed <- FALSE
  for (m in levels(d$model)) {
    for (p in levels(d$prompt_variant)) {
      s <- tryCatch(fit_signature(db[db$model == m & db$prompt_variant == p, ]),
                    error = function(e) c(tau=NA_real_, k=NA_real_))
      if (any(!is.finite(s))) failed <- TRUE
      values[[paste(m, p, "tau", sep="|")]] <- s[["tau"]]
      values[[paste(m, p, "k", sep="|")]] <- s[["k"]]
    }
  }
  boot_rows[[b]] <- c(iteration=b, failed=failed, unlist(values))
}
boot <- as.data.frame(do.call(rbind, boot_rows), check.names=FALSE)
write.csv(boot, file.path(out_dir, "prompt_ets_case_bootstrap.csv"), row.names=FALSE)

qci <- function(x) quantile(x[is.finite(x)], c(.025, .975), na.rm=TRUE, names=FALSE)
ci_rows <- list()
for (m in levels(d$model)) {
  for (p in levels(d$prompt_variant)) {
    point <- signatures[signatures$model == m & signatures$prompt_variant == p, ]
    tau_col <- paste(m, p, "tau", sep="|")
    k_col <- paste(m, p, "k", sep="|")
    tau_ci <- qci(boot[[tau_col]])
    k_ci <- qci(boot[[k_col]])
    ci_rows[[length(ci_rows)+1]] <- data.frame(
      model=m, prompt_variant=p, tau=point$tau, tau_ci_low=tau_ci[[1]], tau_ci_high=tau_ci[[2]],
      k=point$k, k_ci_low=k_ci[[1]], k_ci_high=k_ci[[2]], bootstrap_n=n_boot
    )
  }
}
ci_table <- do.call(rbind, ci_rows)
write.csv(ci_table, file.path(out_dir, "prompt_ets_estimates_with_ci.csv"), row.names=FALSE)

delta_rows <- list()
for (m in levels(d$model)) {
  for (p in c("P1", "P2")) {
    point_p <- signatures[signatures$model == m & signatures$prompt_variant == p, ]
    point_0 <- signatures[signatures$model == m & signatures$prompt_variant == "P0", ]
    dtau <- boot[[paste(m,p,"tau",sep="|")]] - boot[[paste(m,"P0","tau",sep="|")]]
    dk <- boot[[paste(m,p,"k",sep="|")]] - boot[[paste(m,"P0","k",sep="|")]]
    dtau_ci <- qci(dtau); dk_ci <- qci(dk)
    delta_rows[[length(delta_rows)+1]] <- data.frame(
      model=m, contrast=paste0(p,"-P0"), delta_tau=point_p$tau-point_0$tau,
      delta_tau_ci_low=dtau_ci[[1]], delta_tau_ci_high=dtau_ci[[2]],
      delta_k=point_p$k-point_0$k, delta_k_ci_low=dk_ci[[1]], delta_k_ci_high=dk_ci[[2]]
    )
  }
}
write.csv(do.call(rbind, delta_rows), file.path(out_dir, "prompt_ets_differences_from_p0.csv"), row.names=FALSE)

between_model_rows <- list()
model_pairs <- list(
  c("seed", "deepseek"),
  c("seed", "qwen_llamacpp"),
  c("deepseek", "qwen_llamacpp")
)
for (p in levels(d$prompt_variant)) {
  for (pair in model_pairs) {
    m1 <- pair[[1]]
    m2 <- pair[[2]]
    point_1 <- signatures[signatures$model == m1 & signatures$prompt_variant == p, ]
    point_2 <- signatures[signatures$model == m2 & signatures$prompt_variant == p, ]
    dtau <- boot[[paste(m1,p,"tau",sep="|")]] - boot[[paste(m2,p,"tau",sep="|")]]
    dk <- boot[[paste(m1,p,"k",sep="|")]] - boot[[paste(m2,p,"k",sep="|")]]
    dtau_ci <- qci(dtau)
    dk_ci <- qci(dk)
    between_model_rows[[length(between_model_rows)+1]] <- data.frame(
      prompt_variant=p, comparison=paste0(m1,"-",m2),
      delta_tau=point_1$tau-point_2$tau,
      delta_tau_ci_low=dtau_ci[[1]], delta_tau_ci_high=dtau_ci[[2]],
      delta_k=point_1$k-point_2$k,
      delta_k_ci_low=dk_ci[[1]], delta_k_ci_high=dk_ci[[2]]
    )
  }
}
write.csv(do.call(rbind, between_model_rows),
          file.path(out_dir, "prompt_between_model_ets_differences.csv"), row.names=FALSE)

unit_effects <- merge(
  d[d$level == "B1", c("model", "prompt_variant", "unit_id", "support")],
  d[d$level == "B4", c("model", "prompt_variant", "unit_id", "support")],
  by=c("model", "prompt_variant", "unit_id"), suffixes=c("_B1", "_B4")
)
unit_effects$delta_B4_B1 <- unit_effects$support_B4 - unit_effects$support_B1
b4_b1_rows <- list()
for (m in levels(d$model)) {
  for (p in levels(d$prompt_variant)) {
    z <- unit_effects[unit_effects$model == m & unit_effects$prompt_variant == p, ]
    set.seed(829 + nrow(z))
    boot_delta <- replicate(n_boot, mean(sample(z$delta_B4_B1, nrow(z), replace=TRUE)))
    delta_ci <- qci(boot_delta)
    b4_b1_rows[[length(b4_b1_rows)+1]] <- data.frame(
      model=m, prompt_variant=p, paired_n=nrow(z),
      mean_delta=mean(z$delta_B4_B1),
      ci_low=delta_ci[[1]], ci_high=delta_ci[[2]],
      positive_delta_n=sum(z$delta_B4_B1 > 0),
      negative_delta_n=sum(z$delta_B4_B1 < 0)
    )
  }
}
write.csv(do.call(rbind, b4_b1_rows),
          file.path(out_dir, "prompt_b4_b1_paired_effects.csv"), row.names=FALSE)

p0_labels <- d[d$prompt_variant == "P0", c("model", "unit_id", "level", "label")]
names(p0_labels)[4] <- "p0_label"
agreement <- merge(d[d$prompt_variant != "P0", ], p0_labels, by=c("model", "unit_id", "level"))
agreement$match <- as.character(agreement$label) == as.character(agreement$p0_label)
agreement_summary <- aggregate(match ~ model + prompt_variant, agreement, function(x) c(n=length(x), matches=sum(x), rate=mean(x)))
agreement_summary <- data.frame(agreement_summary[1:2], agreement_summary$match,
                                row.names = NULL, check.names = FALSE)
write.csv(agreement_summary, file.path(out_dir, "prompt_label_agreement_with_p0.csv"), row.names = FALSE)

full_fit <- glmer(support ~ model * level * prompt_variant + (1 | unit_id), data=d, family=binomial(),
                  control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
reduced_fit <- glmer(support ~ model * level + prompt_variant + (1 | unit_id), data=d, family=binomial(),
                     control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
comparison <- anova(reduced_fit, full_fit, test="Chisq")
capture.output(summary(full_fit), file=file.path(out_dir, "prompt_sensitivity_mixed_model.txt"))
capture.output(comparison, file=file.path(out_dir, "prompt_interaction_model_comparison.txt"))

d$phase <- factor(ifelse(d$level_num <= 2, "low", "high"), levels=c("low", "high"))
phase_rates <- aggregate(support ~ model + prompt_variant + phase, d,
                         function(x) c(n=length(x), support=sum(x), rate=mean(x)))
phase_rates <- data.frame(phase_rates[1:3], phase_rates$support,
                          row.names=NULL, check.names=FALSE)
write.csv(phase_rates, file.path(out_dir, "prompt_phase_support_rates.csv"), row.names=FALSE)

phase_reduced_fit <- glmer(
  support ~ (model + phase + prompt_variant)^2 + (1 | unit_id),
  data=d, family=binomial(),
  control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=5e5))
)
phase_full_fit <- glmer(
  support ~ model * phase * prompt_variant + (1 | unit_id),
  data=d, family=binomial(),
  control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=5e5))
)
phase_comparison <- anova(phase_reduced_fit, phase_full_fit, test="Chisq")
capture.output(summary(phase_full_fit),
               file=file.path(out_dir, "prompt_phase_mixed_model.txt"))
capture.output(phase_comparison,
               file=file.path(out_dir, "prompt_phase_interaction_model_comparison.txt"))

cat("analysis complete\n")
print(ci_table)
print(agreement_summary)
print(comparison)
print(phase_comparison)
