suppressPackageStartupMessages({
  library(stats)
})

args <- commandArgs(trailingOnly = TRUE)
input_path <- if (length(args) >= 1) args[[1]] else
  "results/paper_revision_final/01_frozen_ets/ets_observations_frozen_450.csv"
out_dir <- if (length(args) >= 2) args[[2]] else
  "results/paper_revision_final/07_ets_model_robustness"
n_boot <- if (length(args) >= 3) as.integer(args[[3]]) else 5000L
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

d <- read.csv(input_path, stringsAsFactors = FALSE, check.names = FALSE)
d <- d[d$status == "ok", c("model", "unit_id", "level", "level_num", "probe_support")]
names(d)[names(d) == "probe_support"] <- "support"
d$level_num <- as.numeric(d$level_num)
d$support <- as.integer(d$support)

stopifnot(nrow(d) == 450L)
stopifnot(length(unique(d$unit_id)) == 30L)
stopifnot(all(table(d$model, d$level) == 30L))

eps <- 1e-6
clip_prob <- function(p) pmin(pmax(p, eps), 1 - eps)
log_loss <- function(y, p) -mean(y * log(clip_prob(p)) + (1-y) * log(clip_prob(1-p)))
brier <- function(y, p) mean((y-p)^2)

level_summary <- function(z) {
  out <- aggregate(support ~ level_num, z, function(x) c(n=length(x), y=sum(x)))
  data.frame(level_num=out$level_num, n=out$support[,"n"], y=out$support[,"y"])
}

pava <- function(y, w) {
  values <- as.numeric(y)
  weights <- as.numeric(w)
  starts <- seq_along(values)
  ends <- seq_along(values)
  i <- 1L
  while (i < length(values)) {
    if (values[[i]] <= values[[i+1]] + 1e-12) {
      i <- i + 1L
    } else {
      new_w <- weights[[i]] + weights[[i+1]]
      values[[i]] <- (values[[i]] * weights[[i]] + values[[i+1]] * weights[[i+1]]) / new_w
      weights[[i]] <- new_w
      ends[[i]] <- ends[[i+1]]
      values <- values[-(i+1)]
      weights <- weights[-(i+1)]
      starts <- starts[-(i+1)]
      ends <- ends[-(i+1)]
      if (i > 1L) i <- i - 1L
    }
  }
  fitted <- numeric(sum(ends - starts + 1L))
  for (j in seq_along(values)) fitted[starts[[j]]:ends[[j]]] <- values[[j]]
  list(fitted=fitted, blocks=length(values))
}

fit_2pl <- function(z) {
  fit <- glm(support ~ level_num, data=z, family=binomial())
  list(
    name="logistic_2pl", npar=2L,
    predict=function(newdata) clip_prob(predict(fit, newdata=newdata, type="response")),
    extra=list(tau=-coef(fit)[[1]]/coef(fit)[[2]], k=coef(fit)[[2]])
  )
}

fit_change_point <- function(z) {
  candidates <- lapply(1:4, function(cut) {
    high <- as.integer(z$level_num >= cut)
    low_y <- sum(z$support[high == 0]); low_n <- sum(high == 0)
    high_y <- sum(z$support[high == 1]); high_n <- sum(high == 1)
    p_low <- (low_y + .5) / (low_n + 1)
    p_high <- (high_y + .5) / (high_n + 1)
    pred <- ifelse(high == 1, p_high, p_low)
    list(cut=cut, p_low=p_low, p_high=p_high,
         nll=log_loss(z$support, pred) * nrow(z))
  })
  best <- candidates[[which.min(vapply(candidates, function(x) x$nll, numeric(1)))]]
  list(
    name="change_point", npar=3L,
    predict=function(newdata) ifelse(newdata$level_num >= best$cut, best$p_high, best$p_low),
    extra=list(cut=best$cut, p_low=best$p_low, p_high=best$p_high)
  )
}

fit_categorical <- function(z) {
  s <- level_summary(z)
  probs <- (s$y + .5) / (s$n + 1)
  names(probs) <- as.character(s$level_num)
  list(
    name="categorical", npar=5L,
    predict=function(newdata) as.numeric(probs[as.character(newdata$level_num)]),
    extra=list()
  )
}

fit_isotonic <- function(z) {
  s <- level_summary(z)
  raw <- (s$y + .5) / (s$n + 1)
  iso <- pava(raw, s$n)
  probs <- iso$fitted
  names(probs) <- as.character(s$level_num)
  list(
    name="isotonic", npar=iso$blocks,
    predict=function(newdata) as.numeric(probs[as.character(newdata$level_num)]),
    extra=list(blocks=iso$blocks)
  )
}

fit_4pl <- function(z) {
  objective <- function(par) {
    lower <- plogis(par[[1]])
    upper <- lower + (1-lower) * plogis(par[[2]])
    tau <- -1 + 6 * plogis(par[[3]])
    k <- exp(par[[4]])
    p <- lower + (upper-lower) * plogis(k * (z$level_num-tau))
    log_loss(z$support, p) * nrow(z)
  }
  starts <- list(
    c(qlogis(.05), qlogis(.90), qlogis((2+1)/6), log(1)),
    c(qlogis(.20), qlogis(.90), qlogis((2.5+1)/6), log(2)),
    c(qlogis(.30), qlogis(.95), qlogis((3+1)/6), log(4))
  )
  fits <- lapply(starts, function(start) optim(start, objective, method="BFGS",
                                               control=list(maxit=5000)))
  opt <- fits[[which.min(vapply(fits, function(x) x$value, numeric(1)))]]
  par <- opt$par
  lower <- plogis(par[[1]])
  upper <- lower + (1-lower) * plogis(par[[2]])
  tau <- -1 + 6 * plogis(par[[3]])
  k <- exp(par[[4]])
  list(
    name="logistic_4pl", npar=4L,
    predict=function(newdata) clip_prob(lower + (upper-lower) *
      plogis(k * (newdata$level_num-tau))),
    extra=list(lower=lower, upper=upper, tau=tau, k=k, convergence=opt$convergence)
  )
}

fitters <- list(
  logistic_2pl=fit_2pl,
  logistic_4pl=fit_4pl,
  change_point=fit_change_point,
  isotonic=fit_isotonic,
  categorical=fit_categorical
)

model_ids <- unique(d$model)
fit_rows <- list()
prediction_rows <- list()
for (m in model_ids) {
  z <- d[d$model == m, ]
  observed <- aggregate(support ~ level_num, z, mean)
  for (fit_name in names(fitters)) {
    fit <- fitters[[fit_name]](z)
    p_obs <- fit$predict(z)
    nll <- log_loss(z$support, p_obs) * nrow(z)
    level_grid <- data.frame(level_num=0:4)
    level_pred <- fit$predict(level_grid)
    merged <- merge(observed, data.frame(level_num=0:4, predicted=level_pred), by="level_num")
    fit_rows[[length(fit_rows)+1]] <- data.frame(
      model=m, fit=fit_name, n=nrow(z), npar=fit$npar,
      log_loss=log_loss(z$support, p_obs), brier=brier(z$support, p_obs),
      level_rmse=sqrt(mean((merged$support-merged$predicted)^2)),
      level_mae=mean(abs(merged$support-merged$predicted)),
      max_level_error=max(abs(merged$support-merged$predicted)),
      AIC=2*nll + 2*fit$npar,
      tau=ifelse(is.null(fit$extra$tau), NA, fit$extra$tau),
      k=ifelse(is.null(fit$extra$k), NA, fit$extra$k),
      change_point=ifelse(is.null(fit$extra$cut), NA, fit$extra$cut),
      lower_asymptote=ifelse(is.null(fit$extra$lower), NA, fit$extra$lower),
      upper_asymptote=ifelse(is.null(fit$extra$upper), NA, fit$extra$upper)
    )
    prediction_rows[[length(prediction_rows)+1]] <- data.frame(
      model=m, fit=fit_name, level_num=0:4,
      observed_rate=merged$support, predicted_rate=merged$predicted,
      residual=merged$support-merged$predicted
    )
  }
}
fit_table <- do.call(rbind, fit_rows)
prediction_table <- do.call(rbind, prediction_rows)
write.csv(fit_table, file.path(out_dir, "in_sample_fit_comparison.csv"), row.names=FALSE)
write.csv(prediction_table, file.path(out_dir, "level_calibration_predictions.csv"), row.names=FALSE)

cv_rows <- list()
for (m in model_ids) {
  z <- d[d$model == m, ]
  for (unit in unique(z$unit_id)) {
    train <- z[z$unit_id != unit, ]
    test <- z[z$unit_id == unit, ]
    for (fit_name in names(fitters)) {
      fit <- fitters[[fit_name]](train)
      pred <- fit$predict(test)
      cv_rows[[length(cv_rows)+1]] <- data.frame(
        model=m, unit_id=unit, fit=fit_name,
        log_loss=log_loss(test$support, pred),
        brier=brier(test$support, pred),
        selected_change_point=ifelse(is.null(fit$extra$cut), NA, fit$extra$cut)
      )
    }
  }
}
cv <- do.call(rbind, cv_rows)
write.csv(cv, file.path(out_dir, "loco_case_item_scores.csv"), row.names=FALSE)

cv_summary <- aggregate(cbind(log_loss, brier) ~ model + fit, cv, mean)
write.csv(cv_summary, file.path(out_dir, "loco_cv_summary.csv"), row.names=FALSE)

qci <- function(x) quantile(x, c(.025,.975), names=FALSE, na.rm=TRUE)
set.seed(829)
paired_rows <- list()
for (m in model_ids) {
  base <- cv[cv$model == m & cv$fit == "logistic_2pl", ]
  for (competitor in setdiff(names(fitters), "logistic_2pl")) {
    alt <- cv[cv$model == m & cv$fit == competitor, ]
    pair <- merge(base, alt, by=c("model","unit_id"), suffixes=c("_2pl","_alt"))
    for (metric in c("log_loss","brier")) {
      delta <- pair[[paste0(metric,"_2pl")]] - pair[[paste0(metric,"_alt")]]
      boot_mean <- replicate(n_boot, mean(sample(delta, length(delta), replace=TRUE)))
      ci <- qci(boot_mean)
      paired_rows[[length(paired_rows)+1]] <- data.frame(
        model=m, competitor=competitor, metric=metric, paired_n=length(delta),
        delta_2pl_minus_competitor=mean(delta), ci_low=ci[[1]], ci_high=ci[[2]]
      )
    }
  }
}
paired_table <- do.call(rbind, paired_rows)
write.csv(paired_table, file.path(out_dir, "loco_paired_model_differences.csv"), row.names=FALSE)

change_counts <- aggregate(unit_id ~ model + selected_change_point,
                           cv[cv$fit == "change_point", ], length)
names(change_counts)[names(change_counts) == "unit_id"] <- "fold_n"
write.csv(change_counts, file.path(out_dir, "change_point_selection_by_fold.csv"), row.names=FALSE)

writeLines(c(
  "# ETS response-model robustness",
  "",
  paste0("- Frozen observations: ", nrow(d)),
  paste0("- Case-items: ", length(unique(d$unit_id))),
  paste0("- Case-level bootstrap iterations for paired CV differences: ", n_boot),
  "- CV unit: one complete case-item (all five B0--B4 levels).",
  "- Positive paired differences mean that 2PL has worse out-of-sample loss than the competitor.",
  "- Change-point selection is nested within each training fold.",
  "- Categorical and isotonic probabilities use Jeffreys smoothing to avoid zero-probability predictions."
), file.path(out_dir, "README.md"))

cat("analysis complete\n")
print(fit_table)
print(cv_summary)
print(paired_table)
print(change_counts)
