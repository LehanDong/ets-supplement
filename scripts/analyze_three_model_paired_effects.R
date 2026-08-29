#!/usr/bin/env Rscript

input_path <- "results/paper_revision_final/04_three_model_paired/schema_measurements_three_models_450.csv"
out_dir <- "results/paper_revision_final/04_three_model_paired"
n_boot <- 5000L

d <- read.csv(input_path, stringsAsFactors = FALSE, check.names = FALSE, fileEncoding = "UTF-8")
stopifnot(nrow(d) == 450L)
d$sufficiency_ord <- unname(c(insufficient = 0, borderline = 1, sufficient = 2)[d$evidence_adequacy])
d$sufficient_binary <- as.integer(d$evidence_adequacy == "sufficient")
d$structured_label_1 <- as.integer(d$structured_label == 1)
d$probe_label_1 <- as.integer(d$probe_label == 1)

metrics <- c(
  "critical_fact_detection_rate", "mean_chain_strength", "sufficiency_ord",
  "structured_label_1", "probe_label_1", "probe_margin_1_9"
)

qboot <- function(x, seed) {
  x <- x[is.finite(x)]
  if (!length(x)) return(c(mean = NA, low = NA, high = NA))
  set.seed(seed)
  means <- replicate(n_boot, mean(sample(x, length(x), replace = TRUE)))
  c(mean = mean(x), low = unname(quantile(means, .025)), high = unname(quantile(means, .975)))
}

paired_delta <- function(df, metric, subset_name = "all_pairs") {
  z <- df[df$condition %in% c("scattered-complete", "broken-bridge"),
          c("model", "unit_id", "condition", metric)]
  names(z)[4] <- "value"
  wide <- reshape(z, idvar = c("model", "unit_id"), timevar = "condition", direction = "wide")
  delta <- wide[["value.broken-bridge"]] - wide[["value.scattered-complete"]]
  rows <- lapply(sort(unique(wide$model)), function(m) {
    x <- delta[wide$model == m]
    x <- x[is.finite(x)]
    ci <- qboot(x, 829 + match(metric, metrics) + match(m, sort(unique(wide$model))) * 100)
    data.frame(
      model = m, metric = metric, paired_n = length(x), mean_delta = ci[["mean"]],
      ci_low = ci[["low"]], ci_high = ci[["high"]], positive_delta_n = sum(x > 0),
      negative_delta_n = sum(x < 0), subset = subset_name
    )
  })
  do.call(rbind, rows)
}

forest <- do.call(rbind, lapply(metrics, function(metric) paired_delta(d, metric)))
write.csv(forest, file.path(out_dir, "broken_vs_scattered_forest_data_three_models.csv"), row.names = FALSE)

condition_rows <- do.call(rbind, lapply(sort(unique(d$model)), function(m) {
  do.call(rbind, lapply(sort(unique(d$condition)), function(cond) {
    z <- d[d$model == m & d$condition == cond, ]
    data.frame(
      model = m, condition = cond, n = nrow(z),
      critical_fact_detection_rate = mean(z$critical_fact_detection_rate, na.rm = TRUE),
      mean_chain_strength = mean(z$mean_chain_strength, na.rm = TRUE),
      sufficiency_ord = mean(z$sufficiency_ord, na.rm = TRUE),
      structured_label_1_rate = mean(z$structured_label_1, na.rm = TRUE),
      probe_label_1_rate = mean(z$probe_label_1, na.rm = TRUE),
      probe_margin_1_9 = mean(z$probe_margin_1_9, na.rm = TRUE)
    )
  }))
}))
write.csv(condition_rows, file.path(out_dir, "condition_summary_three_models.csv"), row.names = FALSE)

cat(out_dir, "\n")
