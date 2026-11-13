which <- commandArgs(trailingOnly = TRUE)[1]
out <- "tables"

labels <- c(b_Intercept = "Intercept",
            b_social_info_factor1 = "Social information [Donate]",
            b_age_scaled = "Age",
            b_stdinDegreepeer = "Peer in-degree",
            b_peerisfriend1 = "Peer is friend",
            `b_social_info_factor1:age_scaled` = "Social information [Donate] x Age",
            `b_age_scaled:stdinDegreepeer` = "Age x Peer in-degree",
            `b_stdinDegreepeer:peerisfriend1` = "Peer in-degree x Peer is friend")

summarise_fit <- function(fit, table_name) {
  dr <- as_draws_df(fit)
  pars <- intersect(names(labels), variables(dr))
  res <- bind_rows(lapply(pars, function(p) {
    x <- dr[[p]]
    tibble(Predictor = labels[[p]], parameter = p,
           median = median(x), sd = sd(x),
           q2.5 = unname(quantile(x, .025)), q97.5 = unname(quantile(x, .975)),
           pd = max(mean(x > 0), mean(x < 0)),
           Estimate = sprintf("%.2f", median(x)), SE = sprintf("%.2f", sd(x)),
           `95% CI` = sprintf("[%.2f, %.2f]", quantile(x, .025), quantile(x, .975)),
           pd_fmt = sprintf("%.3f", max(mean(x > 0), mean(x < 0))))
  }))

  ord <- c("Intercept", "Social information [Donate]", "Age", "Peer in-degree", "Peer is friend",
           "Social information [Donate] x Age", "Age x Peer in-degree", "Peer in-degree x Peer is friend")
  res <- res[order(match(res$Predictor, ord)), ]
  write.csv(res, file.path(out, paste0(table_name, "_full_precision.csv")), row.names = FALSE, fileEncoding = "UTF-8")
  fmt <- res %>% select(Predictor, Estimate, SE, `95% CI`, pd = pd_fmt)
  write.csv(fmt, file.path(out, paste0(table_name, "_formatted.csv")), row.names = FALSE, fileEncoding = "UTF-8")
  cat("\n==", table_name, "nobs =", nobs(fit), "\n"); print(as.data.frame(res[, c("Predictor","median","sd","q2.5","q97.5","pd")]), digits = 4)
  invisible(res)
}

if (which == "S2S3") {
  base::load("modelfits/follow_model_all_data_pre_reg.RData")
  summarise_fit(follow_model_all_data_pre_reg, "TableS2_prereg")
  dr <- as_draws_df(follow_model_all_data_pre_reg)
  slope_donate <- dr$b_age_scaled + dr$`b_social_info_factor1:age_scaled`
  slope_avg <- dr$b_age_scaled + 0.5 * dr$`b_social_info_factor1:age_scaled`
  f <- function(x, nm) cat(sprintf("%s: median %.4f, 95%% ETI [%.4f, %.4f], pd %.4f\n", nm, median(x), quantile(x,.025), quantile(x,.975), max(mean(x>0), mean(x<0))))
  f(dr$b_age_scaled, "age slope, keep info (b_age)")
  f(slope_donate, "age slope, donate info (b_age + b_int)")
  f(slope_avg, "average age slope (b_age + 0.5 b_int)")
  print(hypothesis(follow_model_all_data_pre_reg, "age_scaled + 0.5 * social_info_factor1:age_scaled = 0"))
  rm(follow_model_all_data_pre_reg, dr); gc()
  base::load("modelfits/follow_model_no2019_pre_reg.RData")
  summarise_fit(follow_model_no2019, "TableS3_study2_only")
} else if (which == "S4") {
  base::load("modelfits/follow_model_all_data_pre_reg_robustness_check.RData")
  summarise_fit(follow_model_all_data_pre_reg_robustness_check, "TableS4_friendship")
}

if (which == "S2S3_8pp") {
  base::load("modelfits/follow_model_all_data_pre_reg.RData")
  fit <- follow_model_all_data_pre_reg; md <- fit$data
  lo <- min(md$stdinDegreepeer); hi <- max(md$stdinDegreepeer); a <- mean(md$age_scaled)
  g <- expand.grid(stdinDegreepeer = c(lo, hi), social_info_factor = factor(c("0","1"), levels = c("0","1")), age_scaled = a)
  ep <- posterior_epred(fit, newdata = g, re_formula = NA)
  avg_lo <- rowMeans(ep[, g$stdinDegreepeer == lo]); avg_hi <- rowMeans(ep[, g$stdinDegreepeer == hi])
  keep_lo <- ep[, g$stdinDegreepeer == lo & g$social_info_factor == "0"]; keep_hi <- ep[, g$stdinDegreepeer == hi & g$social_info_factor == "0"]
  cat(sprintf("in-degree range in model data: [%.3f, %.3f]; mean age_scaled %.4f (age %.2f)\n", lo, hi, a, a + 14))
  cat(sprintf("Figure2_avg.R panel D (averaged over info): p at min %.4f, at max %.4f, difference of medians %.4f; median of difference %.4f [%.4f, %.4f]\n",
      median(avg_lo), median(avg_hi), median(avg_hi) - median(avg_lo), median(avg_hi - avg_lo), quantile(avg_hi - avg_lo, .025), quantile(avg_hi - avg_lo, .975)))
  cat(sprintf("conditional_effects default (keep rounds only, as in superseded figure2.R): p at min %.4f, at max %.4f, difference %.4f\n",
      median(keep_lo), median(keep_hi), median(keep_hi - keep_lo)))
}
