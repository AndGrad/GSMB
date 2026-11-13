# Selfish vs. prosocial conformity across age
# --------------------------------------------------------------------

library(tidyverse)
library(brms)

# ---- 1. Load the fitted model and set the centring constant ------------
load("modelfits/follow_model_all_data_pre_reg.RData")

age_mean <- mean(all_data$age)                         

# ---- 2. Coefficient names ----------------------------------------------
fixef_names <- rownames(fixef(follow_model_all_data_pre_reg))
soc_name <- fixef_names |> str_subset("^social_info_factor") |> str_subset(":", negate = TRUE)
int_name <- fixef_names |> str_subset("social_info_factor") |> str_subset("age_scaled")
stopifnot(length(soc_name) == 1, length(int_name) == 1)

soc_levels <- levels(follow_model_all_data_pre_reg$data$social_info_factor)   # c("keep", "donate") expected
message("Levels: ", paste(soc_levels, collapse = " / "),
        " | main effect: ", soc_name, " | interaction: ", int_name)

draws <- as_draws_df(follow_model_all_data_pre_reg) |>
  as_tibble() |>
  select(b_soc = all_of(paste0("b_", soc_name)),
         b_int = all_of(paste0("b_", int_name)))

# ---- 3. Ages to evaluate -------------------------------------------------
ages_obs <- follow_model_all_data_pre_reg$data |>
  distinct(IDself, age_scaled) |>
  pull(age_scaled)

eval_ages <- bind_rows(
  tibble(label = "Youngest",        age_c = min(ages_obs)),
  tibble(label = "5th percentile",  age_c = quantile(ages_obs, 0.05, names = FALSE)),
  tibble(label = "95th percentile", age_c = quantile(ages_obs, 0.95, names = FALSE)),
  tibble(label = "Oldest",          age_c = max(ages_obs)),
  tibble(age_c = seq(ceiling(min(ages_obs) + age_mean), floor(max(ages_obs) + age_mean)) - age_mean) |>
    mutate(label = paste0("Age ", round(age_c + age_mean)))
) |>
  mutate(age = age_c + age_mean,
         n_participants = map_int(age_c, \(a) sum(round(ages_obs + age_mean) == round(a + age_mean))))

# ---- 4. Contrast on the log-odds scale ----------------------------------
summ <- function(x) tibble(
  est = median(x), lower = quantile(x, 0.025), upper = quantile(x, 0.975),
  pd = max(mean(x > 0), mean(x < 0))
)

contrast_logodds <- eval_ages |>
  mutate(res = map(age_c, \(a) summ(draws$b_soc + draws$b_int * a))) |>
  unnest(res)

# ---- 5. Contrast on the probability scale -------------------------------
# Population-level predictions (random effects set to zero), in-degree at 0.
newdata <- eval_ages |>
  select(label, age_c) |>
  crossing(social_info_factor = factor(soc_levels, levels = soc_levels)) |>
  mutate(age_scaled = age_c, stdinDegreepeer = 0)

epred <- posterior_epred(follow_model_all_data_pre_reg, newdata = newdata, re_formula = NA)  # draws x rows

prob_tbl <- newdata |>
  mutate(draw_col = row_number()) |>
  select(label, social_info_factor, draw_col) |>
  pivot_wider(names_from = social_info_factor, values_from = draw_col) |>
  mutate(
    p_selfish   = map(.data[[soc_levels[1]]], \(j) epred[, j]),
    p_prosocial = map(.data[[soc_levels[2]]], \(j) epred[, j]),
    selfish     = map_dbl(p_selfish, median),
    prosocial   = map_dbl(p_prosocial, median),
    diff        = map2(p_prosocial, p_selfish, \(p, s) summ(100 * (p - s)))
  ) |>
  select(label, selfish, prosocial, diff) |>
  unnest(diff, names_sep = "_pp_")

results <- contrast_logodds |>
  left_join(prob_tbl, by = "label") |>
  select(label, age, n_participants,
         contrast_logodds = est, lower, upper, pd,
         p_selfish = selfish, p_prosocial = prosocial,
         diff_pp = diff_pp_est, diff_pp_lower = diff_pp_lower, diff_pp_upper = diff_pp_upper)

print(results)

# ---- 6. Crossover age (where the two conformity rates are equal) ---------
# Defined only when the interaction is non-zero; with pd = 1.00 for the
# interaction this is not an issue, but extreme draws are trimmed by the quantiles.
crossover <- draws |>
  mutate(age_cross = -b_soc / b_int + age_mean) |>
  summarise(est = median(age_cross),
            lower = quantile(age_cross, 0.025),
            upper = quantile(age_cross, 0.975))
print(crossover)


# ---- 8. Save ------------------------------------------------------------
write_csv(results,   "tables/selfish_prosocial_contrast_by_age.csv")
write_csv(crossover, "tables/selfish_prosocial_crossover_age.csv")
