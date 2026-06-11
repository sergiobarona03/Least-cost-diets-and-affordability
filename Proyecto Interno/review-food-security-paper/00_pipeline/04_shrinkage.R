########################################################
## SCRIPT 04 — JS shrinkage + optimal threshold bounds
## Reads:  FORECAST_DIR/lambda_raw.csv
##         FORECAST_DIR/params_optimal.csv
## Writes: FORECAST_DIR/lambda_final.csv
##         ROBUST_DIR/fig_lambda_distribution.png
##         ROBUST_DIR/fig_shrinkage_effect.png
########################################################

source("00_config.R")
library(tidyverse)
library(writexl)

# -----------------------------------------------------------------------
# Load inputs
# -----------------------------------------------------------------------
message("Loading inputs...")
lambda_raw <- read_csv(file.path(FORECAST_DIR, "lambda_raw.csv"),
                       show_col_types = FALSE)

params <- read_csv(file.path(FORECAST_DIR, "params_optimal.csv"),
                   show_col_types = FALSE)

R2_MIN  <- params$R2_MIN[1]
LAM_MIN <- params$LAM_MIN[1]
LAM_MAX <- params$LAM_MAX[1]

message(sprintf("  Thresholds: R²≥%.2f | λ∈[%.2f, %.2f]",
                R2_MIN, LAM_MIN, LAM_MAX))

# -----------------------------------------------------------------------
# JS shrinkage
# -----------------------------------------------------------------------
message("Applying JS shrinkage...")

subclass_js <- lambda_raw %>%
  filter(apply_D, est_ok, !is.na(lambda)) %>%
  group_by(subclase_ipc, ciudad) %>%
  summarise(
    k_city    = n_distinct(articulo),
    sigma2_sc = mean(sigma2, na.rm = TRUE),
    Q_sc      = sum((lambda - 1)^2, na.rm = TRUE),
    .groups   = "drop")

lambda_final <- lambda_raw %>%
  left_join(subclass_js, by = c("subclase_ipc", "ciudad")) %>%
  mutate(
    delta = case_when(
      !apply_D | !est_ok  ~ NA_real_,
      k == 1              ~ 1.0,
      k >= 3 ~ pmax(0, 1 - (k_city - 2) * sigma2_sc / Q_sc),
      k == 2 ~ {
        dev <- (lambda - 1)^2
        if_else((dev + sigma2) > 0, dev / (dev + sigma2), 0)},
      TRUE ~ NA_real_),
    lambda_shrunk = case_when(
      !apply_D                 ~ 1.0,
      !est_ok | is.na(lambda)  ~ 1.0,
      TRUE                     ~ 1 + delta * (lambda - 1)),
    lambda_shrunk = if_else(
      !is.finite(lambda_shrunk), 1.0, lambda_shrunk),
    reversion_reason = case_when(
      !apply_D                   ~ "single_item",
      !est_ok | is.na(lambda)    ~ "est_failed",
      r2 < R2_MIN                ~ "low_R2",
      lambda_shrunk < LAM_MIN    ~ "low_lambda",
      lambda_shrunk > LAM_MAX    ~ "high_lambda",
      TRUE                       ~ "none"),
    lambda_final = if_else(
      reversion_reason == "none", lambda_shrunk, 1.0),
    method_label = case_when(
      !apply_D                          ~ "BASE",
      reversion_reason == "est_failed"  ~ "BASE_fallback",
      reversion_reason == "low_R2"      ~ "BASE_lowR2",
      reversion_reason == "low_lambda"  ~ "BASE_lowlambda",
      reversion_reason == "high_lambda" ~ "BASE_highlambda",
      TRUE                              ~ "D_city"))

# Diagnostics
shrink_diag <- lambda_final %>%
  filter(apply_D, est_ok, !is.na(lambda)) %>%
  summarise(
    n               = n(),
    delta_mean      = mean(delta,                  na.rm = TRUE),
    mean_abs_raw    = mean(abs(lambda - 1),        na.rm = TRUE),
    mean_abs_final  = mean(abs(lambda_final - 1),  na.rm = TRUE),
    pct_shrinkage   = (1 - mean_abs_final / mean_abs_raw) * 100)

message(sprintf(
  "  Shrinkage: |λ-1| %.3f → %.3f (%.1f%% | mean δ=%.3f)",
  shrink_diag$mean_abs_raw, shrink_diag$mean_abs_final,
  shrink_diag$pct_shrinkage, shrink_diag$delta_mean))
message(sprintf(
  "  D_city: %d | low R²: %d | low λ: %d | high λ: %d",
  sum(lambda_final$method_label == "D_city",         na.rm=TRUE),
  sum(lambda_final$reversion_reason == "low_R2",     na.rm=TRUE),
  sum(lambda_final$reversion_reason == "low_lambda",  na.rm=TRUE),
  sum(lambda_final$reversion_reason == "high_lambda", na.rm=TRUE)))

# -----------------------------------------------------------------------
# Figures
# -----------------------------------------------------------------------
lambda_dcity <- lambda_final %>%
  filter(apply_D, est_ok, method_label == "D_city") %>%
  mutate(ciudad_lbl = CITY_LABELS[ciudad])

p_dist <- ggplot(lambda_dcity,
                 aes(x = lambda_final, fill = ciudad_lbl)) +
  geom_histogram(bins = 30, alpha = 0.75,
                 position = "identity", color = "white") +
  geom_vline(xintercept = 1, linetype = "dashed",
             color = "#C0392B", linewidth = 0.8) +
  geom_vline(xintercept = c(LAM_MIN, LAM_MAX),
             linetype = "dotted", color = "grey40", linewidth = 0.6) +
  scale_fill_manual(values = CITY_COLORS) +
  labs(
    title    = "JS-shrunk lambda — D_city items (after bounds)",
    subtitle = sprintf(
      "Mean=%.3f | SD=%.3f | |λ-1| reduced %.1f%%\nR²≥%.2f | λ∈[%.2f, %.2f]",
      mean(lambda_dcity$lambda_final, na.rm=TRUE),
      sd(lambda_dcity$lambda_final,   na.rm=TRUE),
      shrink_diag$pct_shrinkage,
      R2_MIN, LAM_MIN, LAM_MAX),
    x = "Lambda (JS-shrunk)", y = "Count", fill = "City") +
  theme_bw(base_size = 11) +
  theme(legend.position = "bottom")

ggsave(file.path(ROBUST_DIR, "fig_lambda_distribution.png"),
       p_dist, width = 8, height = 5, dpi = 200)

p_shrink <- lambda_final %>%
  filter(apply_D, est_ok, !is.na(lambda)) %>%
  mutate(ciudad_lbl = CITY_LABELS[ciudad]) %>%
  ggplot(aes(x = lambda, y = lambda_final, color = ciudad_lbl)) +
  geom_point(alpha = 0.4, size = 1.5) +
  geom_abline(slope = 1, intercept = 0,
              linetype = "dashed", color = "black", linewidth = 0.6) +
  geom_hline(yintercept = c(LAM_MIN, LAM_MAX),
             linetype = "dotted", color = "grey50") +
  geom_vline(xintercept = 1, linetype = "dashed",
             color = "#C0392B", linewidth = 0.5) +
  scale_color_manual(values = CITY_COLORS) +
  labs(
    title    = "Shrinkage effect: OLS vs JS-shrunk lambda",
    subtitle = sprintf("|λ-1| reduced %.1f%%",
                       shrink_diag$pct_shrinkage),
    x = "OLS lambda (raw)", y = "JS-shrunk lambda (final)",
    color = "City") +
  theme_bw(base_size = 11) +
  theme(legend.position = "bottom")

ggsave(file.path(ROBUST_DIR, "fig_shrinkage_effect.png"),
       p_shrink, width = 7, height = 6, dpi = 200)

# -----------------------------------------------------------------------
# Save
# -----------------------------------------------------------------------
write_csv(lambda_final, file.path(FORECAST_DIR, "lambda_final.csv"))
write_xlsx(list(lambda_final = lambda_final),
           file.path(ROBUST_DIR, "lambda_estimates.xlsx"))

message("Done. Run 05_build_prices.R next.")