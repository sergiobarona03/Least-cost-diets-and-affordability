########################################################
## SCRIPT 03 — Grid search threshold optimization
## Grid: R2_MIN × LAM_MIN × LAM_MAX (36 combinations)
## Criterion: MAPE on multi-item sub-classes
## Output: FORECAST_DIR/threshold_grid_results.csv
##         FORECAST_DIR/params_optimal.csv
##         ROBUST_DIR/fig_threshold_heatmap.png
##         ROBUST_DIR/fig_threshold_improvement.png
##
## CHANGE 1 (2026-10-20): the shrinkage branch is selected with k_city
## (items in the sub-class × city with a successful estimate) instead of
## k (items assigned to the sub-class). The James-Stein factor already
## used k_city, so condition and formula now count the same thing. The
## dead `k == 1` branch was removed: apply_D == FALSE already captures
## single-item sub-classes in the first condition.
##
## CHANGE 2 (2026-10-20): a series that is the ONLY successful estimate
## in its sub-class and city (k_city == 1) now receives INDIVIDUAL
## shrinkage instead of reverting to lambda = 1. The two-item formula,
## delta = dev / (dev + sigma2), uses only the item's own deviation and
## residual variance, so it needs no group; the earlier reversion was
## over-conservative. Tested in 06d_reversion_rules.R: for the three
## affected series the validation MAPE falls from 8.34% to 4.75% in the
## evaluation window and from 2.54% to 1.59% in the selection window,
## and two of the four series that determine CoCA are among them
## (HARINA DE TRIGO in Cali, 10.97% -> 3.60%; HARINA PRECOCIDA in
## Bogota, 11.59% -> 8.20%).
##
## The R2 reversion criterion was also tested in 06d and is KEPT: the
## alternatives improve the evaluation window but worsen the selection
## window, and they gain in low-lambda series while losing in mid-lambda
## ones, so the evidence does not support changing it.
########################################################

source("00_config.R")
library(tidyverse)
library(writexl)

# -----------------------------------------------------------------------
# Load inputs
# -----------------------------------------------------------------------
message("Loading inputs...")
dane          <- readRDS(file.path(CACHE_DIR, "dane.rds"))
ipc           <- readRDS(file.path(CACHE_DIR, "ipc.rds"))
anchor_prices <- readRDS(file.path(CACHE_DIR, "anchor_prices.rds"))

lambda_raw <- read_csv(file.path(FORECAST_DIR, "lambda_raw.csv"),
                       show_col_types = FALSE)

obs_val <- dane %>%
  filter(fecha >= VAL_START, fecha <= VAL_END, !is.na(precio_500g)) %>%
  select(ciudad = nombre_ciudad, articulo, fecha,
         price_obs = precio_500g)

ipc_t0_lookup <- ipc %>%
  filter(fecha == T0) %>%
  select(ciudad, cod_subclase, ipc_t0 = ipc)

ipc_val <- ipc %>%
  filter(fecha >= VAL_START, fecha <= VAL_END) %>%
  select(ciudad, cod_subclase, fecha, ipc)

# -----------------------------------------------------------------------
# Grid definition
# -----------------------------------------------------------------------
grid <- expand.grid(
  R2_MIN  = c(0.05, 0.10, 0.15, 0.20),
  LAM_MIN = c(0.10, 0.20, 0.30),
  LAM_MAX = c(3.00, 4.00, 5.00),
  stringsAsFactors = FALSE
) %>% filter(LAM_MIN < LAM_MAX)

message(sprintf("Grid: %d combinations", nrow(grid)))

# -----------------------------------------------------------------------
# Pre-compute shrinkage (threshold-independent)
# -----------------------------------------------------------------------
message("Pre-computing shrinkage...")
subclass_js <- lambda_raw %>%
  filter(apply_D, est_ok, !is.na(lambda)) %>%
  group_by(subclase_ipc, ciudad) %>%
  summarise(
    k_city    = n_distinct(articulo),
    sigma2_sc = mean(sigma2, na.rm = TRUE),
    Q_sc      = sum((lambda - 1)^2, na.rm = TRUE),
    .groups   = "drop")

lambda_shrunk_base <- lambda_raw %>%
  left_join(subclass_js, by = c("subclase_ipc", "ciudad")) %>%
  mutate(
    delta = case_when(
      !apply_D | !est_ok  ~ NA_real_,
      k_city >= 3 ~ pmax(0, 1 - (k_city - 2) * sigma2_sc / Q_sc),
      # individual shrinkage: needs only the item's own dev and sigma2,
      # so it also covers k_city == 1 (no group required)
      k_city <= 2 ~ {
        dev <- (lambda - 1)^2
        if_else((dev + sigma2) > 0, dev / (dev + sigma2), 0)},
      TRUE ~ NA_real_),
    lambda_shrunk = case_when(
      !apply_D                 ~ 1.0,
      !est_ok | is.na(lambda)  ~ 1.0,
      TRUE                     ~ 1 + delta * (lambda - 1)),
    lambda_shrunk = if_else(
      !is.finite(lambda_shrunk), 1.0, lambda_shrunk)
  ) %>%
  select(ciudad, articulo, subclase_ipc, k, k_city,
         apply_D, est_ok, lambda, r2, lambda_shrunk)

# Pre-build price base (anchor × IPC ratio — threshold-independent)
price_base_df <- lambda_shrunk_base %>%
  filter(!is.na(subclase_ipc)) %>%
  mutate(cod_subclase = substr(paste0(subclase_ipc, "00"), 1, 8)) %>%
  inner_join(anchor_prices, by = c("ciudad", "articulo")) %>%
  inner_join(ipc_t0_lookup, by = c("ciudad", "cod_subclase")) %>%
  inner_join(ipc_val,       by = c("ciudad", "cod_subclase")) %>%
  mutate(ipc_ratio = ipc / ipc_t0) %>%
  select(ciudad, articulo, fecha, subclase_ipc, k, k_city,
         apply_D, est_ok, lambda, r2, lambda_shrunk,
         precio_anchor, ipc_ratio)

message(sprintf("  %d rows for grid evaluation", nrow(price_base_df)))

# -----------------------------------------------------------------------
# Grid search
# -----------------------------------------------------------------------
message("Running grid search...")
grid_results <- list()

for (g in seq_len(nrow(grid))) {
  
  R2_g  <- grid$R2_MIN[g]
  LMin_g <- grid$LAM_MIN[g]
  LMax_g <- grid$LAM_MAX[g]
  
  pred_g <- price_base_df %>%
    mutate(
      reversion = case_when(
        !apply_D                    ~ "single_item",
        !est_ok | is.na(lambda)     ~ "est_failed",
        r2 < R2_g                   ~ "low_R2",
        lambda_shrunk < LMin_g      ~ "low_lambda",
        lambda_shrunk > LMax_g      ~ "high_lambda",
        TRUE                        ~ "none"),
      lambda_final = if_else(reversion == "none", lambda_shrunk, 1.0),
      price_pred   = precio_anchor * ipc_ratio ^ lambda_final,
      price_BASE   = precio_anchor * ipc_ratio)
  
  ape_g <- pred_g %>%
    inner_join(obs_val, by = c("ciudad", "articulo", "fecha")) %>%
    filter(!is.na(price_obs), price_obs > 0) %>%
    mutate(
      ape_S5   = abs((price_pred - price_obs) / price_obs) * 100,
      ape_BASE = abs((price_BASE - price_obs) / price_obs) * 100)
  
  if (nrow(ape_g) == 0) next
  
  ape_multi <- ape_g %>% filter(k > 1)
  
  grid_results[[g]] <- tibble(
    R2_MIN       = R2_g,
    LAM_MIN      = LMin_g,
    LAM_MAX      = LMax_g,
    MAPE_S5      = mean(ape_g$ape_S5,        na.rm=TRUE),
    MAPE_BASE    = mean(ape_g$ape_BASE,       na.rm=TRUE),
    imp_pp       = mean(ape_g$ape_BASE,       na.rm=TRUE) -
      mean(ape_g$ape_S5,         na.rm=TRUE),
    MAPE_S5_multi   = mean(ape_multi$ape_S5,  na.rm=TRUE),
    MAPE_BASE_multi = mean(ape_multi$ape_BASE,na.rm=TRUE),
    imp_pp_multi    = mean(ape_multi$ape_BASE,na.rm=TRUE) -
      mean(ape_multi$ape_S5,  na.rm=TRUE),
    n_dcity      = sum(pred_g$reversion == "none"       & pred_g$apply_D, na.rm=TRUE),
    n_low_r2     = sum(pred_g$reversion == "low_R2",    na.rm=TRUE),
    n_low_lam    = sum(pred_g$reversion == "low_lambda", na.rm=TRUE),
    n_high_lam   = sum(pred_g$reversion == "high_lambda",na.rm=TRUE),
    n_obs        = nrow(ape_g))
  
  if (g %% 9 == 0)
    message(sprintf("  %d/%d done...", g, nrow(grid)))
}

grid_df <- bind_rows(grid_results) %>%
  arrange(MAPE_S5_multi)

best_row     <- grid_df %>% slice(1)
params_optimal <- tibble(
  R2_MIN          = best_row$R2_MIN,
  LAM_MIN         = best_row$LAM_MIN,
  LAM_MAX         = best_row$LAM_MAX,
  MAPE_S5_multi   = best_row$MAPE_S5_multi,
  MAPE_BASE_multi = best_row$MAPE_BASE_multi,
  imp_pp_multi    = best_row$imp_pp_multi,
  n_dcity         = best_row$n_dcity,
  n_low_r2        = best_row$n_low_r2)

message(sprintf(
  "Optimal: R2_MIN=%.2f | LAM_MIN=%.2f | LAM_MAX=%.2f | MAPE=%.4f%%",
  params_optimal$R2_MIN, params_optimal$LAM_MIN,
  params_optimal$LAM_MAX, params_optimal$MAPE_S5_multi))

# -----------------------------------------------------------------------
# Figures
# -----------------------------------------------------------------------
p_heat <- grid_df %>%
  mutate(LAM_MAX_lbl = paste0("LAM_MAX = ", LAM_MAX),
         R2_lbl      = paste0("R²≥", R2_MIN),
         LAM_MIN_lbl = paste0("λ≥", LAM_MIN)) %>%
  ggplot(aes(x = LAM_MIN_lbl, y = R2_lbl, fill = MAPE_S5_multi)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = sprintf("%.3f", MAPE_S5_multi)),
            size = 3, color = "black") +
  scale_fill_gradient(low = "#1A7A4A", high = "#E74C3C",
                      name = "MAPE_S5\n(multi-item)") +
  facet_wrap(~ LAM_MAX_lbl, ncol = 3) +
  labs(
    title    = "Grid search — MAPE by threshold combination",
    subtitle = sprintf(
      "Optimal: R²≥%.2f | λ∈[%.2f,%.2f] → MAPE=%.4f%%",
      params_optimal$R2_MIN, params_optimal$LAM_MIN,
      params_optimal$LAM_MAX, params_optimal$MAPE_S5_multi),
    x = "LAM_MIN", y = "R²_MIN") +
  theme_bw(base_size = 11) +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

ggsave(file.path(ROBUST_DIR, "fig_threshold_heatmap.png"),
       p_heat, width = 13, height = 6, dpi = 200)

p_imp <- grid_df %>%
  mutate(LAM_MAX_lbl = paste0("LAM_MAX = ", LAM_MAX),
         R2_lbl      = paste0("R²≥", R2_MIN),
         LAM_MIN_lbl = paste0("λ≥", LAM_MIN)) %>%
  ggplot(aes(x = LAM_MIN_lbl, y = R2_lbl, fill = imp_pp_multi)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = sprintf("%+.3f", imp_pp_multi)),
            size = 3, color = "black") +
  scale_fill_gradient2(low = "#E74C3C", mid = "white",
                       high = "#1A7A4A", midpoint = 0,
                       name = "Improvement\n(pp)") +
  facet_wrap(~ LAM_MAX_lbl, ncol = 3) +
  labs(title    = "Grid search — improvement over BASE (pp)",
       subtitle = "Positive = S5 better than BASE | Multi-item sub-classes only",
       x = "LAM_MIN", y = "R²_MIN") +
  theme_bw(base_size = 11) +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

ggsave(file.path(ROBUST_DIR, "fig_threshold_improvement.png"),
       p_imp, width = 13, height = 6, dpi = 200)

# -----------------------------------------------------------------------
# Save
# -----------------------------------------------------------------------
write_csv(grid_df,        file.path(FORECAST_DIR, "threshold_grid_results.csv"))
write_csv(params_optimal, file.path(FORECAST_DIR, "params_optimal.csv"))
write_xlsx(list(grid = grid_df, optimal = params_optimal),
           file.path(ROBUST_DIR, "threshold_grid_results.xlsx"))

message("Done. Run 04_shrinkage_and_bounds.R next.")