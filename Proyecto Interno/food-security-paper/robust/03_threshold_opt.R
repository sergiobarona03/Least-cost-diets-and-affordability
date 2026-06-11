########################################################
## SCRIPT 03 — Grid search threshold optimization
##
## Reads:
##   cache/: dane.rds, ipc.rds, anchor_prices.rds
##   price_forecasting/lambda_raw.csv
##
## For each combination of (R2_MIN, LAM_MIN, LAM_MAX):
##   1. Applies JS shrinkage
##   2. Applies candidate thresholds
##   3. Builds price predictions for validation window
##   4. Computes MAPE vs DANE observed prices
##
## Grid:
##   R2_MIN  ∈ {0.05, 0.10, 0.15, 0.20}
##   LAM_MIN ∈ {0.10, 0.20, 0.30}
##   LAM_MAX ∈ {3.00, 4.00, 5.00}
##   → 36 combinations
##
## Writes to .../price_forecasting/:
##   threshold_grid_results.csv   ← MAPE per combination
##   params_optimal.csv           ← best combination
##   fig_threshold_heatmap.png    ← visual summary
##
## Validation window: Feb 2016 – Mar 2018
## (Jan excluded: IPC ratio = 1 at T0)
########################################################

library(tidyverse)
library(writexl)

# -----------------------------------------------------------------------
# 1. Directories and parameters
# -----------------------------------------------------------------------
dirs <- c(
  "C:\\Users\\sergio.barona\\Desktop\\Least-cost-diets-and-affordability\\Proyecto Interno",
  "C:/Users/Portatil/Desktop/Least-cost-diets-and-affordability/Proyecto Interno",
  "C:/Users/danie/OneDrive/Escritorio/Least-cost-diets-and-affordability/Proyecto Interno"
)
base_dir <- dirs[dir.exists(dirs)][1]
if (is.na(base_dir)) stop("No base directory found.")

cache_dir    <- file.path(base_dir,
                          "food-security-paper/output/price_forecasting/cache")
out_forecast <- file.path(base_dir,
                          "food-security-paper/output/price_forecasting")
robust_dir   <- file.path(base_dir, "food-security-paper/robust")

dir.create(robust_dir, recursive = TRUE, showWarnings = FALSE)

T0        <- as.Date("2016-01-01")
VAL_START <- as.Date("2016-02-01")
VAL_END   <- as.Date("2018-03-01")

# Grid definition
grid <- expand.grid(
  R2_MIN  = c(0.05, 0.10, 0.15, 0.20),
  LAM_MIN = c(0.10, 0.20, 0.30),
  LAM_MAX = c(3.00, 4.00, 5.00),
  stringsAsFactors = FALSE
) %>%
  filter(LAM_MIN < LAM_MAX)   # sanity check

message(sprintf("Grid: %d combinations", nrow(grid)))

# -----------------------------------------------------------------------
# 2. Load inputs
# -----------------------------------------------------------------------
message("Loading inputs...")

dane          <- readRDS(file.path(cache_dir, "dane.rds"))
ipc           <- readRDS(file.path(cache_dir, "ipc.rds"))
anchor_prices <- readRDS(file.path(cache_dir, "anchor_prices.rds"))

lambda_raw <- read_csv(
  file.path(out_forecast, "lambda_raw.csv"),
  show_col_types = FALSE)

# Observed prices for validation (DANE raw, Feb–Mar 2018)
obs_val <- dane %>%
  filter(fecha >= VAL_START, fecha <= VAL_END, !is.na(precio_500g)) %>%
  select(ciudad = nombre_ciudad, articulo, fecha,
         price_obs = precio_500g)

# IPC at anchor date T0
ipc_t0_lookup <- ipc %>%
  filter(fecha == T0) %>%
  select(ciudad, cod_subclase, ipc_t0 = ipc)

# IPC for validation window
ipc_val <- ipc %>%
  filter(fecha >= VAL_START, fecha <= VAL_END) %>%
  select(ciudad, cod_subclase, fecha, ipc)

message(sprintf("  obs_val: %d rows | ipc_val: %d rows",
                nrow(obs_val), nrow(ipc_val)))

# -----------------------------------------------------------------------
# 3. Pre-compute JS shrinkage parameters
#    (these are independent of thresholds)
# -----------------------------------------------------------------------
message("Pre-computing JS shrinkage parameters...")

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
      !is.finite(lambda_shrunk), 1.0, lambda_shrunk)
  ) %>%
  select(ciudad, articulo, subclase_ipc, k,
         apply_D, est_ok, lambda, r2, lambda_shrunk)

# Pre-build price prediction base (anchor × IPC ratio per item × month)
# This part is threshold-independent; we multiply by lambda later
price_base_df <- lambda_shrunk_base %>%
  filter(!is.na(subclase_ipc)) %>%
  mutate(cod_subclase = substr(paste0(subclase_ipc, "00"), 1, 8)) %>%
  inner_join(anchor_prices, by = c("ciudad", "articulo")) %>%
  inner_join(ipc_t0_lookup, by = c("ciudad", "cod_subclase")) %>%
  inner_join(ipc_val,       by = c("ciudad", "cod_subclase")) %>%
  mutate(ipc_ratio = ipc / ipc_t0) %>%
  select(ciudad, articulo, fecha, subclase_ipc, k,
         apply_D, est_ok, lambda, r2, lambda_shrunk,
         precio_anchor, ipc_ratio)

message(sprintf("  %d item × city × month rows for grid evaluation",
                nrow(price_base_df)))

# -----------------------------------------------------------------------
# 4. Grid search
# -----------------------------------------------------------------------
message("Running grid search...")

grid_results <- list()

for (g in seq_len(nrow(grid))) {
  
  R2_MIN_g  <- grid$R2_MIN[g]
  LAM_MIN_g <- grid$LAM_MIN[g]
  LAM_MAX_g <- grid$LAM_MAX[g]
  
  # Apply thresholds to shrunk lambdas
  pred_g <- price_base_df %>%
    mutate(
      reversion = case_when(
        !apply_D                   ~ "single_item",
        !est_ok | is.na(lambda)    ~ "est_failed",
        r2 < R2_MIN_g              ~ "low_R2",
        lambda_shrunk < LAM_MIN_g  ~ "low_lambda",
        lambda_shrunk > LAM_MAX_g  ~ "high_lambda",
        TRUE                       ~ "none"),
      lambda_final = if_else(reversion == "none", lambda_shrunk, 1.0),
      price_pred   = precio_anchor * ipc_ratio ^ lambda_final,
      price_BASE   = precio_anchor * ipc_ratio
    )
  
  # Join observed and compute APE
  ape_g <- pred_g %>%
    inner_join(obs_val, by = c("ciudad", "articulo", "fecha")) %>%
    filter(!is.na(price_obs), price_obs > 0) %>%
    mutate(
      ape_S5   = abs((price_pred - price_obs) / price_obs) * 100,
      ape_BASE = abs((price_BASE - price_obs) / price_obs) * 100)
  
  if (nrow(ape_g) == 0) next
  
  # Global MAPE
  mape_s5   <- mean(ape_g$ape_S5,   na.rm = TRUE)
  mape_base <- mean(ape_g$ape_BASE, na.rm = TRUE)
  
  # MAPE for multi-item only (where thresholds actually matter)
  ape_multi <- ape_g %>% filter(k > 1)
  mape_s5_multi   <- mean(ape_multi$ape_S5,   na.rm = TRUE)
  mape_base_multi <- mean(ape_multi$ape_BASE, na.rm = TRUE)
  
  # Count reversions
  n_dcity    <- sum(pred_g$reversion == "none"      & pred_g$apply_D, na.rm=TRUE)
  n_low_r2   <- sum(pred_g$reversion == "low_R2",   na.rm=TRUE)
  n_low_lam  <- sum(pred_g$reversion == "low_lambda",na.rm=TRUE)
  n_high_lam <- sum(pred_g$reversion == "high_lambda",na.rm=TRUE)
  
  grid_results[[g]] <- tibble(
    R2_MIN       = R2_MIN_g,
    LAM_MIN      = LAM_MIN_g,
    LAM_MAX      = LAM_MAX_g,
    MAPE_S5      = mape_s5,
    MAPE_BASE    = mape_base,
    imp_pp       = mape_base - mape_s5,
    MAPE_S5_multi   = mape_s5_multi,
    MAPE_BASE_multi = mape_base_multi,
    imp_pp_multi    = mape_base_multi - mape_s5_multi,
    n_dcity      = n_dcity,
    n_low_r2     = n_low_r2,
    n_low_lam    = n_low_lam,
    n_high_lam   = n_high_lam,
    n_obs        = nrow(ape_g))
  
  if (g %% 9 == 0)
    message(sprintf("  %d/%d done...", g, nrow(grid)))
}

grid_df <- bind_rows(grid_results) %>%
  arrange(MAPE_S5_multi)

message("\nTop 5 combinations (by MAPE_S5 on multi-item sub-classes):")
print(grid_df %>% select(R2_MIN, LAM_MIN, LAM_MAX,
                         MAPE_S5_multi, imp_pp_multi,
                         n_dcity, n_low_r2) %>%
        head(5), n = 5)

# -----------------------------------------------------------------------
# 5. Select optimal parameters
# -----------------------------------------------------------------------
# Criterion: minimise MAPE_S5 on multi-item sub-classes
# (these are the only sub-classes where thresholds have any effect)
best_row <- grid_df %>% slice(1)

params_optimal <- tibble(
  R2_MIN  = best_row$R2_MIN,
  LAM_MIN = best_row$LAM_MIN,
  LAM_MAX = best_row$LAM_MAX,
  MAPE_S5_multi   = best_row$MAPE_S5_multi,
  MAPE_BASE_multi = best_row$MAPE_BASE_multi,
  imp_pp_multi    = best_row$imp_pp_multi,
  n_dcity         = best_row$n_dcity,
  n_low_r2        = best_row$n_low_r2
)

message(sprintf(
  "\nOptimal thresholds: R2_MIN=%.2f | LAM_MIN=%.2f | LAM_MAX=%.2f",
  params_optimal$R2_MIN,
  params_optimal$LAM_MIN,
  params_optimal$LAM_MAX))
message(sprintf(
  "MAPE (multi-item): BASE=%.4f%% → S5=%.4f%% (%+.4f pp)",
  params_optimal$MAPE_BASE_multi,
  params_optimal$MAPE_S5_multi,
  params_optimal$imp_pp_multi))

# -----------------------------------------------------------------------
# 6. Figures
# -----------------------------------------------------------------------
message("Building heatmap figures...")

# One heatmap per LAM_MAX value, rows = R2_MIN, cols = LAM_MIN
p_heat <- grid_df %>%
  mutate(
    LAM_MAX_lbl = paste0("LAM_MAX = ", LAM_MAX),
    R2_lbl      = paste0("R²≥", R2_MIN),
    LAM_MIN_lbl = paste0("λ≥", LAM_MIN)
  ) %>%
  ggplot(aes(x = LAM_MIN_lbl, y = R2_lbl,
             fill = MAPE_S5_multi)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = sprintf("%.3f", MAPE_S5_multi)),
            size = 3, color = "black") +
  scale_fill_gradient(low = "#1A7A4A", high = "#E74C3C",
                      name = "MAPE_S5\n(multi-item)") +
  facet_wrap(~ LAM_MAX_lbl, ncol = 3) +
  labs(
    title    = "Grid search — MAPE by threshold combination",
    subtitle = sprintf(
      "Validation: Feb 2016–Mar 2018 | Criterion: MAPE on multi-item sub-classes\nOptimal: R²≥%.2f | λ∈[%.2f, %.2f] → MAPE=%.4f%%",
      params_optimal$R2_MIN, params_optimal$LAM_MIN,
      params_optimal$LAM_MAX, params_optimal$MAPE_S5_multi),
    x = "LAM_MIN (lower bound)",
    y = "R²_MIN (signal threshold)"
  ) +
  theme_bw(base_size = 11) +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

ggsave(file.path(robust_dir, "fig_threshold_heatmap.png"),
       p_heat, width = 13, height = 6, dpi = 200)

# Improvement over BASE
p_imp <- grid_df %>%
  mutate(
    LAM_MAX_lbl = paste0("LAM_MAX = ", LAM_MAX),
    R2_lbl      = paste0("R²≥", R2_MIN),
    LAM_MIN_lbl = paste0("λ≥", LAM_MIN)
  ) %>%
  ggplot(aes(x = LAM_MIN_lbl, y = R2_lbl,
             fill = imp_pp_multi)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = sprintf("%+.3f", imp_pp_multi)),
            size = 3, color = "black") +
  scale_fill_gradient2(low = "#E74C3C", mid = "white",
                       high = "#1A7A4A", midpoint = 0,
                       name = "Improvement\n(pp)") +
  facet_wrap(~ LAM_MAX_lbl, ncol = 3) +
  labs(
    title    = "Grid search — improvement over BASE (pp)",
    subtitle = "Positive = S5 better than BASE | Multi-item sub-classes only",
    x = "LAM_MIN", y = "R²_MIN"
  ) +
  theme_bw(base_size = 11) +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

ggsave(file.path(robust_dir, "fig_threshold_improvement.png"),
       p_imp, width = 13, height = 6, dpi = 200)

# -----------------------------------------------------------------------
# 7. Save outputs
# -----------------------------------------------------------------------
write_csv(grid_df,        file.path(out_forecast, "threshold_grid_results.csv"))
write_csv(params_optimal, file.path(out_forecast, "params_optimal.csv"))

write_xlsx(
  list(grid_results = grid_df, optimal = params_optimal),
  file.path(robust_dir, "threshold_grid_results.xlsx"))

message(sprintf("Optimal params saved to: %s", out_forecast))
message("Done. Run Script 04 next.")