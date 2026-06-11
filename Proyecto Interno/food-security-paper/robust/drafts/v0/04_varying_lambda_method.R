########################################################
## ROBUSTNESS CHECK — Solution D + Improvements 2 + 4
## Script 05e: James-Stein shrinkage + city-heterogeneous lambda
##
## Builds directly on Script 05c (Improvement 2).
## Improvement 3 (rolling window) was discarded after
## validation showed no meaningful gain over full-history
## JS shrinkage.
##
## Selection rule (unchanged):
##   k == 1  →  BASE  (single-item sub-class)
##   k >  1  →  D with city-specific JS shrinkage
##
## Improvement 4 — City-heterogeneous lambda:
##   Script 05c estimated one lambda_j per item, pooling
##   all three cities. This script estimates lambda_jc
##   separately for each item × city combination.
##
##   Motivation from diagnostic:
##     - 65 multi-item items have lambda in all 3 cities
##     - Mean inter-city lambda range: 0.286
##     - 32.3% of items have inter-city range > 0.3
##     - P75 of range: 0.442
##   This heterogeneity is large enough to matter for
##   price prediction.
##
##   Model (first differences, per item × city):
##     Δlog(p_jct) = λ_jc × Δlog(IPC_sct) + ε_jct
##
##   JS shrinkage (per sub-class × city):
##     Shrinks {λ_jc}_j toward 1 using sub-class×city params.
##     For k >= 3: delta_JS = max(0, 1-(k-2)*σ²_sc/Q_sc)
##     For k == 2: delta_ridge = (λ_jc-1)²/((λ_jc-1)²+σ²_jc)
##
##   Items with lambda only in 1 or 2 cities:
##     Use the city-specific estimate where available;
##     fall back to pooled estimate (from 05c) elsewhere.
##
## Three methods compared on Jan 2016–Mar 2018:
##   BASE         : lambda = 1 (current method)
##   D_shrunk     : lambda = JS-shrunk, pooled across cities (05c)
##   D_shrunk_C   : lambda = JS-shrunk, city-specific (this script)
##
## Outputs (.../food-security-paper/robust/):
##   - lambda_city.csv              (λ_jc, shrunk, delta per item×city)
##   - D4_validation_subclass.csv
##   - D4_validation_item.csv
##   - D4_validation_global.csv
##   - prices_extended_corrected_D4.csv / .rds
##   - fig_lambda_city_heterogeneity.png
##   - fig_D4_mape_by_subclass.png
##   - fig_lambda_pooled_vs_city.png
##
## Decision print:
##   Compares BASE, D_shrunk (05c), D_shrunk_C (this script)
##   globally and for multi-item sub-classes only.
########################################################

# -----------------------------
# 0. Packages
# -----------------------------
library(tidyverse)
library(readxl)
library(janitor)
library(lubridate)
library(writexl)
library(broom)

# -----------------------------
# 1. Directories
# -----------------------------
dirs <- c(
  "C:\\Users\\sergio.barona\\Desktop\\Least-cost-diets-and-affordability\\Proyecto Interno",
  "C:/Users/Portatil/Desktop/Least-cost-diets-and-affordability/Proyecto Interno",
  "C:/Users/danie/OneDrive/Escritorio/Least-cost-diets-and-affordability/Proyecto Interno"
)
base_dir <- dirs[dir.exists(dirs)][1]
if (is.na(base_dir)) stop("Ninguno de los directorios existe")

in_prices <- file.path(base_dir,
                       "Precios DANE/OUTPUT_DANE/precios_unadj_DANE_1999_2018.xlsx")
in_ipc  <- file.path(base_dir, "var-ipc/IPC.xls")
in_ipc2 <- file.path(base_dir, "var-ipc/IPC_2.xls")
in_ipc3 <- file.path(base_dir, "var-ipc/IPC_3.xls")
in_ipc4 <- file.path(base_dir, "var-ipc/IPC_4.xls")
in_corr1 <- file.path(base_dir, "var-ipc/correlativa_ipc.xlsx")
in_corr2 <- file.path(base_dir, "var-ipc/correlativa_ipc_articulos.xlsx")
in_prices_extended <- file.path(base_dir,
                                "food-security-paper/output/forecasting_fullsample/prices_extended_city_article_month.csv")

# Outputs from 05c
in_shrunk_05c <- file.path(base_dir,
                           "food-security-paper/robust/shrinkage_params.csv")

out_dir <- file.path(base_dir, "food-security-paper/robust")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# -----------------------------
# 2. Parameters
# -----------------------------
TRAIN_END    <- as.Date("2015-12-01")
T0           <- as.Date("2016-01-01")
VAL_END      <- as.Date("2018-03-01")
EXTRAP_START <- as.Date("2018-04-01")
cities_use   <- c("CALI", "BOGOTÁ D.C.", "MEDELLÍN")
MIN_OBS      <- 24L

# -----------------------------
# 3. Auxiliary functions
# -----------------------------
meses_esp <- c("Ene","Feb","Mar","Ago","Sep","Oct","Nov","Dic",
               "Abr","May","Jun","Jul")
meses_esp <- c("Ene","Feb","Mar","Abr",
               "May","Jun","Jul","Ago","Sep","Oct","Nov","Dic")

make_date <- function(ano, mes_num)
  as.Date(sprintf("%04d-%02d-01",
                  as.integer(ano), as.integer(mes_num)))

mode1 <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_character_)
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Estimate lambda_jc via first-difference OLS (no intercept)
estimate_lambda_fd <- function(obs_slice, ipc_series,
                               min_obs = MIN_OBS) {
  if (nrow(obs_slice) < min_obs) return(NULL)
  
  merged <- obs_slice %>%
    select(fecha, precio_500g) %>%
    inner_join(ipc_series %>% select(fecha, ipc), by = "fecha") %>%
    arrange(fecha) %>%
    mutate(
      d_log_p   = log(precio_500g) - lag(log(precio_500g)),
      d_log_ipc = log(ipc)         - lag(log(ipc))
    ) %>%
    filter(!is.na(d_log_p), !is.na(d_log_ipc),
           is.finite(d_log_p), is.finite(d_log_ipc))
  
  if (nrow(merged) < min_obs) return(NULL)
  
  fit <- tryCatch(
    lm(d_log_p ~ d_log_ipc - 1, data = merged),
    error = function(e) NULL
  )
  if (is.null(fit)) return(NULL)
  
  list(
    lambda = coef(fit)[["d_log_ipc"]],
    sigma2 = sum(resid(fit)^2) / max(nrow(merged) - 1, 1),
    n_obs  = nrow(merged)
  )
}

# James-Stein / ridge shrinkage
shrink_lambda <- function(lambda, sigma2, k, sigma2_s, Q_s) {
  if (k == 1)  return(1.0)
  if (k >= 3) {
    delta <- max(0, 1 - (k - 2) * sigma2_s / Q_s)
  } else {
    dev   <- (lambda - 1)^2
    delta <- if ((dev + sigma2) > 0) dev / (dev + sigma2) else 0
  }
  1 + delta * (lambda - 1)
}

# -----------------------------
# 4. Load data
# -----------------------------
message("Loading DANE prices (1999-2018)...")
dane <- read_excel(in_prices) %>%
  mutate(
    fecha        = make_date(ano, mes_num),
    cod_subclase = paste0("0", substr(codigo_articulo, 1, 6), "0")
  ) %>%
  filter(!is.na(fecha), !is.na(precio_500g),
         nombre_ciudad %in% cities_use)

message("Loading CPI series...")
ipc <- map(list(in_ipc, in_ipc2, in_ipc3, in_ipc4),
           ~ read_excel(.x) %>% clean_names()) %>%
  bind_rows() %>%
  mutate(
    ciudad = case_when(
      ciudad == "CARTAGENA DE INDIAS" ~ "CARTAGENA",
      ciudad == "BOGOTÁ, D.C."        ~ "BOGOTÁ D.C.",
      TRUE ~ ciudad
    ),
    cod_subclase = substr(subclase, 1, 8),
    mes_num      = match(mes, meses_esp),
    ipc          = as.numeric(numero_indice),
    fecha        = make_date(as.integer(ano), mes_num)
  ) %>%
  select(ciudad, cod_subclase, fecha, ipc) %>%
  filter(!is.na(fecha), !is.na(ipc),
         !is.na(cod_subclase), !is.na(ciudad),
         ciudad %in% cities_use) %>%
  distinct(ciudad, cod_subclase, fecha, .keep_all = TRUE) %>%
  arrange(ciudad, cod_subclase, fecha)

message("Loading crosswalk tables...")
corr_subclase <- read_excel(in_corr1) %>%
  fill(subclase, ipc, .direction = "down") %>%
  mutate(cod_subclase = paste0("0", gasto_basico, "00")) %>%
  select(cod_subclase, subclase) %>%
  mutate(across(everything(), as.character))

corr_producto <- read_excel(in_corr2) %>%
  rename(codigo_articulo = cod_dane, subclase = cod_ipc) %>%
  mutate(across(everything(), as.character))

message("Loading extended prices and 05c shrinkage params...")
prices_extended <- read_csv(in_prices_extended,
                            show_col_types = FALSE) %>%
  mutate(fecha = as.Date(fecha))

shrunk_05c <- read_csv(in_shrunk_05c, show_col_types = FALSE)

# Pooled shrunk lambda from 05c — used as fallback
pooled_lambda <- shrunk_05c %>%
  select(ciudad, articulo, subclase_ipc, k, apply_D,
         lambda_pooled    = lambda,
         lambda_shrunk_pooled = lambda_shrunk)

# -----------------------------
# 5. Sub-class assignment
# -----------------------------
assign_subclase <- function(df_series, corr_subclase, corr_producto) {
  tmp1 <- df_series %>%
    left_join(corr_subclase, by = "cod_subclase") %>%
    mutate(subclase = as.character(subclase))
  sub1 <- mode1(tmp1$subclase)
  if (is.na(sub1) || length(unique(na.omit(tmp1$subclase))) > 1) {
    tmp2 <- df_series %>%
      left_join(corr_producto, by = "codigo_articulo") %>%
      mutate(subclase = as.character(subclase))
    return(list(subclase = mode1(tmp2$subclase), method = "producto"))
  }
  list(subclase = sub1, method = "gasto_basico")
}

# Anchor prices at T0
prices_at_T0 <- dane %>%
  filter(fecha == T0) %>%
  select(nombre_ciudad, articulo, precio_500g) %>%
  rename(precio_anchor = precio_500g)

valid_items <- prices_at_T0 %>%
  distinct(nombre_ciudad, articulo)

# -----------------------------------------------------------------------
# 6. Estimate city-specific lambda_jc for all multi-item items
# -----------------------------------------------------------------------
message("Estimating city-specific lambdas...")

all_lambda_city <- list()

for (i in seq_len(nrow(valid_items))) {
  
  city_name <- valid_items$nombre_ciudad[i]
  food_name <- valid_items$articulo[i]
  
  # Check if multi-item
  pool_row <- pooled_lambda %>%
    filter(ciudad == city_name, articulo == food_name)
  
  apply_d <- if (nrow(pool_row) > 0) pool_row$apply_D[1] else FALSE
  k_val   <- if (nrow(pool_row) > 0) pool_row$k[1]       else 1L
  
  # Single-item: skip, will use BASE
  if (!apply_d) next
  
  df_obs <- dane %>%
    filter(nombre_ciudad == city_name, articulo == food_name,
           fecha <= TRAIN_END, !is.na(precio_500g)) %>%
    mutate(codigo_articulo = as.character(codigo_articulo),
           cod_subclase    = as.character(cod_subclase)) %>%
    arrange(fecha)
  
  if (nrow(df_obs) == 0) next
  
  sub_info     <- assign_subclase(df_obs, corr_subclase, corr_producto)
  subclase_ipc <- sub_info$subclase
  if (is.na(subclase_ipc)) next
  
  subclase_code <- substr(paste0(subclase_ipc, "00"), 1, 8)
  
  df_ipc <- ipc %>%
    filter(ciudad == city_name, cod_subclase == subclase_code,
           fecha <= TRAIN_END) %>%
    select(fecha, ipc) %>% arrange(fecha)
  
  if (nrow(df_ipc) == 0) next
  
  est <- estimate_lambda_fd(df_obs, df_ipc)
  
  # If estimation fails: flag for pooled fallback
  if (is.null(est)) {
    all_lambda_city[[length(all_lambda_city) + 1]] <- tibble(
      ciudad          = city_name,
      articulo        = food_name,
      subclase_ipc    = subclase_ipc,
      k               = k_val,
      apply_D         = TRUE,
      lambda_city     = NA_real_,
      sigma2_city     = NA_real_,
      n_obs_city      = NA_integer_,
      use_pooled      = TRUE
    )
    next
  }
  
  all_lambda_city[[length(all_lambda_city) + 1]] <- tibble(
    ciudad          = city_name,
    articulo        = food_name,
    subclase_ipc    = subclase_ipc,
    k               = k_val,
    apply_D         = TRUE,
    lambda_city     = est$lambda,
    sigma2_city     = est$sigma2,
    n_obs_city      = est$n_obs,
    use_pooled      = FALSE
  )
}

lambda_city_raw <- bind_rows(all_lambda_city)

message(sprintf("  City-specific lambdas estimated for %d series.",
                sum(!lambda_city_raw$use_pooled, na.rm = TRUE)))
message(sprintf("  Falling back to pooled lambda for %d series.",
                sum(lambda_city_raw$use_pooled, na.rm = TRUE)))

# -----------------------------------------------------------------------
# 7. Apply JS shrinkage using city-specific sub-class parameters
#
#    Sub-class JS params are now computed per sub-class × city,
#    using the city-specific lambda estimates.
# -----------------------------------------------------------------------
message("Computing city-specific JS shrinkage factors...")

# Sub-class × city parameters
subclass_city_params <- lambda_city_raw %>%
  filter(!use_pooled, !is.na(lambda_city)) %>%
  group_by(subclase_ipc, ciudad) %>%
  summarise(
    k_city    = n_distinct(articulo),
    sigma2_sc = mean(sigma2_city, na.rm = TRUE),
    Q_sc      = sum((lambda_city - 1)^2, na.rm = TRUE),
    .groups   = "drop"
  )

# Apply shrinkage
lambda_city_shrunk <- lambda_city_raw %>%
  left_join(subclass_city_params,
            by = c("subclase_ipc", "ciudad")) %>%
  left_join(pooled_lambda %>%
              select(ciudad, articulo,
                     lambda_pooled, lambda_shrunk_pooled),
            by = c("ciudad", "articulo")) %>%
  mutate(
    # City-specific shrunk lambda
    lambda_shrunk_city = case_when(
      # Single-item or failed estimation: BASE
      !apply_D | is.na(lambda_city) ~ 1.0,
      # k == 1 in this city (shouldn't happen but guard)
      k_city == 1 ~ 1.0,
      # k >= 3: James-Stein
      k_city >= 3 ~ pmax(0, 1 - (k_city - 2) *
                           sigma2_sc / Q_sc) *
        (lambda_city - 1) + 1,
      # k == 2: ridge shrinkage
      TRUE ~ {
        dev   <- (lambda_city - 1)^2
        delta <- if_else((dev + sigma2_city) > 0,
                         dev / (dev + sigma2_city), 0)
        1 + delta * (lambda_city - 1)
      }
    ),
    # Fallback: if city lambda failed, use pooled shrunk
    lambda_final_city = case_when(
      use_pooled | is.na(lambda_shrunk_city) ~ lambda_shrunk_pooled,
      !is.finite(lambda_shrunk_city)          ~ lambda_shrunk_pooled,
      TRUE                                    ~ lambda_shrunk_city
    ),
    # Delta (shrinkage factor)
    delta_city = case_when(
      use_pooled | is.na(lambda_city) ~ NA_real_,
      abs(lambda_city - 1) < 1e-10   ~ 1.0,
      TRUE ~ (lambda_final_city - 1) / (lambda_city - 1)
    ),
    # How much did city-specific lambda shift vs pooled?
    delta_vs_pooled = lambda_final_city - lambda_shrunk_pooled
  )

# Shrinkage summary
shrinkage_summary <- lambda_city_shrunk %>%
  filter(apply_D, !use_pooled) %>%
  summarise(
    n                   = n(),
    delta_mean          = mean(delta_city,        na.rm = TRUE),
    delta_median        = median(delta_city,      na.rm = TRUE),
    mean_abs_raw        = mean(abs(lambda_city - 1),         na.rm = TRUE),
    mean_abs_shrunk     = mean(abs(lambda_final_city - 1),   na.rm = TRUE),
    shrinkage_pct       = (1 - mean_abs_shrunk / mean_abs_raw) * 100,
    mean_abs_shift_pooled = mean(abs(delta_vs_pooled),       na.rm = TRUE),
    pct_shifted_gt_0.1  = mean(abs(delta_vs_pooled) > 0.1,  na.rm = TRUE) * 100
  )

message(sprintf(
  "  Mean |lambda_city - 1| → before: %.3f, after shrinkage: %.3f (%.1f%% reduction)",
  shrinkage_summary$mean_abs_raw,
  shrinkage_summary$mean_abs_shrunk,
  shrinkage_summary$shrinkage_pct))
message(sprintf(
  "  Mean shift vs pooled lambda: %.3f | %.1f%% of series shifted >0.1",
  shrinkage_summary$mean_abs_shift_pooled,
  shrinkage_summary$pct_shifted_gt_0.1))

# -----------------------------------------------------------------------
# 8. Build price predictions for all items × months
# -----------------------------------------------------------------------
message("Building price predictions...")

all_series <- list()

for (i in seq_len(nrow(valid_items))) {
  
  city_name <- valid_items$nombre_ciudad[i]
  food_name <- valid_items$articulo[i]
  
  # Lambda lookup
  lam_row <- lambda_city_shrunk %>%
    filter(ciudad == city_name, articulo == food_name)
  
  pool_row <- pooled_lambda %>%
    filter(ciudad == city_name, articulo == food_name)
  
  apply_d <- if (nrow(pool_row) > 0) pool_row$apply_D[1] else FALSE
  k_val   <- if (nrow(pool_row) > 0) pool_row$k[1]       else 1L
  
  # Sub-class
  df_obs <- dane %>%
    filter(nombre_ciudad == city_name, articulo == food_name,
           !is.na(precio_500g)) %>%
    mutate(codigo_articulo = as.character(codigo_articulo),
           cod_subclase    = as.character(cod_subclase)) %>%
    arrange(fecha)
  
  if (nrow(df_obs) == 0) next
  
  sub_info     <- assign_subclase(df_obs, corr_subclase, corr_producto)
  subclase_ipc <- sub_info$subclase
  if (is.na(subclase_ipc)) next
  
  subclase_code <- substr(paste0(subclase_ipc, "00"), 1, 8)
  
  df_ipc <- ipc %>%
    filter(ciudad == city_name, cod_subclase == subclase_code) %>%
    select(fecha, ipc) %>% arrange(fecha)
  
  if (nrow(df_ipc) == 0) next
  
  anchor <- prices_at_T0 %>%
    filter(nombre_ciudad == city_name, articulo == food_name) %>%
    pull(precio_anchor)
  if (length(anchor) == 0) next
  anchor <- anchor[1]
  
  ipc_t0 <- df_ipc %>% filter(fecha == T0) %>% pull(ipc)
  if (length(ipc_t0) == 0 || is.na(ipc_t0)) next
  ipc_t0 <- ipc_t0[1]
  
  # Lambda values
  lambda_shrunk_p <- if (nrow(pool_row) > 0)
    pool_row$lambda_shrunk_pooled[1] else 1.0
  lambda_shrunk_c <- if (nrow(lam_row) > 0)
    lam_row$lambda_final_city[1]     else lambda_shrunk_p
  
  if (!is.finite(lambda_shrunk_c)) lambda_shrunk_c <- lambda_shrunk_p
  
  # Observed in validation window
  obs_val <- df_obs %>%
    filter(fecha >= T0, fecha <= VAL_END) %>%
    select(fecha, price_obs = precio_500g)
  
  # Predictions
  full_pred <- df_ipc %>%
    filter(fecha >= T0) %>%
    mutate(
      ciudad             = city_name,
      articulo           = food_name,
      subclase_ipc       = subclase_ipc,
      k                  = k_val,
      apply_D            = apply_d,
      lambda_shrunk_pooled = lambda_shrunk_p,
      lambda_shrunk_city = lambda_shrunk_c,
      ipc_ratio          = ipc / ipc_t0,
      price_BASE         = anchor * ipc_ratio,
      price_D_shrunk     = anchor * ipc_ratio ^ lambda_shrunk_p,
      price_D_shrunk_C   = anchor * ipc_ratio ^ lambda_shrunk_c,
      period             = case_when(
        fecha <= VAL_END      ~ "validation",
        fecha >= EXTRAP_START ~ "extrapolation",
        TRUE                  ~ "other"
      )
    ) %>%
    left_join(obs_val, by = "fecha")
  
  all_series[[length(all_series) + 1]] <- full_pred
}

series_df <- bind_rows(all_series)
message(sprintf("  %d item × city × month observations built.",
                nrow(series_df)))

# -----------------------------------------------------------------------
# 9. Compute APEs in validation window
# -----------------------------------------------------------------------
val_ape <- series_df %>%
  filter(period == "validation", !is.na(price_obs)) %>%
  mutate(
    ape_BASE       = abs((price_BASE     - price_obs) / price_obs) * 100,
    ape_D_shrunk   = abs((price_D_shrunk - price_obs) / price_obs) * 100,
    ape_D_shrunk_C = abs((price_D_shrunk_C - price_obs) / price_obs) * 100
  )

# -----------------------------------------------------------------------
# 10. Validation summaries
# -----------------------------------------------------------------------

# 10a. Global
global_val <- val_ape %>%
  summarise(
    n_obs                = n(),
    MAPE_BASE            = mean(ape_BASE,       na.rm = TRUE),
    MAPE_D_shrunk        = mean(ape_D_shrunk,   na.rm = TRUE),
    MAPE_D_shrunk_C      = mean(ape_D_shrunk_C, na.rm = TRUE),
    max_BASE             = max(ape_BASE,        na.rm = TRUE),
    max_D_shrunk         = max(ape_D_shrunk,    na.rm = TRUE),
    max_D_shrunk_C       = max(ape_D_shrunk_C,  na.rm = TRUE),
    pct_C_beats_BASE     = mean(ape_D_shrunk_C < ape_BASE,
                                na.rm = TRUE) * 100,
    pct_C_beats_pooled   = mean(ape_D_shrunk_C < ape_D_shrunk,
                                na.rm = TRUE) * 100,
    imp_C_vs_BASE        = MAPE_BASE      - MAPE_D_shrunk_C,
    imp_C_vs_pooled      = MAPE_D_shrunk  - MAPE_D_shrunk_C
  )

# 10b. Multi-item only
global_multi <- val_ape %>%
  filter(apply_D) %>%
  summarise(
    n_obs                = n(),
    MAPE_BASE            = mean(ape_BASE,       na.rm = TRUE),
    MAPE_D_shrunk        = mean(ape_D_shrunk,   na.rm = TRUE),
    MAPE_D_shrunk_C      = mean(ape_D_shrunk_C, na.rm = TRUE),
    pct_C_beats_BASE     = mean(ape_D_shrunk_C < ape_BASE,
                                na.rm = TRUE) * 100,
    pct_C_beats_pooled   = mean(ape_D_shrunk_C < ape_D_shrunk,
                                na.rm = TRUE) * 100,
    imp_C_vs_BASE        = MAPE_BASE     - MAPE_D_shrunk_C,
    imp_C_vs_pooled      = MAPE_D_shrunk - MAPE_D_shrunk_C
  )

# 10c. By sub-class
subclass_val <- val_ape %>%
  group_by(subclase_ipc, apply_D) %>%
  summarise(
    n_items          = n_distinct(articulo),
    n_obs            = n(),
    MAPE_BASE        = mean(ape_BASE,       na.rm = TRUE),
    MAPE_D_shrunk    = mean(ape_D_shrunk,   na.rm = TRUE),
    MAPE_D_shrunk_C  = mean(ape_D_shrunk_C, na.rm = TRUE),
    imp_C_vs_BASE    = mean(ape_BASE,       na.rm = TRUE) -
      mean(ape_D_shrunk_C, na.rm = TRUE),
    imp_C_vs_pooled  = mean(ape_D_shrunk,   na.rm = TRUE) -
      mean(ape_D_shrunk_C, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    best_method = case_when(
      !apply_D                                    ~ "BASE (single-item)",
      MAPE_D_shrunk_C <= MAPE_D_shrunk &
        MAPE_D_shrunk_C <= MAPE_BASE              ~ "D_shrunk_C",
      MAPE_D_shrunk <= MAPE_D_shrunk_C &
        MAPE_D_shrunk <= MAPE_BASE                ~ "D_shrunk_pooled",
      TRUE                                        ~ "BASE"
    )
  ) %>%
  arrange(desc(MAPE_BASE))

# 10d. By item
item_val <- val_ape %>%
  group_by(ciudad, articulo, subclase_ipc,
           apply_D, lambda_shrunk_pooled, lambda_shrunk_city) %>%
  summarise(
    n_months         = n(),
    MAPE_BASE        = mean(ape_BASE,       na.rm = TRUE),
    MAPE_D_shrunk    = mean(ape_D_shrunk,   na.rm = TRUE),
    MAPE_D_shrunk_C  = mean(ape_D_shrunk_C, na.rm = TRUE),
    imp_C_vs_BASE    = mean(ape_BASE,       na.rm = TRUE) -
      mean(ape_D_shrunk_C, na.rm = TRUE),
    imp_C_vs_pooled  = mean(ape_D_shrunk,   na.rm = TRUE) -
      mean(ape_D_shrunk_C, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    best_method = case_when(
      !apply_D                                   ~ "BASE",
      MAPE_D_shrunk_C <= MAPE_D_shrunk &
        MAPE_D_shrunk_C <= MAPE_BASE             ~ "D_shrunk_C",
      MAPE_D_shrunk <= MAPE_D_shrunk_C &
        MAPE_D_shrunk <= MAPE_BASE               ~ "D_shrunk_pooled",
      TRUE                                       ~ "BASE"
    )
  ) %>%
  arrange(desc(MAPE_BASE))

# -----------------------------------------------------------------------
# 11. Apply best correction to extrapolation period (2018-2024)
# -----------------------------------------------------------------------
message("Applying city-specific correction to extrapolation period...")

extrap_corrected <- series_df %>%
  filter(period == "extrapolation") %>%
  mutate(
    precio_corrected = if_else(apply_D,
                               price_D_shrunk_C,
                               price_BASE),
    source_method    = if_else(apply_D,
                               "D_shrunk_city",
                               "BASE")
  )

prices_extended_corrected <- prices_extended %>%
  left_join(
    extrap_corrected %>%
      select(ciudad, articulo, fecha,
             precio_corrected, source_method),
    by = c("ciudad", "articulo", "fecha")
  ) %>%
  mutate(
    precio_final_D4 = if_else(
      !is.na(precio_corrected) & status == "imputed",
      precio_corrected,
      precio_final
    ),
    method_D4 = if_else(
      !is.na(source_method) & status == "imputed",
      source_method,
      "observed"
    )
  )

n_corrected <- sum(
  !is.na(prices_extended_corrected$precio_corrected) &
    prices_extended_corrected$status == "imputed",
  na.rm = TRUE)
n_imputed <- sum(
  prices_extended_corrected$status == "imputed",
  na.rm = TRUE)

# -----------------------------------------------------------------------
# 12. Figures
# -----------------------------------------------------------------------

# 12a. City heterogeneity: distribution of inter-city lambda range
p_city_het <- lambda_city_shrunk %>%
  filter(apply_D, !use_pooled) %>%
  group_by(articulo, subclase_ipc) %>%
  filter(n_distinct(ciudad) == 3) %>%
  summarise(
    lambda_range = max(lambda_city) - min(lambda_city),
    lambda_sd    = sd(lambda_city),
    .groups = "drop"
  ) %>%
  ggplot(aes(x = lambda_range)) +
  geom_histogram(bins = 30, fill = "#2E5FA3",
                 color = "white", alpha = 0.85) +
  geom_vline(xintercept = c(0.2, 0.3), linetype = "dashed",
             color = "#C0392B", linewidth = 0.7) +
  annotate("text", x = 0.21, y = Inf, vjust = 1.5, hjust = 0,
           label = "0.2", color = "#C0392B", size = 3.5) +
  annotate("text", x = 0.31, y = Inf, vjust = 1.5, hjust = 0,
           label = "0.3", color = "#C0392B", size = 3.5) +
  labs(
    title    = "Inter-city lambda heterogeneity (items with 3 cities)",
    subtitle = paste0(
      sprintf("Mean range = %.3f | P75 = %.3f | ",
              mean(lambda_city_shrunk %>%
                     filter(apply_D, !use_pooled) %>%
                     group_by(articulo) %>%
                     filter(n_distinct(ciudad) == 3) %>%
                     summarise(r = max(lambda_city) - min(lambda_city),
                               .groups="drop") %>%
                     pull(r), na.rm = TRUE),
              quantile(lambda_city_shrunk %>%
                         filter(apply_D, !use_pooled) %>%
                         group_by(articulo) %>%
                         filter(n_distinct(ciudad) == 3) %>%
                         summarise(r = max(lambda_city) - min(lambda_city),
                                   .groups="drop") %>%
                         pull(r), 0.75, na.rm = TRUE)),
      sprintf("%.1f%% of items have range > 0.3",
              mean(lambda_city_shrunk %>%
                     filter(apply_D, !use_pooled) %>%
                     group_by(articulo) %>%
                     filter(n_distinct(ciudad) == 3) %>%
                     summarise(r = max(lambda_city) - min(lambda_city),
                               .groups="drop") %>%
                     pull(r) > 0.3, na.rm = TRUE) * 100)),
    x = "Range of lambda across cities (max - min)",
    y = "Count"
  ) +
  theme_bw(base_size = 12)

ggsave(file.path(out_dir, "fig_lambda_city_heterogeneity.png"),
       p_city_het, width = 8, height = 5, dpi = 200)

# 12b. Pooled vs city-specific shrunk lambda
p_pooled_vs_city <- lambda_city_shrunk %>%
  filter(apply_D, !is.na(lambda_shrunk_pooled),
         !is.na(lambda_final_city)) %>%
  ggplot(aes(x = lambda_shrunk_pooled,
             y = lambda_final_city,
             color = ciudad)) +
  geom_point(alpha = 0.6, size = 2.5) +
  geom_abline(slope = 1, intercept = 0,
              linetype = "dashed", color = "grey50",
              linewidth = 0.7) +
  geom_hline(yintercept = 1, linetype = "dotted",
             color = "#C0392B", linewidth = 0.5) +
  geom_vline(xintercept = 1, linetype = "dotted",
             color = "#C0392B", linewidth = 0.5) +
  scale_color_manual(
    values = c("BOGOTÁ D.C." = "#2E5FA3",
               "CALI"        = "#1A7A4A",
               "MEDELLÍN"    = "#C0392B")) +
  labs(
    title    = "Pooled lambda (05c) vs. city-specific lambda (05e)",
    subtitle = paste0(
      "Points off the diagonal = city heterogeneity matters.\n",
      "Dashed = no change. Red lines = BASE assumption."),
    x     = "lambda_shrunk_pooled (05c)",
    y     = "lambda_shrunk_city (05e)",
    color = "City"
  ) +
  theme_bw(base_size = 11)

ggsave(file.path(out_dir, "fig_lambda_pooled_vs_city.png"),
       p_pooled_vs_city, width = 8, height = 6, dpi = 200)

# 12c. MAPE by sub-class: three methods
p_mape <- subclass_val %>%
  mutate(
    subclase_ipc = fct_reorder(subclase_ipc, MAPE_BASE),
    flag = case_when(
      !apply_D               ~ "single-item (BASE)",
      imp_C_vs_BASE >  0     ~ "D_city improves",
      TRUE                   ~ "D_city worsens"
    )
  ) %>%
  ggplot(aes(y = subclase_ipc)) +
  geom_segment(
    aes(x = pmin(MAPE_D_shrunk, MAPE_D_shrunk_C),
        xend = MAPE_BASE,
        yend = subclase_ipc),
    color = "grey70", linewidth = 1.1) +
  geom_point(aes(x = MAPE_BASE),
             color = "#95A5A6", size = 3.5) +
  geom_point(aes(x = MAPE_D_shrunk),
             color = "#E67E22", size = 3.5, shape = 17) +
  geom_point(aes(x = MAPE_D_shrunk_C, color = flag),
             size = 3.5, shape = 15) +
  scale_color_manual(
    values = c("single-item (BASE)" = "#95A5A6",
               "D_city improves"    = "#1A7A4A",
               "D_city worsens"     = "#C0392B")) +
  geom_vline(xintercept = c(3, 5), linetype = "dashed",
             color = "grey50", linewidth = 0.5) +
  labs(
    title    = "MAPE by sub-class: BASE vs D_shrunk_pooled vs D_shrunk_city",
    subtitle = paste0(
      "Circle=BASE | Triangle=D_pooled (05c) | Square=D_city (05e)\n",
      "Green square = city heterogeneity helps | Red = it worsens"),
    x = "MAPE (%)", y = "CPI sub-class", color = NULL
  ) +
  theme_bw(base_size = 11) +
  theme(legend.position = "bottom",
        panel.grid.major.y = element_blank())

ggsave(file.path(out_dir, "fig_D4_mape_by_subclass.png"),
       p_mape, width = 10, height = 8, dpi = 200)

# -----------------------------------------------------------------------
# 13. Save outputs
# -----------------------------------------------------------------------
write_csv(lambda_city_shrunk,
          file.path(out_dir, "lambda_city.csv"))
write_csv(subclass_val,
          file.path(out_dir, "D4_validation_subclass.csv"))
write_csv(item_val,
          file.path(out_dir, "D4_validation_item.csv"))
write_csv(
  global_val %>% pivot_longer(everything(),
                              names_to = "metric", values_to = "value"),
  file.path(out_dir, "D4_validation_global.csv"))
write_csv(prices_extended_corrected,
          file.path(out_dir, "prices_extended_corrected_D4.csv"))

write_xlsx(
  list(
    lambda_city      = lambda_city_shrunk,
    global_all       = global_val %>%
      pivot_longer(everything(),
                   names_to = "metric", values_to = "value"),
    global_multiitem = global_multi %>%
      pivot_longer(everything(),
                   names_to = "metric", values_to = "value"),
    by_subclass      = subclass_val,
    by_item          = item_val
  ),
  file.path(out_dir, "D4_validation_results.xlsx")
)

saveRDS(prices_extended_corrected,
        file.path(out_dir, "prices_extended_corrected_D4.rds"))

# -----------------------------------------------------------------------
# 14. DECISION PRINT
# -----------------------------------------------------------------------
cat("\n")
cat(strrep("=", 72), "\n")
cat("  DECISION PRINT — Solution D + Improvements 2 + 4\n")
cat("  (JS shrinkage + city-heterogeneous lambda)\n")
cat(strrep("=", 72), "\n\n")

cat("── SHRINKAGE SUMMARY (city-specific) ──\n\n")
cat(sprintf("  Series with city-specific lambda:    %d\n",
            sum(!lambda_city_raw$use_pooled, na.rm = TRUE)))
cat(sprintf("  Series using pooled fallback:        %d\n",
            sum(lambda_city_raw$use_pooled, na.rm = TRUE)))
cat(sprintf("  Mean shrinkage factor (delta):       %.3f\n",
            shrinkage_summary$delta_mean))
cat(sprintf("  Mean |lambda_city-1| before:         %.3f\n",
            shrinkage_summary$mean_abs_raw))
cat(sprintf("  Mean |lambda_city-1| after:          %.3f  (%.1f%% reduced)\n",
            shrinkage_summary$mean_abs_shrunk,
            shrinkage_summary$shrinkage_pct))
cat(sprintf("  Mean shift vs pooled lambda:         %.3f\n",
            shrinkage_summary$mean_abs_shift_pooled))
cat(sprintf("  Series shifted >0.1 vs pooled:       %.1f%%\n\n",
            shrinkage_summary$pct_shifted_gt_0.1))

cat("── GLOBAL MAPE: ALL ITEMS ──\n\n")
cat(sprintf("  %-40s  %8s  %8s\n",
            "Method", "MAPE(%)", "Max APE(%)"))
cat(sprintf("  %-40s  %8s  %8s\n",
            strrep("-", 40), strrep("-", 8), strrep("-", 8)))
cat(sprintf("  %-40s  %8.3f  %8.3f\n",
            "BASE (current method)",
            global_val$MAPE_BASE, global_val$max_BASE))
cat(sprintf("  %-40s  %8.3f  %8.3f\n",
            "D_shrunk pooled (Improvement 2, 05c)",
            global_val$MAPE_D_shrunk, global_val$max_D_shrunk))
cat(sprintf("  %-40s  %8.3f  %8.3f\n",
            "D_shrunk city (Impr. 2+4, this script)",
            global_val$MAPE_D_shrunk_C, global_val$max_D_shrunk_C))
cat("\n")
cat(sprintf("  D_city vs BASE:     %+.4f pp  (%s)\n",
            global_val$imp_C_vs_BASE,
            if (global_val$imp_C_vs_BASE > 0)
              "IMPROVEMENT" else "DETERIORATION"))
cat(sprintf("  D_city vs pooled:   %+.4f pp  (%s)\n",
            global_val$imp_C_vs_pooled,
            if (global_val$imp_C_vs_pooled > 0)
              "city > pooled" else "pooled >= city"))
cat(sprintf("  D_city beats BASE in %.1f%% of obs\n\n",
            global_val$pct_C_beats_BASE))

cat("── GLOBAL MAPE: MULTI-ITEM SUB-CLASSES ONLY ──\n\n")
cat(sprintf("  %-40s  %8s\n", "Method", "MAPE(%)"))
cat(sprintf("  %-40s  %8s\n", strrep("-", 40), strrep("-", 8)))
cat(sprintf("  %-40s  %8.3f\n",
            "BASE", global_multi$MAPE_BASE))
cat(sprintf("  %-40s  %8.3f\n",
            "D_shrunk pooled (05c)",
            global_multi$MAPE_D_shrunk))
cat(sprintf("  %-40s  %8.3f\n",
            "D_shrunk city (05e)",
            global_multi$MAPE_D_shrunk_C))
cat(sprintf("\n  imp city vs BASE (multi):    %+.4f pp\n",
            global_multi$imp_C_vs_BASE))
cat(sprintf("  imp city vs pooled (multi):  %+.4f pp\n\n",
            global_multi$imp_C_vs_pooled))

cat("── SUB-CLASS BREAKDOWN ──\n\n")
cat(sprintf("  %-12s  %5s  %10s  %13s  %11s  %10s  %10s\n",
            "sub-class", "k", "BASE(%)",
            "D_shrunk(%)", "D_city(%)",
            "imp_C(pp)", "best"))
cat(sprintf("  %s\n", strrep("-", 80)))
for (i in seq_len(nrow(subclass_val))) {
  r   <- subclass_val[i, ]
  sym <- if (!r$apply_D)             "  —"
  else if (r$imp_C_vs_BASE > 0) "  ✓"
  else                           "  ✗"
  cat(sprintf(
    "  %-12s  %5d  %10.3f  %13.3f  %11.3f  %+10.3f  %-10s%s\n",
    r$subclase_ipc, r$n_items,
    r$MAPE_BASE, r$MAPE_D_shrunk, r$MAPE_D_shrunk_C,
    r$imp_C_vs_BASE, r$best_method, sym))
}
cat("\n")

cat("── COVERAGE ──\n\n")
cat(sprintf("  Rows corrected / total imputed:  %d / %d (%.1f%%)\n\n",
            n_corrected, n_imputed,
            n_corrected / n_imputed * 100))

cat("── DECISION GUIDE ──\n\n")
imp_g <- global_val$imp_C_vs_BASE
imp_m <- global_multi$imp_C_vs_BASE
imp_p <- global_val$imp_C_vs_pooled

if (imp_g > 0 & imp_m > 0 & imp_p > 0) {
  cat("  [FULL IMPROVEMENT] D_city beats BASE and D_pooled\n")
  cat("  both globally and within multi-item sub-classes.\n\n")
  if (imp_g >= 0.5) {
    cat("  >> Improvement is meaningful (>=0.5 pp globally).\n")
    cat("  >> Use prices_extended_corrected_D4 as final price series.\n")
    cat("  >> This is the recommended series for the LP models.\n")
  } else {
    cat("  >> Improvement is small (<0.5 pp globally).\n")
    cat("  >> Use D4 if you want the most refined estimate.\n")
    cat("  >> D2 (05c) is also defensible given the small gap.\n")
  }
} else if (imp_m > 0 & imp_p > 0) {
  cat("  [MULTI-ITEM IMPROVEMENT] D_city helps within multi-item\n")
  cat("  sub-classes and over pooled shrinkage, but not globally.\n\n")
  cat("  Single-item sub-classes use BASE by construction, so\n")
  cat("  the global average is diluted. The relevant comparison\n")
  cat("  is the multi-item MAPE above.\n\n")
  cat("  >> Use prices_extended_corrected_D4 as final series.\n")
} else if (imp_m > 0 & imp_p <= 0) {
  cat("  [MIXED] D_city helps vs BASE in multi-item but NOT\n")
  cat("  over pooled shrinkage. City heterogeneity adds noise\n")
  cat("  rather than signal once JS shrinkage is applied.\n\n")
  cat("  >> Use prices_extended_corrected_D2 (05c) as final series.\n")
  cat("  >> City-specific estimation does not add value over pooling.\n")
} else {
  cat("  [NO IMPROVEMENT] D_city does not improve over BASE\n")
  cat("  or pooled shrinkage in multi-item sub-classes.\n\n")
  cat("  >> Check fig_lambda_pooled_vs_city.png:\n")
  cat("     if points cluster on the diagonal, city heterogeneity\n")
  cat("     is small despite the diagnostic suggesting otherwise.\n")
  cat("  >> Use prices_extended_corrected_D2 (05c) as final series.\n")
}

cat("\n")
cat(strrep("=", 72), "\n")
cat("  Key output: prices_extended_corrected_D4.csv\n")
cat("  All files saved to: ", out_dir, "\n")
cat(strrep("=", 72), "\n\n")