########################################################
## FINAL PRICE SERIES — S5 only
## BASE + D_city (JS shrinkage + R² + lambda bounds)
##
## Rule:
##   k = 1  →  BASE: p_hat = anchor × (IPC_t / IPC_t0)
##   k > 1  →  D_city: p_hat = anchor × (IPC_t / IPC_t0)^λ*
##             where λ* = JS-shrunk OLS lambda, subject to:
##               R² >= 0.10
##               λ* ∈ [0.20, 4.00]
##             Items failing either condition → BASE
##
## Output folder:
##   .../food-security-paper/output/price_forecasting/
##     prices_final.csv / .rds   ← main output
##     lambda_estimates.csv      ← lambda per item × city
##
##   .../food-security-paper/robust/
##     final_validation_global.csv
##     final_validation_subclass.csv
##     final_validation_item.csv
##     final_validation_results.xlsx
##     validation_plots/         ← time series plots
##     fig_final_mape_by_subclass.png
##     fig_lambda_distribution.png
##     fig_validation_overview.png
########################################################

# -----------------------------------------------------------------------
# 0. Packages
# -----------------------------------------------------------------------
library(tidyverse)
library(readxl)
library(janitor)
library(lubridate)
library(writexl)

# -----------------------------------------------------------------------
# 1. Directories
# -----------------------------------------------------------------------
dirs <- c(
  "C:\\Users\\sergio.barona\\Desktop\\Least-cost-diets-and-affordability\\Proyecto Interno",
  "C:/Users/Portatil/Desktop/Least-cost-diets-and-affordability/Proyecto Interno",
  "C:/Users/danie/OneDrive/Escritorio/Least-cost-diets-and-affordability/Proyecto Interno"
)
base_dir <- dirs[dir.exists(dirs)][1]
if (is.na(base_dir)) stop("Ninguno de los directorios base existe.")

in_prices <- file.path(base_dir,
                       "Precios DANE/OUTPUT_DANE/precios_unadj_DANE_1999_2018.xlsx")
in_ipc    <- file.path(base_dir, "var-ipc/IPC.xls")
in_ipc2   <- file.path(base_dir, "var-ipc/IPC_2.xls")
in_ipc3   <- file.path(base_dir, "var-ipc/IPC_3.xls")
in_ipc4   <- file.path(base_dir, "var-ipc/IPC_4.xls")
in_corr1  <- file.path(base_dir, "var-ipc/correlativa_ipc.xlsx")
in_corr2  <- file.path(base_dir, "var-ipc/correlativa_ipc_articulos.xlsx")
in_prices_extended <- file.path(base_dir,
                                "food-security-paper/output/forecasting_fullsample/prices_extended_city_article_month.csv")

# Output directories
out_forecast <- file.path(base_dir,
                          "food-security-paper/output/price_forecasting")
robust_dir   <- file.path(base_dir, "food-security-paper/robust")
val_plot_dir <- file.path(robust_dir, "validation_plots")

dir.create(out_forecast, recursive = TRUE, showWarnings = FALSE)
dir.create(robust_dir,   recursive = TRUE, showWarnings = FALSE)
dir.create(val_plot_dir, recursive = TRUE, showWarnings = FALSE)

# -----------------------------------------------------------------------
# 2. Parameters
# -----------------------------------------------------------------------
TRAIN_END    <- as.Date("2015-12-01")
T0           <- as.Date("2016-01-01")
VAL_END      <- as.Date("2018-03-01")
EXTRAP_START <- as.Date("2018-04-01")
cities_use   <- c("CALI", "BOGOTÁ D.C.", "MEDELLÍN")
MIN_OBS      <- 24L

# Validity bounds (S5)
R2_MIN  <- 0.10
LAM_MIN <- 0.20
LAM_MAX <- 4.00

# Colours
COL_OBS   <- "#2C3E50"
COL_BASE  <- "#95A5A6"
COL_DCITY <- "#E74C3C"
COL_VLINE <- "#2E5FA3"

city_labels <- c(
  "BOGOTÁ D.C." = "Bogotá",
  "CALI"        = "Cali",
  "MEDELLÍN"    = "Medellín"
)

# -----------------------------------------------------------------------
# 3. Auxiliary functions
# -----------------------------------------------------------------------
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
    r2     = summary(fit)$r.squared,
    n_obs  = nrow(merged)
  )
}

safe_name <- function(x) {
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  x <- gsub("[^A-Za-z0-9_]+", "_", x)
  x <- gsub("_+", "_", x)
  x <- gsub("^_|_$", "", x)
  x
}

# -----------------------------------------------------------------------
# 4. Load data
# -----------------------------------------------------------------------
message("Step 1/9 — Loading data...")

dane <- read_excel(in_prices) %>%
  mutate(
    fecha        = make_date(ano, mes_num),
    cod_subclase = paste0("0", substr(codigo_articulo, 1, 6), "0")
  ) %>%
  filter(!is.na(fecha), !is.na(precio_500g),
         nombre_ciudad %in% cities_use)

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

corr_subclase <- read_excel(in_corr1) %>%
  fill(subclase, ipc, .direction = "down") %>%
  mutate(cod_subclase = paste0("0", gasto_basico, "00")) %>%
  select(cod_subclase, subclase) %>%
  mutate(across(everything(), as.character))

corr_producto <- read_excel(in_corr2) %>%
  rename(codigo_articulo = cod_dane, subclase = cod_ipc) %>%
  mutate(across(everything(), as.character))

prices_extended <- read_csv(in_prices_extended,
                            show_col_types = FALSE) %>%
  mutate(fecha = as.Date(fecha))

message(sprintf(
  "  DANE: %d rows | IPC: %d rows | Extended: %d rows",
  nrow(dane), nrow(ipc), nrow(prices_extended)))

# -----------------------------------------------------------------------
# 5. Sub-class assignment and sizes
# -----------------------------------------------------------------------
message("Step 2/9 — Assigning sub-classes...")

item_subclass_map <- dane %>%
  filter(fecha <= TRAIN_END) %>%
  mutate(codigo_articulo = as.character(codigo_articulo),
         cod_subclase    = as.character(cod_subclase)) %>%
  group_by(nombre_ciudad, articulo) %>%
  group_modify(~ {
    sub_info <- assign_subclase(.x, corr_subclase, corr_producto)
    tibble(subclase_ipc = sub_info$subclase)
  }) %>%
  ungroup() %>%
  filter(!is.na(subclase_ipc))

subclass_sizes <- item_subclass_map %>%
  group_by(nombre_ciudad, subclase_ipc) %>%
  summarise(k = n_distinct(articulo), .groups = "drop")

prices_at_T0 <- dane %>%
  filter(fecha == T0) %>%
  select(nombre_ciudad, articulo, precio_500g) %>%
  rename(precio_anchor = precio_500g)

valid_items <- prices_at_T0 %>% distinct(nombre_ciudad, articulo)

message(sprintf("  %d item × city series | sub-class sizes: %s",
                nrow(valid_items),
                paste(sort(unique(subclass_sizes$k)), collapse = ", ")))

# -----------------------------------------------------------------------
# 6. Estimate city-specific lambdas (OLS, first differences)
# -----------------------------------------------------------------------
message("Step 3/9 — Estimating lambdas (OLS, 1999–2015)...")

all_lambda_raw <- list()

for (i in seq_len(nrow(valid_items))) {
  
  city_name <- valid_items$nombre_ciudad[i]
  food_name <- valid_items$articulo[i]
  
  sub_row      <- item_subclass_map %>%
    filter(nombre_ciudad == city_name, articulo == food_name)
  subclase_ipc <- if (nrow(sub_row) > 0) sub_row$subclase_ipc[1] else NA
  
  k_val <- subclass_sizes %>%
    filter(nombre_ciudad == city_name,
           subclase_ipc  == subclase_ipc) %>%
    pull(k)
  k_val <- if (length(k_val) > 0) k_val[1] else 1L
  
  if (k_val == 1) {
    all_lambda_raw[[length(all_lambda_raw) + 1]] <- tibble(
      ciudad = city_name, articulo = food_name,
      subclase_ipc = subclase_ipc, k = 1L,
      apply_D = FALSE, lambda = 1.0,
      sigma2 = NA_real_, r2 = NA_real_,
      n_obs = NA_integer_, est_ok = TRUE)
    next
  }
  
  df_obs <- dane %>%
    filter(nombre_ciudad == city_name, articulo == food_name,
           fecha <= TRAIN_END, !is.na(precio_500g)) %>%
    mutate(codigo_articulo = as.character(codigo_articulo),
           cod_subclase    = as.character(cod_subclase)) %>%
    arrange(fecha)
  
  if (nrow(df_obs) == 0 || is.na(subclase_ipc)) next
  
  subclase_code <- substr(paste0(subclase_ipc, "00"), 1, 8)
  
  df_ipc <- ipc %>%
    filter(ciudad == city_name, cod_subclase == subclase_code,
           fecha <= TRAIN_END) %>%
    select(fecha, ipc) %>% arrange(fecha)
  
  if (nrow(df_ipc) == 0) next
  
  est <- estimate_lambda_fd(df_obs, df_ipc)
  
  all_lambda_raw[[length(all_lambda_raw) + 1]] <- tibble(
    ciudad = city_name, articulo = food_name,
    subclase_ipc = subclase_ipc, k = k_val,
    apply_D = TRUE,
    lambda  = if (!is.null(est)) est$lambda else NA_real_,
    sigma2  = if (!is.null(est)) est$sigma2 else NA_real_,
    r2      = if (!is.null(est)) est$r2     else NA_real_,
    n_obs   = if (!is.null(est)) est$n_obs  else NA_integer_,
    est_ok  = !is.null(est))
}

lambda_raw_df <- bind_rows(all_lambda_raw)

# -----------------------------------------------------------------------
# 7. James-Stein shrinkage + validity bounds (S5)
# -----------------------------------------------------------------------
message("Step 4/9 — JS shrinkage + validity bounds...")
message(sprintf("  Bounds: R²>=%.2f | λ∈[%.2f, %.2f]",
                R2_MIN, LAM_MIN, LAM_MAX))

subclass_js <- lambda_raw_df %>%
  filter(apply_D, est_ok, !is.na(lambda)) %>%
  group_by(subclase_ipc, ciudad) %>%
  summarise(
    k_city    = n_distinct(articulo),
    sigma2_sc = mean(sigma2, na.rm = TRUE),
    Q_sc      = sum((lambda - 1)^2, na.rm = TRUE),
    .groups   = "drop")

lambda_final_df <- lambda_raw_df %>%
  left_join(subclass_js, by = c("subclase_ipc", "ciudad")) %>%
  mutate(
    # JS shrinkage factor
    delta = case_when(
      !apply_D | !est_ok  ~ NA_real_,
      k == 1              ~ 1.0,
      k >= 3 ~ pmax(0, 1 - (k_city - 2) * sigma2_sc / Q_sc),
      k == 2 ~ {
        dev <- (lambda - 1)^2
        if_else((dev + sigma2) > 0, dev / (dev + sigma2), 0)
      },
      TRUE ~ NA_real_),
    
    # Shrunk lambda
    lambda_shrunk = case_when(
      !apply_D                  ~ 1.0,
      !est_ok | is.na(lambda)   ~ 1.0,
      TRUE                      ~ 1 + delta * (lambda - 1)),
    lambda_shrunk = if_else(
      !is.finite(lambda_shrunk), 1.0, lambda_shrunk),
    
    # Validity bounds → reversion reason
    reversion_reason = case_when(
      !apply_D                  ~ "single_item",
      !est_ok | is.na(lambda)   ~ "est_failed",
      r2 < R2_MIN               ~ "low_R2",
      lambda_shrunk < LAM_MIN   ~ "low_lambda",
      lambda_shrunk > LAM_MAX   ~ "high_lambda",
      TRUE                      ~ "none"),
    
    # Final lambda (S5)
    lambda_final = if_else(
      reversion_reason == "none", lambda_shrunk, 1.0),
    
    # Method label
    method_label = case_when(
      !apply_D                          ~ "BASE",
      reversion_reason == "est_failed"  ~ "BASE_fallback",
      reversion_reason == "low_R2"      ~ "BASE_lowR2",
      reversion_reason == "low_lambda"  ~ "BASE_lowlambda",
      reversion_reason == "high_lambda" ~ "BASE_highlambda",
      TRUE                              ~ "D_city")
  )

# Diagnostics
shrink_diag <- lambda_final_df %>%
  filter(apply_D, est_ok, !is.na(lambda)) %>%
  summarise(
    n               = n(),
    delta_mean      = mean(delta,                 na.rm = TRUE),
    mean_abs_raw    = mean(abs(lambda - 1),       na.rm = TRUE),
    mean_abs_final  = mean(abs(lambda_final - 1), na.rm = TRUE),
    pct_shrinkage   = (1 - mean_abs_final / mean_abs_raw) * 100)

n_low_r2     <- sum(lambda_final_df$reversion_reason == "low_R2",    na.rm=TRUE)
n_low_lam    <- sum(lambda_final_df$reversion_reason == "low_lambda", na.rm=TRUE)
n_high_lam   <- sum(lambda_final_df$reversion_reason == "high_lambda",na.rm=TRUE)
n_dcity      <- sum(lambda_final_df$method_label == "D_city",        na.rm=TRUE)

message(sprintf(
  "  |λ-1|: %.3f → %.3f (%.1f%% shrinkage, mean δ=%.3f)",
  shrink_diag$mean_abs_raw, shrink_diag$mean_abs_final,
  shrink_diag$pct_shrinkage, shrink_diag$delta_mean))
message(sprintf(
  "  Reversions: low R²=%d | low λ=%d | high λ=%d | D_city kept=%d",
  n_low_r2, n_low_lam, n_high_lam, n_dcity))

# Save lambda estimates
write_csv(lambda_final_df,
          file.path(out_forecast, "lambda_estimates.csv"))
write_csv(lambda_final_df,
          file.path(robust_dir,   "lambda_city.csv"))

# -----------------------------------------------------------------------
# 8. Build price predictions
# -----------------------------------------------------------------------
message("Step 5/9 — Building price predictions...")

all_series <- list()

for (i in seq_len(nrow(valid_items))) {
  
  city_name <- valid_items$nombre_ciudad[i]
  food_name <- valid_items$articulo[i]
  
  lam_row <- lambda_final_df %>%
    filter(ciudad == city_name, articulo == food_name)
  if (nrow(lam_row) == 0) next
  
  lambda_use   <- lam_row$lambda_final[1]
  method_use   <- lam_row$method_label[1]
  subclase_ipc <- lam_row$subclase_ipc[1]
  k_val        <- lam_row$k[1]
  r2_val       <- lam_row$r2[1]
  
  if (!is.finite(lambda_use)) lambda_use <- 1.0
  if (is.na(subclase_ipc)) next
  
  subclase_code <- substr(paste0(subclase_ipc, "00"), 1, 8)
  
  anchor <- prices_at_T0 %>%
    filter(nombre_ciudad == city_name, articulo == food_name) %>%
    pull(precio_anchor)
  if (length(anchor) == 0) next
  anchor <- anchor[1]
  
  df_ipc <- ipc %>%
    filter(ciudad == city_name, cod_subclase == subclase_code) %>%
    select(fecha, ipc) %>% arrange(fecha)
  if (nrow(df_ipc) == 0) next
  
  ipc_t0 <- df_ipc %>% filter(fecha == T0) %>% pull(ipc)
  if (length(ipc_t0) == 0 || is.na(ipc_t0)) next
  ipc_t0 <- ipc_t0[1]
  
  # Observed prices (for validation APE)
  obs_val <- dane %>%
    filter(nombre_ciudad == city_name, articulo == food_name,
           fecha >= T0, fecha <= VAL_END, !is.na(precio_500g)) %>%
    select(fecha, price_obs = precio_500g)
  
  full_pred <- df_ipc %>%
    filter(fecha >= T0) %>%
    mutate(
      ciudad       = city_name,
      articulo     = food_name,
      subclase_ipc = subclase_ipc,
      k            = k_val,
      method       = method_use,
      lambda       = lambda_use,
      r2_train     = r2_val,
      ipc_ratio    = ipc / ipc_t0,
      price_BASE   = anchor * ipc_ratio,
      price_final  = anchor * ipc_ratio ^ lambda_use,
      period       = case_when(
        fecha <= VAL_END      ~ "validation",
        fecha >= EXTRAP_START ~ "extrapolation",
        TRUE                  ~ "other")
    ) %>%
    left_join(obs_val, by = "fecha")
  
  all_series[[length(all_series) + 1]] <- full_pred
}

series_df <- bind_rows(all_series)

obs_full_df <- dane %>%
  filter(nombre_ciudad %in% cities_use, !is.na(precio_500g)) %>%
  select(ciudad = nombre_ciudad, articulo, fecha,
         price_obs = precio_500g)

message(sprintf("  %d item × city × month rows.", nrow(series_df)))

# Save validation panel for Script 09 (CoNA validation)
series_df %>%
  filter(period == "validation") %>%
  select(ciudad, articulo, fecha,
         price_BASE, price_final, method) %>%
  write_csv(file.path(out_forecast, "prices_validation_panel.csv"))
message(sprintf("  Saved prices_validation_panel.csv -> %s", out_forecast))

# -----------------------------------------------------------------------
# 9. Validation APEs (Jan 2016 – Mar 2018)
# -----------------------------------------------------------------------
message("Step 6/9 — Computing validation APEs...")

val_ape <- series_df %>%
  filter(period == "validation", !is.na(price_obs)) %>%
  mutate(
    ape_BASE  = abs((price_BASE  - price_obs) / price_obs) * 100,
    ape_final = abs((price_final - price_obs) / price_obs) * 100)

global_val <- val_ape %>%
  summarise(
    n_obs                = n(),
    MAPE_BASE            = mean(ape_BASE,  na.rm = TRUE),
    MAPE_final           = mean(ape_final, na.rm = TRUE),
    max_BASE             = max(ape_BASE,   na.rm = TRUE),
    max_final            = max(ape_final,  na.rm = TRUE),
    pct_final_beats_BASE = mean(ape_final < ape_BASE, na.rm = TRUE)*100,
    imp_vs_BASE          = MAPE_BASE - MAPE_final)

global_by_method <- val_ape %>%
  group_by(method) %>%
  summarise(
    n_series   = n_distinct(paste(ciudad, articulo)),
    n_obs      = n(),
    MAPE_BASE  = mean(ape_BASE,  na.rm = TRUE),
    MAPE_final = mean(ape_final, na.rm = TRUE),
    imp        = MAPE_BASE - MAPE_final,
    .groups    = "drop") %>%
  arrange(method)

subclass_val <- val_ape %>%
  group_by(subclase_ipc, k, method) %>%
  summarise(
    n_items    = n_distinct(articulo),
    n_obs      = n(),
    MAPE_BASE  = mean(ape_BASE,  na.rm = TRUE),
    MAPE_final = mean(ape_final, na.rm = TRUE),
    imp        = MAPE_BASE - MAPE_final,
    .groups    = "drop") %>%
  arrange(desc(MAPE_BASE))

item_val <- val_ape %>%
  group_by(ciudad, articulo, subclase_ipc,
           k, method, lambda, r2_train) %>%
  summarise(
    n_months   = n(),
    MAPE_BASE  = mean(ape_BASE,  na.rm = TRUE),
    MAPE_final = mean(ape_final, na.rm = TRUE),
    imp        = MAPE_BASE - MAPE_final,
    .groups    = "drop") %>%
  arrange(desc(MAPE_BASE))

# -----------------------------------------------------------------------
# 10. Time series validation plots
# -----------------------------------------------------------------------
message("Step 7/9 — Building validation plots...")

subclass_city_combos <- series_df %>%
  filter(period == "validation") %>%
  distinct(subclase_ipc, ciudad, k, method)

message(sprintf("  Generating %d sub-class × city plots...",
                nrow(subclass_city_combos)))

for (i in seq_len(nrow(subclass_city_combos))) {
  
  sub_ipc   <- subclass_city_combos$subclase_ipc[i]
  city_name <- subclass_city_combos$ciudad[i]
  k_val     <- subclass_city_combos$k[i]
  method_i  <- subclass_city_combos$method[i]
  
  items_here <- series_df %>%
    filter(subclase_ipc == sub_ipc, ciudad == city_name) %>%
    pull(articulo) %>% unique() %>% sort()
  if (length(items_here) == 0) next
  
  pred_here <- series_df %>%
    filter(subclase_ipc == sub_ipc, ciudad == city_name,
           period == "validation") %>%
    select(articulo, fecha, price_BASE, price_final,
           lambda, method, r2_train)
  
  obs_here <- obs_full_df %>%
    filter(ciudad == city_name, articulo %in% items_here)
  
  mape_here <- item_val %>%
    filter(subclase_ipc == sub_ipc, ciudad == city_name) %>%
    select(articulo, lambda, r2_train,
           MAPE_BASE, MAPE_final, method)
  
  item_subtitles <- mape_here %>%
    mutate(
      revert_note = case_when(
        method == "BASE_lowR2"      ~ " [low R²]",
        method == "BASE_lowlambda"  ~ " [low λ]",
        method == "BASE_highlambda" ~ " [high λ]",
        method == "BASE_fallback"   ~ " [est. failed]",
        TRUE                        ~ ""
      ),
      lbl = sprintf(
        "λ=%.3f | R²=%.3f | BASE=%.1f%% → %s=%.1f%%%s",
        lambda,
        if_else(is.na(r2_train), 0, r2_train),
        MAPE_BASE,
        if_else(method == "D_city", "D_city", "BASE"),
        MAPE_final,
        revert_note)
    ) %>%
    select(articulo, lbl)
  
  pred_labeled <- pred_here %>%
    left_join(item_subtitles, by = "articulo") %>%
    mutate(item_label = if_else(!is.na(lbl),
                                paste0(articulo, "\n", lbl),
                                articulo))
  
  obs_labeled <- obs_here %>%
    left_join(pred_labeled %>% distinct(articulo, item_label),
              by = "articulo")
  
  pred_long <- pred_labeled %>%
    pivot_longer(cols = c(price_BASE, price_final),
                 names_to = "pred_method", values_to = "price_pred") %>%
    mutate(pred_label = case_when(
      pred_method == "price_BASE" ~ "BASE (λ=1)",
      method == "D_city"          ~ "D_city (JS λ)",
      TRUE                        ~ "Final (=BASE)"
    ))
  
  # Hide redundant D_city line when method is not D_city
  if (method_i != "D_city") {
    pred_long <- pred_long %>% filter(pred_method == "price_BASE")
  }
  
  city_lbl <- city_labels[city_name]
  if (is.na(city_lbl)) city_lbl <- city_name
  
  n_items <- length(items_here)
  ncols   <- min(3L, n_items)
  nrows   <- ceiling(n_items / ncols)
  fig_h   <- max(4, 3.5 * nrows)
  fig_w   <- min(14, 4.5 * ncols)
  
  p <- ggplot() +
    annotate("rect",
             xmin = as.Date("1999-01-01"), xmax = TRAIN_END,
             ymin = -Inf, ymax = Inf,
             fill = "#F0F0F0", alpha = 0.6) +
    annotate("rect",
             xmin = T0, xmax = VAL_END,
             ymin = -Inf, ymax = Inf,
             fill = "#D6EAF8", alpha = 0.6) +
    geom_vline(xintercept = T0,
               linetype = "solid", color = COL_VLINE,
               linewidth = 0.6) +
    geom_vline(xintercept = VAL_END,
               linetype = "solid", color = COL_VLINE,
               linewidth = 0.5, alpha = 0.6) +
    geom_line(data = obs_labeled %>% filter(fecha < T0),
              aes(x = fecha, y = price_obs, group = item_label),
              color = COL_OBS, linewidth = 0.4, alpha = 0.4) +
    geom_line(data = obs_labeled %>%
                filter(fecha >= T0, fecha <= VAL_END),
              aes(x = fecha, y = price_obs, group = item_label),
              color = COL_OBS, linewidth = 1.0) +
    geom_point(data = obs_labeled %>%
                 filter(fecha >= T0, fecha <= VAL_END),
               aes(x = fecha, y = price_obs),
               color = COL_OBS, size = 1.2, alpha = 0.8) +
    geom_line(data = pred_long %>% filter(pred_method == "price_BASE"),
              aes(x = fecha, y = price_pred, group = item_label),
              color = COL_BASE, linewidth = 0.8, linetype = "dashed") +
    {if (method_i == "D_city")
      geom_line(data = pred_long %>%
                  filter(pred_method == "price_final"),
                aes(x = fecha, y = price_pred, group = item_label),
                color = COL_DCITY, linewidth = 0.8, linetype = "dashed")
    } +
    facet_wrap(~ item_label, scales = "free_y", ncol = ncols) +
    scale_x_date(date_breaks = "6 months",
                 date_labels = "%b\n%Y",
                 limits = c(as.Date("2013-01-01"), VAL_END)) +
    labs(
      title    = sprintf(
        "Price validation — Sub-class %s | %s | k=%d | %s",
        sub_ipc, city_lbl, k_val, method_i),
      subtitle = paste0(
        "Grey = training (pre-2016) | Blue = validation (Jan 2016–Mar 2018)\n",
        "Black = observed | Grey dashed = BASE | Red dashed = D_city\n",
        sprintf("R²≥%.2f | λ∈[%.2f,%.2f] — items outside → BASE",
                R2_MIN, LAM_MIN, LAM_MAX)),
      x = NULL, y = "Price (COP per 500g)"
    ) +
    theme_bw(base_size = 9.5) +
    theme(
      strip.text       = element_text(size = 7, face = "bold"),
      strip.background = element_rect(fill = "#EBF5FB"),
      panel.grid.minor = element_blank(),
      axis.text.x      = element_text(size = 7),
      plot.title       = element_text(size = 10, face = "bold"),
      plot.subtitle    = element_text(size = 7.5, color = "grey40"),
      legend.position  = "none")
  
  ggsave(
    file.path(val_plot_dir,
              sprintf("ts_%s_%s.png", sub_ipc, safe_name(city_name))),
    p, width = fig_w, height = fig_h, dpi = 180, bg = "white")
}

message(sprintf("  Saved %d plots.", nrow(subclass_city_combos)))

# -----------------------------------------------------------------------
# 11. Overview figure — top 12 worst items by MAPE_BASE
# -----------------------------------------------------------------------
message("  Building overview figure...")

top_worst <- item_val %>%
  filter(method == "D_city") %>%
  slice_max(MAPE_BASE, n = 12, with_ties = FALSE)

if (nrow(top_worst) > 0) {
  
  worst_pred <- series_df %>%
    inner_join(top_worst %>% select(ciudad, articulo),
               by = c("ciudad", "articulo")) %>%
    filter(period == "validation") %>%
    left_join(
      top_worst %>%
        select(ciudad, articulo,
               tw_lambda    = lambda,
               tw_r2        = r2_train,
               tw_MAPE_BASE = MAPE_BASE,
               tw_MAPE_final= MAPE_final),
      by = c("ciudad", "articulo")
    ) %>%
    mutate(item_label = paste0(
      articulo, " | ", city_labels[ciudad], "\n",
      sprintf("λ=%.2f | R²=%.2f | BASE=%.1f%% → D=%.1f%%",
              tw_lambda,
              if_else(is.na(tw_r2), 0, tw_r2),
              tw_MAPE_BASE, tw_MAPE_final)))
  
  worst_obs <- obs_full_df %>%
    inner_join(top_worst %>% select(ciudad, articulo),
               by = c("ciudad", "articulo")) %>%
    filter(fecha >= T0, fecha <= VAL_END) %>%
    left_join(worst_pred %>% distinct(ciudad, articulo, item_label),
              by = c("ciudad", "articulo"))
  
  p_overview <- ggplot() +
    annotate("rect", xmin = T0, xmax = VAL_END,
             ymin = -Inf, ymax = Inf,
             fill = "#D6EAF8", alpha = 0.5) +
    geom_vline(xintercept = T0, color = COL_VLINE, linewidth = 0.5) +
    geom_line(data = worst_pred,
              aes(x = fecha, y = price_BASE, group = item_label),
              color = COL_BASE, linewidth = 0.7, linetype = "dashed") +
    geom_line(data = worst_pred,
              aes(x = fecha, y = price_final, group = item_label),
              color = COL_DCITY, linewidth = 0.7, linetype = "dashed") +
    geom_line(data = worst_obs,
              aes(x = fecha, y = price_obs, group = item_label),
              color = COL_OBS, linewidth = 0.9) +
    geom_point(data = worst_obs,
               aes(x = fecha, y = price_obs),
               color = COL_OBS, size = 0.9) +
    facet_wrap(~ item_label, scales = "free_y", ncol = 3) +
    scale_x_date(date_breaks = "6 months", date_labels = "%b\n%Y") +
    labs(
      title    = "Overview — Top 12 D_city items by MAPE_BASE",
      subtitle = paste0(
        "Blue = validation (Jan 2016–Mar 2018) | ",
        "Black = observed | Grey dashed = BASE | Red dashed = D_city"),
      x = NULL, y = "Price (COP per 500g)"
    ) +
    theme_bw(base_size = 8.5) +
    theme(
      strip.text       = element_text(size = 6, face = "bold"),
      strip.background = element_rect(fill = "#EBF5FB"),
      panel.grid.minor = element_blank(),
      plot.title       = element_text(size = 10, face = "bold"),
      plot.subtitle    = element_text(size = 7.5, color = "grey40"))
  
  ggsave(file.path(robust_dir, "fig_validation_overview.png"),
         p_overview, width = 14, height = 14, dpi = 180, bg = "white")
  message("  Saved fig_validation_overview.png")
}

# -----------------------------------------------------------------------
# 12. Summary figures
# -----------------------------------------------------------------------
message("Step 8/9 — Building summary figures...")

p_lambda <- lambda_final_df %>%
  filter(apply_D, est_ok, method_label == "D_city") %>%
  mutate(ciudad_lbl = city_labels[ciudad]) %>%
  ggplot(aes(x = lambda_final, fill = ciudad_lbl)) +
  geom_histogram(bins = 30, alpha = 0.75,
                 position = "identity", color = "white") +
  geom_vline(xintercept = 1, linetype = "dashed",
             color = "#C0392B", linewidth = 0.8) +
  geom_vline(xintercept = c(LAM_MIN, LAM_MAX),
             linetype = "dotted", color = "grey40", linewidth = 0.6) +
  scale_fill_manual(
    values = c("Bogotá"   = "#2E5FA3",
               "Cali"     = "#1A7A4A",
               "Medellín" = "#C0392B")) +
  labs(
    title    = "JS-shrunk lambda — D_city items (after bounds)",
    subtitle = sprintf(
      "Mean=%.3f | SD=%.3f | |λ-1| reduced %.1f%% via shrinkage",
      mean(lambda_final_df$lambda_final[
        lambda_final_df$method_label == "D_city"], na.rm=TRUE),
      sd(lambda_final_df$lambda_final[
        lambda_final_df$method_label == "D_city"], na.rm=TRUE),
      shrink_diag$pct_shrinkage),
    x = "Lambda (JS-shrunk)", y = "Count", fill = "City"
  ) +
  theme_bw(base_size = 11) +
  theme(legend.position = "bottom")

ggsave(file.path(robust_dir, "fig_lambda_distribution.png"),
       p_lambda, width = 8, height = 5, dpi = 200)

p_mape <- subclass_val %>%
  mutate(
    subclase_ipc = fct_reorder(subclase_ipc, MAPE_BASE),
    flag = case_when(
      str_starts(method, "BASE") ~ paste0("BASE (", method, ")"),
      imp > 0                    ~ "D_city improves",
      TRUE                       ~ "D_city worsens")
  ) %>%
  ggplot(aes(y = subclase_ipc)) +
  geom_segment(
    aes(x = pmin(MAPE_BASE, MAPE_final),
        xend = pmax(MAPE_BASE, MAPE_final),
        yend = subclase_ipc),
    color = "grey70", linewidth = 1.0) +
  geom_point(aes(x = MAPE_BASE), color = "#95A5A6", size = 3.2) +
  geom_point(aes(x = MAPE_final, color = flag),
             size = 3.2, shape = 15) +
  scale_color_manual(
    values = c(
      "BASE (BASE)"            = "#95A5A6",
      "BASE (BASE_fallback)"   = "#E67E22",
      "BASE (BASE_lowR2)"      = "#F39C12",
      "BASE (BASE_lowlambda)"  = "#BDC3C7",
      "BASE (BASE_highlambda)" = "#BDC3C7",
      "D_city improves"        = "#1A7A4A",
      "D_city worsens"         = "#C0392B")) +
  geom_vline(xintercept = c(3, 5), linetype = "dashed",
             color = "grey50", linewidth = 0.5) +
  labs(
    title    = "Validation MAPE by sub-class: BASE vs S5 (D_city)",
    subtitle = sprintf(
      "Global: BASE=%.3f%% → S5=%.3f%% (%+.3f pp) | D_city series=%d",
      global_val$MAPE_BASE, global_val$MAPE_final,
      global_val$imp_vs_BASE, n_dcity),
    x = "MAPE (%)", y = "CPI sub-class", color = NULL
  ) +
  theme_bw(base_size = 11) +
  theme(legend.position = "bottom",
        panel.grid.major.y = element_blank())

ggsave(file.path(robust_dir, "fig_final_mape_by_subclass.png"),
       p_mape, width = 10, height = 8, dpi = 200)

# -----------------------------------------------------------------------
# 13. Build and save final price file
# -----------------------------------------------------------------------
message("Step 9/9 — Building final price file...")

extrap_corrected <- series_df %>%
  filter(period == "extrapolation") %>%
  select(ciudad, articulo, fecha, price_final, method)

prices_final <- prices_extended %>%
  left_join(
    extrap_corrected %>%
      rename(precio_corrected = price_final,
             method_final     = method),
    by = c("ciudad", "articulo", "fecha")
  ) %>%
  mutate(
    precio_final_method = if_else(
      !is.na(precio_corrected) & status == "imputed",
      precio_corrected, precio_final),
    method_final = if_else(
      !is.na(method_final) & status == "imputed",
      method_final, "observed"))

# Coverage counts
n_dcity_rows <- sum(prices_final$method_final == "D_city",          na.rm=TRUE)
n_base_rows  <- sum(prices_final$method_final == "BASE",            na.rm=TRUE)
n_obs_rows   <- sum(prices_final$method_final == "observed",        na.rm=TRUE)
n_fb_rows    <- sum(prices_final$method_final == "BASE_fallback",   na.rm=TRUE)
n_lr2_rows   <- sum(prices_final$method_final == "BASE_lowR2",      na.rm=TRUE)
n_llam_rows  <- sum(prices_final$method_final == "BASE_lowlambda",  na.rm=TRUE)
n_hlam_rows  <- sum(prices_final$method_final == "BASE_highlambda", na.rm=TRUE)

# Save validation outputs to robust folder
write_csv(
  global_val %>% pivot_longer(everything(),
                              names_to = "metric", values_to = "value"),
  file.path(robust_dir, "final_validation_global.csv"))
write_csv(global_by_method,
          file.path(robust_dir, "final_validation_by_method.csv"))
write_csv(subclass_val,
          file.path(robust_dir, "final_validation_subclass.csv"))
write_csv(item_val,
          file.path(robust_dir, "final_validation_item.csv"))

write_xlsx(
  list(
    global          = global_val %>%
      pivot_longer(everything(),
                   names_to = "metric", values_to = "value"),
    by_method       = global_by_method,
    by_subclass     = subclass_val,
    by_item         = item_val,
    lambda_estimates = lambda_final_df
  ),
  file.path(robust_dir, "final_validation_results.xlsx"))

# Save final prices to price_forecasting folder
write_csv(prices_final,
          file.path(out_forecast, "prices_final.csv"))
saveRDS(prices_final,
        file.path(out_forecast, "prices_final.rds"))

message("All files saved.")
message(sprintf("  prices_final.csv → %s", out_forecast))

# -----------------------------------------------------------------------
# 14. Decision print
# -----------------------------------------------------------------------
cat("\n")
cat(strrep("=", 72), "\n")
cat("  FINAL PRICE SERIES — S5 (BASE + D_city JS + bounds)\n")
cat("  Rule: k=1→BASE | k>1→D_city with R²≥0.10 & λ∈[0.20,4.00]\n")
cat(strrep("=", 72), "\n\n")

cat("── LAMBDA ESTIMATION ──\n\n")
cat(sprintf("  Total item × city series:    %d\n", nrow(lambda_final_df)))
cat(sprintf("  Single-item (k=1) → BASE:   %d\n",
            sum(!lambda_final_df$apply_D)))
cat(sprintf("  Multi-item, OLS OK:          %d\n",
            sum(lambda_final_df$apply_D &
                  lambda_final_df$est_ok, na.rm=TRUE)))
cat(sprintf("  Multi-item, OLS failed:      %d\n\n",
            sum(lambda_final_df$apply_D &
                  !lambda_final_df$est_ok, na.rm=TRUE)))

cat("── JS SHRINKAGE ──\n\n")
cat(sprintf("  Mean delta:                  %.3f\n",
            shrink_diag$delta_mean))
cat(sprintf("  |λ-1| before → after:        %.3f → %.3f (%.1f%% reduction)\n\n",
            shrink_diag$mean_abs_raw,
            shrink_diag$mean_abs_final,
            shrink_diag$pct_shrinkage))

cat("── VALIDITY BOUNDS ──\n\n")
cat(sprintf("  R² < %.2f  → BASE:           %d series\n",
            R2_MIN, n_low_r2))
cat(sprintf("  λ  < %.2f  → BASE:           %d series\n",
            LAM_MIN, n_low_lam))
cat(sprintf("  λ  > %.2f → BASE:            %d series\n",
            LAM_MAX, n_high_lam))
cat(sprintf("  D_city kept:                 %d series\n\n", n_dcity))

cat("── VALIDATION: Jan 2016 – Mar 2018 ──\n\n")
cat(sprintf("  %-22s  %8s  %8s  %10s\n",
            "Method", "MAPE_BASE", "MAPE_S5", "imp(pp)"))
cat(sprintf("  %s\n", strrep("-", 56)))
for (i in seq_len(nrow(global_by_method))) {
  r   <- global_by_method[i, ]
  sym <- if (str_starts(r$method, "BASE")) "  —"
  else if (r$imp > 0)               "  ✓"
  else                               "  ✗"
  cat(sprintf("  %-22s  %8.4f  %8.4f  %+10.4f%s\n",
              r$method, r$MAPE_BASE, r$MAPE_final, r$imp, sym))
}
cat(sprintf("  %-22s  %8.4f  %8.4f  %+10.4f\n",
            "GLOBAL",
            global_val$MAPE_BASE,
            global_val$MAPE_final,
            global_val$imp_vs_BASE))
cat(sprintf("\n  S5 beats BASE in %.1f%% of observations\n",
            global_val$pct_final_beats_BASE))
cat(sprintf("  Max APE: BASE=%.2f%% | S5=%.2f%%\n\n",
            global_val$max_BASE, global_val$max_final))

cat("── SUB-CLASS DETAIL ──\n\n")
cat(sprintf("  %-12s  %5s  %-18s  %9s  %9s  %9s\n",
            "sub-class", "k", "method",
            "BASE(%)", "S5(%)", "imp(pp)"))
cat(sprintf("  %s\n", strrep("-", 68)))
for (i in seq_len(nrow(subclass_val))) {
  r   <- subclass_val[i, ]
  sym <- if (str_starts(r$method, "BASE")) "  —"
  else if (r$imp > 0)               "  ✓"
  else                               "  ✗"
  cat(sprintf("  %-12s  %5d  %-18s  %9.4f  %9.4f  %+9.4f%s\n",
              r$subclase_ipc, r$k, r$method,
              r$MAPE_BASE, r$MAPE_final, r$imp, sym))
}
cat("\n")

cat("── OUTPUT FILE COVERAGE ──\n\n")
cat(sprintf("  D_city imputed rows:       %d\n", n_dcity_rows))
cat(sprintf("  BASE imputed (k=1):        %d\n", n_base_rows))
cat(sprintf("  BASE fallback:             %d\n", n_fb_rows))
cat(sprintf("  BASE low R²:               %d\n", n_lr2_rows))
cat(sprintf("  BASE low lambda:           %d\n", n_llam_rows))
cat(sprintf("  BASE high lambda:          %d\n", n_hlam_rows))
cat(sprintf("  Observed (unchanged):      %d\n\n", n_obs_rows))

cat("── OUTPUT LOCATIONS ──\n\n")
cat(sprintf("  prices_final.csv  → %s\n", out_forecast))
cat(sprintf("  lambda_estimates  → %s\n", out_forecast))
cat(sprintf("  Validation files  → %s\n", robust_dir))
cat(sprintf("  TS plots          → %s\n\n", val_plot_dir))

cat("── HOW TO USE ──\n\n")
cat("  Replace in your food table pipeline:\n")
cat("    read_csv('.../prices_extended_city_article_month.csv')\n")
cat("  → read_csv('.../price_forecasting/prices_final.csv')\n\n")
cat("  Column: precio_final → precio_final_method\n\n")
cat("  method_final values:\n")
cat("    'observed'         original DANE price\n")
cat("    'BASE'             IPC extrap, k=1\n")
cat("    'D_city'           JS-shrunk lambda, k>1\n")
cat("    'BASE_fallback'    estimation failed\n")
cat("    'BASE_lowR2'       R²<0.10\n")
cat("    'BASE_lowlambda'   λ<0.20\n")
cat("    'BASE_highlambda'  λ>4.00\n\n")

cat(strrep("=", 72), "\n\n")