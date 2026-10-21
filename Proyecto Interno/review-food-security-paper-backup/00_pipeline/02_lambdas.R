########################################################
## SCRIPT 02 — Sub-class assignment and OLS lambdas
## Output: FORECAST_DIR/lambda_raw.csv
##         FORECAST_DIR/anchor_prices.csv
##         CACHE_DIR/item_subclass_map.rds
##         CACHE_DIR/subclass_sizes.rds
##         CACHE_DIR/anchor_prices.rds
########################################################

source("00_config.R")
library(tidyverse)

# -----------------------------------------------------------------------
# Load cache
# -----------------------------------------------------------------------
message("Loading cache...")
dane          <- readRDS(file.path(CACHE_DIR, "dane.rds"))
ipc           <- readRDS(file.path(CACHE_DIR, "ipc.rds"))
corr_subclase <- readRDS(file.path(CACHE_DIR, "corr_subclase.rds"))
corr_producto <- readRDS(file.path(CACHE_DIR, "corr_producto.rds"))

# -----------------------------------------------------------------------
# Auxiliary functions
# -----------------------------------------------------------------------
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
    error = function(e) NULL)
  if (is.null(fit)) return(NULL)
  list(
    lambda = coef(fit)[["d_log_ipc"]],
    sigma2 = sum(resid(fit)^2) / max(nrow(merged) - 1, 1),
    r2     = summary(fit)$r.squared,
    n_obs  = nrow(merged))
}

# -----------------------------------------------------------------------
# Sub-class assignment
# -----------------------------------------------------------------------
message("Assigning sub-classes...")
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

anchor_prices <- dane %>%
  filter(fecha == T0) %>%
  select(ciudad = nombre_ciudad, articulo,
         precio_anchor = precio_500g)

valid_items <- anchor_prices %>% distinct(ciudad, articulo)

message(sprintf("  %d item × city series | k sizes: %s",
                nrow(valid_items),
                paste(sort(unique(subclass_sizes$k)), collapse = ", ")))

# -----------------------------------------------------------------------
# Estimate OLS lambdas (training period: Jan 1999 – Dec 2015)
# -----------------------------------------------------------------------
message("Estimating OLS lambdas...")
all_lambda <- list()

for (i in seq_len(nrow(valid_items))) {
  
  city_name <- valid_items$ciudad[i]
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
    all_lambda[[length(all_lambda) + 1]] <- tibble(
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
  
  all_lambda[[length(all_lambda) + 1]] <- tibble(
    ciudad = city_name, articulo = food_name,
    subclase_ipc = subclase_ipc, k = k_val,
    apply_D = TRUE,
    lambda  = if (!is.null(est)) est$lambda else NA_real_,
    sigma2  = if (!is.null(est)) est$sigma2 else NA_real_,
    r2      = if (!is.null(est)) est$r2     else NA_real_,
    n_obs   = if (!is.null(est)) est$n_obs  else NA_integer_,
    est_ok  = !is.null(est))
}

lambda_raw <- bind_rows(all_lambda)

message(sprintf(
  "  %d series | multi-item OK: %d | single-item: %d | failed: %d",
  nrow(lambda_raw),
  sum(lambda_raw$apply_D & lambda_raw$est_ok, na.rm=TRUE),
  sum(!lambda_raw$apply_D),
  sum(lambda_raw$apply_D & !lambda_raw$est_ok, na.rm=TRUE)))

# -----------------------------------------------------------------------
# Save
# -----------------------------------------------------------------------
write_csv(lambda_raw,    file.path(FORECAST_DIR, "lambda_raw.csv"))
write_csv(anchor_prices, file.path(FORECAST_DIR, "anchor_prices.csv"))
saveRDS(item_subclass_map, file.path(CACHE_DIR, "item_subclass_map.rds"))
saveRDS(subclass_sizes,    file.path(CACHE_DIR, "subclass_sizes.rds"))
saveRDS(anchor_prices,     file.path(CACHE_DIR, "anchor_prices.rds"))

message("Done. Run 03_threshold_optimization.R next.")