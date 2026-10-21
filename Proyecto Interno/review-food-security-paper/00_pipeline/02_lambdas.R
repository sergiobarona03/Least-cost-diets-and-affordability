########################################################
## SCRIPT 02 — Sub-class assignment and OLS lambdas
## Output: FORECAST_DIR/lambda_raw.csv
##         FORECAST_DIR/anchor_prices.csv
##         FORECAST_DIR/lambda_attrition.csv   (NEW)
##         CACHE_DIR/item_subclass_map.rds
##         CACHE_DIR/subclass_sizes.rds
##         CACHE_DIR/anchor_prices.rds
##
## FIX (2026-10-20): the local variable holding the item's sub-class was
## named `subclase_ipc`, identical to the column of the same name in
## `subclass_sizes`. Under dplyr data masking the column takes precedence,
## so `filter(subclase_ipc == subclase_ipc)` evaluated as a tautology and
## `k` was taken from the city's FIRST sub-class instead of the item's own.
## The local variable is now `sub_ipc_i`. Nothing else in the estimation
## changed: lambda, sigma2, r2 and n_obs were already correct.
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
# (assign_method is now kept: it reports how many items were matched
#  through gasto_basico and how many through producto)
# -----------------------------------------------------------------------
message("Assigning sub-classes...")
item_subclass_map <- dane %>%
  filter(fecha <= TRAIN_END) %>%
  mutate(codigo_articulo = as.character(codigo_articulo),
         cod_subclase    = as.character(cod_subclase)) %>%
  group_by(nombre_ciudad, articulo) %>%
  group_modify(~ {
    sub_info <- assign_subclase(.x, corr_subclase, corr_producto)
    tibble(subclase_ipc  = sub_info$subclase,
           assign_method = sub_info$method)
  }) %>%
  ungroup() %>%
  filter(!is.na(subclase_ipc))

message(sprintf("  assignment method: %s",
                paste(sprintf("%s=%d",
                              names(table(item_subclass_map$assign_method)),
                              as.integer(table(item_subclass_map$assign_method))),
                      collapse = " | ")))

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
attrition  <- list()   # NEW: records every item dropped inside the loop

log_drop <- function(city_name, food_name, sub_ipc_i, reason) {
  tibble(ciudad = city_name, articulo = food_name,
         subclase_ipc = sub_ipc_i, drop_reason = reason)
}

for (i in seq_len(nrow(valid_items))) {
  
  city_name <- valid_items$ciudad[i]
  food_name <- valid_items$articulo[i]
  
  sub_row   <- item_subclass_map %>%
    filter(nombre_ciudad == city_name, articulo == food_name)
  sub_ipc_i <- if (nrow(sub_row) > 0) sub_row$subclase_ipc[1] else NA_character_
  
  # --- FIXED: filter on the item's own sub-class, not on itself -----------
  k_val <- subclass_sizes %>%
    filter(nombre_ciudad == city_name,
           subclase_ipc  == sub_ipc_i) %>%
    pull(k)
  k_val <- if (length(k_val) > 0) k_val[1] else 1L
  
  if (k_val == 1) {
    all_lambda[[length(all_lambda) + 1]] <- tibble(
      ciudad = city_name, articulo = food_name,
      subclase_ipc = sub_ipc_i, k = 1L,
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
  
  if (nrow(df_obs) == 0 || is.na(sub_ipc_i)) {
    attrition[[length(attrition) + 1]] <- log_drop(
      city_name, food_name, sub_ipc_i,
      if (is.na(sub_ipc_i)) "no_subclass_assigned" else "no_training_obs")
    next
  }
  
  subclase_code <- substr(paste0(sub_ipc_i, "00"), 1, 8)
  df_ipc <- ipc %>%
    filter(ciudad == city_name, cod_subclase == subclase_code,
           fecha <= TRAIN_END) %>%
    select(fecha, ipc) %>% arrange(fecha)
  
  if (nrow(df_ipc) == 0) {
    attrition[[length(attrition) + 1]] <- log_drop(
      city_name, food_name, sub_ipc_i, "no_ipc_series")
    next
  }
  
  est <- estimate_lambda_fd(df_obs, df_ipc)
  
  all_lambda[[length(all_lambda) + 1]] <- tibble(
    ciudad = city_name, articulo = food_name,
    subclase_ipc = sub_ipc_i, k = k_val,
    apply_D = TRUE,
    lambda  = if (!is.null(est)) est$lambda else NA_real_,
    sigma2  = if (!is.null(est)) est$sigma2 else NA_real_,
    r2      = if (!is.null(est)) est$r2     else NA_real_,
    n_obs   = if (!is.null(est)) est$n_obs  else NA_integer_,
    est_ok  = !is.null(est))
}

lambda_raw        <- bind_rows(all_lambda)
lambda_attrition  <- bind_rows(attrition)

message(sprintf(
  "  %d series | multi-item OK: %d | single-item: %d | failed: %d",
  nrow(lambda_raw),
  sum(lambda_raw$apply_D & lambda_raw$est_ok, na.rm=TRUE),
  sum(!lambda_raw$apply_D),
  sum(lambda_raw$apply_D & !lambda_raw$est_ok, na.rm=TRUE)))

# -----------------------------------------------------------------------
# Attrition report (NEW) — items with an anchor price that never reach
# lambda_raw, and therefore disappear from the price panel in Script 05
# -----------------------------------------------------------------------
if (nrow(lambda_attrition) > 0) {
  message("  dropped inside loop: ",
          paste(sprintf("%s=%d",
                        names(table(lambda_attrition$drop_reason)),
                        as.integer(table(lambda_attrition$drop_reason))),
                collapse = " | "))
} else {
  message("  dropped inside loop: none")
}

anchor_no_lambda <- anchor_prices %>%
  anti_join(lambda_raw, by = c("ciudad", "articulo"))

map_no_anchor <- item_subclass_map %>%
  anti_join(anchor_prices,
            by = c("nombre_ciudad" = "ciudad", "articulo"))

message(sprintf(
  "  items with anchor but no lambda: %d | assigned but no anchor at T0: %d",
  nrow(anchor_no_lambda), nrow(map_no_anchor)))

# -----------------------------------------------------------------------
# k distribution check — guards against the masking bug returning
# -----------------------------------------------------------------------
k_check <- lambda_raw %>% count(ciudad, k) %>% arrange(ciudad, k)
print(k_check)
if (nrow(lambda_raw) > 0 &&
    all(tapply(lambda_raw$k, lambda_raw$ciudad,
               function(z) length(unique(z))) == 1)) {
  warning("Every item within a city shares the same k. ",
          "Check the subclass_sizes filter before continuing.")
}

# -----------------------------------------------------------------------
# Save
# -----------------------------------------------------------------------
write_csv(lambda_raw,       file.path(FORECAST_DIR, "lambda_raw.csv"))
write_csv(anchor_prices,    file.path(FORECAST_DIR, "anchor_prices.csv"))
write_csv(lambda_attrition, file.path(FORECAST_DIR, "lambda_attrition.csv"))
saveRDS(item_subclass_map, file.path(CACHE_DIR, "item_subclass_map.rds"))
saveRDS(subclass_sizes,    file.path(CACHE_DIR, "subclass_sizes.rds"))
saveRDS(anchor_prices,     file.path(CACHE_DIR, "anchor_prices.rds"))

message("Done. Run 03_threshold_optimization.R next.")