########################################################
## SCRIPT 05 — Build price series from scratch
##
## Everything is built from:
##   DANE raw prices (anchor at T0 = Jan 2016)
##   IPC sub-class series (extrapolation signal)
##   lambda_final.csv (JS-shrunk elasticities)
##
## No external price files are used.
##
## Two output panels:
##   prices_validation.csv  — Feb 2016–Mar 2018
##     price_BASE   : anchor × (IPC_t/IPC_T0)^1
##     price_S5     : anchor × (IPC_t/IPC_T0)^λ*
##     price_obs    : DANE observed (ground truth)
##
##   prices_paper.csv       — Jan 2019–Dec 2024
##     price_S5     : anchor × (IPC_t/IPC_T0)^λ*
##     (observed where available, S5 otherwise)
##
## Writes: FORECAST_DIR/prices_validation.csv
##         FORECAST_DIR/prices_paper.csv
##         FORECAST_DIR/prices_paper.rds
########################################################

source("00_config.R")
library(tidyverse)

# -----------------------------------------------------------------------
# Load inputs
# -----------------------------------------------------------------------
message("Loading inputs...")
dane          <- readRDS(file.path(CACHE_DIR, "dane.rds"))
ipc           <- readRDS(file.path(CACHE_DIR, "ipc.rds"))
anchor_prices <- readRDS(file.path(CACHE_DIR, "anchor_prices.rds"))

lambda_final <- read_csv(file.path(FORECAST_DIR, "lambda_final.csv"),
                         show_col_types = FALSE)

# -----------------------------------------------------------------------
# Build price predictions for all periods
# Covers: T0 → VAL_END (validation) and T0 → PAPER_END (paper)
# -----------------------------------------------------------------------
message("Building price predictions...")

all_series <- list()

for (i in seq_len(nrow(lambda_final))) {
  
  city_name    <- lambda_final$ciudad[i]
  food_name    <- lambda_final$articulo[i]
  lambda_use   <- lambda_final$lambda_final[i]
  method_use   <- lambda_final$method_label[i]
  subclase_ipc <- lambda_final$subclase_ipc[i]
  
  if (!is.finite(lambda_use)) lambda_use <- 1.0
  if (is.na(subclase_ipc)) next
  
  subclase_code <- substr(paste0(subclase_ipc, "00"), 1, 8)
  
  anchor <- anchor_prices %>%
    filter(ciudad == city_name, articulo == food_name) %>%
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
  
  # DANE observed prices — all available dates
  obs_dane <- dane %>%
    filter(nombre_ciudad == city_name, articulo == food_name,
           !is.na(precio_500g)) %>%
    select(fecha, price_obs = precio_500g)
  
  pred <- df_ipc %>%
    filter(fecha >= T0) %>%
    mutate(
      ciudad      = city_name,
      articulo    = food_name,
      method      = method_use,
      subclase_ipc= subclase_ipc,
      ipc_ratio   = ipc / ipc_t0,
      price_BASE  = anchor * ipc_ratio,
      price_S5    = anchor * ipc_ratio ^ lambda_use,
      period      = case_when(
        fecha >= VAL_START & fecha <= VAL_END   ~ "validation",
        fecha >= PAPER_START & fecha <= PAPER_END ~ "paper",
        fecha == T0                               ~ "anchor",
        TRUE                                      ~ "other")
    ) %>%
    left_join(obs_dane, by = "fecha")
  
  all_series[[length(all_series) + 1]] <- pred
}

series_df <- bind_rows(all_series)
message(sprintf("  %d item × city × month rows total",
                nrow(series_df)))

# -----------------------------------------------------------------------
# Panel 1: Validation (Feb 2016 – Mar 2018)
# Contains BASE, S5, and observed prices for MADE calculation
# -----------------------------------------------------------------------
prices_validation <- series_df %>%
  filter(period == "validation") %>%
  select(ciudad, articulo, fecha, subclase_ipc, method,
         price_BASE, price_S5, price_obs) %>%
  arrange(ciudad, articulo, fecha)

message(sprintf(
  "  Validation panel: %d rows | with obs: %d",
  nrow(prices_validation),
  sum(!is.na(prices_validation$price_obs))))

# -----------------------------------------------------------------------
# Panel 2: Paper period (Jan 2019 – Dec 2024)
# Uses S5 extrapolation — no observed prices exist beyond Mar 2018
# -----------------------------------------------------------------------
prices_paper <- series_df %>%
  filter(period == "paper") %>%
  select(ciudad, articulo, fecha, subclase_ipc, method,
         price_S5) %>%
  rename(precio_500g = price_S5) %>%
  arrange(ciudad, articulo, fecha)

message(sprintf(
  "  Paper panel: %d rows | %d cities | %d items | %s–%s",
  nrow(prices_paper),
  n_distinct(prices_paper$ciudad),
  n_distinct(prices_paper$articulo),
  min(prices_paper$fecha),
  max(prices_paper$fecha)))

# -----------------------------------------------------------------------
# Save
# -----------------------------------------------------------------------
write_csv(prices_validation,
          file.path(FORECAST_DIR, "prices_validation.csv"))
write_csv(prices_paper,
          file.path(FORECAST_DIR, "prices_paper.csv"))
saveRDS(prices_paper,
        file.path(FORECAST_DIR, "prices_paper.rds"))

message("Outputs saved to: ", FORECAST_DIR)
message("Done. Run 06_price_validation.R next.")