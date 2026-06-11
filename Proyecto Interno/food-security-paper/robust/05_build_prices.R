########################################################
## SCRIPT 05 — Build final price series
##
## Reads:
##   cache/: dane.rds, ipc.rds, anchor_prices.rds
##   price_forecasting/lambda_final.csv
##   prices_extended_city_article_month.csv (original pipeline)
##
## Builds:
##   - Validation panel (Feb 2016 – Mar 2018)
##   - Extrapolation series (Apr 2018 – Dec 2024)
##
## Writes to .../price_forecasting/:
##   prices_validation_panel.csv  ← for Script 07 (CoNA validation)
##   prices_final.csv / .rds      ← main output for paper pipeline
########################################################

library(tidyverse)
library(readxl)

# -----------------------------------------------------------------------
# 1. Directories
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

in_prices_extended <- file.path(base_dir,
                                "food-security-paper/output/forecasting_fullsample/prices_extended_city_article_month.csv")

T0           <- as.Date("2016-01-01")
VAL_START    <- as.Date("2016-02-01")
VAL_END      <- as.Date("2018-03-01")
EXTRAP_START <- as.Date("2018-04-01")

# -----------------------------------------------------------------------
# 2. Load inputs
# -----------------------------------------------------------------------
message("Loading inputs...")

dane          <- readRDS(file.path(cache_dir, "dane.rds"))
ipc           <- readRDS(file.path(cache_dir, "ipc.rds"))
anchor_prices <- readRDS(file.path(cache_dir, "anchor_prices.rds"))

lambda_final <- read_csv(
  file.path(out_forecast, "lambda_final.csv"),
  show_col_types = FALSE)

prices_extended <- read_csv(in_prices_extended,
                            show_col_types = FALSE) %>%
  mutate(fecha = as.Date(fecha))

message(sprintf(
  "  lambda_final: %d rows | extended: %d rows",
  nrow(lambda_final), nrow(prices_extended)))

# -----------------------------------------------------------------------
# 3. Build price predictions (validation + extrapolation)
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
  
  obs_val <- dane %>%
    filter(nombre_ciudad == city_name, articulo == food_name,
           fecha >= T0, fecha <= VAL_END, !is.na(precio_500g)) %>%
    select(fecha, price_obs = precio_500g)
  
  pred <- df_ipc %>%
    filter(fecha >= T0) %>%
    mutate(
      ciudad     = city_name,
      articulo   = food_name,
      method     = method_use,
      ipc_ratio  = ipc / ipc_t0,
      price_BASE = anchor * ipc_ratio,
      price_final= anchor * ipc_ratio ^ lambda_use,
      period     = case_when(
        fecha <= VAL_END      ~ "validation",
        fecha >= EXTRAP_START ~ "extrapolation",
        TRUE                  ~ "other")
    ) %>%
    left_join(obs_val, by = "fecha")
  
  all_series[[length(all_series) + 1]] <- pred
}

series_df <- bind_rows(all_series)
message(sprintf("  %d item × city × month rows", nrow(series_df)))

# -----------------------------------------------------------------------
# 4. Save validation panel for Script 07
# -----------------------------------------------------------------------
series_df %>%
  filter(period == "validation") %>%
  select(ciudad, articulo, fecha,
         price_BASE, price_final, method) %>%
  write_csv(file.path(out_forecast, "prices_validation_panel.csv"))

message("  Saved prices_validation_panel.csv")

# -----------------------------------------------------------------------
# 5. Build and save final price file (extrapolation period only)
# -----------------------------------------------------------------------
message("Building final price file...")

extrap <- series_df %>%
  filter(period == "extrapolation") %>%
  select(ciudad, articulo, fecha, price_final, method)

prices_final <- prices_extended %>%
  left_join(
    extrap %>%
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

n_dcity <- sum(prices_final$method_final == "D_city",    na.rm=TRUE)
n_base  <- sum(prices_final$method_final == "BASE",      na.rm=TRUE)
n_obs   <- sum(prices_final$method_final == "observed",  na.rm=TRUE)

message(sprintf(
  "  Coverage — D_city: %d | BASE: %d | observed: %d",
  n_dcity, n_base, n_obs))

write_csv(prices_final, file.path(out_forecast, "prices_final.csv"))
saveRDS(prices_final,   file.path(out_forecast, "prices_final.rds"))

message("Done. Run Script 06 (price validation) or Script 07 (CoNA validation).")