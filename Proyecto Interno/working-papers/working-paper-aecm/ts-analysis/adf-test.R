###################################################
## Prueba: ADF tests (mayoristas y minoristas)   ##
## Guarda resultados en: output/ts-output        ##
## ADF con drift (no trend) y con drift+trend    ##
###################################################

library(lubridate)
library(tidyverse)
library(readxl)
library(writexl)
library(urca)

setwd("C:\\Users\\sergio.barona\\Desktop\\Least-cost-diets-and-affordability\\Proyecto Interno\\")

date_tag <- "261225"

path.in <- paste0("working-papers\\working-paper-aecm\\input\\",
                  date_tag, "_selected_foods_dataset.xlsx")

dataset <- read_excel(path.in) %>% na.omit()

out_dir <- "working-papers\\working-paper-aecm\\output\\ts-output\\"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# ---------------------------
# Aux: ADF + extract key stats
# type = "drift" (constante) o "trend" (constante + tendencia)
# ---------------------------
run_adf <- function(x_ts, type = c("drift","trend")) {
  type <- match.arg(type)
  test <- ur.df(x_ts, type = type)
  
  stat <- as.numeric(test@teststat[1])
  cval <- test@cval[1, ]
  
  tibble(
    adf_type  = type,
    test_stat = stat,
    cv_1pct   = as.numeric(cval["1pct"]),
    cv_5pct   = as.numeric(cval["5pct"]),
    cv_10pct  = as.numeric(cval["10pct"])
  )
}

# ---------------------------
# Core runner (per city × item)
# item_var: "alimento_sipsa" OR "articulo_ipc"
# adf_types: vector c("drift","trend")
# ---------------------------
make_tests <- function(df, price_var, item_var, city_list,
                       start_year = 2013, start_month = 1,
                       freq = 12, adf_types = c("drift","trend")) {
  
  df2 <- df %>%
    select(cod_mun, Year, Month, !!sym(item_var), !!sym(price_var)) %>%
    distinct() %>%
    arrange(cod_mun, !!sym(item_var), Year, Month)
  
  results <- list()
  
  for (c in city_list) {
    dfc <- df2 %>% filter(cod_mun == c)
    items <- unique(dfc[[item_var]])
    
    for (it in items) {
      dfi <- dfc %>% filter(.data[[item_var]] == it)
      
      x_ts <- ts(
        data = dfi[[price_var]],
        start = c(start_year, start_month),
        frequency = freq
      )
      
      for (tp in adf_types) {
        res <- run_adf(x_ts, type = tp) %>%
          mutate(
            cod_mun  = c,
            item_var = item_var,
            item     = as.character(it),
            variable = price_var,
            n_obs    = length(x_ts)
          )
        
        results[[length(results) + 1]] <- res
      }
    }
  }
  
  bind_rows(results)
}

# Cities
cities_required <- c("76001", "11001", "05001")

# ---------------------------
# WHOLESALE ADF: by alimento_sipsa (drift + trend)
# ---------------------------
wholesale_results <- make_tests(
  df = dataset %>% select(cod_mun, Year, Month, alimento_sipsa, precio_sipsa) %>% distinct(),
  price_var = "precio_sipsa",
  item_var  = "alimento_sipsa",
  city_list = cities_required,
  start_year = 2013, start_month = 1,
  adf_types = c("drift","trend")
)

# ---------------------------
# RETAIL ADF: by articulo_ipc (drift + trend)
# ---------------------------
retail_results <- make_tests(
  df = dataset %>% select(cod_mun, Year, Month, articulo_ipc, precio_ipc) %>% distinct(),
  price_var = "precio_ipc",
  item_var  = "articulo_ipc",
  city_list = cities_required,
  start_year = 2013, start_month = 1,
  adf_types = c("drift","trend")
)

# ---------------------------
# Save outputs
# ---------------------------
write_xlsx(
  list(
    "ADF_wholesale_alimento_drift_trend" = wholesale_results,
    "ADF_retail_articulo_drift_trend"    = retail_results
  ),
  path = file.path(out_dir, paste0(date_tag, "_adf_results_drift_trend.xlsx"))
)

readr::write_csv(wholesale_results, file.path(out_dir, paste0(date_tag, "_adf_wholesale_drift_trend.csv")))
readr::write_csv(retail_results,   file.path(out_dir, paste0(date_tag, "_adf_retail_drift_trend.csv")))

cat("\nSaved ADF outputs to:\n", out_dir, "\n")
print(head(wholesale_results, 12))
print(head(retail_results, 12))
