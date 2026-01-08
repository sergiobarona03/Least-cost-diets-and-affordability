###################################################
## ADF tests (wholesale vs retail)               ##
## Seasonally adjusted (X-13) + log-levels       ##
## Saves to: working-papers/.../output/ts-output ##
## ADF with drift and drift+trend                ##
###################################################

library(lubridate)
library(tidyverse)
library(readxl)
library(writexl)
library(urca)
library(seasonal)

setwd("C:\\Users\\sergio.barona\\Desktop\\Least-cost-diets-and-affordability\\Proyecto Interno\\")

date_tag <- "261225"

path.in <- paste0("working-papers\\working-paper-aecm\\input\\",
                  date_tag, "_selected_foods_dataset.xlsx")

dataset <- read_excel(path.in) %>% na.omit()

out_dir <- "working-papers\\working-paper-aecm\\output\\ts-output\\adf-test\\"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# ---------------------------
# Aux: ADF + extract key stats
# type = "drift" (constante) or "trend" (constante + tendencia)
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
# Aux: Seasonal adjustment (X-13 via seasonal)
# Input: ts in log-levels
# Output: seasonally adjusted log series (final)
# ---------------------------
seasonal_adjust_x13 <- function(x_ts) {
  fit <- try(
    seas(
      x_ts,
      x11 = "",
      transform.function = "none",  # IMPORTANT: we are already in logs
      regression.aictest = NULL,
      outlier = NULL
    ),
    silent = TRUE
  )
  
  if (inherits(fit, "try-error")) return(NULL)
  
  # "final" is the seasonally adjusted series in seasonal
  as.numeric(final(fit))
}

# ---------------------------
# Core runner (per city × item)
# price_var: "precio_sipsa" OR "precio_ipc"
# item_var:  "alimento_sipsa" OR "articulo_ipc"
# adf_types: c("drift","trend")
# ---------------------------
make_tests <- function(df, price_var, item_var, city_list,
                       start_year = 2013, start_month = 1,
                       freq = 12, adf_types = c("drift","trend")) {
  
  df2 <- df %>%
    select(cod_mun, Year, Month, !!sym(item_var), !!sym(price_var)) %>%
    distinct() %>%
    arrange(cod_mun, !!sym(item_var), Year, Month) %>%
    mutate(
      price_num = as.numeric(.data[[price_var]]),
      log_price = log(price_num)
    ) %>%
    filter(is.finite(log_price))
  
  results <- list()
  
  for (c in city_list) {
    dfc <- df2 %>% filter(cod_mun == c)
    items <- unique(dfc[[item_var]])
    
    for (it in items) {
      dfi <- dfc %>% filter(.data[[item_var]] == it)
      
      # Build log-level monthly ts (assumes balanced monthly panel)
      x_log_ts <- ts(
        data = dfi$log_price,
        start = c(start_year, start_month),
        frequency = freq
      )
      
      # Seasonal adjustment on log-levels
      x_sa_vec <- seasonal_adjust_x13(x_log_ts)
      
      # If X-13 failed for this series, skip (or record NA)
      if (is.null(x_sa_vec)) {
        for (tp in adf_types) {
          results[[length(results) + 1]] <- tibble(
            adf_type  = tp,
            test_stat = NA_real_,
            cv_1pct   = NA_real_,
            cv_5pct   = NA_real_,
            cv_10pct  = NA_real_
          ) %>%
            mutate(
              cod_mun   = c,
              item_var  = item_var,
              item      = as.character(it),
              variable  = paste0("log(", price_var, ")_SA_X13"),
              n_obs     = length(x_log_ts),
              sa_failed = TRUE
            )
        }
        next
      }
      
      # Convert SA back to ts with same start/frequency
      x_sa_ts <- ts(x_sa_vec, start = c(start_year, start_month), frequency = freq)
      
      for (tp in adf_types) {
        res <- run_adf(x_sa_ts, type = tp) %>%
          mutate(
            cod_mun   = c,
            item_var  = item_var,
            item      = as.character(it),
            variable  = paste0("log(", price_var, ")_SA_X13"),
            n_obs     = length(x_sa_ts),
            sa_failed = FALSE
          )
        
        results[[length(results) + 1]] <- res
      }
    }
  }
  
  bind_rows(results)
}

# Cities (as characters)
cities_required <- c("76001", "11001", "05001")

# ---------------------------
# WHOLESALE ADF: by alimento_sipsa
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
# RETAIL ADF: by articulo_ipc
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
    "ADF_wholesale_logSA_X13" = wholesale_results,
    "ADF_retail_logSA_X13"    = retail_results
  ),
  path = file.path(out_dir, paste0(date_tag, "_adf_results_logSA_X13.xlsx"))
)

readr::write_csv(wholesale_results, file.path(out_dir, paste0(date_tag, "_adf_wholesale_logSA_X13.csv")))
readr::write_csv(retail_results,   file.path(out_dir, paste0(date_tag, "_adf_retail_logSA_X13.csv")))

cat("\nSaved ADF outputs to:\n", out_dir, "\n")
print(head(wholesale_results, 12))
print(head(retail_results, 12))
