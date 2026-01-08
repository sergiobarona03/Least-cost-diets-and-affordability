###############################################################
## coint-eg-test.R
## Cointegration tests using aTSA::coint.test with IC-selected nlag
## on seasonally adjusted log prices (X-13 via seasonal)
##
## Lag selection is done under TWO ADF specifications:
##  (1) adf_type = "drift"
##  (2) adf_type = "trend"
##
## Saves results in:
##   working-papers/working-paper-aecm/output/ts-output/coint-test/
###############################################################

suppressPackageStartupMessages({
  library(tidyverse)
  library(readxl)
  library(writexl)
  library(lubridate)
  library(seasonal)
  library(aTSA)
})

# -------------------------------------------------------------
# Source helpers
# -------------------------------------------------------------
source("aux-eg-test.R")

# -------------------------------------------------------------
# Setup
# -------------------------------------------------------------
wd_path <- "C:\\Users\\sergio.barona\\Desktop\\Least-cost-diets-and-affordability\\Proyecto Interno\\"
setwd(wd_path)

date_tag <- "261225"

path_in <- file.path(
  "working-papers", "working-paper-aecm", "input",
  paste0(date_tag, "_selected_foods_dataset.xlsx")
)

out_base <- file.path("working-papers", "working-paper-aecm", "output", "ts-output")
out_dir  <- file.path(out_base, "coint-test")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

plot_dir <- file.path(out_dir, paste0(date_tag, "_coint_plots_SA"))
dir.create(plot_dir, showWarnings = FALSE, recursive = TRUE)

cities_required <- c("76001", "11001", "05001")
city_labs <- c("76001" = "Cali", "11001" = "Bogotá", "05001" = "Medellín")

# Lag-selection settings
ic_used <- "BIC"   # "BIC" or "AIC"
max_lag <- 12      # monthly data

# Two ADF specifications for lag selection
adf_specs <- c("drift", "trend")

# -------------------------------------------------------------
# Load + prepare
# -------------------------------------------------------------
df0 <- read_excel(path_in) %>%
  mutate(
    cod_mun = sprintf("%05d", as.integer(cod_mun)),
    Year = as.integer(Year),
    Month = as.integer(Month),
    date = as.Date(sprintf("%d-%02d-01", Year, Month)),
    articulo_ipc = str_squish(as.character(articulo_ipc)),
    alimento_sipsa = str_squish(as.character(alimento_sipsa)),
    precio_ipc = as.numeric(precio_ipc),
    precio_sipsa = as.numeric(precio_sipsa)
  ) %>%
  filter(!is.na(date), !is.na(precio_ipc), !is.na(precio_sipsa)) %>%
  filter(precio_ipc > 0, precio_sipsa > 0) %>%
  filter(cod_mun %in% cities_required)

# Collapse duplicates: one obs per city-month-pair
df <- df0 %>%
  group_by(cod_mun, date, articulo_ipc, alimento_sipsa) %>%
  summarise(
    precio_ipc_m   = mean(precio_ipc, na.rm = TRUE),
    precio_sipsa_m = mean(precio_sipsa, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    l_ipc   = log(precio_ipc_m),
    l_sipsa = log(precio_sipsa_m)
  ) %>%
  arrange(cod_mun, articulo_ipc, alimento_sipsa, date)

pairs_tbl <- df %>%
  distinct(articulo_ipc, alimento_sipsa) %>%
  arrange(articulo_ipc, alimento_sipsa)

# -------------------------------------------------------------
# Helper to run everything for a given ADF spec
# -------------------------------------------------------------
run_all_for_adf_spec <- function(adf_type_spec) {
  
  results <- list()
  
  for (i in seq_len(nrow(pairs_tbl))) {
    
    art_i <- pairs_tbl$articulo_ipc[i]
    ali_i <- pairs_tbl$alimento_sipsa[i]
    
    dat_pair <- df %>%
      filter(articulo_ipc == art_i, alimento_sipsa == ali_i) %>%
      mutate(city = recode(cod_mun, !!!city_labs)) %>%
      arrange(cod_mun, date)
    
    # Seasonal adjustment by city (X-13) on LOG series
    dat_pair_sa <- dat_pair %>%
      group_by(cod_mun, city) %>%
      group_modify(~{
        .x <- .x %>% arrange(date)
        start_ym <- c(year(min(.x$date)), month(min(.x$date)))
        
        ts_ipc   <- ts(.x$l_ipc,   start = start_ym, frequency = 12)
        ts_sipsa <- ts(.x$l_sipsa, start = start_ym, frequency = 12)
        
        sa_ipc   <- sa_x13(ts_ipc)
        sa_sipsa <- sa_x13(ts_sipsa)
        
        .x$l_ipc_sa   <- if (is.null(sa_ipc)) NA_real_ else sa_ipc
        .x$l_sipsa_sa <- if (is.null(sa_sipsa)) NA_real_ else sa_sipsa
        .x$sa_failed  <- is.null(sa_ipc) || is.null(sa_sipsa)
        
        .x
      }) %>%
      ungroup()
    
    # Cointegration tests by city using aTSA with IC-selected nlag (by adf_type_spec)
    res_pair <- dat_pair_sa %>%
      group_by(cod_mun, city) %>%
      group_modify(~{
        x <- .x$l_ipc_sa
        y <- .x$l_sipsa_sa
        
        if (anyNA(x) || anyNA(y) || length(x) < 24) {
          tibble(
            type = NA_character_, lag = NA_integer_, EG = NA_real_, p.value = NA_real_,
            d = 0, n_obs = length(x),
            ic_used = ic_used, adf_type = adf_type_spec,
            ic_lag = NA_integer_, ic_value = NA_real_, aic = NA_real_, bic = NA_real_,
            sa_failed = TRUE
          )
        } else {
          run_atsa_coint_ic(x, y, d = 0, max_lag = max_lag, ic = ic_used, adf_type = adf_type_spec) %>%
            mutate(sa_failed = FALSE)
        }
      }) %>%
      ungroup() %>%
      mutate(
        articulo_ipc = art_i,
        alimento_sipsa = ali_i
      ) %>%
      select(articulo_ipc, alimento_sipsa, cod_mun, city,
             type, lag, EG, p.value, d, n_obs,
             ic_used, adf_type, ic_lag, ic_value, aic, bic, sa_failed)
    
    results[[length(results) + 1]] <- res_pair
    
    # Plot (same plot for both specs; save with spec tag to avoid overwrite)
    plot_dat <- dat_pair_sa %>%
      select(city, date, l_ipc_sa, l_sipsa_sa) %>%
      pivot_longer(c(l_ipc_sa, l_sipsa_sa), names_to = "series", values_to = "sa_log_price") %>%
      mutate(series = recode(series,
                             l_ipc_sa   = "Retail (IPC) SA log(price)",
                             l_sipsa_sa = "Wholesale (SIPSA) SA log(price)")) %>%
      filter(is.finite(sa_log_price))
    
    p <- ggplot(plot_dat, aes(date, sa_log_price, color = series)) +
      geom_line(linewidth = 0.7, alpha = 0.95) +
      facet_wrap(~ city, ncol = 1, scales = "free_y") +
      labs(
        title = paste0("Seasonally adjusted log prices: ", art_i, " vs ", ali_i),
        subtitle = paste0("Engle–Granger (aTSA) with IC-selected nlag: ", ic_used,
                          " over 1–", max_lag, " | ADF spec for lag selection: ", adf_type_spec),
        x = NULL, y = "SA log(price)", color = NULL
      ) +
      theme_minimal(base_size = 12) +
      theme(legend.position = "bottom")
    
    file_png <- file.path(
      plot_dir,
      paste0(date_tag, "_", adf_type_spec, "_SA_", safe_name(art_i), "__", safe_name(ali_i), ".png")
    )
    ggsave(file_png, p, width = 11, height = 8, dpi = 300)
  }
  
  bind_rows(results) %>%
    arrange(articulo_ipc, alimento_sipsa, cod_mun, type)
}

# -------------------------------------------------------------
# Run both specifications and save separately
# -------------------------------------------------------------
coint_results_drift <- run_all_for_adf_spec("drift")
coint_results_trend <- run_all_for_adf_spec("trend")

# Save outputs (two separate files)
out_xlsx_drift <- file.path(out_dir, paste0(date_tag, "_coint_results_SA_logprices_IC_", ic_used, "_maxlag", max_lag, "_drift.xlsx"))
out_csv_drift  <- file.path(out_dir, paste0(date_tag, "_coint_results_SA_logprices_IC_", ic_used, "_maxlag", max_lag, "_drift.csv"))

out_xlsx_trend <- file.path(out_dir, paste0(date_tag, "_coint_results_SA_logprices_IC_", ic_used, "_maxlag", max_lag, "_trend.xlsx"))
out_csv_trend  <- file.path(out_dir, paste0(date_tag, "_coint_results_SA_logprices_IC_", ic_used, "_maxlag", max_lag, "_trend.csv"))

write_xlsx(list("coint_test_SA_logprices_drift" = coint_results_drift), out_xlsx_drift)
readr::write_csv(coint_results_drift, out_csv_drift)

write_xlsx(list("coint_test_SA_logprices_trend" = coint_results_trend), out_xlsx_trend)
readr::write_csv(coint_results_trend, out_csv_trend)

cat("\nSaved cointegration outputs to:\n", out_dir, "\n")
cat("Plots saved to:\n", plot_dir, "\n")
cat("\nFiles:\n",
    " - ", out_xlsx_drift, "\n",
    " - ", out_xlsx_trend, "\n")
