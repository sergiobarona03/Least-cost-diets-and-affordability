###############################################################
## Cointegration tests (aTSA::coint.test) + plots
## - log(precio_ipc) vs log(precio_sipsa)
## - For each (articulo_ipc × alimento_sipsa) pair, by city
## Outputs:
##   1) Excel + CSV with full coint.test table (type 1/2/3)
##   2) Plots per pair faceted by city
###############################################################

library(tidyverse)
library(readxl)
library(writexl)
library(lubridate)
library(aTSA)

setwd("C:\\Users\\sergio.barona\\Desktop\\Least-cost-diets-and-affordability\\Proyecto Interno\\")
date_tag <- "261225"

# -------------------------------------------------------------
# Paths
# -------------------------------------------------------------
path.in <- paste0("working-papers\\working-paper-aecm\\input\\",
                  date_tag, "_selected_foods_dataset.xlsx")

out_dir  <- "working-papers\\working-paper-aecm\\output\\ts-output\\"

plot_dir <- file.path(out_dir, paste0(date_tag, "_coint_plots"))

# -------------------------------------------------------------
# Load + prepare
# -------------------------------------------------------------
df0 <- read_excel(path.in) %>%
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
  filter(precio_ipc > 0, precio_sipsa > 0)

# Collapse duplicates: one obs per city-month-pair
df <- df0 %>%
  group_by(cod_mun, date, articulo_ipc, alimento_sipsa) %>%
  summarise(
    precio_ipc_m   = mean(precio_ipc, na.rm = TRUE),
    precio_sipsa_m = mean(precio_sipsa, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(is.finite(precio_ipc_m), is.finite(precio_sipsa_m)) %>%
  filter(precio_ipc_m > 0, precio_sipsa_m > 0) %>%
  mutate(
    l_ipc   = log(precio_ipc_m),
    l_sipsa = log(precio_sipsa_m)
  ) %>%
  arrange(cod_mun, articulo_ipc, alimento_sipsa, date)

cities_required <- c("76001", "11001", "05001")
df <- df %>% filter(cod_mun %in% cities_required)

city_labs <- c("76001"="Cali", "11001"="Bogotá", "05001"="Medellín")

# -------------------------------------------------------------
# Helper: safe file name
# -------------------------------------------------------------
safe_name <- function(x) {
  x %>%
    str_replace_all("[^A-Za-z0-9]+", "_") %>%
    str_replace_all("_+", "_") %>%
    str_replace_all("^_|_$", "")
}

# -------------------------------------------------------------
# Helper: run coint.test and return tidy table (type 1/2/3)
# IMPORTANT: coint.test returns something printable as table
# -------------------------------------------------------------
run_coint_tidy <- function(x, y, d = 0) {
  
  # minimal length guard
  if (length(x) < 24 || length(y) < 24) {
    return(tibble(
      type = NA_character_,
      lag = NA_integer_,
      EG = NA_real_,
      p.value = NA_real_,
      d = d,
      n_obs = length(x)
    ))
  }
  
  ct <- aTSA::coint.test(x, y, d = d)
  
  tab <- as.data.frame(ct)
  
  # standardize names expected from aTSA output
  # usually: lag, EG, p.value (and rownames are "type 1/2/3")
  tab <- tab %>%
    tibble::rownames_to_column("type") %>%
    as_tibble()
  
  # ensure columns exist and are numeric
  tab %>%
    mutate(
      lag = as.integer(lag),
      EG = as.numeric(EG),
      p.value = as.numeric(p.value),
      d = d,
      n_obs = length(x)
    ) %>%
    select(type, lag, EG, p.value, d, n_obs)
}

# -------------------------------------------------------------
# Loop: for each pair, run tests by city + save plot
# -------------------------------------------------------------
pairs_tbl <- df %>% distinct(articulo_ipc, alimento_sipsa) %>%
  arrange(articulo_ipc, alimento_sipsa)

results <- list()

for (i in seq_len(nrow(pairs_tbl))) {

  art_i <- pairs_tbl$articulo_ipc[i]
  ali_i <- pairs_tbl$alimento_sipsa[i]
  
  dat_pair <- df %>%
    filter(articulo_ipc == art_i, alimento_sipsa == ali_i) %>%
    mutate(city = recode(cod_mun, !!!city_labs))
  
  # ---- Cointegration tests by city (keeps type 1/2/3 rows)
  res_pair <- dat_pair %>%
    group_by(cod_mun, city) %>%
    group_modify(~{
      out <- run_coint_tidy(.x$l_ipc, .x$l_sipsa, d = 0)
      out
    }) %>%
    ungroup() %>%
    mutate(
      articulo_ipc = art_i,
      alimento_sipsa = ali_i
    ) %>%
    select(articulo_ipc, alimento_sipsa, cod_mun, city,
           type, lag, EG, p.value, d, n_obs)
  
  results[[length(results) + 1]] <- res_pair
  
  # ---- Plot: log retail vs log wholesale, facet by city
  plot_dat <- dat_pair %>%
    select(city, date, l_ipc, l_sipsa) %>%
    pivot_longer(c(l_ipc, l_sipsa),
                 names_to = "series", values_to = "log_price") %>%
    mutate(series = recode(series,
                           "l_ipc" = "Retail (IPC) log(price)",
                           "l_sipsa" = "Wholesale (SIPSA) log(price)"))
  
  p <- ggplot(plot_dat, aes(date, log_price, linetype = series, 
                            group = series)) +
    geom_line(linewidth = 0.7) +
    facet_wrap(~ city, ncol = 1, scales = "free_y") +
    labs(
      title = paste0("Wholesale vs Retail (log prices): ", art_i, " ↔ ", ali_i),
      x = NULL, y = "log(price)", linetype = NULL
    ) +
    theme_minimal(base_size = 12)
  
  file_png <- file.path(plot_dir,
                        paste0(date_tag, "_", safe_name(art_i), "__", safe_name(ali_i), ".png"))
  ggsave(file_png, p, width = 11, height = 8, dpi = 300)
}

coint_results <- bind_rows(results) %>%
  arrange(articulo_ipc, alimento_sipsa, cod_mun, type)

# -------------------------------------------------------------
# Save results
# -------------------------------------------------------------
out_xlsx <- file.path(out_dir, paste0(date_tag, "_coint_results_logprices.xlsx"))
out_csv  <- file.path(out_dir, paste0(date_tag, "_coint_results_logprices.csv"))

write_xlsx(list("coint_test_logprices" = coint_results), out_xlsx)
readr::write_csv(coint_results, out_csv)

