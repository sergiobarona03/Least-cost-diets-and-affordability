##################################################################
## Prueba: análisis de la lista de alimentos (definir mapeo)    ##
## Fecha: 31 de julio de 2025                                   ##
##################################################################

# Cargar librerías
library(lubridate)
library(tidyverse)

# Definir directorio de trabajo
setwd("C:\\Users\\sergio.barona\\Desktop\\Least-cost-diets-and-affordability\\Proyecto Interno\\")

date_tag = "261225"

path = paste0("working-papers\\working-paper-aecm\\input\\",
              date_tag, "_dataset_ipc_sipsa.xlsx")

data_merged = readxl::read_excel(path)

# ---------------------------
# 0) Parameters
# ---------------------------
start_date <- as.Date("2013-01-01")
end_date   <- as.Date("2018-03-01")

# Required cities (Cali, Bogotá, Medellín)
cities_required <- c("76001", "11001", "05001")

# Calendar (monthly)
cal <- seq.Date(start_date, end_date, by = "month")
Ttot <- length(cal)

# Choose how to collapse duplicates within city-month-product:
# "mean" or "median"
collapse_fun <- "mean"

# Minimum coverage share if you want "near-full" options (e.g., 0.95)
min_share <- 1.00

# ---------------------------
# 1) Prepare and standardize
# ---------------------------
df0 <- data_merged %>%
  mutate(
    cod_mun = sprintf("%05d", as.integer(cod_mun)),
    Year = as.integer(Year),
    Month = as.integer(Month),
    date = as.Date(sprintf("%d-%02d-01", Year, Month)),
    articulo_ipc = str_squish(as.character(articulo_ipc)),
    alimento_sipsa = str_squish(as.character(alimento_sipsa)),
    codigo_articulo = as.character(codigo_articulo),
    precio_ipc = as.numeric(precio_ipc),
    precio_sipsa = as.numeric(precio_sipsa)
  ) %>%
  filter(date >= start_date, date <= end_date) %>%
  filter(cod_mun %in% cities_required)

# ---------------------------
# 2) Collapse duplicates to ONE observation per:
#    (city, month, articulo_ipc, codigo_articulo, alimento_sipsa)
# ---------------------------
collapse_vec <- function(x) {
  if (all(is.na(x))) return(NA_real_)
  if (collapse_fun == "median") return(stats::median(x, na.rm = TRUE))
  mean(x, na.rm = TRUE)
}

df <- df0 %>%
  group_by(cod_mun, date, articulo_ipc, codigo_articulo, alimento_sipsa) %>%
  summarise(
    n_ipc_obs   = sum(!is.na(precio_ipc)),
    n_sipsa_obs = sum(!is.na(precio_sipsa)),
    precio_ipc_m   = ifelse(n_ipc_obs == 0, NA_real_, collapse_vec(precio_ipc)),
    precio_sipsa_m = ifelse(n_sipsa_obs == 0, NA_real_, collapse_vec(precio_sipsa)),
    .groups = "drop"
  )

# ---------------------------
# 3) Coverage table per pair (articulo_ipc × alimento_sipsa)
#    Rule: full coverage in 3 cities for all months (or >= min_share)
# ---------------------------
coverage_city <- df %>%
  group_by(articulo_ipc, alimento_sipsa, cod_mun) %>%
  summarise(
    months_both = sum(!is.na(precio_ipc_m) & !is.na(precio_sipsa_m)),
    months_any  = n_distinct(date),
    .groups = "drop"
  )

# -------------------------------------------------
# 5. Guardar en ruta
# -------------------------------------------------
path = paste0("working-papers\\working-paper-aecm\\input\\",
              date_tag, "_coverage_ipc_sipsa.xlsx")

writexl::write_xlsx(coverage_city %>% filter(
months_both == 63, months_any == 63  
), path)
