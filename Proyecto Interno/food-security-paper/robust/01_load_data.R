########################################################
## SCRIPT 01 — Load and cache raw data
##
## Reads:
##   - DANE prices (precios_unadj_DANE_1999_2018.xlsx)
##   - IPC sub-class series (IPC.xls … IPC_4.xls)
##   - Crosswalk files (correlativa_ipc*.xlsx)
##
## Writes to .../price_forecasting/cache/:
##   dane.rds
##   ipc.rds
##   corr_subclase.rds
##   corr_producto.rds
##
## All downstream scripts source this cache.
## Re-run only when raw data changes.
########################################################

library(tidyverse)
library(readxl)
library(janitor)

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

in_prices <- file.path(base_dir,
                       "Precios DANE/OUTPUT_DANE/precios_unadj_DANE_1999_2018.xlsx")
in_ipc    <- file.path(base_dir, "var-ipc/IPC.xls")
in_ipc2   <- file.path(base_dir, "var-ipc/IPC_2.xls")
in_ipc3   <- file.path(base_dir, "var-ipc/IPC_3.xls")
in_ipc4   <- file.path(base_dir, "var-ipc/IPC_4.xls")
in_corr1  <- file.path(base_dir, "var-ipc/correlativa_ipc.xlsx")
in_corr2  <- file.path(base_dir, "var-ipc/correlativa_ipc_articulos.xlsx")

cache_dir <- file.path(base_dir,
                       "food-security-paper/output/price_forecasting/cache")
dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

# -----------------------------------------------------------------------
# 2. Parameters
# -----------------------------------------------------------------------
cities_use <- c("CALI", "BOGOTÁ D.C.", "MEDELLÍN")

meses_esp <- c("Ene","Feb","Mar","Abr",
               "May","Jun","Jul","Ago","Sep","Oct","Nov","Dic")

make_date <- function(ano, mes_num)
  as.Date(sprintf("%04d-%02d-01",
                  as.integer(ano), as.integer(mes_num)))

# -----------------------------------------------------------------------
# 3. DANE prices
# -----------------------------------------------------------------------
message("Loading DANE prices...")

dane <- read_excel(in_prices) %>%
  mutate(
    fecha        = make_date(ano, mes_num),
    cod_subclase = paste0("0", substr(codigo_articulo, 1, 6), "0")
  ) %>%
  filter(!is.na(fecha), !is.na(precio_500g),
         nombre_ciudad %in% cities_use)

message(sprintf("  %d rows | %d items | %d cities",
                nrow(dane),
                n_distinct(dane$articulo),
                n_distinct(dane$nombre_ciudad)))

# -----------------------------------------------------------------------
# 4. IPC sub-class series
# -----------------------------------------------------------------------
message("Loading IPC series...")

ipc <- map(list(in_ipc, in_ipc2, in_ipc3, in_ipc4),
           ~ read_excel(.x) %>% clean_names()) %>%
  bind_rows() %>%
  mutate(
    ciudad = case_when(
      ciudad == "CARTAGENA DE INDIAS" ~ "CARTAGENA",
      ciudad == "BOGOTÁ, D.C."        ~ "BOGOTÁ D.C.",
      TRUE ~ ciudad),
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

message(sprintf("  %d rows | %d sub-classes | date range: %s – %s",
                nrow(ipc),
                n_distinct(ipc$cod_subclase),
                min(ipc$fecha), max(ipc$fecha)))

# -----------------------------------------------------------------------
# 5. Crosswalk files
# -----------------------------------------------------------------------
message("Loading crosswalks...")

corr_subclase <- read_excel(in_corr1) %>%
  fill(subclase, ipc, .direction = "down") %>%
  mutate(cod_subclase = paste0("0", gasto_basico, "00")) %>%
  select(cod_subclase, subclase) %>%
  mutate(across(everything(), as.character))

corr_producto <- read_excel(in_corr2) %>%
  rename(codigo_articulo = cod_dane, subclase = cod_ipc) %>%
  mutate(across(everything(), as.character))

# -----------------------------------------------------------------------
# 6. Save cache
# -----------------------------------------------------------------------
message("Saving cache...")

saveRDS(dane,          file.path(cache_dir, "dane.rds"))
saveRDS(ipc,           file.path(cache_dir, "ipc.rds"))
saveRDS(corr_subclase, file.path(cache_dir, "corr_subclase.rds"))
saveRDS(corr_producto,  file.path(cache_dir, "corr_producto.rds"))

message(sprintf("Cache saved to: %s", cache_dir))
message("Done. Run Script 02 next.")