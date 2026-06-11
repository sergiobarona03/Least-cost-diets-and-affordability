########################################################
## SCRIPT 01 — Load and cache raw data
## Output: CACHE_DIR/*.rds
########################################################

source("00_config.R")
library(tidyverse)
library(readxl)
library(janitor)

# -----------------------------------------------------------------------
# Helper
# -----------------------------------------------------------------------
meses_esp <- c("Ene","Feb","Mar","Abr",
               "May","Jun","Jul","Ago","Sep","Oct","Nov","Dic")

make_date <- function(ano, mes_num)
  as.Date(sprintf("%04d-%02d-01",
                  as.integer(ano), as.integer(mes_num)))

# -----------------------------------------------------------------------
# DANE prices
# -----------------------------------------------------------------------
message("Loading DANE prices...")
dane <- read_excel(IN_PRICES) %>%
  mutate(
    fecha        = make_date(ano, mes_num),
    cod_subclase = paste0("0", substr(codigo_articulo, 1, 6), "0")
  ) %>%
  filter(!is.na(fecha), !is.na(precio_500g),
         nombre_ciudad %in% CITIES_USE)

message(sprintf("  %d rows | %d items | %d cities",
                nrow(dane),
                n_distinct(dane$articulo),
                n_distinct(dane$nombre_ciudad)))

# -----------------------------------------------------------------------
# IPC sub-class series
# -----------------------------------------------------------------------
message("Loading IPC...")
ipc <- map(list(IN_IPC, IN_IPC2, IN_IPC3, IN_IPC4),
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
         ciudad %in% CITIES_USE) %>%
  distinct(ciudad, cod_subclase, fecha, .keep_all = TRUE) %>%
  arrange(ciudad, cod_subclase, fecha)

message(sprintf("  %d rows | %d sub-classes | %s – %s",
                nrow(ipc), n_distinct(ipc$cod_subclase),
                min(ipc$fecha), max(ipc$fecha)))

# -----------------------------------------------------------------------
# Crosswalks
# -----------------------------------------------------------------------
message("Loading crosswalks...")
corr_subclase <- read_excel(IN_CORR1) %>%
  fill(subclase, ipc, .direction = "down") %>%
  mutate(cod_subclase = paste0("0", gasto_basico, "00")) %>%
  select(cod_subclase, subclase) %>%
  mutate(across(everything(), as.character))

corr_producto <- read_excel(IN_CORR2) %>%
  rename(codigo_articulo = cod_dane, subclase = cod_ipc) %>%
  mutate(across(everything(), as.character))

# -----------------------------------------------------------------------
# Save to cache
# -----------------------------------------------------------------------
message("Saving cache to: ", CACHE_DIR)
saveRDS(dane,          file.path(CACHE_DIR, "dane.rds"))
saveRDS(ipc,           file.path(CACHE_DIR, "ipc.rds"))
saveRDS(corr_subclase, file.path(CACHE_DIR, "corr_subclase.rds"))
saveRDS(corr_producto,  file.path(CACHE_DIR, "corr_producto.rds"))

message("Done. Run 02_lambdas.R next.")