########################################################
## 05_figures/00_deflator.R
## Builds monthly food CPI deflator — base: December 2018
##
## Source: IPC División 01100000 — Alimentos y bebidas
##         no alcohólicas (DANE)
##         review-food-security-paper/input/prices/IPC.xls
##
## This is the natural deflator for diet costs since
## it tracks the price level of the food basket directly.
## Base: December 2018 (index = 100 by construction).
##
## Writes: PREP_DIR/deflator_monthly.rds
##         PREP_DIR/deflator_monthly.csv
########################################################

source("00_config.R")
library(tidyverse)
library(readxl)
library(janitor)

# -----------------------------------------------------------------------
# Input
# -----------------------------------------------------------------------
in_ipc <- file.path(REVIEW_DIR, "input", "prices", "IPC.xls")

BASE_DATE <- as.Date("2018-12-01")

meses_esp <- c("Ene","Feb","Mar","Abr","May","Jun",
               "Jul","Ago","Sep","Oct","Nov","Dic")

# -----------------------------------------------------------------------
# Load and clean
# -----------------------------------------------------------------------
message("Loading food CPI (División 01100000)...")

ipc_raw <- read_excel(in_ipc) %>%
  clean_names()

# Inspect columns
message(sprintf("  Columns: %s", paste(names(ipc_raw), collapse = ", ")))

# Standardise column names
# Expected: año, mes, niveles_de_canasta, division, grupo, ciudad, numero_indice
ipc_clean <- ipc_raw %>%
  mutate(
    mes_num = match(mes, meses_esp),
    fecha   = as.Date(sprintf("%04d-%02d-01",
                              as.integer(ano),
                              as.integer(mes_num))),
    ipc     = as.numeric(numero_indice),
    ciudad  = case_when(
      grepl("BOGOT", ciudad, ignore.case = TRUE) ~ "BOGOTA",
      grepl("MEDEL", ciudad, ignore.case = TRUE) ~ "MEDELLIN",
      grepl("CALI",  ciudad, ignore.case = TRUE) ~ "CALI",
      TRUE ~ toupper(ciudad))
  ) %>%
  filter(ciudad %in% c("BOGOTA", "MEDELLIN", "CALI"),
         !is.na(fecha), !is.na(ipc)) %>%
  select(ciudad, fecha, ipc)

message(sprintf(
  "  %d city-months | cities: %s | range: %s – %s",
  nrow(ipc_clean),
  paste(sort(unique(ipc_clean$ciudad)), collapse = ", "),
  min(ipc_clean$fecha), max(ipc_clean$fecha)))

# -----------------------------------------------------------------------
# Build deflator
# Base: December 2018 — index = 100 by construction in the source file
# deflator = 100 / ipc_t → multiply nominal cost by deflator → real cost
# -----------------------------------------------------------------------
ipc_base <- ipc_clean %>%
  filter(fecha == BASE_DATE) %>%
  select(ciudad, ipc_base = ipc)

# Sanity: base should be 100
base_vals <- ipc_base$ipc_base
if (!all(abs(base_vals - 100) < 0.1))
  warning("Base values at Dec-2018 differ from 100: ",
          paste(round(base_vals, 2), collapse = ", "))

deflator <- ipc_clean %>%
  left_join(ipc_base, by = "ciudad") %>%
  mutate(
    deflator  = ipc_base / ipc,   # multiply nominal cost by this → real COP
    ipc_index = ipc / ipc_base    # price level relative to Dec-2018
  ) %>%
  filter(!is.na(deflator)) %>%
  arrange(ciudad, fecha)

# National deflator (mean across cities)
deflator_nat <- deflator %>%
  group_by(fecha) %>%
  summarise(
    ipc      = mean(ipc,      na.rm = TRUE),
    ipc_base = mean(ipc_base, na.rm = TRUE),
    deflator = mean(deflator, na.rm = TRUE),
    ipc_index= mean(ipc_index,na.rm = TRUE),
    .groups  = "drop") %>%
  mutate(ciudad = "NACIONAL")

deflator_full <- bind_rows(deflator, deflator_nat) %>%
  arrange(ciudad, fecha)

# -----------------------------------------------------------------------
# Sanity checks
# -----------------------------------------------------------------------
base_check <- deflator %>%
  filter(fecha == BASE_DATE) %>%
  pull(deflator)
stopifnot(all(abs(base_check - 1) < 1e-4))
message("  Base check passed: deflator = 1.0 at December 2018")

# Print inflation summary
infl_summary <- deflator %>%
  filter(fecha %in% c(as.Date("2019-01-01"),
                      as.Date("2021-12-01"),
                      as.Date("2022-12-01"),
                      as.Date("2024-12-01"))) %>%
  select(ciudad, fecha, ipc_index) %>%
  mutate(ipc_index = round(ipc_index, 3))

message("  Food CPI index at key dates (Dec-2018 = 1.0):")
print(infl_summary)

# -----------------------------------------------------------------------
# Save
# -----------------------------------------------------------------------
saveRDS(deflator_full,
        file.path(PREP_DIR, "deflator_monthly.rds"))
write_csv(deflator_full,
          file.path(PREP_DIR, "deflator_monthly.csv"))

message(sprintf("Done. %d rows saved to PREP_DIR.", nrow(deflator_full)))