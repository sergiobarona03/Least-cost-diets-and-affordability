########################################################
## SCRIPT 02_dataprep/02_eer_3members.R
## Prepara los inputs de EER para el hogar representativo de
## 3 miembros (adulto hombre 31-51, adulta mujer 31-51, niña
## 10-14). Toma como insumo el agregado de EER que
## produce 03-EER-agregados.R.
##
## Representative household:
##   Adult male   31-51
##   Adult female 31-51
##   Female child 10-14
##
## Reads:  eer_output_dir/230726_agg_eer.xlsx
##         FoodpriceR::EER_LL
##         FoodpriceR::UL
##
## Writes: household_dir/household_eer.rds
##         household_dir/household_eer_ll.rds
##         household_dir/household_ul.rds
########################################################

library(tidyverse)
library(readxl)
library(FoodpriceR)

# ============================================================
# Rutas
# ============================================================

dirs <- c(
  "C:/Users/Portatil/Desktop/Least-cost-diets-and-affordability/Proyecto Interno",
  "C:/Users/danie/OneDrive/Escritorio/Least-cost-diets-and-affordability/Proyecto Interno"
)

base_dir <- dirs[dir.exists(dirs)][1]

if (is.na(base_dir)) {
  stop("Ninguno de los directorios existe")
}

eer_output_dir <- file.path(base_dir, "interno/02_dataprep/eer/output")
household_dir  <- file.path(base_dir, "interno/02_dataprep/household eer")

dir.create(household_dir, recursive = TRUE, showWarnings = FALSE)

in_eer <- file.path(eer_output_dir, "230726_agg_eer.xlsx")

# -----------------------------------------------------------------------
# Load EER
# -----------------------------------------------------------------------

message("Preparing EER inputs...")

agg_eer <- read_excel(in_eer)

# Representative household members
# Keep original column names (rango, sex, eer) plus ciudad
# Each model script handles the rename internally
household_eer <- agg_eer %>%
  filter(
    (sex == "Masculino" & rango == "[31,51)") |
      (sex == "Femenino"  & rango == "[31,51)") |
      (sex == "Femenino"  & rango == "[10, 14)")
  ) %>%
  mutate(ciudad = case_when(
    cod_mun == "08001" ~ "BARRANQUILLA",
    cod_mun == "11001" ~ "BOGOTA",
    cod_mun == "68001" ~ "BUCARAMANGA",
    cod_mun == "76001" ~ "CALI",
    cod_mun == "13001" ~ "CARTAGENA",
    cod_mun == "54001" ~ "CUCUTA",
    cod_mun == "73001" ~ "IBAGUE",
    cod_mun == "17001" ~ "MANIZALES",
    cod_mun == "05001" ~ "MEDELLIN",
    cod_mun == "23001" ~ "MONTERIA",
    cod_mun == "52001" ~ "PASTO",
    cod_mun == "66001" ~ "PEREIRA",
    cod_mun == "50001" ~ "VILLAVICENCIO",
    TRUE               ~ cod_mun)) %>%
  filter(ciudad %in% c(
    "BARRANQUILLA", "BOGOTA", "BUCARAMANGA", "CALI", "CARTAGENA",
    "CUCUTA", "IBAGUE", "MANIZALES", "MEDELLIN", "MONTERIA",
    "PASTO", "PEREIRA", "VILLAVICENCIO"
  )) %>%
  as.data.frame()

message(sprintf("  %d household members x %d cities",
                nrow(household_eer) / n_distinct(household_eer$ciudad),
                n_distinct(household_eer$ciudad)))

# -----------------------------------------------------------------------
# EER lower limits and upper limits from FoodpriceR
# -----------------------------------------------------------------------

eer_ll_base <- FoodpriceR::EER_LL %>%
  mutate(Age = recode(Age,
                      "31 a 50 años" = "[31,51)",
                      "9 a 13 años"  = "[10, 14)"))

ul_base <- FoodpriceR::UL %>%
  mutate(Age = recode(Age,
                      "31 a 50 años" = "[31,51)",
                      "9 a 13 años"  = "[10, 14)"))

# Renamed version for merging with FoodpriceR (needs Age/Sex keys)
household_eer_renamed <- household_eer %>%
  rename(Age = rango, Sex = sex, Energy = eer) %>%
  mutate(Sex = if_else(Sex == "Masculino", 0L, 1L))

# Merge EER + lower limits
household_eer_ll <- merge(
  household_eer_renamed,
  eer_ll_base %>% select(-Energy),
  by = c("Sex", "Age"))

# Merge EER + upper limits (replace NA con número grande)
household_ul <- merge(
  household_eer_renamed,
  ul_base %>% select(-Energy),
  by = c("Sex", "Age")) %>%
  select(-Energy) %>%
  mutate(across(where(is.numeric), ~ replace_na(.x, 9999999)))

message(sprintf(
  "  household_eer: %d rows | eer_ll: %d rows | ul: %d rows",
  nrow(household_eer), nrow(household_eer_ll), nrow(household_ul)))

# -----------------------------------------------------------------------
# Save
# -----------------------------------------------------------------------

saveRDS(household_eer,    file.path(household_dir, "household_eer.rds"))
saveRDS(household_eer_ll, file.path(household_dir, "household_eer_ll.rds"))
saveRDS(household_ul,     file.path(household_dir, "household_ul.rds"))

message("Done. household_eer, household_eer_ll y household_ul guardados en ", household_dir)