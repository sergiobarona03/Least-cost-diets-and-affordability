########################################################
## SCRIPT 01_data_preparation/05_eer.R
## Prepara los inputs de EER para todos los modelos
## (household_eer, household_eer_ll, household_ul)
## Estos son constantes en el tiempo y compartidos
## por CoCA, CoNA, CoRD y CC-CoNA.
##
## Representative household:
##   Adult male   31–51
##   Adult female 31–51
##   Female child 10–14
##
## Reads:  IN_EER (220326_agg_eer.xlsx)
##         FoodpriceR::EER_LL
##         FoodpriceR::UL
##
## Writes: PREP_DIR/household_eer.rds
##         PREP_DIR/household_eer_ll.rds
##         PREP_DIR/household_ul.rds
########################################################

source("00_config.R")
library(tidyverse)
library(readxl)
library(FoodpriceR)

# -----------------------------------------------------------------------
# Load EER
# -----------------------------------------------------------------------
message("Preparing EER inputs...")

agg_eer <- read_excel(IN_EER)

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
    cod_mun == "05001" ~ "MEDELLIN",
    cod_mun == "11001" ~ "BOGOTA",
    cod_mun == "76001" ~ "CALI",
    TRUE               ~ cod_mun)) %>%
  filter(ciudad %in% c("BOGOTA", "MEDELLIN", "CALI")) %>%
  as.data.frame()

message(sprintf("  %d household members × %d cities",
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

# Merge EER + upper limits (replace NA with large number)
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
saveRDS(household_eer,    file.path(PREP_DIR, "household_eer.rds"))
saveRDS(household_eer_ll, file.path(PREP_DIR, "household_eer_ll.rds"))
saveRDS(household_ul,     file.path(PREP_DIR, "household_ul.rds"))

message("Done. All 01_data_preparation scripts complete.")
message("Proceed to 02_models/ scripts.")