# =========================================================
# Tamaño promedio del hogar - CNPV 2018
# Bogotá, Medellín y Cali
# =========================================================

library(readr)
library(dplyr)
library(stringr)

#----------------------------------------------------------
# 1. Directorio
#----------------------------------------------------------

base_dir <- "C:/Users/danie/OneDrive/Escritorio/Least-cost-diets-and-affordability/Proyecto Interno/CENSO"

path_bog <- file.path(base_dir, "CNPV2018_2HOG_A2_11.CSV")
path_ant <- file.path(base_dir, "CNPV2018_2HOG_A2_05.CSV")
path_val <- file.path(base_dir, "CNPV2018_2HOG_A2_76.CSV")

#----------------------------------------------------------
# 2. Función para leer módulo hogar
#----------------------------------------------------------

read_hog <- function(path_file){
  
  read_csv(path_file, show_col_types = FALSE) %>%
    mutate(
      U_DPTO = str_pad(as.character(U_DPTO), 2, pad = "0"),
      U_MPIO = str_pad(as.character(U_MPIO), 3, pad = "0"),
      HA_TOT_PER = as.numeric(HA_TOT_PER)
    ) %>%
    distinct(
      U_DPTO,
      U_MPIO,
      COD_ENCUESTAS,
      U_VIVIENDA,
      H_NROHOG,
      .keep_all = TRUE
    ) %>%
    filter(!is.na(HA_TOT_PER), HA_TOT_PER > 0)
  
}

#----------------------------------------------------------
# 3. Leer bases
#----------------------------------------------------------

bogota_hog <- read_hog(path_bog)
antioquia_hog <- read_hog(path_ant)
valle_hog <- read_hog(path_val)

#----------------------------------------------------------
# 4. Filtrar ciudades
#----------------------------------------------------------

# Bogotá
bogota <- bogota_hog %>%
  filter(U_MPIO == "001")

# Medellín
medellin <- antioquia_hog %>%
  filter(U_MPIO == "001")

# Cali
cali <- valle_hog %>%
  filter(U_MPIO == "001")

#----------------------------------------------------------
# 5. Calcular tamaño promedio del hogar
#----------------------------------------------------------

resumen_hogar <- bind_rows(
  
  bogota %>%
    dplyr::summarise(
      city = "Bogotá",
      avg_household_size = mean(HA_TOT_PER, na.rm = TRUE),
      n_households = n()
    ),
  
  medellin %>%
    dplyr::summarise(
      city = "Medellín",
      avg_household_size = mean(HA_TOT_PER, na.rm = TRUE),
      n_households = n()
    ),
  
  cali %>%
    dplyr::summarise(
      city = "Cali",
      avg_household_size = mean(HA_TOT_PER, na.rm = TRUE),
      n_households = n()
    )
  
) %>%
  dplyr::mutate(
    avg_household_size = round(avg_household_size, 2)
  )

print(resumen_hogar)