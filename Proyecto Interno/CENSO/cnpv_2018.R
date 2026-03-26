# =========================================================
# Composición demográfica más frecuente por tamaño de hogar
# Bogotá, Medellín y Cali
# =========================================================

library(readr)
library(dplyr)
library(stringr)

# Directorio
base_dir <- "C:/Users/danie/OneDrive/Escritorio/Least-cost-diets-and-affordability/Proyecto Interno/CENSO"

path_per_bog <- file.path(base_dir, "CNPV2018_5PER_A2_11.CSV")
path_per_med <- file.path(base_dir, "CNPV2018_5PER_A2_05.CSV")
path_per_cal <- file.path(base_dir, "CNPV2018_5PER_A2_76.CSV")

# Leer personas
read_per <- function(path_file){
  read_csv(path_file, show_col_types = FALSE) %>%
    mutate(
      U_DPTO = str_pad(as.character(U_DPTO), 2, pad = "0"),
      U_MPIO = str_pad(as.character(U_MPIO), 3, pad = "0"),
      P_NROHOG = as.numeric(P_NROHOG),
      P_SEXO = as.numeric(P_SEXO),
      P_EDADR = as.numeric(P_EDADR)
    )
}

# Etiquetas edad
edad_lab <- c(
  "1" = "00_04", "2" = "05_09", "3" = "10_14", "4" = "15_19",
  "5" = "20_24", "6" = "25_29", "7" = "30_34", "8" = "35_39",
  "9" = "40_44", "10" = "45_49", "11" = "50_54", "12" = "55_59",
  "13" = "60_64", "14" = "65_69", "15" = "70_74", "16" = "75_79",
  "17" = "80_84", "18" = "85_89", "19" = "90_94", "20" = "95_99",
  "21" = "100_mas"
)

# Función por ciudad
procesar_ciudad <- function(path_file, nombre_ciudad){
  
  per <- read_per(path_file) %>%
    filter(U_MPIO == "001") %>%
    mutate(
      sexo = case_when(
        P_SEXO == 1 ~ "H",
        P_SEXO == 2 ~ "M",
        TRUE ~ NA_character_
      ),
      edad = edad_lab[as.character(P_EDADR)],
      grupo = paste0(sexo, "_", edad)
    ) %>%
    filter(!is.na(grupo))
  
  comp_hogar <- per %>%
    dplyr::count(U_DPTO, U_MPIO, COD_ENCUESTAS, U_VIVIENDA, P_NROHOG, grupo) %>%
    dplyr::arrange(U_DPTO, U_MPIO, COD_ENCUESTAS, U_VIVIENDA, P_NROHOG, grupo) %>%
    dplyr::group_by(U_DPTO, U_MPIO, COD_ENCUESTAS, U_VIVIENDA, P_NROHOG) %>%
    dplyr::summarise(
      composicion = paste0(grupo, "=", n, collapse = "; "),
      n_personas = sum(n),
      .groups = "drop"
    )
  
  comp_hogar %>%
    dplyr::mutate(composicion = paste0(composicion, "; total=", n_personas)) %>%
    dplyr::filter(n_personas == 3) %>%
    dplyr::count(composicion, sort = TRUE) %>%
    slice(1:10) %>%
    dplyr::mutate(city = nombre_ciudad, n_personas = 3) %>%
    dplyr::select(city, n_personas, composicion, hogares = n)
}

# Resultado
resultado <- bind_rows(
  procesar_ciudad(path_per_bog, "Bogotá"),
  procesar_ciudad(path_per_med, "Medellín"),
  procesar_ciudad(path_per_cal, "Cali")
)

print(resultado)