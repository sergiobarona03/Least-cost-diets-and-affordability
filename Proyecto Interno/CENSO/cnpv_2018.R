# =========================================================
# Composición demográfica más frecuente por tamaño de hogar
# Bogotá, Medellín y Cali
# =========================================================

library(readr)
library(dplyr)
library(stringr)

# Directorio
base_dir <- "C:/Users/danie/OneDrive/Escritorio/Least-cost-diets-and-affordability/Proyecto Interno/CENSO"

# Rutas
path_per_bog <- file.path(base_dir, "CNPV2018_5PER_A2_11.rds")
path_per_med <- file.path(base_dir, "CNPV2018_5PER_A2_05.rds")
path_per_cal <- file.path(base_dir, "CNPV2018_5PER_A2_76.rds")

# Leer personas desde .rds
read_per <- function(path_file){
  readRDS(path_file) %>%
    mutate(
      U_DPTO = str_pad(as.character(U_DPTO), 2, pad = "0"),
      U_MPIO = str_pad(as.character(U_MPIO), 3, pad = "0"),
      P_NROHOG = as.numeric(P_NROHOG),
      P_SEXO = as.numeric(P_SEXO),
      P_EDADR = as.numeric(P_EDADR)
    )
}

procesar_hogar_promedio <- function(path_file, nombre_ciudad){
  
  per <- read_per(path_file) %>%
    filter(U_MPIO == "001") %>%
    mutate(
      sexo = case_when(
        P_SEXO == 1 ~ "Hombre",
        P_SEXO == 2 ~ "Mujer",
        TRUE ~ NA_character_
      ),
      edad_grupo = edad_lab[as.character(P_EDADR)]
    ) %>%
    filter(!is.na(sexo), !is.na(edad_grupo))
  
  # Tamaño de cada hogar
  tam_hogar <- per %>%
    dplyr::count(U_DPTO, U_MPIO, COD_ENCUESTAS, U_VIVIENDA, P_NROHOG, name = "n_personas")
  
  # Tamaño promedio del hogar en la ciudad
  tam_promedio <- mean(tam_hogar$n_personas, na.rm = TRUE)
  
  # Entero más cercano para representar el hogar promedio
  tam_objetivo <- round(tam_promedio)
  
  # Hogares con ese tamaño
  hogares_objetivo <- tam_hogar %>%
    filter(n_personas == tam_objetivo)
  
  # Personas de esos hogares
  per_obj <- per %>%
    inner_join(
      hogares_objetivo,
      by = c("U_DPTO", "U_MPIO", "COD_ENCUESTAS", "U_VIVIENDA", "P_NROHOG")
    )
  
  # Resumen general
  resumen <- hogares_objetivo %>%
    dplyr::summarise(
      city = nombre_ciudad,
      tam_promedio_hogar = round(tam_promedio, 2),
      tam_representativo = tam_objetivo,
      n_hogares_usados = n()
    )
  
  # Composición demográfica: sexo + rango de edad
  composicion <- per_obj %>%
    dplyr::count(sexo, edad_grupo, sort = TRUE) %>%
    mutate(
      city = nombre_ciudad,
      tam_representativo = tam_objetivo,
      participacion = round(n / sum(n) * 100, 2)
    ) %>%
    dplyr::select(city, tam_representativo, sexo, edad_grupo, n, participacion)
  
  # Composición completa del hogar, para ver la estructura más frecuente
  composicion_hogar <- per_obj %>%
    dplyr::count(U_DPTO, U_MPIO, COD_ENCUESTAS, U_VIVIENDA, P_NROHOG, sexo, edad_grupo) %>%
    dplyr::arrange(U_DPTO, U_MPIO, COD_ENCUESTAS, U_VIVIENDA, P_NROHOG, sexo, edad_grupo) %>%
    dplyr::group_by(U_DPTO, U_MPIO, COD_ENCUESTAS, U_VIVIENDA, P_NROHOG) %>%
    dplyr::summarise(
      composicion = paste0(sexo, "_", edad_grupo, "=", n, collapse = "; "),
      .groups = "drop"
    ) %>%
    dplyr::count(composicion, sort = TRUE) %>%
    mutate(
      city = nombre_ciudad,
      tam_representativo = tam_objetivo
    ) %>%
    dplyr::select(city, tam_representativo, composicion, hogares = n)
  
  list(
    resumen = resumen,
    composicion = composicion,
    composicion_hogar = composicion_hogar
  )
}

# Ejecutar por ciudad
hogar_bog <- procesar_hogar_promedio(path_per_bog, "Bogotá")
hogar_med <- procesar_hogar_promedio(path_per_med, "Medellín")
hogar_cal <- procesar_hogar_promedio(path_per_cal, "Cali")

# Composición demográfica detallada por ciudad
composicion_demografica <- bind_rows(
  hogar_bog$composicion,
  hogar_med$composicion,
  hogar_cal$composicion
)

print(composicion_demografica)

# Estructura más frecuente del hogar promedio por ciudad
composicion_hogar_frecuente <- bind_rows(
  hogar_bog$composicion_hogar %>% slice(1:10),
  hogar_med$composicion_hogar %>% slice(1:10),
  hogar_cal$composicion_hogar %>% slice(1:10)
)

print(composicion_hogar_frecuente)

write_xlsx(
  composicion_hogar_frecuente,
  path = file.path(base_dir, "composicion_hogar_promedio.xlsx")
)
