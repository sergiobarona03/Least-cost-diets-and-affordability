# =========================================================
# Composición demográfica más frecuente por tamaño de hogar
# Todas las ciudades disponibles
# =========================================================

library(dplyr)
library(stringr)
library(purrr)
library(writexl)

base_dir <- "C:/Users/danie/OneDrive/Escritorio/Least-cost-diets-and-affordability/Proyecto Interno/CENSO"

per_dir <- file.path(base_dir, "personas")
hog_dir <- file.path(base_dir, "hogares")

# Bases de personas
archivos_per <- c(
  "CNPV2018_5PER_A2_05.rds",
  "CNPV2018_5PER_A2_08.rds",
  "CNPV2018_5PER_A2_11.rds",
  "CNPV2018_5PER_A2_13.rds",
  "CNPV2018_5PER_A2_17.rds",
  "CNPV2018_5PER_A2_23.rds",
  "CNPV2018_5PER_A2_50.rds",
  "CNPV2018_5PER_A2_52.rds",
  "CNPV2018_5PER_A2_54.rds",
  "CNPV2018_5PER_A2_66.rds",
  "CNPV2018_5PER_A2_68.rds",
  "CNPV2018_5PER_A2_73.rds",
  "CNPV2018_5PER_A2_76.rds"
)

# Bases de hogares
archivos_hog <- c(
  "CNPV2018_2HOG_A2_05.rds",
  "CNPV2018_2HOG_A2_08.rds",
  "CNPV2018_2HOG_A2_11.rds",
  "CNPV2018_2HOG_A2_13.rds",
  "CNPV2018_2HOG_A2_17.rds",
  "CNPV2018_2HOG_A2_23.rds",
  "CNPV2018_2HOG_A2_50.rds",
  "CNPV2018_2HOG_A2_52.rds",
  "CNPV2018_2HOG_A2_54.rds",
  "CNPV2018_2HOG_A2_66.rds",
  "CNPV2018_2HOG_A2_68.rds",
  "CNPV2018_2HOG_A2_73.rds",
  "CNPV2018_2HOG_A2_76.rds"
)

rutas <- tibble(
  dpto = str_extract(archivos_per, "\\d{2}(?=\\.rds$)"),
  path_per = file.path(per_dir, archivos_per),
  path_hog = file.path(hog_dir, archivos_hog)
)

ciudades <- c(
  "05" = "Medellín",
  "08" = "Barranquilla",
  "11" = "Bogotá",
  "13" = "Cartagena",
  "17" = "Manizales",
  "23" = "Montería",
  "50" = "Villavicencio",
  "52" = "Pasto",
  "54" = "Cúcuta",
  "66" = "Pereira",
  "68" = "Bucaramanga",
  "73" = "Ibagué",
  "76" = "Cali"
)

edad_lab <- c(
  "1" = "0-4", "2" = "5-9", "3" = "10-14", "4" = "15-19",
  "5" = "20-24", "6" = "25-29", "7" = "30-34", "8" = "35-39",
  "9" = "40-44", "10" = "45-49", "11" = "50-54", "12" = "55-59",
  "13" = "60-64", "14" = "65-69", "15" = "70-74", "16" = "75-79",
  "17" = "80-84", "18" = "85-89", "19" = "90-94", "20" = "95-99",
  "21" = "100 o más"
)

read_per <- function(path_file) {
  readRDS(path_file) %>%
    mutate(
      U_DPTO = str_pad(as.character(U_DPTO), 2, pad = "0"),
      U_MPIO = str_pad(as.character(U_MPIO), 3, pad = "0"),
      P_NROHOG = as.numeric(P_NROHOG),
      P_SEXO = as.numeric(P_SEXO),
      P_EDADR = as.numeric(P_EDADR)
    )
}

read_hog <- function(path_file) {
  readRDS(path_file) %>%
    mutate(
      U_DPTO = str_pad(as.character(U_DPTO), 2, pad = "0"),
      U_MPIO = str_pad(as.character(U_MPIO), 3, pad = "0"),
      H_NROHOG = as.numeric(H_NROHOG)
    )
}

procesar_hogar_promedio <- function(path_per, path_hog, cod_dpto) {
  
  nombre_ciudad <- ciudades[cod_dpto]
  
  per <- read_per(path_per) %>%
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
  
  hog <- read_hog(path_hog) %>%
    filter(U_MPIO == "001")
  
  tam_hogar <- per %>%
    count(U_DPTO, U_MPIO, COD_ENCUESTAS, U_VIVIENDA, P_NROHOG, name = "n_personas")
  
  hogares_base <- hog %>%
    select(U_DPTO, U_MPIO, COD_ENCUESTAS, U_VIVIENDA, H_NROHOG) %>%
    rename(P_NROHOG = H_NROHOG)
  
  tam_hogar <- hogares_base %>%
    left_join(
      tam_hogar,
      by = c("U_DPTO", "U_MPIO", "COD_ENCUESTAS", "U_VIVIENDA", "P_NROHOG")
    ) %>%
    mutate(n_personas = if_else(is.na(n_personas), 0L, as.integer(n_personas)))
  
  tam_promedio <- mean(tam_hogar$n_personas, na.rm = TRUE)
  tam_objetivo <- round(tam_promedio)
  
  hogares_objetivo <- tam_hogar %>%
    filter(n_personas == tam_objetivo)
  
  per_obj <- per %>%
    inner_join(
      hogares_objetivo,
      by = c("U_DPTO", "U_MPIO", "COD_ENCUESTAS", "U_VIVIENDA", "P_NROHOG")
    )
  
  resumen <- tibble(
    city = nombre_ciudad,
    dpto = cod_dpto,
    tam_promedio_hogar = round(tam_promedio, 2),
    tam_representativo = tam_objetivo,
    n_hogares_usados = nrow(hogares_objetivo)
  )
  
  composicion <- per_obj %>%
    count(sexo, edad_grupo, sort = TRUE) %>%
    mutate(
      city = nombre_ciudad,
      dpto = cod_dpto,
      tam_representativo = tam_objetivo,
      participacion = round(n / sum(n) * 100, 2)
    ) %>%
    select(city, dpto, tam_representativo, sexo, edad_grupo, n, participacion)
  
  composicion_hogar <- per_obj %>%
    count(U_DPTO, U_MPIO, COD_ENCUESTAS, U_VIVIENDA, P_NROHOG, sexo, edad_grupo) %>%
    arrange(U_DPTO, U_MPIO, COD_ENCUESTAS, U_VIVIENDA, P_NROHOG, sexo, edad_grupo) %>%
    group_by(U_DPTO, U_MPIO, COD_ENCUESTAS, U_VIVIENDA, P_NROHOG) %>%
    summarise(
      composicion = paste0(sexo, "_", edad_grupo, "=", n, collapse = "; "),
      .groups = "drop"
    ) %>%
    count(composicion, sort = TRUE) %>%
    mutate(
      city = nombre_ciudad,
      dpto = cod_dpto,
      tam_representativo = tam_objetivo
    ) %>%
    select(city, dpto, tam_representativo, composicion, hogares = n)
  
  list(
    resumen = resumen,
    composicion = composicion,
    composicion_hogar = composicion_hogar
  )
}

resultados <- pmap(
  list(rutas$path_per, rutas$path_hog, rutas$dpto),
  procesar_hogar_promedio
)

resumen_hogares <- bind_rows(map(resultados, "resumen"))

composicion_demografica <- bind_rows(map(resultados, "composicion"))

composicion_hogar_frecuente <- bind_rows(map(resultados, "composicion_hogar")) %>%
  group_by(city) %>%
  slice_max(order_by = hogares, n = 10, with_ties = FALSE) %>%
  ungroup()

print(resumen_hogares)
print(composicion_demografica)
print(composicion_hogar_frecuente)

write_xlsx(
  list(
    resumen_hogares = resumen_hogares,
    composicion_demografica = composicion_demografica,
    composicion_hogar_frecuente = composicion_hogar_frecuente
  ),
  path = file.path(base_dir, "comp_hogar_prom.xlsx")
)