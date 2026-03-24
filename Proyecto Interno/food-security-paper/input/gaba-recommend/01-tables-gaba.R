########################################################
## Procesamiento de las recomendaciones GABAS
## Formatear las tablas del documento técnico
## Versión extendida: 8 hojas
########################################################

# Librerías
library(tidyverse)
library(readxl)

# Directorio base
base_dir = "C:\\Users\\Portatil\\Desktop\\Least-cost-diets-and-affordability\\Proyecto Interno"

# Input de las tablas gabas
path_gaba = "food-security-paper\\input\\gaba-recommend\\"

##----------------------------------------------------------
## Función auxiliar para recodificar la tabla nutricional
##----------------------------------------------------------

recodificar_tabla_nutricional <- function(df) {
  
  col_grupo        <- "Grupo de alimentos"
  col_edad         <- "edad"
  col_sexo         <- "sexo"
  col_intercambios <- "Nº  Intercambios"
  
  df_work <- df
  names(df_work)[names(df_work) == col_grupo]        <- "grupo_alimentos"
  names(df_work)[names(df_work) == col_edad]         <- "edad"
  names(df_work)[names(df_work) == col_sexo]         <- "sexo"
  names(df_work)[names(df_work) == col_intercambios] <- "n_intercambios"
  
  df_work$grupo_alimentos <- trimws(as.character(df_work$grupo_alimentos))
  
  patron_encabezados <- c(
    "CEREALES, RAÍCES, TUBÉRCULOS Y PLÁTANOS",
    "FRUTAS Y VERDURAS",
    "LECHE Y PRODUCTOS LÁCTEOS",
    "CARNES, HUEVOS, LEGUMINOSAS, FRUTOS SECOS Y SEMILLAS",
    "GRASAS",
    "AZÚCARES"
  )
  
  df_work$tipo_fila <- dplyr::case_when(
    df_work$grupo_alimentos %in% patron_encabezados                              ~ "encabezado_grupo",
    grepl("APORTE TOTAL",   df_work$grupo_alimentos, ignore.case = TRUE)         ~ "aporte_total",
    grepl("RECOMENDACIÓN",  df_work$grupo_alimentos, ignore.case = TRUE)         ~ "recomendacion",
    TRUE                                                                          ~ "dato"
  )
  
  grupo_actual <- NA_character_
  df_work$grupo_principal <- NA_character_
  
  for (i in seq_len(nrow(df_work))) {
    if (df_work$tipo_fila[i] == "encabezado_grupo") {
      grupo_actual <- df_work$grupo_alimentos[i]
    }
    df_work$grupo_principal[i] <- grupo_actual
  }
  
  df_work$grupo_principal[df_work$tipo_fila %in% c("aporte_total", "recomendacion")] <- NA_character_
  
  df_work$subgrupo <- ifelse(df_work$tipo_fila == "dato", df_work$grupo_alimentos, NA_character_)
  
  df_work$subgrupo <- dplyr::case_when(
    grepl("PROMEDIO DEL GRUPO", df_work$subgrupo, ignore.case = TRUE) &
      df_work$grupo_principal == "CEREALES, RAÍCES, TUBÉRCULOS Y PLÁTANOS"        ~ "promedio_cereales",
    grepl("PROM[EI]DIO DEL GRUPO", df_work$subgrupo, ignore.case = TRUE) &
      df_work$grupo_principal == "FRUTAS Y VERDURAS"                              ~ "promedio_frutas_verduras",
    grepl("PROMEDIO DEL SUBGRUPO ENTERO",     df_work$subgrupo, ignore.case = TRUE) ~ "lacteos_entero",
    grepl("PROMEDIO SUB-GRUPO BAJO EN GRASA", df_work$subgrupo, ignore.case = TRUE) ~ "lacteos_bajo_grasa",
    grepl("PROMEDIO DEL SUB.*CARNES MAGRAS",  df_work$subgrupo, ignore.case = TRUE) ~ "carnes_magras_huevos_leguminosas",
    grepl("NUECES Y SEMILLAS",                df_work$subgrupo, ignore.case = TRUE) ~ "nueces_semillas",
    grepl("PROMEDIO DEL SUBGRUPO ALTO EN GRASA", df_work$subgrupo, ignore.case = TRUE) ~ "carnes_alto_grasa",
    grepl("POLIINSATURADAS", df_work$subgrupo, ignore.case = TRUE)                ~ "grasas_poliinsaturadas",
    grepl("MONOINSATURADAS", df_work$subgrupo, ignore.case = TRUE)                ~ "grasas_monoinsaturadas",
    grepl("SATURADAS",       df_work$subgrupo, ignore.case = TRUE)                ~ "grasas_saturadas",
    grepl("AZÚCARES SIMPLES", df_work$subgrupo, ignore.case = TRUE)               ~ "azucares_simples",
    grepl("DULCES Y POSTRES", df_work$subgrupo, ignore.case = TRUE)               ~ "dulces_postres",
    !is.na(df_work$subgrupo) ~ tolower(df_work$subgrupo),
    TRUE ~ NA_character_
  )
  
  cols_meta  <- c("edad", "sexo", "grupo_principal", "subgrupo", "tipo_fila")
  cols_datos <- setdiff(names(df_work), c(cols_meta, "grupo_alimentos", "n_intercambios"))
  if ("n_intercambios" %in% names(df_work)) cols_meta <- c(cols_meta, "n_intercambios")
  
  df_work[, c(cols_meta, cols_datos), drop = FALSE]
}

##----------------------------------------------------------
## Función para separar FRUTAS Y VERDURAS en dos grupos
##----------------------------------------------------------

separar_frutas_verduras <- function(df) {
  fv <- df %>% filter(grupo_principal == "FRUTAS Y VERDURAS")
  
  frutas   <- fv %>% mutate(grupo_principal = "FRUTAS",   n_exchanges = n_exchanges / 2, e_kcal = e_kcal / 2)
  verduras <- fv %>% mutate(grupo_principal = "VERDURAS", n_exchanges = n_exchanges / 2, e_kcal = e_kcal / 2)
  
  df %>%
    filter(grupo_principal != "FRUTAS Y VERDURAS") %>%
    bind_rows(frutas, verduras)
}

##----------------------------------------------------------
## Función para expandir sexo "mixto" a male / female
##----------------------------------------------------------

expandir_sexo <- function(df) {
  if ("mixto" %in% unique(df$sexo)) {
    bind_rows(
      df %>% mutate(sexo = "male"),
      df %>% mutate(sexo = "female")
    )
  } else {
    df
  }
}

##----------------------------------------------------------
## Función principal: procesa UNA hoja
##----------------------------------------------------------

procesar_hoja <- function(path_xlsx, sheet) {
  
  raw <- readxl::read_excel(path_xlsx, sheet = sheet)
  recod <- recodificar_tabla_nutricional(raw)
  
  # --- 2a. Intercambios y energía por grupo ----------------------------------
  exchanges <- recod %>%
    as.data.frame() %>%
    filter(tipo_fila == "dato") %>%
    janitor::clean_names() %>%
    select(edad, sexo, grupo_principal, n_intercambios, energia_kcal) %>%
    group_by(edad, sexo, grupo_principal) %>%
    summarize(
      n_exchanges = sum(as.numeric(n_intercambios), na.rm = TRUE),
      e_kcal      = sum(as.numeric(energia_kcal),   na.rm = TRUE),
      .groups = "drop"
    ) %>%
    separar_frutas_verduras() %>%
    expandir_sexo()
  
  # --- 2b. Aporte total y recomendación de energía ---------------------------
  energia <- recod %>%
    as.data.frame() %>%
    filter(tipo_fila %in% c("aporte_total", "recomendacion")) %>%
    janitor::clean_names() %>%
    select(edad, sexo, tipo_fila, energia_kcal) %>%
    expandir_sexo()
  
  list(exchanges = exchanges, energia = energia)
}

##----------------------------------------------------------
## 1. Definir las 8 hojas a procesar
##----------------------------------------------------------

path_xlsx  <- file.path(base_dir, path_gaba, "tables-gaba.xlsx")
n_sheets   <- 8   # ajusta si el número cambia

##----------------------------------------------------------
## 2. Iterar sobre las 8 hojas y combinar resultados
##----------------------------------------------------------

resultados <- lapply(seq_len(n_sheets), function(sheet) {
  message("Procesando hoja ", sheet, " ...")
  tryCatch(
    procesar_hoja(path_xlsx, sheet),
    error = function(e) {
      warning("Error en hoja ", sheet, ": ", conditionMessage(e))
      NULL
    }
  )
})

# Separar en dos listas y apilar verticalmente
df.exchanges <- resultados %>%
  purrr::keep(~ !is.null(.x)) %>%
  purrr::map("exchanges") %>%
  bind_rows()

df.energia <- resultados %>%
  purrr::keep(~ !is.null(.x)) %>%
  purrr::map("energia") %>%
  bind_rows()

##----------------------------------------------------------
## 3. Guardar archivos
##----------------------------------------------------------

out_gaba = "food-security-paper\\output\\gaba\\"

writexl::write_xlsx(df.exchanges, file.path(base_dir,
                                            out_gaba,
                                            "230326_gaba_exchanges_base.xlsx"))

writexl::write_xlsx(df.energia, file.path(base_dir,
                                            out_gaba,
                                            "230326_gaba_energy_base.xlsx"))

