########################################################
## SCRIPT 01_data_preparation/03_gaba_base.R
## Procesa las tablas GABA del documento técnico
## (intercambios por grupo de alimentos y energía base)
##
## Reads:  BASE_DIR/review-food-security-paper/input/
##           gaba-recommend/tables-gaba.xlsx
## Writes: PREP_DIR/gaba_exchanges_base.rds
##         PREP_DIR/gaba_energy_base.rds
########################################################

source("00_config.R")
library(tidyverse)
library(readxl)
library(janitor)

# -----------------------------------------------------------------------
# Input — GABA tables live in the review paper input folder
# -----------------------------------------------------------------------
in_gaba <- file.path(REVIEW_DIR, "input", "gaba-recommend", "tables-gaba.xlsx")

# -----------------------------------------------------------------------
# Auxiliary functions (unchanged from original)
# -----------------------------------------------------------------------
recodificar_tabla_nutricional <- function(df) {
  
  df_work <- df
  names(df_work)[names(df_work) == "Grupo de alimentos"] <- "grupo_alimentos"
  names(df_work)[names(df_work) == "edad"]               <- "edad"
  names(df_work)[names(df_work) == "sexo"]               <- "sexo"
  names(df_work)[names(df_work) == "Nº  Intercambios"]   <- "n_intercambios"
  
  df_work$grupo_alimentos <- trimws(as.character(df_work$grupo_alimentos))
  
  patron_encabezados <- c(
    "CEREALES, RAÍCES, TUBÉRCULOS Y PLÁTANOS",
    "FRUTAS Y VERDURAS",
    "LECHE Y PRODUCTOS LÁCTEOS",
    "CARNES, HUEVOS, LEGUMINOSAS, FRUTOS SECOS Y SEMILLAS",
    "GRASAS", "AZÚCARES")
  
  df_work$tipo_fila <- case_when(
    df_work$grupo_alimentos %in% patron_encabezados                      ~ "encabezado_grupo",
    grepl("APORTE TOTAL",  df_work$grupo_alimentos, ignore.case = TRUE)  ~ "aporte_total",
    grepl("RECOMENDACIÓN", df_work$grupo_alimentos, ignore.case = TRUE)  ~ "recomendacion",
    TRUE ~ "dato")
  
  grupo_actual <- NA_character_
  df_work$grupo_principal <- NA_character_
  for (i in seq_len(nrow(df_work))) {
    if (df_work$tipo_fila[i] == "encabezado_grupo")
      grupo_actual <- df_work$grupo_alimentos[i]
    df_work$grupo_principal[i] <- grupo_actual
  }
  df_work$grupo_principal[
    df_work$tipo_fila %in% c("aporte_total","recomendacion")] <- NA_character_
  
  df_work$subgrupo <- ifelse(df_work$tipo_fila == "dato",
                             df_work$grupo_alimentos, NA_character_)
  
  df_work$subgrupo <- case_when(
    grepl("PROMEDIO DEL GRUPO", df_work$subgrupo, ignore.case=TRUE) &
      df_work$grupo_principal == "CEREALES, RAÍCES, TUBÉRCULOS Y PLÁTANOS" ~ "promedio_cereales",
    grepl("PROM[EI]DIO DEL GRUPO", df_work$subgrupo, ignore.case=TRUE) &
      df_work$grupo_principal == "FRUTAS Y VERDURAS"                       ~ "promedio_frutas_verduras",
    grepl("PROMEDIO DEL SUBGRUPO ENTERO",      df_work$subgrupo, ignore.case=TRUE) ~ "lacteos_entero",
    grepl("PROMEDIO SUB-GRUPO BAJO EN GRASA",  df_work$subgrupo, ignore.case=TRUE) ~ "lacteos_bajo_grasa",
    grepl("PROMEDIO DEL SUB.*CARNES MAGRAS",   df_work$subgrupo, ignore.case=TRUE) ~ "carnes_magras_huevos_leguminosas",
    grepl("NUECES Y SEMILLAS",                 df_work$subgrupo, ignore.case=TRUE) ~ "nueces_semillas",
    grepl("PROMEDIO DEL SUBGRUPO ALTO EN GRASA",df_work$subgrupo,ignore.case=TRUE) ~ "carnes_alto_grasa",
    grepl("POLIINSATURADAS", df_work$subgrupo, ignore.case=TRUE) ~ "grasas_poliinsaturadas",
    grepl("MONOINSATURADAS", df_work$subgrupo, ignore.case=TRUE) ~ "grasas_monoinsaturadas",
    grepl("SATURADAS",       df_work$subgrupo, ignore.case=TRUE) ~ "grasas_saturadas",
    grepl("AZÚCARES SIMPLES",df_work$subgrupo, ignore.case=TRUE) ~ "azucares_simples",
    grepl("DULCES Y POSTRES",df_work$subgrupo, ignore.case=TRUE) ~ "dulces_postres",
    !is.na(df_work$subgrupo) ~ tolower(df_work$subgrupo),
    TRUE ~ NA_character_)
  
  cols_meta  <- c("edad", "sexo", "grupo_principal", "subgrupo", "tipo_fila")
  if ("n_intercambios" %in% names(df_work))
    cols_meta <- c(cols_meta, "n_intercambios")
  cols_datos <- setdiff(names(df_work),
                        c(cols_meta, "grupo_alimentos", "n_intercambios"))
  df_work[, c(cols_meta, cols_datos), drop = FALSE]
}

separar_frutas_verduras <- function(df) {
  fv       <- df %>% filter(grupo_principal == "FRUTAS Y VERDURAS")
  frutas   <- fv %>% mutate(grupo_principal = "FRUTAS",
                            n_exchanges = n_exchanges / 2,
                            e_kcal      = e_kcal / 2)
  verduras <- fv %>% mutate(grupo_principal = "VERDURAS",
                            n_exchanges = n_exchanges / 2,
                            e_kcal      = e_kcal / 2)
  df %>%
    filter(grupo_principal != "FRUTAS Y VERDURAS") %>%
    bind_rows(frutas, verduras)
}

expandir_sexo <- function(df) {
  if ("mixto" %in% unique(df$sexo)) {
    bind_rows(df %>% mutate(sexo = "male"),
              df %>% mutate(sexo = "female"))
  } else df
}

procesar_hoja <- function(path_xlsx, sheet) {
  raw   <- read_excel(path_xlsx, sheet = sheet)
  recod <- recodificar_tabla_nutricional(raw)
  
  exchanges <- recod %>%
    as.data.frame() %>%
    filter(tipo_fila == "dato") %>%
    clean_names() %>%
    select(edad, sexo, grupo_principal, n_intercambios, energia_kcal) %>%
    group_by(edad, sexo, grupo_principal) %>%
    summarise(
      n_exchanges = sum(as.numeric(n_intercambios), na.rm=TRUE),
      e_kcal      = sum(as.numeric(energia_kcal),   na.rm=TRUE),
      .groups = "drop") %>%
    separar_frutas_verduras() %>%
    expandir_sexo()
  
  energia <- recod %>%
    as.data.frame() %>%
    filter(tipo_fila %in% c("aporte_total", "recomendacion")) %>%
    clean_names() %>%
    select(edad, sexo, tipo_fila, energia_kcal) %>%
    expandir_sexo()
  
  list(exchanges = exchanges, energia = energia)
}

# -----------------------------------------------------------------------
# Process all sheets
# -----------------------------------------------------------------------
n_sheets <- 8
message(sprintf("Processing %d GABA sheets from: %s", n_sheets, in_gaba))

resultados <- lapply(seq_len(n_sheets), function(sheet) {
  message("  Sheet ", sheet, "...")
  tryCatch(
    procesar_hoja(in_gaba, sheet),
    error = function(e) {
      warning("Error in sheet ", sheet, ": ", conditionMessage(e))
      NULL
    })
})

df.exchanges <- resultados %>%
  keep(~ !is.null(.x)) %>%
  map("exchanges") %>%
  bind_rows()

df.energia <- resultados %>%
  keep(~ !is.null(.x)) %>%
  map("energia") %>%
  bind_rows()

message(sprintf("  exchanges: %d rows | energia: %d rows",
                nrow(df.exchanges), nrow(df.energia)))

# -----------------------------------------------------------------------
# Save
# -----------------------------------------------------------------------
saveRDS(df.exchanges, file.path(PREP_DIR, "gaba_exchanges_base.rds"))
saveRDS(df.energia,   file.path(PREP_DIR, "gaba_energy_base.rds"))
writexl::write_xlsx(df.exchanges,
                    file.path(PREP_DIR, "gaba_exchanges_base.xlsx"))
writexl::write_xlsx(df.energia,
                    file.path(PREP_DIR, "gaba_energy_base.xlsx"))

message("Done. Run 04_gaba_adjusted.R next.")
