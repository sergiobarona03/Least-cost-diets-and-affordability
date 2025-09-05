##########################
# Output IPC 1999 - 2018 #
##########################

# Cargar librerías
library(tidyverse)
library(lubridate)
library(readxl)
library(janitor)
library(writexl)

# Definir directorio de trabajo
setwd("C:/Users/danie/OneDrive/Escritorio/Least-cost-diets-and-affordability/Proyecto Interno/")

# Cargar y procesar datos de precios
dane_data <- read_excel("Precios DANE\\OUTPUT_DANE\\precios_unadj_DANE_1999_2018.xlsx") %>%
  filter(nombre_ciudad == "CALI",
         as.Date(paste(ano, mes_num, "01", sep = "-")) < as.Date("2018-04-01")) %>%
  mutate(fecha = as.Date(paste(ano, mes_num, "01", sep = "-")),
         cod_subclase = paste0("0", substr(codigo_articulo, 1, 6), "0"))

# Cargar y procesar tablas correlativas
correlativa_ipc <- read_excel("var-ipc\\correlativa_ipc.xlsx") %>%
  fill(subclase, ipc, .direction = "down") %>%
  mutate(cod_subclase = paste0("0", gasto_basico, "00"))

correlativa_articulos <- read_excel("var-ipc\\correlativa_ipc_articulos.xlsx") %>%
  mutate(codigo_articulo = cod_dane, subclase = cod_ipc)

# ------- IPC -------
meses_esp <- c("Ene","Feb","Mar","Abr","May","Jun",
               "Jul","Ago","Sep","Oct","Nov","Dic")

## === Lector IPC ===
read_ipc_file <- function(path){
  readxl::read_excel(path) %>%
    janitor::clean_names() %>%
    transmute(
      ciudad = trimws(as.character(ciudad)),
      subclase = as.character(subclase),
      ano  = as.numeric(ano),
      mes  = as.character(mes),
      numero_indice = as.numeric(numero_indice)
    ) %>%
    mutate(
      mes_num = dplyr::case_when(
        suppressWarnings(!any(is.na(as.integer(mes)))) ~ as.integer(mes),
        TRUE ~ match(mes, meses_esp)
      )
    )
}

## === 1) Apilar 99–05, 06–14 y 15-18 en un solo data frame ===
ipc_raw <- dplyr::bind_rows(
  read_ipc_file("var-ipc/IPC_99_05.xls"),
  read_ipc_file("var-ipc/IPC_06_14.xls"),
  read_ipc_file("var-ipc/IPC.xls")
)

## (opcional) chequeo rápido
cat("ipc_raw filas totales:", nrow(ipc_raw), "\n")

## === 2) Estandarizar ciudad y generar cod_subclase (8 dígitos) ===
ipc_data <- ipc_raw %>%
  mutate(
    ciudad = dplyr::recode(ciudad,
                           "CARTAGENA DE INDIAS" = "CARTAGENA",
                           "BOGOTÁ, D.C."        = "BOGOTÁ D.C.",
                           "BOGOTC, D.C."        = "BOGOTÁ D.C.",
                           "BOGOTC, D.C."       = "BOGOTÁ D.C."
    ),
    cod_subclase = substr(gsub("[^0-9]", "", subclase), 1, 8)
  ) %>%
  filter(ciudad == "CALI") %>%
  select(ciudad, cod_subclase, ano, mes_num, ipc = numero_indice) %>%
  arrange(ano, mes_num)

## (opcional) chequeos
cat("ipc_data (CALI) filas:", nrow(ipc_data), "\n")

# ---------------------------------------------------------------------------

# Función para procesar cada alimento
procesar_alimento <- function(articulo, datos) {
  
  df_aux <- filter(datos, articulo == !!articulo)
  if (nrow(df_aux) == 0) return(NULL)
  
  # Encontrar subclase IPC
  subclase_info <- df_aux %>%
    left_join(select(correlativa_ipc, cod_subclase, subclase), by = "cod_subclase") %>%
    distinct(subclase)
  
  if (nrow(subclase_info) > 1 || any(is.na(subclase_info$subclase))) {
    subclase_info <- df_aux %>%
      left_join(select(correlativa_articulos, codigo_articulo, subclase),
                by = "codigo_articulo") %>%
      distinct(subclase)
  }
  
  if (nrow(subclase_info) == 0 || any(is.na(subclase_info$subclase))) return(NULL)
  
  # Filtrar IPC para la subclase
  ipc_filtrado <- ipc_data %>%
    filter(cod_subclase == paste0(unique(subclase_info$subclase), "00"))
  
  if (nrow(ipc_filtrado) == 0) return(NULL)
  
  # Unir con datos de precios
  test <- df_aux %>%
    left_join(ipc_filtrado, by = c("ano", "mes_num")) %>%
    select(
      fecha, ano, mes_num, nombre_ciudad,
      cod_subclase = cod_subclase.x,
      codigo_articulo, articulo, precio_500g, ipc
    ) %>%
    arrange(fecha)
  
  test
}

# Procesar todos los alimentos
output_final <- purrr::map_dfr(unique(dane_data$articulo),
                               ~procesar_alimento(.x, dane_data))

# Guardar
write_xlsx(output_final, "var-ipc\\output_final_union_ipc.xlsx")
