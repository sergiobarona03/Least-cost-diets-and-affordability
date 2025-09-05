########################################################
## Prueba: estimación según variación mensual del IPC ##
## Fecha: 22 de junio de 2025                         ##
########################################################

# Cargar librerías
library(tidyverse)
library(lubridate)
library(readxl)
library(janitor)

# Definir directorio de trabajo
setwd("C:\\Users\\Portatil\\Desktop\\Least-cost-diets-and-affordability\\Proyecto Interno\\")

# Cargar y procesar datos de precios
dane_data <- read_excel("Precios DANE\\OUTPUT_DANE\\precios_unadj_DANE_1999_2018.xlsx") %>%
  filter(nombre_ciudad == "CALI", 
         as.Date(paste(ano, mes_num, "01", sep = "-")) < "2018-04-01") %>%
  mutate(fecha = as.Date(paste(ano, mes_num, "01", sep = "-")),
         cod_subclase = paste0("0", substr(codigo_articulo, 1, 6), "0"))

# Cargar y procesar tablas correlativas
correlativa_ipc <- read_excel("var-ipc\\correlativa_ipc.xlsx") %>%
  fill(subclase, ipc, .direction = "down") %>%
  mutate(cod_subclase = paste0("0", gasto_basico, "00"))

correlativa_articulos <- read_excel("var-ipc\\correlativa_ipc_articulos.xlsx") %>%
  mutate(codigo_articulo = cod_dane, subclase = cod_ipc)

# Cargar y procesar datos IPC
meses_esp <- c("Ene", "Feb", "Mar", "Abr", "May", "Jun", 
               "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")

ipc_data <- read_excel("var-ipc\\IPC.xls") %>%
  clean_names() %>%
  mutate(ciudad = recode(ciudad, 
                         "CARTAGENA DE INDIAS" = "CARTAGENA",
                         "BOGOTÁ, D.C." = "BOGOTÁ D.C."),
         cod_subclase = substr(subclase, 1, 8),
         mes_num = match(mes, meses_esp),
         ano = as.numeric(ano)) %>%
  filter(ciudad == "CALI") %>%
  select(ciudad, cod_subclase, ano, mes_num, ipc = numero_indice)

# Función para procesar cada alimento
procesar_alimento <- function(articulo, datos) {
  articulo = unique(dane_data$articulo)[1]
  datos = dane_data
  df_aux <- filter(datos, articulo == !!articulo)
  
  if (nrow(df_aux) == 0) return(NULL)
  
  # Encontrar subclase IPC
  subclase_info <- df_aux %>%
    left_join(select(correlativa_ipc, cod_subclase, subclase), by = "cod_subclase") %>%
    distinct(subclase)
  
  if (nrow(subclase_info) > 1 || any(is.na(subclase_info$subclase))) {
    subclase_info <- df_aux %>%
      left_join(select(correlativa_articulos, codigo_articulo, subclase), by = "codigo_articulo") %>%
      distinct(subclase)
  }
  
  if (nrow(subclase_info) == 0 || any(is.na(subclase_info$subclase))) return(NULL)
  
  # Filtrar IPC para la subclase
  ipc_filtrado <- ipc_data %>%
    filter(cod_subclase == paste0(unique(subclase_info$subclase), "00"))
  
  if (nrow(ipc_filtrado) == 0) return(NULL)
  
  # Unir con datos de precios
  test = df_aux %>%
    left_join(ipc_filtrado, by = c("ano", "mes_num")) %>%
    select(fecha, ano, mes_num, nombre_ciudad, cod_subclase = cod_subclase.x,
           codigo_articulo, articulo, precio_500g, ipc) %>%
    arrange(fecha)
}

# Procesar todos los alimentos
output_final <- map_dfr(unique(dane_data$articulo), 
                        ~procesar_alimento(.x, dane_data))

# Guardar resultados
write_xlsx(output_final, "margen-dist\\pca\\v3_input_pca_0925.xlsx")
