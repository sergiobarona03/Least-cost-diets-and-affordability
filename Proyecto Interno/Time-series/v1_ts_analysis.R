
##-----------------------------------------##
## Análisis preliminar de series de tiempo ##
## Fecha: 15 de junio de 2025              ##
##-----------------------------------------##

# Cargar librerías
library(tidyverse)

# Definir directorio de trabajo
setwd("C:\\Users\\Portatil\\Desktop\\Least-cost-diets-and-affordability\\Proyecto Interno\\")

# Cargar función:
source("Time-series\\f1_plot_retail.R")

##------------------------------------------------##
## Cargar datos: precios minoristas (1999 - 2018) ##
##------------------------------------------------##

# Cargar datos (13 ciudades principales)
retail_99_18 = readxl::read_excel("Precios DANE\\OUTPUT_DANE\\precios_IPC_1999_2018.xlsx")

# Eliminar alimentos que son ultraprocesados o preparaciones
alimentos_excluir <- c(
  # Ultraprocesados
  "AREPAS  PRECOCIDAS", "AREPAS RELLENAS CON ALGO", "BOCADILLOS",
  "CEREAL ALIMENTO PARA BEBÉ", "CEREAL PARA DESAYUNO", "CHOCOLATE INSTANTANEO",
  "CHORIZO", "GALLETAS DE SAL", "GALLETAS DULCES", "GALLETAS INTEGRALES",
  "GASEOSAS", "GELATINA O FLAN", "HARINA PARA TORTAS", "HELADOS DE CREMA",
  "JAMÓN", "JUGOS INSTANTANEOS O EN POLVO", "JUGOS PROCESADOS",
  "MALTAS", "MARGARINA", "MAYONESA", "MERMELADA", "MORTADELA",
   "PIZZA", "SALCHICHAS","SALCHICHÓN", "SALSA DE TOMATE", 
  "SOPAS", "YOGOURT", "CREMA DE LECHE", "PAPAS FRITAS",
  
  # Condimentos y hierbas
  "CILANTRO", "COLOR", "COMINOS", "LAUREL", "MOSTAZA",
  "PIMIENTA", "TOMILLO", "REVUELTO VERDE",
  
  # Preparaciones y alimentos compuestos
  "ALMUERZO CORRIENTE O EJECUTIVO", "ALMUERZO ESPECIAL O A LA CARTA",
  "CHOCOLATE EN PASTA", "CAFÉ INSTANTANEO", "COMBOS", 
  "CREMAS", "ENSALADA  DE FRUTAS", "QUESO CREMA", "TINTO", 
  "HAMBURGUESA", "KUMIS", "JUGOS NATURALES", "SUERO",
  
  # Otros
  "AGUA  MINERAL"
)

# Excluir alimentos
retail_99_18 <- retail_99_18 %>% filter(!articulo %in% alimentos_excluir)

# Examinar NAs
na_producto_ciudad <- retail_99_18 %>%
  group_by(ano, articulo, nombre_ciudad ) %>%
  summarise(n_NA_precio = sum(is.na(as.numeric(precio))), .groups = "drop") %>%
  arrange(desc(n_NA_precio))

# Ver los primeros resultados
View(na_producto_ciudad[na_producto_ciudad$n_NA_precio > 0, ])

# Guardar la lista de alimentos (para el mapeo DANE-SIPSA)
writexl::write_xlsx(data.frame(retail = levels(as.factor(retail_99_18$articulo))),
                    "Time-series\\lista_retail_99_18.xlsx")

# Analizar las series por ciudad y artículo
graficar_precio(retail_99_18, "ARROZ PARA SECO",
                c("MEDELLÍN", "CALI", "BOGOTÁ D.C."))

##------------------------------------------------##
## Cargar datos: precios mayoristas (1997 - 2018) ##
##------------------------------------------------##

# Cargar función:
source("Time-series\\f2_plot_wholesale.R")

whole_list = vector(mode = "list", length = length(2013:2018))

# Cargar series de sipsa
for (k in 2013:2018) {
  whole_list[[k]] = readRDS(paste0("Precios al por mayor\\Bases historicas\\", k,".rds"))
}

# whole_18 significa whole hasta 2018
whole_18 <- do.call(rbind, whole_list)

# Identificar los mercados de las principales ciudades

whole_18 <- whole_18 %>%
  mutate(nombre_ciudad = case_when(
    str_detect(Mercado, regex("Barranquilla", ignore_case = TRUE)) ~ "BARRANQUILLA",
    str_detect(Mercado, regex("Bogotá", ignore_case = TRUE)) ~ "BOGOTÁ D.C.",
    str_detect(Mercado, regex("Bucaramanga", ignore_case = TRUE)) ~ "BUCARAMANGA",
    str_detect(Mercado, regex("Cali", ignore_case = TRUE)) ~ "CALI",
    str_detect(Mercado, regex("Cartagena", ignore_case = TRUE)) ~ "CARTAGENA",
    str_detect(Mercado, regex("Cúcuta", ignore_case = TRUE)) ~ "CÚCUTA",
    str_detect(Mercado, regex("Manizales", ignore_case = TRUE)) ~ "MANIZALES",
    str_detect(Mercado, regex("Medellín", ignore_case = TRUE)) ~ "MEDELLÍN",
    str_detect(Mercado, regex("Montería", ignore_case = TRUE)) ~ "MONTERÍA",
    str_detect(Mercado, regex("Neiva", ignore_case = TRUE)) ~ "NEIVA",
    str_detect(Mercado, regex("Pasto", ignore_case = TRUE)) ~ "PASTO",
    str_detect(Mercado, regex("Pereira", ignore_case = TRUE)) ~ "PEREIRA",
    str_detect(Mercado, regex("Villavicencio", ignore_case = TRUE)) ~ "VILLAVICENCIO",
    TRUE ~ NA_character_
  ))

# Los mercados identificados para cada ciudad principal son los siguientes
writexl::write_xlsx(dplyr::count(whole_18, nombre_ciudad, Mercado),
                    "Time-series\\2013_2018_mercados_sipsa.xlsx")

# Filtrar para las 13 ciudades principales
whole_18 <- whole_18 %>% filter(!is.na(nombre_ciudad))

# Crear el precio promedio para cada alimento según: año, mes, ciudad, alimento
whole_18_mean <- whole_18 %>% group_by(Year, Month, nombre_ciudad, Alimento) %>%
  summarise(precio_medio = mean(Precio_kg, na.rm = TRUE))

# Analizar el comportamiento de las series:
plot_wholesale(whole_18_mean,"Arroz de primera",
               c("Bogotá D.C.", "Barranquilla", "Cali"))




