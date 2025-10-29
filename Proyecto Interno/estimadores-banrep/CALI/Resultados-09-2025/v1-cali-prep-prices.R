
#####################################################
#####################################################
## Prueba: preparación de base de datos de precios ##
#####################################################
#####################################################

# Cargar librerías
library(lubridate)
library(tidyverse)

# Definir directorio de trabajo
setwd("C:/Users/danie/OneDrive/Escritorio/Least-cost-diets-and-affordability/Proyecto Interno")

##------------------------------------##
## Cargar datos de precios mayoristas ##
##------------------------------------##

# Lista output
whole_list = vector(mode = "list", length = length(2013:2024))

# Cargar series de sipsa
for (k in 2013:2024) {
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

# Filtrar para las 13 ciudades principales
whole_18 <- whole_18 %>% filter(!is.na(nombre_ciudad))

# Armonizar los nombres de las ciudades (código DIVIPOLA)
whole_18 <- whole_18 %>%
  mutate(cod_mun = case_when(
    nombre_ciudad == "BARRANQUILLA"   ~ "08001",
    nombre_ciudad == "BOGOTÁ D.C."    ~ "11001",
    nombre_ciudad == "BUCARAMANGA"    ~ "68001",
    nombre_ciudad == "CALI"           ~ "76001",
    nombre_ciudad == "CARTAGENA"      ~ "13001",
    nombre_ciudad == "CÚCUTA"         ~ "54001",
    nombre_ciudad == "MANIZALES"      ~ "17001",
    nombre_ciudad == "MEDELLÍN"       ~ "05001",
    nombre_ciudad == "MONTERÍA"       ~ "23001",
    nombre_ciudad == "NEIVA"          ~ "41001",
    nombre_ciudad == "PASTO"          ~ "52001",
    nombre_ciudad == "PEREIRA"        ~ "66001",
    nombre_ciudad == "VILLAVICENCIO"  ~ "50001",
    TRUE ~ NA_character_
  ))

# Antes de calcular el precio medio, se armonizan las unidad (P500g)
# El precio 
whole_18 <- whole_18 %>%
  mutate(
    precio_500g = case_when(
      str_detect(Alimento, regex("aceite", ignore_case = TRUE)) ~ Precio_kg * (500 / 920),
      
      Alimento %in% c("Huevo blanco A", "Huevo rojo A") ~ Precio_kg * (500 / 50),
      Alimento %in% c("Huevo blanco AA", "Huevo rojo AA") ~ Precio_kg * (500 / 60),
      Alimento %in% c("Huevo blanco extra", "Huevo rojo extra") ~ Precio_kg * (500 / 67),
      
      TRUE ~ Precio_kg / 2  # Por defecto, mitad del precio por kg
    )
  ) %>%
  filter(!Alimento %in% c("Jugo de frutas", "Bocadillo veleño", "Vinagre",
                          "Huevo blanco B", "Huevo rojo B"))


# Crear el precio promedio para cada alimento según: año, mes, ciudad, alimento
whole_18_mean <- whole_18 %>% group_by(Year, Month, cod_mun, Alimento) %>%
  summarise(precio_medio = mean(precio_500g, na.rm = TRUE))

##------------------------------------##
## Cargar mapeo: DANE (IPC) - SIPSA   ##
##------------------------------------##

# Cargar el mapeo de ambas bases:
ipc_sipsa = readxl::read_excel("Time-series\\mapeo_retail_sipsa.xlsx")

# Añadir al mayorista las denominaciones de sipsa
whole_18_mean = whole_18_mean %>% left_join(ipc_sipsa, by = c("Alimento" = "sipsa"))


##----------------------------------------------------##
## Cargar margen de comercialización por artículo     ##
##----------------------------------------------------##

# Cargar la distribución del margen
q1_q3_productos = read.csv("margen-dist/output-ciudades/CALI/300725_q1_q3_margen_producto.csv")

# Añadir el margen (q1-q3) a la base de datos
whole_18_mean = merge(whole_18_mean,q1_q3_productos, by.x = "retail",
                      by.y = "articulo")

# Estimación de precios minoristas
whole_18_mean$precio_hat_q1 = whole_18_mean$precio_medio*(1+(whole_18_mean$m_q1/100))
whole_18_mean$precio_hat_q2 = whole_18_mean$precio_medio*(1+(whole_18_mean$m_q2/100))
whole_18_mean$precio_hat_q3 = whole_18_mean$precio_medio*(1+(whole_18_mean$m_q3/100))

# Guardar input para cali
input_cali = whole_18_mean %>% filter(cod_mun == "76001") %>%
  rename(food_sipsa = Alimento, year = Year, month = Month) %>%
  select(cod_mun, year, month, food_sipsa, m_q1,
         m_q2, m_q3, precio_hat_q1,
         precio_hat_q2, precio_hat_q3)

readr::write_csv(input_cali,
                 "estimadores-banrep/CALI/input/010825_q1_q3_price_data_cali.csv")


