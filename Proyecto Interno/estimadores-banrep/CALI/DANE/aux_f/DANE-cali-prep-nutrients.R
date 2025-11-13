
#####################################################################
#####################################################################
## Prueba: preparación de base de datos de composición nutricional ##
#####################################################################
#####################################################################

# Cargar librerías
library(lubridate)
library(tidyverse)

# Definir directorio de trabajo
setwd("C:/Users/Portatil/Desktop/Least-cost-diets-and-affordability/Proyecto Interno")

##-------------------------------------------##
## Cargar base de datos de precios estimados ##
##-------------------------------------------##

# Cargar datos de precios estimados
input_cali_hat = read.csv("estimadores-banrep/CALI/Resultados-10-2025/estimadores-DANE/input/291025_retail_price_data.csv")

##-------------------------------------------------##
## Cargar base de datos de composición nutricional ##
##-------------------------------------------------##

# Cargar datos de composición nutricional
dane_tcac = readxl::read_excel("composicion-nut/TCAC_DANE 020925_rev.xlsx") %>%
  janitor::clean_names() %>% rename(articulo = articulo_dane)

# Seleccionar variables y eliminar duplicados
dane_tcac = dane_tcac[,-c(34,36)] %>% select(-c("nombre_del_alimento",
                                                 "factores_de_conversion")) %>%
  distinct()

# Unir ambas bases de datos
input_cali_hat = input_cali_hat %>% left_join(dane_tcac %>% select(-c("nombre_ciudad")),
                                              by = c("codigo_articulo", "articulo"))
  
# Conversión a 100 gramos en parte comestible
input_cali_hat2 = input_cali_hat %>%
  mutate(
    pc = parte_comestible_percent,
    precio_100g = precio_500g*(100/(5*pc))) %>%
  distinct()

readr::write_csv(input_cali_hat2,
                 "estimadores-banrep/CALI/Resultados-10-2025/estimadores-DANE/input/291025_comp_price_data_cali.csv")

