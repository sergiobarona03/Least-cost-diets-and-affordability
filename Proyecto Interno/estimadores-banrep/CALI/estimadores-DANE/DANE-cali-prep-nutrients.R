
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
input_cali_hat = read.csv("estimadores-banrep/CALI/estimadores-DANE/input/010925_retail_price_data.csv")
input_cali_hat$food_sipsa = input_cali_hat$sipsa

##-------------------------------------------------##
## Cargar base de datos de composición nutricional ##
##-------------------------------------------------##

# Cargar datos de composición nutricional
sipsa_tcac = readxl::read_excel("composicion-nut/1823_mapeo_sipsa_tcac v1.0_2025.xlsx") %>%
  janitor::clean_names() %>% rename(food_sipsa = alimento_nombre_sipsa)

# Seleccionar variables y eliminar duplicados
sipsa_tcac = sipsa_tcac[,-(35:39)] %>% select(-c("nombre_original",
                                                 "nota_de_codificacion",
                                                 "factor_de_conversion")) %>%
  distinct()

# Unir ambas bases de datos
input_cali_hat = input_cali_hat %>% left_join(sipsa_tcac,
                                              by = "food_sipsa")
  
# Conversión a 100 gramos en parte comestible
input_cali_hat2 = input_cali_hat %>%
  mutate(
    pc = parte_comestible_percent,
    precio_100g = precio_500g*(100/(5*pc))) %>% select(-c("sipsa", "food_sipsa")) %>%
  distinct()

readr::write_csv(input_cali_hat2,
                 "estimadores-banrep/CALI/estimadores-DANE/input/010925_comp_price_data_cali.csv")

