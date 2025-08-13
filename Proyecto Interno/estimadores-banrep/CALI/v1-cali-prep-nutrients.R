
#####################################################################
#####################################################################
## Prueba: preparación de base de datos de composición nutricional ##
#####################################################################
#####################################################################

# Cargar librerías
library(lubridate)
library(tidyverse)

# Definir directorio de trabajo
setwd("C:/Users/danie/OneDrive/Escritorio/Least-cost-diets-and-affordability/Proyecto Interno")

##-------------------------------------------##
## Cargar base de datos de precios estimados ##
##-------------------------------------------##

# Cargar datos de precios estimados
input_cali_hat = read.csv("estimadores-banrep/CALI/input/010825_q1_q3_price_data_cali.csv")

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
input_cali_hat = input_cali_hat %>%
  mutate(
    pc = parte_comestible_percent,
    precio_q1_100g = precio_hat_q1*(100/(5*pc)),
    precio_q2_100g = precio_hat_q2*(100/(5*pc)),
    precio_q3_100g = precio_hat_q3*(100/(5*pc)))

readr::write_csv(input_cali_hat,
                 "estimadores-banrep/CALI/input/010825_q1_q3_comp_price_data_cali.csv")

