# Librerías 

library(dplyr)
library(tidyverse)


# Cargar bases
year_2018 <- readRDS("C:\\Users\\danie\\OneDrive\\Escritorio\\Least-cost-diets-and-affordability\\Proyecto Interno\\Flujos de carga (Abastecimiento)\\Bases historicas\\2018.rds")

year_2019 <- readRDS("C:\\Users\\danie\\OneDrive\\Escritorio\\Least-cost-diets-and-affordability\\Proyecto Interno\\Flujos de carga (Abastecimiento)\\Bases historicas\\2019.rds")

year_2020 <- readRDS("C:\\Users\\danie\\OneDrive\\Escritorio\\Least-cost-diets-and-affordability\\Proyecto Interno\\Flujos de carga (Abastecimiento)\\Bases historicas\\2020.rds")

year_2021 <- readRDS("C:\\Users\\danie\\OneDrive\\Escritorio\\Least-cost-diets-and-affordability\\Proyecto Interno\\Flujos de carga (Abastecimiento)\\Bases historicas\\2021.rds")

year_2022 <- readRDS("C:\\Users\\danie\\OneDrive\\Escritorio\\Least-cost-diets-and-affordability\\Proyecto Interno\\Flujos de carga (Abastecimiento)\\Bases historicas\\2022.rds")

year_2023 <- readRDS("C:\\Users\\danie\\OneDrive\\Escritorio\\Least-cost-diets-and-affordability\\Proyecto Interno\\Flujos de carga (Abastecimiento)\\Bases historicas\\2023.rds")

year_2024 <- readRDS("C:\\Users\\danie\\OneDrive\\Escritorio\\Least-cost-diets-and-affordability\\Proyecto Interno\\Flujos de carga (Abastecimiento)\\Bases historicas\\2024.rds")


# Crear una lista con todas las bases
lista_bases <- list(year_2018, year_2019, year_2020, year_2021, year_2022, year_2023, year_2024)

# Normalizar columnas
lista_bases <- lapply(lista_bases, function(df) {
  if (!"Cod_CPC" %in% colnames(df)) {
    df$Cod_CPC <- NA  
  }
  return(df)
})

# Unir todas las bases
datos_consolidados <- bind_rows(lista_bases)

# Convertir columnas a formatos correctos
datos_consolidados <- datos_consolidados %>%
  mutate(
    Fecha = as.Date(Fecha),  
    Cantidad_KG = as.numeric(Cantidad_KG) 
  )

# Verificar que se unieron correctamente
glimpse(datos_consolidados)

# Guardar la base consolidada
saveRDS(datos_consolidados, "C:\\Users\\danie\\OneDrive\\Escritorio\\Least-cost-diets-and-affordability\\Proyecto Interno\\Flujos de carga (Abastecimiento)\\Bases historicas\\datos_consolidados_2018_2024.rds")



