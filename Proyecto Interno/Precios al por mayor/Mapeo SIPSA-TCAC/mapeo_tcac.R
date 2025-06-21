
##------------------------------------------##
## Mapeo: SIPSA (precios mayoristas) - TCAC ##
##------------------------------------------##

# Cargar paquetes
library(tidyverse)

# Directorio
setwd("C:\\Users\\Portatil\\Desktop\\Least-cost-diets-and-affordability\\Proyecto Interno\\Precios al por mayor\\")

##---------------------------------##
## 1. Mapeo SIPSA-TCAC 2018 - 2023 ##
##---------------------------------##

# Cargar datos sobre precios mayoristas (SIPSA)
input <- vector(mode = "list", length = 6)
year <- 2013:2023
  
for (k in 1:length(year)) {
  print(paste0("Done: ", year[k]))
  input[[k]] <- readRDS(paste0("Bases historicas\\",year[k],".rds"))
}

overall_input <- do.call(rbind, input) %>% select(Fecha, Year, Month,
                                                  Grupo, Alimento, Mercado, Precio_kg)

# Eliminar duplicados
list_input <- data.frame(Alimento = levels(as.factor(overall_input$Alimento)))

# Extrar la base de datos TCAC desde el paqueute FoodpriceR
library(FoodpriceR)
tcac <- FoodpriceR::Mapeo_Sipsa_TCAC

# Mapeo simple
overall_tcac_sipsa <- merge(list_input, tcac, by = "Alimento", all.x = TRUE)

# Guardar 
writexl::write_xlsx(overall_tcac_sipsa, "Mapeo SIPSA-TCAC\\1823_mapeo_sipsa_tcac.xlsx")


##---------------------------------##
## 1. Mapeo SIPSA-TCAC 2018 - 2024 ##
##---------------------------------##

rm(list = ls())

# Cargar datos sobre precios mayoristas (SIPSA)
input <- vector(mode = "list", length = 7)
year <- 2018:2024

for (k in 1:length(year)) {
  print(paste0("Done: ", year[k]))
  input[[k]] <- readRDS(paste0("Bases historicas\\",year[k],".rds"))
}

overall_input <- do.call(rbind, input) %>% select(Fecha, Year, Month,
                                                  Grupo, Alimento, Mercado, Precio_kg)

# Eliminar duplicados
list_input <- data.frame(Alimento = levels(as.factor(overall_input$Alimento)))

# Extrar la base de datos TCAC desde el paqueute FoodpriceR
library(FoodpriceR)
tcac <- FoodpriceR::Mapeo_Sipsa_TCAC

# Mapeo simple
overall_tcac_sipsa <- merge(list_input, tcac, by = "Alimento", all.x = TRUE)

# Guardar 
writexl::write_xlsx(overall_tcac_sipsa, "Mapeo SIPSA-TCAC\\1824_mapeo_sipsa_tcac.xlsx")


##-----------------------------------##
## 2. Mapeo SIPSA-TCAC 2018 - 2024:  ##
##-----------------------------------##

rm(list = ls())

# Cargar datos sobre precios mayoristas (SIPSA)
input <- vector(mode = "list", length = 7)
year <- 2018:2024

for (k in 1:length(year)) {
  print(paste0("Done: ", year[k]))
  input[[k]] <- readRDS(paste0("Bases historicas\\",year[k],".rds"))
}

overall_input <- do.call(rbind, input) %>% select(Fecha, Year, Month,
                                                  Grupo, Alimento, Mercado)

# Crear la variable municipio
overall_input <- overall_input %>%
  mutate(
    Municipio = str_trim(str_extract(Mercado, "^[^(,]+"))
  ) %>% distinct()

# Extrar la base de datos TCAC desde el paqueute FoodpriceR
library(FoodpriceR)
tcac <- FoodpriceR::Mapeo_Sipsa_TCAC

# Mapeo simple
overall_tcac_sipsa <- merge(overall_input, tcac, by = "Alimento", all.x = TRUE)

overall_tcac_sipsa <- overall_tcac_sipsa %>% janitor::clean_names()


# Guardar 
writexl::write_xlsx(overall_tcac_sipsa, "Mapeo SIPSA-TCAC\\1324_mun_mapeo_sipsa_tcac.xlsx")






