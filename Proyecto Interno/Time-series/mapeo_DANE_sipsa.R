# Paquetes 
library(readxl)   
library(dplyr)   
library(writexl)  

# Carpetas 

setwd("C:\\Users\\danie\\OneDrive\\Escritorio\\Least-cost-diets-and-affordability\\Proyecto Interno\\")

dane  <- read_excel("composicion-nut\\Copia_DANE_4_DIC_2025act.xlsx")
sipsa <- read_excel("composicion-nut\\1823_mapeo_sipsa_tcac v1.0_2025.xlsx")

# Llaves para anclar entre bases 

dane  <- dane  %>% mutate(Mapeo_SIPSA = as.character(Mapeo_SIPSA))
sipsa <- sipsa %>% mutate(Codigo_TCAC = as.character(Codigo_TCAC))

# Mapeo DANE–SIPSA 

mapeo_retail_sipsa <-
  dane %>%
  left_join(
    sipsa %>%
      select(
        Codigo_TCAC,
        nombre_sipsa = `Alimento (Nombre sipsa)`
      ),
    by = c("Mapeo_SIPSA" = "Codigo_TCAC")
  ) %>%
  filter(!is.na(nombre_sipsa)) %>%
  transmute(
    retail = articuloDANE,
    sipsa  = nombre_sipsa
  ) %>%
  distinct(retail, sipsa) %>%
  arrange(retail, sipsa)

head(mapeo_retail_sipsa, 30)


write_xlsx(mapeo_retail_sipsa, "Time-series\\mapeo_retail_sipsa_v2.xlsx")
