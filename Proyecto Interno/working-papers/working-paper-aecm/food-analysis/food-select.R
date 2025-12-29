###################################################
## Prueba: Definir alimentos para el análisis    ##
###################################################

# Cargar librerías
library(lubridate)
library(tidyverse)
library(readxl)
library(writexl)

# Definir directorio de trabajo
setwd("C:\\Users\\sergio.barona\\Desktop\\Least-cost-diets-and-affordability\\Proyecto Interno\\")

date_tag <- "261225"

# Cargar datos
path.cover <- paste0("working-papers\\working-paper-aecm\\input\\",
                     date_tag, "_coverage_ipc_sipsa.xlsx")
path.input <- paste0("working-papers\\working-paper-aecm\\input\\",
                     date_tag, "_dataset_ipc_sipsa.xlsx")

coverage_city <- read_excel(path.cover)
data_merged   <- read_excel(path.input)

# Estandarizar cod_mun (por si viene numérico)
coverage_city <- coverage_city %>%
  mutate(cod_mun = sprintf("%05d", as.integer(cod_mun)))

data_merged <- data_merged %>%
  mutate(cod_mun = sprintf("%05d", as.integer(cod_mun)))

# Eliminar alimentos ad hoc
coverage_city <- coverage_city %>%
  filter(!alimento_sipsa %in% c("Cebolla cabezona roja importada",
                                "Tomate riñón",
                                "Arroz de segunda"))

# Dataset para cada ciudad
cali     <- data_merged %>% filter(cod_mun == "76001")
bogota   <- data_merged %>% filter(cod_mun == "11001")
medellin <- data_merged %>% filter(cod_mun == "05001")

# Para cada dataset: quedarte solo con alimentos presentes en coverage_city de esa ciudad
cali.foods <- coverage_city %>% filter(cod_mun == "76001")
cali <- cali %>% filter(alimento_sipsa %in% unique(cali.foods$alimento_sipsa))

bogota.foods <- coverage_city %>% filter(cod_mun == "11001")
bogota <- bogota %>% filter(alimento_sipsa %in% unique(bogota.foods$alimento_sipsa))

medellin.foods <- coverage_city %>% filter(cod_mun == "05001")
medellin <- medellin %>% filter(alimento_sipsa %in% unique(medellin.foods$alimento_sipsa))

# Dataset final (apilado)
final.dataset <- bind_rows(cali, bogota, medellin)

# Guardar
path.out <- paste0("working-papers\\working-paper-aecm\\input\\",
                   date_tag, "_selected_foods_dataset.xlsx")

write_xlsx(final.dataset, path.out)
