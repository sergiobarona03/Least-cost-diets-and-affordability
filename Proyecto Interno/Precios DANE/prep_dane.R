##-----------------------------------##
## Carga y procesamiento: Datos DANE ##
## Fecha: 14 de junio de 2025        ##
##-----------------------------------##

# Cargar bibliotecas
library(readxl)
library(dplyr)
library(tidyr)
library(janitor)

# Definir directorio de trabajo
setwd("C:/Users/Portatil/Desktop/Least-cost-diets-and-affordability/Proyecto Interno/Precios DANE")

# Leer funciones para la limpieza de datos
source("f1_prep.R")

##----------------------------##
## Carga y limpieza de datos  ##
##----------------------------##

# Definir años
years = 1999:2018

# Iniciar bucle
year_output = vector(mode = "list", length = length(years))

for (k in 1:length(years)) {
  cat(strrep("-", 50), "\n")
  cat("DONE:", years[k], "\n")
  cat(strrep("-", 50), "\n\n")
  
  year_files = list.files(as.character(years[k]))
  
  if (years[k] != 2018) {
    p_aux <- suppressMessages(
      suppressWarnings(
        readxl::read_excel(paste0(years[k],"/",year_files))
      )
    )
  } else {
    p_aux <- suppressMessages(
      suppressWarnings(
        readxl::read_excel(paste0(years[k],"/",year_files), sheet = 3)
      )
    )
  }
  

  year_output[[k]] = f1(p_aux, years[k])
  cat("Vista previa:", "\n")
  print(head(year_output[[k]]))
}

# Unir las listas a través de un rbind
df_output <- do.call(rbind, year_output) 


# Guardar los datos como dataframes:
writexl::write_xlsx(df_output, paste0("OUTPUT_DANE/precios_IPC_",years[1],"_",
                                      years[length(years)],".xlsx"))

