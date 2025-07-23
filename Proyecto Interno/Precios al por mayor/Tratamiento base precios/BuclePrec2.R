#--------------------------------------#
# 1. Cargar librerías necesarias       #
#--------------------------------------#
library(readxl)
library(dplyr)
library(lubridate)
library(stringi)
library(stringr)

limon <- read_excel("C:\\Users\\danie\\Downloads\\limon_tahiti.xlsx")

# Separar en 2: día de la semana y el resto de la fecha
limon <- limon %>%
  separate(Fecha, into = c("Dia_Semana", "Fecha_Resto"), sep = ",\\s*", remove = FALSE)

# Separar en partes: Mes, Día, "de", Año
limon <- limon %>%
  separate(Fecha_Resto, into = c("Month", "Dia", "DE", "Year"), sep = "\\s+", remove = FALSE)

# Convertir columnas numéricas
limon <- limon %>%
  mutate(Year = as.integer(Year),
         Month = str_to_title(Month)) 

# Mapear nombre de mes a número
meses <- c("Enero"=1, "Febrero"=2, "Marzo"=3, "Abril"=4,
           "Mayo"=5, "Junio"=6, "Julio"=7, "Agosto"=8,
           "Septiembre"=9, "Octubre"=10, "Noviembre"=11, "Diciembre"=12)

# Pasar los meses a número
limon <- limon %>%
  mutate(
    Month = meses[Month])

# Quedarse con las columnas necesarias
limon_final <- limon %>%
  select(Mercado, Producto, `Precio $/KG`, Month, Year)

# Base final
limon2 <- limon %>%
  mutate(
    Grupo = "FRUTAS",
    Alimento = Producto,
    Precio_kg = `Precio $/KG`) %>%
  select(Grupo, Alimento, Mercado, Precio_kg, Month, Year)

# Guardar la base
saveRDS(limon2, "C:\\Users\\danie\\Downloads\\limon_convertido.rds")



