# Librerías
library(tidyverse)

# Ruta de los archivos
ruta <- "C:/Users/danie/OneDrive/Documentos/Pobreza/Personas"

# Listar todos los CSV de personas
archivos <- list.files(
  path = ruta,
  pattern = "\\.csv$",
  full.names = TRUE
)

# Leer y unir (rbind)
personas_pobreza <- archivos %>%
  map_dfr(read_csv, show_col_types = FALSE)

# Guardar la base final en .rds
saveRDS(
  personas_pobreza,
  file = file.path(ruta, "personas_pobreza_2018_2024.rds")
)
