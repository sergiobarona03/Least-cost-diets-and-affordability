library(tidyverse)
library(readr)

ruta <- "C:/Users/danie/OneDrive/Documentos/Pobreza/Personas"

# 1) Listar archivos y extraer el año 
archivos <- list.files(ruta, pattern = "\\.csv$", full.names = TRUE)

info <- tibble(
  file = archivos,
  name = basename(archivos),
  year = readr::parse_number(name) 
) %>%
  filter(!is.na(year)) %>%
  arrange(year)

years <- unique(info$year)

# 2) Loop por año
for (y in years) {
  message(paste0("==> Procesando año: ", y))
  
  file_y <- info %>% filter(year == y) %>% pull(file)
  
  if (length(file_y) != 1) {
    warning(paste0("Para el año ", y, " encontré ", length(file_y), " archivos. Reviso nombres: ",
                   paste(basename(file_y), collapse = " | ")))
    next
  }
  
  # Leer el CSV del año (separador ;)
  df_year <- read_delim(
    file = file_y,
    delim = ";",
    col_types = cols(.default = col_character()),
    na = c("", "NA", "N/A", ".", " ", "NULL"),
    show_col_types = FALSE
  ) %>%
    mutate(Year = y)
  
  # 3) Normalizar variables
 df_year <- df_year %>%
  mutate(across(matches("^(P|p)\\d{4,}$"),
                ~ parse_number(.x)))
  
  # 4) Guardar RDS por año
  saveRDS(df_year, file = file.path(ruta, paste0("personas_pobreza_", y, ".rds")))
  message(paste0("Guardado: personas_pobreza_", y, ".rds"))
}

# OPCIONAL: Unir todo lo anual en una sola base y guardar
archivos_rds <- list.files(ruta, pattern = "^personas_pobreza_\\d{4}\\.rds$", full.names = TRUE)

personas_pobreza_2018_2024 <- archivos_rds %>%
  map_dfr(readRDS)

saveRDS(personas_pobreza_2018_2024, file = file.path(ruta, "personas_pobreza_2018_2024.rds"))
