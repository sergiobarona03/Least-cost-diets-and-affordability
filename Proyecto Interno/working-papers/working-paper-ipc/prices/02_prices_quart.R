#######################################################################
## Figura 1 (QUART): Precios reales minoristas TRIMESTRALES (promedio)
#######################################################################

library(tidyverse)
library(lubridate)
library(janitor)
library(readxl)

#----------------------------------------------------------------------
# Directorio base
#----------------------------------------------------------------------
base_dir <- "C:\\Users\\danie\\OneDrive\\Escritorio\\Least-cost-diets-and-affordability\\Proyecto Interno\\"
setwd(base_dir)

#----------------------------------------------------------------------
# Directorio salida
#----------------------------------------------------------------------
out_dir <- "working-papers/working-paper-ipc/output/prices/quart"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

#----------------------------------------------------------------------
# Cargar Excel
#----------------------------------------------------------------------
excel_path <- "Precios DANE/OUTPUT_DANE/precios_DANE_deflactados_base2018_12.xlsx"

prices_raw <- read_excel(excel_path) %>%
  clean_names()

#----------------------------------------------------------------------
# Crear fecha mensual y asegurar numéricos
#----------------------------------------------------------------------
prices_clean <- prices_raw %>%
  mutate(
    fecha = ym(anio_mes),
    precio_real_base2018_12 = as.numeric(precio_real_base2018_12)
  ) %>%
  filter(!is.na(fecha),
         !is.na(precio_real_base2018_12))

# Verificación
cat("Filas base limpia:", nrow(prices_clean), "\n")

#----------------------------------------------------------------------
# Crear fecha trimestral real
#----------------------------------------------------------------------
prices_clean <- prices_clean %>%
  mutate(fecha_q = floor_date(fecha, unit = "quarter"))

#----------------------------------------------------------------------
# Promedio trimestral correcto
#----------------------------------------------------------------------
prices_quart <- prices_clean %>%
  group_by(nombre_ciudad, articulo, subclase6, fecha_q) %>%
  summarise(
    precio_real_q = mean(precio_real_base2018_12, na.rm = TRUE),
    .groups = "drop"
  )

cat("Filas trimestrales:", nrow(prices_quart), "\n")

#----------------------------------------------------------------------
# Loop por ciudad y alimento
#----------------------------------------------------------------------
ciudades <- unique(prices_quart$nombre_ciudad)

for (cc in ciudades) {
  
  cat("Procesando:", cc, "\n")
  
  cc_folder <- gsub("[^A-Za-z0-9]", "_", cc)
  dir_city <- file.path(out_dir, cc_folder)
  dir.create(dir_city, recursive = TRUE, showWarnings = FALSE)
  
  df_city <- prices_quart %>%
    filter(nombre_ciudad == cc)
  
  alimentos <- unique(df_city$articulo)
  
  for (a in alimentos) {
    
    df_a <- df_city %>%
      filter(articulo == a)
    
    if (nrow(df_a) < 4) next
    
    subclase_txt <- df_a$subclase6[!is.na(df_a$subclase6)][1]
    
    p <- ggplot(df_a, aes(x = fecha_q, y = precio_real_q)) +
      geom_line(linewidth = 1, color = "black") +
      labs(
        title = paste0(cc, " — ", a),
        subtitle = paste0("Subclase IPC: ", subclase_txt, " (gasto_basico)"),
        x = NULL,
        y = "Precio real trimestral (base 2018)"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        plot.title = element_text(face = "bold", size = 18),
        plot.subtitle = element_text(size = 12)
      )
    
    file_name <- paste0(
      gsub("[^A-Za-z0-9]", "_", cc), "_",
      gsub("[^A-Za-z0-9]", "_", a),
      "_Q.png"
    )
    
    ggsave(
      filename = file.path(dir_city, file_name),
      plot = p,
      width = 11,
      height = 6.5,
      dpi = 300
    )
  }
}



#######################################################################
#                               FIN
#######################################################################
