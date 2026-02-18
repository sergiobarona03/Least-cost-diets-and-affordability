#######################################################################
## Figura 1: Precios reales minoristas por ciudad
#######################################################################

#----------------------------------------------------------------------
# Paquetes (solo los necesarios)
#----------------------------------------------------------------------
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
# Directorio de salida
#----------------------------------------------------------------------
out_dir <- "working-papers/working-paper-ipc/output/prices"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

#----------------------------------------------------------------------
# Ruta Excel (relativa al base_dir)
#----------------------------------------------------------------------
excel_path <- "Precios DANE/OUTPUT_DANE/precios_DANE_deflactados_base2018_12.xlsx"

#----------------------------------------------------------------------
# Cargar base
#----------------------------------------------------------------------
prices_df <- read_excel(excel_path) %>%
  clean_names()

#----------------------------------------------------------------------
# Crear fecha mensual desde anio_mes 
#----------------------------------------------------------------------
prices_df <- prices_df %>%
  dplyr::mutate(fecha = lubridate::ym(anio_mes)) %>%
  dplyr::filter(!is.na(fecha))

#----------------------------------------------------------------------
# Asegurar numérico: precio_real_base2018_12 
#----------------------------------------------------------------------
to_num <- function(x){
  as.numeric(gsub(",", ".", as.character(x)))
}

prices_df <- prices_df %>%
  dplyr::mutate(precio_real_base2018_12 = to_num(precio_real_base2018_12)) %>%
  dplyr::filter(!is.na(precio_real_base2018_12))

#----------------------------------------------------------------------
# Helper: nombre de archivo 
#----------------------------------------------------------------------
safe_name <- function(x){
  x %>%
    str_to_upper() %>%
    str_replace_all("Á", "A") %>%
    str_replace_all("É", "E") %>%
    str_replace_all("Í", "I") %>%
    str_replace_all("Ó", "O") %>%
    str_replace_all("Ú", "U") %>%
    str_replace_all("Ñ", "N") %>%
    str_replace_all("[^A-Z0-9]+", "_") %>%
    str_replace_all("^_+|_+$", "")
}

#----------------------------------------------------------------------
# 4 alimentos 
#----------------------------------------------------------------------
foods_fix <- c(
  "HARINA DE TRIGO",
  "HARINA PARA TORTAS",
  "HARINA PRECOCIDA",
  "FECULA DE MAIZ"
)

#----------------------------------------------------------------------
# Loop por ciudad y por alimento
#----------------------------------------------------------------------
ciudades <- prices_df %>% dplyr::distinct(nombre_ciudad) %>% dplyr::pull(nombre_ciudad)

for (cc in ciudades) {
  
  cat("Procesando ciudad:", cc, "\n")
  
  cc_folder <- cc %>%
    str_replace_all("\\.", "") %>%
    str_replace_all(",", "") %>%
    str_replace_all(" ", "_") %>%
    safe_name()
  
  dir_city <- file.path(out_dir, cc_folder)
  dir.create(dir_city, recursive = TRUE, showWarnings = FALSE)
  
  df_city <- prices_df %>%
    dplyr::filter(nombre_ciudad == cc) %>%
    dplyr::arrange(fecha)
  
  alimentos <- df_city %>% dplyr::distinct(articulo) %>% dplyr::pull(articulo)
  
  for (a in alimentos) {
    
    df_a <- df_city %>%
      dplyr::filter(articulo == a) %>%
      dplyr::arrange(fecha)
    
    if (!(a %in% foods_fix) && nrow(df_a) < 6) next
    
    if (nrow(df_a) < 2) next
    
    subclase_txt <- df_a %>% dplyr::distinct(subclase6) %>% dplyr::pull(subclase6)
    subclase_txt <- subclase_txt[!is.na(subclase_txt)][1]
    if (is.na(subclase_txt)) subclase_txt <- "NA"
    
    p <- ggplot(df_a, aes(x = fecha, y = precio_real_base2018_12)) +
      geom_line(linewidth = 1, color = "black") +
      labs(
        title = paste0(cc, " — ", a),
        subtitle = paste0("Subclase IPC: ", subclase_txt, " (gasto_basico)"),
        x = NULL,
        y = "Precio real (base 2018)"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        plot.title = element_text(face = "bold", size = 18),
        plot.subtitle = element_text(size = 12)
      )
    
    file_name <- paste0(cc_folder, "_", safe_name(a), ".png")
    file_path <- file.path(dir_city, file_name)
    
    ggsave(filename = file_path, plot = p, width = 11, height = 6.5, dpi = 300)
  }
}

cat("✅ Listo. Gráficos guardados en: ", file.path(base_dir, out_dir), "\n")

