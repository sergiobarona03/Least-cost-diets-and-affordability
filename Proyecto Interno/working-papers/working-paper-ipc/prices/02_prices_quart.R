#######################################################################
## Figura 1 (QUART): Real retail prices by city (QUARTERLY average)
#######################################################################

library(tidyverse)
library(lubridate)
library(janitor)
library(readxl)

#----------------------------------------------------------------------
# Base directory
#----------------------------------------------------------------------
base_dir <- "C:\\Users\\danie\\OneDrive\\Escritorio\\Least-cost-diets-and-affordability\\Proyecto Interno\\"
setwd(base_dir)

#----------------------------------------------------------------------
# Output directory
#----------------------------------------------------------------------
out_dir <- "working-papers/working-paper-ipc/output/prices/quart"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

#----------------------------------------------------------------------
# Excel path (relative to base_dir)
#----------------------------------------------------------------------
excel_path <- "Precios DANE/OUTPUT_DANE/precios_DANE_deflactados_base2018_12.xlsx"

#----------------------------------------------------------------------
# Read data
#----------------------------------------------------------------------
prices_raw <- read_excel(excel_path) %>%
  clean_names()

#----------------------------------------------------------------------
# Helpers
#----------------------------------------------------------------------
to_num <- function(x){
  # robust: "1,234" -> 1.234 ; also works if it is already numeric
  as.numeric(gsub(",", ".", as.character(x)))
}

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
# Monthly date + numeric price (same logic as your working monthly code)
#----------------------------------------------------------------------
prices_clean <- prices_raw %>%
  dplyr::mutate(
    fecha = lubridate::ym(anio_mes),
    precio_real_base2018_12 = to_num(precio_real_base2018_12)
  ) %>%
  dplyr::filter(
    !is.na(fecha),
    !is.na(precio_real_base2018_12)
  )

cat("Rows in clean base:", nrow(prices_clean), "\n")

#----------------------------------------------------------------------
# Quarter date
#----------------------------------------------------------------------
prices_clean <- prices_clean %>%
  dplyr::mutate(fecha_q = floor_date(fecha, unit = "quarter"))

#----------------------------------------------------------------------
# Quarterly average (city x item x subclase x quarter)
#----------------------------------------------------------------------
prices_quart <- prices_clean %>%
  dplyr::group_by(nombre_ciudad, articulo, subclase6, fecha_q) %>%
  dplyr::summarise(
    precio_real_q = mean(precio_real_base2018_12, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::arrange(nombre_ciudad, articulo, fecha_q)

cat("Rows in quarterly base:", nrow(prices_quart), "\n")

#----------------------------------------------------------------------
# Loop by city and item (save in city folders)
#----------------------------------------------------------------------
ciudades <- prices_quart %>% dplyr::distinct(nombre_ciudad) %>% dplyr::pull(nombre_ciudad)

for (cc in ciudades) {
  
  cat("Processing city:", cc, "\n")
  
  cc_folder <- safe_name(cc)
  dir_city <- file.path(out_dir, cc_folder)
  dir.create(dir_city, recursive = TRUE, showWarnings = FALSE)
  
  df_city <- prices_quart %>%
    dplyr::filter(nombre_ciudad == cc) %>%
    dplyr::arrange(fecha_q)
  
  alimentos <- df_city %>% dplyr::distinct(articulo) %>% dplyr::pull(articulo)
  
  for (a in alimentos) {
    
    df_a <- df_city %>%
      dplyr::filter(articulo == a) %>%
      dplyr::arrange(fecha_q)
    
    # At least 4 quarters to make the plot meaningful
    if (nrow(df_a) < 4) next
    
    subclase_txt <- df_a %>% dplyr::distinct(subclase6) %>% dplyr::pull(subclase6)
    subclase_txt <- subclase_txt[!is.na(subclase_txt)][1]
    if (is.na(subclase_txt)) subclase_txt <- "NA"
    
    p <- ggplot(df_a, aes(x = fecha_q, y = precio_real_q)) +
      geom_line(linewidth = 1, color = "black") +
      scale_x_date(
        date_breaks = "1 year",
        date_labels = "%Y Q%q",
        expand = expansion(mult = c(0.01, 0.01))
      ) +
      labs(
        title = paste0(cc, " — ", a),
        subtitle = paste0("IPC subclase: ", subclase_txt, " (gasto_basico)"),
        x = NULL,
        y = "Real quarterly price (2018 base)"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        plot.title = element_text(face = "bold", size = 18),
        plot.subtitle = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
    
    file_name <- paste0(cc_folder, "_", safe_name(a), "_Q.png")
    file_path <- file.path(dir_city, file_name)
    
    ggsave(
      filename = file_path,
      plot = p,
      width = 11,
      height = 6.5,
      dpi = 300,
      bg = "white"
    )
  }
}

cat("✅ Done. Quarterly plots saved in: ", file.path(base_dir, out_dir), "\n")
