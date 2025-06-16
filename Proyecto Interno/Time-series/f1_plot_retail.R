library(dplyr)
library(stringr)
library(lubridate)
library(ggplot2)

graficar_precio <- function(df, alimento, ciudad) {
  
  meses_diccionario <- c(
    "enero" = "01", "febrero" = "02", "marzo" = "03", "abril" = "04",
    "mayo" = "05", "junio" = "06", "julio" = "07", "agosto" = "08",
    "septiembre" = "09", "octubre" = "10", "noviembre" = "11", "diciembre" = "12",
    "ene" = "01", "feb" = "02", "mar" = "03", "abr" = "04",
    "may" = "05", "jun" = "06", "jul" = "07", "ago" = "08",
    "sep" = "09", "oct" = "10", "nov" = "11", "dic" = "12"
  )
  
  limpiar_texto <- function(x) {
    x %>%
      str_to_lower() %>%
      str_replace_all("[á]", "a") %>%
      str_replace_all("[é]", "e") %>%
      str_replace_all("[í]", "i") %>%
      str_replace_all("[ó]", "o") %>%
      str_replace_all("[ú]", "u") %>%
      str_replace_all("[ñ]", "n") %>%
      str_trim()
  }
  
  alimento_limpio <- limpiar_texto(alimento)
  ciudades_limpias <- limpiar_texto(ciudad)
  
  datos_filtrados <- df %>%
    mutate(
      ciudad_limpia = limpiar_texto(nombre_ciudad),
      articulo_limpio = limpiar_texto(articulo),
      mes_limpio = limpiar_texto(mes),
      mes_num = meses_diccionario[mes_limpio],
      fecha = suppressWarnings(ymd(paste(ano, mes_num, "01"))),
      precio_num = as.numeric(str_replace(precio, ",", "."))
    ) %>%
    filter(
      articulo_limpio == alimento_limpio,
      ciudad_limpia %in% ciudades_limpias,
      !is.na(fecha),
      !is.na(precio_num)
    ) %>%
    arrange(fecha)
  
  if (nrow(datos_filtrados) == 0) {
    warning("No hay datos disponibles para esas ciudades y ese alimento.")
    return(NULL)
  }
  
  ggplot(datos_filtrados, aes(x = fecha, y = precio_num, color = nombre_ciudad)) +
    geom_line() +
    geom_point(size = 1.5) +
    labs(
      title = paste("Evolución del precio de", str_to_title(alimento)),
      subtitle = paste("Ciudades:", paste(unique(ciudad), collapse = ", ")),
      x = "Fecha",
      y = "Precio (COP)",
      color = "Ciudad"
    ) +
    theme_bw() +
    theme(
      legend.position = c(0.98, 0.2),
      legend.justification = c("right", "top"),
      legend.background = element_rect(fill = "white", color = "black"),
      legend.box.background = element_rect(color = "black")
    )
}
