library(dplyr)
library(stringr)
library(ggplot2)
library(lubridate)

plot_wholesale <- function(df, alimento, ciudades) {

  # Función para limpiar texto
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
  
  # Limpiar entradas
  alimento_limpio2 <- limpiar_texto(alimento)
  ciudades_limpias <- limpiar_texto(ciudades)
  
  # Procesar datos
  datos_filtrados <- df %>%
    mutate(
      ciudad_limpia = limpiar_texto(nombre_ciudad),
      alimento_limpio = limpiar_texto(Alimento),
      fecha = suppressWarnings(ymd(paste(Year, Month, "01")))
    ) %>%
    filter(
      alimento_limpio == alimento_limpio2,
      ciudad_limpia %in% ciudades_limpias,
      !is.na(fecha),
      !is.na(precio_medio)
    ) %>%
    arrange(fecha)
  
  if (nrow(datos_filtrados) == 0) {
    warning("No hay datos disponibles para esas ciudades y ese alimento.")
    return(NULL)
  }
  
  # Graficar
  ggplot(datos_filtrados, aes(x = fecha, y = precio_medio, color = nombre_ciudad)) +
    geom_line() +
    geom_point(size = 1.5) +
    labs(
      title = paste("Evolución del precio mayorista de", str_to_title(alimento)),
      subtitle = paste("Ciudades:", paste(unique(ciudades), collapse = ", ")),
      x = "Fecha",
      y = "Precio medio (COP)",
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
