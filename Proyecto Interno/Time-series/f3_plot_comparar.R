

library(dplyr)
library(ggplot2)
library(stringr)

comparar_precios <- function(df, cod_articulo, 
                             cod_ciudad) {
  
  df_filtrado <- df %>%
    filter(as.numeric(cod_mun) == cod_ciudad &
             as.numeric(codigo_articulo) == cod_articulo) 
  
  if (nrow(df_filtrado) == 0) {
    warning("No hay datos para esa ciudad.")
    return(NULL)
  }
  
  df_filtrado <- df_filtrado %>%
    mutate(
      fecha = as.Date(paste(ano, mes_num, "01", sep = "-")),
      precio = as.numeric(precio),
      precio_medio = as.numeric(precio_medio)
    ) %>%
    filter(!is.na(precio), !is.na(fecha)) %>%
    select(fecha, nombre_ciudad, articulo, sipsa, precio, precio_medio)
  
  df_melt = reshape2::melt(df_filtrado, id.vars = c("fecha", "nombre_ciudad",
                                                    "articulo", "sipsa")) %>%
    na.omit()
  df_melt$variable = as.character(df_melt$variable)
  df_melt$variable[df_melt$variable == "precio"] = "Precio IPC"
  df_melt$variable[df_melt$variable == "precio_medio"] = "Precio SIPSA"
  
  df_melt <- df_melt %>%
    group_by(nombre_ciudad, articulo, sipsa) %>%
    filter(all(c("Precio IPC", "Precio SIPSA") %in% variable)) %>%
    ungroup()
  
  df_melt %>%
    mutate(
      faceta = paste(articulo, sipsa, sep = " | "),
      value_label = round(value, 0)
    ) %>%
    ggplot(aes(x = fecha, y = value, 
               color = variable, group = variable)) +
    geom_line(linewidth = 1) +
    geom_point(size = 3) +
    geom_text(
      aes(label = value_label),
      vjust = -0.7,
      size = 3,
      show.legend = FALSE,
      col = "black"
    ) +
    facet_wrap(~ faceta, scales = "free_y") +
    labs(
      title = "Comparación de precios Retail (IPC) y Mayorista (SIPSA)",
      subtitle = paste0("Ciudad principal: ", unique(df_melt$nombre_ciudad)),
      x = "Fecha",
      y = "Precio (COP)",
      color = "Fuente"
    ) +
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      strip.text = element_text(size = 10, face = "bold"),
      legend.title = element_blank(),
      legend.position = "bottom",
      legend.background = element_rect(fill = "white", color = "black"),
      legend.box.background = element_rect(color = "black")
    )
  
  
}
