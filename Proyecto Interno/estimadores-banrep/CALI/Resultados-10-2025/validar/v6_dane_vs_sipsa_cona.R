#############################################
#############################################
### Comparación resultados: SIPSA vs. DANE ##
#############################################
#############################################

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

setwd("C:/Users/sergio.barona/Desktop/Least-cost-diets-and-affordability/Proyecto Interno")

sipsa_cona <- read_excel(
  "estimadores-banrep/CALI/Resultados-10-2025/cona/010825_cona_cali.xlsx"
) %>%
  mutate(fecha = as.Date(fecha))

dane_cona  <- read_excel(
  "estimadores-banrep\\CALI\\DANE\\output\\v6\\v6_dane_cona_cali.xlsx"
) %>%
  mutate(
    fecha = as.Date(fecha),
    
    Sex = case_when(
      Sex %in% c(0, "0") ~ "Hombres",
      Sex %in% c(1, "1") ~ "Mujeres",
      TRUE               ~ as.character(Sex)
    ),
    escenario = "precio_100g"
  )


dane_cona2 <- readxl::read_excel(
  "estimadores-banrep\\CALI\\DANE\\output\\v6\\v6_dane_cona_cali.xlsx"
)

overall_cona <- rbind(sipsa_cona, dane_cona)

# ----------------------------------------------------------- #

plot_cona_band <- function(data, sexo, age) {
  data_sexo <- data %>%
    filter(Sex == sexo & Demo_Group == age)
  
  # Pivotar para columnas Q1, Q2, Q3
  data_wide <- data_sexo %>%
    select(fecha, Demo_Group, escenario, cost_day) %>%
    pivot_wider(names_from = escenario, values_from = cost_day)
  
  data_wide$fecha <- as.Date(data_wide$fecha)
  
  for (col in c("precio_q1_100g", "precio_q2_100g", "precio_q3_100g")) {
    if (!col %in% names(data_wide)) {
      data_wide[[col]] <- NA_real_
    }
  }
  # ----------------------------------------------------------- #
  
  ggplot(data_wide, aes(x = fecha, group = Demo_Group)) +
    geom_ribbon(aes(ymin = precio_q1_100g,
                    ymax = precio_q3_100g),
                fill = "red", alpha = 0.15, na.rm = TRUE) +  # Banda roja
    geom_line(aes(y = precio_q1_100g), 
              color = "red", linetype = 2, size = 0.6, na.rm = TRUE) +
    geom_line(aes(y = precio_q3_100g), 
              color = "red", linetype = 2, size = 0.6, na.rm = TRUE) +
    geom_line(aes(y = precio_q2_100g), color = "black", size = 1, na.rm = TRUE) +
    geom_line(aes(y = precio_100g),    color = "green", size = 1, na.rm = TRUE) +
    facet_wrap(~ Demo_Group, scales = "free_y", ncol = 3) +
    scale_x_date(date_labels = "%Y-%m", date_breaks = "6 months") +
    labs(
      title = paste("CoNA - Evolución del costo diario con banda entre Q1 y Q3 (", 
                    sexo, " - ", age ,")", sep = ""),
      x = "Fecha",
      y = "Costo diario (COP)"
    ) +
    theme_bw(base_size = 14) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid.minor = element_blank(),
      legend.position = "none"
    )
}


for (sx in levels(as.factor(overall_cona$Sex))) {
  for (agx in overall_cona %>% filter(Sex == sx) %>% 
       mutate(Ages = as.factor(as.character(Demo_Group))) %>%
       dplyr::select(Ages) %>% pull() %>% levels()) {
    
    p <- plot_cona_band(overall_cona, sx, agx)
    
    ggsave(
      filename = paste0(
        "estimadores-banrep\\CALI\\DANE\\validar\\v6\\cona_comparacion_",
        sx,"_",agx, ".png"
      ),
      plot   = p,
      width  = 16,
      height = 12,
      dpi    = 300
    )
  }
}
