
#############################################
#############################################
### Comparación resultados: SIPSA vs. DANE ##
#############################################
#############################################

sipsa_coca = readxl::read_excel("estimadores-banrep/CALI/input/sipsa_coca_cali.csv")
dane_coca = readxl::read_excel("estimadores-banrep/CALI/estimadores-DANE/010925_dane_coca_cali.csv")

overall_coca = rbind(sipsa_coca, dane_coca)

plot_coca_band <- function(data, sexo, age) {
  data_sexo <- data %>% filter(Sex == sexo & Demo_Group == age)
  
  data_wide <- data_sexo %>%
    select(fecha, Demo_Group, escenario, cost_day) %>%
    pivot_wider(names_from = escenario, values_from = cost_day)
  
  data_wide$fecha = as.Date(data_wide$fecha)
  
  ggplot(data_wide, aes(x = fecha, group = Demo_Group)) +
    geom_ribbon(aes(ymin = precio_q1_100g,
                    ymax = precio_q3_100g),
                fill = "red", alpha = 0.15) +
    geom_line(aes(y = precio_q1_100g), color = "red", linetype = 2, size = 0.6) +
    geom_line(aes(y = precio_q3_100g), color = "red", linetype = 2, size = 0.6) +
    geom_line(aes(y = precio_q2_100g), color = "black", size = 1) +
    geom_line(aes(y = precio_100g), color = "green", size = 1) +
    facet_wrap(~ Demo_Group, scales = "free_y", ncol = 3) +
    scale_x_date(date_labels = "%Y-%m", date_breaks = "6 months") +
    labs(
      title = paste("CoCA - Evolución del costo diario con banda entre Q1 y Q3 (", 
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


for (sx in levels(as.factor(overall_coca$Sex))) {
  
  
  for (agx in overall_coca %>% filter(Sex == sx) %>% 
       mutate(Ages = as.factor(as.character(Demo_Group))) %>%
       dplyr::select(Ages) %>% pull() %>% levels()) {
    
    p = plot_coca_band(overall_coca, sx, agx)
    
    
    ggsave(
      filename = paste0("estimadores-banrep/CALI/validar/coca/coca_comparacion_", sx,"_",agx, ".png"),
      plot = p,
      width = 16,
      height = 12,
      dpi = 300
    )
    
  }
  
}

