#############################################
#############################################
### Comparación resultados: SIPSA vs. DANE ##
#############################################
#############################################

sipsa_cona = readxl::read_excel("estimadores-banrep/CALI/Resultados-09-2025/input/sipsa_cona_cali.csv")
dane_cona = readxl::read_excel("estimadores-banrep/CALI/Resultados-09-2025/estimadores-DANE/010925_dane_cona_cali.csv")
dane_cona$escenario = "precio_100g"

overall_cona = rbind(sipsa_cona, dane_cona)


plot_cona_band <- function(data, sexo, age) {
  data_sexo <- data %>% filter(Sex == sexo & Demo_Group == age)
  
  # Pivotar para columnas Q1, Q2, Q3
  data_wide <- data_sexo %>%
    select(fecha, Demo_Group, escenario, cost_day) %>%
    pivot_wider(names_from = escenario, values_from = cost_day)
  
  data_wide$fecha = as.Date(data_wide$fecha)
  
  ggplot(data_wide, aes(x = fecha, group = Demo_Group)) +
    geom_ribbon(aes(ymin = precio_q1_100g,
                    ymax = precio_q3_100g),
                fill = "red", alpha = 0.15) +  # Banda roja transparente
    geom_line(aes(y = precio_q1_100g), 
              color = "red", linetype = 2, size = 0.6) +  # Q1 roja punteada
    geom_line(aes(y = precio_q3_100g), 
              color = "red", linetype = 2, size = 0.6) +  # Q3 roja punteada
    geom_line(aes(y = precio_q2_100g), color = "black", size = 1) +  # Q2 negra sólida
    geom_line(aes(y = precio_100g), color = "green", size = 1) +  # Q2 negra sólida
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

# --- Loop para imprimir gráficos por sexo y por edad ---
for (sx in levels(as.factor(overall_cona$Sex))) {
  

  
  for (agx in overall_cona %>% filter(Sex == sx) %>% 
       mutate(Ages = as.factor(as.character(Demo_Group))) %>%
       dplyr::select(Ages) %>% pull() %>% levels()) {
    

    
    p = (plot_cona_band(overall_cona, sx, agx))
    
    
    ggsave(
      filename = paste0("estimadores-banrep/CALI/Resultados-09-2025/validar/cona/cona_comparacion_", sx,"_",agx, ".png"),
      plot = p,
      width = 16,
      height = 12,
      dpi = 300
    )
    
  }
  
}
