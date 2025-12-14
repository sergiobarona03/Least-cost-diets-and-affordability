
##################################################################
## Prueba: análisis sobre margen de comercialización (subclase) ##
## Fecha: 31 de julio de 2025                                   ##
##################################################################

# Cargar librerías
library(lubridate)
library(tidyverse)

# Definir directorio de trabajo
setwd( "C:/Users/sergio.barona/Desktop/Least-cost-diets-and-affordability/Proyecto Interno")

# Cargar datos
source("margen-dist/v1-join-ipc-sipsa.R")

######################################
###--------------------------------###
### Datos sobre precios minoristas ###
###--------------------------------###
######################################

# Filtro para Cali, Medellín y Bogotá
whole_tres = retail_whole_18 %>% filter(nombre_ciudad %in% c("MEDELLÍN",
                                                             "CALI",
                                                             "BOGOTÁ D.C.")) %>%
  rename(precio_500g_ipc = precio_500g,
         precio_500g_sipsa = precio_medio)

# Organizar la base de datos
data_min_may = whole_tres %>% select(cod_mun ,ciudad, nombre_ciudad,
                                     Year,
                                     mes, Month, Alimento, retail,
                                     codigo_articulo, precio_500g_sipsa,
                                     precio_500g_ipc) %>%
  rename(mes_num = Month,
         ano = Year,
         sipsa = Alimento,
         articulo = retail)

# Recodificar fecha
data_min_may$fecha = as.Date(paste(data_min_may$ano, 
                                   data_min_may$mes_num, 
                                "01", sep = "-"))

# Filtrar para la ciudad principal
ciudad.input = c("CALI")
data_min_may = data_min_may %>% filter(nombre_ciudad %in% ciudad.input)

# Condición de exclusión:
food.exclude = vector(mode = "list")
foods_no_exclude = levels(as.factor(as.character(data_min_may$sipsa)))

# Función para definir la exclusión
tiene_48 <- function(serie_mat) {
  
  serie_vec <- as.vector(t(serie_mat))
  no_na <- !is.na(serie_vec)
  max_consecutivos <- rle(no_na)$lengths[which(rle(no_na)$values)]
  any(max_consecutivos >= 36)
  
}

for (k in 1:length(foods_no_exclude)) {
  data_k = data_min_may %>% filter(sipsa == foods_no_exclude[[k]])
  
  start1 = min(data_k$fecha)
  end1 = max(data_k$fecha)
  
  series_sipsa = ts(data_k$precio_500g_sipsa, 
                    start = c(substr(start1, 1, 4), substr(start1, 6, 7)), 
                    end = c(substr(end1, 1, 4), substr(end1, 6, 7))
                    , frequency = 12)
  
  series_ipc = ts(data_k$precio_500g_ipc, 
                  start = c(substr(start1, 1, 4), substr(start1, 6, 7)), 
                  end = c(substr(end1, 1, 4), substr(end1, 6, 7))
                  , frequency = 12)
  
  if(!(tiene_48(series_sipsa) & tiene_48(series_ipc))){
    food.exclude = append(food.exclude, unique(data_k$sipsa))
  }
  
}

# Excluimos los alimentos que no cumplen la condición
data_min_may = data_min_may %>% filter(!sipsa %in% unlist(food.exclude))

# Crear la variable "código subclase"
data_min_may$cod_subclase = paste0("0",substr(data_min_may$codigo_articulo, 1, 6), "0")

# Calcular los cuartiles por subclase
margen_articulo <- data_min_may %>% 
  group_by(articulo) %>%
  mutate(
    factor = precio_500g_ipc / precio_500g_sipsa,
    margen = (factor - 1) * 100
  ) %>% filter(margen > 0)

# Calcular los cuartiles (Q1, Q2, Q3) por subclase
cuartiles_articulo <- margen_articulo %>%
  group_by(articulo) %>%
  summarize(
    m_q1 = quantile(margen, 0.25, na.rm = TRUE),
    m_q2 = quantile(margen, 0.50, na.rm = TRUE),
    m_q3 = quantile(margen, 0.75, na.rm = TRUE),
    .groups = "drop"
  ) 

# Guardar el margen por artículo
readr::write_csv(cuartiles_articulo,
                 "margen-dist/output-ciudades/CALI/111225_q1_q3_margen_producto.csv")


# Asociar los margenes a cada producto según artículo
ipc_sipsa = merge(data_min_may,cuartiles_articulo, by = "articulo")

# Prueba con una ciudad y una alimento
city_i = levels(as.factor(ipc_sipsa$nombre_ciudad))
food_j = levels(as.factor(ipc_sipsa$sipsa))

# Base de datos fallidos
list.fail = vector(mode = "list")

# Métricas 
resultados_metricas <- data.frame(
  ciudad = character(),
  articulo = character(),
  rmse_q1 = numeric(),
  rmse_q2 = numeric(),
  rmse_q3 = numeric(),
  mape_q1 = numeric(),
  mape_q2 = numeric(),
  mape_q3 = numeric(),
  stringsAsFactors = FALSE
)
# Bucle
for (i in 1:length(city_i)) {
  for (j in 1:length(food_j)) {
    print(paste0(city_i[i], " - ", food_j[j]))
    
    # Base de datos aux.
    df.aux = ipc_sipsa %>% filter(nombre_ciudad == city_i[i] &
                                    sipsa == food_j[j])
    
    # Filtrar fechas_75
    train.df = df.aux %>% dplyr::filter(fecha <= "2015-01-01")
    test.df = df.aux %>% filter(fecha >= "2015-01-01")
    
    if (all(is.na(df.aux$precio_500g_sipsa))) {
      df.aux2 = ipc_sipsa %>% filter(sipsa == food_j[j])
      list.fail[[length(list.fail) + 1]] <- data.frame(
        ciudad = city_i[i],
        articulo = unique(df.aux2$sipsa),
        cod_subclase = unique(df.aux2$cod_subclase),
        articulo = food_j[j]
      )
    } else {
      
      # Formato long
      library(tidyverse)
      meses_esp <- c("Ene", "Feb", "Mar", "Abr", "May", "Jun", 
                     "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")
      
      # Merge
      ipc_df = test.df %>%
        select(fecha, ano, mes_num, ciudad, nombre_ciudad, 
               cod_subclase,
               codigo_articulo, articulo, precio_500g_ipc, precio_500g_sipsa,
               m_q1, m_q2, m_q3) %>%
        arrange(fecha) %>%
        mutate(precio_hat_q1 = NA,
               precio_hat_q2 = NA,
               precio_hat_q3 = NA)
      
      # Llenar precios hacia adelante usando la variación del IPC
      ipc_df$precio_hat_q1 = ipc_df$precio_500g_sipsa*(1+(ipc_df$m_q1/100))
      ipc_df$precio_hat_q2 = ipc_df$precio_500g_sipsa*(1+(ipc_df$m_q2/100))
      ipc_df$precio_hat_q3 = ipc_df$precio_500g_sipsa*(1+(ipc_df$m_q3/100))
      
      
      # Unir con la base de datos
      test.df2 = test.df %>% left_join(ipc_df[c("fecha",
                                                "precio_hat_q1",
                                                "precio_hat_q2",
                                                "precio_hat_q3")],
                                       by = "fecha") %>%
        select(fecha, nombre_ciudad, articulo, precio_500g_ipc,
               precio_hat_q1, precio_hat_q2, precio_hat_q3, sipsa, precio_500g_sipsa)
      
      # Unir con la base de datos de entrenamiento
      test.df2 = bind_rows(train.df, test.df2)
      
      # Filtrar datos de validación (predicciones no faltantes)
      valid_df <- test.df2 %>% filter(!is.na(precio_hat_q1),
                                      !is.na(precio_hat_q2),
                                      !is.na(precio_hat_q3))
      
      # Cálculo de métricas para cada estimación
      rmse_q1 <- sqrt(mean((valid_df$precio_500g_ipc - valid_df$precio_hat_q1)^2, na.rm = TRUE))
      rmse_q2 <- sqrt(mean((valid_df$precio_500g_ipc - valid_df$precio_hat_q2)^2, na.rm = TRUE))
      rmse_q3 <- sqrt(mean((valid_df$precio_500g_ipc - valid_df$precio_hat_q3)^2, na.rm = TRUE))
      
      mape_q1 <- mean(abs(valid_df$precio_500g_ipc - valid_df$precio_hat_q1) / valid_df$precio_500g_ipc, na.rm = TRUE) * 100
      mape_q2 <- mean(abs(valid_df$precio_500g_ipc - valid_df$precio_hat_q2) / valid_df$precio_500g_ipc, na.rm = TRUE) * 100
      mape_q3 <- mean(abs(valid_df$precio_500g_ipc - valid_df$precio_hat_q3) / valid_df$precio_500g_ipc, na.rm = TRUE) * 100
      
      # Guardar resultados
      resultados_metricas <- resultados_metricas %>%
        add_row(
          ciudad = unique(valid_df$nombre_ciudad),
          articulo = unique(valid_df$articulo),
          rmse_q1 = rmse_q1,
          rmse_q2 = rmse_q2,
          rmse_q3 = rmse_q3,
          mape_q1 = mape_q1,
          mape_q2 = mape_q2,
          mape_q3 = mape_q3
        )
      
      
      # Validar la estimación:
      plot_aux = test.df2 %>%
        ggplot(aes(x = fecha)) +
        geom_line(aes(y = precio_500g_ipc, color = "Precio minorista"), size = 1) +
        geom_line(aes(y = precio_500g_sipsa, color = "Precio mayorista"), size = 1) +
        geom_line(aes(y = precio_hat_q2, color = "Precio estimado"), 
                  linetype = 1, size = 1) +
        geom_ribbon(aes(ymin = precio_hat_q1, ymax = precio_hat_q3),
                    fill = "red", alpha = 0.3, linetype = "dashed",
                    col = "red") +
        labs(
          title = "Comparación entre precio real y estimado",
          subtitle = paste0(
            unique(valid_df$nombre_ciudad), " - ", unique(valid_df$sipsa),
            "\nMargen de comercialización: ", unique(valid_df$articulo),
            "\nRMSE = ", round(rmse_q2, 2),
            " | MAPE Q1 = ", round(mape_q1, 2), "%",
            " | MAPE Q2 = ", round(mape_q2, 2), "%",
            " | MAPE Q3 = ", round(mape_q3, 2), "%",
            "\n(Rango: RMSE Q1=", round(rmse_q1, 2),
            " - Q3=", round(rmse_q3, 2), ")"
          ),
          x = "Fecha",
          y = "Precio (500g)",
          color = " "
        ) +
        scale_color_manual(values = c("Precio minorista" = "black", "Precio estimado" = "red",
                                      "Precio mayorista" = "darkgreen")) +
        theme_bw() +
        theme(
          legend.position = "bottom",
          plot.title = element_text(face = "bold")
        )
      
      
      print(plot_aux)
      
      ggsave(plot = plot_aux, 
             filename = paste0("margen-dist/output-ciudades/CALI/price_hat_producto/",
                               unique(test.df2$sipsa),".png"),
             dpi = 300, height = 12, width = 12)
      
    }
    
  }
  
}


readr::write_csv(resultados_metricas, "margen-dist/output-ciudades/CALI/300725_resultados_metricas_ipc.csv")










