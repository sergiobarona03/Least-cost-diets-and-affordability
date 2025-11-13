##################################################################
## Prueba: análisis sobre margen de comercialización (subclase) ##
## PRUEBAS DE NORMALIDAD                                        ##
##################################################################

# Cargar librerías
library(lubridate)
library(tidyverse)
library(dplyr)
library(purrr)
library(tidyr)
library(moments)   # skewness() y kurtosis()
library(tseries)   # jarque.bera.test()
library(broom)
library(ggplot2)
library(readr)
library(stringr)
library(forcats)
library(stringr)
library(stringi)

# Definir directorio de trabajo
setwd("C:/Users/danie/OneDrive/Escritorio/Least-cost-diets-and-affordability/Proyecto Interno")

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
                 "margen-dist/output-ciudades/CALI/300725_q1_q3_margen_producto.csv")


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
    }
  }
}


#########################################
### Normalidad de márgenes por artículo #
#########################################

# Carpetas de salida
out_dir_norm <- "margen-dist/output-ciudades/CALI/normalidad"
if (!dir.exists(out_dir_norm)) dir.create(out_dir_norm, recursive = TRUE)
viz_dir <- file.path(out_dir_norm, "viz_simple")
if (!dir.exists(viz_dir)) dir.create(viz_dir, recursive = TRUE)

# --- Función para métricas + tests ---
.normalidad_vec <- function(x){
  x <- x[is.finite(x)]
  n <- length(x)
  m  <- if (n > 0) mean(x) else NA_real_
  v  <- if (n > 1) var(x)  else NA_real_
  
  # Curtosis 2
  kurt <- if (is.finite(v) && v > 0 && n > 3) {
    mx <- x - m
    m4 <- mean(mx^4)
    m4 / (v^2)
  } else NA_real_
  
  p_shapiro <- tryCatch({
    if (n >= 3 && n <= 5000) stats::shapiro.test(x)$p.value else NA_real_
  }, error = function(e) NA_real_)
  
  p_jb <- tryCatch({
    if (n >= 4) tseries::jarque.bera.test(x)$p.value else NA_real_
  }, error = function(e) NA_real_)
  
  tibble(
    n = n, media = m, var = v,
    kurtosis = kurt,
    p_shapiro = p_shapiro,
    p_jarque_bera = p_jb
  )
}

# --- Cálculo por artículo ---
normalidad_articulo <-
  margen_articulo %>%
  dplyr::group_by(articulo) %>%
  dplyr::summarise(.normalidad_vec(margen), .groups = "drop") %>%
  dplyr::mutate(
    # Ajuste BH por comparaciones múltiples
    p_shapiro_bh     = p.adjust(p_shapiro, method = "BH"),
    p_jarque_bera_bh = p.adjust(p_jarque_bera, method = "BH"),
    # Indicadores a 5%
    rej_shapiro      = !is.na(p_shapiro_bh)     & p_shapiro_bh     < 0.05,
    rej_jarque_bera  = !is.na(p_jarque_bera_bh) & p_jarque_bera_bh < 0.05,
    rechaza_normalidad_ambas  = rej_shapiro & rej_jarque_bera,
    rechaza_normalidad_alguna = rej_shapiro | rej_jarque_bera
  ) %>%
  dplyr::arrange(articulo)

# Guardar tabla
readr::write_csv(
  normalidad_articulo,
  file.path(out_dir_norm, "curtosis_y_tests_por_articulo.csv")
)

# ================================
# Gráficas de normalidad
# ================================

# 1) Barras: curtosis por artículo 
df_k <- normalidad_articulo %>%
  mutate(articulo = fct_reorder(articulo, kurtosis))

p_kurt_simple <- ggplot(df_k, aes(x = articulo, y = kurtosis)) +
  geom_col() +
  geom_hline(yintercept = 3, linetype = "dashed") +
  coord_flip() +
  labs(
    title = "Curtosis por artículo",
    subtitle = "Línea punteada = 3 (normal)",
    x = NULL, y = "Curtosis"
  ) +
  theme_bw(base_size = 11)

ggsave(filename = file.path(viz_dir, "kurtosis_por_articulo.png"),
       plot = p_kurt_simple, width = 9, height = 12, dpi = 300)

# 2) P-values BH: Shapiro y Jarque-Bera por artículo (0.05)
df_p <- normalidad_articulo %>%
  select(articulo, Shapiro = p_shapiro_bh, `Jarque-Bera` = p_jarque_bera_bh) %>%
  pivot_longer(cols = c(Shapiro, `Jarque-Bera`),
               names_to = "prueba", values_to = "p_bh") %>%
  mutate(articulo = fct_reorder(articulo, p_bh, .fun = function(z) {
    suppressWarnings(min(z, na.rm = TRUE))  # ordena por p más chico
  }))

p_pvals_simple <- ggplot(df_p, aes(x = articulo, y = p_bh, shape = prueba)) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0.05, linetype = "dashed") +
  coord_flip() +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
  labs(
    title = "p-values ajustados (BH) por artículo",
    subtitle = "Línea punteada = 0.05",
    x = NULL, y = "p (0–1)"
  ) +
  theme_bw(base_size = 11) +
  theme(legend.position = "bottom")

ggsave(filename = file.path(viz_dir, "pvalues_por_articulo.png"),
       plot = p_pvals_simple, width = 9, height = 12, dpi = 300)
