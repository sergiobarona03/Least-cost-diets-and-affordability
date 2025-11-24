###############################################
### Distribuciones paramétricas del margen  ###
###############################################

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
library(MASS)         # fitdistr()
library(fitdistrplus)

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
data_min_may = whole_tres %>% dplyr::select(cod_mun ,ciudad, nombre_ciudad,
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
        dplyr::select(fecha, ano, mes_num, ciudad, nombre_ciudad, 
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
        dplyr::select(fecha, nombre_ciudad, articulo, precio_500g_ipc,
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


#-----------------------------
# Función: ajustar distribuciones al vector x
#-----------------------------
ajustar_distribuciones <- function(x){
  
  # Nos quedamos con valores finitos y > 0 (márgenes positivos)
  x <- x[is.finite(x) & x > 0]
  n <- length(x)
  
  # Si casi no hay datos, devolvemos NA
  if (n < 5) {
    return(tibble::tibble(
      distribucion = NA_character_,
      AIC = NA_real_
    ))
  }
  
  resultados <- list()
  aics <- c()
  
  # --- Normal ---
  fit_norm <- tryCatch(
    MASS::fitdistr(x, "normal"),
    error = function(e) NULL
  )
  if (!is.null(fit_norm)) {
    resultados$normal <- fit_norm
    aics["normal"] <- AIC(fit_norm)
  }
  
  # --- Lognormal ---
  fit_lnorm <- tryCatch(
    MASS::fitdistr(x, "lognormal"),
    error = function(e) NULL
  )
  if (!is.null(fit_lnorm)) {
    resultados$lognormal <- fit_lnorm
    aics["lognormal"] <- AIC(fit_lnorm)
  }
  
  # --- Gamma ---
  fit_gamma <- tryCatch(
    MASS::fitdistr(x, "gamma"),
    error = function(e) NULL
  )
  if (!is.null(fit_gamma)) {
    resultados$gamma <- fit_gamma
    aics["gamma"] <- AIC(fit_gamma)
  }
  
  # --- Weibull ---
  fit_weibull <- tryCatch(
    MASS::fitdistr(x, "weibull"),
    error = function(e) NULL
  )
  if (!is.null(fit_weibull)) {
    resultados$weibull <- fit_weibull
    aics["weibull"] <- AIC(fit_weibull)
  }
  
  # Si ninguna distribución se pudo ajustar
  if (length(aics) == 0) {
    return(tibble::tibble(
      distribucion = NA_character_,
      AIC = NA_real_
    ))
  }
  
  mejor <- names(which.min(aics))
  
  tibble::tibble(
    distribucion = mejor,
    AIC = as.numeric(min(aics))
  )
}

#------------------------------------------
# Mejor distribución por artículo (según AIC)
#------------------------------------------
dist_articulos <- margen_articulo %>% 
  dplyr::group_by(articulo) %>% 
  dplyr::group_modify(~ ajustar_distribuciones(.x$margen)) %>% 
  dplyr::ungroup()

# Guardar tabla con el “ganador” por artículo
readr::write_csv(
  dist_articulos,
  file.path(out_dir_norm, "mejor_distribucion_por_articulo.csv")
)

###########################################
### Density plots: empírico vs teórico  ###
###########################################

# Carpeta de salida para gráficas de densidad
out_dir_norm <- "margen-dist/output-ciudades/CALI/normalidad"
if (!dir.exists(out_dir_norm)) dir.create(out_dir_norm, recursive = TRUE)

viz_dir_dist <- file.path(out_dir_norm, "densidades")
if (!dir.exists(viz_dir_dist)) dir.create(viz_dir_dist, recursive = TRUE)

#------------------------------------------
# Función para hacer el gráfico por artículo
#------------------------------------------
plot_distribucion <- function(df, articulo_name, out_dir){
  
  # Filtrar datos de ese artículo
  x <- df$margen[df$articulo == articulo_name]
  x <- x[is.finite(x) & x > 0]
  if (length(x) < 5) return(invisible(NULL))
  
  # Ajuste y selección de mejor distribución
  res  <- ajustar_distribuciones(x)
  dist <- res$distribucion
  
  if (is.na(dist)) return(invisible(NULL))
  
  # Ajuste final con la distribución elegida
  fit <- switch(
    dist,
    "normal"    = MASS::fitdistr(x, "normal"),
    "lognormal" = MASS::fitdistr(x, "lognormal"),
    "gamma"     = MASS::fitdistr(x, "gamma"),
    "weibull"   = MASS::fitdistr(x, "weibull"),
    stop("Distribución no reconocida")
  )
  
  params <- fit$estimate
  
  # Función de densidad teórica según distribución
  dens_teorica <- switch(
    dist,
    "normal" = function(z) stats::dnorm(z, mean = params["mean"], sd = params["sd"]),
    "lognormal" = function(z) stats::dlnorm(z, meanlog = params["meanlog"], sdlog = params["sdlog"]),
    "gamma" = function(z) stats::dgamma(z, shape = params["shape"], rate = params["rate"]),
    "weibull" = function(z) stats::dweibull(z, shape = params["shape"], scale = params["scale"])
  )
  
  plot_df <- data.frame(x = x)
  
  p <- ggplot(plot_df, aes(x = x)) +
    # Distribución empírica
    geom_density(
      aes(color = "Distribución empírica"),
      fill = "steelblue",
      alpha = 0.25,
      linewidth = 1
    ) +
    # Distribución ajustada
    stat_function(
      fun = dens_teorica,
      aes(color = "Distribución ajustada"),
      linewidth = 1.2
    ) +
    scale_color_manual(
      values = c(
        "Distribución empírica" = "steelblue",
        "Distribución ajustada" = "firebrick"
      )
    ) +
    labs(
      title    = paste("Ajuste paramétrico del margen –", articulo_name),
      subtitle = paste("Distribución seleccionada (AIC):", dist),
      x        = "Margen (%)",
      y        = "Densidad",
      color    = NULL
    ) +
    theme_bw() +
    theme(legend.position = "bottom")
  
  # Exportar
  file_safe <- stringr::str_replace_all(articulo_name, "[^[:alnum:]]+", "_")
  ggsave(
    filename = file.path(out_dir, paste0("ajuste_", file_safe, ".png")),
    plot     = p,
    width    = 8,
    height   = 5,
    dpi      = 300
  )
  
  invisible(NULL)
}

#------------------------------------------
# Loop: generar gráficas para todos los artículos
#------------------------------------------
for(a in unique(margen_articulo$articulo)){
  message("Graficando distribución para: ", a)
  try({
    plot_distribucion(margen_articulo, a, viz_dir_dist)
  }, silent = TRUE)
}
