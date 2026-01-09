

#########################################
## Estimación de modelo CoNA           ##
## A partir de diferentes estimaciones ##
#########################################

# Definir directorio de trabajo
setwd("C:\\Users\\Portatil\\Desktop\\Least-cost-diets-and-affordability\\Proyecto Interno\\")

##----------------------------##
## M1: Cargar datos estimados ##
##----------------------------##

# Cargar datos (13 ciudades principales)
m1_dataset = read.csv(file.path("working-papers/working-paper-0125/input",
                                "m1_forecast_dataset.csv"))

# Calcular los niveles de precios
m1_dataset = m1_dataset %>%
  mutate(
    pred_q1 = precio_sipsa * (1 + margen_q1 / 100),
    pred_q2 = precio_sipsa * (1 + margen_q2 / 100),
    pred_q3 = precio_sipsa * (1 + margen_q3 / 100),
    fecha = date
  )

m1_dataset = m1_dataset %>% select(fecha, alimento_sipsa, pred_q1,
                                   pred_q2, pred_q3) %>% distinct() %>% na.omit()

##----------------------------##
## M5: Cargar datos estimados ##
##----------------------------##
m5_dataset = read.csv(file.path("working-papers/working-paper-0125/input",
                                "m5_forecast_dataset.csv"))

# Calcular los precios estimados (desde 2016-09-01)
m5_dataset$precio_hat = NA
m5_dataset$precio_hat_lwr = NA
m5_dataset$precio_hat_upr = NA

for (j in 1:nrow(m5_dataset)) {
  if(m5_dataset$fecha[j] < "2016-09-01"){
    m5_dataset$precio_hat[j] = m5_dataset$precio_ipc[j]
  }
  
  if(m5_dataset$fecha[j] >= "2016-09-01" &
     is.na(m5_dataset$precio_hat[j])){
    m5_dataset$precio_hat[j] = m5_dataset$precio_ipc[j-1] + exp(m5_dataset$dlog_fit[j])
    m5_dataset$precio_hat_lwr[j] = m5_dataset$precio_ipc[j-1] + exp(m5_dataset$dlog_lwr[j])
    m5_dataset$precio_hat_upr[j] = m5_dataset$precio_ipc[j-1] + exp(m5_dataset$dlog_upr[j])
  }
}

m5_dataset = m5_dataset %>% select(fecha, alimento_sipsa,
                                   precio_sipsa, precio_ipc,
                                   precio_hat, precio_hat_lwr,
                                   precio_hat_upr)

##----------------------------##
## M6: Cargar datos estimados ##
##----------------------------##

m6_dataset = read.csv(file.path("working-papers/working-paper-0125/input",
                                "m6_forecast_dataset.csv"))

for (j in 1:nrow(m6_dataset)) {
  if(m6_dataset$fecha[j] < "2016-09-01"){
    m6_dataset$fit_level[j] = m6_dataset$obs_level[j]
  }
}



m6_dataset = m6_dataset %>% mutate(precio_ipc = obs_level,
                                   precio_hat = fit_level,
                                   precio_hat_lwr = lwr_level,
                                   precio_hat_upr = upr_level) %>%
  select(fecha, alimento_sipsa, precio_ipc,
                                   precio_hat, precio_hat_lwr,
                                   precio_hat_upr)

##-------------------------------------------------##
## Cargar base de datos de composición nutricional ##
##-------------------------------------------------##

# Cargar datos de composición nutricional
sipsa_tcac <- readxl::read_excel("composicion-nut/1823_mapeo_sipsa_tcac v1.0_2025.xlsx") %>%
  janitor::clean_names() %>%
  rename(alimento_sipsa = alimento_nombre_sipsa)

sipsa_tcac <- sipsa_tcac[, -(35:39)] %>%
  select(
    -c(
      "nombre_original",
      "nota_de_codificacion",
      "factor_de_conversion"
    )
  ) %>%
  distinct()

############### Unir base de datos m1
m1_dataset <- m1_dataset %>%
  left_join(sipsa_tcac, by = "alimento_sipsa")

m1_dataset <- m1_dataset %>%
  mutate(
    pc = parte_comestible_percent,
    precio_q1_100g = pred_q1 * (100 / (5 * pc)),
    precio_q2_100g = pred_q2 * (100 / (5 * pc)),
    precio_q3_100g = pred_q3 * (100 / (5 * pc))
  )

saveRDS(m1_dataset,
        "working-papers\\working-paper-0125\\food_tables\\m1_input.RDS")

############### Unir base de datos m5
m5_dataset <- m5_dataset %>%
  left_join(sipsa_tcac, by = "alimento_sipsa")

m5_dataset <- m5_dataset %>%
  mutate(
    pc = parte_comestible_percent,
    precio_hat_100g = precio_hat * (100 / (5 * pc)),
    precio_lwr_100g = precio_hat_lwr * (100 / (5 * pc)),
    precio_upr_100g = precio_hat_upr * (100 / (5 * pc))
  )

saveRDS(m5_dataset,
        "working-papers\\working-paper-0125\\food_tables\\m5_input.RDS")

############### Unir base de datos m6
m6_dataset <- m6_dataset %>%
  left_join(sipsa_tcac, by = "alimento_sipsa")

m6_dataset <- m6_dataset %>%
  mutate(
    pc = parte_comestible_percent,
    precio_hat_100g = precio_hat * (100 / (5 * pc)),
    precio_lwr_100g = precio_hat_lwr * (100 / (5 * pc)),
    precio_upr_100g = precio_hat_upr * (100 / (5 * pc))
  )

saveRDS(m6_dataset,
        "working-papers\\working-paper-0125\\food_tables\\m6_input.RDS")




