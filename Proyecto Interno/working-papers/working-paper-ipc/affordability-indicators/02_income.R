#######################################################################
## Cálculo de indicadores de asequibilidad (Afford) ciudad–mes
#######################################################################

#----------------------------------------------------------------------
# Paquetes
#----------------------------------------------------------------------
library(devtools)
library(tidyverse)
library(FoodpriceR)
library(lubridate)
library(reshape2)

#----------------------------------------------------------------------
# Directorios y función Afford
#----------------------------------------------------------------------
base_dir <- "C:/Users/Portatil/Desktop/Least-cost-diets-and-affordability/Proyecto Interno"
setwd(base_dir)

# Función Afford (ajusta ruta si cambia)
source("working-papers\\working-paper-ipc\\affordability-indicators\\aux_functions\\Afford_Expansion.R")

# Directorio general de output
out_dir <- file.path(base_dir, "working-papers/working-paper-ipc/output")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# Directorios ya existentes de costo e ingreso
income_dir       <- file.path(out_dir, "incomecol")
afford_cost_dir  <- file.path(out_dir, "affordability")

# NUEVO directorio para métricas de asequibilidad (resultados de Afford)
afford_metrics_dir <- file.path(out_dir, "affordability_metrics")
dir.create(afford_metrics_dir, recursive = TRUE, showWarnings = FALSE)

#----------------------------------------------------------------------
# Cargar resultados previos de CoCA / CoNA e ingreso
#----------------------------------------------------------------------
coca_df   <- read.csv(file.path(afford_cost_dir, "CoCA_city_month.csv"))
cona_df   <- read.csv(file.path(afford_cost_dir, "CoNA_city_month.csv"))
income_df <- read.csv(file.path(income_dir, "IncomeCol_city_month.csv"))

#----------------------------------------------------------------------
# Asegurar que las fechas sean Date en los tres data frames
#----------------------------------------------------------------------
suppressWarnings({
  coca_df$fecha   <- as.Date(coca_df$fecha)
  cona_df$fecha   <- as.Date(cona_df$fecha)
  income_df$fecha <- as.Date(income_df$fecha)
})

#----------------------------------------------------------------------
# Vectores ciudad–mes
#----------------------------------------------------------------------
city_vector <- sort(unique(income_df$ciudad_panel))
date_vector <- sort(unique(income_df$fecha))   # puede ser Date, pero igual iteramos por índice

#----------------------------------------------------------------------
# Bucle Afford ciudad–mes usando índices para la fecha
#----------------------------------------------------------------------
res_afford <- list()
k <- 1

for (city.x in city_vector) {
  for (t in 1:length(date_vector)) {
    
    fecha_raw <- date_vector[t]
    fecha.x   <- fecha_raw   # ya debería ser Date; si no, convertimos
    
    # Si viene como NA o algo raro, intentamos forzar Date
    if (is.na(fecha.x)) {
      fecha.x <- try(as.Date(fecha_raw), silent = TRUE)
    }
    if (inherits(fecha.x, "try-error") || is.na(fecha.x)) {
      message("Fecha no legible para índice t = ", t, " (", fecha_raw, ")")
      next
    }
    
    message("Procesando Afford: ciudad = ", city.x,
            " | fecha = ", fecha.x, " ...")
    
    #--------------------------
    # Filtrar IncomeCol
    #--------------------------
    inc_aux <- income_df %>%
      filter(
        ciudad_panel == city.x,
        fecha == fecha.x
      )
    
    if (nrow(inc_aux) == 0) {
      message("   -> sin IncomeCol para esta ciudad/fecha")
      next
    }
    
    #--------------------------
    # Filtrar CoCA y CoNA
    #--------------------------
    coca_aux <- coca_df %>%
      filter(
        ciudad == city.x,
        fecha  == fecha.x
      )
    
    cona_aux <- cona_df %>%
      filter(
        ciudad == city.x,
        fecha  == fecha.x
      )
    
    if (nrow(coca_aux) == 0 || nrow(cona_aux) == 0) {
      message("   -> sin CoCA/CoNA para esta ciudad/fecha")
      next
    }
    
    #--------------------------
    # Cálculo de Afford
    #--------------------------
    out <- try(
      Afford(
        Hexpense   = inc_aux,
        Model_CoCA = coca_aux,
        Model_CoNA = cona_aux
      ),
      silent = TRUE
    )
    
    if (inherits(out, "try-error") || is.null(out)) {
      message("   -> error en Afford()")
      next
    }
    
    po <- out$Poverty_outcome %>%
      mutate(
        ciudad = city.x,
        fecha  = fecha.x
      )
    
    res_afford[[k]] <- po
    k <- k + 1
  }
}

#----------------------------------------------------------------------
# Consolidar resultados
#----------------------------------------------------------------------
afford_df <- bind_rows(res_afford)

# Guardar tabla completa
saveRDS(afford_df, file = file.path(afford_metrics_dir, "Afford_city_month.rds"))
write.csv(afford_df,
          file = file.path(afford_metrics_dir, "Afford_city_month.csv"),
          row.names = FALSE)

#######################################################################
## Gráficos básicos de los indicadores de asequibilidad
#######################################################################

# Resumen promedio por ciudad–fecha–modelo
aff_sum <- afford_df %>%
  group_by(ciudad, fecha, model) %>%
  summarise(
    rate     = mean(rate, na.rm = TRUE),
    gap      = mean(gap, na.rm = TRUE),
    severity = mean(severity, na.rm = TRUE),
    .groups  = "drop"
  )

# Asegurar que fecha es Date
suppressWarnings({
  aff_sum$fecha <- as.Date(aff_sum$fecha)
})

#--------------------------
# 1) Tasa de pobreza (rate)
#--------------------------
g_rate <- ggplot(aff_sum,
                 aes(x = fecha, y = rate, color = ciudad)) +
  geom_line() +
  facet_wrap(~ model) +
  labs(
    title = "Affordability: Tasa de pobreza por ciudad",
    x = "Fecha",
    y = "Poverty rate",
    color = "Ciudad"
  ) +
  theme_minimal()

ggsave(
  filename = file.path(afford_metrics_dir, "Afford_PovertyRate_city_model.png"),
  plot     = g_rate,
  width    = 10,
  height   = 5
)

#--------------------------
# 2) Brecha de pobreza (gap)
#--------------------------
g_gap <- ggplot(aff_sum,
                aes(x = fecha, y = gap, color = ciudad)) +
  geom_line() +
  facet_wrap(~ model) +
  labs(
    title = "Affordability: Brecha de pobreza por ciudad",
    x = "Fecha",
    y = "Poverty gap",
    color = "Ciudad"
  ) +
  theme_minimal()

ggsave(
  filename = file.path(afford_metrics_dir, "Afford_PovertyGap_city_model.png"),
  plot     = g_gap,
  width    = 10,
  height   = 5
)

