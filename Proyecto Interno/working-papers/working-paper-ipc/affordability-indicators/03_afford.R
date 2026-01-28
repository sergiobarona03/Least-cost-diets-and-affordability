#######################################################################
## Cálculo de indicadores de asequibilidad (Afford) ciudad–mes
## y construcción de INCIDENCIA por modelo (CoCA, CoNA)
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
income_dir      <- file.path(out_dir, "incomecol")
afford_cost_dir <- file.path(out_dir, "affordability")

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
date_vector <- sort(unique(income_df$fecha))   # iteraremos por índice

#######################################################################
## BLOQUE 1: Cálculo de Afford ciudad–mes (si ya lo tienes, puedes saltarlo)
#######################################################################

res_afford <- list()
k <- 1

for (city.x in city_vector) {
  for (t in 1:length(date_vector)) {
    
    fecha_raw <- date_vector[t]
    fecha.x   <- fecha_raw   # ya viene como Date; si no, abajo lo corregimos
    
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

afford_df <- bind_rows(res_afford)

# Guardar tabla de resultados por decil
saveRDS(afford_df, file = file.path(afford_metrics_dir, "Afford_city_month.rds"))
write.csv(afford_df,
          file = file.path(afford_metrics_dir, "Afford_city_month.csv"),
          row.names = FALSE)

#######################################################################
## BLOQUE 2: Construcción de la INCIDENCIA por ciudad–mes–modelo
#######################################################################

# Si prefieres ENSAMBLAR desde archivo (por si ya está creado):
# afford_df <- read.csv(file.path(afford_metrics_dir, "Afford_city_month.csv"))
# suppressWarnings({ afford_df$fecha <- as.Date(afford_df$fecha) })

# La tabla afford_df tiene columnas:
# deciles, rate, gap, severity, model, ciudad, fecha

# Para cada ciudad–mes–modelo, calculamos incidencia total:
# Suponiendo que cada decil tiene el mismo peso (10%),
# incidencia (%) = (1 / número_de_deciles) * sum(rate_d)
# Ejemplo: solo Decil 1 con rate = 48.2 -> incidencia = 48.2 / 10 = 4.82 %

aff_inc <- afford_df %>%
  group_by(ciudad, fecha, model) %>%
  dplyr::summarise(
    n_deciles = n_distinct(deciles),
    sum_rate  = sum(rate, na.rm = TRUE),
    incidence = (1 / n_deciles) * sum_rate,  # porcentaje total
    .groups   = "drop"
  )

# Guardar tabla de incidencia
saveRDS(aff_inc, file = file.path(afford_metrics_dir, "Afford_incidence_city_month.rds"))
write.csv(aff_inc,
          file = file.path(afford_metrics_dir, "Afford_incidence_city_month.csv"),
          row.names = FALSE)

#######################################################################
## BLOQUE 3: Gráficas de incidencia para CoCA y CoNA
#######################################################################

# Asegurar que fecha es Date
suppressWarnings({
  aff_inc$fecha <- as.Date(aff_inc$fecha)
})

#--------------------------
# Gráfica 1: CoCA
#--------------------------
aff_inc_coca <- aff_inc %>%
  filter(model == "CoCA")

g_coca_inc <- ggplot(aff_inc_coca,
                     aes(x = fecha, y = incidence, color = ciudad)) +
  geom_line() +
  labs(
    title = "Incidencia de pobreza de asequibilidad (CoCA)",
    x = "Fecha",
    y = "Incidencia (%)",
    color = "Ciudad"
  ) +
  theme_classic()

ggsave(
  filename = file.path(afford_metrics_dir, "Afford_Incidence_CoCA_city_time.png"),
  plot     = g_coca_inc,
  width    = 10,
  height   = 5
)

#--------------------------
# Gráfica 2: CoNA
#--------------------------
aff_inc_cona <- aff_inc %>%
  filter(model == "CoNA")

g_cona_inc <- ggplot(aff_inc_cona,
                     aes(x = fecha, y = incidence, color = ciudad)) +
  geom_line() +
  labs(
    title = "Incidencia de pobreza de asequibilidad (CoNA)",
    x = "Fecha",
    y = "Incidencia (%)",
    color = "Ciudad"
  ) +
  theme_classic()

ggsave(
  filename = file.path(afford_metrics_dir, "Afford_Incidence_CoNA_city_time.png"),
  plot     = g_cona_inc,
  width    = 10,
  height   = 5
)
