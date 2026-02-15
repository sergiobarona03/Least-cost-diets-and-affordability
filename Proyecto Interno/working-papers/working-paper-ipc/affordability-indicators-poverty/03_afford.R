#######################################################################
## Cálculo de indicadores de asequibilidad (Afford)
## Compatible con IncomeCol_YYYY.rds
#######################################################################

#----------------------------------------------------------------------
# Paquetes
#----------------------------------------------------------------------
library(devtools)
library(tidyverse)
library(FoodpriceR)
library(lubridate)
library(janitor)

#----------------------------------------------------------------------
# Directorios
#----------------------------------------------------------------------
base_dir <- "C:\\Users\\danie\\OneDrive\\Escritorio\\Least-cost-diets-and-affordability\\Proyecto Interno\\"
setwd(base_dir)

source("working-papers/working-paper-ipc/affordability-indicators/aux_functions/Afford_Expansion.R")

out_dir <- file.path(base_dir, "working-papers/working-paper-ipc/output")

income_dir          <- file.path(out_dir, "incomecol")
afford_cost_dir     <- file.path(out_dir, "affordability")
afford_metrics_dir  <- file.path(out_dir, "affordability_metrics")

dir.create(afford_metrics_dir, recursive = TRUE, showWarnings = FALSE)

#----------------------------------------------------------------------
#  Cargar IncomeCol (archivos anuales)
#----------------------------------------------------------------------

income_files <- list.files(income_dir,
                           pattern = "IncomeCol_.*\\.rds$",
                           full.names = TRUE)

income_list <- lapply(income_files, readRDS)

income_df <- bind_rows(income_list)

#-------------------------
# Ajustes estructurales
#-------------------------
income_df <- income_df %>%
  mutate(
    fecha   = ymd(paste(year, mes, "01", sep = "-")),
    ciudad  = dominio,
    weight  = as.numeric(fex_c)
  )

#----------------------------------------------------------------------
#  Cargar CoCA y CoNA
#----------------------------------------------------------------------

coca_df <- read.csv(file.path(afford_cost_dir, "CoCA_city_month.csv"))
cona_df <- read.csv(file.path(afford_cost_dir, "CoNA_city_month.csv"))

coca_df$fecha <- as.Date(coca_df$fecha)
cona_df$fecha <- as.Date(cona_df$fecha)

#----------------------------------------------------------------------
# Vectores ciudad–fecha
#----------------------------------------------------------------------

income_df$ciudad[income_df$ciudad == "MEDELLIN"] = "MEDELLÍN"
income_df$ciudad[income_df$ciudad == "BOGOTA"] = "BOGOTÁ D.C."

income_df$ung = income_df$nug
income_df$deciles = paste0("Decil ", income_df$deciles)

city_vector <- sort(unique(income_df$ciudad))
date_vector <- sort(unique(income_df$fecha))

#######################################################################
## BLOQUE 1: Cálculo de Afford ciudad–mes
#######################################################################

res_afford <- list()
k <- 1

for (city.x in city_vector) {
  for (fecha.x in date_vector) {
    
    message("Procesando Afford: ", city.x, " | ", fecha.x)
    
    #-------------------------
    # Filtrar ingreso
    #-------------------------
    inc_aux <- income_df %>%
      filter(ciudad == city.x,
             fecha  == fecha.x)
    
    if (nrow(inc_aux) == 0) next
    
    #-------------------------
    # Filtrar CoCA y CoNA
    #-------------------------
    coca_aux <- coca_df %>%
      filter(ciudad == city.x,
             fecha  == fecha.x)
    
    cona_aux <- cona_df %>%
      filter(ciudad == city.x,
             fecha  == fecha.x)
    
    if (nrow(coca_aux) == 0 | nrow(cona_aux) == 0) next
    
    #-------------------------
    # Ejecutar Afford
    #-------------------------
    out <- try(
      Afford_expansion(
        Hexpense   = inc_aux,
        Model_CoCA = coca_aux,
        Model_CoNA = cona_aux
      ),
      silent = TRUE
    )
    
    if (inherits(out, "try-error") | is.null(out)) next
    
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

#----------------------------------------------------------------------
# Guardar resultados detallados
#----------------------------------------------------------------------

saveRDS(afford_df,
        file = file.path(afford_metrics_dir, "Afford_city_month.rds"))

write.csv(afford_df,
          file = file.path(afford_metrics_dir, "Afford_city_month.csv"),
          row.names = FALSE)

#######################################################################
## BLOQUE 2: Incidencia promedio por ciudad–mes
#######################################################################

aff_inc <- afford_df %>%
  group_by(ciudad, fecha, model) %>%
  dplyr::summarise(
    n_deciles = n_distinct(deciles),
    sum_rate  = sum(rate, na.rm = TRUE),
    incidence = (1 / n_deciles) * sum_rate,
    .groups   = "drop"
  )

saveRDS(aff_inc,
        file = file.path(afford_metrics_dir,
                         "Afford_incidence_city_month.rds"))

write.csv(aff_inc,
          file = file.path(afford_metrics_dir,
                           "Afford_incidence_city_month.csv"),
          row.names = FALSE)

#######################################################################
## BLOQUE 3: Gráficas
#######################################################################

# CoCA
g_coca_inc <- aff_inc %>%
  filter(model == "CoCA") %>%
  ggplot(aes(x = fecha, y = incidence, color = ciudad)) +
  geom_line(linewidth = 1) +
  scale_x_date(date_labels = "%Y-%m",
               date_breaks = "6 months") +
  labs(title = "Incidencia de pobreza de asequibilidad (CoCA)",
       x = "Fecha",
       y = "Incidencia (%)",
       color = "Ciudad") +
  theme_classic()

ggsave(file.path(afford_metrics_dir,
                 "Afford_Incidence_CoCA_city_time.png"),
       g_coca_inc, width = 10, height = 5)

# CoNA
g_cona_inc <- aff_inc %>%
  filter(model == "CoNA") %>%
  ggplot(aes(x = fecha, y = incidence, color = ciudad)) +
  geom_line(linewidth = 1) +
  scale_x_date(date_labels = "%Y-%m",
               date_breaks = "6 months") +
  labs(title = "Incidencia de pobreza de asequibilidad (CoNA)",
       x = "Fecha",
       y = "Incidencia (%)",
       color = "Ciudad") +
  theme_classic()

ggsave(file.path(afford_metrics_dir,
                 "Afford_Incidence_CoNA_city_time.png"),
       g_cona_inc, width = 10, height = 5)
