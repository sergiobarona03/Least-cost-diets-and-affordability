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
library(scales)
library(ggsci)

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
#  Cargar CoCA, CoNA y CoRD  <<< NUEVO
#----------------------------------------------------------------------
coca_df <- read.csv(file.path(afford_cost_dir, "CoCA_city_month.csv"))
cona_df <- read.csv(file.path(afford_cost_dir, "CoNA_city_month.csv"))

# CoRD puede existir o no: lo cargamos "suave" para no romper nada
cord_path_csv <- file.path(afford_cost_dir, "CoRD_city_month.csv")
cord_df <- NULL
if (file.exists(cord_path_csv)) {
  cord_df <- read.csv(cord_path_csv)
}

coca_df$fecha <- as.Date(coca_df$fecha)
cona_df$fecha <- as.Date(cona_df$fecha)
if (!is.null(cord_df)) cord_df$fecha <- as.Date(cord_df$fecha)

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
    # Filtrar CoCA, CoNA y CoRD
    #-------------------------
    coca_aux <- coca_df %>%
      filter(ciudad == city.x,
             fecha  == fecha.x)
    
    cona_aux <- cona_df %>%
      filter(ciudad == city.x,
             fecha  == fecha.x)
    
    cord_aux <- cord_df %>%
      filter(ciudad == city.x,
             fecha  == fecha.x)
    
    if (nrow(coca_aux) == 0 | nrow(cona_aux) == 0 | nrow(cord_aux) == 0) next
    
    #-------------------------
    # Ejecutar Afford
    #-------------------------
    out <- try(
      Afford_expansion(
        Hexpense   = inc_aux,
        Model_CoCA = coca_aux,
        Model_CoNA = cona_aux,
        Model_CoRD = cord_aux   
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
# Orden de ciudades
city_levels <- c("BOGOTA", "CALI", "MEDELLIN")

aff_inc <- aff_inc %>%
  mutate(
    ciudad = as.character(ciudad),
    ciudad = case_when(
      ciudad %in% c("BOGOTÁ D.C.", "BOGOTA D.C.", "BOGOTA") ~ "BOGOTA",
      ciudad %in% c("MEDELLÍN", "MEDELLIN")                 ~ "MEDELLIN",
      ciudad %in% c("CALI")                                 ~ "CALI",
      TRUE ~ ciudad
    ),
    ciudad = factor(ciudad, levels = city_levels),
    fecha  = as.Date(fecha)
  )

# Tema base
theme_paper <- theme_classic(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    axis.title = element_text(size = 11),
    axis.text  = element_text(size = 10, color = "black"),
    legend.title = element_text(size = 10),
    legend.text  = element_text(size = 10),
    legend.position = "top",
    legend.justification = "left",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(linewidth = 0.4),
    axis.ticks = element_line(linewidth = 0.4),
    plot.margin = margin(6, 6, 6, 6)
  )

# Escalas X
x_scale_6m <- scale_x_date(
  date_breaks = "6 months",
  date_labels = "%Y-%m",
  expand = expansion(mult = c(0.01, 0.01))
)

# Y como %
y_scale_pct <- scale_y_continuous(
  labels = label_percent(scale = 1, accuracy = 0.1),
  expand = expansion(mult = c(0.02, 0.02))
)

# Paleta
color_scale <- scale_color_nejm(name = "City")

#----------------------------------------------------------------------
# CoCA
#----------------------------------------------------------------------
g_coca_inc <- aff_inc %>%
  filter(model == "CoCA", !is.na(ciudad), !is.na(fecha), !is.na(incidence)) %>%
  arrange(ciudad, fecha) %>%
  ggplot(aes(x = fecha, y = incidence, color = ciudad, group = ciudad)) +
  geom_line(linewidth = 0.9) +
  x_scale_6m +
  y_scale_pct +
  labs(
    title = "Affordability poverty incidence (CoCA)",
    x = NULL,
    y = "Incidence"
  ) +
  color_scale +
  theme_paper +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(
  filename = file.path(afford_metrics_dir, "Afford_Incidence_CoCA_city_time.png"),
  plot = g_coca_inc,
  width = 10, height = 5, dpi = 300, bg = "white"
)

#----------------------------------------------------------------------
# CoNA
#----------------------------------------------------------------------
g_cona_inc <- aff_inc %>%
  filter(model == "CoNA", !is.na(ciudad), !is.na(fecha), !is.na(incidence)) %>%
  arrange(ciudad, fecha) %>%
  ggplot(aes(x = fecha, y = incidence, color = ciudad, group = ciudad)) +
  geom_line(linewidth = 0.9) +
  x_scale_6m +
  y_scale_pct +
  labs(
    title = "Affordability poverty incidence (CoNA)",
    x = NULL,
    y = "Incidence"
  ) +
  color_scale +
  theme_paper +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(
  filename = file.path(afford_metrics_dir, "Afford_Incidence_CoNA_city_time.png"),
  plot = g_cona_inc,
  width = 10, height = 5, dpi = 300, bg = "white"
)

#----------------------------------------------------------------------
# CoRD
#----------------------------------------------------------------------
g_cord_inc <- aff_inc %>%
  filter(model == "CoRD", !is.na(ciudad), !is.na(fecha), !is.na(incidence)) %>%
  arrange(ciudad, fecha) %>%
  ggplot(aes(x = fecha, y = incidence, color = ciudad, group = ciudad)) +
  geom_line(linewidth = 0.9) +
  x_scale_6m +
  y_scale_pct +
  labs(
    title = "Affordability poverty incidence (CoRD)",
    x = NULL,
    y = "Incidence"
  ) +
  color_scale +
  theme_paper +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(
  filename = file.path(afford_metrics_dir, "Afford_Incidence_CoRD_city_time.png"),
  plot = g_cord_inc,
  width = 10, height = 5, dpi = 300, bg = "white"
)