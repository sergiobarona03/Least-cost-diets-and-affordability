#######################################################################
## Afford trimestral: costos promediados y hogares agrupados por trimestre
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
base_dir <- "C:\\Users\\danie\\OneDrive\\Escritorio\\Least-cost-diets-and-affordability\\Proyecto Interno\\"
setwd(base_dir)

# Función Afford (ajusta ruta si cambia)
source("working-papers\\working-paper-ipc\\affordability-indicators\\aux_functions\\Afford_Expansion.R")

# Directorio general de output
out_dir <- file.path(base_dir, "working-papers/working-paper-ipc/output")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# Directorios ya existentes de costo e ingreso (MENSUALES)
income_dir      <- file.path(out_dir, "incomecol")
afford_cost_dir <- file.path(out_dir, "affordability")

# NUEVO directorio para métricas trimestrales
afford_metrics_q_dir <- file.path(out_dir, "affordability_metrics_quarterly")
dir.create(afford_metrics_q_dir, recursive = TRUE, showWarnings = FALSE)

#----------------------------------------------------------------------
# Cargar resultados previos MENSUALES de CoCA / CoNA e ingreso
#----------------------------------------------------------------------
coca_df   <- read.csv(file.path(afford_cost_dir, "CoCA_city_month.csv"))
cona_df   <- read.csv(file.path(afford_cost_dir, "CoNA_city_month.csv"))
income_df <- read.csv(file.path(income_dir, "IncomeCol_city_month.csv"))

#----------------------------------------------------------------------
# Asegurar que las fechas sean Date
#----------------------------------------------------------------------
suppressWarnings({
  coca_df$fecha   <- as.Date(coca_df$fecha)
  cona_df$fecha   <- as.Date(cona_df$fecha)
  income_df$fecha <- as.Date(income_df$fecha)
})

#----------------------------------------------------------------------
# Crear variables de año y trimestre
#----------------------------------------------------------------------
coca_df <- coca_df %>%
  mutate(
    year    = year(fecha),
    quarter = quarter(fecha)
  )

cona_df <- cona_df %>%
  mutate(
    year    = year(fecha),
    quarter = quarter(fecha)
  )

income_df <- income_df %>%
  mutate(
    year    = year(fecha),
    quarter = quarter(fecha)
  )

#----------------------------------------------------------------------
# Construir CoCA y CoNA TRIMESTRALES: promediar costos dentro del trimestre
#----------------------------------------------------------------------
# Estructura original típica de CoCA/CoNA:
# Demo_Group, Sex, Person, cost_day, total_household,
# per_capita, per_capita_year, per_capita_month, ciudad, fecha

coca_q <- coca_df %>%
  group_by(ciudad, year, quarter, Demo_Group, Sex, Person) %>%
  dplyr::summarise(
    cost_day         = mean(cost_day, na.rm = TRUE),
    total_household  = mean(total_household, na.rm = TRUE),
    per_capita       = mean(per_capita, na.rm = TRUE),
    per_capita_year  = mean(per_capita_year, na.rm = TRUE),
    per_capita_month = mean(per_capita_month, na.rm = TRUE),
    .groups = "drop"
  )

cona_q <- cona_df %>%
  group_by(ciudad, year, quarter, Demo_Group, Sex, Person) %>%
  dplyr::summarise(
    cost_day         = mean(cost_day, na.rm = TRUE),
    total_household  = mean(total_household, na.rm = TRUE),
    per_capita       = mean(per_capita, na.rm = TRUE),
    per_capita_year  = mean(per_capita_year, na.rm = TRUE),
    per_capita_month = mean(per_capita_month, na.rm = TRUE),
    .groups = "drop"
  )

#----------------------------------------------------------------------
# Construir ingreso TRIMESTRAL: agrupar (stackear) hogares dentro del trimestre
#----------------------------------------------------------------------
income_q <- income_df %>%
  # Solo aseguramos que year y quarter no sean NA
  filter(!is.na(year), !is.na(quarter))

#----------------------------------------------------------------------
# Vectores ciudad–año–trimestre
#----------------------------------------------------------------------
city_vector  <- sort(unique(income_q$ciudad_panel))
year_vector  <- sort(unique(income_q$year))
quarter_vec  <- sort(unique(income_q$quarter))

#######################################################################
## BLOQUE 1: Cálculo de Afford trimestral ciudad–año–trimestre
#######################################################################

res_afford_q <- list()
k <- 1

for (city.x in city_vector) {
  for (yy in year_vector) {
    for (qq in quarter_vec) {
      
      # Filtramos Income para la ciudad–año–trimestre
      inc_aux <- income_q %>%
        filter(
          ciudad_panel == city.x,
          year == yy,
          quarter == qq
        )
      
      if (nrow(inc_aux) == 0) {
        next
      }
      
      # CoCA trimestral correspondiente
      coca_aux <- coca_q %>%
        filter(
          ciudad == city.x,
          year == yy,
          quarter == qq
        )
      
      # CoNA trimestral correspondiente
      cona_aux <- cona_q %>%
        filter(
          ciudad == city.x,
          year == yy,
          quarter == qq
        )
      
      if (nrow(coca_aux) == 0 || nrow(cona_aux) == 0) {
        next
      }
      
      message("Procesando Afford TRIMESTRAL: ciudad = ", city.x,
              " | año = ", yy,
              " | trimestre = ", qq, " ...")
      
      out_q <- try(
        Afford(
          Hexpense   = inc_aux,
          Model_CoCA = coca_aux,
          Model_CoNA = cona_aux
        ),
        silent = TRUE
      )
      
      if (inherits(out_q, "try-error") || is.null(out_q)) {
        message("   -> error en Afford() trimestral")
        next
      }
      
      po_q <- out_q$Poverty_outcome %>%
        mutate(
          ciudad  = city.x,
          year    = yy,
          quarter = qq
        )
      
      res_afford_q[[k]] <- po_q
      k <- k + 1
    }
  }
}

afford_q_df <- bind_rows(res_afford_q)

# Guardar tabla trimestral por decil
saveRDS(afford_q_df, file = file.path(afford_metrics_q_dir, "Afford_quarter_city.rds"))
write.csv(afford_q_df,
          file = file.path(afford_metrics_q_dir, "Afford_quarter_city.csv"),
          row.names = FALSE)

#######################################################################
## BLOQUE 2: Construcción de la INCIDENCIA TRIMESTRAL
#######################################################################

# afford_q_df tiene columnas:
# deciles, rate, gap, severity, model, ciudad, year, quarter

aff_inc_q <- afford_q_df %>%
  group_by(ciudad, year, quarter, model) %>%
  dplyr::summarise(
    n_deciles = n_distinct(deciles),
    sum_rate  = sum(rate, na.rm = TRUE),
    incidence = (1 / n_deciles) * sum_rate,  # porcentaje total trimestral
    .groups   = "drop"
  )

# Definir una fecha representativa del trimestre (primer día del trimestre)
aff_inc_q <- aff_inc_q %>%
  mutate(
    quarter_month = (quarter - 1) * 3 + 1,
    quarter_date  = as.Date(paste0(year, "-", quarter_month, "-01"))
  )

# Guardar tabla de incidencia trimestral
saveRDS(aff_inc_q, file = file.path(afford_metrics_q_dir, "Afford_incidence_quarter_city.rds"))
write.csv(aff_inc_q,
          file = file.path(afford_metrics_q_dir, "Afford_incidence_quarter_city.csv"),
          row.names = FALSE)

#######################################################################
## BLOQUE 3: Gráficas trimestrales de incidencia CoCA y CoNA
#######################################################################

# Estandarización ciudades 
city_levels <- c("BOGOTA", "CALI", "MEDELLIN")

aff_inc_q <- aff_inc_q %>%
  mutate(
    ciudad = as.character(ciudad),
    ciudad = case_when(
      ciudad %in% c("BOGOTÁ D.C.", "BOGOTA D.C.", "BOGOTA") ~ "BOGOTA",
      ciudad %in% c("MEDELLÍN", "MEDELLIN")                 ~ "MEDELLIN",
      ciudad %in% c("CALI")                                 ~ "CALI",
      TRUE ~ ciudad
    ),
    ciudad = factor(ciudad, levels = city_levels),
    quarter_date = as.Date(quarter_date)
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

# Escala
x_scale_q <- scale_x_date(
  date_breaks = "1 year",
  date_labels = "%Y",
  expand = expansion(mult = c(0.01, 0.01))
)

y_scale_pct <- scale_y_continuous(
  labels = label_percent(scale = 1, accuracy = 0.1),
  expand = expansion(mult = c(0.02, 0.02))
)

#------------------------------------------------------------
# 3) Paleta (MISMA que tu bloque mensual)
#------------------------------------------------------------
color_scale <- scale_color_nejm(name = "Ciudad")  # alternativa: scale_color_aaas()

#------------------------------------------------------------
# CoCA trimestral
#------------------------------------------------------------
g_coca_inc_q <- aff_inc_q %>%
  filter(model == "CoCA", !is.na(ciudad), !is.na(quarter_date), !is.na(incidence)) %>%
  arrange(ciudad, quarter_date) %>%
  ggplot(aes(x = quarter_date, y = incidence, color = ciudad, group = ciudad)) +
  geom_line(linewidth = 0.9) +
  x_scale_q +
  y_scale_pct +
  labs(
    title = "Incidencia trimestral de pobreza de asequibilidad (CoCA)",
    x = NULL,
    y = "Incidencia"
  ) +
  color_scale +
  theme_paper +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(
  filename = file.path(afford_metrics_q_dir, "Afford_Incidence_CoCA_quarter_city.png"),
  plot     = g_coca_inc_q,
  width    = 10,
  height   = 5,
  dpi      = 300,
  bg       = "white"
)

#------------------------------------------------------------
# CoNA trimestral
#------------------------------------------------------------
g_cona_inc_q <- aff_inc_q %>%
  filter(model == "CoNA", !is.na(ciudad), !is.na(quarter_date), !is.na(incidence)) %>%
  arrange(ciudad, quarter_date) %>%
  ggplot(aes(x = quarter_date, y = incidence, color = ciudad, group = ciudad)) +
  geom_line(linewidth = 0.9) +
  x_scale_q +
  y_scale_pct +
  labs(
    title = "Incidencia trimestral de pobreza de asequibilidad (CoNA)",
    x = NULL,
    y = "Incidencia"
  ) +
  color_scale +
  theme_paper +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(
  filename = file.path(afford_metrics_q_dir, "Afford_Incidence_CoNA_quarter_city.png"),
  plot     = g_cona_inc_q,
  width    = 10,
  height   = 5,
  dpi      = 300,
  bg       = "white"
)
