#######################################################################
## FIGURA 5 (MENSUAL + TRIMESTRAL) - HEATMAPS
## Incidencia de pobreza de asequibilidad por decil (CoCA / CoNA)
#######################################################################

#----------------------------------------------------------------------
# Paquetes
#----------------------------------------------------------------------
library(tidyverse)
library(lubridate)
library(readr)
library(scales)

#----------------------------------------------------------------------
# Directorios
#----------------------------------------------------------------------
base_dir <- "C:\\Users\\danie\\OneDrive\\Escritorio\\Least-cost-diets-and-affordability\\Proyecto Interno\\"
setwd(base_dir)

out_dir <- file.path(base_dir, "working-papers/working-paper-ipc/output")
afford_metrics_dir <- file.path(out_dir, "affordability_metrics")

cap_p95 <- function(x) {
  p95 <- suppressWarnings(quantile(x, probs = 0.95, na.rm = TRUE, names = FALSE, type = 7))
  pmin(x, p95)
}

#======================================================================
# 1) Cargar base mensual
#======================================================================
afford_m <- readRDS(file.path(afford_metrics_dir, "Afford_city_month.rds")) %>%
  mutate(
    fecha = as.Date(fecha, origin = "1970-01-01"),
    mes = floor_date(fecha, "month"),
    decil_num = readr::parse_number(deciles)
  ) %>%
  # asegurar tipos
  mutate(
    ciudad = as.character(ciudad),
    model = as.character(model),
    decil_num = as.integer(decil_num)
  )

# Completar grilla ciudad-model-decil-mes (para que el heatmap sea “recto”)
mes_levels <- sort(unique(afford_m$mes))
afford_m_full <- afford_m %>%
  select(ciudad, model, mes, decil_num, rate) %>%
  distinct() %>%
  complete(
    ciudad, model, decil_num, mes = mes_levels,
    fill = list(rate = NA_real_)
  ) %>%
  group_by(ciudad, model) %>%
  mutate(rate_cap = cap_p95(rate)) %>%
  ungroup()

#----------------------------------------------------------------------
# MATRIZ MENSUAL (heatmap) - CoCA
#----------------------------------------------------------------------
mat_coca_m <- afford_m_full %>%
  filter(model == "CoCA") %>%
  ggplot(aes(x = mes, y = factor(decil_num), fill = rate_cap)) +
  geom_tile() +
  facet_wrap(~ciudad, nrow = 1) +
  scale_x_date(
    date_breaks = "1 year",
    date_labels = "%Y",
    expand = expansion(mult = c(0, 0))
  ) +
  scale_y_discrete(limits = rev(as.character(1:10))) +
  scale_fill_gradient(
    name = "Incidencia (%)",
    na.value = "grey90"
  ) +
  labs(
    title = "Matriz mensual (heatmap): Incidencia CoCA por decil",
    x = "Año",
    y = "Decil"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    strip.text = element_text(face = "bold")
  )

ggsave(
  file.path(afford_metrics_dir, "CoCA_decil_month_matrix.png"),
  mat_coca_m, width = 14, height = 5
)

#----------------------------------------------------------------------
# MATRIZ MENSUAL (heatmap) - CoNA
#----------------------------------------------------------------------
mat_cona_m <- afford_m_full %>%
  filter(model == "CoNA") %>%
  ggplot(aes(x = mes, y = factor(decil_num), fill = rate_cap)) +
  geom_tile() +
  facet_wrap(~ciudad, nrow = 1) +
  scale_x_date(
    date_breaks = "1 year",
    date_labels = "%Y",
    expand = expansion(mult = c(0, 0))
  ) +
  scale_y_discrete(limits = rev(as.character(1:10))) +
  scale_fill_gradient(
    name = "Incidencia (%)",
    na.value = "grey90"
  ) +
  labs(
    title = "Matriz mensual (heatmap): Incidencia CoNA por decil",
    x = "Año",
    y = "Decil"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    strip.text = element_text(face = "bold")
  )

ggsave(
  file.path(afford_metrics_dir, "CoNA_decil_month_matrix.png"),
  mat_cona_m, width = 14, height = 5
)

#======================================================================
# 2) Cargar base trimestral
#======================================================================
afford_q <- readRDS(file.path(afford_metrics_dir, "Afford_city_cuartiles.rds")) %>%
  mutate(
    decil_num = readr::parse_number(deciles),
    trimestre = as.character(trimestre),
    year = as.integer(str_extract(trimestre, "^\\d{4}")),
    q = as.integer(str_extract(trimestre, "(?<=Q)\\d+")),
    # fecha representativa del trimestre (primer mes del Q)
    tri_date = as.Date(paste0(year, "-", (q - 1) * 3 + 1, "-01"))
  ) %>%
  mutate(
    ciudad = as.character(ciudad),
    model = as.character(model),
    decil_num = as.integer(decil_num)
  )

tri_levels <- sort(unique(afford_q$tri_date))
afford_q_full <- afford_q %>%
  select(ciudad, model, tri_date, decil_num, rate) %>%
  distinct() %>%
  complete(
    ciudad, model, decil_num, tri_date = tri_levels,
    fill = list(rate = NA_real_)
  ) %>%
  group_by(ciudad, model) %>%
  mutate(rate_cap = cap_p95(rate)) %>%
  ungroup()

#----------------------------------------------------------------------
# MATRIZ TRIMESTRAL (heatmap) - CoCA
#----------------------------------------------------------------------
mat_coca_q <- afford_q_full %>%
  filter(model == "CoCA") %>%
  ggplot(aes(x = tri_date, y = factor(decil_num), fill = rate_cap)) +
  geom_tile() +
  facet_wrap(~ciudad, nrow = 1) +
  scale_x_date(
    date_breaks = "1 year",
    date_labels = "%Y",
    expand = expansion(mult = c(0, 0))
  ) +
  scale_y_discrete(limits = rev(as.character(1:10))) +
  scale_fill_gradient(
    name = "Incidencia (%)",
    na.value = "grey90"
  ) +
  labs(
    title = "Matriz trimestral (heatmap): Incidencia CoCA por decil",
    x = "Año",
    y = "Decil"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    strip.text = element_text(face = "bold")
  )

ggsave(
  file.path(afford_metrics_dir, "CoCA_decil_quarter_matrix.png"),
  mat_coca_q, width = 14, height = 5
)

#----------------------------------------------------------------------
# MATRIZ TRIMESTRAL (heatmap) - CoNA
#----------------------------------------------------------------------
mat_cona_q <- afford_q_full %>%
  filter(model == "CoNA") %>%
  ggplot(aes(x = tri_date, y = factor(decil_num), fill = rate_cap)) +
  geom_tile() +
  facet_wrap(~ciudad, nrow = 1) +
  scale_x_date(
    date_breaks = "1 year",
    date_labels = "%Y",
    expand = expansion(mult = c(0, 0))
  ) +
  scale_y_discrete(limits = rev(as.character(1:10))) +
  scale_fill_gradient(
    name = "Incidencia (%)",
    na.value = "grey90"
  ) +
  labs(
    title = "Matriz trimestral (heatmap): Incidencia CoNA por decil",
    x = "Año",
    y = "Decil"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    strip.text = element_text(face = "bold")
  )

ggsave(
  file.path(afford_metrics_dir, "CoNA_decil_quarter_matrix.png"),
  mat_cona_q, width = 14, height = 5
)

#######################################################################
## DONE
#######################################################################

