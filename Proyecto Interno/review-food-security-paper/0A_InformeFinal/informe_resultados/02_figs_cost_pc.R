########################################################
## informe_resultados/fig_costos_percapita.R
##
## Cuatro figuras de costos per cápita:
##   Fig A: Costos nominales (COP/día)
##   Fig B: Costos reales (COP/día, base dic 2018)
##   Fig C: Variación interanual nominal (%)
##   Fig D: Variación interanual real (%)
##
## Cada figura: facet_wrap(~ model, nrow = 1)
##              una línea por ciudad
##              período: 2019-01 a 2024-12
##
## Insumos:
##   review-food-security-paper/02_models/hcost/hcost_full.rds
##   review-food-security-paper/01_data_preparation/deflator_monthly.rds
##
## Output:
##   informe_resultados/output/figuras/fig_costos_nominales.png
##   informe_resultados/output/figuras/fig_costos_reales.png
##   informe_resultados/output/figuras/fig_variacion_nominal.png
##   informe_resultados/output/figuras/fig_variacion_real.png
########################################################

library(tidyverse)
library(scales)
library(lubridate)

# -------------------------------------------------------
# PATHS (autocontenido — sin source("00_config.R"))
# -------------------------------------------------------
BASE_DIRS <- c(
  "C:/Users/sergio.barona/Desktop/Least-cost-diets-and-affordability/Proyecto Interno",
  "C:/Users/Portatil/Desktop/Least-cost-diets-and-affordability/Proyecto Interno",
  "C:/Users/danie/OneDrive/Escritorio/Least-cost-diets-and-affordability/Proyecto Interno"
)
base_dir <- BASE_DIRS[dir.exists(BASE_DIRS)][1]
if (is.na(base_dir)) stop("No se encontró el directorio base.")

hcost_path    <- file.path(base_dir, "review-food-security-paper/02_models/hcost/hcost_full.rds")
deflator_path <- file.path(base_dir, "review-food-security-paper/01_data_preparation/deflator_monthly.rds")

out_dir     <- file.path(base_dir, "review-food-security-paper/0A_InformeFinal/informe_resultados/output")
out_figuras <- file.path(out_dir, "figuras")
out_tablas  <- file.path(out_dir, "tablas")

dir.create(out_figuras, showWarnings = FALSE, recursive = TRUE)
dir.create(out_tablas,  showWarnings = FALSE, recursive = TRUE)

# -------------------------------------------------------
# PARÁMETROS
# -------------------------------------------------------
PAPER_START <- as.Date("2019-01-01")
PAPER_END   <- as.Date("2024-12-01")

CITY_COLORS <- c(
  "Bogotá"   = "#2E5FA3",
  "Medellín" = "#C0392B",
  "Cali"     = "#1A7A4A"
)
CITY_LINES <- c(
  "Bogotá"   = "solid",
  "Medellín" = "dashed",
  "Cali"     = "dotdash"
)
CITY_LABELS <- c(
  "BOGOTÁ D.C." = "Bogotá",
  "BOGOTA"      = "Bogotá",
  "MEDELLÍN"    = "Medellín",
  "MEDELLIN"    = "Medellín",
  "CALI"        = "Cali"
)

MODEL_LEVELS <- c("CoCA", "CoNA", "CoRD")

# -------------------------------------------------------
# TEMA BASE
# -------------------------------------------------------
tema_base <- theme_bw(base_size = 11) +
  theme(
    legend.position       = "bottom",
    legend.direction      = "horizontal",
    legend.text           = element_text(size = 10),
    legend.key.width      = unit(1.4, "cm"),
    legend.background     = element_rect(color = "black", fill = "white",
                                         linewidth = 0.5),
    legend.margin         = margin(3, 8, 3, 8),
    strip.background      = element_rect(fill = "grey92"),
    strip.text            = element_text(face = "bold", size = 11),
    panel.grid.minor      = element_blank(),
    panel.spacing         = unit(0.5, "cm"),
    axis.text.x           = element_text(angle = 45, hjust = 1, size = 8),
    axis.text.y           = element_text(size = 8),
    axis.title.y          = element_text(size = 10),
    plot.title            = element_text(size = 12, face = "bold"),
    plot.subtitle         = element_text(size = 9, color = "grey40")
  )

# -------------------------------------------------------
# 1. CARGAR DATOS
# -------------------------------------------------------
message("Cargando datos...")

hcost <- readRDS(hcost_path) %>%
  mutate(fecha = as.Date(fecha))

deflator <- readRDS(deflator_path) %>%
  filter(ciudad != "NACIONAL") %>%
  select(ciudad, fecha, deflator)

# -------------------------------------------------------
# 2. CONSTRUIR SERIES DE COSTOS
# -------------------------------------------------------
message("Construyendo series de costos per cápita...")

cost_pc <- hcost %>%
  group_by(model, ciudad, fecha) %>%
  summarise(cost_nominal = mean(per_capita, na.rm = TRUE), .groups = "drop") %>%
  left_join(deflator, by = c("ciudad", "fecha")) %>%
  mutate(
    cost_real   = cost_nominal * deflator,
    ciudad_lbl  = factor(CITY_LABELS[ciudad], levels = names(CITY_COLORS)),
    model       = factor(model, levels = MODEL_LEVELS)
  ) %>%
  filter(
    fecha >= PAPER_START, fecha <= PAPER_END,
    !is.na(cost_real), !is.na(ciudad_lbl), !is.na(model)
  )

# Variaciones interanuales
cost_yoy <- cost_pc %>%
  arrange(model, ciudad_lbl, fecha) %>%
  group_by(model, ciudad_lbl) %>%
  mutate(
    yoy_nominal = (cost_nominal / lag(cost_nominal, 12) - 1) * 100,
    yoy_real    = (cost_real    / lag(cost_real,    12) - 1) * 100
  ) %>%
  ungroup() %>%
  filter(!is.na(yoy_nominal))

message(sprintf("  %d obs en cost_pc | %d obs en cost_yoy",
                nrow(cost_pc), nrow(cost_yoy)))

# -------------------------------------------------------
# ESCALAS COMPARTIDAS
# -------------------------------------------------------
color_scale <- scale_color_manual(
  values = CITY_COLORS, name = NULL)

linetype_scale <- scale_linetype_manual(
  values = CITY_LINES, name = NULL)

date_scale <- scale_x_date(
  date_breaks = "1 year", date_labels = "%Y")

# -------------------------------------------------------
# 3. FIGURA A: COSTOS NOMINALES
# -------------------------------------------------------
message("Generando Fig A: costos nominales...")

p_a <- ggplot(cost_pc,
              aes(x = fecha, y = cost_nominal,
                  color = ciudad_lbl, linetype = ciudad_lbl)) +
  geom_line(linewidth = 0.85) +
  facet_wrap(~ model, nrow = 1, scales = "free_y") +
  color_scale + linetype_scale + date_scale +
  scale_y_continuous(labels = comma_format(big.mark = ",")) +
  labs(x = NULL, y = "COP / día (per cápita)") +
  tema_base

ggsave(file.path(out_figuras, "fig_costos_nominales.png"),
       p_a, width = 12, height = 5, dpi = 300, bg = "white")
message("  Fig A guardada.")

# -------------------------------------------------------
# 4. FIGURA B: COSTOS REALES
# -------------------------------------------------------
message("Generando Fig B: costos reales...")

p_b <- ggplot(cost_pc,
              aes(x = fecha, y = cost_real,
                  color = ciudad_lbl, linetype = ciudad_lbl)) +
  geom_line(linewidth = 0.85) +
  facet_wrap(~ model, nrow = 1, scales = "free_y") +
  color_scale + linetype_scale + date_scale +
  scale_y_continuous(labels = comma_format(big.mark = ",")) +
  labs(x = NULL, y = "COP / día (per cápita, base: dic 2018)") +
  tema_base

ggsave(file.path(out_figuras, "fig_costos_reales.png"),
       p_b, width = 12, height = 5, dpi = 300, bg = "white")
message("  Fig B guardada.")

# -------------------------------------------------------
# 5. FIGURA C: VARIACIÓN INTERANUAL NOMINAL
# -------------------------------------------------------
message("Generando Fig C: variación nominal...")

p_c <- ggplot(cost_yoy,
              aes(x = fecha, y = yoy_nominal,
                  color = ciudad_lbl, linetype = ciudad_lbl)) +
  geom_hline(yintercept = 0, color = "grey50",
             linetype = "dashed", linewidth = 0.4) +
  geom_line(linewidth = 0.85) +
  facet_wrap(~ model, nrow = 1, scales = "free_y") +
  color_scale + linetype_scale + date_scale +
  scale_y_continuous(labels = function(x) sprintf("%+.0f%%", x)) +
  labs(x = NULL, y = "Variación interanual (%)") +
  tema_base

ggsave(file.path(out_figuras, "fig_variacion_nominal.png"),
       p_c, width = 12, height = 5, dpi = 300, bg = "white")
message("  Fig C guardada.")

# -------------------------------------------------------
# 6. FIGURA D: VARIACIÓN INTERANUAL REAL
# -------------------------------------------------------
message("Generando Fig D: variación real...")

p_d <- ggplot(cost_yoy,
              aes(x = fecha, y = yoy_real,
                  color = ciudad_lbl, linetype = ciudad_lbl)) +
  geom_hline(yintercept = 0, color = "grey50",
             linetype = "dashed", linewidth = 0.4) +
  geom_line(linewidth = 0.85) +
  facet_wrap(~ model, nrow = 1, scales = "free_y") +
  color_scale + linetype_scale + date_scale +
  scale_y_continuous(labels = function(x) sprintf("%+.0f%%", x)) +
  labs(x = NULL, y = "Variación interanual real (%, base: dic 2018)") +
  tema_base

ggsave(file.path(out_figuras, "fig_variacion_real.png"),
       p_d, width = 12, height = 5, dpi = 300, bg = "white")
message("  Fig D guardada.")

# -------------------------------------------------------
# 7. TABLA RESUMEN
# -------------------------------------------------------
tab_resumen <- cost_pc %>%
  mutate(ano = year(fecha)) %>%
  group_by(model, ciudad_lbl, ano) %>%
  summarise(
    cost_nominal_mean = mean(cost_nominal, na.rm = TRUE),
    cost_real_mean    = mean(cost_real,    na.rm = TRUE),
    .groups = "drop"
  )

tab_yoy <- cost_yoy %>%
  mutate(ano = year(fecha)) %>%
  group_by(model, ciudad_lbl, ano) %>%
  summarise(
    yoy_nominal_mean = mean(yoy_nominal, na.rm = TRUE),
    yoy_real_mean    = mean(yoy_real,    na.rm = TRUE),
    .groups = "drop"
  )

writexl::write_xlsx(
  list(
    costos    = tab_resumen,
    variacion = tab_yoy
  ),
  file.path(out_tablas, "tab_costos_percapita.xlsx")
)

message("  Tabla guardada: tab_costos_percapita.xlsx")
message("\nListo. Outputs en: ", out_dir)