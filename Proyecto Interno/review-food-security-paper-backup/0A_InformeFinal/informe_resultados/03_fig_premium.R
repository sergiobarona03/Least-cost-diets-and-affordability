########################################################
## informe_resultados/fig_primas_calidad.R
##
## Figura: primas de costo por calidad dietaria
##   CoNA / CoCA | CoRD / CoCA | CoRD / CoNA
##
## Geometría: boxplot por año (distribución de los 12
##   meses de ese año), color por ciudad
##   facet_wrap(~ premium, nrow = 1)
##
## Insumos:
##   review-food-security-paper/02_models/hcost/hcost_full.rds
##   review-food-security-paper/01_data_preparation/deflator_monthly.rds
##
## Output:
##   informe_resultados/output/figuras/fig_primas_calidad.png
##   informe_resultados/output/tablas/tab_primas_calidad.xlsx
########################################################

library(tidyverse)
library(scales)
library(lubridate)

# -------------------------------------------------------
# PATHS
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
CITY_LABELS <- c(
  "BOGOTÁ D.C." = "Bogotá",
  "BOGOTA"      = "Bogotá",
  "MEDELLÍN"    = "Medellín",
  "MEDELLIN"    = "Medellín",
  "CALI"        = "Cali"
)

PREM_LEVELS <- c("CoNA / CoCA", "CoRD / CoCA", "CoRD / CoNA")

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
# 2. CONSTRUIR PRIMAS
# -------------------------------------------------------
message("Construyendo primas de calidad dietaria...")

premium_long <- hcost %>%
  group_by(model, ciudad, fecha) %>%
  summarise(cost_pc = mean(per_capita, na.rm = TRUE), .groups = "drop") %>%
  left_join(deflator, by = c("ciudad", "fecha")) %>%
  mutate(cost_real = cost_pc * deflator) %>%
  filter(fecha >= PAPER_START, fecha <= PAPER_END, !is.na(cost_real)) %>%
  select(ciudad, fecha, model, cost_real) %>%
  pivot_wider(names_from = model, values_from = cost_real) %>%
  mutate(
    `CoNA / CoCA` = CoNA / CoCA,
    `CoRD / CoCA` = CoRD / CoCA,
    `CoRD / CoNA` = CoRD / CoNA,
    ciudad_lbl    = factor(CITY_LABELS[ciudad], levels = names(CITY_COLORS)),
    ano           = factor(year(fecha))          # año como factor para el eje X
  ) %>%
  filter(!is.na(ciudad_lbl)) %>%
  pivot_longer(
    cols      = all_of(PREM_LEVELS),
    names_to  = "premium",
    values_to = "ratio"
  ) %>%
  mutate(premium = factor(premium, levels = PREM_LEVELS))

message(sprintf("  %d obs en premium_long", nrow(premium_long)))

# -------------------------------------------------------
# 3. FIGURA: boxplot por año, color por ciudad
# -------------------------------------------------------
message("Generando figura...")

p <- ggplot(premium_long,
            aes(x = ano, y = ratio, fill = ciudad_lbl, color = ciudad_lbl)) +
  geom_hline(yintercept = 1, color = "grey50",
             linetype = "dotted", linewidth = 0.4) +
  # Líneas verticales punteadas que separan los años
  geom_vline(
    xintercept = seq(1.5, length(levels(premium_long$ano)) - 0.5, by = 1),
    color      = "grey70",
    linetype   = "dashed",
    linewidth  = 0.35
  ) +
  geom_boxplot(
    alpha        = 0.35,
    outlier.shape = NA,          # sin puntos outlier para mayor limpieza
    position     = position_dodge(width = 0.75),
    linewidth    = 0.5,
    width        = 0.65
  ) +
  facet_wrap(~ premium, nrow = 1, scales = "free_y") +
  scale_fill_manual(values  = CITY_COLORS, name = NULL) +
  scale_color_manual(values = CITY_COLORS, name = NULL) +
  scale_y_continuous(labels = function(x) sprintf("%.2f×", x)) +
  labs(
    x = NULL,
    y = "Razón de costos (términos reales)"
  ) +
  theme_bw(base_size = 11) +
  theme(
    legend.position       = "bottom",
    legend.direction      = "horizontal",
    legend.text           = element_text(size = 10),
    legend.key.width      = unit(1.2, "cm"),
    legend.background     = element_rect(color = "black", fill = "white",
                                         linewidth = 0.5),
    legend.margin         = margin(3, 8, 3, 8),
    strip.background      = element_rect(fill = "grey92"),
    strip.text            = element_text(face = "bold", size = 11),
    panel.grid.minor      = element_blank(),
    panel.grid.major      = element_blank(),
    panel.spacing         = unit(0.6, "cm"),
    axis.text.x           = element_text(size = 9),
    axis.text.y           = element_text(size = 8.5),
    axis.title.y          = element_text(size = 10)
  )

out_path <- file.path(out_figuras, "fig_primas_calidad.png")
ggsave(out_path, p, width = 13, height = 6, dpi = 300, bg = "white")
message("  Figura guardada: ", out_path)

# -------------------------------------------------------
# 4. TABLA RESUMEN: mediana y rango intercuartílico por
#    prima, ciudad y año
# -------------------------------------------------------
tab_resumen <- premium_long %>%
  group_by(premium, ciudad_lbl, ano) %>%
  summarise(
    mediana = median(ratio, na.rm = TRUE),
    q25     = quantile(ratio, 0.25, na.rm = TRUE),
    q75     = quantile(ratio, 0.75, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(premium, ciudad_lbl, ano)

writexl::write_xlsx(
  list(primas_calidad = tab_resumen),
  file.path(out_tablas, "tab_primas_calidad.xlsx")
)

message("  Tabla guardada: tab_primas_calidad.xlsx")
message("\nListo. Outputs en: ", out_dir)