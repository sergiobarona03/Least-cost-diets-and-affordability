########################################################
## informe_resultados/fig_income_ridgeplot.R
##
## Ridge plot de la distribución del ingreso per cápita
##   del hogar, por ciudad, con la evolución TRIMESTRAL
##   (2019-T1 a 2024-T4, 24 pliegues) como los pliegues
##   del ridge
##
## Variable: log(ingreso per cápita), ponderado por el
##   factor de expansión de la encuesta (fex_c)
##
## Insumo: 03_income/deciles_final.rds
##
## Output:
##   informe_resultados/output/figuras/fig_income_ridgeplot.png
##   informe_resultados/output/tablas/tab_income_distribucion.xlsx
########################################################

library(tidyverse)
library(ggridges)
library(scales)

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

income_path <- file.path(base_dir, "review-food-security-paper/03_income/deciles_final.rds")

out_dir     <- file.path(base_dir, "review-food-security-paper/0A_InformeFinal/informe_resultados/output")
out_figuras <- file.path(out_dir, "figuras")
out_tablas  <- file.path(out_dir, "tablas")
dir.create(out_figuras, showWarnings = FALSE, recursive = TRUE)
dir.create(out_tablas,  showWarnings = FALSE, recursive = TRUE)

# -------------------------------------------------------
# PARÁMETROS
# -------------------------------------------------------
CITY_LABELS <- c(
  "BOGOTA"   = "Bogotá",
  "MEDELLIN" = "Medellín",
  "CALI"     = "Cali"
)
CITY_ORDER <- c("Bogotá", "Medellín", "Cali")

# Paleta de años: un degradado por ciudad, consistente con el resto del informe
CITY_COLORS <- c("Bogotá" = "#2E5FA3", "Medellín" = "#C0392B", "Cali" = "#1A7A4A")

# -------------------------------------------------------
# 1. CARGAR DATOS
# -------------------------------------------------------
message("Cargando deciles_final.rds...")

deciles_final <- readRDS(income_path)

income_df <- deciles_final %>%
  mutate(
    ciudad_lbl = case_when(
      dominio == "MEDELLIN" ~ "Medellín",
      dominio == "BOGOTA"   ~ "Bogotá",
      dominio == "CALI"     ~ "Cali",
      TRUE ~ CITY_LABELS[dominio]
    ),
    ciudad_lbl = factor(ciudad_lbl, levels = CITY_ORDER),
    ano        = as.integer(year),
    mes_num    = as.integer(mes),
    trimestre  = ceiling(mes_num / 3),
    trim_lbl   = sprintf("%04d-T%d", ano, trimestre)
  ) %>%
  filter(
    !is.na(ciudad_lbl), !is.na(per_capita_income), per_capita_income > 0,
    !is.na(fex_c), fex_c > 0,
    ano >= 2019, ano <= 2024
  ) %>%
  mutate(log_income = log(per_capita_income))

message(sprintf("  %d observaciones | %d ciudades | %d trimestres",
                nrow(income_df), n_distinct(income_df$ciudad_lbl),
                n_distinct(income_df$trim_lbl)))

# Orden cronológico de los trimestres para el eje Y del ridge
orden_trim <- income_df %>%
  distinct(ano, trimestre, trim_lbl) %>%
  arrange(ano, trimestre) %>%
  pull(trim_lbl)

income_df <- income_df %>%
  mutate(trim_lbl = factor(trim_lbl, levels = orden_trim))

# -------------------------------------------------------
# 2. RIDGE PLOT
#    facet por ciudad | pliegues = trimestre (24) | x = log(ingreso pc)
#    Densidades ponderadas por el factor de expansión fex_c
# -------------------------------------------------------
message("Generando ridge plot...")

n_trim <- length(orden_trim)
alto_fig <- max(9, n_trim * 0.35)   # escala el alto con el número de pliegues

p <- ggplot(income_df,
            aes(x = log_income, y = trim_lbl,
                fill = ciudad_lbl, weight = fex_c)) +
  ggridges::geom_density_ridges(
    alpha = 0.75, scale = 1.8, color = "white", linewidth = 0.25,
    rel_min_height = 0.001
  ) +
  facet_wrap(~ ciudad_lbl, ncol = 3) +
  scale_fill_manual(values = CITY_COLORS, guide = "none") +
  scale_x_continuous(labels = function(x) sprintf("%.1f", x)) +
  scale_y_discrete(limits = rev(orden_trim)) +   # orden cronológico de arriba a abajo
  labs(
    x = "Log(ingreso per cápita del hogar)",
    y = NULL
  ) +
  theme_ridges(font_size = 11, grid = TRUE) +
  theme(
    strip.background = element_rect(fill = "grey92"),
    strip.text        = element_text(face = "bold", size = 11),
    panel.spacing      = unit(0.8, "cm"),
    axis.text.y        = element_text(size = 7.5),
    axis.text.x        = element_text(size = 8.5)
  )

ggsave(file.path(out_figuras, "fig_income_ridgeplot.png"),
       p, width = 13, height = alto_fig, dpi = 300, bg = "white", limitsize = FALSE)
message("  Figura guardada.")

# -------------------------------------------------------
# 3. TABLA RESUMEN: estadísticos ponderados por ciudad y año
# -------------------------------------------------------
message("Calculando estadísticos ponderados...")

weighted_quantile <- function(x, w, probs) {
  ord <- order(x)
  x_s <- x[ord]; w_s <- w[ord]
  cum_w <- cumsum(w_s) / sum(w_s)
  sapply(probs, function(p) x_s[which(cum_w >= p)[1]])
}

tab_resumen <- income_df %>%
  group_by(ciudad_lbl, ano, trimestre, trim_lbl) %>%
  summarise(
    media_log     = weighted.mean(log_income, fex_c, na.rm = TRUE),
    mediana_log   = weighted_quantile(log_income, fex_c, 0.5),
    p25_log       = weighted_quantile(log_income, fex_c, 0.25),
    p75_log       = weighted_quantile(log_income, fex_c, 0.75),
    media_nivel   = weighted.mean(per_capita_income, fex_c, na.rm = TRUE),
    mediana_nivel = weighted_quantile(per_capita_income, fex_c, 0.5),
    n_hogares     = n(),
    .groups = "drop"
  ) %>%
  arrange(ciudad_lbl, ano, trimestre)

writexl::write_xlsx(
  list(distribucion_ingreso = tab_resumen),
  file.path(out_tablas, "tab_income_distribucion.xlsx")
)

message("  Tabla guardada.")
message("\nListo. Outputs en: ", out_dir)
