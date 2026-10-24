########################################################
## informe_resultados/fig_afford_deciles.R
##
## Sección 5.7 — No asequibilidad de los deciles del ingreso
##
## Figura 11: Tasa de no asequibilidad (FGT0) por decil,
##   2019 vs 2024 — línea por año, facet ciudad × modelo
##
## Figura 12: Brecha de no asequibilidad (FGT1) por decil,
##   2019 vs 2024 — misma estructura que Fig 11
##
## Figura 13: Evolución de la tasa de no asequibilidad por
##   decil — heatmap filas=decil, cols=tiempo, fill=tasa
##   facet ciudad × modelo (versión anual y mensual)
##
## Figura 13B: Evolución de la brecha (FGT1) por decil —
##   misma estructura que Fig 13 (versión anual y mensual)
##
## Figura 14: Tasa de no asequibilidad agregada por
##   ciudad y modelo — boxplot anual / línea mensual
##
## Figura 15: Brecha agregada (FGT1) por ciudad y
##   modelo — boxplot anual / línea mensual
##
## Figura 16 (Apéndice): Tasa contrafactual anual por
##   ciudad y modelo (100% del ingreso a alimentación)
##
## Insumos:
##   03_income/affordability/afford_results.xlsx
##   03_income/affordability/counterfactual_results.rds
##
## Output:
##   informe_resultados/output/figuras/fig_afford_*.png  (6 figuras)
##   informe_resultados/output/tablas/tab_afford_deciles.xlsx
########################################################

library(tidyverse)
library(readxl)
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

afford_path <- file.path(base_dir, "review-food-security-paper/03_income/affordability/afford_results.xlsx")
cf_path     <- file.path(base_dir, "review-food-security-paper/03_income/affordability/counterfactual_results.rds")

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

CITY_ORDER  <- c("Bogotá", "Medellín", "Cali")
CITY_COLORS <- c("Bogotá" = "#2E5FA3", "Medellín" = "#C0392B", "Cali" = "#1A7A4A")
MODEL_ORDER <- c("CoCA", "CoNA", "CoRD")

YEAR_COLORS <- c("2019" = "#2C3E6B", "2024" = "#C0392B")

RATE_PALETTE <- c(
  "0"         = "#F7F7F7",
  "(0, 25]"   = "#FDEBB0",
  "(25, 50]"  = "#FDAE61",
  "(50, 75]"  = "#D73027",
  "(75, 100]" = "#7B1FA2"
)

GAP_PALETTE <- c(
  "0"            = "#F7F7F7",
  "(0, 0.1]"     = "#D9F0D3",
  "(0.1, 0.25]"  = "#A6D96A",
  "(0.25, 0.5]"  = "#FDAE61",
  "(0.5, 1]"     = "#D73027"
)

tema_base <- theme_bw(base_size = 10.5) +
  theme(
    legend.position    = "top",
    legend.direction   = "horizontal",
    legend.key.width   = unit(1.2, "cm"),
    legend.background  = element_rect(color = "black", fill = "white",
                                      linewidth = 0.5),
    legend.margin      = margin(3, 8, 3, 8),
    strip.background   = element_rect(fill = "grey92"),
    strip.text         = element_text(face = "bold", size = 9),
    panel.grid         = element_blank(),
    axis.text.x        = element_text(size = 8),
    axis.text.y        = element_text(size = 8),
    plot.title         = element_text(size = 12, face = "bold"),
    plot.subtitle      = element_text(size = 9, color = "grey40")
  )

tema_heat <- theme_bw(base_size = 10.5) +
  theme(
    axis.text.x       = element_text(angle = 45, hjust = 1, size = 8),
    axis.text.y       = element_text(size = 8),
    strip.background  = element_rect(fill = "grey92"),
    strip.text.x      = element_text(face = "bold", size = 9),
    strip.text.y      = element_text(face = "bold", size = 9, angle = 0),
    legend.position   = "bottom",
    legend.direction  = "horizontal",
    legend.key.width  = unit(1.2, "cm"),
    legend.background = element_rect(color = "black", fill = "white",
                                     linewidth = 0.5),
    legend.margin     = margin(3, 8, 3, 8),
    panel.grid        = element_blank(),
    panel.spacing     = unit(0.35, "cm")
  )

# -------------------------------------------------------
# 1. CARGAR Y ESTANDARIZAR
# -------------------------------------------------------
message("Cargando afford_results.xlsx...")

afford <- read_excel(afford_path) %>%
  mutate(
    fecha      = as.Date(fecha),
    ano        = year(fecha),
    ciudad_lbl = case_when(
      grepl("BOGOT", ciudad, ignore.case = TRUE) ~ "Bogotá",
      grepl("MEDEL", ciudad, ignore.case = TRUE) ~ "Medellín",
      grepl("CALI",  ciudad, ignore.case = TRUE) ~ "Cali",
      TRUE ~ ciudad
    ),
    ciudad_lbl = factor(ciudad_lbl, levels = CITY_ORDER),
    model      = factor(model, levels = MODEL_ORDER),
    decil_num  = as.integer(gsub("\\D", "", deciles)),
    deciles    = factor(deciles, levels = paste0("Decil ", 1:10))
  ) %>%
  filter(fecha >= PAPER_START, fecha <= PAPER_END,
         !is.na(ciudad_lbl), !is.na(model))

message(sprintf("  %d filas | %d ciudades | %d modelos | %s a %s",
                nrow(afford), n_distinct(afford$ciudad_lbl),
                n_distinct(afford$model),
                min(afford$fecha), max(afford$fecha)))

# Promedio anual por decil × ciudad × modelo
afford_anual_decil <- afford %>%
  group_by(deciles, decil_num, ciudad_lbl, model, ano) %>%
  summarise(
    rate     = mean(rate,     na.rm = TRUE),
    gap      = mean(gap,      na.rm = TRUE),
    severity = mean(severity, na.rm = TRUE),
    .groups  = "drop"
  )

# Promedio anual agregado (across deciles) por ciudad × modelo
afford_anual_agg <- afford %>%
  group_by(ciudad_lbl, model, ano) %>%
  summarise(
    rate = mean(rate, na.rm = TRUE),
    gap  = mean(gap,  na.rm = TRUE),
    .groups = "drop"
  )

# -------------------------------------------------------
# 2. FIGURA 11: TASA FGT0 POR DECIL, 2019 vs 2024 — BOXPLOT
# -------------------------------------------------------
message("\n[Fig 11] Tasa de no asequibilidad por decil, 2019 vs 2024 (boxplot)...")

# Usamos las observaciones mensuales (no el promedio anual) para el boxplot
afford_compare_monthly <- afford %>%
  filter(ano %in% c(2019, 2024)) %>%
  mutate(ano_lbl = factor(as.character(ano), levels = c("2019", "2024")))

p11 <- ggplot(afford_compare_monthly,
              aes(x = deciles, y = rate, fill = ano_lbl, color = ano_lbl)) +
  geom_boxplot(alpha = 0.35, outlier.shape = NA,
               position = position_dodge(width = 0.75),
               linewidth = 0.45, width = 0.65) +
  facet_grid(ciudad_lbl ~ model) +
  scale_fill_manual(values = YEAR_COLORS, name = NULL) +
  scale_color_manual(values = YEAR_COLORS, name = NULL) +
  scale_y_continuous(labels = function(x) sprintf("%.0f%%", x), limits = c(0, NA)) +
  scale_x_discrete(labels = 1:10) +
  labs(x = "Decil de ingreso", y = "Tasa de no asequibilidad (%)") +
  tema_base

ggsave(file.path(out_figuras, "fig_afford_rate_deciles.png"),
       p11, width = 12, height = 9, dpi = 300, bg = "white")
message("  Figura 11 guardada.")

# -------------------------------------------------------
# 3. FIGURA 12: BRECHA FGT1 POR DECIL, 2019 vs 2024
# -------------------------------------------------------
message("\n[Fig 12] Brecha de no asequibilidad por decil, 2019 vs 2024...")

p12 <- ggplot(afford_compare_monthly,
              aes(x = deciles, y = gap, fill = ano_lbl, color = ano_lbl)) +
  geom_boxplot(alpha = 0.35, outlier.shape = NA,
               position = position_dodge(width = 0.75),
               linewidth = 0.45, width = 0.65) +
  facet_grid(ciudad_lbl ~ model) +
  scale_fill_manual(values = YEAR_COLORS, name = NULL) +
  scale_color_manual(values = YEAR_COLORS, name = NULL) +
  scale_y_continuous(labels = function(x) sprintf("%.2f", x), limits = c(0, NA)) +
  scale_x_discrete(labels = 1:10) +
  labs(x = "Decil de ingreso", y = "Brecha de no asequibilidad (FGT1, 0-1)") +
  tema_base

ggsave(file.path(out_figuras, "fig_afford_gap_deciles.png"),
       p12, width = 12, height = 9, dpi = 300, bg = "white")
message("  Figura 12 guardada.")

# -------------------------------------------------------
# 4. FIGURA 13: HEATMAP TASA POR DECIL — ANUAL Y MENSUAL
# -------------------------------------------------------
message("\n[Fig 13] Evolución de la tasa por decil — heatmap...")

deciles_rev <- rev(paste0("Decil ", 1:10))

## --- Versión ANUAL ---
afford_heat_anual <- afford_anual_decil %>%
  mutate(
    rate_cat = cut(rate,
                   breaks = c(-Inf, 0, 25, 50, 75, Inf),
                   labels = names(RATE_PALETTE),
                   include.lowest = TRUE, right = TRUE),
    rate_cat = factor(rate_cat, levels = names(RATE_PALETTE)),
    deciles  = factor(deciles, levels = deciles_rev)
  )

p13_anual <- ggplot(afford_heat_anual,
                    aes(x = factor(ano), y = deciles, fill = rate_cat)) +
  geom_tile(color = "white", linewidth = 0.3) +
  geom_text(aes(label = sprintf("%.0f", rate), color = rate > 60),
            size = 2.3, show.legend = FALSE) +
  facet_grid(model ~ ciudad_lbl) +
  scale_fill_manual(values = RATE_PALETTE, name = "Tasa de no\nasequibilidad (%)",
                    drop = FALSE) +
  scale_color_manual(values = c("FALSE" = "grey20", "TRUE" = "white"), guide = "none") +
  labs(
    x = NULL, y = NULL,
    subtitle = paste0("Cada celda: % promedio de hogares de ese decil, ciudad y modelo ",
                      "que no costearon la dieta\nen los 12 meses de ese año")
  ) +
  tema_heat +
  theme(plot.subtitle = element_text(size = 8.5, color = "grey40"))

ggsave(file.path(out_figuras, "fig_afford_rate_heatmap_anual.png"),
       p13_anual, width = 12, height = 11, dpi = 300, bg = "white")
message("  Figura 13 (anual) guardada.")

## --- Versión MENSUAL ---
afford_heat_mensual <- afford %>%
  mutate(
    rate_cat = cut(rate,
                   breaks = c(-Inf, 0, 25, 50, 75, Inf),
                   labels = names(RATE_PALETTE),
                   include.lowest = TRUE, right = TRUE),
    rate_cat = factor(rate_cat, levels = names(RATE_PALETTE)),
    deciles  = factor(deciles, levels = deciles_rev)
  )

p13_mensual <- ggplot(afford_heat_mensual,
                      aes(x = fecha, y = deciles, fill = rate_cat)) +
  geom_tile(color = "white", linewidth = 0.2) +
  facet_grid(model ~ ciudad_lbl) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", expand = c(0, 0)) +
  scale_fill_manual(values = RATE_PALETTE, name = "Tasa de no\nasequibilidad (%)",
                    drop = FALSE) +
  labs(
    x = NULL, y = NULL,
    subtitle = paste0("Cada celda: % de hogares de ese decil, ciudad y modelo ",
                      "que no costearon la dieta\nen ese mes")
  ) +
  tema_heat +
  theme(plot.subtitle = element_text(size = 8.5, color = "grey40"))

ggsave(file.path(out_figuras, "fig_afford_rate_heatmap_mensual.png"),
       p13_mensual, width = 14, height = 11, dpi = 300, bg = "white")
message("  Figura 13 (mensual) guardada.")

# -------------------------------------------------------
# 4B. FIGURA 13B: HEATMAP BRECHA (FGT1) POR DECIL — ANUAL Y MENSUAL
# -------------------------------------------------------
message("\n[Fig 13B] Evolución de la brecha (FGT1) por decil — heatmap...")

## --- Versión ANUAL ---
gap_heat_anual <- afford_anual_decil %>%
  mutate(
    gap_cat = cut(gap,
                  breaks = c(-Inf, 0, 0.1, 0.25, 0.5, Inf),
                  labels = names(GAP_PALETTE),
                  include.lowest = TRUE, right = TRUE),
    gap_cat = factor(gap_cat, levels = names(GAP_PALETTE)),
    deciles = factor(deciles, levels = deciles_rev)
  )

p13b_anual <- ggplot(gap_heat_anual,
                     aes(x = factor(ano), y = deciles, fill = gap_cat)) +
  geom_tile(color = "white", linewidth = 0.3) +
  geom_text(aes(label = sprintf("%.2f", gap), color = gap > 0.5),
            size = 2.3, show.legend = FALSE) +
  facet_grid(model ~ ciudad_lbl) +
  scale_fill_manual(values = GAP_PALETTE, name = "Brecha de no\nasequibilidad\n(FGT1, 0-1)",
                    drop = FALSE) +
  scale_color_manual(values = c("FALSE" = "grey20", "TRUE" = "white"), guide = "none") +
  labs(
    x = NULL, y = NULL,
    subtitle = paste0("Cada celda: déficit proporcional promedio del presupuesto ",
                      "de alimentos de ese decil\nrespecto al costo de la dieta, ",
                      "en los 12 meses de ese año")
  ) +
  tema_heat +
  theme(plot.subtitle = element_text(size = 8.5, color = "grey40"))

ggsave(file.path(out_figuras, "fig_afford_gap_heatmap_anual.png"),
       p13b_anual, width = 12, height = 11, dpi = 300, bg = "white")
message("  Figura 13B (anual) guardada.")

## --- Versión MENSUAL ---
gap_heat_mensual <- afford %>%
  mutate(
    gap_cat = cut(gap,
                  breaks = c(-Inf, 0, 0.1, 0.25, 0.5, Inf),
                  labels = names(GAP_PALETTE),
                  include.lowest = TRUE, right = TRUE),
    gap_cat = factor(gap_cat, levels = names(GAP_PALETTE)),
    deciles = factor(deciles, levels = deciles_rev)
  )

p13b_mensual <- ggplot(gap_heat_mensual,
                       aes(x = fecha, y = deciles, fill = gap_cat)) +
  geom_tile(color = "white", linewidth = 0.2) +
  facet_grid(model ~ ciudad_lbl) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", expand = c(0, 0)) +
  scale_fill_manual(values = GAP_PALETTE, name = "Brecha de no\nasequibilidad\n(FGT1, 0-1)",
                    drop = FALSE) +
  labs(
    x = NULL, y = NULL,
    subtitle = paste0("Cada celda: déficit proporcional del presupuesto de ",
                      "alimentos de ese decil\nrespecto al costo de la dieta, en ese mes")
  ) +
  tema_heat +
  theme(plot.subtitle = element_text(size = 8.5, color = "grey40"))

ggsave(file.path(out_figuras, "fig_afford_gap_heatmap_mensual.png"),
       p13b_mensual, width = 14, height = 11, dpi = 300, bg = "white")
message("  Figura 13B (mensual) guardada.")

# -------------------------------------------------------
# 5. FIGURA 14: TASA AGREGADA POR CIUDAD Y MODELO — ANUAL (BOXPLOT) Y MENSUAL (LÍNEA)
# -------------------------------------------------------
message("\n[Fig 14] Tasa agregada por ciudad y modelo...")

# Promedio mensual across deciles (cada mes = 1 obs por ciudad x modelo)
afford_monthly_agg <- afford %>%
  group_by(ciudad_lbl, model, fecha, ano) %>%
  summarise(rate = mean(rate, na.rm = TRUE), .groups = "drop")

n_anos <- n_distinct(afford_monthly_agg$ano)

## --- Versión ANUAL: boxplot con líneas separadoras ---
p14_anual <- ggplot(afford_monthly_agg,
                    aes(x = factor(ano), y = rate, fill = ciudad_lbl, color = ciudad_lbl)) +
  geom_vline(xintercept = seq(1.5, n_anos - 0.5, by = 1),
             color = "grey70", linetype = "dashed", linewidth = 0.35) +
  geom_boxplot(alpha = 0.35, outlier.shape = NA,
               position = position_dodge(width = 0.75),
               linewidth = 0.45, width = 0.65) +
  facet_wrap(~ model, nrow = 1) +
  scale_fill_manual(values = CITY_COLORS, name = NULL) +
  scale_color_manual(values = CITY_COLORS, name = NULL) +
  scale_y_continuous(labels = function(x) sprintf("%.0f%%", x), limits = c(0, NA)) +
  labs(x = NULL, y = "Tasa de no asequibilidad (%)") +
  tema_base

ggsave(file.path(out_figuras, "fig_afford_rate_agregado_anual.png"),
       p14_anual, width = 13, height = 5.5, dpi = 300, bg = "white")
message("  Figura 14 (anual) guardada.")

## --- Versión MENSUAL: línea por ciudad ---
p14_mensual <- ggplot(afford_monthly_agg,
                      aes(x = fecha, y = rate, color = ciudad_lbl)) +
  geom_line(linewidth = 0.85) +
  facet_wrap(~ model, nrow = 1) +
  scale_color_manual(values = CITY_COLORS, name = NULL) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(labels = function(x) sprintf("%.0f%%", x), limits = c(0, NA)) +
  labs(x = NULL, y = "Tasa de no asequibilidad (%)") +
  tema_base +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(file.path(out_figuras, "fig_afford_rate_agregado_mensual.png"),
       p14_mensual, width = 13, height = 5.5, dpi = 300, bg = "white")
message("  Figura 14 (mensual) guardada.")

# -------------------------------------------------------
# 6. FIGURA 15: BRECHA AGREGADA POR CIUDAD Y MODELO — ANUAL (BOXPLOT) Y MENSUAL (LÍNEA)
# -------------------------------------------------------
message("\n[Fig 15] Brecha agregada por ciudad y modelo...")

afford_monthly_gap <- afford %>%
  group_by(ciudad_lbl, model, fecha, ano) %>%
  summarise(gap = mean(gap, na.rm = TRUE), .groups = "drop")

## --- Versión ANUAL: boxplot con líneas separadoras ---
p15_anual <- ggplot(afford_monthly_gap,
                    aes(x = factor(ano), y = gap, fill = ciudad_lbl, color = ciudad_lbl)) +
  geom_vline(xintercept = seq(1.5, n_anos - 0.5, by = 1),
             color = "grey70", linetype = "dashed", linewidth = 0.35) +
  geom_boxplot(alpha = 0.35, outlier.shape = NA,
               position = position_dodge(width = 0.75),
               linewidth = 0.45, width = 0.65) +
  facet_wrap(~ model, nrow = 1, scales = "free_y") +
  scale_fill_manual(values = CITY_COLORS, name = NULL) +
  scale_color_manual(values = CITY_COLORS, name = NULL) +
  scale_y_continuous(labels = function(x) sprintf("%.3f", x), limits = c(0, NA)) +
  labs(x = NULL, y = "Brecha de no asequibilidad (FGT1, 0-1)") +
  tema_base

ggsave(file.path(out_figuras, "fig_afford_gap_agregado_anual.png"),
       p15_anual, width = 13, height = 5.5, dpi = 300, bg = "white")
message("  Figura 15 (anual) guardada.")

## --- Versión MENSUAL: línea por ciudad ---
p15_mensual <- ggplot(afford_monthly_gap,
                      aes(x = fecha, y = gap, color = ciudad_lbl)) +
  geom_line(linewidth = 0.85) +
  facet_wrap(~ model, nrow = 1, scales = "free_y") +
  scale_color_manual(values = CITY_COLORS, name = NULL) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(labels = function(x) sprintf("%.3f", x), limits = c(0, NA)) +
  labs(x = NULL, y = "Brecha de no asequibilidad (FGT1, 0-1)") +
  tema_base +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(file.path(out_figuras, "fig_afford_gap_agregado_mensual.png"),
       p15_mensual, width = 13, height = 5.5, dpi = 300, bg = "white")
message("  Figura 15 (mensual) guardada.")

# -------------------------------------------------------
# 7. FIGURA 16 (APÉNDICE): TASA CONTRAFACTUAL ANUAL
# -------------------------------------------------------
message("\n[Fig 16] Tasa contrafactual anual (apéndice)...")

cf <- readRDS(cf_path) %>%
  mutate(
    fecha      = as.Date(fecha),
    ano        = year(fecha),
    ciudad_lbl = factor(ciudad_lbl, levels = CITY_ORDER),
    model      = factor(model, levels = MODEL_ORDER)
  ) %>%
  filter(fecha >= PAPER_START, fecha <= PAPER_END,
         !is.na(ciudad_lbl), !is.na(model))

cf_anual <- cf %>%
  group_by(ciudad_lbl, model, ano) %>%
  summarise(rate = mean(rate, na.rm = TRUE), .groups = "drop")

cf_monthly <- cf %>%
  group_by(ciudad_lbl, model, fecha, ano) %>%
  summarise(rate = mean(rate, na.rm = TRUE), .groups = "drop")

p16 <- ggplot(cf_monthly,
              aes(x = factor(ano), y = rate, fill = ciudad_lbl, color = ciudad_lbl)) +
  geom_boxplot(alpha = 0.35, outlier.shape = NA,
               position = position_dodge(width = 0.75),
               linewidth = 0.45, width = 0.65) +
  facet_wrap(~ model, nrow = 3, scales = "free_y") +
  scale_fill_manual(values = CITY_COLORS, name = NULL) +
  scale_color_manual(values = CITY_COLORS, name = NULL) +
  scale_y_continuous(labels = function(x) sprintf("%.0f%%", x), limits = c(0, NA)) +
  labs(x = NULL, y = "Tasa de no asequibilidad contrafactual (%)") +
  tema_base

ggsave(file.path(out_figuras, "fig_afford_counterfactual.png"),
       p16, width = 8, height = 12, dpi = 300, bg = "white")
message("  Figura 16 guardada.")

# -------------------------------------------------------
# 8. TABLAS CONSOLIDADAS
# -------------------------------------------------------
message("\nGuardando tablas...")

writexl::write_xlsx(
  list(
    rate_gap_decil    = afford_anual_decil %>%
      arrange(model, ciudad_lbl, decil_num, ano),
    rate_gap_agregado = afford_anual_agg %>%
      arrange(model, ciudad_lbl, ano),
    rate_mensual_agg  = afford_monthly_agg %>%
      arrange(model, ciudad_lbl, fecha),
    gap_mensual_agg   = afford_monthly_gap %>%
      arrange(model, ciudad_lbl, fecha),
    contrafactual     = cf_anual %>%
      arrange(model, ciudad_lbl, ano)
  ),
  file.path(out_tablas, "tab_afford_deciles.xlsx")
)

message("  Tabla guardada: tab_afford_deciles.xlsx")
message("\nListo. Outputs en: ", out_dir)