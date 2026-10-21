########################################################
## informe_resultados/fig_shadow_prices.R
##
## Sección 5.6 — Análisis de sensibilidad:
##   Nutrientes limitantes y elasticidades de precio sombra
##
## Figura 7: Frecuencia anual de restricciones limitantes
##   heatmap filas=nutrientes | cols=año | fill=% meses
##   facet_grid(ciudad_lbl ~ member)
##
## Figura 8: SPE media anual por nutriente — dot plot
##   punto=media anual ± 1 SD | color=año
##   facet_grid(ciudad_lbl ~ member)
##
## Figura 9: Evolución anual de la SPE — heatmap
##   filas=nutrientes | cols=año | fill=SPE discretizada
##   facet_grid(ciudad_lbl ~ member)
##
## Figura 10 (Apéndice): Contribución media de alimentos
##   al suministro de nutrientes en la dieta CoNA
##   heatmap filas=nutrientes | cols=alimentos
##   facet_grid(member ~ ciudad_lbl)
##
## Insumos:
##   02_models/cona/cona_results.rds
##   01_data_preparation/panel_food_paper.rds
##
## Output:
##   informe_resultados/output/figuras/fig_binding_anual.png
##   informe_resultados/output/figuras/fig_spe_dotplot.png
##   informe_resultados/output/figuras/fig_spe_heatmap.png
##   informe_resultados/output/figuras/fig_nutrient_sources.png
##   informe_resultados/output/tablas/tab_shadow_prices.xlsx
########################################################

library(tidyverse)
library(lubridate)
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

cona_path  <- file.path(base_dir, "review-food-security-paper/02_models/cona/cona_results.rds")
panel_path <- file.path(base_dir, "review-food-security-paper/01_data_preparation/panel_food_paper.rds")

out_dir     <- file.path(base_dir, "review-food-security-paper/0A_InformeFinal/informe_resultados/output")
out_figuras <- file.path(out_dir, "figuras")
out_tablas  <- file.path(out_dir, "tablas")
dir.create(out_figuras, showWarnings = FALSE, recursive = TRUE)
dir.create(out_tablas,  showWarnings = FALSE, recursive = TRUE)

# -------------------------------------------------------
# PARÁMETROS GLOBALES
# -------------------------------------------------------
PAPER_START  <- as.Date("2019-01-01")
PAPER_END    <- as.Date("2024-12-01")
TOP_N_DOT    <- 10    # nutrientes para dot plot
TOP_N_HEAT   <- 20    # nutrientes para heatmap SPE
MIN_PCT_FOOD <- 3     # umbral contribución alimentos (Fig 10)

CITY_LABELS <- c(
  "BOGOTÁ D.C." = "Bogotá", "BOGOTA" = "Bogotá",
  "MEDELLÍN"    = "Medellín", "MEDELLIN" = "Medellín",
  "CALI"        = "Cali"
)
CITY_ORDER <- c("Bogotá", "Medellín", "Cali")

MEMBER_LABS <- c(
  "0_[31,51)"  = "Hombre adulto [31-51)",
  "1_[31,51)"  = "Mujer adulta [31-51)",
  "1_[10, 14)" = "Niña [10-14)"
)
MEMBER_ORDER <- c("Hombre adulto [31-51)", "Mujer adulta [31-51)", "Niña [10-14)")

# Mapa nutriente -> columna en panel_food_paper
NUTRIENT_MAP <- c(
  "Calcium"       = "calcio_mg",
  "Carbohydrates" = "carbohidratos_totales_g",
  "Folate"        = "folatos_mcg",
  "Iron"          = "hierro_mg",
  "Lipids"        = "lipidos_g",
  "Magnesium"     = "magnesio_mg",
  "Niacin"        = "niacina_mg",
  "Protein"       = "proteina_g",
  "Riboflavin"    = "riboflavina_mg",
  "Thiamine"      = "tiamina_mg",
  "VitaminA"      = "vitamina_a_er",
  "VitaminB12"    = "vitamina_b12_mcg",
  "VitaminC"      = "vitamina_c_mg",
  "Zinc"          = "zinc_mg"
)

NUTRIENT_ESP <- c(
  "Calcium"       = "Calcio",       "Carbohydrates" = "Carbohidratos",
  "Folate"        = "Folato",       "Iron"          = "Hierro",
  "Lipids"        = "Lípidos",      "Magnesium"     = "Magnesio",
  "Niacin"        = "Niacina",      "Protein"       = "Proteína",
  "Riboflavin"    = "Riboflavina",  "Thiamine"      = "Tiamina",
  "VitaminA"      = "Vitamina A",   "VitaminB12"    = "Vitamina B12",
  "VitaminC"      = "Vitamina C",   "Zinc"          = "Zinc"
)

# Paletas
FREQ_PALETTE <- c(
  "[0%, 25%)"   = "#EFF3FF", "[25%, 50%)"  = "#BDD7E7",
  "[50%, 75%)"  = "#6BAED6", "[75%, 100%)" = "#2171B5",
  "100%"        = "#084594"
)
SPE_PALETTE <- c(
  "0"            = "#F5F5F5", "(0, 0.02]"    = "#FFF176",
  "(0.02, 0.10]" = "#FB8C00", "(0.10, 0.40]" = "#C62828",
  "(0.40, ∞)"    = "#6A1B9A"
)
ANO_COLORS <- setNames(
  colorRampPalette(c("#A8D5A2", "#1A5C1A"))(6),
  as.character(2019:2024)
)

# Tema compartido
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
# 1. CARGAR DATOS
# -------------------------------------------------------
message("Cargando cona_results.rds...")
cona <- readRDS(cona_path)

# Preprocesar limit
limit_full <- cona$limit %>%
  mutate(
    fecha      = as.Date(fecha),
    ano        = year(fecha),
    member     = MEMBER_LABS[paste0(Sex, "_", Age)],
    member     = factor(member, levels = MEMBER_ORDER),
    ciudad_lbl = factor(CITY_LABELS[ciudad], levels = CITY_ORDER)
  ) %>%
  filter(fecha >= PAPER_START, fecha <= PAPER_END,
         !is.na(member), !is.na(ciudad_lbl))

# Preprocesar spe
spe_full <- cona$spe %>%
  mutate(
    fecha      = as.Date(fecha),
    ano        = year(fecha),
    member     = MEMBER_LABS[paste0(Sex, "_", Age)],
    member     = factor(member, levels = MEMBER_ORDER),
    ciudad_lbl = factor(CITY_LABELS[ciudad], levels = CITY_ORDER)
  ) %>%
  filter(constraint == "Min",
         fecha >= PAPER_START, fecha <= PAPER_END,
         !is.na(member), !is.na(ciudad_lbl))

# Orden global de nutrientes por frecuencia de restricción
nutrient_order_freq <- limit_full %>%
  group_by(Nutrients) %>%
  summarise(freq = mean(Limiting == 1, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(freq)) %>%
  pull(Nutrients)

message(sprintf("  limit: %d filas | spe: %d filas", nrow(limit_full), nrow(spe_full)))

# -------------------------------------------------------
# 2. FIGURA 7: FRECUENCIA ANUAL DE RESTRICCIONES LIMITANTES
# -------------------------------------------------------
message("\n[Fig 7] Frecuencia anual de restricciones limitantes...")

binding_anual <- limit_full %>%
  group_by(Nutrients, member, ciudad_lbl, ano) %>%
  summarise(freq_pct = mean(Limiting == 1, na.rm = TRUE) * 100,
            .groups  = "drop") %>%
  mutate(
    Nutrients = factor(Nutrients, levels = rev(nutrient_order_freq)),
    freq_cat  = cut(freq_pct,
                    breaks = c(-Inf, 25, 50, 75, 99.9, Inf),
                    labels = names(FREQ_PALETTE),
                    include.lowest = TRUE, right = FALSE),
    freq_cat  = factor(freq_cat, levels = names(FREQ_PALETTE))
  )

p7 <- ggplot(binding_anual,
             aes(x = factor(ano), y = Nutrients, fill = freq_cat)) +
  geom_tile(color = "white", linewidth = 0.4) +
  geom_text(aes(label = sprintf("%.0f%%", freq_pct),
                color  = freq_pct > 60),
            size = 2.5, show.legend = FALSE) +
  facet_grid(ciudad_lbl ~ member) +
  scale_fill_manual(values = FREQ_PALETTE, name = "% meses limitante",
                    drop = FALSE, na.value = "grey90") +
  scale_color_manual(values = c("FALSE" = "grey20", "TRUE" = "white"),
                     guide = "none") +
  labs(x = NULL, y = NULL) +
  tema_heat

ggsave(file.path(out_figuras, "fig_binding_anual.png"),
       p7, width = 13, height = 9, dpi = 300, bg = "white")
message("  Figura 7 guardada.")

# -------------------------------------------------------
# 3. FIGURA 8: SPE MEDIA ANUAL — DOT PLOT
# -------------------------------------------------------
message("\n[Fig 8] SPE media anual por nutriente — dot plot...")

top_n_dot <- nutrient_order_freq[1:TOP_N_DOT]

spe_anual_dot <- spe_full %>%
  filter(Nutrients %in% top_n_dot) %>%
  group_by(Nutrients, member, ciudad_lbl, ano) %>%
  summarise(
    mean_SPE = mean(SPE, na.rm = TRUE),
    sd_SPE   = sd(SPE,   na.rm = TRUE),
    .groups  = "drop"
  ) %>%
  mutate(
    lo        = pmax(mean_SPE - sd_SPE, 0),
    hi        = mean_SPE + sd_SPE,
    Nutrients = factor(Nutrients, levels = rev(top_n_dot)),
    ano_fct   = factor(ano)
  )

p8 <- ggplot(spe_anual_dot,
             aes(x = mean_SPE, y = Nutrients, color = ano_fct)) +
  geom_linerange(aes(xmin = lo, xmax = hi),
                 position = position_dodge(width = 0.7),
                 linewidth = 0.6, alpha = 0.5) +
  geom_point(position = position_dodge(width = 0.7),
             size = 1.8, alpha = 0.95) +
  geom_vline(xintercept = 0, color = "grey50",
             linetype = "dotted", linewidth = 0.4) +
  facet_grid(ciudad_lbl ~ member, scales = "free_x") +
  scale_color_manual(values = ANO_COLORS, name = "Año") +
  scale_x_continuous(labels = number_format(accuracy = 0.001)) +
  labs(x = "SPE media (proporción del costo diario de la dieta)",
       y = NULL) +
  theme_bw(base_size = 10.5) +
  theme(
    axis.text.x        = element_text(angle = 45, hjust = 1, size = 7.5),
    axis.text.y        = element_text(size = 8.5),
    strip.background   = element_rect(fill = "grey92"),
    strip.text.x       = element_text(face = "bold", size = 9),
    strip.text.y       = element_text(face = "bold", size = 9, angle = 0),
    legend.position    = "bottom",
    legend.direction   = "horizontal",
    legend.key.width   = unit(0.8, "cm"),
    legend.background  = element_rect(color = "black", fill = "white",
                                      linewidth = 0.5),
    legend.margin      = margin(3, 8, 3, 8),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = "grey92", linewidth = 0.3),
    panel.grid.minor   = element_blank(),
    panel.spacing      = unit(0.35, "cm")
  )

ggsave(file.path(out_figuras, "fig_spe_dotplot.png"),
       p8, width = 13, height = 9, dpi = 300, bg = "white")
message("  Figura 8 guardada.")

# -------------------------------------------------------
# 4. FIGURA 9: EVOLUCIÓN ANUAL DE LA SPE — HEATMAP
# -------------------------------------------------------
message("\n[Fig 9] Evolución anual de la SPE — heatmap...")

top_n_heat <- nutrient_order_freq[1:min(TOP_N_HEAT, length(nutrient_order_freq))]

spe_anual_heat <- spe_full %>%
  filter(Nutrients %in% top_n_heat) %>%
  group_by(Nutrients, member, ciudad_lbl, ano) %>%
  summarise(mean_SPE = mean(SPE, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    Nutrients = factor(Nutrients, levels = rev(top_n_heat)),
    SPE_cat   = cut(mean_SPE,
                    breaks = c(-Inf, 0, 0.02, 0.10, 0.40, Inf),
                    labels = names(SPE_PALETTE),
                    include.lowest = TRUE, right = TRUE),
    SPE_cat   = factor(SPE_cat, levels = names(SPE_PALETTE))
  )

p9 <- ggplot(spe_anual_heat,
             aes(x = factor(ano), y = Nutrients, fill = SPE_cat)) +
  geom_tile(color = "white", linewidth = 0.35) +
  geom_text(aes(label = sprintf("%.3f", mean_SPE),
                color  = mean_SPE > 0.10),
            size = 2.2, show.legend = FALSE) +
  facet_grid(ciudad_lbl ~ member) +
  scale_fill_manual(values = SPE_PALETTE, name = "SPE media anual",
                    drop = FALSE, na.value = "grey90") +
  scale_color_manual(values = c("FALSE" = "grey20", "TRUE" = "white"),
                     guide = "none") +
  labs(x = NULL, y = NULL) +
  tema_heat

ggsave(file.path(out_figuras, "fig_spe_heatmap.png"),
       p9, width = 13, height = 9, dpi = 300, bg = "white")
message("  Figura 9 guardada.")

# -------------------------------------------------------
# 5. FIGURA 10 (APÉNDICE): CONTRIBUCIÓN DE ALIMENTOS
# -------------------------------------------------------
message("\n[Fig 10] Contribución media de alimentos al suministro de nutrientes...")

# Nutrientes limitantes (excl. Sodio)
binding_nutrients <- limit_full %>%
  group_by(Nutrients) %>%
  summarise(freq = mean(Limiting == 1) * 100, .groups = "drop") %>%
  filter(freq > 0, Nutrients != "Sodium") %>%
  pull(Nutrients)

nutrient_map_use <- NUTRIENT_MAP[names(NUTRIENT_MAP) %in% binding_nutrients]

# Densidad nutricional media por alimento × ciudad
data_paper <- readRDS(panel_path)
message("  panel_food_paper.rds cargado.")

nutri_density <- data_paper %>%
  mutate(fecha = as.Date(fecha)) %>%
  filter(fecha >= PAPER_START, fecha <= PAPER_END) %>%
  group_by(ciudad, articulo) %>%
  summarise(
    across(all_of(unname(nutrient_map_use)), ~ mean(.x, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  rename(Food = articulo)

# Contribución mensual
comp <- cona$comp %>%
  mutate(fecha = as.Date(fecha)) %>%
  filter(fecha >= PAPER_START, fecha <= PAPER_END) %>%
  left_join(nutri_density, by = c("ciudad", "Food")) %>%
  mutate(across(all_of(unname(nutrient_map_use)),
                ~ .x * quantity / 100,
                .names = "contrib_{.col}"))

contrib_cols <- paste0("contrib_", unname(nutrient_map_use))

pct_monthly <- comp %>%
  select(ciudad, Demo_Group, Sex, Food, fecha, all_of(contrib_cols)) %>%
  pivot_longer(cols = all_of(contrib_cols),
               names_to = "nutrient_col", values_to = "contrib") %>%
  mutate(
    nutrient_col = gsub("^contrib_", "", nutrient_col),
    Nutrient     = {
      rev_map <- setNames(names(nutrient_map_use), unname(nutrient_map_use))
      rev_map[nutrient_col]
    }
  ) %>%
  filter(!is.na(contrib), !is.na(Nutrient)) %>%
  group_by(ciudad, Demo_Group, Sex, fecha, Nutrient) %>%
  mutate(pct = contrib / sum(contrib, na.rm = TRUE) * 100) %>%
  ungroup()

mean_pct <- pct_monthly %>%
  group_by(ciudad, Demo_Group, Sex, Food, Nutrient) %>%
  summarise(mean_pct = mean(pct, na.rm = TRUE), .groups = "drop")

core_foods <- mean_pct %>%
  group_by(Food) %>%
  summarise(max_pct = max(mean_pct, na.rm = TRUE), .groups = "drop") %>%
  filter(max_pct >= MIN_PCT_FOOD) %>%
  pull(Food)

food_order <- mean_pct %>%
  filter(Food %in% core_foods) %>%
  group_by(Food) %>%
  summarise(overall = mean(mean_pct, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(overall)) %>%
  pull(Food)

nutrient_spe_order <- spe_full %>%
  filter(Nutrients %in% binding_nutrients) %>%
  group_by(Nutrients) %>%
  summarise(mean_SPE = mean(SPE, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(mean_SPE)) %>%
  pull(Nutrients)

plot_data10 <- mean_pct %>%
  filter(Food %in% core_foods) %>%
  mutate(
    ciudad_lbl  = factor(CITY_LABELS[ciudad], levels = CITY_ORDER),
    member_key  = paste0(Sex, "_", Demo_Group),
    member_lbl  = factor(MEMBER_LABS[member_key], levels = MEMBER_ORDER),
    Food        = factor(Food, levels = food_order),
    Nutrient_es = NUTRIENT_ESP[Nutrient],
    Nutrient_es = factor(Nutrient_es,
                         levels = rev(NUTRIENT_ESP[nutrient_spe_order])),
    pct_label   = if_else(mean_pct >= 5,
                          paste0(round(mean_pct, 0), "%"), "")
  ) %>%
  filter(!is.na(ciudad_lbl), !is.na(member_lbl), !is.na(Nutrient_es))

p10 <- ggplot(plot_data10,
              aes(x = Food, y = Nutrient_es, fill = mean_pct)) +
  geom_tile(color = "white", linewidth = 0.25) +
  geom_text(aes(label = pct_label,
                color  = mean_pct > 50),
            size = 1.8, show.legend = FALSE) +
  facet_grid(member_lbl ~ ciudad_lbl) +
  scale_fill_gradient(
    low = "#EFF3FF", high = "#1A5276",
    name   = "% contribución\nmedia mensual",
    limits = c(0, 100),
    labels = function(x) paste0(x, "%"),
    na.value = "grey95"
  ) +
  scale_color_manual(
    values = c("FALSE" = "grey20", "TRUE" = "white"),
    guide  = "none"
  ) +
  labs(x = NULL, y = NULL) +
  theme_bw(base_size = 9.5) +
  theme(
    axis.text.x       = element_text(angle = 45, hjust = 1, size = 7),
    axis.text.y       = element_text(size = 8),
    strip.background  = element_rect(fill = "grey92"),
    strip.text.x      = element_text(face = "bold", size = 9),
    strip.text.y      = element_text(face = "bold", size = 8, angle = 0),
    legend.position   = "right",
    legend.key.height = unit(2, "cm"),
    panel.grid        = element_blank(),
    panel.spacing     = unit(0.3, "cm")
  )

ggsave(file.path(out_figuras, "fig_nutrient_sources.png"),
       p10, width = 16, height = 10, dpi = 300, bg = "white")
message("  Figura 10 guardada.")

# -------------------------------------------------------
# 6. TABLAS CONSOLIDADAS
# -------------------------------------------------------
message("\nGuardando tablas...")

writexl::write_xlsx(
  list(
    binding_anual    = binding_anual %>%
      select(Nutrients, member, ciudad_lbl, ano, freq_pct) %>%
      arrange(Nutrients, member, ciudad_lbl, ano),
    spe_dotplot      = spe_anual_dot %>%
      select(Nutrients, member, ciudad_lbl, ano, mean_SPE, sd_SPE) %>%
      arrange(Nutrients, member, ciudad_lbl, ano),
    spe_heatmap      = spe_anual_heat %>%
      select(Nutrients, member, ciudad_lbl, ano, mean_SPE, SPE_cat) %>%
      arrange(Nutrients, member, ciudad_lbl, ano),
    nutrient_sources = plot_data10 %>%
      select(ciudad_lbl, member_lbl, Food, Nutrient_es, mean_pct) %>%
      arrange(ciudad_lbl, member_lbl, Nutrient_es, desc(mean_pct))
  ),
  file.path(out_tablas, "tab_shadow_prices.xlsx")
)

message("  Tabla consolidada guardada: tab_shadow_prices.xlsx")
message("\nListo. Outputs en: ", out_dir)