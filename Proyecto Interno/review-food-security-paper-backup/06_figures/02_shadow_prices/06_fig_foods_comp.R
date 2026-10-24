########################################################
## appendix/fig_nutrient_sources.R
##
## Figure A. % contribution of each food to total nutrient
##           supply in the CoNA diet, by household member
##
##   Heatmap: rows = binding nutrients (excl. Sodium)
##            cols = core foods
##            fill = mean monthly % contribution
##            (averaged over full period 2019-2024)
##   facet_grid(member_lbl ~ ciudad_lbl)
##
## Logic:
##   1. contrib_{i,m,n,t} = q_{i,m,t} * density_{i,n} / 100
##   2. pct_{i,m,n,t}     = contrib / sum_i(contrib) * 100
##   3. mean_pct_{i,m,n}  = mean_t(pct)  [full period]
##
## Reads:  CONA_DIR/cona_results.rds
##         PREP_DIR/panel_food_paper.rds
##
## Writes: FIG_DIR/final/figA_nutrient_sources.png / .pdf
########################################################

source("00_config.R")
source(file.path(REVIEW_DIR, "06_figures", "00_fig_config.R"))
library(tidyverse)
library(lubridate)
library(scales)

# -----------------------------------------------------------------------
# 1. Load
# -----------------------------------------------------------------------
cona       <- readRDS(file.path(CONA_DIR, "cona_results.rds"))
data_paper <- readRDS(file.path(PREP_DIR, "panel_food_paper.rds"))

# -----------------------------------------------------------------------
# 2. Binding nutrients (excl. Sodium)
# -----------------------------------------------------------------------
binding_nutrients <- cona$limit %>%
  mutate(fecha = as.Date(fecha)) %>%
  filter(fecha >= PAPER_START, fecha <= PAPER_END) %>%
  group_by(Nutrients) %>%
  dplyr::summarise(freq = mean(Limiting == 1) * 100, .groups = "drop") %>%
  filter(freq > 0, Nutrients != "Sodium") %>%
  pull(Nutrients)

# -----------------------------------------------------------------------
# 3. Nutrient map: CoNA names -> data_paper columns
# -----------------------------------------------------------------------
nutrient_map <- c(
  "Calcium"      = "calcio_mg",
  "Carbohydrates"= "carbohidratos_totales_g",
  "Folate"       = "folatos_mcg",
  "Iron"         = "hierro_mg",
  "Lipids"       = "lipidos_g",
  "Magnesium"    = "magnesio_mg",
  "Niacin"       = "niacina_mg",
  "Protein"      = "proteina_g",
  "Riboflavin"   = "riboflavina_mg",
  "Thiamine"     = "tiamina_mg",
  "VitaminA"     = "vitamina_a_er",
  "VitaminB12"   = "vitamina_b12_mcg",
  "VitaminC"     = "vitamina_c_mg",
  "Zinc"         = "zinc_mg")

# Keep only binding nutrients
nutrient_map <- nutrient_map[names(nutrient_map) %in% binding_nutrients]

# -----------------------------------------------------------------------
# 4. Mean nutritional density per food × city (avg across time)
# -----------------------------------------------------------------------
nutri_density <- data_paper %>%
  mutate(fecha = as.Date(fecha)) %>%
  filter(fecha >= PAPER_START, fecha <= PAPER_END) %>%
  group_by(ciudad, articulo) %>%
  dplyr::summarise(
    across(all_of(unname(nutrient_map)),
           ~ mean(.x, na.rm = TRUE)),
    .groups = "drop") %>%
  rename(Food = articulo)

# -----------------------------------------------------------------------
# 5. Step 1: contrib_{i,m,n,t} = q_{i,m,t} * density_{i,n} / 100
# -----------------------------------------------------------------------
comp <- cona$comp %>%
  mutate(fecha = as.Date(fecha)) %>%
  filter(fecha >= PAPER_START, fecha <= PAPER_END) %>%
  left_join(nutri_density, by = c("ciudad","Food")) %>%
  mutate(across(all_of(unname(nutrient_map)),
                ~ .x * quantity / 100,
                .names = "contrib_{.col}"))

# -----------------------------------------------------------------------
# 6. Step 2: pct_{i,m,n,t} = contrib / sum_i(contrib) * 100
#    Compute within each (ciudad, Demo_Group, Sex, fecha, nutrient)
# -----------------------------------------------------------------------
contrib_cols <- paste0("contrib_", unname(nutrient_map))

pct_monthly <- comp %>%
  select(ciudad, Demo_Group, Sex, Food, fecha,
         all_of(contrib_cols)) %>%
  pivot_longer(cols      = all_of(contrib_cols),
               names_to  = "nutrient_col",
               values_to = "contrib") %>%
  mutate(nutrient_col = gsub("^contrib_", "", nutrient_col)) %>%
  mutate(Nutrient = {
    rev_map <- setNames(names(nutrient_map), unname(nutrient_map))
    rev_map[nutrient_col]
  }) %>%
  filter(!is.na(contrib), !is.na(Nutrient)) %>%
  group_by(ciudad, Demo_Group, Sex, fecha, Nutrient) %>%
  mutate(pct = contrib / sum(contrib, na.rm = TRUE) * 100) %>%
  ungroup()

# -----------------------------------------------------------------------
# 7. Step 3: mean_pct_{i,m,n} = mean_t(pct) over full period
# -----------------------------------------------------------------------
mean_pct <- pct_monthly %>%
  group_by(ciudad, Demo_Group, Sex, Food, Nutrient) %>%
  dplyr::summarise(
    mean_pct = mean(pct, na.rm = TRUE),
    .groups  = "drop")

# -----------------------------------------------------------------------
# 8. Prepare for plotting
# -----------------------------------------------------------------------

# Keep only foods contributing >= 3% in any member × nutrient × city
core_foods_plot <- mean_pct %>%
  group_by(Food) %>%
  dplyr::summarise(max_pct = max(mean_pct, na.rm=TRUE), .groups="drop") %>%
  filter(max_pct >= 3) %>%
  pull(Food)

# Member labels
member_labs <- c(
  "1_[10, 14)" = "Girl [10-14)",
  "0_[31,51)"  = "Adult male [31-51)",
  "1_[31,51)"  = "Adult female [31-51)")

# Food English labels
food_label_map <- c(
  "LECHE PASTEURIZADA"       = "Pasteurised milk",
  "ARVEJA SECA"              = "Dried peas",
  "ARVEJA TARRO"             = "Canned peas",
  "GARBANZO"                 = "Chickpeas",
  "LENTEJAS"                 = "Lentils",
  "PESCADO DE RÍO"           = "River fish",
  "VISCERAS - HIGADO"        = "Liver",
  "HARINA DE TRIGO"          = "Wheat flour",
  "HARINA PRECOCIDA"         = "Pre-cooked flour",
  "ARROZ PARA SECO"          = "Rice",
  "YUCA"                     = "Cassava",
  "GUAYABAS"                 = "Guavas",
  "NARANJAS"                 = "Oranges",
  "ZANAHORIA"                = "Carrots",
  "MANTECA VEGETAL"          = "Vegetable fat",
  "ACEITE DE SOYA O DE MAIZ" = "Soybean/corn oil",
  "AZUCAR NATURAL O MORENA"  = "Brown sugar",
  "PANELA"                   = "Panela",
  "SAL"                      = "Salt")

# Nutrient order by SPE magnitude (descending)
nutrient_order <- c("Zinc","Calcium","Carbohydrates","Lipids",
                    "Magnesium","VitaminB12","Protein",
                    "VitaminC","VitaminA","Folate","Niacin",
                    "Riboflavin","Thiamine","Iron")
nutrient_order <- nutrient_order[nutrient_order %in% unique(mean_pct$Nutrient)]

# Food order by overall mean contribution descending
food_order_plot <- mean_pct %>%
  filter(Food %in% core_foods_plot) %>%
  group_by(Food) %>%
  dplyr::summarise(overall = mean(mean_pct, na.rm=TRUE), .groups="drop") %>%
  arrange(desc(overall)) %>%
  mutate(Food_en = ifelse(Food %in% names(food_label_map),
                          food_label_map[Food], Food)) %>%
  pull(Food_en)

plot_data <- mean_pct %>%
  filter(Food %in% core_foods_plot) %>%
  mutate(
    ciudad_lbl = factor(CITY_LABS[ciudad],
                        levels = c("Bogotá","Medellín","Cali")),
    member_key = paste0(Sex, "_", Demo_Group),
    member_lbl = factor(member_labs[member_key],
                        levels = unname(member_labs)),
    Food_en    = ifelse(Food %in% names(food_label_map),
                        food_label_map[Food], Food),
    Food_en    = factor(Food_en, levels = food_order_plot),
    Nutrient   = factor(Nutrient, levels = rev(nutrient_order)),
    pct_label  = ifelse(mean_pct >= 5,
                        paste0(round(mean_pct, 0), "%"), ""))

# -----------------------------------------------------------------------
# 9. Figure — facet_grid(member_lbl ~ ciudad_lbl)
#    x = Food_en, y = Nutrient, fill = mean_pct
# -----------------------------------------------------------------------
fig_sources <- ggplot(plot_data,
                      aes(x = Food_en, y = Nutrient,
                          fill = mean_pct)) +
  geom_tile(color = "white", linewidth = 0.25) +
  geom_text(aes(label = pct_label,
                color  = mean_pct > 50),
            size = 1.8, family = "serif",
            show.legend = FALSE) +
  facet_grid(member_lbl ~ ciudad_lbl) +
  scale_fill_gradient(
    low      = "#EFF3FF",
    high     = "#1A5276",
    name     = "Mean monthly\n% contribution",
    limits   = c(0, 100),
    labels   = function(x) paste0(x, "%"),
    na.value = "grey95") +
  scale_color_manual(
    values = c("FALSE" = "grey20", "TRUE" = "white"),
    guide  = "none") +
  labs(
    title    = paste0("Figure A. Mean contribution of core foods to total ",
                      "nutrient supply in the CoNA diet by household member ",
                      "and city, 2019\u20132024"),
    subtitle = paste0("Mean of monthly % contributions over 2019\u20132024, ",
                      "by household member and city. ",
                      "Only foods contributing \u22653% to at least one ",
                      "nutrient\u2013member\u2013city combination shown. ",
                      "Labels for contributions \u22655%."),
    caption  = paste0(
      "Note: % computed as food contribution / total nutrient supply \u00d7 100 ",
      "within each month, then averaged over the full period.\n",
      "Nutrients ordered by shadow price (SPE) magnitude (highest to lowest). ",
      "Sodium excluded (see text). CoNA = Cost of Nutritional Adequacy."),
    x = NULL,
    y = NULL) +
  paper_theme() +
  theme(
    axis.text.x       = element_text(angle = 45, hjust = 1, size = 7),
    axis.text.y       = element_text(size = 8),
    strip.text.x      = element_text(face = "bold", size = 9),
    strip.text.y      = element_text(face = "bold", size = 8, angle = 0),
    legend.position   = "right",
    legend.key.height = unit(2, "cm"),
    panel.grid        = element_blank(),
    panel.spacing     = unit(0.3, "cm"))

ggsave(file.path(FIG_DIR, "final", "figA_nutrient_sources.png"),
       fig_sources, width = 16, height = 10, dpi = 300, bg = "white")
ggsave(file.path(FIG_DIR, "final", "figA_nutrient_sources.pdf"),
       fig_sources, width = 16, height = 10)

message("Figure A (nutrient sources) saved.")