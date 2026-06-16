########################################################
## 05_figures/bloque1_costos/fig02b_cona_composition.R
##
## Three independent figures:
##
## Fig 2B-i:  Heatmap — all foods ever selected, annual mean qty
## Fig 2B-iii: Cost contribution by food — MONTHLY
##            with vertical dashed lines at year boundaries
##            + CoNA per capita validation line
##
## Reads:  CONA_DIR/cona_results.rds
##         PREP_DIR/panel_food_paper.rds
##         PREP_DIR/deflator_monthly.rds
##         HCOST_DIR/hcost_full.rds
########################################################

source("00_config.R")
source(file.path(REVIEW_DIR, "06_figures", "00_fig_config.R"))
library(tidyverse)
library(lubridate)
library(scales)

# -----------------------------------------------------------------------
# 1. Load data
# -----------------------------------------------------------------------
cona       <- readRDS(file.path(CONA_DIR, "cona_results.rds"))
data_paper <- readRDS(file.path(PREP_DIR, "panel_food_paper.rds"))
hcost      <- readRDS(file.path(HCOST_DIR, "hcost_full.rds")) %>%
  mutate(fecha = as.Date(fecha))

deflator <- readRDS(file.path(PREP_DIR, "deflator_monthly.rds")) %>%
  filter(ciudad != "NACIONAL") %>%
  select(ciudad, fecha, deflator)

# Year boundary dates for vertical lines
year_lines <- as.Date(paste0(2020:2024, "-01-01"))

# -----------------------------------------------------------------------
# 2. All foods ever selected
# -----------------------------------------------------------------------
all_foods <- cona$comp %>%
  filter(quantity > 0) %>%
  pull(Food) %>% unique()

# -----------------------------------------------------------------------
# 3. FIGURE 2B-i: Heatmap — annual mean quantities, ALL foods
# -----------------------------------------------------------------------
# English food name map
food_name_en <- c(
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
  "ACEITE DE SOYA O DE MAÍZ" = "Soybean/corn oil",
  "AZÚCAR NATURAL O MORENA"  = "Brown sugar",
  "PANELA"                   = "Panela",
  "SAL"                      = "Salt")

# All city-food-year combinations (complete grid)
all_cities <- unique(cona$comp$ciudad)
all_years  <- 2019:2024

comp_grid <- expand.grid(
  ciudad = all_cities,
  Food   = all_foods,
  year   = all_years,
  stringsAsFactors = FALSE) %>%
  as_tibble()

comp_annual <- cona$comp %>%
  mutate(fecha = as.Date(fecha), year = year(fecha)) %>%
  filter(Food %in% all_foods,
         fecha >= PAPER_START, fecha <= PAPER_END) %>%
  group_by(ciudad, Food, year) %>%
  dplyr::summarise(
    mean_qty   = mean(quantity[quantity > 0], na.rm = TRUE),
    pct_months = mean(quantity > 0) * 100,
    .groups    = "drop") %>%
  # Complete grid so absent city-food-year cells get NA
  right_join(comp_grid, by = c("ciudad","Food","year")) %>%
  mutate(
    ciudad_lbl = factor(CITY_LABS[ciudad],
                        levels = c("Bogotá","Medellín","Cali")),
    Food_en    = food_name_en[Food])

food_order_all <- comp_annual %>%
  group_by(Food, Food_en) %>%
  dplyr::summarise(overall = mean(mean_qty, na.rm=TRUE), .groups="drop") %>%
  arrange(desc(overall)) %>%
  pull(Food_en)

comp_annual <- comp_annual %>%
  mutate(Food_en = factor(Food_en, levels = rev(food_order_all)))

fig2b_i <- ggplot(comp_annual,
                  aes(x = factor(year), y = Food_en, fill = mean_qty)) +
  geom_tile(color = "white", linewidth = 0.3) +
  geom_text(aes(label = ifelse(is.na(mean_qty), "",
                               as.character(round(mean_qty, 0)))),
            size = 2.3, family = "serif", color = "grey20") +
  facet_wrap(~ ciudad_lbl, nrow = 1) +
  scale_fill_gradient(
    low      = "#EFF3FF",
    high     = "#2166AC",
    name     = "Mean g/day",
    labels   = comma_format(big.mark = ","),
    na.value = "#F5B7B1") +
  labs(
    title    = paste0("Figure 2B(i). Annual mean daily quantity of all foods ",
                      "ever selected in the CoNA diet, 2019\u20132024"),
    subtitle = "Mean quantity among months in which each food was selected (g/day per member).",
    caption  = paste0(
      "Note: Values show mean daily quantity (g/day) among months in which ",
      "each food was selected (quantity > 0). ",
      "Red cells indicate the food was never selected in that city\u2013year.\n",
      "CoNA = Cost of Nutritional Adequacy."),
    x = NULL, y = NULL) +
  paper_theme() +
  theme(
    axis.text.x       = element_text(angle = 45, hjust = 1, size = 8),
    axis.text.y       = element_text(size = 8),
    strip.text        = element_text(face = "bold", size = 10),
    legend.position   = "right",
    legend.key.height = unit(2, "cm"),
    panel.grid        = element_blank(),
    panel.spacing     = unit(0.4, "cm"))

n_foods <- length(food_order_all)
fig_h_i <- max(8, n_foods * 0.45)

ggsave(file.path(FIG_DIR, "final", "fig02b_i_heatmap.png"),
       fig2b_i, width = 13, height = fig_h_i, dpi = 300, bg = "white")
ggsave(file.path(FIG_DIR, "final", "fig02b_i_heatmap.pdf"),
       fig2b_i, width = 13, height = fig_h_i)
message("Figure 2B-i saved.")

# -----------------------------------------------------------------------
# 4. FIGURE 2B-iii: Monthly cost contribution — ALL foods, grouped by GABAS
# -----------------------------------------------------------------------

# Number of household members — to compute correct per capita
N_MEMBERS <- cona$comp %>%
  distinct(Demo_Group, Sex) %>%
  nrow()

# GABAS group assignments
food_groups <- tribble(
  ~Food,                      ~grupo,
  "LECHE PASTEURIZADA",       "Dairy",
  "ARVEJA SECA",              "Meat, eggs & legumes",
  "LENTEJAS",                 "Meat, eggs & legumes",
  "GARBANZO",                 "Meat, eggs & legumes",
  "ARVEJA TARRO",             "Meat, eggs & legumes",
  "PESCADO DE RÍO",           "Meat, eggs & legumes",
  "VISCERAS - HIGADO",        "Meat, eggs & legumes",
  "HARINA DE TRIGO",          "Cereals & starches",
  "HARINA PRECOCIDA",         "Cereals & starches",
  "ARROZ PARA SECO",          "Cereals & starches",
  "YUCA",                     "Cereals & starches",
  "GUAYABAS",                 "Fruits & vegetables",
  "NARANJAS",                 "Fruits & vegetables",
  "ZANAHORIA",                "Fruits & vegetables",
  "MANTECA VEGETAL",          "Fats",
  "ACEITE DE SOYA O DE MAÍZ", "Fats",
  "AZÚCAR NATURAL O MORENA",  "Sugars",
  "PANELA",                   "Sugars",
  "SAL",                      "Other")

# -----------------------------------------------------------------------
# 4a. Per capita quantities — sum across members / N_MEMBERS
# -----------------------------------------------------------------------
qty_all_monthly <- cona$comp %>%
  mutate(fecha = as.Date(fecha)) %>%
  filter(Food %in% all_foods,
         fecha >= PAPER_START, fecha <= PAPER_END) %>%
  group_by(ciudad, Food, fecha) %>%
  dplyr::summarise(
    sum_qty   = sum(quantity, na.rm = TRUE),
    .groups   = "drop") %>%
  mutate(qty_pc = sum_qty / N_MEMBERS)

# -----------------------------------------------------------------------
# 4b. Real prices
# -----------------------------------------------------------------------
prices_all_monthly <- data_paper %>%
  mutate(fecha = as.Date(fecha)) %>%
  filter(articulo %in% all_foods,
         fecha >= PAPER_START, fecha <= PAPER_END) %>%
  left_join(deflator, by = c("ciudad","fecha")) %>%
  mutate(precio_real = precio_100g * deflator) %>%
  group_by(ciudad, articulo, fecha) %>%
  dplyr::summarise(mean_precio_real = mean(precio_real, na.rm=TRUE),
                   .groups = "drop") %>%
  rename(Food = articulo)

# -----------------------------------------------------------------------
# 4c. Cost contribution per capita
# -----------------------------------------------------------------------
cost_contrib_all <- qty_all_monthly %>%
  left_join(prices_all_monthly, by = c("ciudad","Food","fecha")) %>%
  mutate(contrib = mean_precio_real * qty_pc / 100) %>%
  filter(!is.na(contrib), contrib > 0) %>%
  left_join(food_groups, by = "Food") %>%
  mutate(grupo = ifelse(is.na(grupo), "Other", grupo))

# -----------------------------------------------------------------------
# 4d. CoNA per capita real from hcost — validation line
# -----------------------------------------------------------------------
cona_pc_real <- hcost %>%
  filter(model == "CoNA") %>%
  group_by(ciudad, fecha) %>%
  dplyr::summarise(cona_pc = mean(per_capita, na.rm=TRUE),
                   .groups = "drop") %>%
  left_join(deflator, by = c("ciudad","fecha")) %>%
  mutate(
    cona_pc_real = cona_pc * deflator,
    ciudad_lbl   = factor(CITY_LABS[ciudad],
                          levels = c("Bogotá","Medellín","Cali"))) %>%
  filter(fecha >= PAPER_START, fecha <= PAPER_END)

# -----------------------------------------------------------------------
# 4e. Palette and labels
# -----------------------------------------------------------------------
food_palette <- c(
  # Dairy — blue
  "LECHE PASTEURIZADA"       = "#1A5276",
  
  # Meat, eggs & legumes — reds and terracotta
  "ARVEJA SECA"              = "#C0392B",
  "ARVEJA TARRO"             = "#E74C3C",
  "GARBANZO"                 = "#F1948A",
  "LENTEJAS"                 = "#922B21",
  "PESCADO DE RÍO"           = "#FADBD8",
  "VISCERAS - HIGADO"        = "#7B241C",
  
  # Cereals & starches — oranges and ochres
  "HARINA DE TRIGO"          = "#E67E22",
  "HARINA PRECOCIDA"         = "#F39C12",
  "ARROZ PARA SECO"          = "#FAD7A0",
  "YUCA"                     = "#CA6F1E",
  
  # Fruits & vegetables — greens
  "GUAYABAS"                 = "#1E8449",
  "NARANJAS"                 = "#27AE60",
  "ZANAHORIA"                = "#A9DFBF",
  
  # Fats — yellows
  "MANTECA VEGETAL"          = "#D4AC0D",
  "ACEITE DE SOYA O DE MAÍZ" = "#F9E79F",
  
  # Sugars — purples
  "AZÚCAR NATURAL O MORENA"  = "#7D3C98",
  "PANELA"                   = "#C39BD3",
  
  # Other — grey
  "SAL"                      = "#AAB7B8")

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
  "ACEITE DE SOYA O DE MAÍZ" = "Soybean/corn oil",
  "AZÚCAR NATURAL O MORENA"  = "Brown sugar",
  "PANELA"                   = "Panela",
  "SAL"                      = "Salt")

# Order foods: by group then by mean contribution
food_order_contrib <- cost_contrib_all %>%
  group_by(Food, grupo) %>%
  dplyr::summarise(mean_contrib = mean(contrib, na.rm=TRUE), .groups="drop") %>%
  arrange(grupo, desc(mean_contrib)) %>%
  pull(Food)

cost_contrib_all <- cost_contrib_all %>%
  mutate(
    ciudad_lbl = factor(CITY_LABS[ciudad],
                        levels = c("Bogotá","Medellín","Cali")),
    Food       = factor(Food, levels = food_order_contrib))

# -----------------------------------------------------------------------
# 4f. Figure
# -----------------------------------------------------------------------
fig2b_iii <- ggplot(cost_contrib_all,
                    aes(x = fecha, y = contrib, fill = Food)) +
  geom_col(position = "stack", width = 28, alpha = 0.92) +
  # Validation line: CoNA per capita real from hcost
  geom_line(data = cona_pc_real,
            aes(x = fecha, y = cona_pc_real),
            inherit.aes = FALSE,
            color = "black", linewidth = 0.7,
            linetype = "solid") +
  geom_vline(xintercept = as.numeric(year_lines),
             color = "grey30", linewidth = 0.4,
             linetype = "dashed", alpha = 0.7) +
  facet_wrap(~ ciudad_lbl, nrow = 1) +
  scale_fill_manual(values = food_palette,
                    labels = food_label_map,
                    name   = "Food item") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y",
               expand = c(0.01, 0)) +
  scale_y_continuous(labels = comma_format(big.mark = ",")) +
  labs(
    title    = paste0("Figure 2B(iii). Monthly real cost contribution by food item ",
                      "in the CoNA diet, 2019\u20132024"),
    subtitle = paste0("All foods ever selected. Colours grouped by GABAS food category. ",
                      "Black line = CoNA real per capita from model output (validation). ",
                      "Dashed vertical lines mark year boundaries."),
    caption  = paste0(
      "Note: Cost contribution per capita = real price per 100g \u00d7 ",
      "(sum of quantities across household members / N) / 100.\n",
      "N = ", N_MEMBERS, " household members. ",
      "Real COP (base: December 2018). CoNA = Cost of Nutritional Adequacy."),
    x = NULL,
    y = "Real cost contribution (COP/day per capita)") +
  paper_theme() +
  theme(
    axis.text.x       = element_text(angle = 45, hjust = 1, size = 8),
    strip.text        = element_text(face = "bold", size = 10),
    legend.position   = "right",
    legend.text       = element_text(size = 8),
    legend.key.size   = unit(0.5, "cm"),
    panel.spacing     = unit(0.4, "cm"))

ggsave(file.path(FIG_DIR, "final", "fig02b_iii_cost_contrib.png"),
       fig2b_iii, width = 13, height = 6, dpi = 300, bg = "white")
ggsave(file.path(FIG_DIR, "final", "fig02b_iii_cost_contrib.pdf"),
       fig2b_iii, width = 13, height = 6)
message("Figure 2B-iii saved.")