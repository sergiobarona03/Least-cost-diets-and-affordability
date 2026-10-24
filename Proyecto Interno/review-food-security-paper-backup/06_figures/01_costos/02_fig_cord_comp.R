########################################################
## 05_figures/bloque1_costos/fig02c_cord_composition.R
##
## Figure 2C: CoRD composition by GABA food group
##   Stacked bar: annual mean servings × group × city
##   Shows structural stability across cities and time
##
## Reads:  CORD_DIR/cord_results.rds
##
## Writes: FIG_DIR/final/fig02c_cord_composition.png / .pdf
########################################################

#source("00_config.R")
source(file.path(REVIEW_DIR, "06_figures", "00_fig_config.R"))
library(tidyverse)
library(lubridate)
library(scales)

# -----------------------------------------------------------------------
# 0. Local city maps — defined here to avoid relying on
#    CITY_LABS / CITY_LABELS / CITY_COLS from other config files
# -----------------------------------------------------------------------
city_lbl_map <- c(
  "BOGOTA"   = "Bogotá",
  "MEDELLIN" = "Medellín",
  "CALI"     = "Cali")

city_col_map <- c(
  "Bogotá"   = unname(CITY_COLORS["Bogotá"]),
  "Medellín" = unname(CITY_COLORS["Medellín"]),
  "Cali"     = unname(CITY_COLORS["Cali"]))

# -----------------------------------------------------------------------
# 1. Load and prepare
# -----------------------------------------------------------------------
cord       <- readRDS(file.path(CORD_DIR, "cord_results.rds"))
data_paper <- readRDS(file.path(PREP_DIR, "panel_food_paper.rds"))
deflator   <- readRDS(file.path(PREP_DIR, "deflator_monthly.rds")) %>%
  filter(ciudad != "NACIONAL") %>%
  select(ciudad, fecha, deflator)

GROUP_LABS <- c(
  "Grasas"                                                       = "Fats",
  "Leche y productos lácteos"                                    = "Dairy",
  "Carnes, huevos, leguminosas, frutos secos y semillas"         = "Meat, eggs & legumes",
  "Azúcares"                                                     = "Sugars",
  "Cereales, raíces, tubérculos y plátanos"                      = "Cereals & starches",
  "Frutas"                                                       = "Fruits",
  "Verduras"                                                     = "Vegetables")

GROUP_COLS <- c(
  "Fats"                 = "#F1C40F",
  "Dairy"                = "#3498DB",
  "Meat, eggs & legumes" = "#C0392B",
  "Sugars"               = "#9B59B6",
  "Cereals & starches"   = "#E67E22",
  "Fruits"               = "#27AE60",
  "Vegetables"           = "#2ECC71")

# Year boundary lines
year_lines <- as.Date(paste0(2020:2024, "-01-01"))

comp_monthly <- cord$comp %>%
  mutate(
    fecha      = as.Date(fecha),
    ciudad_lbl = factor(city_lbl_map[ciudad],
                        levels = c("Bogotá","Medellín","Cali"))) %>%
  filter(fecha >= PAPER_START, fecha <= PAPER_END) %>%
  group_by(ciudad_lbl, Group, fecha) %>%
  dplyr::summarise(
    mean_servings = mean(Number_Serving, na.rm=TRUE),
    .groups       = "drop") %>%
  mutate(
    Group_en = recode(Group, !!!GROUP_LABS),
    Group_en = factor(Group_en,
                      levels = names(GROUP_COLS)))

# -----------------------------------------------------------------------
# 2. Figure
# -----------------------------------------------------------------------
fig2c <- ggplot(comp_monthly,
                aes(x = fecha,
                    y = mean_servings,
                    fill = Group_en)) +
  geom_col(position = "stack", alpha = 0.9, width = 28) +
  geom_vline(xintercept = as.numeric(year_lines),
             color = "grey30", linewidth = 0.4,
             linetype = "dashed", alpha = 0.7) +
  facet_wrap(~ ciudad_lbl, nrow = 1) +
  scale_fill_manual(values = GROUP_COLS, name = "Food group") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y",
               expand = c(0.01, 0)) +
  scale_y_continuous(labels = number_format(accuracy = 0.1)) +
  labs(
    title    = " ",
    subtitle = " ",
    x = NULL,
    y = "Mean daily servings per member") +
  paper_theme() +
  theme(
    axis.text.x     = element_text(angle = 45, hjust = 1, size = 8),
    strip.text      = element_text(face = "bold", size = 10),
    legend.position = "right",
    legend.text     = element_text(size = 9),
    panel.spacing   = unit(0.4, "cm"))

ggsave(file.path(FIG_DIR, "final", "fig02c_cord_servings.png"),
       fig2c, width = 12, height = 6, dpi = 300, bg = "white")
ggsave(file.path(FIG_DIR, "final", "fig02c_cord_servings.pdf"),
       fig2c, width = 12, height = 6)

# -----------------------------------------------------------------------
# 3. Panel B: real cost contribution by food group × city × year
# -----------------------------------------------------------------------
# Join comp with real prices from data_paper
cord_cost <- cord$comp %>%
  mutate(fecha = as.Date(fecha)) %>%
  filter(fecha >= PAPER_START, fecha <= PAPER_END) %>%
  left_join(
    data_paper %>%
      mutate(fecha = as.Date(fecha)) %>%
      select(ciudad, fecha, articulo, precio_100g,
             gramos_g_1_intercambio_1_intercambio) %>%
      rename(Food = articulo, Serving_g = gramos_g_1_intercambio_1_intercambio),
    by = c("ciudad","fecha","Food")) %>%
  left_join(deflator, by = c("ciudad","fecha")) %>%
  mutate(
    cost_contrib  = (precio_100g * deflator) * Serving_g / 100 * Number_Serving,
    ciudad_lbl    = factor(city_lbl_map[ciudad],
                           levels = c("Bogotá","Medellín","Cali")),
    Group_en      = recode(Group, !!!GROUP_LABS),
    Group_en      = factor(Group_en, levels = names(GROUP_COLS))) %>%
  filter(!is.na(cost_contrib))

cost_contrib_monthly <- cord_cost %>%
  group_by(ciudad_lbl, Group_en, fecha) %>%
  dplyr::summarise(
    mean_cost = mean(cost_contrib, na.rm=TRUE),
    .groups   = "drop")

fig2c_cost <- ggplot(cost_contrib_monthly,
                     aes(x = fecha, y = mean_cost,
                         fill = Group_en)) +
  geom_col(position = "stack", alpha = 0.9, width = 28) +
  geom_vline(xintercept = as.numeric(year_lines),
             color = "grey30", linewidth = 0.4,
             linetype = "dashed", alpha = 0.7) +
  facet_wrap(~ ciudad_lbl, nrow = 1) +
  scale_fill_manual(values = GROUP_COLS, name = "Food group") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y",
               expand = c(0.01, 0)) +
  scale_y_continuous(labels = comma_format(big.mark = ",")) +
  labs(
    title    = " ",
    subtitle = " ",
    x = NULL,
    y = "Real cost contribution (COP/day)") +
  paper_theme() +
  theme(
    axis.text.x     = element_text(angle = 45, hjust = 1, size = 8),
    strip.text      = element_text(face = "bold", size = 10),
    legend.position = "right",
    legend.text     = element_text(size = 9),
    panel.spacing   = unit(0.4, "cm"))

ggsave(file.path(FIG_DIR, "final", "fig02c_cord_cost.png"),
       fig2c_cost, width = 12, height = 6, dpi = 300, bg = "white")
ggsave(file.path(FIG_DIR, "final", "fig02c_cord_cost.pdf"),
       fig2c_cost, width = 12, height = 6)

message("Figure 2C saved (servings + cost contribution).")

# -----------------------------------------------------------------------
# 4. Nutritional adequacy of CoRD vs CoNA requirements
#    For each member: calculate CoRD nutrient supply and compare
#    against CoNA requirements (Rest column in cona$limit)
# -----------------------------------------------------------------------

# 4a. Nutrient requirements from CoNA — average across cities and time
requirements <- cona$limit %>%
  mutate(fecha = as.Date(fecha)) %>%
  filter(fecha >= PAPER_START, fecha <= PAPER_END) %>%
  group_by(Nutrients, Age, Sex) %>%
  dplyr::summarise(
    requirement = mean(Rest, na.rm = TRUE),
    .groups     = "drop")

# 4b. Nutrient map: CoNA names -> data_paper columns
nutrient_map <- c(
  "Calcium"      = "calcio_mg",
  "Carbohydrates"= "carbohidratos_totales_g",
  "Folate"       = "folatos_mcg",
  "Iron"         = "hierro_mg",
  "Lipids"       = "lipidos_g",
  "Magnesium"    = "magnesio_mg",
  "Niacin"       = "niacina_mg",
  "Phosphorus"   = "fosforo_mg",
  "Protein"      = "proteina_g",
  "Riboflavin"   = "riboflavina_mg",
  "Sodium"       = "sodio_mg",
  "Thiamine"     = "tiamina_mg",
  "VitaminA"     = "vitamina_a_er",
  "VitaminB12"   = "vitamina_b12_mcg",
  "VitaminC"     = "vitamina_c_mg",
  "Zinc"         = "zinc_mg")

# 4c. Mean nutritional values per food × city (avg across time to avoid many-to-many)
nutri_per_food <- data_paper %>%
  mutate(fecha = as.Date(fecha)) %>%
  filter(fecha >= PAPER_START, fecha <= PAPER_END) %>%
  group_by(ciudad, articulo) %>%
  dplyr::summarise(
    across(all_of(unname(nutrient_map)),
           ~ mean(.x, na.rm = TRUE)),
    gramos_intercambio = mean(gramos_g_1_intercambio_1_intercambio,
                              na.rm = TRUE),
    .groups = "drop") %>%
  rename(Food = articulo)

# 4d. CoRD nutrient supply per member × city × month
cord_nutri <- cord$comp %>%
  mutate(fecha = as.Date(fecha)) %>%
  filter(fecha >= PAPER_START, fecha <= PAPER_END) %>%
  left_join(nutri_per_food, by = c("ciudad","Food")) %>%
  mutate(gramos_totales = Number_Serving * gramos_intercambio) %>%
  # Calculate supply for each nutrient
  mutate(across(all_of(unname(nutrient_map)),
                ~ .x * gramos_totales / 100,
                .names = "supply_{.col}")) %>%
  group_by(ciudad, Demo_Group, Sex, fecha) %>%
  dplyr::summarise(
    across(starts_with("supply_"), ~ sum(.x, na.rm = TRUE)),
    .groups = "drop")

# Rename supply columns to nutrient names
# Step 1: strip supply_ prefix
names(cord_nutri) <- gsub("^supply_", "", names(cord_nutri))
# Step 2: map data_paper column names -> CoNA nutrient names
reverse_map <- setNames(names(nutrient_map), unname(nutrient_map))
names(cord_nutri) <- ifelse(
  names(cord_nutri) %in% names(reverse_map),
  reverse_map[names(cord_nutri)],
  names(cord_nutri))

# 4e. Join with requirements and compute adequacy ratio
# Fix Sex type mismatch: cord_nutri has character, requirements has double
cord_adequacy <- cord_nutri %>%
  mutate(Sex = as.double(Sex)) %>%
  pivot_longer(cols = all_of(names(nutrient_map)),
               names_to  = "Nutrients",
               values_to = "supply") %>%
  left_join(requirements,
            by = c("Nutrients",
                   "Demo_Group" = "Age",
                   "Sex")) %>%
  mutate(
    adequacy_pct = supply / requirement * 100,
    ciudad_lbl   = factor(city_lbl_map[ciudad],
                          levels = c("Bogotá","Medellín","Cali"))) %>%
  filter(!is.na(requirement))

# 4f. Adequacy ratio: mean supply / requirement × 100
# Exclude Sodium — it is a maximum constraint in CoNA, not a minimum
cord_adequacy <- cord_adequacy %>%
  filter(Nutrients != "Sodium")

# Mean adequacy ratio per nutrient × member × city × year
adequacy_ratio <- cord_adequacy %>%
  mutate(
    ratio      = supply / requirement * 100,
    year       = year(fecha),
    ciudad_lbl = factor(city_lbl_map[ciudad],
                        levels = c("Bogotá","Medellín","Cali"))) %>%
  group_by(ciudad_lbl, Nutrients, Demo_Group, Sex, year) %>%
  dplyr::summarise(
    mean_ratio = mean(ratio, na.rm=TRUE),
    .groups    = "drop")

# Median ratio by city × year — for annotation
ratio_median <- adequacy_ratio %>%
  group_by(ciudad_lbl, year) %>%
  dplyr::summarise(
    median_ratio = round(median(mean_ratio, na.rm=TRUE), 1),
    .groups      = "drop")

# By nutrient × city — for interpretation
ratio_by_nutrient <- adequacy_ratio %>%
  group_by(ciudad_lbl, Nutrients) %>%
  dplyr::summarise(
    mean_ratio = round(mean(mean_ratio, na.rm=TRUE), 1),
    .groups    = "drop") %>%
  filter(mean_ratio < 150) %>%   # show only where there is variation
  arrange(ciudad_lbl, mean_ratio)

cat("\n--- Mean adequacy ratio by nutrient x city (< 150%) ---\n")
print(ratio_by_nutrient, n = 40)
cat("\n--- Median adequacy ratio by city and year ---\n")
print(ratio_median %>% arrange(ciudad_lbl, year), n = 30)

# 4g. Figure: boxplot of adequacy ratio by year × city
# Each obs = one nutrient × member combination
# Cap display at 300% to avoid extreme values dominating
adequacy_ratio_plot <- adequacy_ratio %>%
  mutate(mean_ratio_cap = pmin(mean_ratio, 300))

fig2c_adequacy <- ggplot(adequacy_ratio_plot,
                         aes(x = factor(year),
                             y = mean_ratio_cap,
                             fill = ciudad_lbl)) +
  geom_boxplot(
    alpha         = 0.75,
    outlier.size  = 0.8,
    outlier.alpha = 0.5,
    linewidth     = 0.4) +
  geom_hline(yintercept = 100,
             color     = "#C0392B",
             linetype  = "dashed",
             linewidth = 0.5) +
  # Median annotation below x-axis
  geom_text(data = ratio_median,
            aes(x     = factor(year),
                y     = -18,
                label = paste0(median_ratio, "%")),
            size        = 2.5,
            family      = "serif",
            color       = "grey30",
            inherit.aes = FALSE) +
  facet_wrap(~ ciudad_lbl, nrow = 1) +
  scale_fill_manual(
    values = city_col_map,
    guide = "none") +
  scale_y_continuous(
    limits = c(-22, 310),
    breaks = c(0, 50, 100, 150, 200, 250, 300),
    labels = function(x) ifelse(x < 0, "", paste0(x, "%"))) +
  labs(
    title    = " ",
    subtitle = " ",
    x = NULL,
    y = "Adequacy ratio (supply / requirement \u00d7 100)") +
  paper_theme() +
  theme(
    axis.text.x     = element_text(angle = 45, hjust = 1, size = 8),
    strip.text      = element_text(face = "bold", size = 10),
    panel.spacing   = unit(0.4, "cm"),
    legend.position = "none")

ggsave(file.path(FIG_DIR, "final", "fig02c_adequacy.png"),
       fig2c_adequacy, width = 13, height = 6, dpi = 300, bg = "white")
ggsave(file.path(FIG_DIR, "final", "fig02c_adequacy.pdf"),
       fig2c_adequacy, width = 13, height = 6)

message("Figure 2C(iii) — nutritional adequacy ratio — saved.")

# -----------------------------------------------------------------------
# 5. Figure 2C(iv): Adequacy relative to BOTH lower and upper limits
#    Shows whether CoRD supply falls within the safe range [LL, UL]
#    UL = 9999999 means no upper limit — treated as unbounded
# -----------------------------------------------------------------------

# 5a. Load upper limits
household_ul <- readRDS(file.path(PREP_DIR, "household_ul.rds"))

# 5b. Pivot UL to long format matching nutrient names
ul_long <- household_ul %>%
  select(-cod_mun) %>%
  pivot_longer(cols      = -c(Sex, Age, ciudad),
               names_to  = "Nutrients",
               values_to = "ul") %>%
  mutate(
    ul  = ifelse(ul >= 9999999, NA_real_, ul),  # NA = no upper limit
    Sex = as.double(Sex))

# 5c. Load lower limits from requirements (already computed above)
# requirements has: Nutrients, Age, Sex, requirement (= LL)

# 5d. Join supply with both LL and UL
cord_adequacy_full <- cord_nutri %>%
  mutate(Sex = as.double(Sex)) %>%
  pivot_longer(cols      = all_of(names(nutrient_map)),
               names_to  = "Nutrients",
               values_to = "supply") %>%
  left_join(requirements,
            by = c("Nutrients", "Demo_Group" = "Age", "Sex")) %>%
  left_join(ul_long,
            by = c("ciudad", "Nutrients",
                   "Demo_Group" = "Age", "Sex")) %>%
  filter(!is.na(requirement)) %>%
  # Exclude Sodium: in CoNA it is a maximum constraint (not minimum)
  # CoRD supply well below the sodium cap is desirable, not a deficiency
  filter(Nutrients != "Sodium") %>%
  mutate(
    # Ratio vs lower limit
    ratio_ll   = supply / requirement * 100,
    # Ratio vs upper limit (only where UL exists)
    ratio_ul   = ifelse(!is.na(ul), supply / ul * 100, NA_real_),
    # Status: below LL, within range, above UL
    status = case_when(
      supply < requirement        ~ "Below minimum",
      !is.na(ul) & supply > ul   ~ "Above maximum",
      TRUE                        ~ "Within range"),
    year       = year(fecha),
    ciudad_lbl = factor(city_lbl_map[ciudad],
                        levels = c("Bogotá","Medellín","Cali")))

# 5e. Summary by nutrient × city × year
status_summary <- cord_adequacy_full %>%
  group_by(ciudad_lbl, Nutrients, year) %>%
  dplyr::summarise(
    pct_below = round(mean(status == "Below minimum") * 100, 1),
    pct_within= round(mean(status == "Within range")  * 100, 1),
    pct_above = round(mean(status == "Above maximum") * 100, 1),
    .groups   = "drop")

# Annual summary across all nutrients
status_annual <- cord_adequacy_full %>%
  group_by(ciudad_lbl, year) %>%
  dplyr::summarise(
    pct_below  = round(mean(status == "Below minimum") * 100, 1),
    pct_within = round(mean(status == "Within range")  * 100, 1),
    pct_above  = round(mean(status == "Above maximum") * 100, 1),
    .groups    = "drop")

cat("\n--- Annual status distribution by city ---\n")
print(status_annual %>% arrange(ciudad_lbl, year), n = 30)

# 5f. Figure: stacked bar of status by YEAR × city
# Each bar sums to 100% — shows % of nutrient-member-month obs in each status

status_plot <- cord_adequacy_full %>%
  mutate(status = factor(status,
                         levels = c("Below minimum",
                                    "Within range",
                                    "Above maximum"))) %>%
  group_by(ciudad_lbl, year, status) %>%
  dplyr::summarise(n = n(), .groups = "drop") %>%
  group_by(ciudad_lbl, year) %>%
  mutate(pct = n / sum(n) * 100) %>%
  ungroup()

status_cols <- c(
  "Below minimum" = "#C0392B",
  "Within range"  = "#27AE60",
  "Above maximum" = "#2C3E6B")

fig2c_status <- ggplot(status_plot,
                       aes(x = factor(year), y = pct,
                           fill = status)) +
  geom_col(position = "stack", width = 0.75, alpha = 0.9) +
  facet_wrap(~ ciudad_lbl, nrow = 1) +
  scale_fill_manual(values = status_cols, name = NULL) +
  scale_y_continuous(labels = function(x) paste0(x, "%"),
                     breaks = c(0, 25, 50, 75, 100)) +
  labs(
    title    = " ",
    subtitle = " ",
    x = NULL,
    y = "% of nutrient-member-month observations") +
  paper_theme() +
  theme(
    axis.text.x      = element_text(angle = 45, hjust = 1, size = 8),
    strip.text       = element_text(face = "bold", size = 10),
    legend.position  = "bottom",
    legend.direction = "horizontal",
    legend.text      = element_text(family = "serif", size = 9),
    legend.background = element_rect(color = "black", fill = "white",
                                     linewidth = 0.5),
    legend.margin    = margin(3, 8, 3, 8),
    panel.spacing    = unit(0.4, "cm"))

ggsave(file.path(FIG_DIR, "final", "fig02c_status.png"),
       fig2c_status, width = 12, height = 6, dpi = 300, bg = "white")
ggsave(file.path(FIG_DIR, "final", "fig02c_status.pdf"),
       fig2c_status, width = 12, height = 6)

message("Figure 2C(iv) — nutritional status (LL + UL) — saved.")