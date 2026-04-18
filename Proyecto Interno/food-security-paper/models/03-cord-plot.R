########################################################
## Cost of Recommended Diet (CoRD)
## Analysis: cost series, diet composition by group
## and individual food — Q1 submission
########################################################

library(tidyverse)
library(readxl)
library(scales)
library(patchwork)

##----------------------------------------------------------
## Directories and data
##----------------------------------------------------------

dirs <- c(
  "C:/Users/Portatil/Desktop/Least-cost-diets-and-affordability/Proyecto Interno",
  "C:/Users/danie/OneDrive/Escritorio/Least-cost-diets-and-affordability/Proyecto Interno"
)

base_dir <- dirs[dir.exists(dirs)][1]

if (is.na(base_dir)) {
  stop("Ninguno de los directorios existe")
}

out_cord   <- file.path(base_dir, "food-security-paper", "output", "cord")
out_fig    <- file.path(base_dir, "food-security-paper", "output", "cord")
out_tabs   <- file.path(base_dir, "food-security-paper", "output", "cord")

cord <- readRDS(file.path(out_cord, "230326_cord_full.rds"))

df.cost <- cord$cost %>%
  mutate(fecha = as.Date(fecha), year = lubridate::year(fecha))

df.comp <- cord$comp %>%
  mutate(fecha = as.Date(fecha), year = lubridate::year(fecha))

##----------------------------------------------------------
## Shared aesthetics
##----------------------------------------------------------

city_colors <- c(
  "BOGOTA"   = "#2C3E6B",
  "MEDELLIN" = "#C0392B",
  "CALI"     = "#27AE60"
)

city_labels <- c(
  "BOGOTA"   = "Bogotá",
  "MEDELLIN" = "Medellín",
  "CALI"     = "Cali"
)

member_labels <- c(
  "0_[31,51)"  = "Adult male",
  "1_[10, 14)" = "Female child",
  "1_[31,51)"  = "Adult female"
)

member_order <- c("Adult male", "Adult female", "Female child")

group_colors <- c(
  "Cereales, raíces, tubérculos y plátanos"            = "#E67E22",
  "Frutas"                                              = "#27AE60",
  "Verduras"                                            = "#2ECC71",
  "Leche y productos lácteos"                           = "#3498DB",
  "Carnes, huevos, leguminosas, frutos secos y semillas"= "#C0392B",
  "Grasas"                                              = "#F1C40F",
  "Azúcares"                                            = "#9B59B6"
)

group_labels <- c(
  "Cereales, raíces, tubérculos y plátanos"             = "Cereals, roots & plantains",
  "Frutas"                                              = "Fruits",
  "Verduras"                                            = "Vegetables",
  "Leche y productos lácteos"                           = "Dairy",
  "Carnes, huevos, leguminosas, frutos secos y semillas"= "Meat, eggs & legumes",
  "Grasas"                                              = "Fats",
  "Azúcares"                                            = "Sugars"
)

paper_theme <- theme_minimal(base_size = 11) +
  theme(
    text             = element_text(family = "serif"),
    plot.title       = element_text(face = "bold", size = 12),
    plot.subtitle    = element_text(size = 10, color = "grey40"),
    plot.caption     = element_text(size = 8,  color = "grey50", hjust = 0),
    axis.text.x      = element_text(angle = 45, hjust = 1),
    legend.position  = "bottom",
    legend.title     = element_blank(),
    panel.grid.minor = element_blank(),
    strip.text       = element_text(face = "bold", size = 10)
  )

##----------------------------------------------------------
## Prepare: member label
##----------------------------------------------------------

df.cost <- df.cost %>%
  mutate(
    member       = recode(paste0(Sex, "_", Demo_Group), !!!member_labels),
    member       = factor(member, levels = member_order),
    ciudad_label = recode(ciudad, !!!city_labels)
  )

df.comp <- df.comp %>%
  mutate(
    member       = recode(paste0(Sex, "_", Demo_Group), !!!member_labels),
    member       = factor(member, levels = member_order),
    ciudad_label = recode(ciudad, !!!city_labels),
    Group_en     = recode(Group, !!!group_labels)
  )

##----------------------------------------------------------
## COST FIGURES
##----------------------------------------------------------

# Per capita cost (mean across members) by city × date
cost_pc <- df.cost %>%
  dplyr::group_by(ciudad, ciudad_label, fecha, year) %>%
  dplyr::summarize(
    cost_percap    = mean(cost_day,  na.rm = TRUE),
    cost_household = sum(cost_day,   na.rm = TRUE),
    .groups = "drop"
  )

##----------------------------------------------------------
## Figure 1: Per capita CoRD — time series by city
##----------------------------------------------------------

fig1 <- ggplot(cost_pc,
               aes(x = fecha, y = cost_percap, color = ciudad_label)) +
  geom_line(linewidth = 0.8) +
  geom_smooth(method = "loess", se = FALSE,
              linewidth = 0.4, linetype = "dashed") +
  scale_color_manual(values = setNames(city_colors,
                                       city_labels[names(city_colors)])) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(labels = comma_format(prefix = "$", suffix = " COP")) +
  labs(
    title    = "Figure 1. Daily per capita Cost of the Recommended Diet (CoRD), 2019–2024",
    subtitle = "Daily cost per household member (Colombian pesos, COP)",
    caption  = paste0(
      "Note: Dashed lines represent LOESS trend. CoRD estimated for a representative household\n",
      "comprising one adult male, one adult female, and one female child. Per capita cost\n",
      "computed as the mean daily cost across household members."
    ),
    x = NULL, y = "COP / day"
  ) +
  paper_theme

ggsave(file.path(out_fig, "fig1_cord_percapita_series.png"),
       fig1, width = 8, height = 4.5, dpi = 300)

##----------------------------------------------------------
## Figure 2: CoRD by household member — facet by city
##----------------------------------------------------------

fig2 <- ggplot(df.cost,
               aes(x = fecha, y = cost_day, color = ciudad_label)) +
  geom_line(linewidth = 0.7) +
  facet_wrap(~ member, ncol = 1) +
  scale_color_manual(values = setNames(city_colors,
                                       city_labels[names(city_colors)])) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(labels = comma_format(prefix = "$")) +
  labs(
    title    = "Figure 2. Daily CoRD by household member and city, 2019–2024",
    subtitle = "Daily cost of the recommended diet per member (COP)",
    caption  = "Note: Recommended diet follows GABAS dietary guidelines adjusted by city-specific EER.",
    x = NULL, y = "COP / day"
  ) +
  paper_theme

ggsave(file.path(out_fig, "fig2_cord_by_member.png"),
       fig2, width = 8, height = 9, dpi = 300)

##----------------------------------------------------------
## COMPOSITION FIGURES
##----------------------------------------------------------

##----------------------------------------------------------
## Figure 3: Stacked area — servings by food group over time
##           facet: member (rows) × city (columns)
##----------------------------------------------------------

comp_group <- df.comp %>%
  dplyr::group_by(ciudad_label, fecha, member, Group_en) %>%
  dplyr::summarize(total_servings = sum(Number_Serving, na.rm = TRUE),
            .groups = "drop")

fig3 <- ggplot(comp_group,
               aes(x = fecha, y = total_servings, fill = Group_en)) +
  geom_area(position = "stack", alpha = 0.88) +
  facet_grid(member ~ ciudad_label) +
  scale_fill_manual(values = setNames(group_colors,
                                      group_labels[names(group_colors)])) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y",
               expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(
    title    = "Figure 3. Evolution of recommended diet composition by food group, household member and city, 2019–2024",
    subtitle = "Total daily servings by food group",
    caption  = paste0(
      "Note: Stacked areas represent total daily servings allocated to each food group in the\n",
      "recommended diet. Food group labels follow GABAS classification."
    ),
    x = NULL, y = "Daily servings",
    fill = "Food group"
  ) +
  paper_theme +
  theme(
    legend.position = "bottom",
    legend.text     = element_text(size = 8),
    axis.text.x     = element_text(angle = 60, hjust = 1, size = 8)
  ) +
  guides(fill = guide_legend(nrow = 2))

ggsave(file.path(out_fig, "fig3_cord_comp_group_area.png"),
       fig3, width = 13, height = 10, dpi = 300)

##----------------------------------------------------------
## Figure 4: Heatmap — individual food × time
##           presence (any serving > 0) per city–month
##           facet: member (rows) × city (columns)
##----------------------------------------------------------

# Keep foods appearing in ≥ 10% of city-month obs to avoid clutter
top_foods <- df.comp %>%
  dplyr::group_by(Food) %>%
  dplyr::summarize(freq = n_distinct(paste(ciudad, fecha)), .groups = "drop") %>%
  mutate(share = freq / max(freq)) %>%
  filter(share >= 0.10) %>%
  arrange(Group = df.comp$Group[match(Food, df.comp$Food)], Food) %>%
  pull(Food)

comp_heat <- df.comp %>%
  filter(Food %in% top_foods) %>%
  mutate(
    present = as.integer(Number_Serving > 0),
    # Order foods by group then alphabetically
    Food = factor(Food, levels = rev(top_foods))
  )

fig4 <- ggplot(comp_heat,
               aes(x = fecha, y = Food, fill = Group_en, alpha = present)) +
  geom_tile(color = NA) +
  facet_grid(member ~ ciudad_label) +
  scale_fill_manual(values = setNames(group_colors,
                                      group_labels[names(group_colors)])) +
  scale_alpha_continuous(range = c(0, 1), guide = "none") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y",
               expand = c(0, 0)) +
  labs(
    title    = "Figure 4. Monthly presence of individual foods in the recommended diet, 2019–2024",
    subtitle = "Coloured cells indicate months in which a food item is included; colour denotes food group",
    caption  = paste0(
      "Note: Only food items present in ≥10% of city–month observations are shown.\n",
      "Cell colour indicates food group following GABAS classification."
    ),
    x = NULL, y = NULL, fill = "Food group"
  ) +
  theme_minimal(base_size = 9) +
  theme(
    text            = element_text(family = "serif"),
    plot.title      = element_text(face = "bold", size = 11),
    plot.subtitle   = element_text(size = 9, color = "grey40"),
    plot.caption    = element_text(size = 7, color = "grey50", hjust = 0),
    axis.text.x     = element_text(angle = 60, hjust = 1, size = 7),
    axis.text.y     = element_text(size = 7),
    strip.text      = element_text(face = "bold", size = 9),
    legend.position = "bottom",
    legend.text     = element_text(size = 8),
    panel.grid      = element_blank(),
    panel.spacing   = unit(0.3, "cm")
  ) +
  guides(fill = guide_legend(nrow = 2))

ggsave(file.path(out_fig, "fig4_cord_food_heatmap.png"),
       fig4, width = 14, height = 11, dpi = 300)

##----------------------------------------------------------
## Figure 5: Bar chart — food frequency across city–month obs
##           facet by household member
##----------------------------------------------------------

food_freq <- df.comp %>%
  filter(Food %in% top_foods) %>%
  dplyr::group_by(Food, Group_en, member, ciudad_label) %>%
  dplyr::summarize(
    freq     = mean(Number_Serving > 0, na.rm = TRUE),
    .groups  = "drop"
  ) %>%
  mutate(Food = fct_reorder(Food, freq, .fun = mean, .desc = FALSE))

fig5 <- ggplot(food_freq,
               aes(x = freq, y = Food, fill = Group_en)) +
  geom_col(position = position_dodge(width = 0.7),
           width = 0.65, alpha = 0.9) +
  facet_grid(member ~ ciudad_label) +
  scale_fill_manual(values = setNames(group_colors,
                                      group_labels[names(group_colors)])) +
  scale_x_continuous(labels = percent_format(accuracy = 1),
                     limits = c(0, 1)) +
  labs(
    title    = "Figure 5. Frequency of individual food items in the recommended diet by member and city, 2019–2024",
    subtitle = "Share of city–month observations in which each food item is included",
    caption  = paste0(
      "Note: Only food items present in ≥10% of city–month observations are shown.\n",
      "Bars represent the share of months in which a food item is part of the recommended diet."
    ),
    x = "Share of observations", y = NULL, fill = "Food group"
  ) +
  paper_theme +
  theme(
    axis.text.y     = element_text(size = 8),
    legend.text     = element_text(size = 8),
    panel.spacing   = unit(0.3, "cm")
  ) +
  guides(fill = guide_legend(nrow = 2))

ggsave(file.path(out_fig, "fig5_cord_food_frequency.png"),
       fig5, width = 14, height = 11, dpi = 300)

message("Figures 1–5 saved to: ", out_fig)

##----------------------------------------------------------
## Summary table: mean annual cost by member × city
##----------------------------------------------------------

tab_cost <- df.cost %>%
  dplyr::group_by(year, member, ciudad_label) %>%
  dplyr::summarize(mean_cost = mean(cost_day, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = ciudad_label, values_from = mean_cost) %>%
  arrange(member, year) %>%
  mutate(across(where(is.numeric), ~ round(.x, 1)))

writexl::write_xlsx(tab_cost,
                    file.path(out_tabs, "tableA2_cord_cost_annual.xlsx"))

message("Table saved to: ", out_tabs)