########################################################
## Cost of Recommended Diet (CoRD)
## Figure 6: Food group cost shares
## 100% stacked area (time series) + donut (summary)
########################################################

library(tidyverse)
library(readxl)
library(scales)
library(patchwork)
library(openxlsx)

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

out_cord <- file.path(base_dir, "food-security-paper", "output", "cord")
out_fig  <- file.path(base_dir, "food-security-paper", "output", "figures")
input1_dir <- file.path(base_dir, "food-security-paper", "output", "tcac_food_table")

cord <- readRDS(file.path(out_cord, "230326_cord_full.rds"))

df.comp <- cord$comp %>%
  mutate(fecha = as.Date(fecha), year = lubridate::year(fecha))

##----------------------------------------------------------
## Recover cost per food item from data_paper
## (Price_serving × Number_Serving = daily cost contribution)
##----------------------------------------------------------

data_paper_prices <- readRDS(
  file.path(input1_dir, "panel_city_month_food_1999_2025.rds")
) %>%
  dplyr::select(ciudad, fecha, articulo,
         grupos_gabas, subgrupos_gabas,
         precio_100g,
         gramos_g_1_intercambio_1_intercambio) %>%
  distinct() %>%
  filter(fecha >= "2019-01-01", fecha < "2025-01-01") %>%
  dplyr::rename(
    Food      = articulo,
    Serving_g = gramos_g_1_intercambio_1_intercambio,
    Group     = grupos_gabas
  ) %>%
  mutate(
    fecha         = as.Date(fecha),
    Price_serving = precio_100g * Serving_g / 100,
    Group = if_else(subgrupos_gabas == "FRUTAS",   "FRUTAS",   Group),
    Group = if_else(subgrupos_gabas == "VERDURAS", "VERDURAS", Group)
  )

##----------------------------------------------------------
## Shared aesthetics
##----------------------------------------------------------

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
  "Cereals, roots & plantains" = "#E67E22",
  "Fruits"                     = "#27AE60",
  "Vegetables"                 = "#2ECC71",
  "Dairy"                      = "#3498DB",
  "Meat, eggs & legumes"       = "#C0392B",
  "Fats"                       = "#F1C40F",
  "Sugars"                     = "#9B59B6"
)

group_labels <- c(
  "Cereales, raíces, tubérculos y plátanos"             = "Cereals, roots & plantains",
  "FRUTAS"                                              = "Fruits",
  "Frutas"                                              = "Fruits",
  "VERDURAS"                                            = "Vegetables",
  "Verduras"                                            = "Vegetables",
  "Leche y productos lácteos"                           = "Dairy",
  "Carnes, huevos, leguminosas, frutos secos y semillas"= "Meat, eggs & legumes",
  "Grasas"                                              = "Fats",
  "Azúcares"                                            = "Sugars"
)

paper_theme <- theme_bw(base_size = 11) +
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
## Build cost shares
## Join comp with prices to get cost per food × date × city
##----------------------------------------------------------

comp_cost <- df.comp %>%
  left_join(
    data_paper_prices %>% dplyr::select(ciudad, fecha, Food, Price_serving, Group),
    by = c("ciudad", "fecha", "Food")
  ) %>%
  mutate(
    # Cost contribution = price per serving × number of servings
    cost_contrib = Price_serving * Number_Serving,
    member       = recode(paste0(Sex, "_", Demo_Group), !!!member_labels),
    member       = factor(member, levels = member_order),
    ciudad_label = recode(ciudad, !!!city_labels),
    Group_en     = recode(Group.x, !!!group_labels)   # Group.x from comp
  )

# Cost share by group × member × city × date
shares_time <- comp_cost %>%
  dplyr::group_by(ciudad_label, fecha, member, Group_en) %>%
  dplyr::summarize(cost_group = sum(cost_contrib, na.rm = TRUE), .groups = "drop") %>%
  dplyr::group_by(ciudad_label, fecha, member) %>%
  mutate(share = cost_group / sum(cost_group)) %>%
  ungroup()

# Mean share over entire period for donut
shares_mean <- comp_cost %>%
  dplyr::group_by(Group_en) %>%
  dplyr::summarize(cost_group = sum(cost_contrib, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    share     = cost_group / sum(cost_group),
    share_pct = round(share * 100, 1),
    label     = if_else(share_pct >= 5,
                        paste0(Group_en, "\n", share_pct, "%"), "")
  )

##----------------------------------------------------------
## Panel A: 100% stacked area — cost share over time
##          facet: member (rows) × city (columns)
##----------------------------------------------------------

panel_a <- ggplot(shares_time,
                  aes(x = fecha, y = share, fill = Group_en)) +
  geom_area(position = "fill", alpha = 0.88) +
  facet_grid(member ~ ciudad_label) +
  scale_fill_manual(values = group_colors) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y",
               expand = c(0, 0)) +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     expand = c(0, 0)) +
  labs(
    x = NULL, y = "Share of daily diet cost",
    fill = "Food group"
  ) +
  paper_theme +
  theme(
    axis.text.x     = element_text(angle = 60, hjust = 1, size = 8),
  )

##----------------------------------------------------------
## Panel B: Donut chart — mean cost share over full period
##----------------------------------------------------------

# Donut coordinates
shares_mean <- shares_mean %>%
  arrange(desc(share)) %>%
  mutate(
    ymax  = cumsum(share),
    ymin  = lag(ymax, default = 0),
    ymid  = (ymin + ymax) / 2
  )

panel_b <- ggplot(shares_mean,
                  aes(ymin = ymin, ymax = ymax,
                      xmin = 3, xmax = 4.5, fill = Group_en)) +
  geom_rect(alpha = 0.9, color = "white", linewidth = 0.4) +
  geom_text(aes(x = 4.9, y = ymid, label = label),
            size = 2.8, family = "serif", lineheight = 0.9,
            hjust = 0) +
  coord_polar(theta = "y") +
  xlim(c(1, 6)) +
  scale_fill_manual(values = group_colors) +
  labs(
    x = NULL, y = NULL
  ) +
  theme_void(base_size = 10) +
  theme(
    text            = element_text(family = "serif"),
    plot.subtitle   = element_text(face = "bold", size = 9,
                                   hjust = 0.5, color = "grey30"),
  )

##----------------------------------------------------------
## Save Figure
##----------------------------------------------------------

ggsave(file.path(out_fig, "fig6_cord_cost_shares.png"),
       panel_a, width = 10, height = 6, dpi = 300)