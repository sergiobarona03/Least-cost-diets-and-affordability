########################################################
## Least-cost diets — Results section
## Figure 1: Real daily cost per member
## Figure 2: Real cost per 1,000 kcal per member
########################################################

library(tidyverse)
library(readxl)
library(lubridate)
library(scales)

##----------------------------------------------------------
## Directories
##----------------------------------------------------------

base_dir <- "C:\\Users\\Portatil\\Desktop\\Least-cost-diets-and-affordability\\Proyecto Interno"
out_real <- file.path(base_dir, "food-security-paper", "output", "real")
out_fig  <- file.path(base_dir, "food-security-paper", "output", "figures")

##----------------------------------------------------------
## Load data
##----------------------------------------------------------

coca_real <- read_excel(file.path(out_real, "coca_real.xlsx")) %>%
  select(ciudad, fecha, Demo_Group, Sex, cost_day, Cost_1000kcal) %>%
  mutate(diet = "CoCA")

cona_real <- read_excel(file.path(out_real, "cona_real.xlsx")) %>%
  select(ciudad, fecha, Demo_Group, Sex, cost_day, Cost_1000kcal) %>%
  mutate(diet = "CoNA")

cord_real <- read_excel(file.path(out_real, "cord_real.xlsx")) %>%
  select(ciudad, fecha, Demo_Group, Sex, cost_day, Cost_1000kcal) %>%
  mutate(diet = "CoRD")

diets_real <- bind_rows(coca_real, cona_real, cord_real) %>%
  mutate(
    fecha        = as.Date(fecha),
    member       = case_when(
      Sex == 0 & Demo_Group == "[31,51)"  ~ "Adult male",
      Sex == 1 & Demo_Group == "[31,51)"  ~ "Adult female",
      Sex == 1 & Demo_Group == "[10, 14)" ~ "Female child"
    ),
    member       = factor(member,
                          levels = c("Adult male", "Adult female", "Female child")),
    ciudad_label = recode(ciudad,
                          "BOGOTA"   = "Bogotá",
                          "MEDELLIN" = "Medellín",
                          "CALI"     = "Cali"),
    diet         = factor(diet, levels = c("CoCA", "CoNA", "CoRD"))
  )

##----------------------------------------------------------
## Points every 3 months — subset for geom_point
##----------------------------------------------------------

diets_points <- diets_real %>%
  filter(month(fecha) %in% c(12))   # Jan, Apr, Jul, Oct

##----------------------------------------------------------
## Aesthetics
##----------------------------------------------------------

# Diet palette — consistent with project color scheme
diet_colors <- c(
  "CoCA" = "#2C3E6B",   # dark blue
  "CoNA" = "#C0392B",   # red
  "CoRD" = "#27AE60"    # green
)

diet_linetypes <- c(
  "CoCA" = "solid",
  "CoNA" = "dashed",
  "CoRD" = "dotdash"
)

# Distinct shapes — readable in greyscale and print
diet_shapes <- c(
  "CoCA" = 16,   # filled circle
  "CoNA" = 17,   # filled triangle
  "CoRD" = 15    # filled square
)

# Q1 journal theme — clean, serif, minimal grid
theme_q1 <- theme_bw(base_size = 10) +
  theme(
    text             = element_text(family = "serif"),
    plot.title       = element_text(face = "bold", size = 11,
                                    margin = margin(b = 4)),
    plot.subtitle    = element_text(size = 9, color = "grey35",
                                    margin = margin(b = 6)),
    plot.caption     = element_text(size = 7.5, color = "grey45",
                                    hjust = 0, margin = margin(t = 6)),
    axis.title       = element_text(size = 9),
    axis.text        = element_text(size = 8),
    axis.text.x      = element_text(angle = 45, hjust = 1),
    legend.position  = "bottom",
    legend.title     = element_blank(),
    legend.text      = element_text(size = 9),
    legend.key.width = unit(1.2, "cm"),
    panel.grid.major = element_line(color = "grey90", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "grey96", color = "grey70"),
    strip.text       = element_text(face = "bold", size = 9),
    plot.margin      = margin(6, 8, 6, 6)
  )

##----------------------------------------------------------
## Figure 1: Real daily cost per member × city
##----------------------------------------------------------

fig1 <- ggplot(diets_real,
               aes(x     = fecha,
                   y     = cost_day,
                   color = diet,
                   linetype = diet,
                   shape    = diet)) +
  geom_line(linewidth = 0.55, alpha = 0.9) +
  geom_point(data = diets_points, size = 1.6, alpha = 0.9) +
  facet_grid(ciudad_label ~ member) +
  scale_color_manual(values    = diet_colors)    +
  scale_linetype_manual(values = diet_linetypes) +
  scale_shape_manual(values    = diet_shapes)    +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y",
               expand = c(0.01, 0)) +
  scale_y_continuous(labels = comma_format(prefix   = "$",
                                           big.mark = ",",
                                           suffix   = "")) +
  labs(
    title    = paste0("Figure 1. Real daily diet cost for each member of the ",
                      "representative household, 2019\u20132024"),
    subtitle = "Colombian pesos per day, deflated by national CPI (base year 2019)",
    caption  = paste0(
      "Note: CoCA = Cost of Caloric Adequacy; CoNA = Cost of Nutritional Adequacy; ",
      "CoRD = Cost of the Recommended Diet.\n",
      "The representative household comprises one adult male (31\u201351 years), one adult female ",
      "(31\u201351 years), and one female child (10\u201314 years).\n",
      "Nutritional requirements follow GABAS dietary guidelines adjusted by ",
      "city-specific estimated energy requirements (EER)."
    ),
    x = NULL,
    y = "Real COP / day"
  ) +
  theme_q1

ggsave(file.path(out_fig, "fig1_real_daily_cost_by_member.png"),
       fig1, width = 9, height = 7, dpi = 300)

ggsave(file.path(out_fig, "fig1_real_daily_cost_by_member.pdf"),
       fig1, width = 9, height = 7)

##----------------------------------------------------------
## Figure 2: Real cost per 1,000 kcal per member × city
##----------------------------------------------------------

# Period mean per diet × member × city — dotted horizontal reference
means_1000kcal <- diets_real %>%
  group_by(diet, member, ciudad_label) %>%
  summarize(mean_cost = mean(Cost_1000kcal, na.rm = TRUE), .groups = "drop")

fig2 <- ggplot(diets_real,
               aes(x        = fecha,
                   y        = Cost_1000kcal,
                   color    = diet,
                   linetype = diet,
                   shape    = diet)) +
  geom_line(linewidth = 0.55, alpha = 0.9) +
  geom_point(data = diets_points, size = 1.6, alpha = 0.9) +
  geom_hline(data        = means_1000kcal,
             aes(yintercept = mean_cost, color = diet),
             linetype    = "dotted",
             linewidth   = 0.6,
             alpha       = 0.75,
             inherit.aes = FALSE) +
  facet_grid(ciudad_label ~ member) +
  scale_color_manual(values    = diet_colors)    +
  scale_linetype_manual(values = diet_linetypes) +
  scale_shape_manual(values    = diet_shapes)    +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y",
               expand = c(0.01, 0)) +
  scale_y_continuous(labels = comma_format(prefix   = "$",
                                           big.mark = ",",
                                           suffix   = "")) +
  labs(
    title    = paste0("Figure 2. Real diet cost per 1,000 kcal for each member of the ",
                      "representative household, 2019\u20132024"),
    subtitle = "Colombian pesos per 1,000 kcal, deflated by national CPI (base year 2019)",
    caption  = paste0(
      "Note: Cost per 1,000 kcal is a diet-quality-adjusted cost measure that controls for ",
      "differences in energy density across diets and household members.\n",
      "Dotted horizontal lines indicate the 2019\u20132024 period mean for each dietary model.\n",
      "CoCA = Cost of Caloric Adequacy; CoNA = Cost of Nutritional Adequacy; ",
      "CoRD = Cost of the Recommended Diet.\n",
      "Nutritional requirements follow GABAS dietary guidelines adjusted by ",
      "city-specific estimated energy requirements (EER)."
    ),
    x = NULL,
    y = "Real COP / 1,000 kcal"
  ) +
  theme_q1

ggsave(file.path(out_fig, "fig2_real_cost_1000kcal_by_member.png"),
       fig2, width = 9, height = 7, dpi = 300)

ggsave(file.path(out_fig, "fig2_real_cost_1000kcal_by_member.pdf"),
       fig2, width = 9, height = 7)

message("Figures 1\u20132 saved to: ", out_fig)