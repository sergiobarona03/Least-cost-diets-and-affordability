########################################################
## Least-cost diets — Results section
## Figure 1: Real daily cost per member (CoCA, CoNA, CoRD)
## Figure 2: Real cost per 1,000 kcal — CoNA only, by member
##           (Cost per 1,000 kcal is only meaningful for CoNA
##            because CoCA is energy-constrained by definition
##            and CoRD's kcal composition reflects servings,
##            not cost-minimisation)
########################################################

library(tidyverse)
library(readxl)
library(lubridate)
library(scales)

##----------------------------------------------------------
## Directories
##----------------------------------------------------------

dirs <- c(
  "C:/Users/Portatil/Desktop/Least-cost-diets-and-affordability/Proyecto Interno",
  "C:/Users/danie/OneDrive/Escritorio/Least-cost-diets-and-affordability/Proyecto Interno"
)

base_dir <- dirs[dir.exists(dirs)][1]

if (is.na(base_dir)) {
  stop("Ninguno de los directorios existe")
}

out_real <- file.path(base_dir, "food-security-paper", "output", "real")
out_fig  <- file.path(base_dir, "food-security-paper", "output", "figures")

##----------------------------------------------------------
## Load data
##----------------------------------------------------------

coca_real <- read_excel(file.path(out_real, "coca_real.xlsx")) %>%
  select(ciudad, fecha, Demo_Group, Sex, cost_day_real, Cost_1000kcal_real) %>%
  mutate(diet = "CoCA")

cona_real <- read_excel(file.path(out_real, "cona_real.xlsx")) %>%
  select(ciudad, fecha, Demo_Group, Sex, cost_day_real, Cost_1000kcal_real) %>%
  mutate(diet = "CoNA")

cord_real <- read_excel(file.path(out_real, "cord_real.xlsx")) %>%
  select(ciudad, fecha, Demo_Group, Sex, cost_day_real, Cost_1000kcal_real) %>%
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
## Points every December — annual marker
##----------------------------------------------------------

diets_points <- diets_real %>%
  filter(month(fecha) == 12)

##----------------------------------------------------------
## Aesthetics — diet palette (Figure 1)
##----------------------------------------------------------

diet_colors <- c(
  "CoCA" = "#2C3E6B",
  "CoNA" = "#C0392B",
  "CoRD" = "#27AE60"
)

diet_linetypes <- c(
  "CoCA" = "solid",
  "CoNA" = "dashed",
  "CoRD" = "dotdash"
)

diet_shapes <- c(
  "CoCA" = 16,
  "CoNA" = 17,
  "CoRD" = 15
)

##----------------------------------------------------------
## Aesthetics — member palette (Figure 2)
##----------------------------------------------------------

member_colors <- c(
  "Adult male"   = "#2C3E6B",
  "Adult female" = "#C0392B",
  "Female child" = "#27AE60"
)

member_linetypes <- c(
  "Adult male"   = "solid",
  "Adult female" = "dashed",
  "Female child" = "dotdash"
)

member_shapes <- c(
  "Adult male"   = 16,
  "Adult female" = 17,
  "Female child" = 15
)

##----------------------------------------------------------
## Q1 journal theme
##----------------------------------------------------------

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
##           Three diets, facet: city (rows) × member (columns)
##----------------------------------------------------------

fig1 <- ggplot(diets_real,
               aes(x        = fecha,
                   y        = cost_day_real,
                   color    = ciudad_label,
                   linetype = ciudad_label,
                   shape    = ciudad_label)) +
  geom_line(linewidth = 0.55, alpha = 0.9) +
  geom_point(data = diets_points, size = 1.6, alpha = 0.9) +
  facet_grid(diet ~ member, scales = "free_y") +
  # scale_color_manual(values    = diet_colors)    +
  # scale_linetype_manual(values = diet_linetypes) +
  # scale_shape_manual(values    = diet_shapes)    +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y",
               expand = c(0.01, 0)) +
  scale_y_continuous(labels = comma_format(prefix   = "$",
                                           big.mark = ",",
                                           suffix   = "")) +
  labs(
    title   = " ",
    caption = paste0(
      "Note: CoCA = Cost of Caloric Adequacy; CoNA = Cost of Nutritional Adequacy; ",
      "CoRD = Cost of the Recommended Diet.\n",
      "The representative household comprises one adult male (31\u201351 years), one adult female ",
      "(31\u201351 years), and one female child (10\u201314 years).\n",
      "Nutritional requirements follow GABAS dietary guidelines adjusted by ",
      "city-specific estimated energy requirements (EER). ",
      "Costs deflated by the national CPI (base year 2019)."
    ),
    x = NULL,
    y = "Real COP / day"
  ) +
  theme_q1

ggsave(file.path(out_fig, "fig1_real_daily_cost_by_member.png"),
       fig1, width = 10, height = 8, dpi = 300)

ggsave(file.path(out_fig, "fig1_real_daily_cost_by_member.pdf"),
       fig1, width = 10, height = 8)

##----------------------------------------------------------
## Figure 2: Real cost per 1,000 kcal — CoNA only
##
## Rationale: Cost per 1,000 kcal is a meaningful diet-quality
## measure only for CoNA. For CoCA the energy constraint is
## binding by construction so Cost_1000kcal_real is mechanically
## equal across all foods. For CoRD the recommended servings
## fix the dietary structure independently of energy density,
## so the cost-per-kcal comparison would conflate dietary
## quality with serving-size conventions.
##
## Figure shows cost per 1,000 kcal for CoNA by member × city.
## Dotted lines = period mean per member × city.
##----------------------------------------------------------

cona_1000kcal <- diets_real %>% filter(diet == "CoNA")

cona_points <- diets_points %>% filter(diet == "CoNA")

# Period mean per member × city
means_1000kcal <- cona_1000kcal %>%
  dplyr::group_by(member, ciudad_label) %>%
  dplyr::summarize(mean_cost = mean(Cost_1000kcal_real, na.rm = TRUE),
            sd = sd(Cost_1000kcal_real, na.rm = TRUE))

fig2 <- ggplot(cona_1000kcal,
               aes(x        = fecha,
                   y        = Cost_1000kcal_real,
                   color    = member,
                   linetype = member,
                   shape    = member)) +
  geom_line(linewidth = 0.8, alpha = 0.9) +
  geom_point(data = cona_points, size = 1.6, alpha = 0.9) +
  geom_hline(data        = means_1000kcal,
             aes(yintercept = mean_cost, color = member),
             linetype    = "dotted",
             linewidth   = 0.8,
             alpha       = 0.75,
             inherit.aes = FALSE) +
  facet_wrap(~ ciudad_label, ncol = 3) +
  scale_color_manual(values    = member_colors)    +
  scale_linetype_manual(values = member_linetypes) +
  scale_shape_manual(values    = member_shapes)    +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y",
               expand = c(0.01, 0)) +
  scale_y_continuous(labels = comma_format(prefix   = "$",
                                           big.mark = ",",
                                           suffix   = "")) +
  labs(
    title   = " ",
    caption = paste0(
      "Note: Cost per 1,000 kcal is shown for the Cost of Nutritional Adequacy (CoNA) only. ",
      "This measure is not reported for CoCA — where the energy constraint is binding by ",
      "construction — or for CoRD, where the dietary structure is fixed by recommended ",
      "servings independently of energy density.\n",
      "Dotted horizontal lines indicate the 2019\u20132024 period mean for each household member.\n",
      "Nutritional requirements follow GABAS dietary guidelines adjusted by ",
      "city-specific estimated energy requirements (EER). ",
      "Costs deflated by the national CPI (base year 2019)."
    ),
    x = NULL,
    y = "Real COP / 1,000 kcal (CoNA)"
  ) +
  theme_q1

ggsave(file.path(out_fig, "fig2_real_Cost_1000kcal_real_by_member.png"),
       fig2, width = 10, height = 8, dpi = 300)

ggsave(file.path(out_fig, "fig2_real_Cost_1000kcal_real_by_member.pdf"),
       fig2, width = 10, height = 7)

message("Figures 1\u20132 saved to: ", out_fig)

##----------------------------------------------------------
## Real year-over-year variation
##   Formula:
##   YoY (%) = ((value_t / value_t-12) - 1) * 100
##----------------------------------------------------------

diets_real_yoy <- diets_real %>%
  dplyr::group_by(ciudad_label, member, diet) %>%
  dplyr::arrange(fecha, .by_group = TRUE) %>%
  mutate(
    yoy_cost_day_real = ((cost_day_real / lag(cost_day_real, 12)) - 1) * 100
  ) %>%
  ungroup()

diets_points_yoy <- diets_real_yoy %>%
  dplyr::filter(month(fecha) == 12, !is.na(yoy_cost_day_real))

fig1_yoy <- ggplot(diets_real_yoy %>% filter(!is.na(yoy_cost_day_real)),
                   aes(x        = fecha,
                       y        = yoy_cost_day_real,
                       color    = diet,
                       linetype = diet,
                       shape    = diet)) +
  geom_hline(yintercept = 0, color = "grey70", linewidth = 0.4) +
  geom_line(linewidth = 0.55, alpha = 0.9) +
  geom_point(data = diets_points_yoy, size = 1.6, alpha = 0.9) +
  facet_grid(ciudad_label ~ member) +
  scale_color_manual(values    = diet_colors)    +
  scale_linetype_manual(values = diet_linetypes) +
  scale_shape_manual(values    = diet_shapes)    +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y",
               expand = c(0.01, 0)) +
  scale_y_continuous(labels = function(x) paste0(round(x, 1), "%")) +
  labs(
    title   = " ",
    caption = paste0(
      "Note: Real year-over-year variation is computed as the percentage change ",
      "with respect to the same month in the previous year.",
      "For Figure 1, value refers to real daily cost in COP per day."
    ),
    x = NULL,
    y = "Real year-over-year change (%)"
  ) +
  theme_q1

ggsave(file.path(out_fig, "fig1_real_yoy_daily_cost_by_member.png"),
       fig1_yoy, width = 10, height = 6, dpi = 300)

ggsave(file.path(out_fig, "fig1_real_yoy_daily_cost_by_member.pdf"),
       fig1_yoy, width = 10, height = 6)

##----------------------------------------------------------
## Figure 2: Real year-over-year variation in cost per 1,000 kcal
##           CoNA only, by member × city
##----------------------------------------------------------

cona_1000kcal_yoy <- cona_1000kcal %>%
  dplyr::group_by(ciudad_label, member) %>%
  dplyr::arrange(fecha, .by_group = TRUE) %>%
  mutate(
    yoy_Cost_1000kcal_real = ((Cost_1000kcal_real / lag(Cost_1000kcal_real, 12)) - 1) * 100
  ) %>%
  ungroup()

cona_points_yoy <- cona_1000kcal_yoy %>%
  filter(month(fecha) == 12, !is.na(yoy_Cost_1000kcal_real))

fig2_yoy <- ggplot(cona_1000kcal_yoy %>% filter(!is.na(yoy_Cost_1000kcal_real)),
                   aes(x        = fecha,
                       y        = yoy_Cost_1000kcal_real,
                       color    = member,
                       linetype = member,
                       shape    = member)) +
  geom_hline(yintercept = 0, color = "grey70", linewidth = 0.4) +
  geom_line(linewidth = 0.8, alpha = 0.9) +
  geom_point(data = cona_points_yoy, size = 1.6, alpha = 0.9) +
  facet_wrap(~ ciudad_label, ncol = 3) +
  scale_color_manual(values    = member_colors)    +
  scale_linetype_manual(values = member_linetypes) +
  scale_shape_manual(values    = member_shapes)    +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y",
               expand = c(0.01, 0)) +
  scale_y_continuous(labels = function(x) paste0(round(x, 1), "%")) +
  labs(
    title   = " ",
    caption = paste0(
      "Note: Real year-over-year variation is computed as the percentage change ",
      "with respect to the same month in the previous year. ",
      "For Figure 2, value refers to real cost per 1,000 kcal for CoNA."
    ),
    x = NULL,
    y = "Real year-over-year change (%)"
  ) +
  theme_q1

ggsave(file.path(out_fig, "fig2_real_yoy_Cost_1000kcal_real_by_member.png"),
       fig2_yoy, width = 10, height = 8, dpi = 300)

ggsave(file.path(out_fig, "fig2_real_yoy_Cost_1000kcal_real_by_member.pdf"),
       fig2_yoy, width = 10, height = 7)

message("Real YoY Figures 1–2 saved to: ", out_fig)
