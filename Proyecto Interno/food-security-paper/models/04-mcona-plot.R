########################################################
## IPC-Constrained Cost of Nutritional Adequacy (CoNA-IPC)
## Figures: Diet cost and food composition — Q1 submission
########################################################

library(tidyverse)
library(readxl)
library(scales)
library(patchwork)

##----------------------------------------------------------
## Directories
##----------------------------------------------------------

base_dir <- "C:\\Users\\Portatil\\Desktop\\Least-cost-diets-and-affordability\\Proyecto Interno"
out_ipc  <- file.path(base_dir, "food-security-paper", "output", "cona-ipc")
out_fig  <- file.path(base_dir, "food-security-paper", "output", "cona-ipc")

##----------------------------------------------------------
## Load data
##----------------------------------------------------------

path_xlsx <- file.path(out_ipc, "230326_cona_ipc_full.xlsx")

df.cost <- read_excel(path_xlsx, sheet = "cost") %>%
  mutate(fecha = as.Date(fecha), year = lubridate::year(fecha))

df.comp <- read_excel(path_xlsx, sheet = "comp") %>%
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

# Alpha palette: sequential green — darker = higher alpha (stricter IPC enforcement)
alpha_vals   <- c(0.50, 0.75, 1.00)
alpha_colors <- setNames(
  colorRampPalette(c("#A9DFBF", "#1D6A39"))(length(alpha_vals)),
  as.character(alpha_vals)
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
## Prepare $cost
##----------------------------------------------------------

df.cost <- df.cost %>%
  mutate(
    member       = factor(recode(paste0(Sex, "_", Demo_Group), !!!member_labels),
                          levels = member_order),
    ciudad_label = recode(ciudad, !!!city_labels),
    alpha_val    = as.character(alpha_val)
  )

# Per capita cost (mean across members) by city × date × alpha
cost_pc <- df.cost %>%
  group_by(ciudad, ciudad_label, fecha, year, alpha_val) %>%
  summarize(
    cost_percap    = mean(cost_day, na.rm = TRUE),
    cost_household = sum(cost_day,  na.rm = TRUE),
    .groups = "drop"
  )

##----------------------------------------------------------
## Figure 1: Per capita CoNA-IPC — time series by city
##           one line per alpha, facet by city
##----------------------------------------------------------

fig1 <- ggplot(cost_pc,
               aes(x = fecha, y = cost_percap,
                   color = alpha_val, group = alpha_val)) +
  geom_line(linewidth = 0.7) +
  facet_wrap(~ ciudad_label, ncol = 3) +
  scale_color_manual(values = alpha_colors,
                     name   = expression(alpha)) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(labels = comma_format(prefix = "$", suffix = " COP")) +
  labs(
    title    = "Figure 1. Daily per capita CoNA-IPC by city and α, 2019–2024",
    subtitle = "Daily cost per household member (COP). Darker lines correspond to stricter IPC quantity share enforcement",
    caption  = paste0(
      "Note: α controls the minimum quantity share each food group must represent in the optimal diet,\n",
      "relative to its IPC share. α = 0.5 requires at least 50% of the IPC share; α = 1.0 enforces\n",
      "the full IPC quantity structure. Per capita cost computed as the mean across household members."
    ),
    x = NULL, y = "COP / day"
  ) +
  paper_theme +
  guides(color = guide_legend(nrow = 1, title = expression(alpha)))

ggsave(file.path(out_fig, "fig1_cona_ipc_percapita_series.png"),
       fig1, width = 12, height = 5, dpi = 300)

##----------------------------------------------------------
## Figure 2: CoNA-IPC by household member
##           one line per alpha, facet: member × city
##----------------------------------------------------------

fig2 <- ggplot(df.cost,
               aes(x = fecha, y = cost_day,
                   color = alpha_val, group = alpha_val)) +
  geom_line(linewidth = 0.6) +
  facet_grid(member ~ ciudad_label) +
  scale_color_manual(values = alpha_colors,
                     name   = expression(alpha)) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(labels = comma_format(prefix = "$")) +
  labs(
    title    = "Figure 2. Daily CoNA-IPC by household member, city, and α, 2019–2024",
    subtitle = "Daily cost of meeting nutritional requirements per member (COP)",
    caption  = "Note: Nutritional requirements follow GABAS guidelines adjusted by city-specific EER.",
    x = NULL, y = "COP / day"
  ) +
  paper_theme +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  guides(color = guide_legend(nrow = 1, title = expression(alpha)))

ggsave(file.path(out_fig, "fig2_cona_ipc_by_member.png"),
       fig2, width = 12, height = 9, dpi = 300)

##----------------------------------------------------------
## Figure 3: Cost of dietary realism — mean cost by alpha
##           shows E_IPC(alpha) - E* as alpha increases
##----------------------------------------------------------

cost_by_alpha <- df.cost %>%
  group_by(alpha_val) %>%
  summarize(
    mean_cost   = mean(cost_day,   na.rm = TRUE),
    median_cost = median(cost_day, na.rm = TRUE),
    sd_cost     = sd(cost_day,     na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(alpha_num = as.numeric(alpha_val))

fig3 <- ggplot(cost_by_alpha,
               aes(x = alpha_num, y = mean_cost)) +
  geom_ribbon(aes(ymin = mean_cost - sd_cost,
                  ymax = mean_cost + sd_cost),
              fill = "#A9DFBF", alpha = 0.4) +
  geom_line(color = "#1D6A39", linewidth = 1) +
  geom_point(color = "#1D6A39", size = 3) +
  scale_x_continuous(breaks = alpha_vals,
                     labels = as.character(alpha_vals)) +
  scale_y_continuous(labels = comma_format(prefix = "$", suffix = " COP")) +
  labs(
    title    = "Figure 3. Cost of dietary realism: mean daily CoNA-IPC cost by α, 2019–2024",
    subtitle = "Mean ± SD across all city–month observations and household members",
    caption  = paste0(
      "Note: The difference E_IPC(α) − E* measures the cost premium of enforcing IPC-consistent\n",
      "dietary patterns. E* is the standard CoNA cost (minimum-cost nutritionally adequate diet\n",
      "without dietary pattern constraints)."
    ),
    x = expression(alpha), y = "COP / day"
  ) +
  paper_theme

ggsave(file.path(out_fig, "fig3_cona_ipc_cost_by_alpha.png"),
       fig3, width = 8, height = 4.5, dpi = 300)

##----------------------------------------------------------
## Prepare $comp
##----------------------------------------------------------

df.comp <- df.comp %>%
  mutate(
    member       = factor(recode(paste0(Sex, "_", Demo_Group), !!!member_labels),
                          levels = member_order),
    ciudad_label = recode(ciudad, !!!city_labels),
    alpha_val    = as.character(alpha_val)
  )

# Foods appearing in at least 5% of city × date observations
# evaluated at alpha = 0.5 as reference
top_foods <- df.comp %>%
  filter(alpha_val == "0.5") %>%
  group_by(Food) %>%
  summarize(freq = n_distinct(paste(ciudad, fecha)), .groups = "drop") %>%
  mutate(share = freq / max(freq)) %>%
  filter(share >= 0.05) %>%
  pull(Food)

##----------------------------------------------------------
## Figure 4: Diet composition over time — aggregated across members
##           one panel per city, compare all alpha values
##----------------------------------------------------------

comp_highlight <- df.comp %>%
  filter(Food %in% top_foods) %>%
  group_by(Food, ciudad, ciudad_label, fecha, year, alpha_val) %>%
  summarize(quantity = mean(quantity, na.rm = TRUE), .groups = "drop") %>%
  mutate(alpha_label = paste0("α = ", alpha_val))

fig4 <- ggplot(comp_highlight,
               aes(x = fecha, y = quantity, fill = Food)) +
  geom_area(position = "stack", alpha = 0.85) +
  facet_grid(alpha_label ~ ciudad_label) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(labels = comma_format(suffix = " g")) +
  labs(
    title    = "Figure 4. Diet composition at selected α values by city, 2019–2024",
    subtitle = "Mean daily quantity (grams) aggregated across household members",
    caption  = paste0(
      "Note: Only foods appearing in ≥5% of city–month observations (at α = 0.5) are shown.\n",
      "Rows correspond to α = 0.5, α = 0.75, and α = 1.0 (full IPC quantity enforcement)."
    ),
    x = NULL, y = "Quantity (g / day)", fill = "Food item"
  ) +
  paper_theme +
  theme(legend.position = "bottom",
        legend.text     = element_text(size = 8),
        axis.text.x     = element_text(angle = 60, hjust = 1)) +
  guides(fill = guide_legend(nrow = 3))

ggsave(file.path(out_fig, "fig4_cona_ipc_comp_highlight.png"),
       fig4, width = 13, height = 10, dpi = 300)

##----------------------------------------------------------
## Figure 5: Diet composition — by household member
##           alpha = 0.5 vs alpha = 1.0, facet: member × city
##----------------------------------------------------------

comp_extremes <- df.comp %>%
  filter(Food %in% top_foods, alpha_val %in% c("0.5", "1")) %>%
  group_by(Food, member, ciudad, ciudad_label, fecha, year, alpha_val) %>%
  summarize(quantity = mean(quantity, na.rm = TRUE), .groups = "drop") %>%
  mutate(alpha_label = if_else(alpha_val == "0.5",
                               "α = 0.5 (loose IPC enforcement)",
                               "α = 1.0 (full IPC enforcement)"))

fig5 <- ggplot(comp_extremes,
               aes(x = fecha, y = quantity, fill = Food)) +
  geom_area(position = "stack", alpha = 0.85) +
  facet_grid(member + alpha_label ~ ciudad_label) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(labels = comma_format(suffix = " g")) +
  labs(
    title    = "Figure 5. Diet composition at α = 0.5 and α = 1.0 by household member and city, 2019–2024",
    subtitle = "Comparison of loose (α = 0.5) vs. full (α = 1.0) IPC quantity share enforcement",
    caption  = paste0(
      "Note: Only foods appearing in ≥5% of city–month observations (at α = 0.5) are shown.\n",
      "Each pair of rows corresponds to one household member."
    ),
    x = NULL, y = "Quantity (g / day)", fill = "Food item"
  ) +
  paper_theme +
  theme(legend.position = "bottom",
        legend.text     = element_text(size = 8),
        axis.text.x     = element_text(angle = 60, hjust = 1)) +
  guides(fill = guide_legend(nrow = 3))

ggsave(file.path(out_fig, "fig5_cona_ipc_comp_extremes.png"),
       fig5, width = 14, height = 14, dpi = 300)

message("Figures 1–5 saved to: ", out_fig)