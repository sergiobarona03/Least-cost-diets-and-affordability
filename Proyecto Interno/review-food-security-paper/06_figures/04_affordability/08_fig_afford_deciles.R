########################################################
## 05_figures/bloque4_affordability/fig08_afford_deciles.R
##
## Figure 8: Unaffordability rate by decile × city
##   2019 vs 2024 — before and after inflation episode
##   Facet: model (cols) × city (rows)
##   X: decile | Y: % households unable to afford
##   Color: year (2019 vs 2024)
##
## Reads:  AFFORD_DIR/afford_results.xlsx
##
## Writes: FIG_DIR/final/fig08_afford_deciles.png / .pdf
########################################################

source("00_config.R")
source(file.path(REVIEW_DIR, "06_figures", "00_fig_config.R"))
library(tidyverse)
library(readxl)
library(scales)

# -----------------------------------------------------------------------
# 1. Load and clean
# -----------------------------------------------------------------------
afford <- read_excel(file.path(AFFORD_DIR, "afford_results.xlsx")) %>%
  mutate(
    fecha      = as.Date(fecha),
    ciudad_lbl = case_when(
      grepl("BOGOT", ciudad, ignore.case = TRUE) ~ "Bogotá",
      grepl("MEDEL", ciudad, ignore.case = TRUE) ~ "Medellín",
      grepl("CALI",  ciudad, ignore.case = TRUE) ~ "Cali",
      TRUE ~ ciudad),
    ciudad_lbl = factor(ciudad_lbl,
                        levels = c("Bogotá", "Medellín", "Cali")),
    model      = factor(model, levels = c("CoCA", "CoNA", "CoRD")),
    deciles    = factor(deciles,
                        levels = paste0("Decil ", 1:10)))

# -----------------------------------------------------------------------
# 2. Annual mean by decile × city × model × year
# -----------------------------------------------------------------------
afford_annual <- afford %>%
  group_by(deciles, ciudad_lbl, model, year) %>%
  dplyr::summarise(
    rate     = mean(rate,     na.rm = TRUE),
    gap      = mean(gap,      na.rm = TRUE),
    severity = mean(severity, na.rm = TRUE),
    .groups  = "drop")

# Keep 2019 and 2024 only
afford_compare <- afford_annual %>%
  filter(year %in% c(2019, 2024)) %>%
  mutate(year_lbl = factor(as.character(year),
                           levels = c("2019", "2024")))

message(sprintf("  %d rows | deciles: %s",
                nrow(afford_compare),
                paste(levels(afford_compare$deciles), collapse = ", ")))

# -----------------------------------------------------------------------
# 3. Figure
# -----------------------------------------------------------------------
year_cols <- c("2019" = "#2C3E6B", "2024" = "#C0392B")

fig8 <- ggplot(afford_compare,
               aes(x = deciles, y = rate,
                   color = year_lbl,
                   group = year_lbl)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2) +
  facet_grid(ciudad_lbl ~ model) +
  scale_color_manual(values = year_cols, name = NULL) +
  scale_y_continuous(
    labels = function(x) sprintf("%.0f%%", x),
    limits = c(0, NA)) +
  scale_x_discrete(labels = 1:10) +
  labs(
    title    = paste0("Figure 8. Household unaffordability rate by income decile ",
                      "and city, 2019 vs 2024"),
    subtitle = paste0("Share of households unable to afford each diet. ",
                      "Comparison before and after the inflationary episode."),
    caption  = paste0(
      "Note: Unaffordability rate = share of households in each decile ",
      "whose per capita income is below the daily diet cost.\n",
      "CoCA = Cost of Caloric Adequacy; CoNA = Cost of Nutritional Adequacy; ",
      "CoRD = Cost of Recommended Diet.\n",
      "Annual mean computed over monthly observations within each year."),
    x = "Income decile",
    y = "Unaffordability rate (%)") +
  paper_theme() +
  theme(
    legend.position   = "top",
    legend.direction  = "horizontal",
    legend.text       = element_text(family = "serif", size = 10),
    legend.key.width  = unit(1.2, "cm"),
    legend.background = element_rect(color = "black", fill = "white",
                                     linewidth = 0.5),
    legend.margin     = margin(3, 8, 3, 8),
    axis.text.x       = element_text(angle = 0, size = 8),
    strip.text        = element_text(face = "bold", size = 9),
    panel.spacing     = unit(0.3, "cm"))

ggsave(file.path(FIG_DIR, "final", "fig08_afford_deciles.png"),
       fig8, width = 12, height = 9, dpi = 300, bg = "white")
ggsave(file.path(FIG_DIR, "final", "fig08_afford_deciles.pdf"),
       fig8, width = 12, height = 9)

message("Figure 8 saved.")