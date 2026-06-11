########################################################
## 05_figures/bloque4_affordability/fig09_afford_evolution.R
##
## Figure 9: Heatmap of monthly unaffordability rate
##   X = month | Y = decile | Fill = rate (%)
##   Facet: model (rows) × city (columns)
##
## Reads:  AFFORD_DIR/afford_results.xlsx
##
## Writes: FIG_DIR/final/fig09_afford_evolution.png / .pdf
########################################################

source("00_config.R")
source(file.path(REVIEW_DIR, "06_figures", "00_fig_config.R"))
library(tidyverse)
library(readxl)
library(scales)
library(lubridate)

# -----------------------------------------------------------------------
# 1. Load and clean
# -----------------------------------------------------------------------
afford <- read_excel(file.path(AFFORD_DIR, "afford_results.xlsx"),
                     sheet = 1) %>%
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
                        levels = paste0("Decil ", 1:10))) %>%
  filter(fecha >= PAPER_START, fecha <= PAPER_END)

# -----------------------------------------------------------------------
# 2. Discretise monthly rate into intervals
# -----------------------------------------------------------------------
afford_monthly <- afford %>%
  mutate(rate_cat = cut(rate,
                        breaks         = c(-Inf, 0, 25, 50, 75, Inf),
                        labels         = c("0",
                                           "(0, 25]",
                                           "(25, 50]",
                                           "(50, 75]",
                                           "(75, 100]"),
                        include.lowest = TRUE,
                        right          = TRUE))

message(sprintf("  %d rows | months: %s to %s",
                nrow(afford_monthly),
                min(afford_monthly$fecha),
                max(afford_monthly$fecha)))

# -----------------------------------------------------------------------
# 4. Heatmap
# -----------------------------------------------------------------------
fig9 <- ggplot(afford_monthly,
               aes(x = fecha, y = deciles,
                   fill = rate_cat)) +
  geom_tile(color = "white", linewidth = 0.3) +
  facet_grid(model ~ ciudad_lbl)  +
  scale_fill_manual(
    values = c(
      "0"          = "#F7F7F7",
      "(0, 25]"    = "#FDEBB0",
      "(25, 50]"   = "#FDAE61",
      "(50, 75]"   = "#D73027",
      "(75, 100]"  = "#7B1FA2"),
    name   = "Unaffordability\nrate (%)",
    drop   = FALSE) +
  labs(
    title    = paste0("Figure 9. Annual household unaffordability rate ",
                      "by income decile and city, 2019\u20132024"),
    subtitle = paste0("Monthly share of households unable to afford each diet. ",
                      "Darker fill indicates higher unaffordability."),
    caption  = paste0(
      "Note: Annual mean of monthly unaffordability rates. ",
      "Unaffordability = per capita income below daily diet cost.\n",
      "CoCA = Cost of Caloric Adequacy; CoNA = Cost of Nutritional Adequacy; ",
      "CoRD = Cost of Recommended Diet."),
    x = NULL,
    y = NULL) +
  paper_theme() +
  theme(
    axis.text.x      = element_text(angle = 45, hjust = 1, size = 8),
    axis.text.y      = element_text(size = 8),
    strip.text.x     = element_text(face = "bold", size = 10),
    strip.text.y     = element_text(face = "bold", size = 9, angle = 0),
    legend.position  = "bottom",
    legend.direction = "horizontal",
    legend.text      = element_text(family = "serif", size = 9),
    legend.key.width = unit(1.0, "cm"),
    legend.background = element_rect(color = "black", fill = "white",
                                     linewidth = 0.5),
    legend.margin    = margin(3, 8, 3, 8),
    panel.grid       = element_blank(),
    panel.spacing.x  = unit(0.3, "cm"),
    panel.spacing.y  = unit(0.4, "cm"))

ggsave(file.path(FIG_DIR, "final", "fig09_afford_evolution.png"),
       fig9, width = 13, height = 9, dpi = 300, bg = "white")
ggsave(file.path(FIG_DIR, "final", "fig09_afford_evolution.pdf"),
       fig9, width = 13, height = 9)

message("Figure 9 saved.")