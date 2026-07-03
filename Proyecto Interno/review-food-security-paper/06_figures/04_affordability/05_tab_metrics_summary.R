########################################################
## 05_figures/bloque4_affordability/fig09_afford_gap.R
##
## Figure 9: Affordability gap (FGT1) heatmap
##   Deciles 1, 2, 3 only (as cited in the text)
##   Facet: model (cols) × city (rows)
##   X: year | Y: decile | Fill: mean gap
##
## Reads:  AFFORD_DIR/afford_results.xlsx
##
## Writes: FIG_DIR/final/fig09_afford_gap.png / .pdf
########################################################

source("00_config.R")
source(file.path(REVIEW_DIR, "06_figures", "00_fig_config.R"))
library(tidyverse)
library(readxl)
library(lubridate)
library(scales)

# -----------------------------------------------------------------------
# 1. Load and clean
# -----------------------------------------------------------------------
afford <- read_excel(file.path(AFFORD_DIR, "afford_results.xlsx")) %>%
  mutate(
    fecha      = as.Date(fecha),
    year       = year(fecha),
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
  filter(fecha >= PAPER_START, fecha <= PAPER_END,
         deciles %in% c("Decil 1", "Decil 2", "Decil 3"))

# -----------------------------------------------------------------------
# 2. Annual mean gap by decile × city × model × year
# -----------------------------------------------------------------------
gap_annual <- afford %>%
  group_by(deciles, ciudad_lbl, model, year) %>%
  dplyr::summarise(
    mean_gap = mean(gap, na.rm = TRUE),
    .groups  = "drop") %>%
  mutate(deciles = factor(deciles, levels = c("Decil 1", "Decil 2", "Decil 3")))

message(sprintf("  %d rows | years: %s | deciles: %s",
                nrow(gap_annual),
                paste(sort(unique(gap_annual$year)), collapse = ", "),
                paste(levels(gap_annual$deciles), collapse = ", ")))

# -----------------------------------------------------------------------
# 3. Figure — heatmap
# -----------------------------------------------------------------------
fig9 <- ggplot(gap_annual,
               aes(x = factor(year), y = deciles, fill = mean_gap)) +
  geom_tile(color = "white", linewidth = 0.3) +
  geom_text(aes(label = sprintf("%.2f", mean_gap)),
            size = 2.3, family = "serif",
            color = ifelse(gap_annual$mean_gap > 0.5, "white", "grey20")) +
  facet_grid(ciudad_lbl ~ model) +
  scale_fill_gradient(
    low      = "#FFF7EC",
    high     = "#7B241C",
    limits   = c(0, 1),
    name     = "Mean\naffordability\ngap") +
  scale_y_discrete(labels = c("1", "2", "3")) +
  labs(
    title    = " ",
    subtitle = " ",
    caption  = paste0(
      "Note: The affordability gap measures the mean proportional shortfall ",
      "between food expenditure and diet cost, averaged across all households ",
      "in the decile (0 = no gap; 1 = food budget covers 0% of diet cost).\n",
      "CoCA = Cost of Caloric Adequacy; CoNA = Cost of Nutritional Adequacy; ",
      "CoRD = Cost of Recommended Diet."),
    x = NULL,
    y = "Income decile") +
  paper_theme() +
  theme(
    axis.text.x        = element_text(angle = 45, hjust = 1, size = 8),
    axis.text.y        = element_text(size = 9),
    strip.text         = element_text(face = "bold", size = 9),
    legend.position     = "bottom",
    legend.direction    = "horizontal",
    legend.key.width    = unit(1.4, "cm"),
    legend.key.height   = unit(0.4, "cm"),
    legend.text         = element_text(family = "serif", size = 9),
    legend.title        = element_text(family = "serif", size = 9),
    legend.background   = element_rect(color = "black", fill = "white",
                                       linewidth = 0.5),
    legend.margin       = margin(5, 10, 5, 10),
    panel.grid          = element_blank(),
    panel.spacing       = unit(0.25, "cm"))

ggsave(file.path(FIG_DIR, "final", "fig09_afford_gap.png"),
       fig9, width = 12, height = 7, dpi = 300, bg = "white")
ggsave(file.path(FIG_DIR, "final", "fig09_afford_gap.pdf"),
       fig9, width = 12, height = 7)

message("Figure 9 (affordability gap heatmap) saved.")