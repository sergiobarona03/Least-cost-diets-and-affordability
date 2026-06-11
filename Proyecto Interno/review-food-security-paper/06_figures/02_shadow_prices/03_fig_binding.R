########################################################
## 05_figures/bloque2_shadow_prices/fig03_binding_heatmap.R
##
## Figure 3: Binding nutritional constraints heatmap
##   Rows    = nutrients (ordered by binding frequency)
##   Columns = months (2019–2024)
##   Fill    = SP > 0 (binding) vs SP = 0 (not binding)
##   Facet   = city (rows) × household member (columns)
##
## A constraint is binding when SP > 0, meaning the
## cost-minimising diet exactly meets — but would exceed
## cost if — the nutritional lower bound were tighter.
##
## Reads:  CONA_DIR/cona_results.rds
##
## Writes: FIG_DIR/final/fig03_binding_heatmap.png / .pdf
########################################################

source("00_config.R")
source(file.path(REVIEW_DIR, "06_figures", "00_fig_config.R"))
library(tidyverse)
library(lubridate)

# -----------------------------------------------------------------------
# 1. Load data
# -----------------------------------------------------------------------
message("Loading SPE data...")

# Use $limit for binding identification (Limiting == 1)
limit <- readRDS(file.path(CONA_DIR, "cona_results.rds"))$limit %>%
  mutate(
    fecha      = as.Date(fecha),
    member     = recode(paste0(Sex, "_", Age), !!!MEMBER_LABS),
    member     = factor(member, levels = MEMBER_ORDER),
    ciudad_lbl = CITY_LABS[ciudad],
    ciudad_lbl = factor(ciudad_lbl,
                        levels = c("Bogotá", "Medellín", "Cali"))) %>%
  filter(fecha >= PAPER_START, fecha <= PAPER_END)

# Order nutrients by descending overall binding frequency
nutrient_order <- limit %>%
  group_by(Nutrients) %>%
  dplyr::summarise(freq = mean(Limiting, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(freq)) %>%
  pull(Nutrients)

limit <- limit %>%
  mutate(Nutrients = factor(Nutrients, levels = nutrient_order))

message(sprintf(
  "  %d rows | %d nutrients | %d cities | %d members",
  nrow(limit),
  n_distinct(limit$Nutrients),
  n_distinct(limit$ciudad_lbl),
  n_distinct(limit$member)))

# -----------------------------------------------------------------------
# 2. Figure
# -----------------------------------------------------------------------
fig3 <- ggplot(limit,
               aes(x = fecha, y = Nutrients, fill = factor(Limiting))) +
  geom_tile(color = NA) +
  facet_grid(member ~ ciudad_lbl) +
  scale_fill_manual(
    values = c("0" = "grey92", "1" = "#C0392B"),
    labels = c("0" = "Not binding", "1" = "Binding"),
    name   = NULL) +
  scale_x_date(date_breaks  = "1 year",
               date_labels  = "%Y",
               expand       = c(0, 0)) +
  labs(
    title    = " ",
    subtitle = " ",
    caption  = paste0(
      "Note: A constraint is binding when the cost-minimising diet exactly meets ",
      "the nutritional lower bound.\n",
      "CoNA = Cost of Nutritional Adequacy. ",
      "Household members: adult male [31\u201351), ",
      "adult female [31\u201351), female child [10\u201314)."),
    x = NULL,
    y = NULL) +
  paper_theme() +
  theme(
    axis.text.x      = element_text(angle = 45, hjust = 1, size = 8),
    axis.text.y      = element_text(size = 8),
    strip.text.x     = element_text(face = "bold", size = 10),
    strip.text.y     = element_text(face = "bold", size = 9,
                                    angle = 0),
    legend.position  = "bottom",
    legend.key.size  = unit(0.5, "cm"),
    panel.grid       = element_blank(),
    panel.spacing    = unit(0.3, "cm"))

ggsave(file.path(FIG_DIR, "final", "fig03_binding_heatmap.png"),
       fig3, width = 14, height = 9, dpi = 300, bg = "white")
ggsave(file.path(FIG_DIR, "final", "fig03_binding_heatmap.pdf"),
       fig3, width = 14, height = 9)

message("Figure 3 saved.")