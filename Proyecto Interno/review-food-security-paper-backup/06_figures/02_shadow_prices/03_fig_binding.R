########################################################
## 05_figures/bloque2_shadow_prices/fig03_binding_heatmap.R
##
## Figure 3: Annual binding frequency of nutritional constraints
##   Rows    = nutrients (ordered by overall binding frequency)
##   Columns = years (2019–2024)
##   Fill    = % of months in the year where SP > 0 (binding)
##   Facet   = city (rows) × household member (columns)
##
## A constraint is binding when SP > 0, meaning the
## cost-minimising diet exactly meets — but would exceed
## cost if — the nutritional lower bound were tighter.
##
## Nutrients with 0% binding frequency across every city,
## member, and period are dropped entirely.
##
## Reads:  CONA_DIR/cona_results.rds
##
## Writes: FIG_DIR/final/fig03_binding_heatmap.png / .pdf
########################################################
#source("00_config.R")
source(file.path(REVIEW_DIR, "06_figures", "00_fig_config.R"))
library(tidyverse)
library(lubridate)

# -----------------------------------------------------------------------
# 0. Local city map — avoids relying on CITY_LABS from other config files
# -----------------------------------------------------------------------
city_lbl_map <- c(
  "BOGOTA"   = "Bogotá",
  "MEDELLIN" = "Medellín",
  "CALI"     = "Cali")

# -----------------------------------------------------------------------
# 1. Load data
# -----------------------------------------------------------------------
message("Loading SPE data...")

# Use $limit for binding identification (Limiting == 1)
limit <- readRDS(file.path(CONA_DIR, "cona_results.rds"))$limit %>%
  mutate(
    fecha      = as.Date(fecha),
    year       = year(fecha),
    member     = recode(paste0(Sex, "_", Age), !!!MEMBER_LABS),
    member     = factor(member, levels = MEMBER_ORDER),
    ciudad_lbl = city_lbl_map[ciudad],
    ciudad_lbl = factor(ciudad_lbl,
                        levels = c("Bogotá", "Medellín", "Cali"))) %>%
  filter(fecha >= PAPER_START, fecha <= PAPER_END)

# -----------------------------------------------------------------------
# 2. Annual binding frequency — % of months per year that are binding
# -----------------------------------------------------------------------
binding_annual <- limit %>%
  group_by(Nutrients, ciudad_lbl, member, year) %>%
  dplyr::summarise(
    pct_binding = mean(Limiting, na.rm = TRUE) * 100,
    .groups     = "drop")

# -----------------------------------------------------------------------
# 2b. Drop nutrients with 0% binding in every city, member, and year
# -----------------------------------------------------------------------
nutrients_keep <- binding_annual %>%
  group_by(Nutrients) %>%
  dplyr::summarise(max_pct = max(pct_binding, na.rm = TRUE), .groups = "drop") %>%
  filter(max_pct > 0) %>%
  pull(Nutrients)

n_dropped <- n_distinct(binding_annual$Nutrients) - length(nutrients_keep)

binding_annual <- binding_annual %>%
  filter(Nutrients %in% nutrients_keep)

message(sprintf("Kept %d nutrients, dropped %d with 0%% binding frequency everywhere",
                length(nutrients_keep), n_dropped))

# Order nutrients by descending overall binding frequency
nutrient_order <- limit %>%
  filter(Nutrients %in% nutrients_keep) %>%
  group_by(Nutrients) %>%
  dplyr::summarise(freq = mean(Limiting, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(freq)) %>%
  pull(Nutrients)

binding_annual <- binding_annual %>%
  mutate(Nutrients = factor(Nutrients, levels = nutrient_order))

message(sprintf(
  "  %d rows | %d nutrients | %d cities | %d members | %d years",
  nrow(binding_annual),
  n_distinct(binding_annual$Nutrients),
  n_distinct(binding_annual$ciudad_lbl),
  n_distinct(binding_annual$member),
  n_distinct(binding_annual$year)))

# -----------------------------------------------------------------------
# 3. Figure
# -----------------------------------------------------------------------
fig3 <- ggplot(binding_annual,
               aes(x = factor(year), y = Nutrients, fill = pct_binding)) +
  geom_tile(color = "white", linewidth = 0.3) +
  geom_text(aes(label = paste0(round(pct_binding, 0), "%")),
            size = 2.2, family = "serif", color = "grey20") +
  facet_grid(member ~ ciudad_lbl) +
  scale_fill_gradient(
    low      = "grey92",
    high     = "#C0392B",
    limits   = c(0, 100),
    name     = "% months\nbinding") +
  labs(
    title    = " ",
    subtitle = " ",
    caption  = paste0(
      "Note: A constraint is binding when the cost-minimising diet exactly meets ",
      "the nutritional lower bound. Cell values report the share of months in ",
      "each year that the constraint was binding. Nutrients with 0% binding ",
      "frequency in every period and city are omitted.\n",
      "CoNA = Cost of Nutritional Adequacy. ",
      "Household members: adult male [31\u201351), ",
      "adult female [31\u201351), female child [10\u201314)."),
    x = NULL,
    y = NULL) +
  paper_theme() +
  theme(
    axis.text.x        = element_text(angle = 45, hjust = 1, size = 8),
    axis.text.y        = element_text(size = 8),
    strip.text.x       = element_text(face = "bold", size = 10),
    strip.text.y       = element_text(face = "bold", size = 9,
                                      angle = 0),
    legend.position    = "bottom",
    legend.direction   = "horizontal",
    legend.key.width   = unit(1.4, "cm"),
    legend.key.height  = unit(0.4, "cm"),
    legend.text        = element_text(family = "serif", size = 9),
    legend.title        = element_text(family = "serif", size = 9),
    legend.background  = element_rect(color = "black", fill = "white",
                                      linewidth = 0.5),
    legend.margin      = margin(5, 10, 5, 10),
    panel.grid         = element_blank(),
    panel.spacing      = unit(0.3, "cm"))

ggsave(file.path(FIG_DIR, "final", "fig03_binding_heatmap.png"),
       fig3, width = 14, height = 9, dpi = 300, bg = "white")
ggsave(file.path(FIG_DIR, "final", "fig03_binding_heatmap.pdf"),
       fig3, width = 14, height = 9)
message("Figure 3 saved.")