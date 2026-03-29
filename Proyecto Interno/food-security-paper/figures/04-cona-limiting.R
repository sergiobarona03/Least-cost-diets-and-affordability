########################################################
## Cost of Nutritional Adequacy (CoNA)
## Figure 6: Shadow price elasticity heatmap over time
##           Discrete legend — data-driven breaks
## Table 3:  Binding frequency — simple Excel
########################################################

library(tidyverse)
library(readxl)
library(scales)
library(writexl)

##----------------------------------------------------------
## Directories and data
##----------------------------------------------------------

base_dir <- "C:\\Users\\Portatil\\Desktop\\Least-cost-diets-and-affordability\\Proyecto Interno"
out_cona <- file.path(base_dir, "food-security-paper", "output", "cona")
out_fig  <- file.path(base_dir, "food-security-paper", "output", "figures")
out_tabs <- file.path(base_dir, "food-security-paper", "output", "tables")

path_xlsx <- file.path(out_cona, "230326_cona_full.xlsx")

df.spe   <- read_excel(path_xlsx, sheet = "spe")   %>% mutate(fecha = as.Date(fecha))
df.limit <- read_excel(path_xlsx, sheet = "limit") %>% mutate(fecha = as.Date(fecha))

##----------------------------------------------------------
## Shared labels
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

##----------------------------------------------------------
## Prepare SPE data
##----------------------------------------------------------

df.spe <- df.spe %>%
  filter(constraint == "Min") %>%
  mutate(
    member       = recode(paste0(Sex, "_", Age), !!!member_labels),
    member       = factor(member, levels = member_order),
    ciudad_label = recode(ciudad, !!!city_labels)
  )

# Nutrient order: descending % binding across all observations
nutrient_order_spe <- df.spe %>%
  group_by(Nutrients) %>%
  summarize(pct_bind = mean(SPE > 0, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(pct_bind)) %>%
  pull(Nutrients)

df.spe <- df.spe %>%
  mutate(Nutrients = factor(Nutrients, levels = nutrient_order_spe))

# One observation per nutrient × member × city × month — no aggregation needed
spe_summary <- df.spe %>%
  select(Nutrients, member, ciudad_label, fecha, SPE) %>%
  rename(spe_val = SPE)

##----------------------------------------------------------
## Discrete SPE bins — data-driven breaks
##
## Based on the empirical distribution (N = 10,368):
##   = 0          : not binding (66.2% of obs)
##   (0, 0.02]    : very low — Sodium, Niacin (near-zero binding)
##   (0.02, 0.10] : low      — VitaminC, VitaminA
##   (0.10, 0.40] : moderate — Protein, Zinc, VitaminB12
##   > 0.40       : high     — Calcium (binds at 100%, mean SPE ~ 0.37)
##
## Palette: light grey → teal → gold → deep purple
## Perceptually distinct, reads well in greyscale
##----------------------------------------------------------

spe_breaks <- c(-Inf, 0, 0.02, 0.10, 0.40, Inf)
spe_labels <- c(
  "0 (not binding)",
  "(0, 0.02]",
  "(0.02, 0.10]",
  "(0.10, 0.40]",
  "> 0.40"
)

spe_palette <- c(
  "0 (not binding)" = "#FFFFCC",   # light grey  — not binding
  "(0, 0.02]"       = "#FEE08B",   # pale yellow — very low
  "(0.02, 0.10]"    = "#FC8D59",   # orange      — low
  "(0.10, 0.40]"    = "#D7191C",   # red         — moderate
  "> 0.40"          = "#7B0000"    # dark red    — high
)

spe_summary <- spe_summary %>%
  mutate(
    spe_bin = cut(spe_val,
                  breaks         = spe_breaks,
                  labels         = spe_labels,
                  right          = TRUE,
                  include.lowest = TRUE),
    spe_bin = factor(spe_bin, levels = spe_labels)
  )

##----------------------------------------------------------
## Figure 6: SPE heatmap over time — discrete legend
##           x: fecha (monthly) | y: nutrients
##           facet: city (rows) × member (columns)
##----------------------------------------------------------

fig6 <- ggplot(spe_summary,
               aes(x = fecha, y = Nutrients, fill = spe_bin)) +
  geom_tile(color = NA) +
  facet_grid(ciudad_label ~ member) +
  scale_fill_manual(
    values   = spe_palette,
    name     = "SPE",
    na.value = "grey92",
    drop     = FALSE
  ) +
  scale_x_date(
    date_breaks       = "3 months",
    date_minor_breaks = "1 month",
    date_labels       = "%b %Y",
    expand            = c(0, 0)
  ) +
  labs(
    title    = " ",
    caption  = paste0(
      "Note: The shadow price elasticity (SPE) measures the proportional reduction in diet cost ",
      "from a marginal relaxation of each nutritional lower bound,\n",
      "normalised by the constraint level. Only lower-bound constraints are shown. ",
      "Grey cells indicate months where the constraint is not binding (SPE = 0).\n",
      "Break points are data-driven: (0, 0.02] captures near-zero binding (Sodium, Niacin); ",
      "(0.02, 0.10] captures VitaminC and VitaminA;\n",
      "(0.10, 0.40] captures Protein, Zinc and VitaminB12; ",
      ">0.40 captures Calcium, which binds in 100% of observations.\n",
      "CoNA = Cost of Nutritional Adequacy."
    ),
    x = NULL,
    y = NULL
  ) +
  theme_bw(base_size = 10) +
  theme(
    text              = element_text(family = "serif"),
    plot.title        = element_text(face = "bold", size = 11,
                                     margin = margin(b = 4)),
    plot.subtitle     = element_text(size = 9, color = "grey35",
                                     margin = margin(b = 6)),
    plot.caption      = element_text(size = 7.5, color = "grey45",
                                     hjust = 0, margin = margin(t = 6)),
    axis.text.x       = element_text(size = 7, angle = 45, hjust = 1),
    axis.text.y       = element_text(size = 8),
    strip.background  = element_rect(fill = "grey96", color = "grey70"),
    strip.text.x      = element_text(face = "bold", size = 9),
    strip.text.y      = element_text(face = "bold", size = 8),
    legend.position   = "bottom",
    legend.title      = element_text(size = 9, face = "bold"),
    legend.text       = element_text(size = 8),
    legend.key.size   = unit(0.45, "cm"),
    legend.key.width  = unit(1.0, "cm"),
    panel.grid        = element_blank(),
    panel.spacing     = unit(0.3, "cm"),
    plot.margin       = margin(6, 8, 6, 6)
  ) +
  guides(fill = guide_legend(nrow = 1))

ggsave(file.path(out_fig, "fig6_spe_heatmap_cona.png"),
       fig6, width = 10, height = 7, dpi = 300)

ggsave(file.path(out_fig, "fig6_spe_heatmap_cona.pdf"),
       fig6, width = 10, height = 7)

##----------------------------------------------------------
## Prepare binding frequency data (df.limit)
##----------------------------------------------------------

df.limit <- df.limit %>%
  mutate(
    member       = recode(paste0(Sex, "_", Age), !!!member_labels),
    member       = factor(member, levels = member_order),
    ciudad_label = recode(ciudad, !!!city_labels)
  )

# Nutrient order: descending binding frequency
nutrient_order_lim <- df.limit %>%
  group_by(Nutrients) %>%
  summarize(freq = mean(Limiting == 1, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(freq)) %>%
  pull(Nutrients)

df.limit <- df.limit %>%
  mutate(Nutrients = factor(Nutrients, levels = nutrient_order_lim))

##----------------------------------------------------------
## Table 3: Binding frequency — simple writexl table
##
## Rows    = nutrients ordered by descending binding frequency
## Columns = member × city
## Values  = % of months where constraint is binding
##----------------------------------------------------------

tab_binding <- df.limit %>%
  group_by(Nutrients, member, ciudad_label) %>%
  summarize(
    freq_pct = round(mean(Limiting == 1, na.rm = TRUE) * 100, 1),
    .groups  = "drop"
  ) %>%
  unite("col", member, ciudad_label, sep = " \u2014 ") %>%
  pivot_wider(names_from  = col,
              values_from = freq_pct) %>%
  arrange(match(Nutrients, nutrient_order_lim))

note <- tibble(
  Note = paste0(
    "Values represent the percentage of city-month observations ",
    "(January 2019 - December 2024) in which each nutritional lower bound ",
    "is binding for the cost-minimising CoNA diet. ",
    "A constraint is binding when the optimal diet exactly meets the minimum ",
    "nutritional requirement. Nutrients ordered by descending overall binding frequency."
  )
)

write_xlsx(
  list(`Table 3` = tab_binding,
       `Notes`   = note),
  file.path(out_tabs, "table3_binding_frequency_cona.xlsx")
)
