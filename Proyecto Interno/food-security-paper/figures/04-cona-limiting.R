########################################################
## Cost of Nutritional Adequacy (CoNA)
## Figure 6: Shadow price elasticity heatmap over time
##           Discrete legend — data-driven breaks
## Table:    SPE summary by nutrient × member × city
##           Mean (SD) of SPE over 2019–2024
##           Pooled over months with SPE > 0 (binding only)
##           and over all months (including non-binding)
########################################################

library(tidyverse)
library(readxl)
library(scales)
library(writexl)
library(lubridate)

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
  dplyr::group_by(Nutrients) %>%
  dplyr::summarize(pct_bind = mean(SPE > 0, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(pct_bind)) %>%
  pull(Nutrients)

df.spe <- df.spe %>%
  mutate(Nutrients = factor(Nutrients, levels = nutrient_order_spe))

spe_summary <- df.spe %>%
  select(Nutrients, member, ciudad_label, fecha, SPE) %>%
  dplyr::rename(spe_val = SPE)

##----------------------------------------------------------
## Discrete SPE bins
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
  "0 (not binding)" = "#FFFFCC",
  "(0, 0.02]"       = "#FEE08B",
  "(0.02, 0.10]"    = "#FC8D59",
  "(0.10, 0.40]"    = "#D7191C",
  "> 0.40"          = "#7B0000"
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
## Figure 6: SPE heatmap over time
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
    title   = " ",
    caption = paste0(
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
## Table: SPE summary by nutrient × member × city
##
## Two statistics per cell:
##   mean_all (SD): mean SPE over ALL months (including 0)
##   mean_bind (SD): mean SPE over binding months only (SPE > 0)
##
## Compact version: pooled across cities
## Full version: by member × city
##----------------------------------------------------------

##----------------------------------------------------------
## Compact: pooled across cities
## Rows: nutrient | Columns: member
## Cell: mean_all (SD) / mean_bind (SD)
##----------------------------------------------------------

tab_spe_compact <- spe_summary %>%
  dplyr::group_by(Nutrients, member) %>%
  dplyr::summarize(
    mean_all  = round(mean(spe_val,              na.rm = TRUE), 3),
    sd_all    = round(sd(spe_val,                na.rm = TRUE), 3),
    mean_bind = round(mean(spe_val[spe_val > 0], na.rm = TRUE), 3),
    sd_bind   = round(sd(spe_val[spe_val > 0],   na.rm = TRUE), 3),
    pct_bind  = round(mean(spe_val > 0,          na.rm = TRUE) * 100, 1),
    .groups   = "drop"
  ) %>%
  mutate(
    cell_all  = paste0(mean_all,  " (", sd_all,  ")"),
    cell_bind = paste0(
      if_else(is.nan(mean_bind), "—", paste0(mean_bind, " (", sd_bind, ")")),
      " [", pct_bind, "%]"
    )
  ) %>%
  arrange(match(Nutrients, nutrient_order_spe))

# Wide: mean over all months
tab_wide_all <- tab_spe_compact %>%
  dplyr::select(Nutrients, member, cell_all) %>%
  pivot_wider(names_from = member, values_from = cell_all) %>%
  mutate(Statistic = "Mean SPE, all months (SD)") %>%
  select(Nutrients, Statistic, `Adult male`, `Adult female`, `Female child`)

# Wide: mean over binding months only
tab_wide_bind <- tab_spe_compact %>%
  dplyr::select(Nutrients, member, cell_bind) %>%
  pivot_wider(names_from = member, values_from = cell_bind) %>%
  mutate(Statistic = "Mean SPE, binding months (SD) [% binding]") %>%
  select(Nutrients, Statistic, `Adult male`, `Adult female`, `Female child`)

tab_compact_final <- bind_rows(tab_wide_all, tab_wide_bind) %>%
  arrange(match(Nutrients, nutrient_order_spe), Statistic)

##----------------------------------------------------------
## Full: by member × city
## Rows: nutrient | Columns: member × city
## Cell: mean_all (SD)
##----------------------------------------------------------

tab_spe_full <- spe_summary %>%
  dplyr::group_by(Nutrients, member, ciudad_label) %>%
  dplyr::summarize(
    mean_all = round(mean(spe_val,              na.rm = TRUE), 3),
    sd_all   = round(sd(spe_val,                na.rm = TRUE), 3),
    .groups  = "drop"
  ) %>%
  mutate(
    cell = paste0(mean_all, " (", sd_all, ")")
  ) %>%
  select(Nutrients, member, ciudad_label, cell) %>%
  pivot_wider(
    names_from  = c(member, ciudad_label),
    values_from = cell,
    names_glue  = "{member} | {ciudad_label}"
  ) %>%
  arrange(match(Nutrients, nutrient_order_spe)) %>%
  select(
    Nutrients,
    starts_with("Adult male | Bogotá"),
    starts_with("Adult male | Cali"),
    starts_with("Adult male | Medellín"),
    starts_with("Adult female | Bogotá"),
    starts_with("Adult female | Cali"),
    starts_with("Adult female | Medellín"),
    starts_with("Female child | Bogotá"),
    starts_with("Female child | Cali"),
    starts_with("Female child | Medellín")
  )

##----------------------------------------------------------
## Notes
##----------------------------------------------------------

note_spe <- tibble(
  Note = paste0(
    "Each cell in the compact table reports the mean (SD) of the shadow price elasticity (SPE) ",
    "over all months (row 1) and over binding months only, i.e. months where SPE > 0 (row 2), ",
    "with the percentage of binding months shown in brackets.\n",
    "The SPE measures the proportional reduction in CoNA cost from a 1% marginal relaxation of ",
    "each nutritional lower bound, normalised by the constraint level.\n",
    "A value of 0 indicates a non-binding (non-limiting) constraint in that month. ",
    "Nutrients are ordered by descending binding frequency. ",
    "Period: January 2019 -- December 2024. ",
    "CoNA = Cost of Nutritional Adequacy."
  )
)

##----------------------------------------------------------
## Save
##----------------------------------------------------------

write_xlsx(
  list(
    `Compact (pooled cities)` = tab_compact_final,
    `Full (by member x city)` = tab_spe_full,
    `Notes`                   = note_spe
  ),
  file.path(out_tabs, "table_spe_summary_cona.xlsx")
)

message("Figure 6 saved to: ", out_fig)
message("SPE table saved to: ", out_tabs)
