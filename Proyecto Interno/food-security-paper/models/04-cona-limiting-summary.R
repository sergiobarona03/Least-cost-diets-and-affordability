########################################################
## Cost of Nutritional Adequacy (CoNA)
## Figure: Opt/Rest ratio heatmap over time
## Table:  Summary of Opt/Rest ratio by nutrient × member × city
##         Shows % of months limiting + mean (SD) of ratio
########################################################

library(tidyverse)
library(readxl)
library(lubridate)
library(scales)
library(writexl)

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

out_cona <- file.path(base_dir, "food-security-paper", "output", "cona")
out_fig  <- file.path(base_dir, "food-security-paper", "output", "figures")
out_tabs <- file.path(base_dir, "food-security-paper", "output", "tables")

path_xlsx <- file.path(out_cona, "230326_cona_full.xlsx")

df.limit <- read_excel(path_xlsx, sheet = "limit") %>%
  mutate(fecha = as.Date(fecha))

##----------------------------------------------------------
## Labels
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
## Prepare
##----------------------------------------------------------

df.limit <- df.limit %>%
  mutate(
    ratio        = (Opt / Rest) * 100,
    member       = factor(recode(paste0(Sex, "_", Age), !!!member_labels),
                          levels = member_order),
    ciudad_label = recode(ciudad, !!!city_labels)
  ) %>%
  filter(!is.na(ratio), !is.infinite(ratio))

# Nutrient order: descending binding frequency
nutrient_order <- df.limit %>%
  dplyr::group_by(Nutrients) %>%
  dplyr::summarize(freq = mean(Limiting == 1, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(freq)) %>%
  pull(Nutrients)

df.limit <- df.limit %>%
  mutate(Nutrients = factor(Nutrients, levels = nutrient_order))

##----------------------------------------------------------
## Discrete bins
##----------------------------------------------------------

ratio_breaks <- c(0, 100, 110, 150, 300, Inf)
ratio_labels <- c(
  "= 100% (limiting)",
  "(100, 110]%",
  "(110, 150]%",
  "(150, 300]%",
  "> 300%"
)

ratio_palette <- c(
  "= 100% (limiting)" = "#7B0000",
  "(100, 110]%"       = "#D7191C",
  "(110, 150]%"       = "#FC8D59",
  "(150, 300]%"       = "#FEE08B",
  "> 300%"            = "#FFFFCC"
)

df.limit <- df.limit %>%
  mutate(
    ratio_bin = cut(ratio,
                    breaks         = ratio_breaks,
                    labels         = ratio_labels,
                    right          = TRUE,
                    include.lowest = TRUE),
    ratio_bin = factor(ratio_bin, levels = ratio_labels)
  )

##----------------------------------------------------------
## Figure: Opt/Rest heatmap
##----------------------------------------------------------

fig_ratio <- ggplot(df.limit,
                    aes(x = fecha, y = Nutrients, fill = ratio_bin)) +
  geom_tile(color = NA) +
  facet_grid(ciudad_label ~ member) +
  scale_fill_manual(
    values   = ratio_palette,
    name     = "Opt / Rest ratio",
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
      "Note: The Opt/Rest ratio = (actual nutrient intake at the CoNA optimum / ",
      "minimum dietary requirement) \u00d7 100.\n",
      "A value of 100% indicates a limiting nutrient: the cost-minimising diet ",
      "exactly meets the minimum requirement.\n",
      "Values above 100% indicate that the requirement is exceeded and the nutrient ",
      "does not constrain the dietary solution.\n",
      "Nutrients are ordered by descending binding frequency. ",
      "Rows: cities; columns: household members. ",
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
    legend.key.width  = unit(1.2, "cm"),
    panel.grid        = element_blank(),
    panel.spacing     = unit(0.3, "cm"),
    plot.margin       = margin(6, 8, 6, 6)
  ) +
  guides(fill = guide_legend(nrow = 1))

ggsave(file.path(out_fig, "fig_optratio_heatmap_cona.png"),
       fig_ratio, width = 10, height = 7, dpi = 300)

ggsave(file.path(out_fig, "fig_optratio_heatmap_cona.pdf"),
       fig_ratio, width = 10, height = 7)

##----------------------------------------------------------
## Table: Summary — compact version pooled across cities
##
## Rows: nutrient (ordered by binding frequency)
## Columns: member (Adult male | Adult female | Female child)
## Each cell: % months limiting | mean ratio (SD)
##----------------------------------------------------------

tab_compact <- df.limit %>%
  dplyr::group_by(Nutrients, member) %>%
  dplyr::summarize(
    pct_limiting = round(mean(ratio_bin == "= 100% (limiting)",
                              na.rm = TRUE) * 100, 1),
    mean_ratio   = round(mean(ratio, na.rm = TRUE), 1),
    sd_ratio     = round(sd(ratio,   na.rm = TRUE), 1),
    .groups      = "drop"
  ) %>%
  mutate(
    cell = paste0(pct_limiting, "% | ", mean_ratio, " (", sd_ratio, ")")
  ) %>%
  dplyr::select(Nutrients, member, cell) %>%
  pivot_wider(names_from  = member,
              values_from = cell) %>%
  arrange(match(Nutrients, nutrient_order)) %>%
  dplyr::select(Nutrients, `Adult male`, `Adult female`, `Female child`)

##----------------------------------------------------------
## Table: Full version by member × city
##
## Rows: nutrient
## Columns: member × city (9 columns)
## Each cell: % months limiting | mean ratio (SD)
##----------------------------------------------------------

tab_full <- df.limit %>%
  dplyr::group_by(Nutrients, member, ciudad_label) %>%
  dplyr::summarize(
    pct_limiting = round(mean(ratio_bin == "= 100% (limiting)",
                              na.rm = TRUE) * 100, 1),
    mean_ratio   = round(mean(ratio, na.rm = TRUE), 1),
    sd_ratio     = round(sd(ratio,   na.rm = TRUE), 1),
    .groups      = "drop"
  ) %>%
  mutate(
    cell = paste0(pct_limiting, "% | ", mean_ratio, " (", sd_ratio, ")")
  ) %>%
  dplyr::select(Nutrients, member, ciudad_label, cell) %>%
  pivot_wider(
    names_from  = c(member, ciudad_label),
    values_from = cell,
    names_glue  = "{member} | {ciudad_label}"
  ) %>%
  arrange(match(Nutrients, nutrient_order)) %>%
  dplyr::select(
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

note <- tibble(
  Note = paste0(
    "Each cell reports: % of months in which the nutrient is limiting ",
    "(Opt/Rest = 100%) | Mean Opt/Rest ratio % (SD).\n",
    "The Opt/Rest ratio = (actual nutrient intake at the CoNA optimum / ",
    "minimum dietary requirement) \u00d7 100.\n",
    "A value of 100% indicates a limiting nutrient. ",
    "Values above 100% indicate the requirement is exceeded.\n",
    "Nutrients ordered by descending binding frequency. ",
    "Period: January 2019 -- December 2024. ",
    "CoNA = Cost of Nutritional Adequacy."
  )
)

##----------------------------------------------------------
## Save
##----------------------------------------------------------

write_xlsx(
  list(
    `Compact (pooled cities)` = tab_compact,
    `Full (by member x city)` = tab_full,
    `Notes`                   = note
  ),
  file.path(out_tabs, "table_optratio_summary_cona.xlsx")
)

message("Figure saved to: ", out_fig)
message("Table saved to : ", out_tabs)