########################################################
## Section 5: Nutritionally adequate diets under
## observed consumption patterns
## Affordability analysis — CoNA-IPC per capita nominal cost
## vs household per capita income by decile
##
## Fig 1: Non-affordability rate by decile × alpha × city (heatmap)
## Fig 2: Overall non-affordability rate by alpha × city (time series)
########################################################

library(tidyverse)
library(readxl)
library(lubridate)
library(scales)
library(Hmisc)
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
out_ipc    <- file.path(base_dir, "food-security-paper", "output", "cona-ipc")
out_real   <- file.path(base_dir, "food-security-paper", "output", "real")
income_dir <- file.path(base_dir, "food-security-paper", "output", "income_col")
out_fig    <- out_ipc

##----------------------------------------------------------
## Load CoNA-IPC nominal per capita cost
## Use nominal cost to compare with nominal income
##----------------------------------------------------------

ipc_real <- readRDS(file.path(out_real, "cona_ipc_real.rds"))

cost_pc_nom <- ipc_real$per_capita %>%
  mutate(fecha = as.Date(fecha)) %>%
  select(ciudad, fecha, year, mes, alpha_val, cost_pc_nominal) %>%
  mutate(cost_pc_nominal_year = cost_pc_nominal*365)

##----------------------------------------------------------
## Load income data — monthly deciles
##----------------------------------------------------------

income_data <- read.csv(file.path(income_dir, "deciles_food_income.csv")) %>%
  mutate(fecha = as.Date(paste(year, mes, "01", sep = "-")))

##----------------------------------------------------------
## Compute monthly weighted decile means of per_capita_income
## and food_exp_per_capita using wtd.quantile deciles
##----------------------------------------------------------

income_monthly <- income_data  %>% distinct() %>% select(-deciles) %>%
  dplyr::group_by(dominio, year, mes, fecha) %>%
  mutate(
    deciles_wtd = as.integer(cut(
      per_capita_income,
      breaks         = wtd.quantile(per_capita_income, weights = fex_c,
                                    probs = seq(0, 1, 0.1), na.rm = TRUE),
      labels         = 1:10,
      include.lowest = TRUE,
      right          = TRUE
    ))
  ) %>% dplyr::rename(deciles = deciles_wtd)

##----------------------------------------------------------
## City alignment
##----------------------------------------------------------

city_labels <- c(
  "BOGOTA"   = "Bogotá",
  "MEDELLIN" = "Medellín",
  "CALI"     = "Cali"
)

income_monthly <- income_monthly %>%
  dplyr::rename(ciudad = dominio) %>%
  mutate(ciudad_label = recode(ciudad, !!!city_labels))

cost_pc_nom <- cost_pc_nom %>%
  mutate(ciudad_label = recode(ciudad, !!!city_labels))

##----------------------------------------------------------
## Join: cost × income by city × month × decile × alpha
## Non-affordability: household cannot afford diet if
##   per_capita_income < cost_pc_nominal
##----------------------------------------------------------

afford_ipc = income_monthly %>% filter(fecha >= "2019-01-01") %>%
  left_join(cost_pc_nom[c("alpha_val","ciudad", "ciudad_label", "fecha", "year", "mes",
                          "cost_pc_nominal_year")],
            by = c("ciudad", "ciudad_label", "fecha", "year", "mes")) %>%
  filter(!is.na(deciles)) %>%
  mutate(
    cannot_afford = as.integer(food_exp_per_capita_year < cost_pc_nominal_year),
    alpha_chr     = as.character(alpha_val),
    alpha_fac     = factor(alpha_chr,
                           levels = c("0", "0.25", "0.5", "0.75", "1"))
  )

##----------------------------------------------------------
## Shared aesthetics
##----------------------------------------------------------

alpha_labels <- c(
  "0"    = "\u03b1 = 0 (CoNA)",
  "0.25" = "\u03b1 = 0.25",
  "0.5"  = "\u03b1 = 0.50",
  "0.75" = "\u03b1 = 0.75",
  "1"    = "\u03b1 = 1.00"
)

alpha_palette <- c(
  "0"    = "grey55",
  "0.25" = "#A9DFBF",
  "0.5"  = "#52BE80",
  "0.75" = "#1E8449",
  "1"    = "#0B5A2A"
)

alpha_linetypes <- c(
  "0"    = "solid",
  "0.25" = "dashed",
  "0.5"  = "dashed",
  "0.75" = "dashed",
  "1"    = "dashed"
)

alpha_shapes <- c(
  "0"    = 16,
  "0.25" = 17,
  "0.5"  = 15,
  "0.75" = 18,
  "1"    = 8
)

decile_order <- paste0("Decil ", 1:10)

rate_breaks  <- c(0, 10, 25, 50, 75, 100)
rate_labels  <- c("0\u201310%", "10\u201325%", "25\u201350%",
                  "50\u201375%", "75\u2013100%")
rate_palette <- c(
  "0\u201310%"   = "#F7F7F7",
  "10\u201325%"  = "#CCDAEA",
  "25\u201350%"  = "#7FAED4",
  "50\u201375%"  = "#2C6FAC",
  "75\u2013100%" = "#0D2D5A"
)

theme_q1 <- theme_bw(base_size = 10) +
  theme(
    text             = element_text(family = "serif"),
    plot.caption     = element_text(size = 7.5, color = "grey45",
                                    hjust = 0, margin = margin(t = 6)),
    axis.title       = element_text(size = 9),
    axis.text        = element_text(size = 8),
    axis.text.x      = element_text(angle = 45, hjust = 1),
    legend.position  = "bottom",
    legend.title     = element_blank(),
    legend.text      = element_text(size = 9),
    legend.key.width = unit(1.0, "cm"),
    panel.grid.major = element_line(color = "grey90", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "grey96", color = "grey70"),
    strip.text       = element_text(face = "bold", size = 9),
    plot.margin      = margin(6, 8, 6, 6)
  )

##----------------------------------------------------------
## Figure 1: Non-affordability heatmap by decile × alpha × city
##           x: fecha | y: decile | fill: rate (discrete)
##           facet: alpha (rows) × city (columns)
##----------------------------------------------------------

heatmap_data <- afford_ipc %>%
  mutate(decile_label = factor(paste0("Decil ", deciles),
                               levels = decile_order)) %>%
  group_by(ciudad_label, fecha, alpha_fac, decile_label) %>%
  dplyr::summarize(
    rate = mean(cannot_afford, na.rm = TRUE) * 100,
    .groups = "drop"
  ) %>%
  mutate(
    rate_bin = cut(rate,
                   breaks         = rate_breaks,
                   labels         = rate_labels,
                   include.lowest = TRUE,
                   right          = TRUE),
    rate_bin = factor(rate_bin, levels = rate_labels)
  ) %>% filter(!alpha_fac %in% c(0, 0.25))

fig1 <- ggplot(heatmap_data,
               aes(x = fecha, y = decile_label, fill = rate_bin)) +
  geom_tile(color = NA) +
  facet_grid(alpha_fac ~ ciudad_label,
             labeller = labeller(alpha_fac = alpha_labels)) +
  scale_fill_manual(
    values   = rate_palette,
    name     = "Non-affordability rate",
    na.value = "grey92",
    drop     = FALSE
  ) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y",
               expand = c(0, 0)) +
  labs(
    title   = " ",
    caption = paste0(
      "Note: Non-affordability rate = share of months in which mean per capita household income ",
      "falls below the per capita CoNA-IPC cost.\n",
      "Deciles computed monthly within each city using per capita household income ",
      "weighted by the GEIH expansion factor.\n",
      "\u03b1 = 0 corresponds to the standard CoNA. Higher \u03b1 imposes stricter IPC quantity ",
      "share constraints."
    ),
    x = NULL,
    y = NULL
  ) +
  theme_q1 +
  theme(
    panel.grid       = element_blank(),
    legend.key.size  = unit(0.45, "cm"),
    legend.key.width = unit(0.9, "cm")
  ) +
  guides(fill = guide_legend(nrow = 1))

ggsave(file.path(out_fig, "fig_ipc_afford_heatmap.png"),
       fig1, width = 10, height = 8, dpi = 300)
ggsave(file.path(out_fig, "fig_ipc_afford_heatmap.pdf"),
       fig1, width = 10, height = 8)

##----------------------------------------------------------
## Figure 2: Overall non-affordability rate by alpha × city
##           Mean across deciles by city × month × alpha
##           x: fecha | y: rate | color/linetype: alpha
##           facet: city (columns)
##----------------------------------------------------------

overall_rate <- afford_ipc %>%
  dplyr::group_by(ciudad_label, fecha, alpha_fac) %>%
  dplyr::summarize(
    rate_overall = mean(cannot_afford, na.rm = TRUE) * 100,
    .groups      = "drop"
  )

overall_pts <- overall_rate %>% filter(month(fecha) == 12)

fig2 <- ggplot(overall_rate,
               aes(x        = fecha,
                   y        = rate_overall,
                   color    = alpha_fac,
                   linetype = alpha_fac,
                   group    = alpha_fac)) +
  geom_line(linewidth = 0.6, alpha = 0.9) +
  geom_point(data = overall_pts,
             aes(shape = alpha_fac),
             size = 1.6, alpha = 0.9) +
  facet_wrap(~ ciudad_label, ncol = 3) +
  scale_color_manual(values    = alpha_palette,   labels = alpha_labels) +
  scale_linetype_manual(values = alpha_linetypes, labels = alpha_labels) +
  scale_shape_manual(values    = alpha_shapes,    labels = alpha_labels) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y",
               expand = c(0.01, 0)) +
  scale_y_continuous(labels = function(x) paste0(x, "%"),
                     limits = c(0, NA)) +
  labs(
    title   = " ",
    caption = paste0(
      "Note: Overall non-affordability rate = mean share of households unable to afford ",
      "the CoNA-IPC diet, averaged across income deciles within each city\u2013month.\n",
      "A household cannot afford the diet if its per capita income falls below the per ",
      "capita CoNA-IPC cost.\n",
      "\u03b1 = 0 corresponds to the standard CoNA. Higher \u03b1 imposes stricter IPC ",
      "quantity share constraints."
    ),
    x = NULL,
    y = "Non-affordability rate (%)"
  ) +
  theme_q1 +
  guides(color    = guide_legend(nrow = 1),
         linetype = guide_legend(nrow = 1),
         shape    = guide_legend(nrow = 1))

ggsave(file.path(out_fig, "fig_ipc_afford_overall.png"),
       fig2, width = 8, height = 6, dpi = 300)
ggsave(file.path(out_fig, "fig_ipc_afford_overall.pdf"),
       fig2, width = 8, height = 6)

message("Affordability figures saved to: ", out_fig)