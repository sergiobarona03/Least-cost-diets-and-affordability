########################################################
## Section 5: Nutritionally adequate diets under
## observed consumption patterns
## Cost figures — validated deflated per capita costs
##
## Fig 1: Real per capita cost time series by alpha × city
## Fig 2: Cost premium E_IPC(alpha) - E* by alpha × city
## Fig 3: Monthly cost premium over time by alpha × city
## Table A: Mean real cost and premium by alpha × city
########################################################

library(tidyverse)
library(readxl)
library(lubridate)
library(scales)
library(writexl)

##----------------------------------------------------------
## Directories
##----------------------------------------------------------

base_dir <- "C:\\Users\\sergio.barona\\Desktop\\Least-cost-diets-and-affordability\\Proyecto Interno\\food-security-paper"
out_real <- file.path(base_dir, "output", "real")
out_fig  <- file.path(base_dir, "output", "cona-ipc")
out_tab  <- file.path(base_dir, "output", "cona-ipc")

##----------------------------------------------------------
## Load validated deflated per capita costs
##----------------------------------------------------------
ipc_real <- readRDS(file.path(out_real, "cona_ipc_real.rds"))

cost_pc <- ipc_real$per_capita %>%
  mutate(fecha = as.Date(fecha))

##----------------------------------------------------------
## Shared labels and aesthetics
##----------------------------------------------------------

city_labels <- c(
  "BOGOTA"   = "Bogotá",
  "MEDELLIN" = "Medellín",
  "CALI"     = "Cali"
)

# alpha = 0 is the CoNA benchmark — shown in grey
# alpha > 0 shown in progressive green (stricter = darker)
alpha_levels <- c("0", "0.25", "0.5", "0.75", "1")

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

alpha_labels <- c(
  "0"    = "\u03b1 = 0 (CoNA)",
  "0.25" = "\u03b1 = 0.25",
  "0.5"  = "\u03b1 = 0.50",
  "0.75" = "\u03b1 = 0.75",
  "1"    = "\u03b1 = 1.00"
)

city_colors <- c(
  "Bogotá"   = "#2C3E6B",
  "Cali"     = "#27AE60",
  "Medellín" = "#C0392B"
)

theme_q1 <- theme_bw(base_size = 10) +
  theme(
    text             = element_text(family = "serif"),
    plot.title       = element_text(face = "bold", size = 11,
                                    margin = margin(b = 4)),
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
## Prepare
##----------------------------------------------------------

cost_pc <- cost_pc %>%
  mutate(
    ciudad_label = recode(ciudad, !!!city_labels),
    alpha_chr    = as.character(alpha_val),
    alpha_fac    = factor(alpha_chr, levels = alpha_levels)
  )

cost_pts <- cost_pc %>% filter(month(fecha) == 12)

##----------------------------------------------------------
## Figure 1: Real per capita cost time series
##           Five lines (one per alpha), facet by city
##           alpha = 0 in grey = CoNA benchmark
##----------------------------------------------------------

fig1 <- ggplot(cost_pc,
               aes(x        = fecha,
                   y        = cost_pc_real,
                   color    = alpha_fac,
                   linetype = alpha_fac,
                   group    = alpha_fac)) +
  geom_line(linewidth = 0.6, alpha = 0.9) +
  geom_point(data = cost_pts,
             aes(shape = alpha_fac),
             size = 1.6, alpha = 0.9) +
  facet_wrap(~ ciudad_label, ncol = 3) +
  scale_color_manual(values    = alpha_palette,   labels = alpha_labels) +
  scale_linetype_manual(values = alpha_linetypes, labels = alpha_labels) +
  scale_shape_manual(values    = alpha_shapes,    labels = alpha_labels) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y",
               expand = c(0.01, 0)) +
  scale_y_continuous(labels = comma_format(prefix = "$", big.mark = ",")) +
  labs(
    title   = " ",
    caption = paste0(
      "Note: Per capita cost = mean real daily diet cost across household members ",
      "(adult male, adult female, female child).\n",
      "\u03b1 = 0 corresponds to the standard CoNA (no dietary pattern constraint). ",
      "Higher \u03b1 imposes increasingly strict IPC quantity share constraints.\n",
      "Costs deflated by the national CPI (base: December 2018)."
    ),
    x = NULL,
    y = "Real COP / day (per capita)"
  ) +
  theme_q1 +
  guides(color    = guide_legend(nrow = 1),
         linetype = guide_legend(nrow = 1),
         shape    = guide_legend(nrow = 1))

ggsave(file.path(out_fig, "fig_ipc_cost_series.png"),
       fig1, width = 8, height = 6, dpi = 300)
ggsave(file.path(out_fig, "fig_ipc_cost_series.pdf"),
       fig1, width = 8, height = 6)

##----------------------------------------------------------
## Figure 2: Period-mean cost premium by alpha × city
##           x = alpha, y = premium (%) over alpha = 0
##           One connected line per city
##----------------------------------------------------------

# Period mean by alpha × city
cost_mean <- cost_pc %>%
  group_by(alpha_val, alpha_chr, ciudad, ciudad_label) %>%
  summarize(
    mean_cost = mean(cost_pc_real, na.rm = TRUE),
    sd_cost   = sd(cost_pc_real,   na.rm = TRUE),
    .groups   = "drop"
  )

# E* = period mean at alpha = 0
cost_star <- cost_mean %>%
  filter(alpha_val == 0) %>%
  select(ciudad, cost_star = mean_cost)

cost_premium <- cost_mean %>%
  left_join(cost_star, by = "ciudad") %>%
  mutate(
    premium_cop = mean_cost - cost_star,
    premium_pct = (mean_cost / cost_star - 1) * 100
  )

fig2 <- ggplot(cost_premium,
               aes(x     = alpha_val,
                   y     = premium_pct,
                   color = ciudad_label,
                   group = ciudad_label)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2.5) +
  geom_hline(yintercept = 0, linetype = "dashed",
             color = "grey50", linewidth = 0.4) +
  scale_color_manual(values = city_colors) +
  scale_x_continuous(
    breaks = c(0, 0.25, 0.50, 0.75, 1.00),
    labels = c("0\n(CoNA)", "0.25", "0.50", "0.75", "1.00")
  ) +
  scale_y_continuous(labels = function(x) paste0(round(x, 1), "%")) +
  labs(
    title   = " ",
    caption = paste0(
      "Note: Cost premium = [E\u1d35\u1d3a\u1d36(\u03b1) \u2212 E*] / E* \u00d7 100, ",
      "where E* is the period mean of the CoNA cost (\u03b1 = 0).\n",
      "Period mean computed over all city\u2013month observations, 2019\u20132024. ",
      "Costs deflated by the national CPI (base: December 2018)."
    ),
    x = expression(alpha),
    y = "Cost premium over CoNA (%)"
  ) +
  theme_q1

ggsave(file.path(out_fig, "fig_ipc_cost_premium.png"),
       fig2, width = 8, height = 6, dpi = 300)
ggsave(file.path(out_fig, "fig_ipc_cost_premium.pdf"),
       fig2, width = 8, height = 6)

##----------------------------------------------------------
## Figure 3: Monthly cost premium over time
##           Shows how the premium evolves month by month
##           Facet: city | Color/line: alpha
##           Helps identify if the premium is stable or
##           varies with the inflationary episode
##----------------------------------------------------------

# Monthly premium relative to alpha = 0 in that same month
cost_star_monthly <- cost_pc %>%
  filter(alpha_val == 0) %>%
  select(ciudad, ciudad_label, fecha, cost_star_m = cost_pc_real)

premium_monthly <- cost_pc %>%
  filter(alpha_val > 0) %>%
  left_join(cost_star_monthly, by = c("ciudad", "ciudad_label", "fecha")) %>%
  mutate(
    premium_pct = (cost_pc_real / cost_star_m)
  )

premium_pts <- premium_monthly %>% filter(month(fecha) == 12)

fig3 <- ggplot(premium_monthly,
               aes(x        = fecha,
                   y        = premium_pct,
                   color    = alpha_fac,
                   linetype = alpha_fac,
                   group    = alpha_fac)) +
  geom_hline(yintercept = 1, linetype = "longdash",
             color = "grey50", linewidth = 0.35) +
  geom_line(linewidth = 0.6, alpha = 0.9) +
  geom_point(data = premium_pts,
             aes(shape = alpha_fac),
             size = 1.6, alpha = 0.9) +
  facet_wrap(~ ciudad_label, ncol = 3) +
  scale_color_manual(values    = alpha_palette[2:5],
                     labels    = alpha_labels[2:5])    +
  scale_linetype_manual(values = alpha_linetypes[2:5],
                        labels = alpha_labels[2:5])    +
  scale_shape_manual(values    = alpha_shapes[2:5],
                     labels    = alpha_labels[2:5])    +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y",
               expand = c(0.01, 0)) +
  scale_y_continuous(labels = function(x) paste0(round(x, 3), "%")) +
  labs(
    title   = " ",
    caption = paste0(
      "Note: Monthly cost premium = [E\u1d35\u1d3a\u1d36(\u03b1, t) \u2212 E*(t)] / E*(t) \u00d7 100, ",
      "where E*(t) is the CoNA cost in month t (\u03b1 = 0).\n",
      "Long-dashed grey line at 0 indicates no premium over CoNA. ",
      "Costs deflated by the national CPI (base: December 2018)."
    ),
    x = NULL,
    y = "Monthly cost premium over CoNA (%)"
  ) +
  theme_q1 +
  guides(color    = guide_legend(nrow = 1),
         linetype = guide_legend(nrow = 1),
         shape    = guide_legend(nrow = 1))

ggsave(file.path(out_fig, "fig_ipc_premium_monthly.png"),
       fig3, width = 8, height = 6, dpi = 300)
ggsave(file.path(out_fig, "fig_ipc_premium_monthly.pdf"),
       fig3, width = 8, height = 6)

##----------------------------------------------------------
## Table A: Mean real per capita cost and premium by alpha × city
##----------------------------------------------------------

table_a <- cost_premium %>%
  mutate(
    mean_cost   = round(mean_cost,   0),
    sd_cost     = round(sd_cost,     0),
    cost_star   = round(cost_star,   0),
    premium_cop = round(premium_cop, 0),
    premium_pct = round(premium_pct, 2)
  ) %>%
  select(
    Alpha                          = alpha_val,
    City                           = ciudad_label,
    `Mean CoNA-IPC real (COP/day)` = mean_cost,
    `SD`                           = sd_cost,
    `CoNA cost (alpha = 0) (COP)`  = cost_star,
    `Premium (COP)`                = premium_cop,
    `Premium (%)`                  = premium_pct
  ) %>%
  arrange(City, Alpha)

write_xlsx(
  list(`Table A - Cost premium by alpha` = table_a),
  file.path(out_tab, "table_cona_ipc_cost_premium.xlsx")
)

