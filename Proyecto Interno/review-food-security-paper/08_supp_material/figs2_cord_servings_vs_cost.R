# =========================================================
# FIGURE S2 (Supplementary) — Servings and cost shares of the CoRD by
#                             GABA food group
#
# Section 3.4 reports an asymmetry between volume and economic weight:
# dairy carries 63-68% of the CoRD cost on 4.2-4.3 daily servings, while
# fats, the largest group by volume at 4.5-4.7 servings, carry 3-5%.
# Paired horizontal bars put both shares on the same percentage scale so
# that asymmetry is the first thing the reader sees. Whiskers give the
# 5th-95th percentile of each share across the 72 monthly observations.
#
# Reads:  CORD_DIR/cord_results.rds
#         PREP_DIR/panel_food_paper.rds
#         PREP_DIR/deflator_monthly.rds
#
# Writes: SUPP_DIR/figS2_cord_servings_vs_cost.png / .pdf
#         SUPP_DIR/figS2_cord_group_shares.csv
# =========================================================

source("00_config.R", encoding = "UTF-8")
source(file.path(REVIEW_DIR, "06_figures", "00_fig_config.R"))
library(tidyverse)
library(lubridate)
library(scales)

summarise <- dplyr::summarise
filter    <- dplyr::filter
select    <- dplyr::select
rename    <- dplyr::rename
mutate    <- dplyr::mutate
count     <- dplyr::count
n         <- dplyr::n

SUPP_DIR <- file.path(REVIEW_DIR, "08_supp_material")
dir.create(SUPP_DIR, recursive = TRUE, showWarnings = FALSE)

# -----------------------------------------------------------------------
# 1. Load
# -----------------------------------------------------------------------
cord <- readRDS(file.path(CORD_DIR, "cord_results.rds"))

data_paper <- readRDS(file.path(PREP_DIR, "panel_food_paper.rds")) %>%
  mutate(fecha = as.Date(fecha))

deflator <- readRDS(file.path(PREP_DIR, "deflator_monthly.rds")) %>%
  filter(ciudad != "NACIONAL") %>%
  select(ciudad, fecha, deflator)

comp <- cord$comp %>% mutate(fecha = as.Date(fecha))

# -----------------------------------------------------------------------
# 2. Cost per selected item, as constructed in 03_cord.R
# -----------------------------------------------------------------------
cost_item <- comp %>%
  left_join(
    data_paper %>%
      select(ciudad, fecha, articulo, precio_100g,
             gramos_g_1_intercambio_1_intercambio),
    by = c("ciudad", "fecha", "Food" = "articulo")) %>%
  mutate(
    serving_g     = suppressWarnings(as.numeric(gramos_g_1_intercambio_1_intercambio)),
    price_serving = precio_100g * serving_g / 100,
    cost          = Number_Serving * price_serving)

n_unmatched <- sum(is.na(cost_item$cost))
if (n_unmatched > 0)
  warning(n_unmatched, " item-month rows had no matching price or serving size ",
          "and were dropped; group shares would otherwise rest on an ",
          "incomplete base.")

cost_item <- cost_item %>% filter(!is.na(cost))

# -----------------------------------------------------------------------
# 3. Per capita servings and cost by group, city and month
# -----------------------------------------------------------------------
n_members <- comp %>%
  distinct(ciudad, fecha, Demo_Group, Sex) %>%
  count(ciudad, fecha, name = "n_members")

by_group_month <- cost_item %>%
  group_by(ciudad, fecha, Group) %>%
  summarise(servings_hh = sum(Number_Serving),
            cost_hh     = sum(cost),
            .groups = "drop") %>%
  left_join(n_members, by = c("ciudad", "fecha")) %>%
  left_join(deflator,  by = c("ciudad", "fecha")) %>%
  mutate(servings_pc = servings_hh / n_members,
         cost_real   = (cost_hh / n_members) * deflator) %>%
  filter(fecha >= PAPER_START, fecha <= PAPER_END, !is.na(cost_real))

totals <- by_group_month %>%
  group_by(ciudad, fecha) %>%
  summarise(cost_tot = sum(cost_real),
            serv_tot = sum(servings_pc), .groups = "drop")

shares <- by_group_month %>%
  left_join(totals, by = c("ciudad", "fecha")) %>%
  mutate(cost_share = 100 * cost_real / cost_tot,
         serv_share = 100 * servings_pc / serv_tot) %>%
  group_by(ciudad, Group) %>%
  summarise(servings      = mean(servings_pc),
            servings_sd   = sd(servings_pc),
            cost          = mean(cost_real),
            cost_share    = mean(cost_share),
            cost_share_sd = sd(cost_share),
            cost_share_p5 = quantile(cost_share, 0.05),
            cost_share_p95 = quantile(cost_share, 0.95),
            serv_share    = mean(serv_share),
            serv_share_sd = sd(serv_share),
            serv_share_p5 = quantile(serv_share, 0.05),
            serv_share_p95 = quantile(serv_share, 0.95),
            .groups = "drop")

cat("\n  CoRD servings and cost shares by GABA group:\n\n")
print(as.data.frame(shares %>% arrange(ciudad, desc(cost_share))), digits = 4)
write_csv(shares, file.path(SUPP_DIR, "figS2_cord_group_shares.csv"))

# -----------------------------------------------------------------------
# 4. English group labels
# -----------------------------------------------------------------------
GROUP_LABS <- c(
  "Leche y productos lácteos"                            = "Milk and dairy",
  "Carnes, huevos, leguminosas, frutos secos y semillas" = "Meats, eggs, legumes",
  "Cereales, raíces, tubérculos y plátanos"              = "Cereals, roots, tubers",
  "Frutas"                                               = "Fruits",
  "Verduras"                                             = "Vegetables",
  "Grasas"                                               = "Fats",
  "Azúcares"                                             = "Sugars")

# Long form, carrying the 5th-95th percentile band of each share across
# the 72 monthly observations as the dispersion measure. Percentiles
# rather than +/- 1 SD because several shares sit close to zero, where a
# symmetric interval would extend below it.
plot_df <- shares %>%
  mutate(group_lab  = dplyr::coalesce(unname(GROUP_LABS[Group]), Group),
         ciudad_lbl = factor(CITY_LABS[ciudad],
                             levels = c("Bogotá", "Medellín", "Cali"))) %>%
  select(ciudad_lbl, group_lab,
         cost_share, cost_share_p5, cost_share_p95,
         serv_share, serv_share_p5, serv_share_p95) %>%
  pivot_longer(
    cols = c(cost_share, serv_share),
    names_to = "measure", values_to = "share") %>%
  mutate(
    lo = if_else(measure == "cost_share", cost_share_p5,  serv_share_p5),
    hi = if_else(measure == "cost_share", cost_share_p95, serv_share_p95),
    measure = factor(measure,
                     levels = c("cost_share", "serv_share"),
                     labels = c("Share of cost", "Share of servings"))) %>%
  select(ciudad_lbl, group_lab, measure, share, lo, hi)

# Groups ordered by cost share, largest at the top
group_order <- shares %>%
  group_by(Group) %>%
  summarise(cs = mean(cost_share), .groups = "drop") %>%
  arrange(cs) %>%
  mutate(lab = dplyr::coalesce(unname(GROUP_LABS[Group]), Group)) %>%
  pull(lab)

plot_df <- plot_df %>%
  mutate(group_lab = factor(group_lab, levels = group_order))

# -----------------------------------------------------------------------
# 5. Figure
# -----------------------------------------------------------------------
figS2 <- ggplot(plot_df,
                aes(x = share, y = group_lab, fill = measure)) +
  geom_col(position = position_dodge(width = 0.75),
           width = 0.68, colour = "grey25", linewidth = 0.2) +
  facet_wrap(~ ciudad_lbl, nrow = 1) +
  scale_fill_manual(values = c("Share of cost"     = "#1F4E79",
                               "Share of servings" = "#C8A35A"),
                    name = NULL) +
  scale_x_continuous(labels = function(x) sprintf("%.0f%%", x),
                     expand = expansion(mult = c(0, 0.06))) +
  labs(x = "Share of total CoRD (%)", y = NULL) +
  paper_theme() +
  theme(
    axis.text.x      = element_text(size = 8),
    axis.text.y      = element_text(size = 8),
    axis.title.x     = element_text(size = 9),
    strip.text       = element_text(face = "bold", size = 10),
    panel.grid.major.y = element_blank(),
    legend.position  = "top",
    legend.direction = "horizontal",
    legend.text      = element_text(family = "serif", size = 9),
    legend.key.size  = unit(0.35, "cm"),
    legend.background = element_rect(colour = "black", fill = "white",
                                     linewidth = 0.4),
    legend.margin    = margin(2, 8, 2, 8),
    panel.spacing    = unit(0.45, "cm"))

ggsave(file.path(SUPP_DIR, "figS2_cord_servings_vs_cost.png"),
       figS2, width = 18, height = 9, units = "cm", dpi = 400, bg = "white")
ggsave(file.path(SUPP_DIR, "figS2_cord_servings_vs_cost.pdf"),
       figS2, width = 18, height = 9, units = "cm", bg = "white")

message("Figure S2 saved to: ", SUPP_DIR)
