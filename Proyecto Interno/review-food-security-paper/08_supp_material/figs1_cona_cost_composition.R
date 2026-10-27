# =========================================================
# FIGURE S1 (Supplementary) — Monthly cost decomposition of the CoNA
#                             by food item
#
# Stacked areas in levels: the height is the real per capita CoNA cost
# and each band is one food item's contribution. Levels rather than
# shares, because section 3.4 reports both the cost shares and the
# evolution of the series, and the stacked level shows both.
#
# Items selected in fewer than MIN_MONTHS_PCT of months are pooled into
# "Other": with 14 items the legend would otherwise run longer than the
# plot, and the marginal items carry under 1% of the cost each.
#
# Reads:  CONA_DIR/cona_results.rds
#         PREP_DIR/panel_food_paper.rds
#         PREP_DIR/deflator_monthly.rds
#
# Writes: SUPP_DIR/figS1_cona_cost_decomposition.png / .pdf
#         SUPP_DIR/figS1_cona_cost_shares.csv  (shares, for the text)
# =========================================================

source("00_config.R", encoding = "UTF-8")
source(file.path(REVIEW_DIR, "06_figures", "00_fig_config.R"))
library(tidyverse)
library(lubridate)
library(scales)

# All supplementary-material outputs go here
SUPP_DIR <- file.path(REVIEW_DIR, "08_supp_material")
dir.create(SUPP_DIR, recursive = TRUE, showWarnings = FALSE)

# Bind the verbs: the model functions load packages that mask them
summarise <- dplyr::summarise
filter    <- dplyr::filter
select    <- dplyr::select
rename    <- dplyr::rename
mutate    <- dplyr::mutate
n         <- dplyr::n

MIN_MONTHS_PCT <- 20   # items present in fewer months are pooled as "Other"

# -----------------------------------------------------------------------
# 1. Load
# -----------------------------------------------------------------------
cona <- readRDS(file.path(CONA_DIR, "cona_results.rds"))

data_paper <- readRDS(file.path(PREP_DIR, "panel_food_paper.rds")) %>%
  mutate(fecha = as.Date(fecha))

deflator <- readRDS(file.path(PREP_DIR, "deflator_monthly.rds")) %>%
  filter(ciudad != "NACIONAL") %>%
  select(ciudad, fecha, deflator)

# -----------------------------------------------------------------------
# 2. Cost per food item, per household member, per month
#    comp holds the selected quantity in grams; multiplying by the price
#    per 100 g recovers each item's contribution to the household cost.
# -----------------------------------------------------------------------
cost_item <- cona$comp %>%
  mutate(fecha = as.Date(fecha)) %>%
  left_join(data_paper %>% select(ciudad, fecha, articulo, precio_100g),
            by = c("ciudad", "fecha", "Food" = "articulo")) %>%
  mutate(cost = quantity / 100 * precio_100g)

# Items that failed to match a price would silently drop out of the total
n_unmatched <- sum(is.na(cost_item$cost))
if (n_unmatched > 0)
  warning(n_unmatched, " item-month rows had no matching price and were dropped. ",
          "Check that comp$Food matches panel_food_paper$articulo.")

cost_item <- cost_item %>% filter(!is.na(cost))

# Per capita: mean across the three household members
cost_pc <- cost_item %>%
  group_by(ciudad, fecha, Food) %>%
  summarise(cost_hh = sum(cost), .groups = "drop_last") %>%
  ungroup() %>%
  left_join(
    cona$comp %>% mutate(fecha = as.Date(fecha)) %>%
      distinct(ciudad, fecha, Demo_Group, Sex) %>%
      count(ciudad, fecha, name = "n_members"),
    by = c("ciudad", "fecha")) %>%
  mutate(cost_pc = cost_hh / n_members) %>%
  left_join(deflator, by = c("ciudad", "fecha")) %>%
  mutate(cost_real = cost_pc * deflator) %>%
  filter(fecha >= PAPER_START, fecha <= PAPER_END, !is.na(cost_real))

# -----------------------------------------------------------------------
# 3. Pool marginal items into "Other"
# -----------------------------------------------------------------------
n_months <- n_distinct(cost_pc$fecha)

presence <- cost_pc %>%
  distinct(ciudad, fecha, Food) %>%
  count(Food, name = "months") %>%
  mutate(pct_months = 100 * months / (n_months * n_distinct(cost_pc$ciudad)))

keep_items <- presence %>% filter(pct_months >= MIN_MONTHS_PCT) %>% pull(Food)

message(sprintf("  %d items kept, %d pooled as Other",
                length(keep_items), nrow(presence) - length(keep_items)))
print(as.data.frame(presence %>% arrange(desc(pct_months))), digits = 3)

# English labels, ordered by overall cost share so the stack reads
# from the largest band at the bottom
FOOD_LABS <- c(
  "LECHE PASTEURIZADA"       = "Pasteurised milk",
  "ARVEJA SECA"              = "Dried peas",
  "HARINA DE TRIGO"          = "Wheat flour",
  "MANTECA VEGETAL"          = "Vegetable shortening",
  "AZÚCAR NATURAL O MORENA"  = "Brown sugar",
  "HARINA PRECOCIDA"         = "Pre-cooked flour",
  "GUAYABAS"                 = "Guavas",
  "LENTEJAS"                 = "Lentils",
  "ARROZ PARA SECO"          = "Rice",
  "ACEITE DE SOYA O DE MAÍZ" = "Soybean or corn oil",
  "ZANAHORIA"                = "Carrot",
  "SAL"                      = "Salt",
  "GARBANZO"                 = "Chickpeas",
  "PANELA"                   = "Panela",
  "VISCERAS - HIGADO"        = "Liver",
  "CARNE DE RES CON HUESO"   = "Beef with bone",
  "NARANJAS"                 = "Oranges",
  "YUCA"                     = "Cassava")

plot_df <- cost_pc %>%
  mutate(item = if_else(Food %in% keep_items, Food, "OTHER"),
         item_lab = if_else(item == "OTHER", "Other",
                            dplyr::coalesce(unname(FOOD_LABS[item]), item))) %>%
  group_by(ciudad, fecha, item_lab) %>%
  summarise(cost_real = sum(cost_real), .groups = "drop") %>%
  mutate(ciudad_lbl = factor(CITY_LABS[ciudad],
                             levels = c("Bogotá", "Medellín", "Cali")))

# Order bands by total cost share, "Other" last
item_order <- plot_df %>%
  filter(item_lab != "Other") %>%
  group_by(item_lab) %>%
  summarise(tot = sum(cost_real), .groups = "drop") %>%
  arrange(desc(tot)) %>%
  pull(item_lab)

plot_df <- plot_df %>%
  mutate(item_lab = factor(item_lab, levels = c(item_order, "Other")))

# -----------------------------------------------------------------------
# 4. Cost shares, written out for the text in section 3.4
# -----------------------------------------------------------------------
shares <- cost_pc %>%
  group_by(ciudad, Food) %>%
  summarise(cost_total = sum(cost_real), .groups = "drop") %>%
  group_by(ciudad) %>%
  mutate(share_pct = 100 * cost_total / sum(cost_total)) %>%
  ungroup() %>%
  select(ciudad, Food, share_pct) %>%
  pivot_wider(names_from = ciudad, values_from = share_pct) %>%
  arrange(desc(BOGOTA))

print(as.data.frame(shares), digits = 3)
write_csv(shares, file.path(SUPP_DIR, "figS1_cona_cost_shares.csv"))

# -----------------------------------------------------------------------
# 5. Figure
# -----------------------------------------------------------------------
PALETTE <- c(
  "#1F4E79", "#C0504D", "#9BBB59", "#8064A2", "#4BACC6",
  "#F79646", "#2E75B6", "#A5A5A5", "#70AD47", "#FFC000",
  "#7F6000", "#264478", "#9E480E", "#636363", "#D9D9D9")

n_bands <- nlevels(plot_df$item_lab)
cols <- setNames(PALETTE[seq_len(n_bands)], levels(plot_df$item_lab))
cols["Other"] <- "#BFBFBF"

figS1 <- ggplot(plot_df,
                aes(x = fecha, y = cost_real, fill = item_lab)) +
  geom_area(colour = NA, linewidth = 0) +
  facet_wrap(~ ciudad_lbl, nrow = 1) +
  scale_fill_manual(values = cols, name = NULL,
                    guide = guide_legend(ncol = 1, reverse = FALSE)) +
  date_axis() +
  scale_y_continuous(labels = comma_format(big.mark = ","),
                     expand = expansion(mult = c(0, 0.03))) +
  labs(x = NULL, y = "Real COP / day (per capita)") +
  paper_theme() +
  theme(
    axis.text.x      = element_text(angle = 45, hjust = 1, size = 8),
    axis.text.y      = element_text(size = 8),
    axis.title.y     = element_text(size = 9),
    strip.text       = element_text(face = "bold", size = 10),
    legend.position  = "right",
    legend.text      = element_text(family = "serif", size = 8),
    legend.key.size  = unit(0.35, "cm"),
    legend.margin    = margin(0, 0, 0, 6),
    panel.spacing    = unit(0.4, "cm"))

ggsave(file.path(SUPP_DIR, "figS1_cona_cost_decomposition.png"),
       figS1, width = 23, height = 12, units = "cm", dpi = 400, bg = "white")
ggsave(file.path(SUPP_DIR, "figS1_cona_cost_decomposition.pdf"),
       figS1, width = 23, height = 12, units = "cm", bg = "white")

message("Figure S1 saved to: ", SUPP_DIR)