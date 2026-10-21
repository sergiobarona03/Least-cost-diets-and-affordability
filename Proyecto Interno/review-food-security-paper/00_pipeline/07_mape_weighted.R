########################################################
## SCRIPT 07b — Cost-weighted price validation MAPE
##
## The global MAPE averages every item equally, but the diet metrics
## depend on a handful of foods. This script weights each food's
## validation MAPE by its share of the optimal diet's cost, so the
## reported error refers to the foods that actually determine CoCA and
## CoNA.
##
## Reads:  COCA_DIR/coca_results.rds        (Food, quantity, cost_day)
##         CONA_DIR/cona_results.rds        ($comp: Food, quantity)
##         FORECAST_DIR/prices_paper.rds    (precio_500g, to cost CoNA)
##         ROBUST_DIR/price_validation_item.csv  (MAPE_BASE, MAPE_S5)
##
## Writes: ROBUST_DIR/mape_weighted_global.csv
##         ROBUST_DIR/mape_weighted_item.csv
##         ROBUST_DIR/mape_weighted_coverage.csv
##
## Quantities are in grams; prices are per 500 g of edible portion, so
## cost = quantity / 500 * precio_500g. Verified against coca_results:
## 610.48 g of HARINA DE TRIGO at 1566.34 COP implies 1282.8 per 500 g.
########################################################

source("00_config.R")
library(tidyverse)

# -----------------------------------------------------------------------
# City labels differ between the models (BOGOTA) and the price panel
# (BOGOTÁ D.C.). Harmonise onto the price-panel spelling.
# -----------------------------------------------------------------------
city_to_panel <- c("BOGOTA"      = "BOGOTÁ D.C.",
                   "BOGOTÁ D.C." = "BOGOTÁ D.C.",
                   "CALI"        = "CALI",
                   "MEDELLIN"    = "MEDELLÍN",
                   "MEDELLÍN"    = "MEDELLÍN")

# -----------------------------------------------------------------------
# Load inputs
# -----------------------------------------------------------------------
message("Loading inputs...")

coca <- readRDS(file.path(COCA_DIR, "coca_results.rds"))
cona <- readRDS(file.path(CONA_DIR, "cona_results.rds"))
prices_paper <- readRDS(file.path(FORECAST_DIR, "prices_paper.rds"))

item_val <- read_csv(file.path(ROBUST_DIR, "price_validation_item.csv"),
                     show_col_types = FALSE) %>%
  select(ciudad, articulo, k, method, MAPE_BASE, MAPE_S5)

# -----------------------------------------------------------------------
# CoCA: cost by food is already in the results
# -----------------------------------------------------------------------
coca_cost <- coca %>%
  transmute(model    = "CoCA",
            ciudad   = unname(city_to_panel[ciudad]),
            articulo = Food,
            fecha,
            cost     = cost_day)

# -----------------------------------------------------------------------
# CoNA: comp holds quantities only, so cost them with the price panel
# -----------------------------------------------------------------------
price_lookup <- prices_paper %>%
  transmute(ciudad = unname(city_to_panel[ciudad]),
            articulo = articulo, fecha, precio_500g)

cona_cost <- cona$comp %>%
  transmute(model    = "CoNA",
            ciudad   = unname(city_to_panel[ciudad]),
            articulo = Food,
            fecha,
            quantity) %>%
  left_join(price_lookup, by = c("ciudad", "articulo", "fecha")) %>%
  mutate(cost = quantity / 500 * precio_500g)

n_cona_nomatch <- sum(is.na(cona_cost$cost))
if (n_cona_nomatch > 0)
  message(sprintf("  %d CoNA rows (%.1f%%) found no price in the panel",
                  n_cona_nomatch, 100 * n_cona_nomatch / nrow(cona_cost)))

# -----------------------------------------------------------------------
# Cost shares by food × city × model
# Summed over household members and months, so the share reflects each
# food's weight in total diet cost over the whole study period.
# -----------------------------------------------------------------------
shares <- bind_rows(coca_cost %>% select(model, ciudad, articulo, fecha, cost),
                    cona_cost %>% select(model, ciudad, articulo, fecha, cost)) %>%
  filter(!is.na(cost)) %>%
  group_by(model, ciudad, articulo) %>%
  summarise(cost_total = sum(cost), .groups = "drop") %>%
  group_by(model, ciudad) %>%
  mutate(share = cost_total / sum(cost_total)) %>%
  ungroup()

# -----------------------------------------------------------------------
# Join each food's validation MAPE
# -----------------------------------------------------------------------
weighted_item <- shares %>%
  left_join(item_val, by = c("ciudad", "articulo")) %>%
  mutate(contrib_BASE = share * MAPE_BASE,
         contrib_S5   = share * MAPE_S5) %>%
  arrange(model, ciudad, desc(share))

# Foods in the solutions with no validation MAPE (no observed price in the
# validation window). Their share is what the weighted figure cannot cover.
coverage <- weighted_item %>%
  group_by(model, ciudad) %>%
  summarise(share_matched   = sum(share[!is.na(MAPE_BASE)]),
            share_unmatched = sum(share[is.na(MAPE_BASE)]),
            n_foods         = n(),
            n_unmatched     = sum(is.na(MAPE_BASE)),
            .groups = "drop")

unmatched_list <- weighted_item %>%
  filter(is.na(MAPE_BASE)) %>%
  select(model, ciudad, articulo, share) %>%
  arrange(model, ciudad, desc(share))

# -----------------------------------------------------------------------
# Cost-weighted MAPE, renormalising over the matched share
# -----------------------------------------------------------------------
weighted_global <- weighted_item %>%
  filter(!is.na(MAPE_BASE)) %>%
  group_by(model, ciudad) %>%
  summarise(share_covered   = sum(share),
            MAPE_w_BASE     = sum(contrib_BASE) / sum(share),
            MAPE_w_S5       = sum(contrib_S5)   / sum(share),
            n_foods         = n(),
            .groups = "drop") %>%
  mutate(imp_pp = MAPE_w_BASE - MAPE_w_S5)

# -----------------------------------------------------------------------
# Console summary
# -----------------------------------------------------------------------
cat("\n", strrep("=", 64), "\n")
cat("  COST-WEIGHTED VALIDATION MAPE\n")
cat(strrep("=", 64), "\n\n")
print(as.data.frame(weighted_global), digits = 4)
cat("\n  Coverage (share of diet cost with a validated price):\n\n")
print(as.data.frame(coverage), digits = 4)

if (nrow(unmatched_list) > 0) {
  cat("\n  Foods in the solutions without a validation MAPE:\n\n")
  print(as.data.frame(unmatched_list), digits = 4)
}

cat("\n  Top cost contributors:\n\n")
print(as.data.frame(
  weighted_item %>% group_by(model, ciudad) %>% slice_max(share, n = 5) %>%
    ungroup() %>%
    select(model, ciudad, articulo, share, MAPE_BASE, MAPE_S5, contrib_BASE)),
  digits = 4)

# -----------------------------------------------------------------------
# Save (CSV only)
# -----------------------------------------------------------------------
write_csv(weighted_global, file.path(ROBUST_DIR, "mape_weighted_global.csv"))
write_csv(weighted_item,   file.path(ROBUST_DIR, "mape_weighted_item.csv"))
write_csv(coverage %>% left_join(unmatched_list, by = c("model", "ciudad")),
          file.path(ROBUST_DIR, "mape_weighted_coverage.csv"))

message("Done. Outputs in: ", ROBUST_DIR)