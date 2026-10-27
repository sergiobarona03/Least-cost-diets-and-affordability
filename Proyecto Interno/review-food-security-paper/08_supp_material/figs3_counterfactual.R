# =========================================================
# FIGURE S3 (Supplementary) — Counterfactual unaffordability:
#   "what if households devoted their entire income to food?"
#
# Section 3.6.2 reports that under this scenario unaffordability is
# confined to the first decile, where 10.6-15.8% still cannot afford the
# CoCA, 24.5-39.7% the CoNA and 41.3-60.6% the CoRD. This script
# recomputes those figures and draws the figure in one pass, so the
# estimation no longer depends on a script that was lost.
#
# The counterfactual replaces the food-expenditure share with the whole
# per capita income: a household is unaffordable when its annual per
# capita income falls below the annual per capita cost of the diet. The
# comparison is otherwise identical to the one in 03_income/
# 02_affordability.R, including the use of survey expansion factors.
#
# Reads:  INCOME_DIR/deciles_final.rds
#         HCOST_DIR/hcost_full.rds
#
# Writes: AFFORD_DIR/counterfactual_results.rds
#         SUPP_DIR/figS3_counterfactual.png / .pdf
#         SUPP_DIR/figS3_counterfactual_summary.csv
#
# CHECK BEFORE USING THE NUMBERS: the two conversion choices below are
# the ones that have to match 02_affordability.R, and I could not verify
# them against Afford_Exp(). (i) Annual per capita income is taken as the
# monthly figure times 12. (ii) Annual per capita diet cost is the daily
# figure times 365. If Afford_Exp() annualises differently — 360 days,
# or a monthly rather than annual comparison — the rates will shift and
# the two exercises stop being comparable. The script prints the implied
# ratio of the two magnitudes so the orders of magnitude can be checked.
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

DAYS_YEAR   <- 365
MONTHS_YEAR <- 12

# -----------------------------------------------------------------------
# 1. Load
# -----------------------------------------------------------------------
message("Loading inputs...")

deciles_final <- readRDS(file.path(INCOME_DIR, "deciles_final.rds"))
hcost_full    <- readRDS(file.path(HCOST_DIR,  "hcost_full.rds")) %>%
  mutate(fecha = as.Date(fecha))

message(sprintf("  deciles_final: %d rows | hcost_full: %d rows",
                nrow(deciles_final), nrow(hcost_full)))
message("  hcost_full columns: ", paste(names(hcost_full), collapse = ", "))
message("  deciles_final columns: ", paste(names(deciles_final), collapse = ", "))

# -----------------------------------------------------------------------
# 2. Income side — same recoding as 02_affordability.R
# -----------------------------------------------------------------------
income_df <- deciles_final %>%
  mutate(
    fecha  = ymd(paste(year, mes, "01", sep = "-")),
    ciudad = case_when(dominio == "MEDELLIN" ~ "MEDELLÍN",
                       dominio == "BOGOTA"   ~ "BOGOTÁ D.C.",
                       TRUE                  ~ dominio),
    deciles = paste0("Decil ", deciles),
    # The counterfactual budget: the whole per capita income, annualised
    income_pc_year = per_capita_income * MONTHS_YEAR,
    fex            = fex_c) %>%
  select(ciudad, year, mes, fecha, id_hogar, deciles,
         per_capita_income, income_pc_year, fex) %>%
  filter(!is.na(income_pc_year), !is.na(fex))

# -----------------------------------------------------------------------
# 3. Diet cost side — per capita daily cost, annualised
# -----------------------------------------------------------------------
cost_df <- hcost_full %>%
  mutate(ciudad = case_when(ciudad == "MEDELLIN" ~ "MEDELLÍN",
                            ciudad == "BOGOTA"   ~ "BOGOTÁ D.C.",
                            TRUE                 ~ ciudad),
         year = year(fecha), mes = month(fecha)) %>%
  group_by(model, ciudad, year, mes) %>%
  summarise(cost_pc_day = mean(per_capita, na.rm = TRUE), .groups = "drop") %>%
  mutate(cost_pc_year = cost_pc_day * DAYS_YEAR)

message(sprintf("  models: %s", paste(unique(cost_df$model), collapse = ", ")))

# Sanity check on the two annualisations: the ratio of a median income to
# a median diet cost should sit in a plausible range, not off by 10x
chk <- income_df %>%
  summarise(med_income_year = median(income_pc_year)) %>%
  bind_cols(cost_df %>% filter(model == "CoNA") %>%
              summarise(med_cost_year = median(cost_pc_year)))
message(sprintf(
  "  median annual per capita income: %s | median annual CoNA cost: %s | ratio %.1f",
  comma(round(chk$med_income_year)), comma(round(chk$med_cost_year)),
  chk$med_income_year / chk$med_cost_year))

# -----------------------------------------------------------------------
# 4. Counterfactual rate by decile, city, model and month
#    Weighted by expansion factors, as in the main exercise
# -----------------------------------------------------------------------
cf <- income_df %>%
  inner_join(cost_df, by = c("ciudad", "year", "mes"),
             relationship = "many-to-many") %>%
  mutate(unafford = as.integer(income_pc_year < cost_pc_year)) %>%
  group_by(model, ciudad, deciles, fecha, year, mes) %>%
  summarise(rate = 100 * sum(unafford * fex) / sum(fex),
            n_hh = n(),
            .groups = "drop")

if (nrow(cf) == 0)
  stop("No rows after joining income and cost. Check the city recoding ",
       "and that year/mes types match between the two sources.")

cf <- cf %>%
  mutate(
    model      = factor(model, levels = c("CoCA", "CoNA", "CoRD")),
    ciudad_lbl = factor(dplyr::case_when(
      ciudad == "BOGOTÁ D.C." ~ "Bogotá",
      ciudad == "MEDELLÍN"    ~ "Medellín",
      ciudad == "CALI"        ~ "Cali",
      TRUE                    ~ ciudad),
      levels = c("Bogotá", "Medellín", "Cali")),
    decile_num = as.integer(gsub("Decil ", "", deciles)))

saveRDS(cf, file.path(AFFORD_DIR, "counterfactual_results.rds"))

# -----------------------------------------------------------------------
# 5. Summary for the text in section 3.6.2
# -----------------------------------------------------------------------
summ_decile <- cf %>%
  group_by(model, ciudad_lbl, decile_num) %>%
  summarise(rate_mean = mean(rate), .groups = "drop") %>%
  pivot_wider(names_from = ciudad_lbl, values_from = rate_mean) %>%
  arrange(model, decile_num)

cat("\n  Counterfactual rate by decile, full-period mean:\n\n")
print(as.data.frame(summ_decile), digits = 3)

cat("\n  First decile, by model and city (the figures quoted in 3.6.2):\n\n")
print(as.data.frame(
  cf %>% filter(decile_num == 1) %>%
    group_by(model, ciudad_lbl) %>%
    summarise(rate_mean = mean(rate), rate_min = min(rate),
              rate_max = max(rate), .groups = "drop")), digits = 3)

cat("\n  Deepest decile reached by each model and city (mean rate > 0):\n\n")
print(as.data.frame(
  cf %>% group_by(model, ciudad_lbl, decile_num) %>%
    summarise(rate = mean(rate), .groups = "drop") %>%
    filter(rate > 0) %>%
    group_by(model, ciudad_lbl) %>%
    slice_max(decile_num, n = 1) %>%
    select(model, ciudad_lbl, decile_num, rate)), digits = 3)

write_csv(summ_decile, file.path(SUPP_DIR, "figS3_counterfactual_summary.csv"))

# -----------------------------------------------------------------------
# 6. Figure — monthly aggregate rate, distribution within each year
#    The aggregate rate is the simple mean across the ten deciles, as in
#    the main affordability exercise.
# -----------------------------------------------------------------------
cf_monthly <- cf %>%
  group_by(model, ciudad_lbl, fecha, year) %>%
  summarise(rate = mean(rate, na.rm = TRUE), .groups = "drop")

figS3 <- ggplot(cf_monthly,
                aes(x = factor(year), y = rate, fill = ciudad_lbl)) +
  geom_boxplot(position = position_dodge(width = 0.8),
               width = 0.65, alpha = 0.85,
               outlier.size = 0.8, outlier.alpha = 0.5,
               linewidth = 0.35) +
  facet_wrap(~ model, ncol = 3) +
  scale_fill_manual(
    values = c("Bogotá"   = unname(CITY_COLS["BOGOTA"]),
               "Medellín" = unname(CITY_COLS["MEDELLIN"]),
               "Cali"     = unname(CITY_COLS["CALI"])),
    name = NULL) +
  scale_y_continuous(labels = function(x) sprintf("%.0f%%", x),
                     expand = expansion(mult = c(0.04, 0.06))) +
  labs(x = NULL, y = "Counterfactual unaffordability rate (%)") +
  paper_theme() +
  theme(
    axis.text.x       = element_text(angle = 45, hjust = 1, size = 8),
    axis.text.y       = element_text(size = 8),
    axis.title.y      = element_text(size = 9),
    strip.text        = element_text(face = "bold", size = 10),
    legend.position   = "top",
    legend.direction  = "horizontal",
    legend.text       = element_text(family = "serif", size = 9),
    legend.key.width  = unit(1.2, "cm"),
    legend.background = element_rect(colour = "black", fill = "white",
                                     linewidth = 0.4),
    legend.margin     = margin(2, 8, 2, 8),
    panel.spacing     = unit(0.4, "cm"))

ggsave(file.path(SUPP_DIR, "figS3_counterfactual.png"),
       figS3, width = 20, height = 12, units = "cm", dpi = 400, bg = "white")
ggsave(file.path(SUPP_DIR, "figS3_counterfactual.pdf"),
       figS3, width = 20, height = 12, units = "cm", bg = "white")

message("\nFigure S3 saved to: ", SUPP_DIR)