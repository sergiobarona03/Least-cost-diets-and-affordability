########################################################
## SCRIPT 06 — Price validation (MAPE)
##
## Reads:
##   cache/: dane.rds
##   price_forecasting/prices_validation_panel.csv
##   price_forecasting/lambda_final.csv
##   price_forecasting/params_optimal.csv
##
## Computes MAPE of BASE and S5 vs observed DANE prices
## over Feb 2016 – Mar 2018.
##
## Writes to .../robust/:
##   final_validation_global.csv
##   final_validation_by_method.csv
##   final_validation_subclass.csv
##   final_validation_item.csv
##   final_validation_results.xlsx
##   validation_plots/   ← time series per sub-class × city
##   fig_final_mape_by_subclass.png
##   fig_validation_overview.png
########################################################

library(tidyverse)
library(writexl)

# -----------------------------------------------------------------------
# 1. Directories
# -----------------------------------------------------------------------
dirs <- c(
  "C:\\Users\\sergio.barona\\Desktop\\Least-cost-diets-and-affordability\\Proyecto Interno",
  "C:/Users/Portatil/Desktop/Least-cost-diets-and-affordability/Proyecto Interno",
  "C:/Users/danie/OneDrive/Escritorio/Least-cost-diets-and-affordability/Proyecto Interno"
)
base_dir <- dirs[dir.exists(dirs)][1]
if (is.na(base_dir)) stop("No base directory found.")

cache_dir    <- file.path(base_dir,
                          "food-security-paper/output/price_forecasting/cache")
out_forecast <- file.path(base_dir,
                          "food-security-paper/output/price_forecasting")
robust_dir   <- file.path(base_dir, "food-security-paper/robust")
val_plot_dir <- file.path(robust_dir, "validation_plots")

dir.create(robust_dir,   recursive = TRUE, showWarnings = FALSE)
dir.create(val_plot_dir, recursive = TRUE, showWarnings = FALSE)

VAL_START <- as.Date("2016-02-01")
VAL_END   <- as.Date("2018-03-01")
T0        <- as.Date("2016-01-01")

COL_OBS   <- "#2C3E50"
COL_BASE  <- "#95A5A6"
COL_DCITY <- "#E74C3C"
COL_VLINE <- "#2E5FA3"

city_labels <- c("BOGOTÁ D.C." = "Bogotá",
                 "CALI"        = "Cali",
                 "MEDELLÍN"    = "Medellín")

safe_name <- function(x) {
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  x <- gsub("[^A-Za-z0-9_]+", "_", x)
  x <- gsub("_+|^_|_$", "_", x)
  x
}

# -----------------------------------------------------------------------
# 2. Load inputs
# -----------------------------------------------------------------------
message("Loading inputs...")

dane <- readRDS(file.path(cache_dir, "dane.rds"))

val_panel <- read_csv(
  file.path(out_forecast, "prices_validation_panel.csv"),
  show_col_types = FALSE) %>%
  mutate(fecha = as.Date(fecha))

lambda_final <- read_csv(
  file.path(out_forecast, "lambda_final.csv"),
  show_col_types = FALSE)

params <- read_csv(
  file.path(out_forecast, "params_optimal.csv"),
  show_col_types = FALSE)

R2_MIN  <- params$R2_MIN[1]
LAM_MIN <- params$LAM_MIN[1]
LAM_MAX <- params$LAM_MAX[1]

# Observed prices for validation
obs_val <- dane %>%
  filter(fecha >= VAL_START, fecha <= VAL_END, !is.na(precio_500g)) %>%
  select(ciudad = nombre_ciudad, articulo, fecha,
         price_obs = precio_500g)

obs_full <- dane %>%
  filter(!is.na(precio_500g)) %>%
  select(ciudad = nombre_ciudad, articulo, fecha,
         price_obs = precio_500g)

# -----------------------------------------------------------------------
# 3. Compute APEs
# -----------------------------------------------------------------------
message("Computing APEs...")

val_ape <- val_panel %>%
  filter(fecha >= VAL_START) %>%
  inner_join(obs_val, by = c("ciudad", "articulo", "fecha")) %>%
  left_join(lambda_final %>%
              select(ciudad, articulo, subclase_ipc, k,
                     lambda = lambda_final, r2_train = r2),
            by = c("ciudad", "articulo")) %>%
  mutate(
    ape_BASE  = abs((price_BASE  - price_obs) / price_obs) * 100,
    ape_final = abs((price_final - price_obs) / price_obs) * 100)

global_val <- val_ape %>%
  summarise(
    n_obs                = n(),
    MAPE_BASE            = mean(ape_BASE,  na.rm = TRUE),
    MAPE_final           = mean(ape_final, na.rm = TRUE),
    max_BASE             = max(ape_BASE,   na.rm = TRUE),
    max_final            = max(ape_final,  na.rm = TRUE),
    pct_final_beats_BASE = mean(ape_final < ape_BASE, na.rm=TRUE) * 100,
    imp_vs_BASE          = MAPE_BASE - MAPE_final)

global_by_method <- val_ape %>%
  group_by(method) %>%
  summarise(
    n_series   = n_distinct(paste(ciudad, articulo)),
    n_obs      = n(),
    MAPE_BASE  = mean(ape_BASE,  na.rm = TRUE),
    MAPE_final = mean(ape_final, na.rm = TRUE),
    imp        = MAPE_BASE - MAPE_final,
    .groups    = "drop") %>%
  arrange(method)

subclass_val <- val_ape %>%
  group_by(subclase_ipc, k, method) %>%
  summarise(
    n_items    = n_distinct(articulo),
    n_obs      = n(),
    MAPE_BASE  = mean(ape_BASE,  na.rm = TRUE),
    MAPE_final = mean(ape_final, na.rm = TRUE),
    imp        = MAPE_BASE - MAPE_final,
    .groups    = "drop") %>%
  arrange(desc(MAPE_BASE))

item_val <- val_ape %>%
  group_by(ciudad, articulo, subclase_ipc, k,
           method, lambda, r2_train) %>%
  summarise(
    n_months   = n(),
    MAPE_BASE  = mean(ape_BASE,  na.rm = TRUE),
    MAPE_final = mean(ape_final, na.rm = TRUE),
    imp        = MAPE_BASE - MAPE_final,
    .groups    = "drop") %>%
  arrange(desc(MAPE_BASE))

message(sprintf(
  "  Global MAPE: BASE=%.4f%% → S5=%.4f%% (%+.4f pp)",
  global_val$MAPE_BASE, global_val$MAPE_final,
  global_val$imp_vs_BASE))

# -----------------------------------------------------------------------
# 4. Time series plots
# -----------------------------------------------------------------------
message("Building time series plots...")

subclass_city_combos <- val_panel %>%
  filter(fecha >= VAL_START) %>%
  left_join(lambda_final %>% select(ciudad, articulo, subclase_ipc, k, method_label),
            by = c("ciudad", "articulo")) %>%
  distinct(subclase_ipc, ciudad, k, method_label)

for (i in seq_len(nrow(subclass_city_combos))) {
  
  sub_ipc   <- subclass_city_combos$subclase_ipc[i]
  city_name <- subclass_city_combos$ciudad[i]
  k_val     <- subclass_city_combos$k[i]
  method_i  <- subclass_city_combos$method_label[i]
  
  items_here <- val_panel %>%
    filter(ciudad == city_name) %>%
    left_join(lambda_final %>% select(ciudad, articulo, subclase_ipc),
              by = c("ciudad", "articulo")) %>%
    filter(subclase_ipc == sub_ipc) %>%
    pull(articulo) %>% unique() %>% sort()
  if (length(items_here) == 0) next
  
  mape_here <- item_val %>%
    filter(subclase_ipc == sub_ipc, ciudad == city_name) %>%
    mutate(
      revert_note = case_when(
        method == "BASE_lowR2"      ~ " [low R²]",
        method == "BASE_lowlambda"  ~ " [low λ]",
        method == "BASE_highlambda" ~ " [high λ]",
        method == "BASE_fallback"   ~ " [est. failed]",
        TRUE                        ~ ""),
      lbl = sprintf("λ=%.3f | R²=%.3f | BASE=%.1f%% → %s=%.1f%%%s",
                    lambda,
                    if_else(is.na(r2_train), 0, r2_train),
                    MAPE_BASE,
                    if_else(method == "D_city", "D_city", "BASE"),
                    MAPE_final, revert_note)) %>%
    select(articulo, lbl)
  
  pred_here <- val_panel %>%
    filter(ciudad == city_name, fecha >= VAL_START) %>%
    left_join(lambda_final %>% select(ciudad, articulo, subclase_ipc),
              by = c("ciudad", "articulo")) %>%
    filter(subclase_ipc == sub_ipc) %>%
    left_join(mape_here, by = "articulo") %>%
    mutate(item_label = if_else(!is.na(lbl),
                                paste0(articulo, "\n", lbl), articulo))
  
  obs_here <- obs_full %>%
    filter(ciudad == city_name, articulo %in% items_here)
  
  obs_labeled <- obs_here %>%
    left_join(pred_here %>% distinct(articulo, item_label),
              by = "articulo")
  
  city_lbl <- city_labels[city_name]
  if (is.na(city_lbl)) city_lbl <- city_name
  
  n_items <- length(items_here)
  ncols   <- min(3L, n_items)
  fig_h   <- max(4, 3.5 * ceiling(n_items / ncols))
  fig_w   <- min(14, 4.5 * ncols)
  
  p <- ggplot() +
    annotate("rect", xmin = as.Date("1999-01-01"), xmax = T0,
             ymin = -Inf, ymax = Inf, fill = "#F0F0F0", alpha = 0.6) +
    annotate("rect", xmin = T0, xmax = VAL_END,
             ymin = -Inf, ymax = Inf, fill = "#D6EAF8", alpha = 0.6) +
    geom_vline(xintercept = T0, color = COL_VLINE, linewidth = 0.6) +
    geom_line(data = obs_labeled %>% filter(fecha < T0),
              aes(x = fecha, y = price_obs, group = item_label),
              color = COL_OBS, linewidth = 0.4, alpha = 0.4) +
    geom_line(data = obs_labeled %>%
                filter(fecha >= VAL_START, fecha <= VAL_END),
              aes(x = fecha, y = price_obs, group = item_label),
              color = COL_OBS, linewidth = 1.0) +
    geom_point(data = obs_labeled %>%
                 filter(fecha >= VAL_START, fecha <= VAL_END),
               aes(x = fecha, y = price_obs),
               color = COL_OBS, size = 1.2, alpha = 0.8) +
    geom_line(data = pred_here,
              aes(x = fecha, y = price_BASE, group = item_label),
              color = COL_BASE, linewidth = 0.8, linetype = "dashed") +
    {if (!is.null(method_i) && method_i == "D_city")
      geom_line(data = pred_here,
                aes(x = fecha, y = price_final, group = item_label),
                color = COL_DCITY, linewidth = 0.8, linetype = "dashed")
    } +
    facet_wrap(~ item_label, scales = "free_y", ncol = ncols) +
    scale_x_date(date_breaks = "6 months", date_labels = "%b\n%Y",
                 limits = c(as.Date("2013-01-01"), VAL_END)) +
    labs(
      title    = sprintf("Price validation — %s | %s | k=%d | %s",
                         sub_ipc, city_lbl, k_val,
                         if (is.null(method_i)) "" else method_i),
      subtitle = paste0(
        "Grey = training | Blue = validation (Feb 2016–Mar 2018)\n",
        "Black = observed | Grey dashed = BASE | Red dashed = D_city\n",
        sprintf("R²≥%.2f | λ∈[%.2f,%.2f]", R2_MIN, LAM_MIN, LAM_MAX)),
      x = NULL, y = "Price (COP per 500g)") +
    theme_bw(base_size = 9.5) +
    theme(strip.text = element_text(size = 7, face = "bold"),
          strip.background = element_rect(fill = "#EBF5FB"),
          panel.grid.minor = element_blank(),
          plot.title = element_text(size = 10, face = "bold"),
          legend.position = "none")
  
  ggsave(
    file.path(val_plot_dir,
              sprintf("ts_%s_%s.png", sub_ipc, safe_name(city_name))),
    p, width = fig_w, height = fig_h, dpi = 180, bg = "white")
}

message(sprintf("  Saved %d plots.", nrow(subclass_city_combos)))

# -----------------------------------------------------------------------
# 5. Summary figure
# -----------------------------------------------------------------------
p_mape <- subclass_val %>%
  mutate(
    subclase_ipc = fct_reorder(subclase_ipc, MAPE_BASE),
    flag = case_when(
      str_starts(method, "BASE") ~ paste0("BASE (", method, ")"),
      imp > 0                    ~ "D_city improves",
      TRUE                       ~ "D_city worsens")) %>%
  ggplot(aes(y = subclase_ipc)) +
  geom_segment(
    aes(x = pmin(MAPE_BASE, MAPE_final),
        xend = pmax(MAPE_BASE, MAPE_final),
        yend = subclase_ipc),
    color = "grey70", linewidth = 1.0) +
  geom_point(aes(x = MAPE_BASE), color = "#95A5A6", size = 3.2) +
  geom_point(aes(x = MAPE_final, color = flag), size = 3.2, shape = 15) +
  scale_color_manual(
    values = c("BASE (BASE)"            = "#95A5A6",
               "BASE (BASE_fallback)"   = "#E67E22",
               "BASE (BASE_lowR2)"      = "#F39C12",
               "BASE (BASE_lowlambda)"  = "#BDC3C7",
               "BASE (BASE_highlambda)" = "#BDC3C7",
               "D_city improves"        = "#1A7A4A",
               "D_city worsens"         = "#C0392B")) +
  labs(
    title    = "Validation MAPE by sub-class: BASE vs S5",
    subtitle = sprintf(
      "Global: BASE=%.4f%% → S5=%.4f%% (%+.4f pp)",
      global_val$MAPE_BASE, global_val$MAPE_final,
      global_val$imp_vs_BASE),
    x = "MAPE (%)", y = "CPI sub-class", color = NULL) +
  theme_bw(base_size = 11) +
  theme(legend.position = "bottom", panel.grid.major.y = element_blank())

ggsave(file.path(robust_dir, "fig_final_mape_by_subclass.png"),
       p_mape, width = 10, height = 8, dpi = 200)

# -----------------------------------------------------------------------
# 6. Save outputs
# -----------------------------------------------------------------------
write_csv(global_val %>%
            pivot_longer(everything(), names_to="metric", values_to="value"),
          file.path(robust_dir, "final_validation_global.csv"))
write_csv(global_by_method,
          file.path(robust_dir, "final_validation_by_method.csv"))
write_csv(subclass_val,
          file.path(robust_dir, "final_validation_subclass.csv"))
write_csv(item_val,
          file.path(robust_dir, "final_validation_item.csv"))

write_xlsx(
  list(global     = global_val %>%
         pivot_longer(everything(), names_to="metric", values_to="value"),
       by_method  = global_by_method,
       by_subclass= subclass_val,
       by_item    = item_val),
  file.path(robust_dir, "final_validation_results.xlsx"))

# Decision print
cat("\n", strrep("=", 60), "\n")
cat("  PRICE VALIDATION SUMMARY\n")
cat(sprintf("  Thresholds: R²≥%.2f | λ∈[%.2f, %.2f]\n",
            R2_MIN, LAM_MIN, LAM_MAX))
cat(strrep("=", 60), "\n\n")
cat(sprintf("  Global MAPE BASE:  %.4f%%\n", global_val$MAPE_BASE))
cat(sprintf("  Global MAPE S5:    %.4f%%\n", global_val$MAPE_final))
cat(sprintf("  Improvement:       %+.4f pp\n", global_val$imp_vs_BASE))
cat(sprintf("  S5 beats BASE in:  %.1f%% of obs\n\n",
            global_val$pct_final_beats_BASE))

message("Done. Run Script 07 next (CoNA validation).")