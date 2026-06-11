########################################################
## SCRIPT 06 — Price validation (MAPE)
## Compares BASE and S5 vs DANE observed prices
## Validation window: Feb 2016 – Mar 2018
##
## Reads:  FORECAST_DIR/prices_validation.csv
##         FORECAST_DIR/lambda_final.csv
##         FORECAST_DIR/params_optimal.csv
##
## Writes: ROBUST_DIR/price_validation_global.csv
##         ROBUST_DIR/price_validation_subclass.csv
##         ROBUST_DIR/price_validation_item.csv
##         ROBUST_DIR/price_validation_results.xlsx
##         ROBUST_DIR/fig_mape_by_subclass.png
##         ROBUST_DIR/fig_validation_overview.png
##         VAL_PLOT_DIR/ts_*.png
########################################################

source("00_config.R")
library(tidyverse)
library(writexl)

# -----------------------------------------------------------------------
# Load inputs
# -----------------------------------------------------------------------
message("Loading inputs...")

prices_val <- read_csv(file.path(FORECAST_DIR, "prices_validation.csv"),
                       show_col_types = FALSE) %>%
  mutate(fecha = as.Date(fecha))

lambda_final <- read_csv(file.path(FORECAST_DIR, "lambda_final.csv"),
                         show_col_types = FALSE)

params <- read_csv(file.path(FORECAST_DIR, "params_optimal.csv"),
                   show_col_types = FALSE)

R2_MIN  <- params$R2_MIN[1]
LAM_MIN <- params$LAM_MIN[1]
LAM_MAX <- params$LAM_MAX[1]

# -----------------------------------------------------------------------
# Compute APEs
# -----------------------------------------------------------------------
message("Computing APEs...")

val_ape <- prices_val %>%
  filter(!is.na(price_obs), price_obs > 0) %>%
  left_join(
    lambda_final %>% select(ciudad, articulo, k, r2_train = r2),
    by = c("ciudad", "articulo")) %>%
  mutate(
    ape_BASE = abs((price_BASE - price_obs) / price_obs) * 100,
    ape_S5   = abs((price_S5   - price_obs) / price_obs) * 100)

global_val <- val_ape %>%
  summarise(
    n_obs              = n(),
    MAPE_BASE          = mean(ape_BASE, na.rm = TRUE),
    MAPE_S5            = mean(ape_S5,   na.rm = TRUE),
    max_BASE           = max(ape_BASE,  na.rm = TRUE),
    max_S5             = max(ape_S5,    na.rm = TRUE),
    pct_S5_beats_BASE  = mean(ape_S5 < ape_BASE, na.rm = TRUE) * 100,
    imp_vs_BASE        = MAPE_BASE - MAPE_S5)

subclass_val <- val_ape %>%
  group_by(subclase_ipc, k, method) %>%
  summarise(
    n_items    = n_distinct(articulo),
    n_obs      = n(),
    MAPE_BASE  = mean(ape_BASE, na.rm = TRUE),
    MAPE_S5    = mean(ape_S5,   na.rm = TRUE),
    imp        = MAPE_BASE - MAPE_S5,
    .groups    = "drop") %>%
  arrange(desc(MAPE_BASE))

item_val <- val_ape %>%
  group_by(ciudad, articulo, subclase_ipc, k,
           method, r2_train) %>%
  summarise(
    n_months  = n(),
    MAPE_BASE = mean(ape_BASE, na.rm = TRUE),
    MAPE_S5   = mean(ape_S5,   na.rm = TRUE),
    imp       = MAPE_BASE - MAPE_S5,
    .groups   = "drop") %>%
  arrange(desc(MAPE_BASE))

message(sprintf(
  "  Global MAPE: BASE=%.4f%% | S5=%.4f%% (%+.4f pp)",
  global_val$MAPE_BASE, global_val$MAPE_S5,
  global_val$imp_vs_BASE))

# -----------------------------------------------------------------------
# Summary figure: dumbbell MAPE by sub-class
# -----------------------------------------------------------------------
safe_name <- function(x) {
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  gsub("[^A-Za-z0-9_]+", "_", x)
}

n_dcity <- sum(lambda_final$method_label == "D_city", na.rm = TRUE)

p_mape <- subclass_val %>%
  mutate(
    subclase_ipc = fct_reorder(subclase_ipc, MAPE_BASE),
    flag = case_when(
      str_starts(method, "BASE") ~ paste0("BASE (", method, ")"),
      imp > 0                    ~ "D_city improves",
      TRUE                       ~ "D_city worsens")) %>%
  ggplot(aes(y = subclase_ipc)) +
  geom_segment(
    aes(x = pmin(MAPE_BASE, MAPE_S5),
        xend = pmax(MAPE_BASE, MAPE_S5),
        yend = subclase_ipc),
    color = "grey70", linewidth = 1.0) +
  geom_point(aes(x = MAPE_BASE), color = "#95A5A6", size = 3.2) +
  geom_point(aes(x = MAPE_S5, color = flag), size = 3.2, shape = 15) +
  scale_color_manual(
    values = c(
      "BASE (BASE)"            = "#95A5A6",
      "BASE (BASE_fallback)"   = "#E67E22",
      "BASE (BASE_lowR2)"      = "#F39C12",
      "BASE (BASE_lowlambda)"  = "#BDC3C7",
      "BASE (BASE_highlambda)" = "#BDC3C7",
      "D_city improves"        = "#1A7A4A",
      "D_city worsens"         = "#C0392B")) +
  labs(
    title    = "Validation MAPE by sub-class: BASE vs S5",
    subtitle = sprintf(
      "Global: BASE=%.4f%% → S5=%.4f%% (%+.4f pp) | D_city series=%d",
      global_val$MAPE_BASE, global_val$MAPE_S5,
      global_val$imp_vs_BASE, n_dcity),
    x = "MAPE (%)", y = "CPI sub-class", color = NULL) +
  theme_bw(base_size = 11) +
  theme(legend.position  = "bottom",
        panel.grid.major.y = element_blank())

ggsave(file.path(ROBUST_DIR, "fig_mape_by_subclass.png"),
       p_mape, width = 10, height = 8, dpi = 200)

# -----------------------------------------------------------------------
# Time series plots per sub-class × city
# -----------------------------------------------------------------------
message("Building time series plots...")

# Observed prices for plotting (full series including pre-T0)
dane <- readRDS(file.path(CACHE_DIR, "dane.rds"))
obs_full <- dane %>%
  filter(!is.na(precio_500g)) %>%
  select(ciudad = nombre_ciudad, articulo, fecha,
         price_obs = precio_500g)

combos <- prices_val %>%
  left_join(lambda_final %>%
              select(ciudad, articulo, k, method_label),
            by = c("ciudad", "articulo")) %>%
  distinct(subclase_ipc, ciudad, k, method_label)

message(sprintf("  %d sub-class × city plots...", nrow(combos)))

for (i in seq_len(nrow(combos))) {
  
  sub_ipc   <- combos$subclase_ipc[i]
  city_name <- combos$ciudad[i]
  k_val     <- combos$k[i]
  method_i  <- combos$method_label[i]
  
  pred_here <- prices_val %>%
    filter(subclase_ipc == sub_ipc, ciudad == city_name)
  
  items_here <- unique(pred_here$articulo)
  if (length(items_here) == 0) next
  
  mape_here <- item_val %>%
    filter(subclase_ipc == sub_ipc, ciudad == city_name) %>%
    mutate(lbl = sprintf(
      "R²=%.3f | BASE=%.1f%% → %s=%.1f%%",
      if_else(is.na(r2_train), 0, r2_train),
      MAPE_BASE,
      if_else(method == "D_city", "D_city", "BASE"),
      MAPE_S5)) %>%
    select(articulo, lbl)
  
  pred_labeled <- pred_here %>%
    left_join(mape_here, by = "articulo") %>%
    mutate(item_label = if_else(!is.na(lbl),
                                paste0(articulo, "\n", lbl),
                                articulo))
  
  obs_labeled <- obs_full %>%
    filter(ciudad == city_name, articulo %in% items_here) %>%
    left_join(pred_labeled %>% distinct(articulo, item_label),
              by = "articulo")
  
  city_lbl <- CITY_LABELS[city_name]
  n_items  <- length(items_here)
  ncols    <- min(3L, n_items)
  fig_h    <- max(4, 3.5 * ceiling(n_items / ncols))
  fig_w    <- min(14, 4.5 * ncols)
  
  p <- ggplot() +
    annotate("rect",
             xmin = as.Date("1999-01-01"), xmax = T0,
             ymin = -Inf, ymax = Inf,
             fill = "#F0F0F0", alpha = 0.6) +
    annotate("rect",
             xmin = VAL_START, xmax = VAL_END,
             ymin = -Inf, ymax = Inf,
             fill = "#D6EAF8", alpha = 0.6) +
    geom_vline(xintercept = T0,
               color = COL_VLINE, linewidth = 0.6) +
    geom_line(data = obs_labeled %>% filter(fecha < VAL_START),
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
    geom_line(data = pred_labeled,
              aes(x = fecha, y = price_BASE, group = item_label),
              color = COL_BASE, linewidth = 0.8, linetype = "dashed") +
    {if (!is.null(method_i) && method_i == "D_city")
      geom_line(data = pred_labeled,
                aes(x = fecha, y = price_S5, group = item_label),
                color = COL_S5, linewidth = 0.8, linetype = "dashed")
    } +
    facet_wrap(~ item_label, scales = "free_y", ncol = ncols) +
    scale_x_date(date_breaks = "6 months",
                 date_labels = "%b\n%Y",
                 limits = c(as.Date("2013-01-01"), VAL_END)) +
    labs(
      title    = sprintf("Price validation — %s | %s | k=%d | %s",
                         sub_ipc, city_lbl, k_val,
                         if (is.null(method_i)) "" else method_i),
      subtitle = paste0(
        "Grey = training | Blue = validation (Feb 2016–Mar 2018)\n",
        "Black = observed | Grey dashed = BASE | Red dashed = S5\n",
        sprintf("R²≥%.2f | λ∈[%.2f,%.2f]",
                R2_MIN, LAM_MIN, LAM_MAX)),
      x = NULL, y = "Price (COP per 500g)") +
    theme_bw(base_size = 9.5) +
    theme(
      strip.text       = element_text(size = 7, face = "bold"),
      strip.background = element_rect(fill = "#EBF5FB"),
      panel.grid.minor = element_blank(),
      plot.title       = element_text(size = 10, face = "bold"),
      legend.position  = "none")
  
  ggsave(
    file.path(VAL_PLOT_DIR,
              sprintf("ts_%s_%s.png", sub_ipc, safe_name(city_name))),
    p, width = fig_w, height = fig_h, dpi = 180, bg = "white")
}

message(sprintf("  Saved %d plots.", nrow(combos)))

# -----------------------------------------------------------------------
# Save validation tables
# -----------------------------------------------------------------------
write_csv(
  global_val %>% pivot_longer(everything(),
                              names_to = "metric", values_to = "value"),
  file.path(ROBUST_DIR, "price_validation_global.csv"))
write_csv(subclass_val,
          file.path(ROBUST_DIR, "price_validation_subclass.csv"))
write_csv(item_val,
          file.path(ROBUST_DIR, "price_validation_item.csv"))
write_xlsx(
  list(global     = global_val %>%
         pivot_longer(everything(), names_to="metric", values_to="value"),
       by_subclass= subclass_val,
       by_item    = item_val),
  file.path(ROBUST_DIR, "price_validation_results.xlsx"))

cat("\n", strrep("=", 60), "\n")
cat("  PRICE VALIDATION SUMMARY\n")
cat(sprintf("  Thresholds: R²≥%.2f | λ∈[%.2f, %.2f]\n",
            R2_MIN, LAM_MIN, LAM_MAX))
cat(strrep("=", 60), "\n\n")
cat(sprintf("  n obs:       %d\n",  global_val$n_obs))
cat(sprintf("  MAPE BASE:   %.4f%%\n", global_val$MAPE_BASE))
cat(sprintf("  MAPE S5:     %.4f%%\n", global_val$MAPE_S5))
cat(sprintf("  Improvement: %+.4f pp\n", global_val$imp_vs_BASE))
cat(sprintf("  S5 > BASE:   %.1f%% of obs\n\n",
            global_val$pct_S5_beats_BASE))

message("Done. Run 07_model_validation.R next.")