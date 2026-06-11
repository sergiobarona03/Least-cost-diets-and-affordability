########################################################
## ROBUSTNESS CHECK — Price extrapolation assumption
## Script 01: Internal validation of the proportional
##            extrapolation within the observed period
##
## Logic:
##   - Observed prices available: 1999-01 to 2018-03
##   - Validation base date (t0): 2016-01-01
##   - Validation window: 2016-01 to 2018-03 (27 months)
##   - For each item × city, we simulate the forward-fill
##     from t0 using only CPI sub-class variation, then
##     compare implied prices against actual observed prices.
##
## Key addition (vs. v1):
##   Section 12 classifies each CPI sub-class as
##   "single-item" (APE = 0 by construction) vs.
##   "multi-item" (extrapolation may introduce error),
##   and reports MAPE separately for each group.
##   This is the table that goes to the paper appendix.
##
## Output (saved to .../food-security-paper/robust/):
##   - validation_item_month.csv
##   - validation_summary_item.csv
##   - validation_summary_subclass.csv    (includes n_items per subclass)
##   - validation_summary_horizon.csv
##   - validation_summary_by_type.csv     ← NEW: single vs. multi-item
##   - validation_multiitem_subclass.csv  ← NEW: multi-item only (for appendix)
##   - validation_price_extrapolation.xlsx (all sheets)
##   - validation_plots/
##   - fig_validation_horizon_mape.png
##   - fig_validation_subclass_mape.png
##   - fig_validation_subclass_mape_multiitem.png  ← NEW
########################################################

# -----------------------------
# 0. Packages
# -----------------------------
library(tidyverse)
library(readxl)
library(janitor)
library(lubridate)
library(writexl)

# -----------------------------
# 1. Directories
# -----------------------------
dirs <- c(
  "C:\\Users\\sergio.barona\\Desktop\\Least-cost-diets-and-affordability\\Proyecto Interno",
  "C:/Users/Portatil/Desktop/Least-cost-diets-and-affordability/Proyecto Interno",
  "C:/Users/danie/OneDrive/Escritorio/Least-cost-diets-and-affordability/Proyecto Interno"
)
base_dir <- dirs[dir.exists(dirs)][1]
if (is.na(base_dir)) stop("Ninguno de los directorios existe")

# Inputs
in_prices <- file.path(base_dir, "Precios DANE/OUTPUT_DANE/precios_unadj_DANE_1999_2018.xlsx")
in_ipc    <- file.path(base_dir, "var-ipc/IPC.xls")
in_ipc2   <- file.path(base_dir, "var-ipc/IPC_2.xls")
in_ipc3   <- file.path(base_dir, "var-ipc/IPC_3.xls")
in_ipc4   <- file.path(base_dir, "var-ipc/IPC_4.xls")
in_corr1  <- file.path(base_dir, "var-ipc/correlativa_ipc.xlsx")
in_corr2  <- file.path(base_dir, "var-ipc/correlativa_ipc_articulos.xlsx")

# Output
out_dir <- file.path(base_dir, "food-security-paper/robust")
dir.create(file.path(out_dir, "validation_plots"),
           recursive = TRUE, showWarnings = FALSE)

# -----------------------------
# 2. Key parameter: validation base date
# -----------------------------
T0 <- as.Date("2016-01-01")

# -----------------------------
# 3. Auxiliary functions
# -----------------------------
safe_name <- function(x) {
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  x <- gsub("[^A-Za-z0-9_]+", "_", x)
  x <- gsub("_+", "_", x)
  x <- gsub("^_|_$", "", x)
  x
}

meses_esp <- c("Ene","Feb","Mar","Abr",
               "May","Jun","Jul","Ago","Sep","Oct","Nov","Dic")

make_date <- function(ano, mes_num) {
  as.Date(sprintf("%04d-%02d-01", as.integer(ano), as.integer(mes_num)))
}

mode1 <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_character_)
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# -----------------------------
# 4. Load data
# -----------------------------
message("Loading DANE prices...")
dane <- read_excel(in_prices) %>%
  mutate(
    fecha        = make_date(ano, mes_num),
    cod_subclase = paste0("0", substr(codigo_articulo, 1, 6), "0")
  ) %>%
  filter(!is.na(fecha))

message("Loading CPI series...")
# Read all four IPC files and stack them; duplicates removed by arrange+distinct
ipc_files <- list(in_ipc, in_ipc2, in_ipc3, in_ipc4)
ipc_raw   <- map(ipc_files, ~ read_excel(.x) %>% clean_names()) %>%
  bind_rows()

ipc <- ipc_raw %>%
  mutate(
    ciudad = case_when(
      ciudad == "CARTAGENA DE INDIAS" ~ "CARTAGENA",
      ciudad == "BOGOTÁ, D.C."        ~ "BOGOTÁ D.C.",
      TRUE ~ ciudad
    ),
    cod_subclase = substr(subclase, 1, 8),
    mes_num      = match(mes, meses_esp),
    ano          = as.integer(ano),
    ipc          = as.numeric(numero_indice),
    fecha        = make_date(ano, mes_num)
  ) %>%
  select(ciudad, cod_subclase, fecha, ipc) %>%
  filter(!is.na(fecha), !is.na(ipc),
         !is.na(cod_subclase), !is.na(ciudad)) %>%
  distinct(ciudad, cod_subclase, fecha, .keep_all = TRUE) %>%
  arrange(ciudad, cod_subclase, fecha)

message("Loading crosswalk tables...")
corr_subclase <- read_excel(in_corr1) %>%
  fill(subclase, ipc, .direction = "down") %>%
  mutate(cod_subclase = paste0("0", gasto_basico, "00")) %>%
  select(cod_subclase, subclase) %>%
  mutate(across(everything(), as.character))

corr_producto <- read_excel(in_corr2) %>%
  rename(codigo_articulo = cod_dane, subclase = cod_ipc) %>%
  mutate(across(everything(), as.character))

# -----------------------------
# 5. Filter cities and build validation window
# -----------------------------
cities_use <- c("CALI", "BOGOTÁ D.C.", "MEDELLÍN")

observed_window <- dane %>%
  filter(nombre_ciudad %in% cities_use,
         fecha >= T0,
         fecha <= as.Date("2018-03-01"),
         !is.na(precio_500g)) %>%
  arrange(nombre_ciudad, articulo, fecha)

prices_at_T0 <- dane %>%
  filter(nombre_ciudad %in% cities_use,
         fecha == T0,
         !is.na(precio_500g)) %>%
  select(nombre_ciudad, articulo, codigo_articulo,
         cod_subclase, precio_500g) %>%
  rename(precio_anchor = precio_500g)

valid_items <- prices_at_T0 %>%
  distinct(nombre_ciudad, articulo)

message(sprintf("Items with anchor price at T0: %d series (city x food)",
                nrow(valid_items)))

# -----------------------------
# 6. Sub-class assignment
# -----------------------------
assign_subclase <- function(df_series, corr_subclase, corr_producto) {
  tmp1 <- df_series %>%
    left_join(corr_subclase, by = "cod_subclase") %>%
    mutate(subclase = as.character(subclase))
  sub1 <- mode1(tmp1$subclase)
  if (is.na(sub1) || length(unique(na.omit(tmp1$subclase))) > 1) {
    tmp2 <- df_series %>%
      left_join(corr_producto, by = "codigo_articulo") %>%
      mutate(subclase = as.character(subclase))
    return(list(subclase = mode1(tmp2$subclase), method = "producto"))
  }
  list(subclase = sub1, method = "gasto_basico")
}

# -----------------------------
# 7. Core validation function
# -----------------------------
validate_series <- function(city_name, food_name,
                            anchor_price,
                            df_observed,
                            df_ipc) {
  
  all_months <- seq.Date(T0, as.Date("2018-03-01"), by = "month")
  
  ipc_t0 <- df_ipc %>% filter(fecha == T0) %>% pull(ipc)
  if (length(ipc_t0) == 0 || is.na(ipc_t0)) return(NULL)
  
  results <- map_dfr(all_months, function(t) {
    ipc_t <- df_ipc %>% filter(fecha == t) %>% pull(ipc)
    if (length(ipc_t) == 0 || is.na(ipc_t)) return(NULL)
    
    price_hat <- anchor_price * (ipc_t / ipc_t0)
    
    price_obs <- df_observed %>% filter(fecha == t) %>% pull(precio_500g)
    if (length(price_obs) == 0 || is.na(price_obs)) return(NULL)
    price_obs <- price_obs[1]
    
    ape     <- abs((price_hat - price_obs) / price_obs) * 100
    horizon <- as.integer(round(as.numeric(t - T0) / 30.44))
    
    tibble(
      ciudad       = city_name,
      articulo     = food_name,
      fecha        = t,
      horizon_mths = horizon,
      price_obs    = price_obs,
      price_hat    = price_hat,
      ape          = ape,
      ipc_ratio    = ipc_t / ipc_t0
    )
  })
  
  results
}

# -----------------------------
# 8. Main loop
# -----------------------------
series_keys  <- valid_items %>% arrange(nombre_ciudad, articulo)
all_results  <- list()
fail_log_val <- list()

for (i in seq_len(nrow(series_keys))) {
  
  city_name <- series_keys$nombre_ciudad[i]
  food_name <- series_keys$articulo[i]
  
  message(sprintf("[%d/%d] Validating: %s - %s",
                  i, nrow(series_keys), city_name, food_name))
  
  anchor_price <- prices_at_T0 %>%
    filter(nombre_ciudad == city_name, articulo == food_name) %>%
    pull(precio_anchor)
  if (length(anchor_price) == 0) next
  anchor_price <- anchor_price[1]
  
  df_full <- dane %>%
    filter(nombre_ciudad == city_name, articulo == food_name) %>%
    mutate(codigo_articulo = as.character(codigo_articulo),
           cod_subclase    = as.character(cod_subclase)) %>%
    arrange(fecha)
  if (nrow(df_full) == 0) next
  
  sub_info     <- assign_subclase(df_full, corr_subclase, corr_producto)
  subclase_ipc <- sub_info$subclase
  
  if (is.na(subclase_ipc)) {
    fail_log_val[[length(fail_log_val) + 1]] <- tibble(
      ciudad = city_name, articulo = food_name,
      motivo = "No se pudo asignar sub-clase IPC")
    next
  }
  
  subclase_code <- substr(paste0(subclase_ipc, "00"), 1, 8)
  
  df_ipc_series <- ipc %>%
    filter(ciudad == city_name, cod_subclase == subclase_code) %>%
    select(fecha, ipc) %>% arrange(fecha)
  
  if (nrow(df_ipc_series) == 0) {
    fail_log_val[[length(fail_log_val) + 1]] <- tibble(
      ciudad = city_name, articulo = food_name,
      motivo = "No hay serie IPC para esta sub-clase")
    next
  }
  
  df_obs_window <- observed_window %>%
    filter(nombre_ciudad == city_name, articulo == food_name) %>%
    select(fecha, precio_500g)
  if (nrow(df_obs_window) == 0) next
  
  res <- validate_series(city_name, food_name,
                         anchor_price, df_obs_window, df_ipc_series)
  
  if (is.null(res) || nrow(res) == 0) {
    fail_log_val[[length(fail_log_val) + 1]] <- tibble(
      ciudad = city_name, articulo = food_name,
      motivo = "validate_series returned NULL or empty")
    next
  }
  
  res <- res %>% mutate(subclase_ipc = subclase_ipc,
                        method       = sub_info$method)
  all_results[[length(all_results) + 1]] <- res
  
  # Plot observed vs simulated for this series
  p <- ggplot(res, aes(x = fecha)) +
    geom_line(aes(y = price_hat, color = "Simulated (extrap.)"),
              linewidth = 0.8, linetype = "dashed") +
    geom_line(aes(y = price_obs, color = "Observed"),
              linewidth = 0.8) +
    geom_vline(xintercept = T0, linetype = "dotted", color = "grey40") +
    scale_color_manual(values = c("Observed"            = "#2C3E50",
                                  "Simulated (extrap.)" = "#E74C3C")) +
    labs(
      title    = paste0(city_name, " - ", food_name),
      subtitle = paste0("Sub-clase IPC: ", subclase_ipc,
                        " | Validation window: Jan 2016 - Mar 2018",
                        " | MAPE = ",
                        round(mean(res$ape, na.rm = TRUE), 1), "%"),
      x = NULL, y = "Price (500 g, COP)", color = NULL
    ) +
    theme_bw(base_size = 10) +
    theme(legend.position = "bottom")
  
  out_png <- file.path(out_dir, "validation_plots",
                       safe_name(city_name),
                       paste0(safe_name(food_name), ".png"))
  dir.create(dirname(out_png), recursive = TRUE, showWarnings = FALSE)
  ggsave(out_png, p, width = 10, height = 4, dpi = 180)
}

# -----------------------------
# 9. Consolidate results
# -----------------------------
validation_item_month <- bind_rows(all_results)

# MAPE per item × city
validation_summary_item <- validation_item_month %>%
  group_by(ciudad, articulo, subclase_ipc, method) %>%
  summarise(
    n_months   = n(),
    MAPE       = mean(ape, na.rm = TRUE),
    max_APE    = max(ape, na.rm = TRUE),
    median_APE = median(ape, na.rm = TRUE),
    sd_APE     = sd(ape, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(MAPE))

# MAPE per sub-class — NOW includes n_items (unique food names per sub-class)
# n_items > 1 means within-subclass relative price variation CAN distort results
validation_summary_subclass <- validation_item_month %>%
  group_by(subclase_ipc) %>%
  summarise(
    n_items    = n_distinct(articulo),      # <- KEY: items per sub-class
    n_series   = n_distinct(paste(ciudad, articulo)),
    n_obs      = n(),
    MAPE       = mean(ape, na.rm = TRUE),
    max_APE    = max(ape, na.rm = TRUE),
    median_APE = median(ape, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(MAPE))

# MAPE by horizon
validation_summary_horizon <- validation_item_month %>%
  group_by(horizon_mths) %>%
  summarise(
    n_obs      = n(),
    MAPE       = mean(ape, na.rm = TRUE),
    max_APE    = max(ape, na.rm = TRUE),
    median_APE = median(ape, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(horizon_mths)

# Global summary
global_MAPE    <- mean(validation_item_month$ape, na.rm = TRUE)
global_max_APE <- max(validation_item_month$ape, na.rm = TRUE)
worst_item     <- validation_summary_item %>% slice_max(MAPE, n = 1)

message(sprintf(
  "\n=== GLOBAL VALIDATION SUMMARY ===\n  MAPE (all items, all months): %.2f%%\n  Max APE (worst single obs):  %.2f%%\n  Worst item: %s in %s (MAPE = %.2f%%)\n",
  global_MAPE, global_max_APE,
  worst_item$articulo[1], worst_item$ciudad[1], worst_item$MAPE[1]
))

# -----------------------------
# 10. Horizon and sub-class plots (all items)
# -----------------------------
p_horizon <- ggplot(validation_summary_horizon,
                    aes(x = horizon_mths, y = MAPE)) +
  geom_col(fill = "#2E5FA3", alpha = 0.8, width = 0.7) +
  geom_line(color = "#C0392B", linewidth = 0.8) +
  geom_point(color = "#C0392B", size = 2) +
  geom_hline(yintercept = c(3, 5), linetype = "dashed",
             color = "grey50", linewidth = 0.5) +
  annotate("text", x = max(validation_summary_horizon$horizon_mths),
           y = 3.3, label = "3% threshold", hjust = 1,
           size = 3, color = "grey40") +
  annotate("text", x = max(validation_summary_horizon$horizon_mths),
           y = 5.3, label = "5% threshold", hjust = 1,
           size = 3, color = "grey40") +
  scale_x_continuous(breaks = 0:27) +
  labs(
    title    = "Price extrapolation error by forecast horizon",
    subtitle = paste0("Validation window: Jan 2016 - Mar 2018 | Base date t0 = Jan 2016\n",
                      sprintf("Overall MAPE = %.2f%% | Max APE = %.2f%%",
                              global_MAPE, global_max_APE)),
    x = "Months since base date (t0 = Jan 2016)",
    y = "Mean Absolute Percentage Error (%)"
  ) +
  theme_bw(base_size = 11)

ggsave(file.path(out_dir, "fig_validation_horizon_mape.png"),
       p_horizon, width = 10, height = 5, dpi = 200)

p_subclass <- validation_summary_subclass %>%
  mutate(subclase_ipc = fct_reorder(subclase_ipc, MAPE)) %>%
  ggplot(aes(x = MAPE, y = subclase_ipc)) +
  geom_col(fill = "#2E5FA3", alpha = 0.8) +
  geom_vline(xintercept = c(3, 5), linetype = "dashed",
             color = "#C0392B", linewidth = 0.6) +
  labs(
    title    = "Price extrapolation MAPE by CPI sub-class (all items)",
    subtitle = "Pooled over all items and cities in each sub-class",
    x = "MAPE (%)", y = "CPI sub-class"
  ) +
  theme_bw(base_size = 11)

ggsave(file.path(out_dir, "fig_validation_subclass_mape.png"),
       p_subclass, width = 9, height = 7, dpi = 200)

# -----------------------------------------------------------------------
# 12. NEW — Single-item vs. multi-item sub-class breakdown
#
# Single-item sub-classes: APE = 0 by construction because the sub-class
# index directly tracks the item price (no within-subclass relative price).
# Multi-item sub-classes: extrapolation can miss within-subclass relative
# price variation — these are the cases the reviewer is concerned about.
# -----------------------------------------------------------------------

# Classify each sub-class
subclass_type <- validation_summary_subclass %>%
  mutate(
    subclass_type = if_else(
      n_items == 1,
      "Single-item (exact by construction)",
      "Multi-item (may introduce error)"
    )
  )

# Attach type to item-level data
validation_item_typed <- validation_item_month %>%
  left_join(subclass_type %>% select(subclase_ipc, n_items, subclass_type),
            by = "subclase_ipc")

# Summary table by type — this is the key number for the paper
validation_summary_by_type <- validation_item_typed %>%
  group_by(subclass_type) %>%
  summarise(
    n_subclasses   = n_distinct(subclase_ipc),
    n_items_unique = n_distinct(articulo),
    n_series       = n_distinct(paste(ciudad, articulo)),
    n_obs          = n(),
    MAPE_mean      = mean(ape, na.rm = TRUE),
    MAPE_max       = max(ape, na.rm = TRUE),
    MAPE_median    = median(ape, na.rm = TRUE),
    pct_below_1pct = mean(ape < 1,  na.rm = TRUE) * 100,
    pct_below_3pct = mean(ape < 3,  na.rm = TRUE) * 100,
    pct_below_5pct = mean(ape < 5,  na.rm = TRUE) * 100,
    .groups = "drop"
  )

message("\n=== SINGLE vs. MULTI-ITEM SUB-CLASS BREAKDOWN ===")
print(validation_summary_by_type)

# Multi-item sub-classes only — table for the paper appendix
validation_multiitem_subclass <- subclass_type %>%
  filter(subclass_type == "Multi-item (may introduce error)") %>%
  select(subclase_ipc, n_items, n_series, n_obs,
         MAPE, max_APE, median_APE) %>%
  arrange(desc(MAPE))

message("\nMulti-item sub-classes (ranked by MAPE):")
print(validation_multiitem_subclass)

# Horizon plot restricted to multi-item sub-classes only
# Shows whether error accumulates over time in the problematic cases
horizon_multiitem <- validation_item_typed %>%
  filter(subclass_type == "Multi-item (may introduce error)") %>%
  group_by(horizon_mths) %>%
  summarise(
    n_obs      = n(),
    MAPE       = mean(ape, na.rm = TRUE),
    max_APE    = max(ape, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(horizon_mths)

mape_multi_global  <- mean(
  validation_item_typed$ape[
    validation_item_typed$subclass_type == "Multi-item (may introduce error)"
  ], na.rm = TRUE)
maxape_multi_global <- max(
  validation_item_typed$ape[
    validation_item_typed$subclass_type == "Multi-item (may introduce error)"
  ], na.rm = TRUE)

p_subclass_multi <- validation_multiitem_subclass %>%
  mutate(subclase_ipc = fct_reorder(subclase_ipc, MAPE)) %>%
  ggplot(aes(x = MAPE, y = subclase_ipc)) +
  geom_col(aes(fill = MAPE > 5), alpha = 0.85, show.legend = FALSE) +
  scale_fill_manual(values = c("FALSE" = "#2E5FA3", "TRUE" = "#C0392B")) +
  geom_text(aes(label = paste0(round(MAPE, 1), "%")),
            hjust = -0.15, size = 3.2) +
  geom_vline(xintercept = c(3, 5), linetype = "dashed",
             color = "grey50", linewidth = 0.6) +
  annotate("text", x = 3.15, y = 0.6, label = "3%", size = 3,
           color = "grey40", hjust = 0) +
  annotate("text", x = 5.15, y = 0.6, label = "5%", size = 3,
           color = "grey40", hjust = 0) +
  labs(
    title    = "Price extrapolation MAPE — multi-item sub-classes only",
    subtitle = paste0(
      "These are the sub-classes where within-subclass relative price variation\n",
      "can distort the extrapolated series. Red bars exceed the 5% threshold.\n",
      sprintf("Global MAPE (multi-item only) = %.2f%% | Max APE = %.2f%%",
              mape_multi_global, maxape_multi_global)),
    x = "MAPE (%)", y = "CPI sub-class"
  ) +
  theme_bw(base_size = 11) +
  theme(panel.grid.major.y = element_blank())

ggsave(file.path(out_dir, "fig_validation_subclass_mape_multiitem.png"),
       p_subclass_multi, width = 9, height = 7, dpi = 200)

p_horizon_multi <- ggplot(horizon_multiitem,
                          aes(x = horizon_mths, y = MAPE)) +
  geom_col(fill = "#2E5FA3", alpha = 0.8, width = 0.7) +
  geom_line(color = "#C0392B", linewidth = 0.8) +
  geom_point(color = "#C0392B", size = 2) +
  geom_hline(yintercept = c(3, 5), linetype = "dashed",
             color = "grey50", linewidth = 0.5) +
  scale_x_continuous(breaks = 0:27) +
  labs(
    title    = "Price extrapolation error by horizon — multi-item sub-classes only",
    subtitle = sprintf("Global MAPE = %.2f%% | Max APE = %.2f%%",
                       mape_multi_global, maxape_multi_global),
    x = "Months since t0 (Jan 2016)",
    y = "Mean Absolute Percentage Error (%)"
  ) +
  theme_bw(base_size = 11)

ggsave(file.path(out_dir, "fig_validation_horizon_mape_multiitem.png"),
       p_horizon_multi, width = 10, height = 5, dpi = 200)

# -----------------------------
# 13. Save all outputs
# -----------------------------
fail_df_val <- if (length(fail_log_val) == 0) tibble() else bind_rows(fail_log_val)

write_csv(validation_item_month,         file.path(out_dir, "validation_item_month.csv"))
write_csv(validation_summary_item,       file.path(out_dir, "validation_summary_item.csv"))
write_csv(validation_summary_subclass,   file.path(out_dir, "validation_summary_subclass.csv"))
write_csv(validation_summary_horizon,    file.path(out_dir, "validation_summary_horizon.csv"))
write_csv(validation_summary_by_type,    file.path(out_dir, "validation_summary_by_type.csv"))
write_csv(validation_multiitem_subclass, file.path(out_dir, "validation_multiitem_subclass.csv"))
write_csv(fail_df_val,                   file.path(out_dir, "validation_failures.csv"))

write_xlsx(
  list(
    item_month       = validation_item_month,
    summary_item     = validation_summary_item,
    by_subclass      = validation_summary_subclass,
    by_horizon       = validation_summary_horizon,
    by_type          = validation_summary_by_type,
    multiitem_only   = validation_multiitem_subclass,
    failures         = fail_df_val
  ),
  file.path(out_dir, "validation_price_extrapolation.xlsx")
)

saveRDS(validation_item_month, file.path(out_dir, "validation_item_month.rds"))

message("\nDone. All outputs saved to: ", out_dir)