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
## Output (saved to .../food-security-paper/robust/):
##   - validation_item_month.csv   (APE per item × city × month)
##   - validation_summary_item.csv (MAPE per item × city)
##   - validation_summary_subclass.csv (MAPE per sub-class)
##   - validation_summary_horizon.csv  (MAPE by months-since-t0)
##   - validation_plots/              (one PNG per city × item)
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
  "C:/Users/Portatil/Desktop/Least-cost-diets-and-affordability/Proyecto Interno",
  "C:/Users/danie/OneDrive/Escritorio/Least-cost-diets-and-affordability/Proyecto Interno"
)
base_dir <- dirs[dir.exists(dirs)][1]
if (is.na(base_dir)) stop("Ninguno de los directorios existe")

# Inputs — same as in the main forecasting script
in_prices <- file.path(base_dir, "Precios DANE/OUTPUT_DANE/precios_unadj_DANE_1999_2018.xlsx")
in_ipc    <- file.path(base_dir, "var-ipc/IPC.xls")
in_ipc2   <- file.path(base_dir, "var-ipc/IPC_2.xls")
in_ipc3   <- file.path(base_dir, "var-ipc/IPC_3.xls")
in_corr1  <- file.path(base_dir, "var-ipc/correlativa_ipc.xlsx")
in_corr2  <- file.path(base_dir, "var-ipc/correlativa_ipc_articulos.xlsx")

# Output — new robust folder
out_dir <- file.path(base_dir, "food-security-paper/robust")
dir.create(file.path(out_dir, "validation_plots"), 
           recursive = TRUE, showWarnings = FALSE)

# -----------------------------
# 2. Key parameter: validation base date
# -----------------------------
# t0 must be well inside the observed window so we have
# enough months ahead to compare (we use 27 months: Jan 2016 - Mar 2018).
# You can change this to "2015-01-01" for a 39-month window.
T0 <- as.Date("2016-01-01")

# -----------------------------
# 3. Auxiliary objects (copied from main script)
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
# 4. Load data (identical to main script)
# -----------------------------
message("Loading DANE prices...")
dane <- read_excel(in_prices) %>%
  mutate(
    fecha       = make_date(ano, mes_num),
    cod_subclase = paste0("0", substr(codigo_articulo, 1, 6), "0")
  ) %>%
  filter(!is.na(fecha))

message("Loading CPI series...")
ipc_raw1 <- read_excel(in_ipc)  %>% clean_names()
ipc_raw2 <- read_excel(in_ipc2) %>% clean_names()
ipc_raw3 <- read_excel(in_ipc2) %>% clean_names()

ipc <- bind_rows(ipc_raw1, ipc_raw2, ipc_raw3) %>%
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
# 5. Filter cities and restrict to observed window only
# -----------------------------
cities_use <- c("CALI", "BOGOTÁ D.C.", "MEDELLÍN")

# Keep only the OBSERVED period up to 2018-03
# The validation window starts at T0
observed_window <- dane %>%
  filter(nombre_ciudad %in% cities_use,
         fecha >= T0,                  # from base date
         fecha <= as.Date("2018-03-01"), # last observed month
         !is.na(precio_500g)) %>%
  arrange(nombre_ciudad, articulo, fecha)

# We also need prices AT T0 as the anchor
prices_at_T0 <- dane %>%
  filter(nombre_ciudad %in% cities_use,
         fecha == T0,
         !is.na(precio_500g)) %>%
  select(nombre_ciudad, articulo, codigo_articulo, 
         cod_subclase, precio_500g) %>%
  rename(precio_anchor = precio_500g)

# Items with an anchor price at T0
valid_items <- prices_at_T0 %>%
  distinct(nombre_ciudad, articulo)

message(sprintf("Items with anchor price at T0: %d series (city × food)",
                nrow(valid_items)))

# -----------------------------
# 6. Sub-class assignment (same logic as main script)
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
# For a single city × food series:
#   - Takes observed prices in validation window
#   - Takes anchor price at T0
#   - Takes the CPI sub-class series for that city × sub-class
#   - Simulates forward-fill from T0
#   - Returns APE for each month t > T0

validate_series <- function(city_name, food_name, 
                             anchor_price,
                             df_observed,   # observed prices T0..2018-03
                             df_ipc) {       # CPI sub-class series
  
  # All months in the validation window (T0 to 2018-03)
  all_months <- seq.Date(T0, as.Date("2018-03-01"), by = "month")
  
  # IPC at T0 (anchor for the ratio)
  ipc_t0 <- df_ipc %>% 
    filter(fecha == T0) %>% 
    pull(ipc)
  
  if (length(ipc_t0) == 0 || is.na(ipc_t0)) {
    return(NULL)  # CPI not available at T0 for this sub-class
  }
  
  results <- map_dfr(all_months, function(t) {
    
    # Simulated (extrapolated) price at time t
    ipc_t <- df_ipc %>% filter(fecha == t) %>% pull(ipc)
    
    if (length(ipc_t) == 0 || is.na(ipc_t)) return(NULL)
    
    price_hat <- anchor_price * (ipc_t / ipc_t0)
    
    # Observed price at time t
    price_obs <- df_observed %>%
      filter(fecha == t) %>%
      pull(precio_500g)
    
    # Skip if observed price is missing at this month
    if (length(price_obs) == 0 || is.na(price_obs)) return(NULL)
    price_obs <- price_obs[1]  # take first if duplicates
    
    # Absolute percentage error
    ape <- abs((price_hat - price_obs) / price_obs) * 100
    
    # Horizon in months from T0
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
# 8. Loop over all city × food series
# -----------------------------
series_keys <- valid_items %>%
  arrange(nombre_ciudad, articulo)

all_results  <- list()
fail_log_val <- list()

for (i in seq_len(nrow(series_keys))) {
  
  city_name <- series_keys$nombre_ciudad[i]
  food_name <- series_keys$articulo[i]
  
  message(sprintf("[%d/%d] Validating: %s — %s",
                  i, nrow(series_keys), city_name, food_name))
  
  # --- anchor price ---
  anchor_price <- prices_at_T0 %>%
    filter(nombre_ciudad == city_name, articulo == food_name) %>%
    pull(precio_anchor)
  if (length(anchor_price) == 0) next
  anchor_price <- anchor_price[1]
  
  # --- full observed series for this item (to assign sub-class) ---
  df_full <- dane %>%
    filter(nombre_ciudad == city_name, articulo == food_name) %>%
    mutate(codigo_articulo = as.character(codigo_articulo),
           cod_subclase    = as.character(cod_subclase)) %>%
    arrange(fecha)
  
  if (nrow(df_full) == 0) next
  
  # --- sub-class assignment ---
  sub_info <- assign_subclase(df_full, corr_subclase, corr_producto)
  subclase_ipc <- sub_info$subclase
  
  if (is.na(subclase_ipc)) {
    fail_log_val[[length(fail_log_val) + 1]] <- tibble(
      ciudad = city_name, articulo = food_name,
      motivo = "No se pudo asignar sub-clase IPC"
    )
    next
  }
  
  subclase_code <- substr(paste0(subclase_ipc, "00"), 1, 8)
  
  # --- CPI series for this city × sub-class ---
  df_ipc_series <- ipc %>%
    filter(ciudad == city_name, cod_subclase == subclase_code) %>%
    select(fecha, ipc) %>%
    arrange(fecha)
  
  if (nrow(df_ipc_series) == 0) {
    fail_log_val[[length(fail_log_val) + 1]] <- tibble(
      ciudad = city_name, articulo = food_name,
      motivo = "No hay serie IPC para esta sub-clase"
    )
    next
  }
  
  # --- observed prices in validation window (T0 to 2018-03) ---
  df_obs_window <- observed_window %>%
    filter(nombre_ciudad == city_name, articulo == food_name) %>%
    select(fecha, precio_500g)
  
  if (nrow(df_obs_window) == 0) next
  
  # --- run validation ---
  res <- validate_series(
    city_name    = city_name,
    food_name    = food_name,
    anchor_price = anchor_price,
    df_observed  = df_obs_window,
    df_ipc       = df_ipc_series
  )
  
  if (is.null(res) || nrow(res) == 0) {
    fail_log_val[[length(fail_log_val) + 1]] <- tibble(
      ciudad = city_name, articulo = food_name,
      motivo = "validate_series returned NULL or empty"
    )
    next
  }
  
  # --- attach sub-class label ---
  res <- res %>% mutate(subclase_ipc = subclase_ipc,
                        method       = sub_info$method)
  
  all_results[[length(all_results) + 1]] <- res
  
  # --- plot observed vs. simulated ---
  p <- ggplot(res, aes(x = fecha)) +
    geom_line(aes(y = price_hat, color = "Simulated (extrap.)"),
              linewidth = 0.8, linetype = "dashed") +
    geom_line(aes(y = price_obs, color = "Observed"),
              linewidth = 0.8) +
    geom_vline(xintercept = T0, linetype = "dotted", color = "grey40") +
    scale_color_manual(values = c("Observed" = "#2C3E50",
                                  "Simulated (extrap.)" = "#E74C3C")) +
    labs(
      title    = paste0(city_name, " — ", food_name),
      subtitle = paste0("Sub-clase IPC: ", subclase_ipc,
                        " | Validation window: Jan 2016 – Mar 2018",
                        " | MAPE = ", round(mean(res$ape, na.rm = TRUE), 1), "%"),
      x = NULL, y = "Price (500 g, COP)", color = NULL
    ) +
    theme_bw(base_size = 10) +
    theme(legend.position = "bottom")
  
  out_png <- file.path(
    out_dir, "validation_plots", safe_name(city_name),
    paste0(safe_name(food_name), ".png")
  )
  dir.create(dirname(out_png), recursive = TRUE, showWarnings = FALSE)
  ggsave(out_png, p, width = 10, height = 4, dpi = 180)
}

# -----------------------------
# 9. Consolidate item-level results
# -----------------------------
validation_item_month <- bind_rows(all_results)

# MAPE per item × city (averaged over the 27 validation months)
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

# MAPE per CPI sub-class (pooled over items within the sub-class)
validation_summary_subclass <- validation_item_month %>%
  group_by(subclase_ipc) %>%
  summarise(
    n_series   = n_distinct(paste(ciudad, articulo)),
    n_obs      = n(),
    MAPE       = mean(ape, na.rm = TRUE),
    max_APE    = max(ape, na.rm = TRUE),
    median_APE = median(ape, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(MAPE))

# MAPE by horizon (months since T0): shows whether error accumulates
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

# Global single-number summary (for the paper text)
global_MAPE    <- mean(validation_item_month$ape, na.rm = TRUE)
global_max_APE <- max(validation_item_month$ape, na.rm = TRUE)
worst_item     <- validation_summary_item %>% slice_max(MAPE, n = 1)

message(sprintf(
  "\n=== GLOBAL VALIDATION SUMMARY ===\n  MAPE (all items, all months): %.2f%%\n  Max APE (worst single obs):  %.2f%%\n  Worst item: %s in %s (MAPE = %.2f%%)\n",
  global_MAPE, global_max_APE,
  worst_item$articulo[1], worst_item$ciudad[1], worst_item$MAPE[1]
))

# -----------------------------
# 10. Horizon plot (key figure for the paper)
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
    title   = "Price extrapolation error by forecast horizon",
    subtitle = paste0("Validation window: Jan 2016 – Mar 2018 | ",
                      "Base date t0 = Jan 2016\n",
                      sprintf("Overall MAPE = %.2f%% | Max APE = %.2f%%",
                              global_MAPE, global_max_APE)),
    x = "Months since base date (t0 = Jan 2016)",
    y = "Mean Absolute Percentage Error (%)"
  ) +
  theme_bw(base_size = 11)

ggsave(file.path(out_dir, "fig_validation_horizon_mape.png"),
       p_horizon, width = 10, height = 5, dpi = 200)

# Sub-class MAPE chart
p_subclass <- validation_summary_subclass %>%
  mutate(subclase_ipc = fct_reorder(subclase_ipc, MAPE)) %>%
  ggplot(aes(x = MAPE, y = subclase_ipc)) +
  geom_col(fill = "#2E5FA3", alpha = 0.8) +
  geom_vline(xintercept = c(3, 5), linetype = "dashed",
             color = "#C0392B", linewidth = 0.6) +
  labs(
    title   = "Price extrapolation MAPE by CPI sub-class",
    subtitle = "Pooled over all items and cities in each sub-class",
    x = "MAPE (%)", y = "CPI sub-class"
  ) +
  theme_bw(base_size = 11)

ggsave(file.path(out_dir, "fig_validation_subclass_mape.png"),
       p_subclass, width = 9, height = 7, dpi = 200)

# -----------------------------
# 11. Save all outputs
# -----------------------------
fail_df_val <- if (length(fail_log_val) == 0) tibble() else bind_rows(fail_log_val)

write_csv(validation_item_month,      file.path(out_dir, "validation_item_month.csv"))
write_csv(validation_summary_item,    file.path(out_dir, "validation_summary_item.csv"))
write_csv(validation_summary_subclass,file.path(out_dir, "validation_summary_subclass.csv"))
write_csv(validation_summary_horizon, file.path(out_dir, "validation_summary_horizon.csv"))
write_csv(fail_df_val,                file.path(out_dir, "validation_failures.csv"))

write_xlsx(
  list(
    item_month    = validation_item_month,
    summary_item  = validation_summary_item,
    by_subclass   = validation_summary_subclass,
    by_horizon    = validation_summary_horizon,
    failures      = fail_df_val
  ),
  file.path(out_dir, "validation_price_extrapolation.xlsx")
)

saveRDS(validation_item_month, file.path(out_dir, "validation_item_month.rds"))

message("\nDone. All outputs saved to: ", out_dir)
