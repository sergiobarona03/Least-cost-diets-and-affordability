########################################################
## ROBUSTNESS CHECK — Solution D diagnostic
## Script 05a: Estimate lambda_j (price elasticity w.r.t.
##             CPI sub-class index) per item × city
##
## Purpose:
##   Before implementing the full Solution D, this script
##   checks whether lambda_j differs meaningfully from 1
##   across items. If lambdas are concentrated near 1 with
##   low R², Solution D will not improve upon BASE and
##   should be discarded. If lambdas are dispersed with
##   reasonable R², Solution D is worth implementing.
##
## Model (first differences to eliminate fixed effects):
##   Δlog(p_jct) = α_jc + λ_j × Δlog(IPC_sct) + ε_jct
##
##   λ_j = 1  →  item moves proportionally with sub-class
##   λ_j > 1  →  item amplifies sub-class movements
##   λ_j < 1  →  item dampens sub-class movements
##
## Estimation period: 1999-01 to 2015-12 (training only)
##
## Outputs (.../food-security-paper/robust/):
##   - lambda_estimates.csv      (lambda, R², n per item × city)
##   - lambda_summary_subclass.csv
##   - lambda_summary_global.csv
##   - fig_lambda_distribution.png
##   - fig_lambda_r2_scatter.png
##   - fig_lambda_by_subclass.png
########################################################

# -----------------------------
# 0. Packages
# -----------------------------
library(tidyverse)
library(readxl)
library(janitor)
library(lubridate)
library(writexl)
library(broom)

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

in_prices <- file.path(base_dir,
                       "Precios DANE/OUTPUT_DANE/precios_unadj_DANE_1999_2018.xlsx")
in_ipc    <- file.path(base_dir, "var-ipc/IPC.xls")
in_ipc2   <- file.path(base_dir, "var-ipc/IPC_2.xls")
in_ipc3   <- file.path(base_dir, "var-ipc/IPC_3.xls")
in_ipc4   <- file.path(base_dir, "var-ipc/IPC_4.xls")
in_corr1  <- file.path(base_dir, "var-ipc/correlativa_ipc.xlsx")
in_corr2  <- file.path(base_dir, "var-ipc/correlativa_ipc_articulos.xlsx")

out_dir <- file.path(base_dir, "food-security-paper/robust")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# -----------------------------
# 2. Parameters
# -----------------------------
TRAIN_END  <- as.Date("2015-12-01")
T0         <- as.Date("2016-01-01")
VAL_END    <- as.Date("2018-03-01")
cities_use <- c("CALI", "BOGOTÁ D.C.", "MEDELLÍN")

# Minimum observations required to estimate lambda reliably
MIN_OBS <- 24   # at least 24 monthly first differences

# -----------------------------
# 3. Auxiliary functions
# -----------------------------
meses_esp <- c("Ene","Feb","Mar","Abr",
               "May","Jun","Jul","Ago","Sep","Oct","Nov","Dic")

make_date <- function(ano, mes_num)
  as.Date(sprintf("%04d-%02d-01",
                  as.integer(ano), as.integer(mes_num)))

mode1 <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_character_)
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

safe_name <- function(x) {
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  x <- gsub("[^A-Za-z0-9_]+", "_", x)
  x <- gsub("_+", "_", x)
  x <- gsub("^_|_$", "", x)
  x
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
  filter(!is.na(fecha), !is.na(precio_500g),
         nombre_ciudad %in% cities_use,
         fecha <= TRAIN_END)

message("Loading CPI series...")
ipc <- map(list(in_ipc, in_ipc2, in_ipc3, in_ipc4),
           ~ read_excel(.x) %>% clean_names()) %>%
  bind_rows() %>%
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
         !is.na(cod_subclase), !is.na(ciudad),
         ciudad %in% cities_use,
         fecha <= TRAIN_END) %>%
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
# 5. Sub-class assignment
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
# 6. Main loop: estimate lambda per item × city
# -----------------------------
item_keys <- dane %>%
  distinct(nombre_ciudad, articulo)

message(sprintf("Estimating lambda for %d item × city series...",
                nrow(item_keys)))

all_lambdas <- list()
fail_log    <- list()

for (i in seq_len(nrow(item_keys))) {
  
  city_name <- item_keys$nombre_ciudad[i]
  food_name <- item_keys$articulo[i]
  
  # Observed price series (training period)
  df_obs <- dane %>%
    filter(nombre_ciudad == city_name,
           articulo == food_name) %>%
    mutate(
      codigo_articulo = as.character(codigo_articulo),
      cod_subclase    = as.character(cod_subclase)
    ) %>%
    arrange(fecha)
  
  if (nrow(df_obs) < MIN_OBS + 1) next
  
  # Sub-class assignment
  sub_info     <- assign_subclase(df_obs, corr_subclase, corr_producto)
  subclase_ipc <- sub_info$subclase
  if (is.na(subclase_ipc)) next
  
  subclase_code <- substr(paste0(subclase_ipc, "00"), 1, 8)
  
  # IPC series for this sub-class × city
  df_ipc <- ipc %>%
    filter(ciudad == city_name,
           cod_subclase == subclase_code) %>%
    select(fecha, ipc) %>%
    arrange(fecha)
  
  if (nrow(df_ipc) < MIN_OBS + 1) next
  
  # Merge price and IPC, compute log first differences
  df_merged <- df_obs %>%
    select(fecha, precio_500g) %>%
    inner_join(df_ipc, by = "fecha") %>%
    arrange(fecha) %>%
    mutate(
      log_p   = log(precio_500g),
      log_ipc = log(ipc),
      # First differences — eliminates item-level fixed effects
      d_log_p   = log_p   - lag(log_p),
      d_log_ipc = log_ipc - lag(log_ipc)
    ) %>%
    filter(!is.na(d_log_p), !is.na(d_log_ipc),
           is.finite(d_log_p), is.finite(d_log_ipc))
  
  if (nrow(df_merged) < MIN_OBS) next
  
  # OLS: Δlog(p) = α + λ × Δlog(IPC) + ε
  fit <- tryCatch(
    lm(d_log_p ~ d_log_ipc, data = df_merged),
    error = function(e) NULL
  )
  if (is.null(fit)) next
  
  coefs   <- tidy(fit)
  gofs    <- glance(fit)
  
  lambda  <- coefs$estimate[coefs$term == "d_log_ipc"]
  se_lam  <- coefs$std.error[coefs$term == "d_log_ipc"]
  p_lam   <- coefs$p.value[coefs$term == "d_log_ipc"]
  
  if (length(lambda) == 0) next
  
  # Test H0: lambda = 1 (t-test)
  t_stat_1 <- (lambda - 1) / se_lam
  p_neq_1  <- 2 * pt(-abs(t_stat_1), df = nrow(df_merged) - 2)
  
  all_lambdas[[length(all_lambdas) + 1]] <- tibble(
    ciudad        = city_name,
    articulo      = food_name,
    subclase_ipc  = subclase_ipc,
    n_obs         = nrow(df_merged),
    lambda        = lambda,
    se_lambda     = se_lam,
    p_lambda_zero = p_lam,        # H0: lambda = 0
    p_lambda_one  = p_neq_1,      # H0: lambda = 1
    lambda_neq_1  = p_neq_1 < 0.10,
    r2            = gofs$r.squared,
    sigma         = gofs$sigma     # residual SD = noise level
  )
}

lambdas_df <- bind_rows(all_lambdas)

message(sprintf("Lambda estimated for %d series.", nrow(lambdas_df)))

# -----------------------------
# 7. Summaries
# -----------------------------

# 7a. Global summary
global_summary <- lambdas_df %>%
  summarise(
    n_series          = n(),
    lambda_mean       = mean(lambda,       na.rm = TRUE),
    lambda_median     = median(lambda,     na.rm = TRUE),
    lambda_sd         = sd(lambda,         na.rm = TRUE),
    lambda_p10        = quantile(lambda, 0.10, na.rm = TRUE),
    lambda_p90        = quantile(lambda, 0.90, na.rm = TRUE),
    pct_near_1        = mean(abs(lambda - 1) <= 0.20, na.rm = TRUE) * 100,
    pct_neq_1_signif  = mean(lambda_neq_1, na.rm = TRUE) * 100,
    r2_mean           = mean(r2,           na.rm = TRUE),
    r2_median         = median(r2,         na.rm = TRUE),
    sigma_mean        = mean(sigma,        na.rm = TRUE)
  )

message("\n=== GLOBAL LAMBDA SUMMARY ===")
message(sprintf("  Mean lambda:   %.3f  (SD: %.3f)",
                global_summary$lambda_mean, global_summary$lambda_sd))
message(sprintf("  Median lambda: %.3f",
                global_summary$lambda_median))
message(sprintf("  P10 – P90:     %.3f – %.3f",
                global_summary$lambda_p10, global_summary$lambda_p90))
message(sprintf("  %% with |lambda-1| <= 0.20:  %.1f%%",
                global_summary$pct_near_1))
message(sprintf("  %% with lambda != 1 (p<0.10): %.1f%%",
                global_summary$pct_neq_1_signif))
message(sprintf("  Mean R²:   %.3f", global_summary$r2_mean))
message(sprintf("  Median R²: %.3f", global_summary$r2_median))
message(sprintf("  Mean residual SD (sigma): %.4f", global_summary$sigma_mean))

# 7b. Summary by sub-class
subclass_summary <- lambdas_df %>%
  group_by(subclase_ipc) %>%
  summarise(
    n_items           = n_distinct(articulo),
    n_series          = n(),
    lambda_mean       = mean(lambda,      na.rm = TRUE),
    lambda_sd         = sd(lambda,        na.rm = TRUE),
    lambda_min        = min(lambda,       na.rm = TRUE),
    lambda_max        = max(lambda,       na.rm = TRUE),
    pct_near_1        = mean(abs(lambda - 1) <= 0.20, na.rm = TRUE) * 100,
    pct_neq_1_signif  = mean(lambda_neq_1, na.rm = TRUE) * 100,
    r2_mean           = mean(r2,          na.rm = TRUE),
    r2_median         = median(r2,        na.rm = TRUE),
    sigma_mean        = mean(sigma,       na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(lambda_sd))   # most heterogeneous sub-classes first

message("\n=== LAMBDA SUMMARY BY SUB-CLASS (sorted by heterogeneity) ===")
print(subclass_summary %>%
        select(subclase_ipc, n_items, lambda_mean, lambda_sd,
               pct_near_1, pct_neq_1_signif, r2_mean, sigma_mean),
      n = 40)

# 7c. The three scenarios diagnosis
# Scenario 1: lambdas near 1, low R²    → BASE is optimal, discard D
# Scenario 2: lambdas dispersed, high R² → D has potential
# Scenario 3: lambdas dispersed, low R²  → idiosyncratic noise dominates

scenario_diagnosis <- lambdas_df %>%
  mutate(
    scenario = case_when(
      abs(lambda - 1) <= 0.20 & r2 >= 0.30 ~ "1a: near-1, good fit",
      abs(lambda - 1) <= 0.20 & r2 <  0.30 ~ "1b: near-1, poor fit",
      abs(lambda - 1) >  0.20 & r2 >= 0.30 ~ "2: dispersed, good fit → D viable",
      abs(lambda - 1) >  0.20 & r2 <  0.30 ~ "3: dispersed, poor fit → noise",
      TRUE ~ "other"
    )
  ) %>%
  count(scenario) %>%
  mutate(pct = n / sum(n) * 100)

message("\n=== SCENARIO DIAGNOSIS ===")
print(scenario_diagnosis)

# By sub-class: which scenario dominates?
scenario_by_subclass <- lambdas_df %>%
  mutate(
    scenario = case_when(
      abs(lambda - 1) <= 0.20 & r2 >= 0.30 ~ "1a",
      abs(lambda - 1) <= 0.20 & r2 <  0.30 ~ "1b",
      abs(lambda - 1) >  0.20 & r2 >= 0.30 ~ "2_D_viable",
      abs(lambda - 1) >  0.20 & r2 <  0.30 ~ "3_noise",
      TRUE ~ "other"
    )
  ) %>%
  group_by(subclase_ipc) %>%
  summarise(
    n_items       = n_distinct(articulo),
    dominant_scen = names(sort(table(scenario), decreasing = TRUE))[1],
    pct_D_viable  = mean(scenario == "2_D_viable") * 100,
    r2_mean       = mean(r2),
    lambda_sd     = sd(lambda),
    .groups = "drop"
  ) %>%
  arrange(desc(pct_D_viable))

message("\n=== SCENARIO BY SUB-CLASS (D-viable % descending) ===")
print(scenario_by_subclass, n = 40)

# -----------------------------
# 8. Validation preview:
#    Does lambda-corrected BASE outperform BASE in the
#    validation window (Jan 2016 – Mar 2018)?
#
#    p_hat_D = p_anchor × (IPC_t / IPC_t0)^lambda
#
#    We use the observed prices in 2016-2018 to check.
# -----------------------------
message("\nRunning validation preview with lambda correction...")

in_prices_ext <- file.path(base_dir,
                           "Precios DANE/OUTPUT_DANE/precios_unadj_DANE_1999_2018.xlsx")

dane_val <- read_excel(in_prices_ext) %>%
  mutate(
    fecha        = make_date(ano, mes_num),
    cod_subclase = paste0("0", substr(codigo_articulo, 1, 6), "0")
  ) %>%
  filter(!is.na(fecha), !is.na(precio_500g),
         nombre_ciudad %in% cities_use,
         fecha >= T0, fecha <= VAL_END)

prices_at_T0 <- read_excel(in_prices_ext) %>%
  mutate(
    fecha        = make_date(ano, mes_num),
    cod_subclase = paste0("0", substr(codigo_articulo, 1, 6), "0")
  ) %>%
  filter(!is.na(fecha), !is.na(precio_500g),
         nombre_ciudad %in% cities_use,
         fecha == T0) %>%
  select(nombre_ciudad, articulo, precio_500g) %>%
  rename(precio_anchor = precio_500g)

ipc_val <- map(list(in_ipc, in_ipc2, in_ipc3, in_ipc4),
               ~ read_excel(.x) %>% clean_names()) %>%
  bind_rows() %>%
  mutate(
    ciudad = case_when(
      ciudad == "CARTAGENA DE INDIAS" ~ "CARTAGENA",
      ciudad == "BOGOTÁ, D.C."        ~ "BOGOTÁ D.C.",
      TRUE ~ ciudad
    ),
    cod_subclase = substr(subclase, 1, 8),
    mes_num      = match(mes, meses_esp),
    ipc          = as.numeric(numero_indice),
    fecha        = make_date(as.integer(ano), mes_num)
  ) %>%
  select(ciudad, cod_subclase, fecha, ipc) %>%
  filter(!is.na(fecha), !is.na(ipc),
         ciudad %in% cities_use,
         fecha >= T0, fecha <= VAL_END) %>%
  distinct(ciudad, cod_subclase, fecha, .keep_all = TRUE)

ipc_at_T0 <- ipc_val %>%
  filter(fecha == T0) %>%
  rename(ipc_t0 = ipc) %>%
  select(ciudad, cod_subclase, ipc_t0)

# Join everything and compute APE for BASE and D
val_results <- list()

for (i in seq_len(nrow(item_keys))) {
  
  city_name <- item_keys$nombre_ciudad[i]
  food_name <- item_keys$articulo[i]
  
  lam_row <- lambdas_df %>%
    filter(ciudad == city_name, articulo == food_name)
  if (nrow(lam_row) == 0) next
  
  lambda_est <- lam_row$lambda[1]
  sub_code   <- substr(paste0(lam_row$subclase_ipc[1], "00"), 1, 8)
  
  anchor <- prices_at_T0 %>%
    filter(nombre_ciudad == city_name, articulo == food_name) %>%
    pull(precio_anchor)
  if (length(anchor) == 0) next
  anchor <- anchor[1]
  
  ipc_t0_val <- ipc_at_T0 %>%
    filter(ciudad == city_name, cod_subclase == sub_code) %>%
    pull(ipc_t0)
  if (length(ipc_t0_val) == 0) next
  ipc_t0_val <- ipc_t0_val[1]
  
  obs_val <- dane_val %>%
    filter(nombre_ciudad == city_name, articulo == food_name) %>%
    select(fecha, price_obs = precio_500g)
  
  ipc_series_val <- ipc_val %>%
    filter(ciudad == city_name, cod_subclase == sub_code) %>%
    select(fecha, ipc)
  
  if (nrow(obs_val) == 0 || nrow(ipc_series_val) == 0) next
  
  comp <- obs_val %>%
    inner_join(ipc_series_val, by = "fecha") %>%
    mutate(
      ipc_ratio  = ipc / ipc_t0_val,
      price_BASE = anchor * ipc_ratio,
      price_D    = anchor * ipc_ratio ^ lambda_est,
      ape_BASE   = abs((price_BASE - price_obs) / price_obs) * 100,
      ape_D      = abs((price_D   - price_obs) / price_obs) * 100,
      ciudad     = city_name,
      articulo   = food_name,
      subclase_ipc = lam_row$subclase_ipc[1],
      lambda     = lambda_est,
      r2         = lam_row$r2[1]
    )
  
  val_results[[length(val_results) + 1]] <- comp
}

val_df <- bind_rows(val_results)

# Global MAPE comparison
val_global <- val_df %>%
  summarise(
    n_obs           = n(),
    MAPE_BASE       = mean(ape_BASE, na.rm = TRUE),
    MAPE_D          = mean(ape_D,    na.rm = TRUE),
    max_BASE        = max(ape_BASE,  na.rm = TRUE),
    max_D           = max(ape_D,     na.rm = TRUE),
    pct_D_beats_BASE = mean(ape_D < ape_BASE, na.rm = TRUE) * 100
  )

message("\n=== VALIDATION PREVIEW: BASE vs. Solution D ===")
message(sprintf("  BASE : %.2f%%  (max: %.2f%%)",
                val_global$MAPE_BASE, val_global$max_BASE))
message(sprintf("  D    : %.2f%%  (max: %.2f%%)  — beats BASE in %.1f%% of obs",
                val_global$MAPE_D, val_global$max_D,
                val_global$pct_D_beats_BASE))

# By sub-class
val_subclass <- val_df %>%
  group_by(subclase_ipc) %>%
  summarise(
    n_items        = n_distinct(articulo),
    MAPE_BASE      = mean(ape_BASE, na.rm = TRUE),
    MAPE_D         = mean(ape_D,    na.rm = TRUE),
    imp_D          = MAPE_BASE - MAPE_D,
    D_beats_BASE   = MAPE_D < MAPE_BASE,
    lambda_mean    = mean(lambda,   na.rm = TRUE),
    r2_mean        = mean(r2,       na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(MAPE_BASE))

message("\n=== VALIDATION BY SUB-CLASS ===")
print(val_subclass %>%
        select(subclase_ipc, n_items, MAPE_BASE, MAPE_D,
               imp_D, D_beats_BASE, lambda_mean, r2_mean),
      n = 40)

# Key question: does D help in the problematic sub-classes?
message("\n=== D improvement in HIGH-MAPE sub-classes (MAPE_BASE >= 5%) ===")
print(val_subclass %>%
        filter(MAPE_BASE >= 5) %>%
        select(subclase_ipc, n_items, MAPE_BASE, MAPE_D, imp_D,
               D_beats_BASE, lambda_mean, r2_mean))

# -----------------------------
# 9. Figures
# -----------------------------

# 9a. Lambda distribution
p_lambda_dist <- ggplot(lambdas_df, aes(x = lambda)) +
  geom_histogram(bins = 40, fill = "#2E5FA3", color = "white",
                 alpha = 0.85) +
  geom_vline(xintercept = 1, color = "#C0392B",
             linewidth = 1, linetype = "dashed") +
  geom_vline(xintercept = c(0.8, 1.2), color = "grey50",
             linewidth = 0.7, linetype = "dotted") +
  annotate("text", x = 1.05, y = Inf, vjust = 1.5, hjust = 0,
           label = "lambda = 1\n(BASE assumption)",
           color = "#C0392B", size = 3.5) +
  annotate("text", x = 0.75, y = Inf, vjust = 1.5, hjust = 1,
           label = "±0.20 band",
           color = "grey50", size = 3) +
  labs(
    title    = "Distribution of lambda estimates per item × city",
    subtitle = paste0(
      sprintf("Mean = %.3f | Median = %.3f | SD = %.3f\n",
              global_summary$lambda_mean,
              global_summary$lambda_median,
              global_summary$lambda_sd),
      sprintf("%.1f%% within |lambda-1| <= 0.20 | ",
              global_summary$pct_near_1),
      sprintf("%.1f%% significantly != 1 (p<0.10)",
              global_summary$pct_neq_1_signif)),
    x = "Lambda (price elasticity w.r.t. CPI sub-class)",
    y = "Count"
  ) +
  theme_bw(base_size = 12)

ggsave(file.path(out_dir, "fig_lambda_distribution.png"),
       p_lambda_dist, width = 9, height = 5, dpi = 200)

# 9b. Lambda vs R²: the key diagnostic scatter
p_lambda_r2 <- ggplot(lambdas_df,
                      aes(x = lambda, y = r2, color = subclase_ipc)) +
  geom_point(alpha = 0.6, size = 2) +
  geom_vline(xintercept = c(0.8, 1.2), linetype = "dashed",
             color = "grey60", linewidth = 0.6) +
  geom_hline(yintercept = 0.30, linetype = "dashed",
             color = "#C0392B", linewidth = 0.6) +
  # Annotate four quadrants
  annotate("rect", xmin = 0.8, xmax = 1.2, ymin = 0.30, ymax = 1,
           fill = "#2E5FA3", alpha = 0.06) +
  annotate("rect", xmin = -Inf, xmax = 0.8,  ymin = 0.30, ymax = 1,
           fill = "#1A7A4A", alpha = 0.06) +
  annotate("rect", xmin = 1.2,  xmax = Inf,   ymin = 0.30, ymax = 1,
           fill = "#1A7A4A", alpha = 0.06) +
  annotate("text", x = 1.0,  y = 0.95,
           label = "Scen 1a\nBASE ok",
           size = 3, color = "#2E5FA3", fontface = "bold") +
  annotate("text", x = 1.85, y = 0.95,
           label = "Scen 2\nD viable",
           size = 3, color = "#1A7A4A", fontface = "bold") +
  annotate("text", x = 1.0,  y = 0.10,
           label = "Scen 1b\nBASE ok\n(noise)",
           size = 3, color = "grey40") +
  annotate("text", x = 1.85, y = 0.10,
           label = "Scen 3\nNoise\ndominates",
           size = 3, color = "#C0392B") +
  labs(
    title    = "Lambda vs. R²: scenario diagnosis per item × city",
    subtitle = paste0(
      "Dashed lines: |lambda-1| = 0.20 (vertical) | R² = 0.30 (horizontal)\n",
      "Green zone (Scenario 2) = Solution D potentially useful"),
    x     = "Lambda estimate",
    y     = "R² (goodness of fit)",
    color = "CPI sub-class"
  ) +
  theme_bw(base_size = 11) +
  theme(legend.position = "right",
        legend.text = element_text(size = 7))

ggsave(file.path(out_dir, "fig_lambda_r2_scatter.png"),
       p_lambda_r2, width = 11, height = 6, dpi = 200)

# 9c. Lambda boxplot by sub-class
p_lambda_subclass <- lambdas_df %>%
  mutate(subclase_ipc = fct_reorder(subclase_ipc,
                                    lambda, .fun = median)) %>%
  ggplot(aes(x = lambda, y = subclase_ipc)) +
  geom_boxplot(fill = "#2E5FA3", alpha = 0.6,
               outlier.size = 1.5, outlier.color = "#C0392B") +
  geom_vline(xintercept = 1, linetype = "dashed",
             color = "#C0392B", linewidth = 0.8) +
  geom_vline(xintercept = c(0.8, 1.2), linetype = "dotted",
             color = "grey50", linewidth = 0.6) +
  labs(
    title    = "Lambda distribution by CPI sub-class",
    subtitle = "Dashed red = 1 (BASE assumption) | Dotted grey = ±0.20 band",
    x        = "Lambda",
    y        = "CPI sub-class"
  ) +
  theme_bw(base_size = 11) +
  theme(panel.grid.major.y = element_blank())

ggsave(file.path(out_dir, "fig_lambda_by_subclass.png"),
       p_lambda_subclass, width = 10, height = 8, dpi = 200)

# 9d. Validation: BASE vs D improvement by sub-class
p_val_improvement <- val_subclass %>%
  mutate(
    subclase_ipc = fct_reorder(subclase_ipc, MAPE_BASE),
    color_flag   = case_when(
      imp_D > 1  ~ "Mejora >1pp",
      imp_D > 0  ~ "Mejora leve",
      TRUE       ~ "Empeora"
    )
  ) %>%
  ggplot(aes(y = subclase_ipc)) +
  geom_segment(aes(x = MAPE_D, xend = MAPE_BASE, yend = subclase_ipc),
               color = "grey70", linewidth = 1.2) +
  geom_point(aes(x = MAPE_BASE), color = "#95A5A6", size = 3.5) +
  geom_point(aes(x = MAPE_D, color = color_flag), size = 3.5, shape = 17) +
  scale_color_manual(values = c("Mejora >1pp"  = "#1A7A4A",
                                "Mejora leve"  = "#2E5FA3",
                                "Empeora"      = "#C0392B")) +
  geom_vline(xintercept = c(3, 5), linetype = "dashed",
             color = "grey50", linewidth = 0.5) +
  labs(
    title    = "Validation MAPE: BASE (circle) vs. Solution D (triangle)",
    subtitle = "Triangle left of circle = D improves. Red = D worsens.",
    x        = "MAPE (%)",
    y        = "CPI sub-class",
    color    = NULL
  ) +
  theme_bw(base_size = 11) +
  theme(legend.position = "bottom",
        panel.grid.major.y = element_blank())

ggsave(file.path(out_dir, "fig_solutionD_validation.png"),
       p_val_improvement, width = 10, height = 8, dpi = 200)

# -----------------------------
# 10. Save outputs
# -----------------------------
fail_df <- if (length(fail_log) == 0) tibble() else bind_rows(fail_log)

write_csv(lambdas_df,
          file.path(out_dir, "lambda_estimates.csv"))
write_csv(subclass_summary,
          file.path(out_dir, "lambda_summary_subclass.csv"))
write_csv(global_summary %>% pivot_longer(everything(),
                                          names_to = "metric", values_to = "value"),
          file.path(out_dir, "lambda_summary_global.csv"))
write_csv(scenario_diagnosis,
          file.path(out_dir, "lambda_scenario_diagnosis.csv"))
write_csv(scenario_by_subclass,
          file.path(out_dir, "lambda_scenario_by_subclass.csv"))
write_csv(val_df,
          file.path(out_dir, "solutionD_validation_item_month.csv"))
write_csv(val_subclass,
          file.path(out_dir, "solutionD_validation_subclass.csv"))
write_csv(val_global %>% pivot_longer(everything(),
                                      names_to = "metric", values_to = "value"),
          file.path(out_dir, "solutionD_validation_global.csv"))

write_xlsx(
  list(
    lambda_estimates    = lambdas_df,
    by_subclass         = subclass_summary,
    global_summary      = global_summary %>%
      pivot_longer(everything(),
                   names_to = "metric", values_to = "value"),
    scenario_diagnosis  = scenario_diagnosis,
    scenario_subclass   = scenario_by_subclass,
    validation_global   = val_global %>%
      pivot_longer(everything(),
                   names_to = "metric", values_to = "value"),
    validation_subclass = val_subclass,
    failures            = fail_df
  ),
  file.path(out_dir, "solutionD_diagnostic.xlsx")
)

message("\n=== DONE (Script 05a — Solution D diagnostic) ===")
message("Key output: solutionD_diagnostic.xlsx")
message(sprintf(
  "\nDecision guide:\n  If MAPE_D < MAPE_BASE globally → run Script 05b (full D implementation)\n  If MAPE_D >= MAPE_BASE globally → discard D, BASE is optimal\n  Current result: BASE = %.2f%% | D = %.2f%%",
  val_global$MAPE_BASE, val_global$MAPE_D))