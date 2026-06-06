########################################################
## ROBUSTNESS CHECK — Price extrapolation assumption
## Script 02: CoCA cost validation
##
## Logic:
##   Using the validation window (Jan 2016 – Mar 2018),
##   we estimate the CoCA under:
##     (a) observed prices        → CoCA_obs
##     (b) simulated/extrapolated prices → CoCA_hat
##   and compute ΔCoCA = (CoCA_hat - CoCA_obs) / CoCA_obs × 100
##
## This directly answers the reviewer's concern: even if
## item-level prices are imprecise, does the LP outcome change?
##
## IMPORTANT: this script depends on
##   - Script 01 output: validation_item_month.rds
##   - Your existing CoCA function: CoCA_paper.R
##   - Your existing EER table
##   - Your existing food composition (TCAC) table
##
## Save to: .../food-security-paper/robust/
########################################################

# -----------------------------
# 0. Packages
# -----------------------------
library(tidyverse)
library(readxl)
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

aux_dir    <- file.path(base_dir, "food-security-paper", "models", "aux-functions")
out_eer    <- file.path(base_dir, "food-security-paper", "output", "eer")
tcac_dir   <- file.path(base_dir, "food-security-paper", "output", "tcac_food_table")
robust_dir <- file.path(base_dir, "food-security-paper", "robust")

dir.create(robust_dir, recursive = TRUE, showWarnings = FALSE)

# Load your CoCA function (unchanged)
source(file.path(aux_dir, "CoCA_paper.R"))

# -----------------------------
# 2. Load inputs
# -----------------------------

# 2a. Validation prices from Script 01
# Contains: ciudad, articulo, fecha, price_obs, price_hat, subclase_ipc
val_prices <- readRDS(file.path(robust_dir, "validation_item_month.rds"))

# 2b. Food composition table (TCAC)
# Contains nutritional content per 100g — same as used in main pipeline
panel_full <- readRDS(file.path(tcac_dir, "panel_city_month_food_1999_2025.rds"))

# We need one row per articulo with nutritional info (time-invariant)
tcac_nutrients <- panel_full %>%
  select(articulo, grupos_gabas, subgrupos_gabas,
         energia_kcal, proteina_g, lipidos_g, carbohidratos_totales_g,
         vitamina_c_mg, folatos_mcg, vitamina_a_er, tiamina_mg,
         riboflavina_mg, niacina_mg, vitamina_b12_mcg,
         magnesio_mg, fosforo_mg, sodio_mg, calcio_mg, hierro_mg, zinc_mg,
         gramos_g_1_intercambio_1_intercambio) %>%
  distinct(articulo, .keep_all = TRUE)

# 2c. EER for the representative household (same as main CoCA script)
agg_eer <- read_excel(file.path(out_eer, "220326_agg_eer.xlsx"))

household_eer <- agg_eer %>%
  filter(
    (sex == "Masculino" & rango == "[31,51)") |
    (sex == "Femenino"  & rango == "[31,51)") |
    (sex == "Femenino"  & rango == "[10, 14)")
  ) %>%
  mutate(ciudad = case_when(
    cod_mun == "05001"    ~ "MEDELLIN",
    cod_mun == "11001"    ~ "BOGOTA",
    cod_mun == "76001"    ~ "CALI",
    TRUE ~ cod_mun
  )) %>%
  filter(ciudad != "COLOMBIA")

preparar_eer <- function(eer_ciudad) {
  eer_ciudad %>%
    rename(Age = rango, Sex = sex, Energy = eer) %>%
    mutate(Sex = if_else(Sex == "Masculino", 0L, 1L)) %>%
    as.data.frame()
}

# -----------------------------
# 3. Normalise city names to match across datasets
# -----------------------------
# val_prices uses: "CALI", "BOGOTÁ D.C.", "MEDELLÍN"
# household_eer uses: "CALI", "BOGOTA", "MEDELLIN"
# We unify to the household_eer convention for the LP loop

norm_city_eer <- function(x) {
  case_when(
    x == "BOGOTÁ D.C." ~ "BOGOTA",
    x == "MEDELLÍN"    ~ "MEDELLIN",
    x == "CALI"        ~ "CALI",
    TRUE ~ x
  )
}

# Restrict to cereals only (CoCA only depends on this group —
# the LP selects the cheapest calorie source, which is always a starchy staple)
# This mirrors the filter in your main CoCA script exactly.
val_prices_cereals <- val_prices %>%
  left_join(tcac_nutrients %>% select(articulo, grupos_gabas),
            by = "articulo") %>%
  filter(grupos_gabas == "CEREALES, RAÍCES, TUBÉRCULOS Y PLÁTANOS") %>%
  mutate(ciudad_eer = norm_city_eer(ciudad))

# Validation window: the dates present in Script 01 output
val_fechas <- sort(unique(val_prices_cereals$fecha))

message(sprintf("Validation window: %s to %s (%d months)",
                min(val_fechas), max(val_fechas), length(val_fechas)))

# -----------------------------
# 4. Build the two price data frames for the LP:
#    one using price_obs, one using price_hat
# -----------------------------

# Helper: convert 500g price to price per 100g edible portion
# Edible portion (pc) comes from the panel.
# For CoCA we only need energy and price; pc is already embedded in
# precio_100g in the main pipeline. Here we reconstruct it.

pc_table <- panel_full %>%
  select(articulo, parte_comestible_percent) %>%
  distinct(articulo, .keep_all = TRUE) %>%
  mutate(
    pc = case_when(
      is.na(parte_comestible_percent) ~ NA_real_,
      parte_comestible_percent > 1    ~ parte_comestible_percent / 100,
      TRUE                            ~ parte_comestible_percent
    )
  )

make_food_table <- function(val_data, price_col) {
  # price_col: either "price_obs" or "price_hat" (both in COP/500g)
  val_data %>%
    mutate(precio_500g = .data[[price_col]]) %>%
    left_join(pc_table, by = "articulo") %>%
    left_join(tcac_nutrients, by = "articulo") %>%
    mutate(
      precio_100g = (precio_500g / 5) / pc   # COP per 100g edible
    ) %>%
    filter(!is.na(precio_100g), !is.na(energia_kcal),
           energia_kcal > 0) %>%
    rename(
      Price_100g = precio_100g,
      Food       = articulo,
      Energy     = energia_kcal
    ) %>%
    select(ciudad, ciudad_eer, fecha, Food, Price_100g, Energy) %>%
    filter(Food != "ARROZ PARA SOPA")   # same exclusion as main script
}

food_obs <- make_food_table(val_prices_cereals, "price_obs")
food_hat <- make_food_table(val_prices_cereals, "price_hat")

# -----------------------------
# 5. Run the CoCA LP under both price scenarios
# -----------------------------
cities_eer <- unique(household_eer$ciudad)

run_coca_scenario <- function(food_df, scenario_label) {
  
  results <- list()
  
  for (city_eer in cities_eer) {
    
    eer_aux <- preparar_eer(household_eer %>% filter(ciudad == city_eer))
    fechas  <- sort(unique(food_df$fecha[food_df$ciudad_eer == city_eer]))
    
    for (t in fechas) {
      
      data_aux <- food_df %>%
        filter(ciudad_eer == city_eer,
               fecha == t,
               !is.na(Price_100g)) %>%
        as.data.frame()
      
      if (nrow(data_aux) == 0) next
      
      coca_res <- tryCatch(
        CoCA_paper(data = data_aux, EER = eer_aux),
        error = function(e) NULL
      )
      
      if (!is.null(coca_res)) {
        results[[length(results) + 1]] <- coca_res$cost %>%
          mutate(ciudad = city_eer, fecha = as.Date(t),
                 scenario = scenario_label)
      }
    }
  }
  
  bind_rows(results)
}

message("Running CoCA with OBSERVED prices...")
coca_obs <- run_coca_scenario(food_obs, "observed")

message("Running CoCA with SIMULATED (extrapolated) prices...")
coca_hat <- run_coca_scenario(food_hat, "simulated")

# -----------------------------
# 6. Compute ΔCoCA
# -----------------------------
# CoCA_paper returns one row per household member per city × date.
# We join on ciudad × fecha × member identifier.

# Identify the member column (check what CoCA_paper returns)
member_cols <- setdiff(names(coca_obs),
                       c("ciudad", "fecha", "scenario",
                         "cost", "Cost", "total_cost"))

message("Member identifier columns detected: ",
        paste(member_cols, collapse = ", "))

# Standardise cost column name
standardise_cost <- function(df) {
  if ("cost" %in% names(df))       rename(df, coca_cost = cost)
  else if ("Cost" %in% names(df))  rename(df, coca_cost = Cost)
  else if ("total_cost" %in% names(df)) rename(df, coca_cost = total_cost)
  else {
    # Try to find any numeric column that looks like a cost
    num_cols <- names(df)[sapply(df, is.numeric)]
    rename(df, coca_cost = !!sym(num_cols[1]))
  }
}

coca_obs2 <- standardise_cost(coca_obs) %>%
  rename(coca_obs = coca_cost) %>%
  select(-scenario)

coca_hat2 <- standardise_cost(coca_hat) %>%
  rename(coca_hat = coca_cost) %>%
  select(-scenario)

# Join keys: ciudad, fecha, plus any member/demographic columns
join_keys <- c("ciudad", "fecha", member_cols)

delta_coca <- coca_obs2 %>%
  inner_join(coca_hat2, by = join_keys) %>%
  mutate(
    delta_coca_pct = (coca_hat - coca_obs) / coca_obs * 100,
    abs_delta_pct  = abs(delta_coca_pct)
  )

# -----------------------------
# 7. Summarise ΔCoCA
# -----------------------------

# Global summary
global_delta <- delta_coca %>%
  summarise(
    mean_abs_delta = mean(abs_delta_pct, na.rm = TRUE),
    max_abs_delta  = max(abs_delta_pct, na.rm = TRUE),
    median_delta   = median(delta_coca_pct, na.rm = TRUE),
    sd_delta       = sd(delta_coca_pct, na.rm = TRUE),
    pct_below_1    = mean(abs_delta_pct < 1, na.rm = TRUE) * 100,
    pct_below_3    = mean(abs_delta_pct < 3, na.rm = TRUE) * 100,
    pct_below_5    = mean(abs_delta_pct < 5, na.rm = TRUE) * 100,
    n_obs          = n()
  )

message(sprintf(
  "\n=== CoCA VALIDATION SUMMARY ===\n  Mean |ΔCoCA|: %.2f%%\n  Max  |ΔCoCA|: %.2f%%\n  %% obs with |ΔCoCA| < 1%%: %.1f%%\n  %% obs with |ΔCoCA| < 3%%: %.1f%%\n  %% obs with |ΔCoCA| < 5%%: %.1f%%\n",
  global_delta$mean_abs_delta,
  global_delta$max_abs_delta,
  global_delta$pct_below_1,
  global_delta$pct_below_3,
  global_delta$pct_below_5
))

# By city
delta_by_city <- delta_coca %>%
  group_by(ciudad) %>%
  summarise(
    mean_abs_delta = mean(abs_delta_pct, na.rm = TRUE),
    max_abs_delta  = max(abs_delta_pct, na.rm = TRUE),
    .groups = "drop"
  )

# By horizon (months since T0) — attach horizon from val_prices
horizon_map <- val_prices %>%
  select(ciudad, articulo, fecha, horizon_mths) %>%
  distinct()

delta_coca_horizon <- delta_coca %>%
  left_join(
    horizon_map %>% 
      group_by(ciudad, fecha) %>% 
      summarise(horizon_mths = first(horizon_mths), .groups = "drop"),
    by = c("ciudad", "fecha")
  ) %>%
  group_by(horizon_mths) %>%
  summarise(
    mean_abs_delta = mean(abs_delta_pct, na.rm = TRUE),
    max_abs_delta  = max(abs_delta_pct, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(horizon_mths)

# -----------------------------
# 8. Plots
# -----------------------------

# ΔCoCA distribution
p_dist <- ggplot(delta_coca, aes(x = delta_coca_pct)) +
  geom_histogram(bins = 40, fill = "#2E5FA3", color = "white",
                 alpha = 0.85) +
  geom_vline(xintercept = c(-3, 3), linetype = "dashed",
             color = "#C0392B", linewidth = 0.7) +
  geom_vline(xintercept = 0, color = "black", linewidth = 0.5) +
  annotate("text", x = 3.5, y = Inf, vjust = 1.5, hjust = 0,
           label = "±3% band", color = "#C0392B", size = 3.5) +
  labs(
    title    = "Distribution of CoCA cost deviation: simulated vs. observed prices",
    subtitle = sprintf(
      "ΔCoCA = (CoCA_hat − CoCA_obs) / CoCA_obs × 100\nMean |ΔCoCA| = %.2f%% | Max = %.2f%%",
      global_delta$mean_abs_delta, global_delta$max_abs_delta),
    x = "ΔCoCA (%)", y = "Count"
  ) +
  theme_bw(base_size = 11)

ggsave(file.path(robust_dir, "fig_delta_coca_distribution.png"),
       p_dist, width = 9, height = 5, dpi = 200)

# ΔCoCA by horizon
p_horizon_coca <- ggplot(delta_coca_horizon,
                         aes(x = horizon_mths, y = mean_abs_delta)) +
  geom_col(fill = "#2E5FA3", alpha = 0.8, width = 0.7) +
  geom_line(color = "#C0392B", linewidth = 0.8) +
  geom_point(color = "#C0392B", size = 2) +
  geom_hline(yintercept = c(1, 3), linetype = "dashed",
             color = "grey50", linewidth = 0.5) +
  labs(
    title    = "CoCA cost deviation by forecast horizon",
    subtitle = "Mean |ΔCoCA| at each month since base date t0 = Jan 2016",
    x = "Months since t0",
    y = "Mean |ΔCoCA| (%)"
  ) +
  theme_bw(base_size = 11)

ggsave(file.path(robust_dir, "fig_delta_coca_horizon.png"),
       p_horizon_coca, width = 9, height = 5, dpi = 200)

# Scatter: CoCA_obs vs CoCA_hat by city
p_scatter <- ggplot(delta_coca,
                    aes(x = coca_obs, y = coca_hat, color = ciudad)) +
  geom_point(alpha = 0.5, size = 1.2) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed",
              color = "black", linewidth = 0.6) +
  labs(
    title    = "CoCA: observed vs. simulated prices",
    subtitle = "Each point = one city × month × household member",
    x = "CoCA under observed prices (COP/day)",
    y = "CoCA under simulated prices (COP/day)",
    color = "City"
  ) +
  theme_bw(base_size = 11) +
  theme(legend.position = "bottom")

ggsave(file.path(robust_dir, "fig_coca_scatter_obs_vs_hat.png"),
       p_scatter, width = 7, height = 6, dpi = 200)

# -----------------------------
# 9. Save outputs
# -----------------------------
write_csv(delta_coca,           file.path(robust_dir, "coca_delta_item_month.csv"))
write_csv(global_delta,         file.path(robust_dir, "coca_delta_global_summary.csv"))
write_csv(delta_by_city,        file.path(robust_dir, "coca_delta_by_city.csv"))
write_csv(delta_coca_horizon,   file.path(robust_dir, "coca_delta_by_horizon.csv"))

write_xlsx(
  list(
    delta_by_obs     = delta_coca,
    global_summary   = global_delta,
    by_city          = delta_by_city,
    by_horizon       = delta_coca_horizon
  ),
  file.path(robust_dir, "validation_coca_costs.xlsx")
)

saveRDS(delta_coca, file.path(robust_dir, "coca_delta_item_month.rds"))

message("\nDone. All CoCA validation outputs saved to: ", robust_dir)
