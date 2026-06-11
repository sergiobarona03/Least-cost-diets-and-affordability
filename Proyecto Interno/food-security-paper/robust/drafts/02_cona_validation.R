########################################################
## CoNA VALIDATION — S5 vs BASE vs Observed
## Script robust_09_cona_validation.R
##
## Price inputs come directly from Script 08:
##   prices_validation_panel.csv
##     → price_BASE   : anchor × (IPC_t/IPC_T0)^1
##     → price_final  : anchor × (IPC_t/IPC_T0)^λ* (S5)
##
##   Observed prices: DANE raw survey
##
## Three scenarios fed into CoNA_paper():
##   S1 — BASE    : price_BASE  (λ=1)
##   S2 — S5      : price_final (JS-shrunk λ*)
##   S3 — Observed: DANE precio_500g
##
## Validation window: Feb 2016 – Mar 2018
## (Jan excluded: IPC ratio=1 at T0, all methods identical)
##
## Key metric:
##   MADE = mean |CoNA_S - CoNA_obs| / CoNA_obs × 100
########################################################

# -----------------------------------------------------------------------
# 0. Packages
# -----------------------------------------------------------------------
library(tidyverse)
library(readxl)
library(writexl)
library(FoodpriceR)

# -----------------------------------------------------------------------
# 1. Directories
# -----------------------------------------------------------------------
dirs <- c(
  "C:\\Users\\sergio.barona\\Desktop\\Least-cost-diets-and-affordability\\Proyecto Interno",
  "C:/Users/Portatil/Desktop/Least-cost-diets-and-affordability/Proyecto Interno",
  "C:/Users/danie/OneDrive/Escritorio/Least-cost-diets-and-affordability/Proyecto Interno"
)
base_dir <- dirs[dir.exists(dirs)][1]
if (is.na(base_dir)) stop("Ninguno de los directorios base existe.")

# Output of Script 08
in_val_panel <- file.path(base_dir,
                          "food-security-paper/output/price_forecasting/prices_validation_panel.csv")

# DANE raw prices for S3 (observed ground truth)
in_dane <- file.path(base_dir,
                     "Precios DANE/OUTPUT_DANE/precios_unadj_DANE_1999_2018.xlsx")

aux_dir    <- file.path(base_dir, "food-security-paper", "models",
                        "aux-functions")
out_eer    <- file.path(base_dir, "food-security-paper", "output", "eer")
tcac_dir   <- file.path(base_dir, "food-security-paper", "output",
                        "tcac_food_table")
robust_dir <- file.path(base_dir, "food-security-paper", "robust")

dir.create(robust_dir, recursive = TRUE, showWarnings = FALSE)
source(file.path(aux_dir, "CoNA_paper.R"))

# -----------------------------------------------------------------------
# 2. Parameters
# -----------------------------------------------------------------------
VAL_START  <- as.Date("2016-02-01")   # Feb: first month IPC ratio ≠ 1
VAL_END    <- as.Date("2018-03-01")
cities_use <- c("CALI", "BOGOTÁ D.C.", "MEDELLÍN")

make_date <- function(ano, mes_num)
  as.Date(sprintf("%04d-%02d-01",
                  as.integer(ano), as.integer(mes_num)))

normalise_city <- function(df, col = "ciudad") {
  df %>% mutate(!!col := case_when(
    .data[[col]] == "BOGOTÁ D.C." ~ "BOGOTA",
    .data[[col]] == "MEDELLÍN"    ~ "MEDELLIN",
    TRUE                          ~ .data[[col]]))
}

# -----------------------------------------------------------------------
# 3. Load inputs
# -----------------------------------------------------------------------
message("Loading inputs...")

# Validation panel from Script 08
val_panel <- read_csv(in_val_panel, show_col_types = FALSE) %>%
  mutate(fecha = as.Date(fecha)) %>%
  filter(fecha >= VAL_START, fecha <= VAL_END)

# DANE observed prices
meses_esp <- c("Ene","Feb","Mar","Abr",
               "May","Jun","Jul","Ago","Sep","Oct","Nov","Dic")

dane <- read_excel(in_dane) %>%
  mutate(fecha = make_date(ano, mes_num)) %>%
  filter(!is.na(fecha), !is.na(precio_500g),
         nombre_ciudad %in% cities_use,
         fecha >= VAL_START, fecha <= VAL_END)

# Nutritional composition and edible portion (time-invariant)
panel_full <- readRDS(
  file.path(tcac_dir, "panel_city_month_food_1999_2025.rds"))

nutrients <- panel_full %>%
  select(articulo,
         gramos_g_1_intercambio_1_intercambio,
         energia_kcal, proteina_g, lipidos_g, carbohidratos_totales_g,
         vitamina_c_mg, folatos_mcg, vitamina_a_er, tiamina_mg,
         riboflavina_mg, niacina_mg, vitamina_b12_mcg,
         magnesio_mg, fosforo_mg, sodio_mg, calcio_mg,
         hierro_mg, zinc_mg) %>%
  distinct(articulo, .keep_all = TRUE)

pc_table <- panel_full %>%
  select(articulo, parte_comestible_percent) %>%
  distinct(articulo, .keep_all = TRUE) %>%
  mutate(pc = case_when(
    is.na(parte_comestible_percent)  ~ NA_real_,
    parte_comestible_percent > 1     ~ parte_comestible_percent / 100,
    TRUE                             ~ parte_comestible_percent))

message(sprintf(
  "  val_panel: %d rows | DANE obs: %d rows",
  nrow(val_panel), nrow(dane)))

# -----------------------------------------------------------------------
# 4. Helper: convert precio_500g → precio_100g and join nutrients
# -----------------------------------------------------------------------
rename_nutrients <- function(df) {
  df %>% rename(
    Energy        = energia_kcal,
    Protein       = proteina_g,
    Lipids        = lipidos_g,
    Carbohydrates = carbohidratos_totales_g,
    VitaminC      = vitamina_c_mg,
    Folate        = folatos_mcg,
    VitaminA      = vitamina_a_er,
    Thiamine      = tiamina_mg,
    Riboflavin    = riboflavina_mg,
    Niacin        = niacina_mg,
    VitaminB12    = vitamina_b12_mcg,
    Magnesium     = magnesio_mg,
    Phosphorus    = fosforo_mg,
    Sodium        = sodio_mg,
    Calcium       = calcio_mg,
    Iron          = hierro_mg,
    Zinc          = zinc_mg)
}

out_cols <- c("ciudad", "fecha", "articulo",
              "gramos_g_1_intercambio_1_intercambio", "precio_100g",
              "Energy", "Protein", "Lipids", "Carbohydrates",
              "VitaminC", "Folate", "VitaminA", "Thiamine",
              "Riboflavin", "Niacin", "VitaminB12",
              "Magnesium", "Phosphorus", "Sodium",
              "Calcium", "Iron", "Zinc")

make_cona_panel <- function(df, price_col) {
  # df must have: ciudad, fecha, articulo, <price_col> in COP/500g
  df %>%
    rename(precio_500g = {{ price_col }}) %>%
    left_join(pc_table, by = "articulo") %>%
    mutate(precio_100g = (precio_500g / 5) / pc) %>%
    filter(!is.na(precio_100g), precio_100g > 0) %>%
    left_join(nutrients, by = "articulo") %>%
    filter(!is.na(energia_kcal)) %>%
    normalise_city("ciudad") %>%
    rename_nutrients() %>%
    select(all_of(out_cols)) %>%
    distinct()
}

# -----------------------------------------------------------------------
# 5. Build the three price panels
# -----------------------------------------------------------------------
message("Building price panels...")

# S1 — BASE (λ=1), from Script 08
panel_S1 <- val_panel %>%
  select(ciudad, fecha, articulo, price_BASE) %>%
  make_cona_panel(price_BASE)

# S2 — S5 (JS-shrunk λ*), from Script 08
panel_S2 <- val_panel %>%
  select(ciudad, fecha, articulo, price_final) %>%
  make_cona_panel(price_final)

# S3 — Observed DANE prices (ground truth)
panel_S3 <- dane %>%
  select(ciudad = nombre_ciudad, fecha, articulo, precio_500g) %>%
  make_cona_panel(precio_500g)

message(sprintf(
  "  Panels — S1(BASE): %d | S2(S5): %d | S3(Observed): %d",
  nrow(panel_S1), nrow(panel_S2), nrow(panel_S3)))

# -----------------------------------------------------------------------
# 6. Load EER inputs
# -----------------------------------------------------------------------
message("Loading EER inputs...")

agg_eer <- read_excel(file.path(out_eer, "220326_agg_eer.xlsx"))

household_eer <- agg_eer %>%
  filter(
    (sex == "Masculino" & rango == "[31,51)") |
      (sex == "Femenino"  & rango == "[31,51)") |
      (sex == "Femenino"  & rango == "[10, 14)")
  ) %>%
  mutate(ciudad = case_when(
    cod_mun == "05001" ~ "MEDELLIN",
    cod_mun == "11001" ~ "BOGOTA",
    cod_mun == "76001" ~ "CALI",
    TRUE               ~ cod_mun)) %>%
  filter(ciudad %in% c("BOGOTA", "MEDELLIN", "CALI")) %>%
  rename(Age = rango, Sex = sex, Energy = eer) %>%
  mutate(Sex = if_else(Sex == "Masculino", 0L, 1L)) %>%
  as.data.frame()

eer_ll_base <- FoodpriceR::EER_LL %>%
  mutate(Age = recode(Age,
                      "31 a 50 años" = "[31,51)",
                      "9 a 13 años"  = "[10, 14)"))

ul_base <- FoodpriceR::UL %>%
  mutate(Age = recode(Age,
                      "31 a 50 años" = "[31,51)",
                      "9 a 13 años"  = "[10, 14)"))

household_eer_ll <- merge(household_eer,
                          eer_ll_base %>% select(-Energy),
                          by = c("Sex", "Age"))

household_ul <- merge(household_eer,
                      ul_base %>% select(-Energy),
                      by = c("Sex", "Age")) %>%
  select(-Energy) %>%
  mutate(across(where(is.numeric), ~ replace_na(.x, 9999999)))

# -----------------------------------------------------------------------
# 7. Run CoNA for each scenario × city × date
# -----------------------------------------------------------------------
run_cona <- function(panel, scenario_label,
                     household_eer_ll, household_ul) {
  
  cities <- unique(panel$ciudad)
  fechas <- sort(unique(panel$fecha))
  results <- list()
  n_ok <- 0L; n_fail <- 0L
  
  for (i in cities) {
    eer_ll.aux <- household_eer_ll %>%
      filter(ciudad == i) %>% select(-ciudad)
    ul.aux <- household_ul %>%
      filter(ciudad == i) %>% select(-ciudad)
    if (nrow(eer_ll.aux) == 0) next
    
    for (t in fechas) {
      data.aux <- panel %>%
        filter(ciudad == i, fecha == t, !is.na(precio_100g)) %>%
        filter(articulo != "ARROZ PARA SOPA") %>%
        rename(Price_100g = precio_100g, Food = articulo) %>%
        as.data.frame()
      if (nrow(data.aux) == 0) next
      
      result <- tryCatch(
        CoNA_paper(data   = data.aux,
                   EER_LL = eer_ll.aux,
                   UL     = ul.aux),
        error = function(e) NULL)
      
      if (!is.null(result) && !is.null(result$cost)) {
        results[[length(results) + 1]] <- result$cost %>%
          mutate(ciudad = i, fecha = t, scenario = scenario_label)
        n_ok <- n_ok + 1L
      } else {
        n_fail <- n_fail + 1L
      }
    }
  }
  
  message(sprintf("  %s: %d OK | %d failed",
                  scenario_label, n_ok, n_fail))
  bind_rows(results)
}

message("\nRunning CoNA...")
cona_S1 <- run_cona(panel_S1, "S1_BASE",     household_eer_ll, household_ul)
cona_S2 <- run_cona(panel_S2, "S2_S5",       household_eer_ll, household_ul)
cona_S3 <- run_cona(panel_S3, "S3_Observed", household_eer_ll, household_ul)

# -----------------------------------------------------------------------
# 8. Standardise cost column
# -----------------------------------------------------------------------
standardise <- function(df) df %>% rename(cona_cost = cost_day)

cona_S1 <- standardise(cona_S1)
cona_S2 <- standardise(cona_S2)
cona_S3 <- standardise(cona_S3)

range_check <- range(cona_S3$cona_cost, na.rm = TRUE)
message(sprintf("  S3 cost_day range: %.0f – %.0f COP/day",
                range_check[1], range_check[2]))
if (range_check[2] < 10)
  stop("cost_day looks binary — check CoNA output columns.")

member_cols <- c("Demo_Group", "Sex")
join_keys   <- c("ciudad", "fecha", member_cols)

# -----------------------------------------------------------------------
# 9. Compute deviations ΔCoNA
# -----------------------------------------------------------------------
cona_wide <- cona_S3 %>%
  rename(cona_S3 = cona_cost) %>%
  select(-scenario) %>%
  inner_join(
    cona_S1 %>% rename(cona_S1 = cona_cost) %>% select(-scenario),
    by = join_keys) %>%
  inner_join(
    cona_S2 %>% rename(cona_S2 = cona_cost) %>% select(-scenario),
    by = join_keys) %>%
  mutate(
    delta_S1     = (cona_S1 - cona_S3) / cona_S3 * 100,
    delta_S2     = (cona_S2 - cona_S3) / cona_S3 * 100,
    abs_delta_S1 = abs(delta_S1),
    abs_delta_S2 = abs(delta_S2),
    closer       = if_else(abs_delta_S2 <= abs_delta_S1,
                           "S2_S5", "S1_BASE"),
    imp_pp       = abs_delta_S1 - abs_delta_S2,
    fecha        = as.Date(fecha, origin = "1970-01-01"))

message(sprintf("  Matched observations: %d", nrow(cona_wide)))
if (nrow(cona_wide) == 0)
  stop("No matched observations — check city name normalisation.")

# -----------------------------------------------------------------------
# 10. Summaries
# -----------------------------------------------------------------------
global_summary <- cona_wide %>%
  summarise(
    n_obs               = n(),
    MADE_S1             = mean(abs_delta_S1, na.rm = TRUE),
    MADE_S2             = mean(abs_delta_S2, na.rm = TRUE),
    max_S1              = max(abs_delta_S1,  na.rm = TRUE),
    max_S2              = max(abs_delta_S2,  na.rm = TRUE),
    median_S1           = median(delta_S1,   na.rm = TRUE),
    median_S2           = median(delta_S2,   na.rm = TRUE),
    pct_S2_closer       = mean(closer == "S2_S5", na.rm=TRUE) * 100,
    pct_within_1pct_S1  = mean(abs_delta_S1 < 1,  na.rm=TRUE) * 100,
    pct_within_1pct_S2  = mean(abs_delta_S2 < 1,  na.rm=TRUE) * 100,
    pct_within_3pct_S1  = mean(abs_delta_S1 < 3,  na.rm=TRUE) * 100,
    pct_within_3pct_S2  = mean(abs_delta_S2 < 3,  na.rm=TRUE) * 100,
    improvement_mean_pp = mean(imp_pp, na.rm = TRUE))

city_summary <- cona_wide %>%
  group_by(ciudad) %>%
  summarise(
    n_obs         = n(),
    MADE_S1       = mean(abs_delta_S1, na.rm = TRUE),
    MADE_S2       = mean(abs_delta_S2, na.rm = TRUE),
    pct_S2_closer = mean(closer == "S2_S5", na.rm=TRUE) * 100,
    imp_pp        = MADE_S1 - MADE_S2,
    .groups       = "drop")

member_summary <- cona_wide %>%
  group_by(across(all_of(member_cols))) %>%
  summarise(
    n_obs         = n(),
    MADE_S1       = mean(abs_delta_S1, na.rm = TRUE),
    MADE_S2       = mean(abs_delta_S2, na.rm = TRUE),
    pct_S2_closer = mean(closer == "S2_S5", na.rm=TRUE) * 100,
    imp_pp        = MADE_S1 - MADE_S2,
    .groups       = "drop")

city_member_summary <- cona_wide %>%
  group_by(across(all_of(c("ciudad", member_cols)))) %>%
  summarise(
    n_obs   = n(),
    MADE_S1 = mean(abs_delta_S1, na.rm = TRUE),
    MADE_S2 = mean(abs_delta_S2, na.rm = TRUE),
    imp_pp  = MADE_S1 - MADE_S2,
    .groups = "drop")

# -----------------------------------------------------------------------
# 11. Figures
# -----------------------------------------------------------------------
COL_S1 <- "#95A5A6"
COL_S2 <- "#E74C3C"
COL_S3 <- "#2C3E50"

city_labels <- c("BOGOTA" = "Bogotá",
                 "MEDELLIN" = "Medellín",
                 "CALI" = "Cali")

# 11a. Distribution of ΔCoNA
delta_long <- cona_wide %>%
  select(ciudad, fecha, all_of(member_cols), delta_S1, delta_S2) %>%
  pivot_longer(cols = c(delta_S1, delta_S2),
               names_to = "scenario", values_to = "delta_pct") %>%
  mutate(
    scenario_lbl = if_else(scenario == "delta_S1",
                           "S1: BASE", "S2: S5 (D_city JS)"),
    ciudad_lbl   = city_labels[ciudad])

p_dist <- ggplot(delta_long,
                 aes(x = delta_pct, fill = scenario_lbl,
                     color = scenario_lbl)) +
  geom_histogram(bins = 50, alpha = 0.55, position = "identity") +
  geom_vline(xintercept = 0, color = "black", linewidth = 0.6) +
  geom_vline(xintercept = c(-3, 3), color = "grey50",
             linewidth = 0.5, linetype = "dashed") +
  scale_fill_manual(values  = c("S1: BASE"           = COL_S1,
                                "S2: S5 (D_city JS)" = COL_S2)) +
  scale_color_manual(values = c("S1: BASE"           = COL_S1,
                                "S2: S5 (D_city JS)" = COL_S2)) +
  facet_wrap(~ ciudad_lbl, ncol = 3) +
  labs(
    title    = "CoNA deviation from observed prices",
    subtitle = sprintf(
      "ΔCoNA = (CoNA_S − CoNA_obs) / CoNA_obs × 100\nMADE — S1(BASE): %.4f%% | S2(S5): %.4f%%",
      global_summary$MADE_S1, global_summary$MADE_S2),
    x = "ΔCoNA (%)", y = "Count", fill = NULL, color = NULL) +
  theme_bw(base_size = 11) +
  theme(legend.position = "bottom")

ggsave(file.path(robust_dir, "fig_cona_delta_distribution.png"),
       p_dist, width = 12, height = 5, dpi = 200)

# 11b. Time series — mean CoNA per city
ts_data <- cona_wide %>%
  group_by(ciudad, fecha) %>%
  summarise(across(c(cona_S1, cona_S2, cona_S3),
                   ~ mean(.x, na.rm = TRUE)),
            .groups = "drop") %>%
  mutate(ciudad_lbl = city_labels[ciudad]) %>%
  pivot_longer(cols = c(cona_S1, cona_S2, cona_S3),
               names_to = "scenario", values_to = "cona_cost") %>%
  mutate(scenario_lbl = factor(
    case_when(scenario == "cona_S1" ~ "S1: BASE",
              scenario == "cona_S2" ~ "S2: S5 (D_city JS)",
              scenario == "cona_S3" ~ "S3: Observed"),
    levels = c("S3: Observed", "S1: BASE", "S2: S5 (D_city JS)")))

p_ts <- ggplot(ts_data,
               aes(x = fecha, y = cona_cost,
                   color = scenario_lbl,
                   linetype = scenario_lbl,
                   linewidth = scenario_lbl)) +
  geom_line() +
  facet_wrap(~ ciudad_lbl, scales = "free_y", ncol = 3) +
  scale_color_manual(
    values = c("S3: Observed"       = COL_S3,
               "S1: BASE"           = COL_S1,
               "S2: S5 (D_city JS)" = COL_S2)) +
  scale_linetype_manual(
    values = c("S3: Observed"       = "solid",
               "S1: BASE"           = "dashed",
               "S2: S5 (D_city JS)" = "dashed")) +
  scale_linewidth_manual(
    values = c("S3: Observed"       = 1.1,
               "S1: BASE"           = 0.8,
               "S2: S5 (D_city JS)" = 0.8)) +
  scale_x_date(date_breaks = "6 months", date_labels = "%b\n%Y") +
  labs(title    = "CoNA — validation window Feb 2016–Mar 2018",
       subtitle = "Mean daily cost | Black=observed | Grey=BASE | Red=S5",
       x = NULL, y = "CoNA (COP/day)",
       color = NULL, linetype = NULL, linewidth = NULL) +
  theme_bw(base_size = 11) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank())

ggsave(file.path(robust_dir, "fig_cona_timeseries.png"),
       p_ts, width = 13, height = 5, dpi = 200)

# 11c. Time series by member
ts_member <- cona_wide %>%
  mutate(ciudad_lbl = city_labels[ciudad],
         member_lbl = paste0(Demo_Group, " | ",
                             if_else(Sex == 0, "Male", "Female"))) %>%
  group_by(ciudad_lbl, fecha, member_lbl) %>%
  summarise(across(c(cona_S1, cona_S2, cona_S3),
                   ~ mean(.x, na.rm = TRUE)),
            .groups = "drop") %>%
  pivot_longer(cols = c(cona_S1, cona_S2, cona_S3),
               names_to = "scenario", values_to = "cona_cost") %>%
  mutate(scenario_lbl = factor(
    case_when(scenario == "cona_S1" ~ "S1: BASE",
              scenario == "cona_S2" ~ "S2: S5 (D_city JS)",
              scenario == "cona_S3" ~ "S3: Observed"),
    levels = c("S3: Observed", "S1: BASE", "S2: S5 (D_city JS)")))

p_ts_m <- ggplot(ts_member,
                 aes(x = fecha, y = cona_cost,
                     color = scenario_lbl,
                     linetype = scenario_lbl,
                     linewidth = scenario_lbl)) +
  geom_line() +
  facet_grid(member_lbl ~ ciudad_lbl, scales = "free_y") +
  scale_color_manual(
    values = c("S3: Observed"       = COL_S3,
               "S1: BASE"           = COL_S1,
               "S2: S5 (D_city JS)" = COL_S2)) +
  scale_linetype_manual(
    values = c("S3: Observed"       = "solid",
               "S1: BASE"           = "dashed",
               "S2: S5 (D_city JS)" = "dashed")) +
  scale_linewidth_manual(
    values = c("S3: Observed"       = 1.0,
               "S1: BASE"           = 0.7,
               "S2: S5 (D_city JS)" = 0.7)) +
  scale_x_date(date_breaks = "6 months", date_labels = "%b\n%Y") +
  labs(title = "CoNA by household member — validation window",
       x = NULL, y = "CoNA (COP/day)",
       color = NULL, linetype = NULL, linewidth = NULL) +
  theme_bw(base_size = 9) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank())

ggsave(file.path(robust_dir, "fig_cona_timeseries_by_member.png"),
       p_ts_m, width = 13, height = 9, dpi = 200)

# 11d & 11e. Scatter plots
for (sc in c("S1", "S2")) {
  col  <- paste0("cona_", sc)
  made <- if (sc == "S1") global_summary$MADE_S1 else global_summary$MADE_S2
  lbl  <- if (sc == "S1") "BASE" else "S5 (D_city JS)"
  
  p_sc <- ggplot(
    cona_wide %>% mutate(ciudad_lbl = city_labels[ciudad]),
    aes(x = cona_S3, y = .data[[col]], color = ciudad_lbl)) +
    geom_point(alpha = 0.4, size = 1.5) +
    geom_abline(slope = 1, intercept = 0,
                linetype = "dashed", color = "black", linewidth = 0.7) +
    scale_color_manual(
      values = c("Bogotá"   = "#2E5FA3",
                 "Cali"     = "#1A7A4A",
                 "Medellín" = "#C0392B")) +
    labs(title    = sprintf("CoNA — %s vs Observed", lbl),
         subtitle = sprintf("MADE = %.4f%%", made),
         x = "CoNA observed (COP/day)",
         y = sprintf("CoNA %s (COP/day)", lbl),
         color = "City") +
    theme_bw(base_size = 11) +
    theme(legend.position = "bottom")
  
  ggsave(file.path(robust_dir,
                   sprintf("fig_cona_scatter_%s_vs_s3.png", tolower(sc))),
         p_sc, width = 7, height = 6, dpi = 200)
}

# -----------------------------------------------------------------------
# 12. Save outputs
# -----------------------------------------------------------------------
message("Saving outputs...")

write_csv(cona_wide,
          file.path(robust_dir, "cona_validation_cost.csv"))
write_csv(
  global_summary %>% pivot_longer(everything(),
                                  names_to = "metric", values_to = "value"),
  file.path(robust_dir, "cona_validation_global.csv"))
write_csv(city_summary,
          file.path(robust_dir, "cona_validation_city.csv"))
write_csv(member_summary,
          file.path(robust_dir, "cona_validation_member.csv"))
write_csv(city_member_summary,
          file.path(robust_dir, "cona_validation_city_member.csv"))

write_xlsx(
  list(
    cost_wide      = cona_wide,
    global         = global_summary %>%
      pivot_longer(everything(),
                   names_to = "metric", values_to = "value"),
    by_city        = city_summary,
    by_member      = member_summary,
    by_city_member = city_member_summary),
  file.path(robust_dir, "cona_validation_results.xlsx"))

message("All outputs saved.")

# -----------------------------------------------------------------------
# 13. Decision print
# -----------------------------------------------------------------------
cat("\n")
cat(strrep("=", 70), "\n")
cat("  CoNA VALIDATION\n")
cat("  S1=BASE | S2=S5 (D_city JS) | S3=Observed\n")
cat("  Prices from: prices_validation_panel.csv (Script 08)\n")
cat("  Validation window: Feb 2016 – Mar 2018\n")
cat(strrep("=", 70), "\n\n")

cat("── GLOBAL SUMMARY ──\n\n")
cat(sprintf("  Matched observations:              %d\n",
            global_summary$n_obs))
cat(sprintf("  MADE S1 (BASE vs observed):        %.4f%%\n",
            global_summary$MADE_S1))
cat(sprintf("  MADE S2 (S5 vs observed):          %.4f%%\n",
            global_summary$MADE_S2))
cat(sprintf("  Improvement S2 over S1:            %+.4f pp\n",
            global_summary$improvement_mean_pp))
cat(sprintf("  S2 closer to observed:             %.1f%% of obs\n\n",
            global_summary$pct_S2_closer))
cat(sprintf("  Within ±1%% — S1: %.1f%% | S2: %.1f%%\n",
            global_summary$pct_within_1pct_S1,
            global_summary$pct_within_1pct_S2))
cat(sprintf("  Within ±3%% — S1: %.1f%% | S2: %.1f%%\n\n",
            global_summary$pct_within_3pct_S1,
            global_summary$pct_within_3pct_S2))

cat("── BY CITY ──\n\n")
cat(sprintf("  %-12s  %8s  %8s  %10s  %10s\n",
            "City", "MADE_S1", "MADE_S2", "imp(pp)", "S2 closer"))
cat(sprintf("  %s\n", strrep("-", 56)))
for (i in seq_len(nrow(city_summary))) {
  r   <- city_summary[i, ]
  sym <- if (r$imp_pp > 0) "  ✓" else "  ✗"
  cat(sprintf("  %-12s  %8.4f  %8.4f  %+10.4f  %9.1f%%%s\n",
              r$ciudad, r$MADE_S1, r$MADE_S2,
              r$imp_pp, r$pct_S2_closer, sym))
}
cat("\n")

cat("── BY HOUSEHOLD MEMBER ──\n\n")
cat(sprintf("  %-12s  %-6s  %8s  %8s  %10s\n",
            "Demo_Group", "Sex", "MADE_S1", "MADE_S2", "imp(pp)"))
cat(sprintf("  %s\n", strrep("-", 52)))
for (i in seq_len(nrow(member_summary))) {
  r      <- member_summary[i, ]
  sx_lbl <- if_else(r$Sex == 0, "Male  ", "Female")
  sym    <- if (r$imp_pp > 0) "  ✓" else "  ✗"
  cat(sprintf("  %-12s  %-6s  %8.4f  %8.4f  %+10.4f%s\n",
              r$Demo_Group, sx_lbl,
              r$MADE_S1, r$MADE_S2, r$imp_pp, sym))
}
cat("\n")

cat("── INTERPRETATION ──\n\n")
if (global_summary$MADE_S2 < global_summary$MADE_S1) {
  cat(sprintf(
    "  S5 IMPROVES CoNA accuracy vs BASE:\n  MADE %.4f%% → %.4f%% (%+.4f pp).\n\n",
    global_summary$MADE_S1, global_summary$MADE_S2,
    global_summary$improvement_mean_pp))
} else {
  cat(sprintf(
    "  BASE and S5 produce similar CoNA accuracy.\n  MADE — BASE: %.4f%% | S5: %.4f%%\n\n",
    global_summary$MADE_S1, global_summary$MADE_S2))
}

cat(strrep("=", 70), "\n")
cat("  Outputs → ", robust_dir, "\n")
cat(strrep("=", 70), "\n\n")