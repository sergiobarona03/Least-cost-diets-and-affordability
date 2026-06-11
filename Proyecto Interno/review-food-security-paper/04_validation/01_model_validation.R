########################################################
## SCRIPT 04_validation/01_model_validation.R
## Model validation — pseudo out-of-sample
## Feb 2016 – Mar 2018
##
## Three price scenarios:
##   S1 — BASE     : anchor × (IPC_t/IPC_T0)^1
##   S2 — S5       : anchor × (IPC_t/IPC_T0)^λ*
##   S3 — Observed : DANE raw prices (ground truth)
##
## Models validated:
##   CoCA, CoNA, CoRD, CC-CoNA (all alphas)
##
## Metric: MADE = mean |ΔCost / Cost_S3| × 100
##
## Reads:  FORECAST_DIR/prices_validation.csv
##         CACHE_DIR/dane.rds
##         PREP_DIR/tcac_master.rds (nutrients + edible portion)
##         PREP_DIR/gaba_exchanges_adj.rds
##         PREP_DIR/household_eer.rds
##         PREP_DIR/household_eer_ll.rds
##         PREP_DIR/household_ul.rds
##         BASE_DIR/.../cpi-weights-2024.xlsx
##
## Writes: VALID_DIR/validation_cost_all.csv
##         VALID_DIR/validation_summary_global.csv
##         VALID_DIR/validation_summary_model.csv
##         VALID_DIR/validation_summary_city.csv
##         VALID_DIR/validation_results.xlsx
##         VALID_DIR/fig_made_by_model.png
##         VALID_DIR/fig_made_timeseries.png
########################################################

source("00_config.R")
library(tidyverse)
library(readxl)
library(writexl)
library(lubridate)
library(FoodpriceR)

source(file.path(IN_AUX_DIR, "CoCA_paper.R"))
source(file.path(IN_AUX_DIR, "CoNA_paper.R"))
source(file.path(IN_AUX_DIR, "CoRD_Herforth.R"))
source(file.path(IN_AUX_DIR, "CoNA_IPC_paper.R"))

# -----------------------------------------------------------------------
# 1. Load price panels
# -----------------------------------------------------------------------
message("Loading price panels...")

prices_val <- read_csv(
  file.path(FORECAST_DIR, "prices_validation.csv"),
  show_col_types = FALSE) %>%
  mutate(fecha = as.Date(fecha)) %>%
  filter(fecha >= VAL_START, fecha <= VAL_END)

dane <- readRDS(file.path(CACHE_DIR, "dane.rds")) %>%
  filter(fecha >= VAL_START, fecha <= VAL_END,
         !is.na(precio_500g))

message(sprintf("  prices_val: %d rows | dane obs: %d rows",
                nrow(prices_val), nrow(dane)))

# -----------------------------------------------------------------------
# 2. Load nutritional composition from TCAC master
#    (same source as 01_data_preparation/02_food_table.R)
# -----------------------------------------------------------------------
message("Loading nutritional composition from tcac_master...")

tcac_master <- readRDS(file.path(PREP_DIR, "tcac_master.rds"))

nutrients <- tcac_master %>%
  select(articulo,
         gramos_g_1_intercambio_1_intercambio,
         grupos_gabas, subgrupos_gabas,
         energia_kcal, proteina_g, lipidos_g,
         carbohidratos_totales_g,
         vitamina_c_mg, folatos_mcg, vitamina_a_er,
         tiamina_mg, riboflavina_mg, niacina_mg,
         vitamina_b12_mcg, magnesio_mg, fosforo_mg,
         sodio_mg, calcio_mg, hierro_mg, zinc_mg) %>%
  distinct(articulo, .keep_all = TRUE)

pc_table <- tcac_master %>%
  select(articulo, parte_comestible_percent) %>%
  distinct(articulo, .keep_all = TRUE) %>%
  mutate(pc = case_when(
    is.na(parte_comestible_percent)  ~ NA_real_,
    parte_comestible_percent > 1     ~ parte_comestible_percent / 100,
    TRUE                             ~ parte_comestible_percent))

# subclase_ipc comes from prices_validation — extract lookup table
subclase_lookup <- prices_val %>%
  select(articulo, subclase_ipc) %>%
  distinct(articulo, .keep_all = TRUE)

# -----------------------------------------------------------------------
# 3. Load EER inputs
# -----------------------------------------------------------------------
message("Loading EER inputs...")

household_eer    <- readRDS(file.path(PREP_DIR, "household_eer.rds"))
household_eer_ll <- readRDS(file.path(PREP_DIR, "household_eer_ll.rds"))
household_ul     <- readRDS(file.path(PREP_DIR, "household_ul.rds"))

# -----------------------------------------------------------------------
# 4. Load GABA adjusted exchanges (for CoRD)
# -----------------------------------------------------------------------
serv_adj <- readRDS(file.path(PREP_DIR, "gaba_exchanges_adj.rds")) %>%
  filter(
    (sex == "Masculino" & rango == "[31,51)") |
      (sex == "Femenino"  & rango == "[31,51)") |
      (sex == "Femenino"  & rango == "[10, 14)")
  ) %>%
  mutate(
    Group = recode(grupo_principal,
                   "AZÚCARES"                                              = "Azúcares",
                   "CARNES, HUEVOS, LEGUMINOSAS, FRUTOS SECOS Y SEMILLAS"  = "Carnes, huevos, leguminosas, frutos secos y semillas",
                   "CEREALES, RAÍCES, TUBÉRCULOS Y PLÁTANOS"               = "Cereales, raíces, tubérculos y plátanos",
                   "FRUTAS"    = "Frutas",
                   "GRASAS"    = "Grasas",
                   "LECHE Y PRODUCTOS LÁCTEOS" = "Leche y productos lácteos",
                   "VERDURAS"  = "Verduras"),
    Age     = rango,
    Sex     = if_else(sex == "Masculino", 0L, 1L),
    Serving = n_exchanges_adj
  ) %>%
  select(cod_mun, ciudad, Age, Sex, Group, Serving)

GROUP_CANON <- c(
  "Cereales, raíces, tubérculos y plátanos", "Frutas", "Verduras",
  "Leche y productos lácteos",
  "Carnes, huevos, leguminosas, frutos secos y semillas",
  "Grasas", "Azúcares")

div_cord <- tribble(
  ~Group,                                                        ~Number,
  "Cereales, raíces, tubérculos y plátanos",                      3,
  "Frutas",                                                       2,
  "Verduras",                                                     2,
  "Leche y productos lácteos",                                    1,
  "Carnes, huevos, leguminosas, frutos secos y semillas",         2,
  "Grasas",                                                       1,
  "Azúcares",                                                     1)

# -----------------------------------------------------------------------
# 5. Load IPC shares (for CC-CoNA)
# -----------------------------------------------------------------------
in_weights <- file.path(BASE_DIR,
                        "food-security-paper", "input", "cpi-weights", "cpi-weights-2024.xlsx")

weights_24 <- read_excel(in_weights) %>%
  janitor::clean_names() %>%
  select(nivel, codigo, nombre, total_ingresos)

alphas_ccona <- c(0, 0.25, 0.50, 0.75, 1.00)

# -----------------------------------------------------------------------
# 6. Helper: build food panel from price column
# -----------------------------------------------------------------------
normalise_city <- function(df, col = "ciudad") {
  df %>% mutate(!!col := case_when(
    .data[[col]] == "BOGOTÁ D.C." ~ "BOGOTA",
    .data[[col]] == "MEDELLÍN"    ~ "MEDELLIN",
    TRUE                          ~ .data[[col]]))
}

make_panel <- function(price_df, price_col) {
  # price_col passed as string e.g. "price_BASE"
  # Use base R rename to avoid tidy eval issues
  names(price_df)[names(price_df) == price_col] <- "precio_500g"
  price_df %>%
    left_join(pc_table,  by = "articulo") %>%
    left_join(nutrients, by = "articulo") %>%
    mutate(
      precio_100g = (precio_500g / 5) / pc,
      clase_ipc   = paste0(substr(subclase_ipc, 1, 4), "0000"),
      Group_ipc   = clase_ipc,
      # CoRD groups
      Group_cord  = if_else(subgrupos_gabas == "FRUTAS",   "FRUTAS",   grupos_gabas),
      Group_cord  = if_else(subgrupos_gabas == "VERDURAS", "VERDURAS", Group_cord),
      Group_cord  = recode(Group_cord,
                           "AZUCARES"                                                   = "Azúcares",
                           "CARNES, HUEVOS, LEGUMINOSAS SECAS, FRUTOS SECOS Y SEMILLAS" = "Carnes, huevos, leguminosas, frutos secos y semillas",
                           "CEREALES, RAÍCES, TUBÉRCULOS Y PLÁTANOS"                    = "Cereales, raíces, tubérculos y plátanos",
                           "FRUTAS"   = "Frutas",
                           "GRASAS"   = "Grasas",
                           "LECHE Y PRODUCTOS LACTEOS" = "Leche y productos lácteos",
                           "VERDURAS" = "Verduras"),
      Price_serving = precio_100g * gramos_g_1_intercambio_1_intercambio / 100,
      Serving_g     = gramos_g_1_intercambio_1_intercambio
    ) %>%
    filter(!is.na(precio_100g), precio_100g > 0,
           !is.na(energia_kcal)) %>%
    normalise_city("ciudad")
}

# Build the three panels
message("Building price panels for three scenarios...")

panel_S1 <- prices_val %>%
  select(ciudad, fecha, articulo, subclase_ipc,
         price_BASE) %>%
  make_panel("price_BASE")

panel_S2 <- prices_val %>%
  select(ciudad, fecha, articulo, subclase_ipc,
         price_S5) %>%
  make_panel("price_S5")

panel_S3 <- dane %>%
  select(ciudad = nombre_ciudad, fecha, articulo,
         precio_500g) %>%
  left_join(subclase_lookup, by = "articulo") %>%
  make_panel("precio_500g")

scenarios <- list(S1_BASE = panel_S1,
                  S2_S5   = panel_S2,
                  S3_Obs  = panel_S3)

message(sprintf("  S1: %d | S2: %d | S3: %d rows",
                nrow(panel_S1), nrow(panel_S2), nrow(panel_S3)))

# -----------------------------------------------------------------------
# 7. Run models for each scenario
# -----------------------------------------------------------------------
cities <- sort(unique(panel_S3$ciudad))
fechas <- sort(unique(panel_S3$fecha))

# Helper: normalise EER column names (handles both old and new column names)
normalise_eer <- function(df) {
  if ("rango"  %in% names(df)) names(df)[names(df) == "rango"]  <- "Age"
  if ("sex"    %in% names(df)) names(df)[names(df) == "sex"]    <- "Sex"
  if ("eer"    %in% names(df)) names(df)[names(df) == "eer"]    <- "Energy"
  if ("Sex"    %in% names(df) && is.character(df$Sex))
    df$Sex <- if_else(df$Sex == "Masculino", 0L, 1L)
  df
}

run_all_models <- function(panel, scenario_label,
                           household_eer, household_eer_ll,
                           household_ul, serv_adj,
                           ipc_shares, alphas_ccona) {
  
  results     <- list()
  spe_results <- list()
  n_ok <- 0L; n_fail <- 0L
  
  for (i in cities) {
    
    # EER inputs for this city
    eer.aux <- household_eer %>%
      filter(ciudad == i) %>%
      select(-ciudad, -any_of("cod_mun")) %>%
      as.data.frame() %>%
      normalise_eer()
    
    eer_ll.aux <- household_eer_ll %>%
      filter(ciudad == i) %>%
      select(-ciudad, -any_of("cod_mun")) %>%
      as.data.frame() %>%
      normalise_eer()
    
    ul.aux <- household_ul %>%
      filter(ciudad == i) %>%
      select(-ciudad, -any_of("cod_mun")) %>%
      as.data.frame() %>%
      normalise_eer()
    
    serv.aux <- serv_adj %>%
      filter(ciudad == i) %>%
      select(Age, Sex, Group, Serving)
    
    # IPC shares
    clases_v   <- unique(panel$clase_ipc)
    ipc_sh_aux <- ipc_shares %>%
      filter(codigo %in% clases_v) %>%
      mutate(share = total_ingresos / sum(total_ingresos),
             Group = codigo) %>%
      select(Group, nombre, share) %>%
      as.data.frame()
    
    for (t in fechas) {
      
      data_t <- panel %>%
        filter(ciudad == i, fecha == t,
               !is.na(precio_100g)) %>%
        filter(articulo != "ARROZ PARA SOPA")
      
      if (nrow(data_t) == 0) next
      
      # ── CoCA ──────────────────────────────────────────
      data_coca <- data_t %>%
        filter(grupos_gabas ==
                 "CEREALES, RAÍCES, TUBÉRCULOS Y PLÁTANOS") %>%
        as.data.frame()
      names(data_coca)[names(data_coca) == "articulo"]    <- "Food"
      names(data_coca)[names(data_coca) == "precio_100g"] <- "Price_100g"
      names(data_coca)[names(data_coca) == "energia_kcal"]<- "Energy" 
      
      res_coca <- tryCatch(
        CoCA_paper(data = data_coca, EER = eer.aux),
        error = function(e) NULL)
      
      if (!is.null(res_coca$cost)) {
        results[[length(results) + 1]] <- res_coca$cost %>%
          mutate(model = "CoCA", ciudad = i,
                 fecha = t, scenario = scenario_label,
                 alpha_val = NA_real_)
        n_ok <- n_ok + 1L
      }
      
      # ── CoNA ──────────────────────────────────────────
      data_cona <- data_t %>% as.data.frame()
      names(data_cona)[names(data_cona) == "articulo"]               <- "Food"
      names(data_cona)[names(data_cona) == "precio_100g"]            <- "Price_100g"
      names(data_cona)[names(data_cona) == "energia_kcal"]           <- "Energy"
      names(data_cona)[names(data_cona) == "proteina_g"]             <- "Protein"
      names(data_cona)[names(data_cona) == "lipidos_g"]              <- "Lipids"
      names(data_cona)[names(data_cona) == "carbohidratos_totales_g"]<- "Carbohydrates"
      names(data_cona)[names(data_cona) == "vitamina_c_mg"]          <- "VitaminC"
      names(data_cona)[names(data_cona) == "folatos_mcg"]            <- "Folate"
      names(data_cona)[names(data_cona) == "vitamina_a_er"]          <- "VitaminA"
      names(data_cona)[names(data_cona) == "tiamina_mg"]             <- "Thiamine"
      names(data_cona)[names(data_cona) == "riboflavina_mg"]         <- "Riboflavin"
      names(data_cona)[names(data_cona) == "niacina_mg"]             <- "Niacin"
      names(data_cona)[names(data_cona) == "vitamina_b12_mcg"]       <- "VitaminB12"
      names(data_cona)[names(data_cona) == "magnesio_mg"]            <- "Magnesium"
      names(data_cona)[names(data_cona) == "fosforo_mg"]             <- "Phosphorus"
      names(data_cona)[names(data_cona) == "sodio_mg"]               <- "Sodium"
      names(data_cona)[names(data_cona) == "calcio_mg"]              <- "Calcium"
      names(data_cona)[names(data_cona) == "hierro_mg"]              <- "Iron"
      names(data_cona)[names(data_cona) == "zinc_mg"]                <- "Zinc" 
      
      res_cona <- tryCatch(
        CoNA_paper(data   = data_cona,
                   EER_LL = eer_ll.aux,
                   UL     = ul.aux),
        error = function(e) NULL)
      
      if (!is.null(res_cona$cost)) {
        results[[length(results) + 1]] <- res_cona$cost %>%
          mutate(model = "CoNA", ciudad = i,
                 fecha = t, scenario = scenario_label,
                 alpha_val = NA_real_)
        # store spe separately for shadow price validation
        if (!is.null(res_cona$spe))
          spe_results[[length(spe_results) + 1]] <- res_cona$spe %>%
            mutate(model = "CoNA", ciudad = i,
                   fecha = t, scenario = scenario_label)
        n_ok <- n_ok + 1L
      }
      
      # ── CoRD ──────────────────────────────────────────
      data_cord <- data_t %>%
        filter(Group_cord %in% GROUP_CANON) %>%
        as.data.frame()
      names(data_cord)[names(data_cord) == "articulo"]   <- "Food"
      names(data_cord)[names(data_cord) == "Group_cord"] <- "Group" 
      
      res_cord <- tryCatch(
        CoRD_Herforth(data    = data_cord,
                      serv    = serv.aux,
                      diverse = div_cord),
        error = function(e) NULL)
      
      if (!is.null(res_cord$cost)) {
        results[[length(results) + 1]] <- res_cord$cost %>%
          mutate(model = "CoRD", ciudad = i,
                 fecha = t, scenario = scenario_label,
                 alpha_val = NA_real_)
        n_ok <- n_ok + 1L
      }
      
      # ── CC-CoNA (all alphas) ───────────────────────────
      data_ccona <- data_cona %>%
        mutate(Group = data_t$Group_ipc)
      
      for (alp in alphas_ccona) {
        res_cc <- tryCatch(
          CoNA_IPC_paper(
            data       = data_ccona,
            EER_LL     = eer_ll.aux,
            UL         = ul.aux,
            IPC_shares = ipc_sh_aux,
            alpha      = alp),
          error = function(e) NULL)
        
        if (!is.null(res_cc$cost)) {
          results[[length(results) + 1]] <- res_cc$cost %>%
            mutate(model = "CC-CoNA", ciudad = i,
                   fecha = t, scenario = scenario_label,
                   alpha_val = alp)
          n_ok <- n_ok + 1L
        }
      }
      
    } # end fechas
  } # end cities
  
  message(sprintf("  %s: %d OK | %d failed",
                  scenario_label, n_ok, n_fail))
  list(
    cost = bind_rows(results),
    spe  = bind_rows(spe_results)
  )
}

# -----------------------------------------------------------------------
# Run all scenarios
# -----------------------------------------------------------------------
message("\nRunning models — S1 (BASE)...")
res_S1 <- run_all_models(panel_S1, "S1_BASE",
                         household_eer, household_eer_ll,
                         household_ul, serv_adj,
                         weights_24, alphas_ccona)

message("\nRunning models — S2 (S5)...")
res_S2 <- run_all_models(panel_S2, "S2_S5",
                         household_eer, household_eer_ll,
                         household_ul, serv_adj,
                         weights_24, alphas_ccona)

message("\nRunning models — S3 (Observed)...")
res_S3 <- run_all_models(panel_S3, "S3_Obs",
                         household_eer, household_eer_ll,
                         household_ul, serv_adj,
                         weights_24, alphas_ccona)

# -----------------------------------------------------------------------
# 8. Compute MADE
# -----------------------------------------------------------------------
message("\nComputing MADE...")

# Standardise cost column name
standardise_cost <- function(df) {
  cost_col <- intersect(names(df),
                        c("cost_day", "Cost", "cost", "CoNA_cost",
                          "total_cost", "diet_cost"))
  if (length(cost_col) == 1)
    names(df)[names(df) == cost_col] <- "cost"
  df
}

# res_S1/S2/S3 are now lists with $cost and $spe
res_all <- bind_rows(
  standardise_cost(res_S1$cost),
  standardise_cost(res_S2$cost),
  standardise_cost(res_S3$cost)) %>%
  mutate(fecha = as.Date(fecha))

# Shadow prices from S3 (observed prices — ground truth)
spe_obs <- bind_rows(
  res_S1$spe %>% mutate(scenario = "S1_BASE"),
  res_S2$spe %>% mutate(scenario = "S2_S5"),
  res_S3$spe %>% mutate(scenario = "S3_Obs")) %>%
  mutate(fecha = as.Date(fecha))

# Identify cost column after standardise
if (!"cost" %in% names(res_all)) {
  message("  Available columns: ", paste(names(res_all), collapse=", "))
  stop("Could not find cost column — check CoNA/CoCA/CoRD output names.")
}

join_keys <- c("ciudad", "fecha", "model", "alpha_val",
               "Demo_Group", "Sex")

# Pivot wide: one row per city × date × model × member
cost_wide <- res_all %>%
  filter(scenario == "S3_Obs") %>%
  select(all_of(join_keys), cost_S3 = cost) %>%
  inner_join(
    res_all %>% filter(scenario == "S1_BASE") %>%
      select(all_of(join_keys), cost_S1 = cost),
    by = join_keys) %>%
  inner_join(
    res_all %>% filter(scenario == "S2_S5") %>%
      select(all_of(join_keys), cost_S2 = cost),
    by = join_keys) %>%
  mutate(
    delta_S1     = (cost_S1 - cost_S3) / cost_S3 * 100,
    delta_S2     = (cost_S2 - cost_S3) / cost_S3 * 100,
    abs_delta_S1 = abs(delta_S1),
    abs_delta_S2 = abs(delta_S2),
    closer       = if_else(abs_delta_S2 <= abs_delta_S1,
                           "S2_S5", "S1_BASE"),
    imp_pp       = abs_delta_S1 - abs_delta_S2)

message(sprintf("  Matched observations: %d", nrow(cost_wide)))

# -----------------------------------------------------------------------
# 9. Summaries
# -----------------------------------------------------------------------
global_summary <- cost_wide %>%
  dplyr::summarise(
    n_obs          = n(),
    MADE_S1        = mean(abs_delta_S1, na.rm=TRUE),
    MADE_S2        = mean(abs_delta_S2, na.rm=TRUE),
    max_S1         = max(abs_delta_S1,  na.rm=TRUE),
    max_S2         = max(abs_delta_S2,  na.rm=TRUE),
    pct_within_1_S1= mean(abs_delta_S1 < 1, na.rm=TRUE) * 100,
    pct_within_1_S2= mean(abs_delta_S2 < 1, na.rm=TRUE) * 100,
    pct_within_3_S1= mean(abs_delta_S1 < 3, na.rm=TRUE) * 100,
    pct_within_3_S2= mean(abs_delta_S2 < 3, na.rm=TRUE) * 100,
    pct_S2_closer  = mean(closer == "S2_S5", na.rm=TRUE) * 100,
    imp_mean_pp    = mean(imp_pp, na.rm=TRUE))

model_summary <- cost_wide %>%
  group_by(model, alpha_val) %>%
  dplyr::summarise(
    n_obs         = n(),
    MADE_S1       = mean(abs_delta_S1, na.rm=TRUE),
    MADE_S2       = mean(abs_delta_S2, na.rm=TRUE),
    pct_within_3_S1 = mean(abs_delta_S1 < 3, na.rm=TRUE) * 100,
    pct_within_3_S2 = mean(abs_delta_S2 < 3, na.rm=TRUE) * 100,
    pct_S2_closer = mean(closer == "S2_S5", na.rm=TRUE) * 100,
    imp_pp        = MADE_S1 - MADE_S2,
    .groups       = "drop") %>%
  arrange(model, alpha_val)

city_summary <- cost_wide %>%
  group_by(model, ciudad, alpha_val) %>%
  dplyr::summarise(
    n_obs   = n(),
    MADE_S1 = mean(abs_delta_S1, na.rm=TRUE),
    MADE_S2 = mean(abs_delta_S2, na.rm=TRUE),
    imp_pp  = MADE_S1 - MADE_S2,
    .groups = "drop") %>%
  arrange(model, ciudad, alpha_val)

# Print summary
cat("\n", strrep("=", 65), "\n")
cat("  MODEL VALIDATION SUMMARY\n")
cat("  S1=BASE | S2=S5 | S3=Observed\n")
cat("  Window: Feb 2016 – Mar 2018\n")
cat(strrep("=", 65), "\n\n")
cat(sprintf("  n obs (all models):    %d\n", global_summary$n_obs))
cat(sprintf("  MADE S1 (BASE):        %.4f%%\n", global_summary$MADE_S1))
cat(sprintf("  MADE S2 (S5):          %.4f%%\n", global_summary$MADE_S2))
cat(sprintf("  Improvement:           %+.4f pp\n", global_summary$imp_mean_pp))
cat(sprintf("  Within ±1%% — S1: %.1f%% | S2: %.1f%%\n",
            global_summary$pct_within_1_S1,
            global_summary$pct_within_1_S2))
cat(sprintf("  Within ±3%% — S1: %.1f%% | S2: %.1f%%\n\n",
            global_summary$pct_within_3_S1,
            global_summary$pct_within_3_S2))

cat("  BY MODEL:\n\n")
cat(sprintf("  %-10s  %6s  %8s  %8s  %10s\n",
            "Model", "Alpha", "MADE_S1", "MADE_S2", "imp(pp)"))
cat(sprintf("  %s\n", strrep("-", 50)))
for (i in seq_len(nrow(model_summary))) {
  r <- model_summary[i, ]
  alp_lbl <- if (is.na(r$alpha_val)) "  —   " else
    sprintf("%.2f  ", r$alpha_val)
  cat(sprintf("  %-10s  %6s  %8.4f  %8.4f  %+10.4f\n",
              r$model, alp_lbl, r$MADE_S1, r$MADE_S2, r$imp_pp))
}
cat("\n")

# -----------------------------------------------------------------------
# 10. Figures
# -----------------------------------------------------------------------
model_fig <- model_summary %>%
  mutate(
    model_lbl = if_else(
      model == "CC-CoNA",
      paste0("CC-CoNA\nα=", alpha_val),
      model),
    model_lbl = fct_reorder(model_lbl, MADE_S1)) %>%
  pivot_longer(c(MADE_S1, MADE_S2),
               names_to = "scenario",
               values_to = "MADE") %>%
  mutate(scenario_lbl = if_else(scenario == "MADE_S1",
                                "BASE", "S5"))

p_made <- ggplot(model_fig,
                 aes(x = MADE, y = model_lbl,
                     color = scenario_lbl,
                     shape = scenario_lbl)) +
  geom_point(size = 3.5, alpha = 0.9) +
  geom_line(aes(group = model_lbl),
            color = "grey70", linewidth = 0.7) +
  scale_color_manual(
    values = c("BASE" = COL_BASE, "S5" = COL_S5)) +
  scale_shape_manual(
    values = c("BASE" = 16, "S5" = 15)) +
  geom_vline(xintercept = c(1, 3, 5),
             linetype = "dotted",
             color = "grey60", linewidth = 0.4) +
  labs(
    title    = "Model validation — MADE by model and scenario",
    subtitle = sprintf(
      "Validation window: Feb 2016–Mar 2018\nGlobal MADE — BASE: %.4f%% | S5: %.4f%%",
      global_summary$MADE_S1, global_summary$MADE_S2),
    x = "MADE (%)", y = NULL,
    color = "Scenario", shape = "Scenario") +
  theme_bw(base_size = 11) +
  theme(legend.position = "bottom",
        panel.grid.major.y = element_blank())

ggsave(file.path(VALID_DIR, "fig_made_by_model.png"),
       p_made, width = 9, height = 7, dpi = 200)

# Time series of MADE by model
ts_made <- cost_wide %>%
  filter(is.na(alpha_val) | alpha_val == 0.50) %>%
  mutate(
    model_lbl = if_else(
      model == "CC-CoNA",
      "CC-CoNA (α=0.50)", model)) %>%
  group_by(model_lbl, fecha) %>%
  dplyr::summarise(
    MADE_S1 = mean(abs_delta_S1, na.rm=TRUE),
    MADE_S2 = mean(abs_delta_S2, na.rm=TRUE),
    .groups = "drop") %>%
  pivot_longer(c(MADE_S1, MADE_S2),
               names_to = "scenario",
               values_to = "MADE") %>%
  mutate(scenario_lbl = if_else(scenario == "MADE_S1",
                                "BASE", "S5"))

p_ts <- ggplot(ts_made,
               aes(x = fecha, y = MADE,
                   color = scenario_lbl,
                   linetype = scenario_lbl)) +
  geom_line(linewidth = 0.8) +
  facet_wrap(~ model_lbl, ncol = 2, scales = "free_y") +
  scale_color_manual(
    values = c("BASE" = COL_BASE, "S5" = COL_S5)) +
  scale_linetype_manual(
    values = c("BASE" = "dashed", "S5" = "solid")) +
  scale_x_date(date_breaks = "6 months",
               date_labels = "%b\n%Y") +
  labs(
    title    = "MADE over time by model",
    subtitle = "Feb 2016–Mar 2018 | CC-CoNA shown at α=0.50",
    x = NULL, y = "MADE (%)",
    color = NULL, linetype = NULL) +
  theme_bw(base_size = 11) +
  theme(legend.position    = "bottom",
        panel.grid.minor   = element_blank(),
        strip.text         = element_text(face = "bold"))

ggsave(file.path(VALID_DIR, "fig_made_timeseries.png"),
       p_ts, width = 11, height = 8, dpi = 200)

# -----------------------------------------------------------------------
# 11. Save outputs
# -----------------------------------------------------------------------
message("Saving outputs...")

write_csv(cost_wide,
          file.path(VALID_DIR, "validation_cost_all.csv"))
write_csv(
  global_summary %>%
    pivot_longer(everything(),
                 names_to = "metric", values_to = "value"),
  file.path(VALID_DIR, "validation_summary_global.csv"))
write_csv(model_summary,
          file.path(VALID_DIR, "validation_summary_model.csv"))
write_csv(city_summary,
          file.path(VALID_DIR, "validation_summary_city.csv"))

write_xlsx(
  list(
    cost_wide    = cost_wide,
    global       = global_summary %>%
      pivot_longer(everything(),
                   names_to = "metric", values_to = "value"),
    by_model     = model_summary,
    by_city      = city_summary,
    shadow_prices= spe_obs),
  file.path(VALID_DIR, "validation_results.xlsx"))

# Save shadow prices separately
write_csv(spe_obs,
          file.path(VALID_DIR, "validation_shadow_prices.csv"))

message(sprintf("Outputs saved to: %s", VALID_DIR))
message("Done.")