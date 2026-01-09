############################################################
# CoCA y CoNA para m5 y m6 con 3 escenarios (hat/lwr/upr)
# - lwr/upr SOLO desde 2016-09-01 en adelante
############################################################

library(dplyr)
library(FoodpriceR)
library(tidyr)

# -----------------------
# 0) Paths
# -----------------------
base_dir <- "C:/Users/Portatil/Desktop/Least-cost-diets-and-affordability/Proyecto Interno"
setwd(base_dir)

in_dir  <- file.path(base_dir, "working-papers", "working-paper-0125", "food_tables")
out_dir <- file.path(base_dir, "working-papers", "working-paper-0125", "output")

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(out_dir, "coca"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(out_dir, "cona"), recursive = TRUE, showWarnings = FALSE)

cutoff_date <- as.Date("2016-09-01")

# -----------------------
# 1) Load inputs (m5 + m6)
# -----------------------
m5_dataset <- readRDS(file.path(in_dir, "m5_input.RDS")) %>%
  mutate(fecha = as.Date(fecha))

m6_dataset <- readRDS(file.path(in_dir, "m6_input.RDS")) %>%
  mutate(fecha = as.Date(fecha))

# -----------------------
# 2) Helper: prepare data for FoodpriceR (generic)
# -----------------------
prep_foodpricer_data <- function(df, price_var) {
  
  if (!price_var %in% names(df)) {
    stop("La variable de precio '", price_var, "' no existe en el dataset.")
  }
  
  df %>%
    rename(
      Food          = alimento_sipsa,
      Price_100g    = !!price_var,
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
      Zinc          = zinc_mg
    ) %>%
    filter(
      !is.na(Price_100g),
      !is.na(Energy)
    )
}

# -----------------------
# 3) Helper: run CoCA / CoNA per date
# -----------------------
run_coca_cona <- function(df_prepped, EER, EER_LL, UL, verbose = TRUE) {
  
  fechas <- sort(unique(df_prepped$fecha))
  
  coca_list <- vector("list", length(fechas))
  cona_cost_list <- vector("list", length(fechas))
  cona_comp_list <- vector("list", length(fechas))
  
  for (t in seq_along(fechas)) {
    if (verbose) message("CoCA: ", t, " de ", length(fechas), " | ", fechas[t])
    dat_t <- df_prepped %>% filter(fecha == fechas[t])
    coca_aux <- FoodpriceR::CoCA(data = dat_t, EER = EER)
    coca_list[[t]] <- tibble(fecha = fechas[t], coca_cost = coca_aux$cost)
  }
  
  for (t in seq_along(fechas)) {
    if (verbose) message("CoNA: ", t, " de ", length(fechas), " | ", fechas[t])
    dat_t <- df_prepped %>% filter(fecha == fechas[t])
    cona_aux <- FoodpriceR::CoNA(data = dat_t, EER_LL = EER_LL, UL = UL)
    
    cona_cost_list[[t]] <- tibble(fecha = fechas[t], cona_cost = cona_aux$cost)
    comp <- if ("solution" %in% names(cona_aux)) cona_aux$solution else NA
    cona_comp_list[[t]] <- tibble(fecha = fechas[t], cona_comp = list(comp))
  }
  
  list(
    coca_cost = bind_rows(coca_list),
    cona_cost = bind_rows(cona_cost_list),
    cona_comp = bind_rows(cona_comp_list)
  )
}

# -----------------------
# 4) Runner for one model (m5 or m6)
# -----------------------
run_model_3scenarios <- function(df, model_name, EER, EER_LL, UL, cutoff_date, verbose = TRUE) {
  
  scenarios <- tibble(
    scenario  = c("hat", "lwr", "upr"),
    price_var = c("precio_hat_100g", "precio_lwr_100g", "precio_upr_100g")
  )
  
  results <- list()
  
  for (i in seq_len(nrow(scenarios))) {
    
    scn  <- scenarios$scenario[i]
    pvar <- scenarios$price_var[i]
    
    message("\n========================")
    message("Running ", model_name, " scenario: ", scn, " | price_var = ", pvar)
    message("========================")
    
    df_use <- if (scn %in% c("lwr", "upr")) df %>% filter(fecha >= cutoff_date) else df
    
    df_prepped <- prep_foodpricer_data(df_use, price_var = pvar)
    
    if (nrow(df_prepped) == 0) {
      message("No hay observaciones válidas para ", model_name, " ", scn, " (todo NA / sin datos). Se omite.")
      next
    }
    
    res <- run_coca_cona(df_prepped, EER = EER, EER_LL = EER_LL, UL = UL, verbose = verbose)
    results[[scn]] <- res
    
    saveRDS(res$coca_cost, file.path(out_dir, "coca", paste0(model_name, "_", scn, "_coca_cost.RDS")))
    saveRDS(res$cona_cost, file.path(out_dir, "cona", paste0(model_name, "_", scn, "_cona_cost.RDS")))
    saveRDS(res$cona_comp, file.path(out_dir, "cona", paste0(model_name, "_", scn, "_cona_comp.RDS")))
  }
  
  # optional: long + wide outputs
  coca_long <- bind_rows(lapply(names(results), function(scn) results[[scn]]$coca_cost %>% mutate(scenario = scn))) %>%
    arrange(fecha, scenario)
  
  cona_long <- bind_rows(lapply(names(results), function(scn) results[[scn]]$cona_cost %>% mutate(scenario = scn))) %>%
    arrange(fecha, scenario)
  
  saveRDS(coca_long, file.path(out_dir, "coca", paste0(model_name, "_coca_cost_long_hat_lwr_upr.RDS")))
  saveRDS(cona_long, file.path(out_dir, "cona", paste0(model_name, "_cona_cost_long_hat_lwr_upr.RDS")))
  
  write.csv(coca_long, file.path(out_dir, "coca", paste0(model_name, "_coca_cost_long_hat_lwr_upr.csv")), row.names = FALSE)
  write.csv(cona_long, file.path(out_dir, "cona", paste0(model_name, "_cona_cost_long_hat_lwr_upr.csv")), row.names = FALSE)
  
  coca_wide <- coca_long %>% pivot_wider(names_from = scenario, values_from = coca_cost) %>% arrange(fecha)
  cona_wide <- cona_long %>% pivot_wider(names_from = scenario, values_from = cona_cost) %>% arrange(fecha)
  
  saveRDS(coca_wide, file.path(out_dir, "coca", paste0(model_name, "_coca_cost_wide_hat_lwr_upr.RDS")))
  saveRDS(cona_wide, file.path(out_dir, "cona", paste0(model_name, "_cona_cost_wide_hat_lwr_upr.RDS")))
  
  invisible(results)
}

# -----------------------
# 5) Run m5 and m6
# -----------------------
m5_results <- run_model_3scenarios(
  df = m5_dataset,
  model_name = "m5",
  EER = EER, EER_LL = EER_LL, UL = UL,
  cutoff_date = cutoff_date,
  verbose = TRUE
)

m6_results <- run_model_3scenarios(
  df = m6_dataset,
  model_name = "m6",
  EER = EER, EER_LL = EER_LL, UL = UL,
  cutoff_date = cutoff_date,
  verbose = TRUE
)
