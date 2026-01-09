############################################################
# Estimación de métricas CoCA y CoNA (baseline + m0)
# - baseline: Price_100g = precio_100g
# - m0:       Price_100g = precio_100g_hat
############################################################

library(dplyr)
library(readr)
library(janitor)
library(stringi)
library(FoodpriceR)

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

# -----------------------
# 1) Load inputs (ONLY baseline + m0)
# -----------------------
baseline_dataset <- readRDS(file.path(in_dir, "baseline_input.RDS"))
m0_dataset       <- readRDS(file.path(in_dir, "m0_input.RDS"))

# -----------------------
# 2) Helper: prepare data for FoodpriceR
# -----------------------
prep_foodpricer_data <- function(df,
                                 price_var = "precio_100g") {
  
  if (!price_var %in% names(df)) {
    stop("La variable de precio '", price_var, "' no existe en el dataset.")
  }
  
  df  %>%
    rename(
      Food          = articulo,
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
# 3) Helper: run CoCA / CoNA per date and return tidy outputs
# -----------------------
run_coca_cona <- function(df_prepped, EER, EER_LL, UL, verbose = TRUE) {
  
  fechas <- sort(unique(df_prepped$fecha))
  
  coca_list <- vector("list", length(fechas))
  cona_cost_list <- vector("list", length(fechas))
  cona_comp_list <- vector("list", length(fechas))
  
  names(coca_list)      <- as.character(fechas)
  names(cona_cost_list) <- as.character(fechas)
  names(cona_comp_list) <- as.character(fechas)
  
  # ---- CoCA
  for (t in seq_along(fechas)) {
    if (verbose) message("CoCA: ", t, " de ", length(fechas), " | ", fechas[t])
    
    dat_t <- df_prepped %>% filter(fecha == fechas[t])
    coca_aux <- FoodpriceR::CoCA(data = dat_t, EER = EER)
    
    coca_list[[t]] <- tibble(
      fecha = fechas[t],
      coca_cost = coca_aux$cost
    )
  }
  
  # ---- CoNA
  for (t in seq_along(fechas)) {
    if (verbose) message("CoNA: ", t, " de ", length(fechas), " | ", fechas[t])
    
    dat_t <- df_prepped %>% filter(fecha == fechas[t])
    cona_aux <- FoodpriceR::CoNA(data = dat_t, EER_LL = EER_LL, UL = UL)
    
    cona_cost_list[[t]] <- tibble(
      fecha = fechas[t],
      cona_cost = cona_aux$cost
    )
    
    # composición: si CoNA devuelve solution, la guardamos; si no, guardamos NA
    comp <- if ("solution" %in% names(cona_aux)) cona_aux$solution else NA
    
    cona_comp_list[[t]] <- tibble(
      fecha = fechas[t],
      cona_comp = list(comp)
    )
  }
  
  list(
    coca_cost = bind_rows(coca_list),
    cona_cost = bind_rows(cona_cost_list),
    cona_comp = bind_rows(cona_comp_list)
  )
}

# -----------------------
# 4) Run: baseline
# -----------------------
baseline_prepped <- prep_foodpricer_data(
  df = baseline_dataset %>% 
    filter(ciudad == "76") %>%
    mutate(
      fecha = as.Date(sprintf("%04d-%02d-01", ano, mes_num))
    ),
  price_var = "precio_100g"
)

baseline_res <- run_coca_cona(
  df_prepped = baseline_prepped,
  EER = EER,
  EER_LL = EER_LL,
  UL = UL,
  verbose = TRUE
)

saveRDS(baseline_res$coca_cost, file.path(out_dir, "coca", "baseline_coca_cost.RDS"))
saveRDS(baseline_res$cona_cost, file.path(out_dir, "cona", "baseline_cona_cost.RDS"))
saveRDS(baseline_res$cona_comp, file.path(out_dir, "cona", "baseline_cona_comp.RDS"))

# -----------------------
# 5) Run: m0 (precio_100g_hat)
# -----------------------
m0_prepped <- prep_foodpricer_data(
  df = m0_dataset,
  price_var = "precio_100g_hat"
)

m0_res <- run_coca_cona(
  df_prepped = m0_prepped,
  EER = EER,
  EER_LL = EER_LL,
  UL = UL,
  verbose = TRUE
)

saveRDS(m0_res$coca_cost, file.path(out_dir, "coca", "m0_coca_cost.RDS"))
saveRDS(m0_res$cona_cost, file.path(out_dir, "cona", "m0_cona_cost.RDS"))
saveRDS(m0_res$cona_comp, file.path(out_dir, "cona", "m0_cona_comp.RDS"))

