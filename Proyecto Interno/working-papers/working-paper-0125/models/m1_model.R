############################################################
# CoCA y CoNA para m1 con 3 escenarios (q1 / q2 / q3)
# - Los 3 escenarios existen en TODO el horizonte (sin cutoff)
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

# -----------------------
# 1) Load input (m1)
# -----------------------
m1_dataset <- readRDS(file.path(in_dir, "m1_input.RDS")) %>%
  mutate(fecha = as.Date(fecha))

# -----------------------
# 2) Helper: prepare data for FoodpriceR (m1)
# -----------------------
prep_foodpricer_data_m1 <- function(df, price_var) {
  
  if (!price_var %in% names(df)) {
    stop("La variable de precio '", price_var, "' no existe en m1_dataset.")
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
# 4) Run m1 for each scenario (q1/q2/q3)
# -----------------------
scenarios <- tibble(
  scenario  = c("q1", "q2", "q3"),
  price_var = c("precio_q1_100g", "precio_q2_100g", "precio_q3_100g")
)

m1_results <- list()

for (i in seq_len(nrow(scenarios))) {
  
  scn  <- scenarios$scenario[i]
  pvar <- scenarios$price_var[i]
  
  message("\n========================")
  message("Running m1 scenario: ", scn, " | price_var = ", pvar)
  message("========================")
  
  df_prepped <- prep_foodpricer_data_m1(m1_dataset, price_var = pvar)
  
  if (nrow(df_prepped) == 0) {
    message("No hay observaciones válidas para escenario ", scn, " (todo NA / sin datos). Se omite.")
    next
  }
  
  res <- run_coca_cona(df_prepped, EER = EER, EER_LL = EER_LL, UL = UL, verbose = TRUE)
  m1_results[[scn]] <- res
  
  saveRDS(res$coca_cost, file.path(out_dir, "coca", paste0("m1_", scn, "_coca_cost.RDS")))
  saveRDS(res$cona_cost, file.path(out_dir, "cona", paste0("m1_", scn, "_cona_cost.RDS")))
  saveRDS(res$cona_comp, file.path(out_dir, "cona", paste0("m1_", scn, "_cona_comp.RDS")))
}

# -----------------------
# 5) Long + wide comparison tables (q1 vs q2 vs q3)
# -----------------------
m1_coca_long <- bind_rows(lapply(names(m1_results), function(scn) {
  m1_results[[scn]]$coca_cost %>% mutate(scenario = scn)
})) %>% arrange(fecha, scenario)

m1_cona_long <- bind_rows(lapply(names(m1_results), function(scn) {
  m1_results[[scn]]$cona_cost %>% mutate(scenario = scn)
})) %>% arrange(fecha, scenario)

saveRDS(m1_coca_long, file.path(out_dir, "coca", "m1_coca_cost_long_q1_q2_q3.RDS"))
saveRDS(m1_cona_long, file.path(out_dir, "cona", "m1_cona_cost_long_q1_q2_q3.RDS"))

write.csv(m1_coca_long, file.path(out_dir, "coca", "m1_coca_cost_long_q1_q2_q3.csv"), row.names = FALSE)
write.csv(m1_cona_long, file.path(out_dir, "cona", "m1_cona_cost_long_q1_q2_q3.csv"), row.names = FALSE)

m1_coca_wide <- m1_coca_long %>%
  pivot_wider(names_from = scenario, values_from = coca_cost) %>%
  arrange(fecha)

m1_cona_wide <- m1_cona_long %>%
  pivot_wider(names_from = scenario, values_from = cona_cost) %>%
  arrange(fecha)

saveRDS(m1_coca_wide, file.path(out_dir, "coca", "m1_coca_cost_wide_q1_q2_q3.RDS"))
saveRDS(m1_cona_wide, file.path(out_dir, "cona", "m1_cona_cost_wide_q1_q2_q3.RDS"))
