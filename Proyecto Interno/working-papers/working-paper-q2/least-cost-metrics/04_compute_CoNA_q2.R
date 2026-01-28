########################################################
## 04_compute_CoNA_fullsample.R
## CoNA (cost + comp) for 3 cities, all months (panel monthly)
## Three price scenarios: q1, q2, q3
########################################################

source("working-papers/working-paper-q2/least-cost-metrics/00_config_q2.R")

suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(readr)
  library(writexl)
  library(FoodpriceR)
})

# -----------------------------
# 1) Load panel (NEW INPUT)
# -----------------------------
in_panel <- file.path(q2_fullsample_dir, "panel_city_month_food_2013_2025.rds")
if (!file.exists(in_panel)) {
  stop("No se encontró el panel: ", in_panel)
}

panel <- readRDS(in_panel)

# Nutrient rename block (FoodpriceR::CoNA expects these names)
panel2 <- panel %>%
  rename(
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
  mutate(
    ciudad = as.character(ciudad),
    fecha  = as.Date(fecha),
    alimento_sipsa = as.character(alimento_sipsa)
  )

# Chequeos mínimos
req_cols <- c(
  "ciudad","fecha","alimento_sipsa",
  "precio_100g_q1","precio_100g_q2","precio_100g_q3",
  "Energy","Protein","Lipids","Carbohydrates","VitaminC","Folate","VitaminA",
  "Thiamine","Riboflavin","Niacin","VitaminB12","Magnesium","Phosphorus",
  "Sodium","Calcium","Iron","Zinc"
)
missing_cols <- setdiff(req_cols, names(panel2))
if (length(missing_cols) > 0) {
  stop("Al panel le faltan estas columnas requeridas: ", paste(missing_cols, collapse = ", "))
}

message("Panel rows: ", nrow(panel2))
message("Cities: ", paste(sort(unique(panel2$ciudad)), collapse = ", "))
message("Date range: ", min(panel2$fecha, na.rm = TRUE), " to ", max(panel2$fecha, na.rm = TRUE))

nutr_cols <- c(
  "Energy","Protein","Lipids","Carbohydrates","VitaminC","Folate","VitaminA",
  "Thiamine","Riboflavin","Niacin","VitaminB12","Magnesium","Phosphorus",
  "Sodium","Calcium","Iron","Zinc"
)

# -----------------------------
# 2) Scenario mapping (q1/q2/q3)
# -----------------------------
scenario_map <- tibble(
  escenario = c("q1", "q2", "q3"),
  price_col = c("precio_100g_q1", "precio_100g_q2", "precio_100g_q3")
)

# -----------------------------
# 3) Compute CoNA by city x month x scenario
# -----------------------------
results_cost <- list()
results_comp <- list()
fail_cona <- list()

cities <- sort(unique(panel2$ciudad))

for (cc in cities) {
  
  message("== CoNA city: ", cc)
  
  panel_c <- panel2 %>%
    filter(ciudad == cc) %>%
    arrange(fecha)
  
  fechas <- sort(unique(panel_c$fecha))
  
  for (k in 1:length(fechas)) {
    
    f <- fechas[k]
    df_f <- panel_c %>% filter(fecha == f)
    
    for (s in seq_len(nrow(scenario_map))) {
      
      esc  <- scenario_map$escenario[s]
      pcol <- scenario_map$price_col[s]
      
      df.aux <- df_f %>%
        transmute(
          Food       = alimento_sipsa,
          Price_100g = .data[[pcol]],
          Serving    = 100,
          across(all_of(nutr_cols), ~ .x)
        ) %>%
        filter(
          !is.na(Price_100g),
          !is.na(Energy),
          Price_100g > 0,
          Energy > 0
        )
      
      if (nrow(df.aux) == 0) {
        fail_cona[[length(fail_cona) + 1]] <- tibble(
          ciudad = cc, fecha = f, escenario = esc,
          motivo = "No valid foods (missing/invalid price or energy)"
        )
        next
      }
      
      # Collapse duplicates by Food (avoids LP problems)
      df.aux <- df.aux %>%
        group_by(Food) %>%
        summarise(
          Price_100g = mean(Price_100g, na.rm = TRUE),
          Serving = 100,
          across(all_of(nutr_cols), ~ mean(.x, na.rm = TRUE)),
          .groups = "drop"
        )
      
      out <- tryCatch({
        
        cona.aux <- FoodpriceR::CoNA(data = df.aux, EER_LL = EER_LL, UL = UL)
        
        cost_df <- cona.aux$cost %>%
          mutate(ciudad = cc, fecha = f, escenario = esc)
        
        comp_df <- cona.aux$comp %>%
          mutate(ciudad = cc, fecha = f, escenario = esc)
        
        list(cost = cost_df, comp = comp_df)
        
      }, error = function(e) {
        fail_cona[[length(fail_cona) + 1]] <- tibble(
          ciudad = cc, fecha = f, escenario = esc, motivo = e$message
        )
        NULL
      })
      
      if (!is.null(out)) {
        results_cost[[length(results_cost) + 1]] <- out$cost
        results_comp[[length(results_comp) + 1]] <- out$comp
      }
    }
  }
}

cona_cost <- if (length(results_cost) == 0) tibble() else bind_rows(results_cost)
cona_comp <- if (length(results_comp) == 0) tibble() else bind_rows(results_comp)
fail_df   <- if (length(fail_cona) == 0) tibble() else bind_rows(fail_cona)

# -----------------------------
# 4) Save outputs
# -----------------------------
# (a) todo junto
saveRDS(cona_cost, file.path(q2_fullsample_dir, "CoNA_cost_city_month_q1_q2_q3.rds"))
saveRDS(cona_comp, file.path(q2_fullsample_dir, "CoNA_comp_city_month_q1_q2_q3.rds"))

write_csv(cona_cost, file.path(q2_fullsample_dir, "CoNA_cost_city_month_q1_q2_q3.csv"))
write_csv(cona_comp, file.path(q2_fullsample_dir, "CoNA_comp_city_month_q1_q2_q3.csv"))

# (b) separados por escenario
for (esc in c("q1", "q2", "q3")) {
  cost_esc <- cona_cost %>% filter(escenario == esc)
  comp_esc <- cona_comp %>% filter(escenario == esc)
  
  write_csv(cost_esc, file.path(q2_fullsample_dir, paste0("CoNA_cost_city_month_", esc, ".csv")))
  write_csv(comp_esc, file.path(q2_fullsample_dir, paste0("CoNA_comp_city_month_", esc, ".csv")))
  
  saveRDS(cost_esc, file.path(q2_fullsample_dir, paste0("CoNA_cost_city_month_", esc, ".rds")))
  saveRDS(comp_esc, file.path(q2_fullsample_dir, paste0("CoNA_comp_city_month_", esc, ".rds")))
}

# (c) excel con failures
write_xlsx(
  list(
    cona_cost = cona_cost,
    cona_comp = cona_comp,
    failures  = fail_df
  ),
  file.path(q2_fullsample_dir, "CoNA_city_month_q1_q2_q3.xlsx")
)

message("Saved CoNA outputs in: ", q2_fullsample_dir)
message("Rows in CoNA cost: ", nrow(cona_cost))
message("Rows in CoNA comp: ", nrow(cona_comp))
message("Failures: ", nrow(fail_df))
