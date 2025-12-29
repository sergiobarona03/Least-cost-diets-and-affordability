########################################################
## 04_compute_CoNA_fullsample.R
## CoNA (cost + comp) for 3 cities, all months
########################################################

source("working-papers/working-paper-ipc/least-cost-metrics/00_config.R")

panel <- readRDS(file.path(tmp_dir, "panel_city_month_food_1999_2025.rds"))

panel2 <- panel %>%
  rename(
    Energy = energia_kcal,
    Protein = proteina_g,
    Lipids = lipidos_g,
    Carbohydrates = carbohidratos_totales_g,
    VitaminC = vitamina_c_mg,
    Folate = folatos_mcg,
    VitaminA = vitamina_a_er,
    Thiamine = tiamina_mg,
    Riboflavin = riboflavina_mg,
    Niacin = niacina_mg,
    VitaminB12 = vitamina_b12_mcg,
    Magnesium = magnesio_mg,
    Phosphorus = fosforo_mg,
    Sodium = sodio_mg,
    Calcium = calcio_mg,
    Iron = hierro_mg,
    Zinc = zinc_mg
  )

nutr_cols <- c("Energy","Protein","Lipids","Carbohydrates","VitaminC","Folate","VitaminA",
               "Thiamine","Riboflavin","Niacin","VitaminB12","Magnesium","Phosphorus",
               "Sodium","Calcium","Iron","Zinc")

results_cost <- list()
results_comp <- list()
fail_cona <- list()

for (cc in sort(unique(panel2$ciudad))) {
  
  message("== CoNA city: ", cc)
  
  panel_c <- panel2 %>% filter(ciudad == cc) %>% arrange(fecha)
  fechas <- sort(unique(panel_c$fecha))
  
  for (f in fechas) {
    
    df.aux <- panel_c %>%
      filter(fecha == f) %>%
      transmute(
        Food = articulo,
        Price_100g = precio_100g,
        Serving = 100,
        across(all_of(nutr_cols), ~ .x)
      ) %>%
      filter(!is.na(Price_100g), !is.na(Energy), Price_100g > 0, Energy > 0)
    
    if (nrow(df.aux) == 0) {
      fail_cona[[length(fail_cona) + 1]] <- tibble(ciudad = cc, fecha = f, motivo = "No valid foods")
      next
    }
    
    # collapse duplicates by Food (avoids LP problems)
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
        mutate(ciudad = cc, fecha = f, escenario = "precio_100g")
      
      comp_df <- cona.aux$comp %>%
        mutate(ciudad = cc, fecha = f, escenario = "precio_100g")
      
      list(cost = cost_df, comp = comp_df)
      
    }, error = function(e) {
      fail_cona[[length(fail_cona) + 1]] <- tibble(ciudad = cc, fecha = f, motivo = e$message)
      NULL
    })
    
    if (!is.null(out)) {
      results_cost[[length(results_cost) + 1]] <- out$cost
      results_comp[[length(results_comp) + 1]] <- out$comp
    }
  }
}

cona_cost <- if (length(results_cost) == 0) tibble() else bind_rows(results_cost)
cona_comp <- if (length(results_comp) == 0) tibble() else bind_rows(results_comp)
fail_df   <- if (length(fail_cona) == 0) tibble() else bind_rows(fail_cona)

saveRDS(cona_cost, file.path(out_dir, "cona_cost_fullsample.rds"))
saveRDS(cona_comp, file.path(out_dir, "cona_comp_fullsample.rds"))
write_csv(cona_cost, file.path(out_dir, "cona_cost_fullsample.csv"))
write_csv(cona_comp, file.path(out_dir, "cona_comp_fullsample.csv"))

write_xlsx(
  list(cona_cost = cona_cost, cona_comp = cona_comp, failures = fail_df),
  file.path(out_dir, "cona_fullsample.xlsx")
)

message("Saved CoNA in: ", out_dir)
