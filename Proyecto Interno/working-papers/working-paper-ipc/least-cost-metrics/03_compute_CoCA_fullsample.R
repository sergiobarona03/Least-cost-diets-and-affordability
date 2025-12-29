########################################################
## 03_compute_CoCA_fullsample.R
## CoCA for 3 cities, all months up to last IPC date
########################################################

source("working-papers/working-paper-ipc/least-cost-metrics/00_config.R")

panel <- readRDS(file.path(tmp_dir, "panel_city_month_food_1999_2025.rds"))

results_coca <- list()
fail_coca <- list()

for (cc in sort(unique(panel$ciudad))) {
  
  message("== CoCA city: ", cc)
  
  panel_c <- panel %>% filter(ciudad == cc) %>% arrange(fecha)
  fechas <- sort(unique(panel_c$fecha))
  
  for (f in fechas) {
    
    df.aux <- panel_c %>%
      filter(fecha == f) %>%
      transmute(
        Food = articulo,
        Price_100g = precio_100g,
        Serving = 100,
        Energy = energia_kcal
      ) %>%
      filter(!is.na(Price_100g), !is.na(Energy), Price_100g > 0, Energy > 0) %>%
      mutate(Price_kcal = Price_100g / Energy)
    
    if (nrow(df.aux) == 0) {
      fail_coca[[length(fail_coca) + 1]] <- tibble(ciudad = cc, fecha = f, motivo = "No valid foods")
      next
    }
    
    out <- tryCatch({
      coca.aux <- FoodpriceR::CoCA(data = df.aux, EER = EER)
      coca.aux$cost %>%
        mutate(ciudad = cc, fecha = f, escenario = "precio_100g")
    }, error = function(e) {
      fail_coca[[length(fail_coca) + 1]] <- tibble(ciudad = cc, fecha = f, motivo = e$message)
      NULL
    })
    
    if (!is.null(out)) results_coca[[length(results_coca) + 1]] <- out
  }
}

coca_df <- if (length(results_coca) == 0) tibble() else bind_rows(results_coca)
fail_df <- if (length(fail_coca) == 0) tibble() else bind_rows(fail_coca)

saveRDS(coca_df, file.path(out_dir, "coca_fullsample.rds"))
write_csv(coca_df, file.path(out_dir, "coca_fullsample.csv"))

write_xlsx(
  list(coca = coca_df, failures = fail_df),
  file.path(out_dir, "coca_fullsample.xlsx")
)

message("Saved CoCA in: ", out_dir)
