########################################################
## 03_compute_CoCA_fullsample.R
## CoCA for 3 cities, all months (panel monthly)
## Three price scenarios: q1, q2, q3
########################################################

source("working-papers/working-paper-q2/scripts/00_config_q2.R")

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

# Chequeos mínimos
req_cols <- c("ciudad", "fecha", "articulo_ipc", "energia_kcal",
              "precio_100g_q1", "precio_100g_q2", "precio_100g_q3")
missing_cols <- setdiff(req_cols, names(panel))
if (length(missing_cols) > 0) {
  stop("Al panel le faltan estas columnas requeridas: ", paste(missing_cols, collapse = ", "))
}

panel <- panel %>%
  mutate(
    ciudad = as.character(ciudad),
    fecha  = as.Date(fecha),
    articulo_ipc = as.character(articulo_ipc)
  )

message("Panel rows: ", nrow(panel))
message("Cities: ", paste(sort(unique(panel$ciudad)), collapse = ", "))
message("Date range: ", min(panel$fecha, na.rm = TRUE), " to ", max(panel$fecha, na.rm = TRUE))

# -----------------------------
# 2) Scenario mapping (q1/q2/q3)
# -----------------------------
scenario_map <- tibble(
  escenario = c("q1", "q2", "q3"),
  price_col = c("precio_100g_q1", "precio_100g_q2", "precio_100g_q3")
)

# -----------------------------
# 3) Compute CoCA by city x month x scenario
# -----------------------------
results_coca <- list()
fail_coca <- list()

cities <- sort(unique(panel$ciudad))

for (cc in cities) {
  message("== CoCA city: ", cc)
  
  panel_c <- panel %>%
    filter(ciudad == cc) %>%
    arrange(fecha)
  
  fechas <- sort(unique(panel_c$fecha))
  
  for (k in 1:length(fechas)) {
    f = fechas[k]
    df_f <- panel_c %>% filter(fecha == f)
    
    for (s in seq_len(nrow(scenario_map))) {
      esc <- scenario_map$escenario[s]
      pcol <- scenario_map$price_col[s]
      
      # construir df para CoCA
      df.aux <- df_f %>%
        transmute(
          Food      = alimento_sipsa,
          Price_100g = .data[[pcol]],
          Serving   = 100,
          Energy    = energia_kcal
        ) %>%
        filter(!is.na(Price_100g),
               !is.na(Energy),
               Price_100g > 0,
               Energy > 0)
      
      if (nrow(df.aux) == 0) {
        fail_coca[[length(fail_coca) + 1]] <- tibble(
          ciudad = cc, fecha = f, escenario = esc,
          motivo = "No valid foods (missing/invalid price or energy)"
        )
        next
      }
      
      out <- tryCatch({
        coca.aux <- FoodpriceR::CoCA(data = df.aux, EER = EER)
        # coca.aux$cost devuelve el costo (estructura del paquete)
        coca.aux$cost %>%
          mutate(ciudad = cc, fecha = f, escenario = esc)
      }, error = function(e) {
        fail_coca[[length(fail_coca) + 1]] <- tibble(
          ciudad = cc, fecha = f, escenario = esc, motivo = e$message
        )
        NULL
      })
      
      if (!is.null(out)) results_coca[[length(results_coca) + 1]] <- out
    }
  }
}

coca_df <- if (length(results_coca) == 0) tibble() else bind_rows(results_coca)
fail_df <- if (length(fail_coca) == 0) tibble() else bind_rows(fail_coca)

# -----------------------------
# 4) Save outputs
# -----------------------------
# (a) todo junto
saveRDS(coca_df, file.path(q2_fullsample_dir, "CoCA_city_month_q1_q2_q3.rds"))
write_csv(coca_df, file.path(q2_fullsample_dir, "CoCA_city_month_q1_q2_q3.csv"))

# (b) opcional: separados por escenario (útil para tu pipeline)
for (esc in c("q1", "q2", "q3")) {
  df_esc <- coca_df %>% filter(escenario == esc)
  write_csv(df_esc, file.path(q2_fullsample_dir, paste0("CoCA_city_month_", esc, ".csv")))
  saveRDS(df_esc, file.path(q2_fullsample_dir, paste0("CoCA_city_month_", esc, ".rds")))
}

# (c) excel con failures
write_xlsx(
  list(
    coca = coca_df,
    failures = fail_df
  ),
  file.path(q2_fullsample_dir, "CoCA_city_month_q1_q2_q3.xlsx")
)

message("Saved CoCA outputs in: ", q2_fullsample_dir)
message("Rows in CoCA: ", nrow(coca_df))
message("Failures: ", nrow(fail_df))
