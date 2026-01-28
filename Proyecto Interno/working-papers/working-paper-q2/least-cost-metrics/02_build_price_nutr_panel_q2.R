########################################################
## 02_build_panel_city_month_food.R
## Une: predicción de precios (SIPSA completa) + TCAC
## y construye variables precio_500g/100g (Q1/Q2/Q3)
########################################################

source("working-papers/working-paper-q2/least-cost-metrics/01_tcac_master_q2.R")

# -----------------------------
# 1) Cargar predicciones de precios (SIPSA completa)
# -----------------------------

prices_pred <- readRDS(in_prices_pred) %>%
  mutate(
    ciudad = as.character(ciudad),
    alimento_sipsa = as.character(alimento_sipsa),
    fecha = as.Date(date)
  ) %>%
  filter(ciudad %in% cities_use)

message("Prices_pred date range: ",
        min(prices_pred$fecha, na.rm = TRUE), " to ",
        max(prices_pred$fecha, na.rm = TRUE))

# -----------------------------
# 2) Cargar tcac_master
# -----------------------------
in_tcac_master <- file.path(tcac_dir, "tcac_master.rds")

tcac_master <- readRDS(in_tcac_master) %>%
  mutate(alimento_sipsa = as.character(alimento_sipsa))

# -----------------------------
# 3) Merge prices + tcac
# -----------------------------
panel <- prices_pred %>%
  left_join(tcac_master, by = "alimento_sipsa")

# -----------------------------
# 4) Construir variables de precios (Q1/Q2/Q3)
#     - precio_ipc_pred_q* están en 500g (según tu estándar previo)
#     - Convertir a 100g: /5
#     - Ajustar por parte comestible pc
# -----------------------------
panel <- panel %>%
  mutate(
    precio_500g_q1 = precio_ipc_pred_q1,
    precio_500g_q2 = precio_ipc_pred_q2,
    precio_500g_q3 = precio_ipc_pred_q3,
    
    pc_raw = parte_comestible_percent,
    pc = case_when(
      is.na(pc_raw) ~ NA_real_,
      pc_raw > 1 ~ pc_raw / 100,  # si viene como 80
      TRUE ~ pc_raw              # si viene como 0.80
    ),
    
    precio_100g_q1 = (precio_500g_q1 / 5) / pc,
    precio_100g_q2 = (precio_500g_q2 / 5) / pc,
    precio_100g_q3 = (precio_500g_q3 / 5) / pc,
    
    ano = Year,
    mes_num = Month
  )

# -----------------------------
# 5) Guardar panel final
# -----------------------------
out_rds <- file.path(q2_fullsample_dir, "panel_city_month_food_2013_2025.rds")
out_csv <- file.path(q2_fullsample_dir, "panel_city_month_food_2013_2025.csv")

saveRDS(panel, out_rds)
write_csv(panel, out_csv)

message("Saved panel:")
message(" - ", out_rds)
message(" - ", out_csv)
