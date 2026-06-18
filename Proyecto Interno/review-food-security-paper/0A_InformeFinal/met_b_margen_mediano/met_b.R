########################################################
## Metodología B: Estimación de precios minoristas          ##
## a partir del margen de comercialización mediano (Q1–Q3)   ##
## entre precios mayoristas SIPSA y precios minoristas IPC   ##
##                                                            ##
## A diferencia de A1/A2, NO hay ancla ni acumulación:        ##
##   P_hat_min_{i,c,t} = P_sipsa_{i,c,t} * (1 + Q2_{i,c}/100) ##
## El precio imputado depende SOLO del precio mayorista       ##
## contemporáneo y del margen calibrado, mes a mes.            ##
##                                                            ##
## DOS VENTANAS:                                               ##
##   1) Validación:                                              ##
##        Margen Q1-Q2-Q3 calibrado con pares IPC-SIPSA          ##
##        observados con fecha <= 2015-12                         ##
##        Se aplica el margen a SIPSA contemporáneo en             ##
##        2016-02 a 2018-03 y se compara contra IPC observado       ##
##   2) Pronóstico:                                                  ##
##        Margen recalibrado con todo el histórico (<= 2018-03)      ##
##        Se aplica a SIPSA contemporáneo de 2018-04 a 2024-12         ##
##        Sin verdad observada (no hay IPC real en ese tramo)          ##
##                                                            ##
## Output:                                                     ##
##   - output/metB_precios_dos_ventanas.rds (panel completo)    ##
##   - output/tablas/metB_margenes_q1_q3_por_ventana.xlsx          ##
##   - output/tablas/metB_metricas_ajuste_ventana1.xlsx             ##
##   - output/figuras/<ventana>/<ciudad>.pdf (multi-página)          ##
########################################################

# -----------------------------
# Packages
# -----------------------------
library(tidyverse)
library(readxl)
library(lubridate)
library(writexl)

# -----------------------------
# Paths (EDITAR AQUÍ — sin 00_config.R)
# -----------------------------
base_dir <- "C:/Users/Portatil/Desktop/Least-cost-diets-and-affordability/Proyecto Interno/review-food-security-paper/0A_InformeFinal/"

in_merged <- file.path(base_dir, "input/121225_dataset_ipc_sipsa.xlsx")

out_dir     <- file.path(base_dir, "met_b_margen_mediano/output")
out_figuras <- file.path(out_dir, "figuras")
out_tablas  <- file.path(out_dir, "tablas")

dir.create(out_dir,     showWarnings = FALSE, recursive = TRUE)
dir.create(out_figuras, showWarnings = FALSE, recursive = TRUE)
dir.create(out_tablas,  showWarnings = FALSE, recursive = TRUE)

# -----------------------------
# Ciudades a procesar
# -----------------------------
CITY_COLORS <- c("CALI" = "#27AE60", "BOGOTA D.C." = "#2C3E6B", "MEDELLIN" = "#C0392B")
CITY_LABELS <- c("CALI" = "Cali", "BOGOTA D.C." = "Bogotá", "MEDELLIN" = "Medellín")

# -----------------------------
# Parámetro mínimo de observaciones para que el margen sea "estable"
# -----------------------------
MIN_OBS_MARGEN <- 12

# -----------------------------
# Definición de las dos ventanas
# -----------------------------
ventanas <- list(
  list(
    nombre     = "ventana1_validacion",
    TRAIN_END  = as.Date("2015-12-01"),
    EVAL_START = as.Date("2016-02-01"),
    EVAL_END   = as.Date("2018-03-01"),
    tipo       = "validacion"
  ),
  list(
    nombre     = "ventana2_pronostico",
    TRAIN_END  = as.Date("2018-03-01"),
    EVAL_START = as.Date("2018-04-01"),
    EVAL_END   = as.Date("2024-12-01"),
    tipo       = "pronostico"
  )
)

# -----------------------------
# Helpers
# -----------------------------
safe_name <- function(x) {
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  x <- gsub("[^A-Za-z0-9_]+", "_", x)
  x <- gsub("_+", "_", x)
  x <- gsub("^_|_$", "", x)
  x
}

strip_accents <- function(x) iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT")

# -----------------------------
# 1. Cargar datos (una sola vez, se reutiliza en ambas ventanas)
# -----------------------------
message("Cargando panel IPC-SIPSA unido...")
data_merged <- read_excel(in_merged) %>%
  mutate(
    ciudad = strip_accents(as.character(ciudad)),
    fecha  = as.Date(paste(Year, Month, "01", sep = "-"))
  )

message(sprintf("  %d filas | %d ciudades | %d alimentos SIPSA",
                nrow(data_merged), n_distinct(data_merged$ciudad), n_distinct(data_merged$alimento_sipsa)))

CITIES_USE <- sort(unique(data_merged$ciudad))
message("  Ciudades detectadas: ", paste(CITIES_USE, collapse = ", "))

# -----------------------------
# Acumuladores globales
# -----------------------------
margenes_all   <- list()
metricas_all   <- list()
prices_all_windows <- list()

margenes_v1 <- NULL  # margen calibrado en ventana 1, por si se necesita comparar/reusar

# -----------------------------
# 2. Loop principal por ventana
# -----------------------------
for (v in seq_along(ventanas)) {
  
  vent <- ventanas[[v]]
  message("\n=== Procesando ", vent$nombre, " | TRAIN_END: ", vent$TRAIN_END,
          " | eval: ", vent$EVAL_START, " a ", vent$EVAL_END, " ===")
  
  vent_dir <- file.path(out_figuras, vent$nombre)
  dir.create(vent_dir, recursive = TRUE, showWarnings = FALSE)
  
  # -----------------------------------------------------------------
  # 2.1 Calibrar margen Q1-Q2-Q3 SOLO con datos <= TRAIN_END
  # -----------------------------------------------------------------
  message("  Calibrando márgenes de comercialización...")
  
  data_margenes <- data_merged %>%
    filter(
      fecha <= vent$TRAIN_END,
      !is.na(precio_sipsa), !is.na(precio_ipc),
      precio_sipsa > 0
    ) %>%
    mutate(margen = (precio_ipc - precio_sipsa) / precio_sipsa * 100)
  
  margenes_q <- data_margenes %>%
    group_by(ciudad, alimento_sipsa) %>%
    summarise(
      n_obs     = n(),
      margen_q1 = quantile(margen, 0.25, na.rm = TRUE),
      margen_q2 = quantile(margen, 0.50, na.rm = TRUE),
      margen_q3 = quantile(margen, 0.75, na.rm = TRUE),
      .groups   = "drop"
    ) %>%
    filter(n_obs >= MIN_OBS_MARGEN)
  
  message(sprintf("    %d combinaciones ciudad-alimento con margen estable (n_obs >= %d)",
                  nrow(margenes_q), MIN_OBS_MARGEN))
  
  margenes_all[[vent$nombre]] <- margenes_q %>% mutate(ventana = vent$nombre)
  if (vent$tipo == "validacion") margenes_v1 <- margenes_q
  
  # -----------------------------------------------------------------
  # 2.2 Construir predicciones: P_hat = P_sipsa_contemporaneo * (1 + margen/100)
  #     Aplicado sobre el periodo de evaluación de esta ventana
  # -----------------------------------------------------------------
  message("  Construyendo precios estimados...")
  
  data_pred <- data_merged %>%
    filter(
      fecha >= vent$EVAL_START, fecha <= vent$EVAL_END,
      !is.na(precio_sipsa), precio_sipsa > 0
    ) %>%
    left_join(
      margenes_q %>% select(ciudad, alimento_sipsa, margen_q1, margen_q2, margen_q3),
      by = c("ciudad", "alimento_sipsa")
    ) %>%
    mutate(
      precio_ipc_pred_q1 = if_else(!is.na(margen_q1), precio_sipsa * (1 + margen_q1 / 100), NA_real_),
      precio_ipc_pred_q2 = if_else(!is.na(margen_q2), precio_sipsa * (1 + margen_q2 / 100), NA_real_),
      precio_ipc_pred_q3 = if_else(!is.na(margen_q3), precio_sipsa * (1 + margen_q3 / 100), NA_real_),
      ventana      = vent$nombre,
      tipo_ventana = vent$tipo
    ) %>%
    arrange(ciudad, alimento_sipsa, fecha)
  
  message(sprintf("    %d filas en el panel de predicción de esta ventana", nrow(data_pred)))
  
  prices_all_windows[[vent$nombre]] <- data_pred %>%
    select(ventana, tipo_ventana, ciudad, alimento_sipsa, fecha,
           precio_sipsa, precio_ipc_obs = precio_ipc,
           margen_q1, margen_q2, margen_q3,
           precio_ipc_pred_q1, precio_ipc_pred_q2, precio_ipc_pred_q3)
  
  # -----------------------------------------------------------------
  # 2.3 Métricas de ajuste (SOLO ventana de validación, donde hay IPC observado)
  # -----------------------------------------------------------------
  if (vent$tipo == "validacion") {
    
    message("  Calculando métricas de ajuste contra IPC observado...")
    
    data_eval <- data_pred %>%
      filter(!is.na(precio_ipc)) %>%
      mutate(
        error_q1 = precio_ipc_pred_q1 - precio_ipc,
        error_q2 = precio_ipc_pred_q2 - precio_ipc,
        error_q3 = precio_ipc_pred_q3 - precio_ipc,
        ape_q1   = abs(error_q1) / precio_ipc * 100,
        ape_q2   = abs(error_q2) / precio_ipc * 100,
        ape_q3   = abs(error_q3) / precio_ipc * 100
      )
    
    resumen_global <- data_eval %>%
      summarise(
        n_obs      = n(),
        mape_q1    = mean(ape_q1, na.rm = TRUE),
        mape_q2    = mean(ape_q2, na.rm = TRUE),
        mape_q3    = mean(ape_q3, na.rm = TRUE),
        me_q2      = mean(error_q2, na.rm = TRUE),
        mae_q2     = mean(abs(error_q2), na.rm = TRUE),
        rmse_q2    = sqrt(mean(error_q2^2, na.rm = TRUE)),
        med_ape_q2 = median(ape_q2, na.rm = TRUE)
      ) %>%
      mutate(ciudad = "TODAS", ventana = vent$nombre)
    
    resumen_ciudad <- data_eval %>%
      group_by(ciudad) %>%
      summarise(
        n_obs      = n(),
        mape_q1    = mean(ape_q1, na.rm = TRUE),
        mape_q2    = mean(ape_q2, na.rm = TRUE),
        mape_q3    = mean(ape_q3, na.rm = TRUE),
        me_q2      = mean(error_q2, na.rm = TRUE),
        mae_q2     = mean(abs(error_q2), na.rm = TRUE),
        rmse_q2    = sqrt(mean(error_q2^2, na.rm = TRUE)),
        med_ape_q2 = median(ape_q2, na.rm = TRUE),
        .groups    = "drop"
      ) %>%
      mutate(ventana = vent$nombre)
    
    metricas_all[[vent$nombre]] <- bind_rows(resumen_ciudad, resumen_global)
    
    message(sprintf("    MAPE Q2 (mediana del margen) global: %.2f%%", resumen_global$mape_q2))
    print(resumen_ciudad)
  }
  
  # -----------------------------------------------------------------
  # 2.4 Figuras: observado vs. estimado (Q2), un PDF multi-página por ciudad
  # -----------------------------------------------------------------
  message("  Generando figuras observado vs. estimado...")
  
  plots_by_city <- setNames(vector("list", length(CITIES_USE)), CITIES_USE)
  
  # Solo graficamos combinaciones ciudad-alimento que SÍ tienen margen calibrado
  # (es decir, que tuvieron suficiente coincidencia IPC-SIPSA en el periodo de entrenamiento).
  # Si no hay margen, precio_ipc_pred_q2 sería todo NA y la figura saldría en blanco — se omite.
  items_in_window <- data_pred %>%
    filter(!is.na(precio_ipc_pred_q2)) %>%
    distinct(ciudad, alimento_sipsa) %>%
    arrange(ciudad, alimento_sipsa)
  
  n_omitidos <- data_pred %>% distinct(ciudad, alimento_sipsa) %>% nrow() - nrow(items_in_window)
  if (n_omitidos > 0) {
    message(sprintf("    %d combinaciones ciudad-alimento omitidas (sin margen calibrado / sin coincidencia mayorista)",
                    n_omitidos))
  }
  
  for (i in seq_len(nrow(items_in_window))) {
    
    city_name <- items_in_window$ciudad[i]
    food_name <- items_in_window$alimento_sipsa[i]
    
    df_plot <- data_pred %>% filter(ciudad == city_name, alimento_sipsa == food_name)
    
    # Salvaguarda adicional: si por algún motivo todas las predicciones quedaron NA, se omite
    if (all(is.na(df_plot$precio_ipc_pred_q2))) next
    
    # Histórico completo de precio observado (desde el inicio del panel, ej. 1999-01),
    # SOLO para referencia visual — no afecta el margen calibrado ni las métricas.
    df_obs_full <- data_merged %>%
      filter(ciudad == city_name, alimento_sipsa == food_name, !is.na(precio_ipc)) %>%
      select(fecha, precio_ipc)
    
    p <- ggplot() +
      geom_ribbon(data = df_plot,
                  aes(x = fecha, ymin = precio_ipc_pred_q1, ymax = precio_ipc_pred_q3),
                  fill = "red", alpha = 0.15) +
      geom_line(data = df_obs_full,
                aes(x = fecha, y = precio_ipc, color = "Precio observado (IPC)"),
                linewidth = 0.35) +
      geom_line(data = df_plot,
                aes(x = fecha, y = precio_ipc_pred_q2, color = "Precio estimado (margen Q2)"),
                linewidth = 0.4, linetype = "dashed") +
      scale_color_manual(values = c(
        "Precio observado (IPC)" = "black",
        "Precio estimado (margen Q2)" = "red"
      )) +
      labs(
        title = paste0("Metodología B — ", vent$nombre),
        subtitle = paste0(city_name, " — ", food_name,
                          " | observado desde inicio del panel | banda sombreada: margen Q1-Q3"),
        x = NULL, y = "Precio", color = ""
      ) +
      theme_bw(base_size = 10) +
      theme(legend.position = "bottom")
    
    plots_by_city[[city_name]][[length(plots_by_city[[city_name]]) + 1]] <- p
  }
  
  for (cn in names(plots_by_city)) {
    if (length(plots_by_city[[cn]]) == 0) next
    pdf_path <- file.path(vent_dir, paste0(safe_name(cn), ".pdf"))
    pdf(pdf_path, width = 10, height = 4, onefile = TRUE)
    for (pl in plots_by_city[[cn]]) print(pl)
    dev.off()
    message("    PDF guardado: ", pdf_path, " (", length(plots_by_city[[cn]]), " páginas)")
  }
}

# -----------------------------
# 3. Unir outputs y guardar
# -----------------------------
prices_all_windows_df <- bind_rows(prices_all_windows)
margenes_all_df        <- bind_rows(margenes_all)
metricas_all_df         <- bind_rows(metricas_all)

saveRDS(prices_all_windows_df, file.path(out_dir, "metB_precios_dos_ventanas.rds"))

write_xlsx(
  split(margenes_all_df, margenes_all_df$ventana),
  file.path(out_tablas, "metB_margenes_q1_q3_por_ventana.xlsx")
)

write_xlsx(
  list(metricas_validacion = metricas_all_df),
  file.path(out_tablas, "metB_metricas_ajuste_ventana1.xlsx")
)

message("\nListo. Outputs en: ", out_dir)
message(sprintf("  %d filas en el panel de precios (2 ventanas)", nrow(prices_all_windows_df)))