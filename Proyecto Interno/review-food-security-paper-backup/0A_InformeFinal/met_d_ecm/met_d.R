########################################################
## Metodología D: Modelo de corrección de error estándar (ECM)  ##
##                                                                ##
## Relación de largo plazo:                                        ##
##   ln(P_min_it) = alpha_i + beta_i*ln(P_may_it) + e_it             ##
##                                                                ##
## Dinámica de corto plazo (restricción de Enders: k>=1, p=q=k,       ##
## sin Δx_t contemporáneo, selección de k por BIC):                    ##
##   Δln(P_min_it) = c_i0 + sum_p beta_ip*Δln(P_min_i,t-p)              ##
##                  + sum_q gamma_iq*Δln(P_may_i,t-q)                    ##
##                  + theta_i*e_i,t-1 + u_it                              ##
##                                                                ##
## APLICACIÓN: DINÁMICA / RECURSIVA                                  ##
##   Se predice Δln(P_min_t) un paso adelante y se ACUMULA mes a       ##
##   mes; los propios valores predichos alimentan los rezagos          ##
##   siguientes y el término de corrección de error (ECT).              ##
##                                                                ##
## Cobertura: SOLO los 7 artículos del mapping IPC-SIPSA                ##
## (ARROZ, CEBOLLA, PAPA, PLÁTANO, TOMATE, YUCA, ZANAHORIA),               ##
## con variedades SIPSA específicas por ciudad (Cali, Bogotá, Medellín)    ##
##                                                                ##
## DOS VENTANAS (mismos cortes que A1/A2/B/C):                          ##
##   1) Validación:  entrena <= 2015-12 | eval 2016-02 a 2018-03          ##
##   2) Pronóstico:  entrena <= 2018-03 | eval 2018-04 a 2024-12            ##
##                                                                ##
## Output:                                                          ##
##   - output/metD_precios_dos_ventanas.rds (panel completo)           ##
##   - output/tablas/metD_coint_longrun_por_ventana.xlsx                  ##
##   - output/tablas/metD_ecm_coeficientes_por_ventana.xlsx                ##
##   - output/tablas/metD_lags_seleccionados_por_ventana.xlsx               ##
##   - output/tablas/metD_metricas_ajuste_ventana1.xlsx                      ##
##   - output/figuras/<ventana>/<ciudad>.pdf (multi-página)                   ##
########################################################

# -----------------------------
# Packages
# -----------------------------
library(tidyverse)
library(readxl)
library(lubridate)
library(writexl)
library(aTSA)
library(purrr)

# -----------------------------
# Paths (EDITAR AQUÍ — sin 00_config.R)
# -----------------------------
base_dir <- "C:/Users/Portatil/Desktop/Least-cost-diets-and-affordability/Proyecto Interno/review-food-security-paper/0A_InformeFinal/"

in_merged <- file.path(base_dir, "input/121225_dataset_ipc_sipsa.xlsx")

out_dir     <- file.path(base_dir, "met_d_ecm/output")
out_figuras <- file.path(out_dir, "figuras")
out_tablas  <- file.path(out_dir, "tablas")

dir.create(out_dir,     showWarnings = FALSE, recursive = TRUE)
dir.create(out_figuras, showWarnings = FALSE, recursive = TRUE)
dir.create(out_tablas,  showWarnings = FALSE, recursive = TRUE)

# -----------------------------
# Funciones auxiliares
# -----------------------------
source(file.path(base_dir, "met_d_ecm/aux_functions/aux_functions_metD.R"))

# -----------------------------
# Parámetros de estimación
# -----------------------------
criterion_ecm <- "BIC"
max_k         <- 6
min_ecm_obs   <- 24

# -----------------------------
# Mapping ciudad-artículo_ipc-alimento_sipsa (hardcodeado, igual que ecm_run.R)
# -----------------------------
cali.mapping <- list(
  "ARROZ PARA SECO"   = c("Arroz de primera"),
  "CEBOLLA CABEZONA"  = c("Cebolla cabezona blanca bogotana"),
  "PAPA"              = c("Papa capira", "Papa parda pastusa", "Papa suprema", "Papa única"),
  "PLÁTANO"           = c("Plátano hartón verde"),
  "TOMATE"            = c("Tomate larga vida"),
  "YUCA"              = c("Yuca ICA"),
  "ZANAHORIA"         = c("Zanahoria bogotana")
)

bogota.mapping <- list(
  "ARROZ PARA SECO"   = c("Arroz de primera"),
  "CEBOLLA CABEZONA"  = c("Cebolla cabezona blanca"),
  "PAPA"              = c("Papa R-12 negra", "Papa parda pastusa", "Papa suprema", "Papa única"),
  "PLÁTANO"           = c("Plátano guineo", "Plátano hartón verde", "Plátano hartón verde llanero"),
  "TOMATE"            = c("Tomate chonto", "Tomate larga vida"),
  "YUCA"              = c("Yuca llanera"),
  "ZANAHORIA"         = c("Zanahoria")
)

medellin.mapping <- list(
  "ARROZ PARA SECO"   = c("Arroz de primera"),
  "CEBOLLA CABEZONA"  = c("Cebolla cabezona blanca"),
  "PAPA"              = c("Papa R-12 negra", "Papa capira", "Papa nevada"),
  "PLÁTANO"           = c("Plátano guineo", "Plátano hartón maduro", "Plátano hartón verde"),
  "TOMATE"            = c("Tomate larga vida"),
  "YUCA"              = c("Yuca ICA"),
  "ZANAHORIA"         = c("Zanahoria larga vida")
)

mapping_to_df <- function(city_code, mp) {
  purrr::imap_dfr(mp, function(sipsa_vec, art) {
    tibble(ciudad = city_code, articulo_ipc = art, alimento_sipsa = sipsa_vec)
  })
}

map_df <- bind_rows(
  mapping_to_df("CALI",        cali.mapping),
  mapping_to_df("BOGOTA D.C.", bogota.mapping),
  mapping_to_df("MEDELLIN",    medellin.mapping)
) %>%
  mutate(
    articulo_ipc   = str_squish(as.character(articulo_ipc)),
    alimento_sipsa = str_squish(as.character(alimento_sipsa))
  )

CITIES_USE <- c("CALI", "BOGOTA D.C.", "MEDELLIN")

# -----------------------------
# Definición de las dos ventanas (mismos cortes que A1/A2/B/C)
# -----------------------------
ventanas <- list(
  list(
    nombre     = "ventana1_validacion",
    TRAIN_END  = as.Date("2015-12-01"),
    EVAL_START = as.Date("2016-01-01"),  # sin hueco: inmediatamente después de TRAIN_END
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
# 1. Cargar datos y filtrar al mapping IPC-SIPSA
# -----------------------------
message("Cargando panel IPC-SIPSA unido...")
data_raw <- read_excel(in_merged) %>%
  mutate(
    ciudad         = strip_accents(as.character(ciudad)),
    articulo_ipc   = str_squish(as.character(articulo_ipc)),
    alimento_sipsa = str_squish(as.character(alimento_sipsa)),
    fecha          = as.Date(paste(Year, Month, "01", sep = "-"))
  ) %>%
  # IMPORTANTE: solo exigimos precio_sipsa válido aquí. precio_ipc puede ser
  # NA (es lo que se predice en la ventana de pronóstico, donde no hay IPC
  # observado más allá de marzo-2018). Si exigiéramos !is.na(precio_ipc) aquí,
  # se eliminarían silenciosamente todas las filas de la ventana de pronóstico.
  filter(!is.na(precio_sipsa), precio_sipsa > 0,
         is.na(precio_ipc) | precio_ipc > 0)

data_merged <- data_raw %>%
  semi_join(map_df, by = c("ciudad", "articulo_ipc", "alimento_sipsa")) %>%
  group_by(ciudad, fecha, articulo_ipc, alimento_sipsa) %>%
  summarise(
    precio_ipc   = mean(precio_ipc, na.rm = TRUE),
    precio_sipsa = mean(precio_sipsa, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    # mean(..., na.rm=TRUE) sobre un grupo TODO-NA da NaN, no NA — normalizamos
    precio_ipc   = if_else(is.nan(precio_ipc),   NA_real_, precio_ipc),
    precio_sipsa = if_else(is.nan(precio_sipsa), NA_real_, precio_sipsa),
    log_ipc   = if_else(is.na(precio_ipc), NA_real_, log(precio_ipc)),
    log_sipsa = log(precio_sipsa),
    mes       = factor(month(fecha), levels = 1:12, labels = month.abb)
  ) %>%
  filter(!is.na(precio_sipsa)) %>%  # precio_sipsa es indispensable siempre
  arrange(ciudad, articulo_ipc, alimento_sipsa, fecha)

message(sprintf("  %d filas | %d ciudades | %d pares articulo-alimento (tras mapping)",
                nrow(data_merged), n_distinct(data_merged$ciudad),
                n_distinct(paste(data_merged$articulo_ipc, data_merged$alimento_sipsa))))

# -----------------------------
# Acumuladores globales
# -----------------------------
coint_all   <- list()
lr_all      <- list()
ecm_coef_all <- list()
lags_all    <- list()
metricas_all <- list()
prices_all_windows <- list()

# -----------------------------
# 2. Loop principal por ventana
# -----------------------------
for (v in seq_along(ventanas)) {
  
  vent <- ventanas[[v]]
  message("\n=== Procesando ", vent$nombre, " | TRAIN_END: ", vent$TRAIN_END,
          " | eval: ", vent$EVAL_START, " a ", vent$EVAL_END, " ===")
  
  vent_dir <- file.path(out_figuras, vent$nombre)
  dir.create(vent_dir, recursive = TRUE, showWarnings = FALSE)
  
  data_train <- data_merged %>% filter(fecha <= vent$TRAIN_END)
  
  keys <- data_train %>%
    distinct(ciudad, articulo_ipc, alimento_sipsa) %>%
    arrange(ciudad, articulo_ipc, alimento_sipsa)
  
  ecm_fits   <- list()   # modelos ajustados, indexados por key_id
  lr_models  <- list()   # relación de largo plazo, indexados por key_id
  k_selected <- list()   # k óptimo, indexado por key_id
  
  coint_long <- tibble()
  lr_long    <- tibble()
  ecm_long   <- tibble()
  lags_long  <- tibble()
  
  # -----------------------------------------------------------------
  # 2.1 Para cada serie: prueba de cointegración + relación de largo
  #     plazo + selección de rezagos + ajuste del ECM
  # -----------------------------------------------------------------
  message("  Estimando cointegración, relación de largo plazo y ECM...")
  
  for (i in seq_len(nrow(keys))) {
    
    city_i <- keys$ciudad[i]
    art_i  <- keys$articulo_ipc[i]
    ali_i  <- keys$alimento_sipsa[i]
    key_id <- paste(city_i, art_i, ali_i, sep = "||")
    
    data.food <- data_train %>%
      filter(ciudad == city_i, articulo_ipc == art_i, alimento_sipsa == ali_i) %>%
      drop_na(log_ipc, log_sipsa, mes) %>%
      arrange(fecha)
    
    if (nrow(data.food) < 36) next
    
    # ---- Prueba de cointegración (Engle-Granger, vía aTSA) ----
    ct <- tryCatch(aTSA::coint.test(y = data.food$log_ipc, X = data.food$log_sipsa),
                   error = function(e) NULL)
    if (!is.null(ct)) {
      m_ct <- as.matrix(ct)
      coint_long <- bind_rows(coint_long, tibble(
        ciudad = city_i, articulo_ipc = art_i, alimento_sipsa = ali_i,
        p_type1 = as.numeric(m_ct[1, 3]),
        p_type2 = as.numeric(m_ct[2, 3]),
        p_type3 = as.numeric(m_ct[3, 3])
      ))
    }
    
    # ---- Relación de largo plazo (sin dummies mensuales) ----
    lr_model <- lm(log_ipc ~ log_sipsa, data = data.food)
    cs_lr <- summary(lr_model)$coefficients
    if ("log_sipsa" %in% rownames(cs_lr)) {
      lr_long <- bind_rows(lr_long, tibble(
        ciudad = city_i, articulo_ipc = art_i, alimento_sipsa = ali_i,
        b = cs_lr["log_sipsa", "Estimate"],
        se = cs_lr["log_sipsa", "Std. Error"],
        p_value = cs_lr["log_sipsa", "Pr(>|t|)"]
      ))
    }
    
    # ---- Selección de rezagos del ECM (restricción de Enders) ----
    sel <- select_ecm_lag_k_full(data.food, max_k = max_k,
                                 criterion = criterion_ecm, min_ecm_obs = min_ecm_obs)
    if (is.null(sel)) next
    
    lags_long <- bind_rows(
      lags_long,
      sel$table %>%
        slice(1:min(5, n())) %>%
        mutate(ciudad = city_i, articulo_ipc = art_i, alimento_sipsa = ali_i,
               criterion = criterion_ecm)
    )
    
    best <- sel$best
    ecm_model <- best$model
    cs <- summary(ecm_model)$coefficients
    
    ecm_long <- bind_rows(
      ecm_long,
      tibble(term = rownames(cs), b = cs[, "Estimate"], se = cs[, "Std. Error"], p_value = cs[, "Pr(>|t|)"]) %>%
        mutate(ciudad = city_i, articulo_ipc = art_i, alimento_sipsa = ali_i,
               k_star = best$k, criterion = criterion_ecm)
    )
    
    ecm_fits[[key_id]]   <- best
    lr_models[[key_id]]  <- lr_model
    k_selected[[key_id]] <- best$k
  }
  
  message(sprintf("    %d series con ECM ajustado (de %d candidatas)", length(ecm_fits), nrow(keys)))
  
  coint_all[[vent$nombre]]    <- coint_long %>% mutate(ventana = vent$nombre)
  lr_all[[vent$nombre]]       <- lr_long    %>% mutate(ventana = vent$nombre)
  ecm_coef_all[[vent$nombre]] <- ecm_long   %>% mutate(ventana = vent$nombre)
  lags_all[[vent$nombre]]     <- lags_long  %>% mutate(ventana = vent$nombre)
  
  # -----------------------------------------------------------------
  # 2.2 Predicción dinámica/recursiva sobre el período de evaluación
  # -----------------------------------------------------------------
  message("  Construyendo predicciones dinámicas (recursivas)...")
  
  data_eval_window <- data_merged %>%
    filter(fecha >= vent$EVAL_START, fecha <= vent$EVAL_END)
  
  pred_list <- list()
  
  for (i in seq_len(nrow(keys))) {
    
    city_i <- keys$ciudad[i]
    art_i  <- keys$articulo_ipc[i]
    ali_i  <- keys$alimento_sipsa[i]
    key_id <- paste(city_i, art_i, ali_i, sep = "||")
    
    if (is.null(ecm_fits[[key_id]])) next
    
    k_use <- k_selected[[key_id]]
    
    df_history <- data_train %>%
      filter(ciudad == city_i, articulo_ipc == art_i, alimento_sipsa == ali_i) %>%
      drop_na(log_ipc, log_sipsa, mes) %>%
      arrange(fecha)
    
    df_eval <- data_eval_window %>%
      filter(ciudad == city_i, articulo_ipc == art_i, alimento_sipsa == ali_i) %>%
      arrange(fecha)
    
    if (nrow(df_eval) == 0) next
    
    pred_out <- tryCatch(
      predict_ecm_dynamic(
        ecm_fit    = ecm_fits[[key_id]],
        lr_model   = lr_models[[key_id]],
        df_history = df_history,
        df_eval    = df_eval,
        k          = k_use
      ),
      error = function(e) NULL
    )
    
    if (is.null(pred_out)) next
    
    pred_list[[length(pred_list) + 1]] <- pred_out %>%
      mutate(ciudad = city_i, articulo_ipc = art_i, alimento_sipsa = ali_i,
             k_used = k_use)
  }
  
  data_pred <- bind_rows(pred_list) %>%
    mutate(ventana = vent$nombre, tipo_ventana = vent$tipo) %>%
    arrange(ciudad, articulo_ipc, alimento_sipsa, fecha)
  
  message(sprintf("    %d filas en el panel de predicción de esta ventana", nrow(data_pred)))
  
  prices_all_windows[[vent$nombre]] <- data_pred
  
  # -----------------------------------------------------------------
  # 2.3 Métricas de ajuste (SOLO ventana de validación)
  # -----------------------------------------------------------------
  if (vent$tipo == "validacion" && nrow(data_pred) > 0) {
    
    message("  Calculando métricas de ajuste contra IPC observado...")
    
    data_metr <- data_pred %>%
      filter(!is.na(price_obs), !is.na(price_pred)) %>%
      mutate(
        error = price_pred - price_obs,
        ape   = abs(error) / price_obs * 100
      )
    
    resumen_global <- data_metr %>%
      summarise(
        n_obs = n(), mape = mean(ape, na.rm = TRUE),
        mae   = mean(abs(error), na.rm = TRUE),
        rmse  = sqrt(mean(error^2, na.rm = TRUE)),
        me    = mean(error, na.rm = TRUE)
      ) %>%
      mutate(ciudad = "TODAS", ventana = vent$nombre)
    
    resumen_ciudad <- data_metr %>%
      group_by(ciudad) %>%
      summarise(
        n_obs = n(), mape = mean(ape, na.rm = TRUE),
        mae   = mean(abs(error), na.rm = TRUE),
        rmse  = sqrt(mean(error^2, na.rm = TRUE)),
        me    = mean(error, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(ventana = vent$nombre)
    
    metricas_all[[vent$nombre]] <- bind_rows(resumen_ciudad, resumen_global)
    
    message(sprintf("    MAPE global: %.2f%%", resumen_global$mape))
    print(resumen_ciudad)
  }
  
  # -----------------------------------------------------------------
  # 2.4 Figuras: observado (histórico completo) vs. estimado,
  #     un PDF multi-página por ciudad
  # -----------------------------------------------------------------
  message("  Generando figuras observado vs. estimado...")
  
  plots_by_city <- setNames(vector("list", length(CITIES_USE)), CITIES_USE)
  
  items_in_window <- data_pred %>%
    filter(!is.na(price_pred)) %>%
    distinct(ciudad, articulo_ipc, alimento_sipsa) %>%
    arrange(ciudad, articulo_ipc, alimento_sipsa)
  
  if (nrow(items_in_window) > 0) {
    for (i in seq_len(nrow(items_in_window))) {
      
      city_name <- items_in_window$ciudad[i]
      art_name  <- items_in_window$articulo_ipc[i]
      food_name <- items_in_window$alimento_sipsa[i]
      
      df_plot <- data_pred %>%
        filter(ciudad == city_name, articulo_ipc == art_name, alimento_sipsa == food_name)
      if (all(is.na(df_plot$price_pred))) next
      
      # Histórico completo de precio observado (desde 1999), solo para referencia visual
      df_obs_full <- data_merged %>%
        filter(ciudad == city_name, articulo_ipc == art_name, alimento_sipsa == food_name) %>%
        select(fecha, precio_ipc)
      
      # Resultado de la prueba de cointegración Engle-Granger, los tres tipos,
      # para anotar en la figura
      coint_row <- coint_long %>%
        filter(ciudad == city_name, articulo_ipc == art_name, alimento_sipsa == food_name)
      
      coint_label <- if (nrow(coint_row) > 0) {
        p1 <- coint_row$p_type1[1]
        p2 <- coint_row$p_type2[1]
        p3 <- coint_row$p_type3[1]
        
        fmt_p <- function(p) if (is.na(p)) "NA" else sprintf("%.3f", p)
        veredicto <- function(p) {
          if (is.na(p)) "?" else if (p < 0.05) "coint." else "no coint."
        }
        
        sprintf(
          "Engle-Granger — Tipo 1 (sin cte/tend): p=%s (%s) | Tipo 2 (con cte): p=%s (%s) | Tipo 3 (cte+tend): p=%s (%s)",
          fmt_p(p1), veredicto(p1),
          fmt_p(p2), veredicto(p2),
          fmt_p(p3), veredicto(p3)
        )
      } else {
        "Cointegración: prueba no disponible para esta serie"
      }
      
      p <- ggplot() +
        geom_ribbon(data = df_plot,
                    aes(x = fecha, ymin = price_pred_lwr, ymax = price_pred_upr),
                    fill = "red", alpha = 0.15) +
        geom_line(data = df_obs_full,
                  aes(x = fecha, y = precio_ipc, color = "Precio observado (IPC)"),
                  linewidth = 0.35) +
        geom_line(data = df_plot,
                  aes(x = fecha, y = price_pred, color = "Precio estimado (ECM)"),
                  linewidth = 0.4, linetype = "dashed") +
        scale_color_manual(values = c(
          "Precio observado (IPC)" = "black",
          "Precio estimado (ECM)"  = "red"
        )) +
        labs(
          title = paste0("Metodología D — ", vent$nombre),
          subtitle = paste0(city_name, " — ", art_name, " (", food_name, ")",
                            " | predicción dinámica/recursiva | banda: IC 95% (varianza acumulada)\n",
                            str_wrap(coint_label, width = 90)),
          x = NULL, y = "Precio", color = ""
        ) +
        theme_bw(base_size = 9.5) +
        theme(legend.position = "bottom")
      
      plots_by_city[[city_name]][[length(plots_by_city[[city_name]]) + 1]] <- p
    }
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
coint_all_df    <- bind_rows(coint_all)
lr_all_df       <- bind_rows(lr_all)
ecm_coef_all_df <- bind_rows(ecm_coef_all)
lags_all_df     <- bind_rows(lags_all)
metricas_all_df <- bind_rows(metricas_all)

saveRDS(prices_all_windows_df, file.path(out_dir, "metD_precios_dos_ventanas.rds"))

write_xlsx(
  list(
    cointegracion = coint_all_df,
    largo_plazo   = lr_all_df
  ),
  file.path(out_tablas, "metD_coint_longrun_por_ventana.xlsx")
)

write_xlsx(
  split(ecm_coef_all_df, ecm_coef_all_df$ventana),
  file.path(out_tablas, "metD_ecm_coeficientes_por_ventana.xlsx")
)

write_xlsx(
  split(lags_all_df, lags_all_df$ventana),
  file.path(out_tablas, "metD_lags_seleccionados_por_ventana.xlsx")
)

write_xlsx(
  list(metricas_validacion = metricas_all_df),
  file.path(out_tablas, "metD_metricas_ajuste_ventana1.xlsx")
)

message("\nListo. Outputs en: ", out_dir)
message(sprintf("  %d filas en el panel de precios (2 ventanas)", nrow(prices_all_windows_df)))