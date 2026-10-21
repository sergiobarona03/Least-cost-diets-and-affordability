########################################################
## Metodología C: Estimación econométrica del pass-through    ##
## mayorista-minorista (SIPSA -> IPC)                          ##
##                                                              ##
## C1 (niveles):                                                 ##
##   ln(P_min_it) = alpha_i + beta_i*ln(P_may_it)                 ##
##                  + sum_m delta_im * S_mt + e_it                 ##
##                                                              ##
## C2 (diferencias):                                               ##
##   Dln(P_min_it) = alpha_i + beta_i*Dln(P_may_it)                 ##
##                   + sum_m delta_im * S_mt + e_it                  ##
##                                                              ##
## APLICACIÓN ESTÁTICA (sin acumulación, sin ancla):              ##
##   C1: P_hat_min_t = exp(alpha_i + beta_i*ln(P_may_t) + dummies)  ##
##   C2: P_hat_min_t = exp(alpha_i + beta_i*Dln(P_may_t) + dummies)  ##
##       * P_may_t  (reconstrucción del nivel desde el mayorista      ##
##       contemporáneo, NO desde el minorista del mes anterior)        ##
##                                                              ##
## DOS VENTANAS (mismos cortes que A1/A2/B):                       ##
##   1) Validación:  entrena <= 2015-12 | eval 2016-02 a 2018-03     ##
##   2) Pronóstico:  entrena <= 2018-03 | eval 2018-04 a 2024-12       ##
##                                                              ##
## Output:                                                       ##
##   - output/metC_precios_dos_ventanas.rds (panel completo)       ##
##   - output/tablas/metC_coeficientes_por_ventana.xlsx               ##
##   - output/tablas/metC_metricas_ajuste_ventana1.xlsx                ##
##   - output/figuras/<ventana>/<ciudad>.pdf (multi-página)             ##
########################################################

# -----------------------------
# Packages
# -----------------------------
library(tidyverse)
library(readxl)
library(lubridate)
library(writexl)
library(broom)

# -----------------------------
# Paths (EDITAR AQUÍ — sin 00_config.R)
# -----------------------------
base_dir <- "C:/Users/Portatil/Desktop/Least-cost-diets-and-affordability/Proyecto Interno/review-food-security-paper/0A_InformeFinal/"

in_merged <- file.path(base_dir, "input/121225_dataset_ipc_sipsa.xlsx")

out_dir     <- file.path(base_dir, "met_c_passthrough/output")
out_figuras <- file.path(out_dir, "figuras")
out_tablas  <- file.path(out_dir, "tablas")

dir.create(out_dir,     showWarnings = FALSE, recursive = TRUE)
dir.create(out_figuras, showWarnings = FALSE, recursive = TRUE)
dir.create(out_tablas,  showWarnings = FALSE, recursive = TRUE)

# -----------------------------
# Parámetro mínimo de observaciones para estimar la regresión
# -----------------------------
MIN_OBS_REG <- 24   # con dummies de 11 meses + alpha + beta, se necesita un mínimo razonable

# -----------------------------
# Definición de las dos ventanas (mismos cortes que A1/A2/B)
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
  ) %>%
  filter(!is.na(precio_ipc), !is.na(precio_sipsa), precio_ipc > 0, precio_sipsa > 0) %>%
  arrange(ciudad, alimento_sipsa, fecha) %>%
  group_by(ciudad, alimento_sipsa) %>%
  mutate(
    log_ipc    = log(precio_ipc),
    log_sipsa  = log(precio_sipsa),
    dlog_ipc   = log_ipc   - lag(log_ipc),
    dlog_sipsa = log_sipsa - lag(log_sipsa),
    mes        = factor(month(fecha), levels = 1:12)
  ) %>%
  ungroup()

message(sprintf("  %d filas | %d ciudades | %d alimentos SIPSA",
                nrow(data_merged), n_distinct(data_merged$ciudad), n_distinct(data_merged$alimento_sipsa)))

CITIES_USE <- sort(unique(data_merged$ciudad))
message("  Ciudades detectadas: ", paste(CITIES_USE, collapse = ", "))

# -----------------------------
# Acumuladores globales
# -----------------------------
coef_all     <- list()
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
  
  # -----------------------------------------------------------------
  # 2.1 Estimar C1 (niveles) y C2 (diferencias) por ciudad x alimento,
  #     SOLO con datos <= TRAIN_END
  # -----------------------------------------------------------------
  message("  Estimando regresiones C1 (niveles) y C2 (diferencias)...")
  
  data_train <- data_merged %>% filter(fecha <= vent$TRAIN_END)
  
  keys <- data_train %>% distinct(ciudad, alimento_sipsa) %>% arrange(ciudad, alimento_sipsa)
  
  coef_list <- list()
  models_c1 <- list()
  models_c2 <- list()
  
  for (i in seq_len(nrow(keys))) {
    
    city_name <- keys$ciudad[i]
    food_name <- keys$alimento_sipsa[i]
    
    df_item <- data_train %>% filter(ciudad == city_name, alimento_sipsa == food_name)
    
    # ---- C1: niveles ----
    df_c1 <- df_item %>% drop_na(log_ipc, log_sipsa, mes)
    c1_ok <- nrow(df_c1) >= MIN_OBS_REG && n_distinct(df_c1$mes) > 1
    
    if (c1_ok) {
      m1 <- tryCatch(lm(log_ipc ~ log_sipsa + mes, data = df_c1), error = function(e) NULL)
    } else {
      m1 <- NULL
    }
    
    # ---- C2: diferencias ----
    df_c2 <- df_item %>% drop_na(dlog_ipc, dlog_sipsa, mes)
    c2_ok <- nrow(df_c2) >= MIN_OBS_REG && n_distinct(df_c2$mes) > 1
    
    if (c2_ok) {
      m2 <- tryCatch(lm(dlog_ipc ~ dlog_sipsa + mes, data = df_c2), error = function(e) NULL)
    } else {
      m2 <- NULL
    }
    
    coef_list[[length(coef_list) + 1]] <- tibble(
      ciudad = city_name, alimento_sipsa = food_name,
      modelo = c("C1_niveles", "C2_diferencias"),
      est_ok = c(!is.null(m1), !is.null(m2)),
      n_obs  = c(if (!is.null(m1)) nrow(df_c1) else NA_integer_,
                 if (!is.null(m2)) nrow(df_c2) else NA_integer_),
      alpha  = c(if (!is.null(m1)) coef(m1)[["(Intercept)"]] else NA_real_,
                 if (!is.null(m2)) coef(m2)[["(Intercept)"]] else NA_real_),
      beta   = c(if (!is.null(m1)) coef(m1)[["log_sipsa"]]  else NA_real_,
                 if (!is.null(m2)) coef(m2)[["dlog_sipsa"]]  else NA_real_),
      r2     = c(if (!is.null(m1)) summary(m1)$r.squared else NA_real_,
                 if (!is.null(m2)) summary(m2)$r.squared else NA_real_)
    )
    
    # Guardamos los modelos en listas indexadas por ciudad||alimento, para usarlos en la predicción
    key_id <- paste(city_name, food_name, sep = "||")
    if (!is.null(m1)) models_c1[[key_id]] <- m1
    if (!is.null(m2)) models_c2[[key_id]] <- m2
  }
  
  coef_df <- bind_rows(coef_list)
  
  message(sprintf("    %d series | C1 estimadas: %d | C2 estimadas: %d",
                  nrow(keys),
                  sum(coef_df$modelo == "C1_niveles"     & coef_df$est_ok),
                  sum(coef_df$modelo == "C2_diferencias" & coef_df$est_ok)))
  
  coef_all[[vent$nombre]] <- coef_df %>% mutate(ventana = vent$nombre)
  
  # -----------------------------------------------------------------
  # 2.2 Predicción ESTÁTICA sobre el periodo de evaluación de esta ventana
  #     C1: P_hat = exp(alpha + beta*log_sipsa_t + dummy_mes_t)
  #     C2: P_hat = exp(alpha + beta*dlog_sipsa_t + dummy_mes_t) * P_sipsa_t
  #         (reconstrucción estática: ancla al mayorista CONTEMPORÁNEO,
  #          no al minorista del mes anterior — evita acumulación de error)
  # -----------------------------------------------------------------
  message("  Construyendo predicciones estáticas...")
  
  data_eval_window <- data_merged %>%
    filter(fecha >= vent$EVAL_START, fecha <= vent$EVAL_END)
  
  pred_list <- list()
  
  for (i in seq_len(nrow(keys))) {
    
    city_name <- keys$ciudad[i]
    food_name <- keys$alimento_sipsa[i]
    key_id    <- paste(city_name, food_name, sep = "||")
    
    df_eval <- data_eval_window %>% filter(ciudad == city_name, alimento_sipsa == food_name)
    if (nrow(df_eval) == 0) next
    
    m1 <- models_c1[[key_id]]
    m2 <- models_c2[[key_id]]
    
    df_eval <- df_eval %>%
      mutate(
        pred_c1     = NA_real_,
        pred_c1_lwr = NA_real_,
        pred_c1_upr = NA_real_,
        pred_c2     = NA_real_,
        pred_c2_lwr = NA_real_,
        pred_c2_upr = NA_real_
      )
    
    if (!is.null(m1)) {
      pi1 <- suppressWarnings(
        tryCatch(predict(m1, newdata = df_eval, interval = "prediction", level = 0.95),
                 error = function(e) NULL)
      )
      if (!is.null(pi1)) {
        df_eval$pred_c1     <- exp(pi1[, "fit"])
        df_eval$pred_c1_lwr <- exp(pi1[, "lwr"])
        df_eval$pred_c1_upr <- exp(pi1[, "upr"])
      }
    }
    
    if (!is.null(m2)) {
      pi2 <- suppressWarnings(
        tryCatch(predict(m2, newdata = df_eval, interval = "prediction", level = 0.95),
                 error = function(e) NULL)
      )
      if (!is.null(pi2)) {
        # Reconstrucción estática: el crecimiento predicho (y su intervalo) se
        # aplica multiplicativamente sobre el mayorista contemporáneo
        df_eval$pred_c2     <- df_eval$precio_sipsa * exp(pi2[, "fit"])
        df_eval$pred_c2_lwr <- df_eval$precio_sipsa * exp(pi2[, "lwr"])
        df_eval$pred_c2_upr <- df_eval$precio_sipsa * exp(pi2[, "upr"])
      }
    }
    
    pred_list[[length(pred_list) + 1]] <- df_eval
  }
  
  data_pred <- bind_rows(pred_list) %>%
    mutate(ventana = vent$nombre, tipo_ventana = vent$tipo) %>%
    arrange(ciudad, alimento_sipsa, fecha)
  
  message(sprintf("    %d filas en el panel de predicción de esta ventana", nrow(data_pred)))
  
  prices_all_windows[[vent$nombre]] <- data_pred %>%
    select(ventana, tipo_ventana, ciudad, alimento_sipsa, fecha,
           precio_sipsa, precio_ipc_obs = precio_ipc,
           pred_c1, pred_c1_lwr, pred_c1_upr,
           pred_c2, pred_c2_lwr, pred_c2_upr)
  
  # -----------------------------------------------------------------
  # 2.3 Métricas de ajuste (SOLO ventana de validación)
  # -----------------------------------------------------------------
  if (vent$tipo == "validacion") {
    
    message("  Calculando métricas de ajuste contra IPC observado...")
    
    data_metr <- data_pred %>%
      filter(!is.na(precio_ipc)) %>%
      mutate(
        error_c1 = pred_c1 - precio_ipc,
        error_c2 = pred_c2 - precio_ipc,
        ape_c1   = abs(error_c1) / precio_ipc * 100,
        ape_c2   = abs(error_c2) / precio_ipc * 100
      )
    
    resumen_global <- data_metr %>%
      summarise(
        n_obs   = n(),
        mape_c1 = mean(ape_c1, na.rm = TRUE),
        mape_c2 = mean(ape_c2, na.rm = TRUE),
        mae_c1  = mean(abs(error_c1), na.rm = TRUE),
        mae_c2  = mean(abs(error_c2), na.rm = TRUE),
        rmse_c1 = sqrt(mean(error_c1^2, na.rm = TRUE)),
        rmse_c2 = sqrt(mean(error_c2^2, na.rm = TRUE)),
        me_c1   = mean(error_c1, na.rm = TRUE),
        me_c2   = mean(error_c2, na.rm = TRUE)
      ) %>%
      mutate(ciudad = "TODAS", ventana = vent$nombre)
    
    resumen_ciudad <- data_metr %>%
      group_by(ciudad) %>%
      summarise(
        n_obs   = n(),
        mape_c1 = mean(ape_c1, na.rm = TRUE),
        mape_c2 = mean(ape_c2, na.rm = TRUE),
        mae_c1  = mean(abs(error_c1), na.rm = TRUE),
        mae_c2  = mean(abs(error_c2), na.rm = TRUE),
        rmse_c1 = sqrt(mean(error_c1^2, na.rm = TRUE)),
        rmse_c2 = sqrt(mean(error_c2^2, na.rm = TRUE)),
        me_c1   = mean(error_c1, na.rm = TRUE),
        me_c2   = mean(error_c2, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(ventana = vent$nombre)
    
    metricas_all[[vent$nombre]] <- bind_rows(resumen_ciudad, resumen_global)
    
    message(sprintf("    MAPE C1 (niveles): %.2f%% | MAPE C2 (diferencias): %.2f%%",
                    resumen_global$mape_c1, resumen_global$mape_c2))
    print(resumen_ciudad)
  }
  
  # -----------------------------------------------------------------
  # 2.4 Figuras: observado vs. C1 vs. C2, un PDF multi-página por ciudad
  #     Se omiten combinaciones sin predicción válida (sin coincidencia mayorista
  #     o sin modelo estimable) para no generar páginas en blanco
  # -----------------------------------------------------------------
  message("  Generando figuras observado vs. estimado...")
  
  plots_by_city <- setNames(vector("list", length(CITIES_USE)), CITIES_USE)
  
  items_in_window <- data_pred %>%
    filter(!is.na(pred_c1) | !is.na(pred_c2)) %>%
    distinct(ciudad, alimento_sipsa) %>%
    arrange(ciudad, alimento_sipsa)
  
  n_omitidos <- data_pred %>% distinct(ciudad, alimento_sipsa) %>% nrow() - nrow(items_in_window)
  if (n_omitidos > 0) {
    message(sprintf("    %d combinaciones ciudad-alimento omitidas (sin modelo estimable / sin coincidencia mayorista)",
                    n_omitidos))
  }
  
  for (i in seq_len(nrow(items_in_window))) {
    
    city_name <- items_in_window$ciudad[i]
    food_name <- items_in_window$alimento_sipsa[i]
    
    df_plot <- data_pred %>% filter(ciudad == city_name, alimento_sipsa == food_name)
    if (all(is.na(df_plot$pred_c1)) && all(is.na(df_plot$pred_c2))) next
    
    # Histórico completo de precio observado (desde el inicio del panel, ej. 1999-01),
    # SOLO para referencia visual — no afecta estimación, predicción ni métricas.
    df_obs_full <- data_merged %>%
      filter(ciudad == city_name, alimento_sipsa == food_name, !is.na(precio_ipc)) %>%
      select(fecha, precio_ipc)
    
    p <- ggplot() +
      geom_ribbon(data = df_plot, aes(x = fecha, ymin = pred_c1_lwr, ymax = pred_c1_upr),
                  fill = "red", alpha = 0.12) +
      geom_ribbon(data = df_plot, aes(x = fecha, ymin = pred_c2_lwr, ymax = pred_c2_upr),
                  fill = "blue", alpha = 0.12) +
      geom_line(data = df_obs_full,
                aes(x = fecha, y = precio_ipc, color = "Precio observado (IPC)"),
                linewidth = 0.35) +
      geom_line(data = df_plot,
                aes(x = fecha, y = pred_c1, color = "C1 (niveles)"),
                linewidth = 0.4, linetype = "dashed") +
      geom_line(data = df_plot,
                aes(x = fecha, y = pred_c2, color = "C2 (diferencias)"),
                linewidth = 0.4, linetype = "dotdash") +
      scale_color_manual(values = c(
        "Precio observado (IPC)" = "black",
        "C1 (niveles)"           = "red",
        "C2 (diferencias)"       = "blue"
      )) +
      labs(
        title = paste0("Metodología C — ", vent$nombre),
        subtitle = paste0(city_name, " — ", food_name,
                          " | observado desde inicio del panel | aplicación estática | bandas: intervalo de predicción 95%"),
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
coef_all_df            <- bind_rows(coef_all)
metricas_all_df         <- bind_rows(metricas_all)

saveRDS(prices_all_windows_df, file.path(out_dir, "metC_precios_dos_ventanas.rds"))

write_xlsx(
  split(coef_all_df, coef_all_df$ventana),
  file.path(out_tablas, "metC_coeficientes_por_ventana.xlsx")
)

write_xlsx(
  list(metricas_validacion = metricas_all_df),
  file.path(out_tablas, "metC_metricas_ajuste_ventana1.xlsx")
)

message("\nListo. Outputs en: ", out_dir)
message(sprintf("  %d filas en el panel de precios (2 ventanas)", nrow(prices_all_windows_df)))