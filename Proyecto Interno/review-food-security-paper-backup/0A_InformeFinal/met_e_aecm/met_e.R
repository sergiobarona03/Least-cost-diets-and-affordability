########################################################
## Metodología E: ECM M-TAR asimétrico (A-ECM)                 ##
##                                                              ##
## Pipeline por serie ciudad-artículo-alimento:                  ##
##   1) Relación de largo plazo: ln(P_min) = α + β*ln(P_may) + μ ##
##   2) Prueba de cointegración Engle-Granger (3 tipos)            ##
##   3) TAR nivel + TAR momentum + M-TAR (búsqueda de τ por AIC    ##
##      + Ljung-Box) sobre el residuo μ̂_t                          ##
##   4) ECM asimétrico univariado:                                   ##
##        Δln(P_min_t) = c + Σβ_j*Δln(P_min_{t-j})                  ##
##                         + Σγ_j*Δln(P_may_{t-j})                   ##
##                         + β+*μ+_{t-1} + β-*μ-_{t-1}               ##
##                         + dummies mensuales + u_t                   ##
##   5) Predicción dinámica/recursiva (μ̂ se actualiza con            ##
##      log_ipc_hat acumulado, igual que en D)                         ##
##   6) Banda IC 95% con varianza acumulada (igual que en D)           ##
##                                                              ##
## DOS VENTANAS (mismos cortes que A1/A2/B/C/D):                   ##
##   1) Validación:  entrena <= 2015-12 | eval 2016-01 a 2018-03     ##
##      (EVAL_START=2016-01 para evitar hueco en cadena recursiva)    ##
##   2) Pronóstico:  entrena <= 2018-03 | eval 2018-04 a 2024-12       ##
##                                                              ##
## Output:                                                       ##
##   - output/metE_precios_dos_ventanas.rds                          ##
##   - output/tablas/metE_coint_longrun_por_ventana.xlsx               ##
##   - output/tablas/metE_mtar_resultados_por_ventana.xlsx              ##
##   - output/tablas/metE_aecm_coeficientes_por_ventana.xlsx             ##
##   - output/tablas/metE_metricas_ajuste_ventana1.xlsx                   ##
##   - output/figuras/<ventana>/<ciudad>.pdf (multi-página)                ##
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
# Paths (EDITAR AQUÍ)
# -----------------------------
base_dir <- "C:/Users/Portatil/Desktop/Least-cost-diets-and-affordability/Proyecto Interno/review-food-security-paper/0A_InformeFinal/"

in_merged <- file.path(base_dir, "input/121225_dataset_ipc_sipsa.xlsx")

out_dir     <- file.path(base_dir, "met_e_aecm/output")
out_figuras <- file.path(out_dir, "figuras")
out_tablas  <- file.path(out_dir, "tablas")

dir.create(out_dir,     showWarnings = FALSE, recursive = TRUE)
dir.create(out_figuras, showWarnings = FALSE, recursive = TRUE)
dir.create(out_tablas,  showWarnings = FALSE, recursive = TRUE)

source(file.path(base_dir, "met_e_aecm/aux_functions/aux_functions_metE.R"))

# -----------------------------
# Parámetros
# -----------------------------
P_LAGS   <- 1      # rezagos en el A-ECM (p_lags = 1, como en Enders-Siklos)
TRIM     <- 0.15   # trimming para búsqueda de tau
ALPHA_Q  <- 0.05   # umbral p-valor Ljung-Box para selección de tau

# -----------------------------
# Mapping hardcodeado (mismo que D)
# -----------------------------
cali.mapping <- list(
  "ARROZ PARA SECO"   = c("Arroz de primera"),
  "CEBOLLA CABEZONA"  = c("Cebolla cabezona blanca bogotana"),
  "PAPA"              = c("Papa capira","Papa parda pastusa","Papa suprema","Papa única"),
  "PLÁTANO"           = c("Plátano hartón verde"),
  "TOMATE"            = c("Tomate larga vida"),
  "YUCA"              = c("Yuca ICA"),
  "ZANAHORIA"         = c("Zanahoria bogotana")
)
bogota.mapping <- list(
  "ARROZ PARA SECO"   = c("Arroz de primera"),
  "CEBOLLA CABEZONA"  = c("Cebolla cabezona blanca"),
  "PAPA"              = c("Papa R-12 negra","Papa parda pastusa","Papa suprema","Papa única"),
  "PLÁTANO"           = c("Plátano guineo","Plátano hartón verde","Plátano hartón verde llanero"),
  "TOMATE"            = c("Tomate chonto","Tomate larga vida"),
  "YUCA"              = c("Yuca llanera"),
  "ZANAHORIA"         = c("Zanahoria")
)
medellin.mapping <- list(
  "ARROZ PARA SECO"   = c("Arroz de primera"),
  "CEBOLLA CABEZONA"  = c("Cebolla cabezona blanca"),
  "PAPA"              = c("Papa R-12 negra","Papa capira","Papa nevada"),
  "PLÁTANO"           = c("Plátano guineo","Plátano hartón maduro","Plátano hartón verde"),
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
) %>% mutate(across(c(articulo_ipc, alimento_sipsa), str_squish))

CITIES_USE <- c("CALI", "BOGOTA D.C.", "MEDELLIN")

# -----------------------------
# Ventanas (EVAL_START=2016-01 en ventana 1 para evitar hueco recursivo)
# -----------------------------
ventanas <- list(
  list(nombre = "ventana1_validacion", TRAIN_END = as.Date("2015-12-01"),
       EVAL_START = as.Date("2016-01-01"), EVAL_END = as.Date("2018-03-01"),
       tipo = "validacion"),
  list(nombre = "ventana2_pronostico", TRAIN_END = as.Date("2018-03-01"),
       EVAL_START = as.Date("2018-04-01"), EVAL_END = as.Date("2024-12-01"),
       tipo = "pronostico")
)

# -----------------------------
# Helpers
# -----------------------------
safe_name <- function(x) {
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  x <- gsub("[^A-Za-z0-9_]+", "_", x); x <- gsub("_+", "_", x)
  x <- gsub("^_|_$", "", x); x
}
strip_accents <- function(x) iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT")

# -----------------------------
# 1. Cargar datos
# -----------------------------
message("Cargando panel IPC-SIPSA unido...")
data_raw <- read_excel(in_merged) %>%
  mutate(
    ciudad         = strip_accents(as.character(ciudad)),
    articulo_ipc   = str_squish(as.character(articulo_ipc)),
    alimento_sipsa = str_squish(as.character(alimento_sipsa)),
    fecha          = as.Date(paste(Year, Month, "01", sep = "-"))
  ) %>%
  filter(!is.na(precio_sipsa), precio_sipsa > 0,
         is.na(precio_ipc) | precio_ipc > 0)

data_merged <- data_raw %>%
  semi_join(map_df, by = c("ciudad", "articulo_ipc", "alimento_sipsa")) %>%
  group_by(ciudad, fecha, articulo_ipc, alimento_sipsa) %>%
  summarise(
    precio_ipc   = mean(precio_ipc,   na.rm = TRUE),
    precio_sipsa = mean(precio_sipsa, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    precio_ipc   = if_else(is.nan(precio_ipc),   NA_real_, precio_ipc),
    precio_sipsa = if_else(is.nan(precio_sipsa), NA_real_, precio_sipsa),
    log_ipc      = if_else(is.na(precio_ipc), NA_real_, log(precio_ipc)),
    log_sipsa    = log(precio_sipsa),
    mes          = factor(month(fecha), levels = 1:12, labels = month.abb)
  ) %>%
  filter(!is.na(precio_sipsa)) %>%
  arrange(ciudad, articulo_ipc, alimento_sipsa, fecha)

message(sprintf("  %d filas | %d ciudades | %d pares articulo-alimento",
                nrow(data_merged), n_distinct(data_merged$ciudad),
                n_distinct(paste(data_merged$articulo_ipc, data_merged$alimento_sipsa))))

# -----------------------------
# Acumuladores globales
# -----------------------------
coint_all    <- list()
lr_all       <- list()
mtar_all     <- list()
aecm_coef_all <- list()
metricas_all <- list()
prices_all_windows <- list()

# -----------------------------
# 2. Loop por ventana
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
  
  coint_long <- tibble()
  lr_long    <- tibble()
  mtar_long  <- tibble()
  aecm_long  <- tibble()
  
  aecm_fits  <- list()
  lr_models  <- list()
  mtar_fits  <- list()
  
  # -----------------------------------------------------------------
  # 2.1 Estimación: cointegración + largo plazo + M-TAR + A-ECM
  # -----------------------------------------------------------------
  message("  Estimando cointegración, M-TAR y A-ECM...")
  
  for (i in seq_len(nrow(keys))) {
    
    city_i <- keys$ciudad[i]; art_i <- keys$articulo_ipc[i]; ali_i <- keys$alimento_sipsa[i]
    key_id <- paste(city_i, art_i, ali_i, sep = "||")
    
    data.food <- data_train %>%
      filter(ciudad == city_i, articulo_ipc == art_i, alimento_sipsa == ali_i) %>%
      drop_na(log_ipc, log_sipsa, mes) %>%
      arrange(fecha)
    
    if (nrow(data.food) < 36) next
    
    # ---- Cointegración ----
    ct <- tryCatch(aTSA::coint.test(y = data.food$log_ipc, X = data.food$log_sipsa),
                   error = function(e) NULL)
    if (!is.null(ct)) {
      m_ct <- as.matrix(ct)
      coint_long <- bind_rows(coint_long, tibble(
        ciudad = city_i, articulo_ipc = art_i, alimento_sipsa = ali_i,
        p_type1 = as.numeric(m_ct[1,3]),
        p_type2 = as.numeric(m_ct[2,3]),
        p_type3 = as.numeric(m_ct[3,3])
      ))
    }
    
    # ---- Relación de largo plazo ----
    lr_model <- lm(log_ipc ~ log_sipsa, data = data.food)
    mu_hat   <- resid(lr_model)
    cs_lr    <- summary(lr_model)$coefficients
    
    if ("log_sipsa" %in% rownames(cs_lr)) {
      lr_long <- bind_rows(lr_long, tibble(
        ciudad = city_i, articulo_ipc = art_i, alimento_sipsa = ali_i,
        alpha = cs_lr["(Intercept)", "Estimate"],
        beta  = cs_lr["log_sipsa",   "Estimate"],
        se    = cs_lr["log_sipsa",   "Std. Error"],
        p_value = cs_lr["log_sipsa", "Pr(>|t|)"]
      ))
    }
    
    # ---- M-TAR sobre μ̂_t (p=0 y p=2, igual que en ecm_es_nonlinear_run.R) ----
    mtar_p0 <- tryCatch(search_tau_AIC_LB(mu_hat, p = 0, trim = TRIM, alpha_Q = ALPHA_Q),
                        error = function(e) NULL)
    mtar_p2 <- tryCatch(search_tau_AIC_LB(mu_hat, p = 2, trim = TRIM, alpha_Q = ALPHA_Q),
                        error = function(e) NULL)
    
    # Seleccionar el mejor M-TAR por AIC entre p=0 y p=2
    best_mtar <- NULL
    if (!is.null(mtar_p0) && !is.null(mtar_p2)) {
      best_mtar <- if (mtar_p0$AIC <= mtar_p2$AIC) mtar_p0 else mtar_p2
    } else if (!is.null(mtar_p0)) {
      best_mtar <- mtar_p0
    } else if (!is.null(mtar_p2)) {
      best_mtar <- mtar_p2
    }
    if (is.null(best_mtar)) next
    
    tau_x <- best_mtar$tau
    
    mtar_long <- bind_rows(mtar_long, tibble(
      ciudad = city_i, articulo_ipc = art_i, alimento_sipsa = ali_i,
      tau = tau_x, p_mtar = best_mtar$p,
      rho1 = best_mtar$rho1, t_rho1 = best_mtar$t_rho1,
      rho2 = best_mtar$rho2, t_rho2 = best_mtar$t_rho2,
      Phi_mu = best_mtar$Phi_mu,
      F_equal = best_mtar$F_equal, p_equal = best_mtar$p_equal,
      AIC_mtar = best_mtar$AIC, BIC_mtar = best_mtar$BIC
    ))
    
    # ---- A-ECM asimétrico univariado ----
    df_ecm <- tryCatch(
      build_ecm_mtar_prices_df(
        mu_hat    = mu_hat,
        log_ipc   = data.food$log_ipc,
        log_sipsa = data.food$log_sipsa,
        mes       = data.food$mes,
        tau_x     = tau_x,
        p_lags    = P_LAGS
      ),
      error = function(e) NULL
    )
    if (is.null(df_ecm) || nrow(df_ecm) < 24) next
    
    aecm_res <- estimate_ecm_mtar_prices(df_ecm, p_lags = P_LAGS)
    if (is.null(aecm_res)) next
    
    aecm_long <- bind_rows(aecm_long, tibble(
      ciudad = city_i, articulo_ipc = art_i, alimento_sipsa = ali_i,
      tau = tau_x,
      beta_plus = aecm_res$beta_plus, t_plus = aecm_res$t_plus,
      beta_minus = aecm_res$beta_minus, t_minus = aecm_res$t_minus,
      F_sym = aecm_res$F_sym, p_sym = aecm_res$p_sym,
      r2 = aecm_res$r2, n_obs = aecm_res$n_obs
    ))
    
    aecm_fits[[key_id]] <- aecm_res
    lr_models[[key_id]] <- lr_model
    mtar_fits[[key_id]] <- best_mtar
  }
  
  message(sprintf("    %d series con A-ECM ajustado (de %d candidatas)", length(aecm_fits), nrow(keys)))
  
  coint_all[[vent$nombre]]     <- coint_long %>% mutate(ventana = vent$nombre)
  lr_all[[vent$nombre]]        <- lr_long    %>% mutate(ventana = vent$nombre)
  mtar_all[[vent$nombre]]      <- mtar_long  %>% mutate(ventana = vent$nombre)
  aecm_coef_all[[vent$nombre]] <- aecm_long  %>% mutate(ventana = vent$nombre)
  
  # -----------------------------------------------------------------
  # 2.2 Predicción dinámica/recursiva
  # -----------------------------------------------------------------
  message("  Construyendo predicciones dinámicas (recursivas)...")
  
  data_eval_window <- data_merged %>%
    filter(fecha >= vent$EVAL_START, fecha <= vent$EVAL_END)
  
  pred_list <- list()
  
  for (i in seq_len(nrow(keys))) {
    
    city_i <- keys$ciudad[i]; art_i <- keys$articulo_ipc[i]; ali_i <- keys$alimento_sipsa[i]
    key_id <- paste(city_i, art_i, ali_i, sep = "||")
    
    if (is.null(aecm_fits[[key_id]])) next
    
    df_history <- data_train %>%
      filter(ciudad == city_i, articulo_ipc == art_i, alimento_sipsa == ali_i) %>%
      drop_na(log_ipc, log_sipsa, mes) %>%
      arrange(fecha)
    
    df_eval <- data_eval_window %>%
      filter(ciudad == city_i, articulo_ipc == art_i, alimento_sipsa == ali_i) %>%
      arrange(fecha)
    
    if (nrow(df_eval) == 0) next
    
    pred_out <- tryCatch(
      predict_aecm_dynamic(
        aecm_fit   = aecm_fits[[key_id]],
        lr_model   = lr_models[[key_id]],
        mtar_fit   = mtar_fits[[key_id]],
        df_history = df_history,
        df_eval    = df_eval,
        p_lags     = P_LAGS
      ),
      error = function(e) NULL
    )
    if (is.null(pred_out)) next
    
    pred_list[[length(pred_list) + 1]] <- pred_out %>%
      mutate(ciudad = city_i, articulo_ipc = art_i, alimento_sipsa = ali_i)
  }
  
  data_pred <- bind_rows(pred_list) %>%
    mutate(ventana = vent$nombre, tipo_ventana = vent$tipo) %>%
    arrange(ciudad, articulo_ipc, alimento_sipsa, fecha)
  
  message(sprintf("    %d filas en el panel de predicción de esta ventana", nrow(data_pred)))
  prices_all_windows[[vent$nombre]] <- data_pred
  
  # -----------------------------------------------------------------
  # 2.3 Métricas (solo ventana de validación)
  # -----------------------------------------------------------------
  if (vent$tipo == "validacion" && nrow(data_pred) > 0) {
    message("  Calculando métricas...")
    data_metr <- data_pred %>%
      filter(!is.na(price_obs), !is.na(price_pred)) %>%
      mutate(error = price_pred - price_obs, ape = abs(error) / price_obs * 100)
    
    resumen_global <- data_metr %>%
      summarise(n_obs = n(), mape = mean(ape, na.rm=TRUE),
                mae = mean(abs(error), na.rm=TRUE),
                rmse = sqrt(mean(error^2, na.rm=TRUE)),
                me = mean(error, na.rm=TRUE)) %>%
      mutate(ciudad = "TODAS", ventana = vent$nombre)
    
    resumen_ciudad <- data_metr %>%
      group_by(ciudad) %>%
      summarise(n_obs = n(), mape = mean(ape, na.rm=TRUE),
                mae = mean(abs(error), na.rm=TRUE),
                rmse = sqrt(mean(error^2, na.rm=TRUE)),
                me = mean(error, na.rm=TRUE), .groups = "drop") %>%
      mutate(ventana = vent$nombre)
    
    metricas_all[[vent$nombre]] <- bind_rows(resumen_ciudad, resumen_global)
    message(sprintf("    MAPE global: %.2f%%", resumen_global$mape))
    print(resumen_ciudad)
  }
  
  # -----------------------------------------------------------------
  # 2.4 Figuras: histórico completo + predicción + banda IC95%
  #     + nota Engle-Granger (3 tipos) en el subtítulo
  # -----------------------------------------------------------------
  message("  Generando figuras...")
  
  plots_by_city <- setNames(vector("list", length(CITIES_USE)), CITIES_USE)
  
  items_in_window <- data_pred %>%
    filter(!is.na(price_pred)) %>%
    distinct(ciudad, articulo_ipc, alimento_sipsa) %>%
    arrange(ciudad, articulo_ipc, alimento_sipsa)
  
  n_omitidos <- data_pred %>% distinct(ciudad, articulo_ipc, alimento_sipsa) %>%
    nrow() - nrow(items_in_window)
  if (n_omitidos > 0)
    message(sprintf("    %d series omitidas (sin A-ECM estimable / sin predicción)", n_omitidos))
  
  for (i in seq_len(nrow(items_in_window))) {
    
    city_name <- items_in_window$ciudad[i]
    art_name  <- items_in_window$articulo_ipc[i]
    food_name <- items_in_window$alimento_sipsa[i]
    
    df_plot <- data_pred %>%
      filter(ciudad == city_name, articulo_ipc == art_name, alimento_sipsa == food_name)
    if (all(is.na(df_plot$price_pred))) next
    
    df_obs_full <- data_merged %>%
      filter(ciudad == city_name, articulo_ipc == art_name, alimento_sipsa == food_name) %>%
      select(fecha, precio_ipc)
    
    # Nota de cointegración (tres tipos)
    coint_row <- coint_long %>%
      filter(ciudad == city_name, articulo_ipc == art_name, alimento_sipsa == food_name)
    
    coint_label <- if (nrow(coint_row) > 0) {
      p1 <- coint_row$p_type1[1]; p2 <- coint_row$p_type2[1]; p3 <- coint_row$p_type3[1]
      fmt_p   <- function(p) if (is.na(p)) "NA" else sprintf("%.3f", p)
      verdict <- function(p) if (is.na(p)) "?" else if (p < 0.05) "coint." else "no coint."
      sprintf("EG — Tipo 1 (sin cte/tend): p=%s (%s) | Tipo 2 (con cte): p=%s (%s) | Tipo 3 (cte+tend): p=%s (%s)",
              fmt_p(p1), verdict(p1), fmt_p(p2), verdict(p2), fmt_p(p3), verdict(p3))
    } else "Cointegración: prueba no disponible"
    
    # Parámetros A-ECM para el subtítulo
    aecm_row <- aecm_long %>%
      filter(ciudad == city_name, articulo_ipc == art_name, alimento_sipsa == food_name)
    aecm_label <- if (nrow(aecm_row) > 0) {
      sprintf("τ=%.4f | β⁺=%.3f (t=%.2f) | β⁻=%.3f (t=%.2f) | p_sym=%.3f",
              aecm_row$tau[1], aecm_row$beta_plus[1], aecm_row$t_plus[1],
              aecm_row$beta_minus[1], aecm_row$t_minus[1], aecm_row$p_sym[1])
    } else ""
    
    p <- ggplot() +
      geom_ribbon(data = df_plot,
                  aes(x = fecha, ymin = price_pred_lwr, ymax = price_pred_upr),
                  fill = "red", alpha = 0.15) +
      geom_line(data = df_obs_full,
                aes(x = fecha, y = precio_ipc, color = "Precio observado (IPC)"),
                linewidth = 0.35) +
      geom_line(data = df_plot,
                aes(x = fecha, y = price_pred, color = "Precio estimado (A-ECM)"),
                linewidth = 0.4, linetype = "dashed") +
      scale_color_manual(values = c(
        "Precio observado (IPC)"  = "black",
        "Precio estimado (A-ECM)" = "red"
      )) +
      labs(
        title    = paste0("Metodología E — ", vent$nombre),
        subtitle = paste0(city_name, " — ", art_name, " (", food_name, ")",
                          " | predicción dinámica/recursiva | banda: IC 95%\n",
                          str_wrap(aecm_label, width = 90), "\n",
                          str_wrap(coint_label, width = 90)),
        x = NULL, y = "Precio", color = ""
      ) +
      theme_bw(base_size = 9.5) +
      theme(legend.position = "bottom")
    
    plots_by_city[[city_name]][[length(plots_by_city[[city_name]]) + 1]] <- p
  }
  
  for (cn in names(plots_by_city)) {
    if (length(plots_by_city[[cn]]) == 0) next
    pdf_path <- file.path(vent_dir, paste0(safe_name(cn), ".pdf"))
    pdf(pdf_path, width = 10, height = 5, onefile = TRUE)
    for (pl in plots_by_city[[cn]]) print(pl)
    dev.off()
    message("    PDF guardado: ", pdf_path, " (", length(plots_by_city[[cn]]), " páginas)")
  }
}

# -----------------------------
# 3. Guardar outputs
# -----------------------------
prices_all_windows_df <- bind_rows(prices_all_windows)

saveRDS(prices_all_windows_df, file.path(out_dir, "metE_precios_dos_ventanas.rds"))

write_xlsx(
  list(cointegracion = bind_rows(coint_all), largo_plazo = bind_rows(lr_all)),
  file.path(out_tablas, "metE_coint_longrun_por_ventana.xlsx")
)
write_xlsx(
  split(bind_rows(mtar_all), bind_rows(mtar_all)$ventana),
  file.path(out_tablas, "metE_mtar_resultados_por_ventana.xlsx")
)
write_xlsx(
  split(bind_rows(aecm_coef_all), bind_rows(aecm_coef_all)$ventana),
  file.path(out_tablas, "metE_aecm_coeficientes_por_ventana.xlsx")
)
write_xlsx(
  list(metricas_validacion = bind_rows(metricas_all)),
  file.path(out_tablas, "metE_metricas_ajuste_ventana1.xlsx")
)

message("\nListo. Outputs en: ", out_dir)
message(sprintf("  %d filas en el panel de precios (2 ventanas)", nrow(prices_all_windows_df)))
