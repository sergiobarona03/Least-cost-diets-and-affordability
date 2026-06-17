########################################################
## Metodología A2: Estimación de precios minoristas         ##
## usando variación del IPC corregida por elasticidad λ      ##
## (OLS en diferencias log + shrinkage James-Stein +         ##
##  grid search de thresholds + bounds)                      ##
##                                                            ##
## p_hat_t = precio_anchor * (IPC_t / IPC_T0) ^ lambda_final  ##
##                                                            ##
## DOS VENTANAS:                                              ##
##   1) Validación:                                            ##
##        TRAIN_END = 2015-12 (estima lambda con 1999-2015)    ##
##        T0        = 2016-01 (ancla de precios)               ##
##        VAL_START = 2016-02 | VAL_END = 2018-03               ##
##        -> grid search propio, comparado contra obs. reales  ##
##   2) Pronóstico:                                             ##
##        TRAIN_END = 2018-03 (todo el histórico disponible)    ##
##        T0        = 2018-03                                   ##
##        FCST_START = 2018-04 | FCST_END = 2024-12              ##
##        -> sin verdad observada; reutiliza lambda_final        ##
##           y thresholds óptimos de la Ventana 1                ##
##                                                            ##
## Output:                                                     ##
##   - output/metA2_precios_dos_ventanas.rds (panel completo)   ##
##   - output/tablas/metA2_lambda_raw_<ventana>.xlsx              ##
##   - output/tablas/metA2_lambda_final_<ventana>.xlsx            ##
##   - output/tablas/metA2_grid_search_ventana1.xlsx               ##
##   - output/tablas/metA2_params_optimal_ventana1.xlsx             ##
##   - output/figuras/<ventana>/<ciudad>.pdf (multi-página)          ##
##   - output/figuras/<ventana>/fig_threshold_heatmap.png (solo V1)  ##
##   - output/figuras/<ventana>/fig_lambda_distribution.png           ##
##   - output/figuras/<ventana>/fig_shrinkage_effect.png               ##
########################################################

# -----------------------------
# Packages
# -----------------------------
library(tidyverse)
library(readxl)
library(janitor)
library(lubridate)
library(writexl)

# -----------------------------
# Paths (EDITAR AQUÍ — sin 00_config.R)
# -----------------------------
base_dir <- "C:/Users/Portatil/Desktop/Least-cost-diets-and-affordability/Proyecto Interno/review-food-security-paper/0A_InformeFinal/"

in_prices <- file.path(base_dir, "input/precios_unadj_DANE_1999_2018.xlsx")
in_ipc    <- file.path(base_dir, "input/IPC.xls")     # tramo 1
in_ipc2   <- file.path(base_dir, "input/IPC_2.xls")   # tramo 2
in_ipc3   <- file.path(base_dir, "input/IPC_3.xls")   # tramo 3
in_ipc4   <- file.path(base_dir, "input/IPC_4.xls")   # tramo 4
in_corr1  <- file.path(base_dir, "input/correlativa_ipc.xlsx")
in_corr2  <- file.path(base_dir, "input/correlativa_ipc_articulos.xlsx")

out_dir     <- file.path(base_dir, "met_a2_ipc_correction/output")
out_figuras <- file.path(out_dir, "figuras")
out_tablas  <- file.path(out_dir, "tablas")

dir.create(out_dir,     showWarnings = FALSE, recursive = TRUE)
dir.create(out_figuras, showWarnings = FALSE, recursive = TRUE)
dir.create(out_tablas,  showWarnings = FALSE, recursive = TRUE)

# -----------------------------
# Ciudades a procesar
# -----------------------------
CITIES_USE <- c("CALI", "BOGOTA D.C.", "MEDELLIN")

CITY_COLORS <- c("CALI" = "#27AE60", "BOGOTA D.C." = "#2C3E6B", "MEDELLIN" = "#C0392B")
CITY_LABELS <- c("CALI" = "Cali", "BOGOTA D.C." = "Bogotá", "MEDELLIN" = "Medellín")

# -----------------------------
# Parámetros mínimos de estimación
# -----------------------------
MIN_OBS <- 12   # mínimo de observaciones (en diferencias) para estimar lambda por OLS

# -----------------------------
# Definición de las dos ventanas
# -----------------------------
ventanas <- list(
  list(
    nombre     = "ventana1_validacion",
    TRAIN_END  = as.Date("2015-12-01"),
    T0         = as.Date("2016-01-01"),
    EVAL_START = as.Date("2016-02-01"),
    EVAL_END   = as.Date("2018-03-01"),
    tipo       = "validacion"     # corre grid search propio, compara contra obs. reales
  ),
  list(
    nombre     = "ventana2_pronostico",
    TRAIN_END  = as.Date("2018-03-01"),
    T0         = as.Date("2018-03-01"),
    EVAL_START = as.Date("2018-04-01"),
    EVAL_END   = as.Date("2024-12-01"),
    tipo       = "pronostico"     # sin verdad observada; reutiliza thresholds de ventana 1
  )
)

# -----------------------------
# Helpers generales
# -----------------------------
safe_name <- function(x) {
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  x <- gsub("[^A-Za-z0-9_]+", "_", x)
  x <- gsub("_+", "_", x)
  x <- gsub("^_|_$", "", x)
  x
}

strip_accents <- function(x) iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT")

meses_esp <- c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic")

make_date <- function(ano, mes_num) {
  as.Date(sprintf("%04d-%02d-01", as.integer(ano), as.integer(mes_num)))
}

mode1 <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_character_)
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

assign_subclase <- function(df_series, corr_subclase, corr_producto) {
  tmp1 <- df_series %>%
    left_join(corr_subclase, by = "cod_subclase") %>%
    mutate(subclase = as.character(subclase))
  sub1 <- mode1(tmp1$subclase)
  if (is.na(sub1) || length(unique(na.omit(tmp1$subclase))) > 1) {
    tmp2 <- df_series %>%
      left_join(corr_producto, by = "codigo_articulo") %>%
      mutate(subclase = as.character(subclase))
    return(list(subclase = mode1(tmp2$subclase), method = "producto"))
  }
  list(subclase = sub1, method = "gasto_basico")
}

# OLS en primeras diferencias logarítmicas: d_log_p ~ d_log_ipc - 1
estimate_lambda_fd <- function(obs_slice, ipc_series, min_obs = MIN_OBS) {
  if (nrow(obs_slice) < min_obs) return(NULL)
  merged <- obs_slice %>%
    select(fecha, precio_500g) %>%
    inner_join(ipc_series %>% select(fecha, ipc), by = "fecha") %>%
    arrange(fecha) %>%
    mutate(
      d_log_p   = log(precio_500g) - lag(log(precio_500g)),
      d_log_ipc = log(ipc)         - lag(log(ipc))
    ) %>%
    filter(!is.na(d_log_p), !is.na(d_log_ipc),
           is.finite(d_log_p), is.finite(d_log_ipc))
  if (nrow(merged) < min_obs) return(NULL)
  fit <- tryCatch(lm(d_log_p ~ d_log_ipc - 1, data = merged), error = function(e) NULL)
  if (is.null(fit)) return(NULL)
  list(
    lambda = coef(fit)[["d_log_ipc"]],
    sigma2 = sum(resid(fit)^2) / max(nrow(merged) - 1, 1),
    r2     = summary(fit)$r.squared,
    n_obs  = nrow(merged))
}

# -----------------------------
# 1. Cargar y limpiar datos (una sola vez, se reutiliza en ambas ventanas)
# -----------------------------
message("Cargando precios DANE...")
dane <- read_excel(in_prices) %>%
  mutate(
    fecha        = make_date(ano, mes_num),
    cod_subclase = paste0("0", substr(codigo_articulo, 1, 6), "0")
  ) %>%
  filter(!is.na(fecha), !is.na(precio_500g)) %>%
  mutate(nombre_ciudad = strip_accents(nombre_ciudad)) %>%
  filter(nombre_ciudad %in% CITIES_USE)

message(sprintf("  %d filas | %d artículos | %d ciudades",
                nrow(dane), n_distinct(dane$articulo), n_distinct(dane$nombre_ciudad)))

message("Cargando IPC (4 archivos, misma estructura, distintos tramos de fecha)...")
ipc <- map(list(in_ipc, in_ipc2, in_ipc3, in_ipc4),
           ~ read_excel(.x) %>% clean_names()) %>%
  bind_rows() %>%
  mutate(
    ciudad = strip_accents(ciudad),
    ciudad = case_when(
      ciudad == "CARTAGENA DE INDIAS" ~ "CARTAGENA",
      ciudad == "BOGOTA, D.C."        ~ "BOGOTA D.C.",
      TRUE ~ ciudad),
    cod_subclase = substr(subclase, 1, 8),
    mes_num      = match(mes, meses_esp),
    ipc          = as.numeric(numero_indice),
    fecha        = make_date(as.integer(ano), mes_num)
  ) %>%
  select(ciudad, cod_subclase, fecha, ipc) %>%
  filter(!is.na(fecha), !is.na(ipc), !is.na(cod_subclase), !is.na(ciudad),
         ciudad %in% CITIES_USE) %>%
  distinct(ciudad, cod_subclase, fecha, .keep_all = TRUE) %>%
  arrange(ciudad, cod_subclase, fecha)

message(sprintf("  %d filas IPC | %d subclases | %s a %s",
                nrow(ipc), n_distinct(ipc$cod_subclase), min(ipc$fecha), max(ipc$fecha)))

message("Cargando correlativas...")
corr_subclase <- read_excel(in_corr1) %>%
  fill(subclase, ipc, .direction = "down") %>%
  mutate(cod_subclase = paste0("0", gasto_basico, "00")) %>%
  select(cod_subclase, subclase) %>%
  mutate(across(everything(), as.character))

corr_producto <- read_excel(in_corr2) %>%
  rename(codigo_articulo = cod_dane, subclase = cod_ipc) %>%
  mutate(across(everything(), as.character))

# -----------------------------
# Acumuladores globales (across ventanas)
# -----------------------------
lambda_raw_all   <- list()
lambda_final_all <- list()
grid_results_all <- list()
params_optimal_all <- list()
prices_all_windows <- list()

# Guardamos el lambda_final y los thresholds óptimos de la ventana de validación
# para reutilizarlos en la ventana de pronóstico
lambda_final_v1   <- NULL
params_optimal_v1 <- NULL

# -----------------------------
# 2. Loop principal por ventana
# -----------------------------
for (v in seq_along(ventanas)) {
  
  vent <- ventanas[[v]]
  message("\n=== Procesando ", vent$nombre, " | TRAIN_END: ", vent$TRAIN_END,
          " | T0: ", vent$T0, " | eval: ", vent$EVAL_START, " a ", vent$EVAL_END, " ===")
  
  vent_dir <- file.path(out_figuras, vent$nombre)
  dir.create(vent_dir, recursive = TRUE, showWarnings = FALSE)
  
  # -----------------------------------------------------------------
  # 2.1 Asignación de subclase (usando solo datos <= TRAIN_END)
  # -----------------------------------------------------------------
  message("  Asignando subclases...")
  item_subclass_map <- dane %>%
    filter(fecha <= vent$TRAIN_END) %>%
    mutate(codigo_articulo = as.character(codigo_articulo),
           cod_subclase    = as.character(cod_subclase)) %>%
    group_by(nombre_ciudad, articulo) %>%
    group_modify(~ {
      sub_info <- assign_subclase(.x, corr_subclase, corr_producto)
      tibble(subclase_ipc = sub_info$subclase)
    }) %>%
    ungroup() %>%
    filter(!is.na(subclase_ipc))
  
  subclass_sizes <- item_subclass_map %>%
    group_by(nombre_ciudad, subclase_ipc) %>%
    summarise(k = n_distinct(articulo), .groups = "drop")
  
  anchor_prices <- dane %>%
    filter(fecha == vent$T0) %>%
    select(ciudad = nombre_ciudad, articulo, precio_anchor = precio_500g)
  
  valid_items <- anchor_prices %>% distinct(ciudad, articulo)
  
  message(sprintf("    %d item x ciudad series con ancla en %s", nrow(valid_items), vent$T0))
  
  # -----------------------------------------------------------------
  # 2.2 Estimar lambdas OLS (solo si hay ancla; se reutiliza si no aplica)
  # -----------------------------------------------------------------
  message("  Estimando lambdas OLS...")
  all_lambda <- list()
  
  for (i in seq_len(nrow(valid_items))) {
    
    city_name <- valid_items$ciudad[i]
    food_name <- valid_items$articulo[i]
    
    sub_row      <- item_subclass_map %>% filter(nombre_ciudad == city_name, articulo == food_name)
    subclase_ipc <- if (nrow(sub_row) > 0) sub_row$subclase_ipc[1] else NA
    
    k_val <- subclass_sizes %>%
      filter(nombre_ciudad == city_name, subclase_ipc == subclase_ipc) %>%
      pull(k)
    k_val <- if (length(k_val) > 0) k_val[1] else 1L
    
    if (is.na(subclase_ipc)) next
    
    if (k_val == 1) {
      all_lambda[[length(all_lambda) + 1]] <- tibble(
        ciudad = city_name, articulo = food_name,
        subclase_ipc = subclase_ipc, k = 1L,
        apply_D = FALSE, lambda = 1.0,
        sigma2 = NA_real_, r2 = NA_real_,
        n_obs = NA_integer_, est_ok = TRUE)
      next
    }
    
    df_obs <- dane %>%
      filter(nombre_ciudad == city_name, articulo == food_name,
             fecha <= vent$TRAIN_END, !is.na(precio_500g)) %>%
      mutate(codigo_articulo = as.character(codigo_articulo),
             cod_subclase    = as.character(cod_subclase)) %>%
      arrange(fecha)
    
    if (nrow(df_obs) == 0) next
    
    subclase_code <- substr(paste0(subclase_ipc, "00"), 1, 8)
    df_ipc <- ipc %>%
      filter(ciudad == city_name, cod_subclase == subclase_code, fecha <= vent$TRAIN_END) %>%
      select(fecha, ipc) %>% arrange(fecha)
    
    if (nrow(df_ipc) == 0) next
    est <- estimate_lambda_fd(df_obs, df_ipc)
    
    all_lambda[[length(all_lambda) + 1]] <- tibble(
      ciudad = city_name, articulo = food_name,
      subclase_ipc = subclase_ipc, k = k_val,
      apply_D = TRUE,
      lambda  = if (!is.null(est)) est$lambda else NA_real_,
      sigma2  = if (!is.null(est)) est$sigma2 else NA_real_,
      r2      = if (!is.null(est)) est$r2     else NA_real_,
      n_obs   = if (!is.null(est)) est$n_obs  else NA_integer_,
      est_ok  = !is.null(est))
  }
  
  lambda_raw <- bind_rows(all_lambda)
  
  message(sprintf(
    "    %d series | multi-item OK: %d | single-item: %d | failed: %d",
    nrow(lambda_raw),
    sum(lambda_raw$apply_D & lambda_raw$est_ok, na.rm = TRUE),
    sum(!lambda_raw$apply_D),
    sum(lambda_raw$apply_D & !lambda_raw$est_ok, na.rm = TRUE)))
  
  lambda_raw_all[[vent$nombre]] <- lambda_raw %>% mutate(ventana = vent$nombre)
  
  # -----------------------------------------------------------------
  # 2.3 Grid search de thresholds (SOLO ventana de validación)
  #     La ventana de pronóstico reutiliza params_optimal_v1
  # -----------------------------------------------------------------
  if (vent$tipo == "validacion") {
    
    message("  Corriendo grid search de thresholds...")
    
    obs_val <- dane %>%
      filter(fecha >= vent$EVAL_START, fecha <= vent$EVAL_END, !is.na(precio_500g)) %>%
      select(ciudad = nombre_ciudad, articulo, fecha, price_obs = precio_500g)
    
    ipc_t0_lookup <- ipc %>%
      filter(fecha == vent$T0) %>%
      select(ciudad, cod_subclase, ipc_t0 = ipc)
    
    ipc_val <- ipc %>%
      filter(fecha >= vent$EVAL_START, fecha <= vent$EVAL_END) %>%
      select(ciudad, cod_subclase, fecha, ipc)
    
    grid <- expand.grid(
      R2_MIN  = c(0.05, 0.10, 0.15, 0.20),
      LAM_MIN = c(0.10, 0.20, 0.30),
      LAM_MAX = c(3.00, 4.00, 5.00),
      stringsAsFactors = FALSE
    ) %>% filter(LAM_MIN < LAM_MAX)
    
    subclass_js <- lambda_raw %>%
      filter(apply_D, est_ok, !is.na(lambda)) %>%
      group_by(subclase_ipc, ciudad) %>%
      summarise(
        k_city    = n_distinct(articulo),
        sigma2_sc = mean(sigma2, na.rm = TRUE),
        Q_sc      = sum((lambda - 1)^2, na.rm = TRUE),
        .groups   = "drop")
    
    lambda_shrunk_base <- lambda_raw %>%
      left_join(subclass_js, by = c("subclase_ipc", "ciudad")) %>%
      mutate(
        delta = case_when(
          !apply_D | !est_ok  ~ NA_real_,
          k == 1              ~ 1.0,
          k >= 3 ~ pmax(0, 1 - (k_city - 2) * sigma2_sc / Q_sc),
          k == 2 ~ {
            dev <- (lambda - 1)^2
            if_else((dev + sigma2) > 0, dev / (dev + sigma2), 0)},
          TRUE ~ NA_real_),
        lambda_shrunk = case_when(
          !apply_D                 ~ 1.0,
          !est_ok | is.na(lambda)  ~ 1.0,
          TRUE                     ~ 1 + delta * (lambda - 1)),
        lambda_shrunk = if_else(!is.finite(lambda_shrunk), 1.0, lambda_shrunk)
      ) %>%
      select(ciudad, articulo, subclase_ipc, k, apply_D, est_ok, lambda, r2, lambda_shrunk)
    
    price_base_df <- lambda_shrunk_base %>%
      filter(!is.na(subclase_ipc)) %>%
      mutate(cod_subclase = substr(paste0(subclase_ipc, "00"), 1, 8)) %>%
      inner_join(anchor_prices, by = c("ciudad", "articulo")) %>%
      inner_join(ipc_t0_lookup, by = c("ciudad", "cod_subclase")) %>%
      inner_join(ipc_val,       by = c("ciudad", "cod_subclase")) %>%
      mutate(ipc_ratio = ipc / ipc_t0) %>%
      select(ciudad, articulo, fecha, subclase_ipc, k, apply_D, est_ok, lambda, r2,
             lambda_shrunk, precio_anchor, ipc_ratio)
    
    message(sprintf("    %d filas para evaluación del grid", nrow(price_base_df)))
    
    grid_results <- list()
    for (g in seq_len(nrow(grid))) {
      
      R2_g   <- grid$R2_MIN[g]
      LMin_g <- grid$LAM_MIN[g]
      LMax_g <- grid$LAM_MAX[g]
      
      pred_g <- price_base_df %>%
        mutate(
          reversion = case_when(
            !apply_D                 ~ "single_item",
            !est_ok | is.na(lambda)  ~ "est_failed",
            r2 < R2_g                ~ "low_R2",
            lambda_shrunk < LMin_g   ~ "low_lambda",
            lambda_shrunk > LMax_g   ~ "high_lambda",
            TRUE                     ~ "none"),
          lambda_final = if_else(reversion == "none", lambda_shrunk, 1.0),
          price_pred   = precio_anchor * ipc_ratio ^ lambda_final,
          price_BASE   = precio_anchor * ipc_ratio)
      
      ape_g <- pred_g %>%
        inner_join(obs_val, by = c("ciudad", "articulo", "fecha")) %>%
        filter(!is.na(price_obs), price_obs > 0) %>%
        mutate(
          ape_S5   = abs((price_pred - price_obs) / price_obs) * 100,
          ape_BASE = abs((price_BASE - price_obs) / price_obs) * 100)
      
      if (nrow(ape_g) == 0) next
      
      ape_multi <- ape_g %>% filter(k > 1)
      
      grid_results[[g]] <- tibble(
        R2_MIN = R2_g, LAM_MIN = LMin_g, LAM_MAX = LMax_g,
        MAPE_S5   = mean(ape_g$ape_S5,   na.rm = TRUE),
        MAPE_BASE = mean(ape_g$ape_BASE, na.rm = TRUE),
        imp_pp    = mean(ape_g$ape_BASE, na.rm = TRUE) - mean(ape_g$ape_S5, na.rm = TRUE),
        MAPE_S5_multi   = mean(ape_multi$ape_S5,   na.rm = TRUE),
        MAPE_BASE_multi = mean(ape_multi$ape_BASE, na.rm = TRUE),
        imp_pp_multi    = mean(ape_multi$ape_BASE, na.rm = TRUE) - mean(ape_multi$ape_S5, na.rm = TRUE),
        n_dcity    = sum(pred_g$reversion == "none" & pred_g$apply_D, na.rm = TRUE),
        n_low_r2   = sum(pred_g$reversion == "low_R2",     na.rm = TRUE),
        n_low_lam  = sum(pred_g$reversion == "low_lambda", na.rm = TRUE),
        n_high_lam = sum(pred_g$reversion == "high_lambda", na.rm = TRUE),
        n_obs      = nrow(ape_g))
    }
    
    grid_df <- bind_rows(grid_results) %>% arrange(MAPE_S5_multi)
    best_row <- grid_df %>% slice(1)
    
    params_optimal <- tibble(
      R2_MIN = best_row$R2_MIN, LAM_MIN = best_row$LAM_MIN, LAM_MAX = best_row$LAM_MAX,
      MAPE_S5_multi = best_row$MAPE_S5_multi, MAPE_BASE_multi = best_row$MAPE_BASE_multi,
      imp_pp_multi = best_row$imp_pp_multi,
      n_dcity = best_row$n_dcity, n_low_r2 = best_row$n_low_r2)
    
    message(sprintf(
      "    Óptimo: R2_MIN=%.2f | LAM_MIN=%.2f | LAM_MAX=%.2f | MAPE=%.4f%%",
      params_optimal$R2_MIN, params_optimal$LAM_MIN, params_optimal$LAM_MAX,
      params_optimal$MAPE_S5_multi))
    
    grid_results_all[[vent$nombre]]   <- grid_df %>% mutate(ventana = vent$nombre)
    params_optimal_all[[vent$nombre]] <- params_optimal %>% mutate(ventana = vent$nombre)
    
    # ---- Figuras de threshold grid (solo ventana de validación) ----
    p_heat <- grid_df %>%
      mutate(LAM_MAX_lbl = paste0("LAM_MAX = ", LAM_MAX),
             R2_lbl      = paste0("R2>=", R2_MIN),
             LAM_MIN_lbl = paste0("lambda>=", LAM_MIN)) %>%
      ggplot(aes(x = LAM_MIN_lbl, y = R2_lbl, fill = MAPE_S5_multi)) +
      geom_tile(color = "white", linewidth = 0.5) +
      geom_text(aes(label = sprintf("%.3f", MAPE_S5_multi)), size = 3, color = "black") +
      scale_fill_gradient(low = "#1A7A4A", high = "#E74C3C", name = "MAPE_S5\n(multi-item)") +
      facet_wrap(~ LAM_MAX_lbl, ncol = 3) +
      labs(
        title = paste0("Metodología A2 — Grid search (", vent$nombre, ")"),
        subtitle = sprintf("Óptimo: R2>=%.2f | lambda en [%.2f,%.2f] -> MAPE=%.4f%%",
                           params_optimal$R2_MIN, params_optimal$LAM_MIN,
                           params_optimal$LAM_MAX, params_optimal$MAPE_S5_multi),
        x = "LAM_MIN", y = "R2_MIN") +
      theme_bw(base_size = 11) +
      theme(axis.text.x = element_text(angle = 20, hjust = 1))
    
    ggsave(file.path(vent_dir, "fig_threshold_heatmap.png"), p_heat, width = 13, height = 6, dpi = 200)
    
    # Guardamos para reutilizar en ventana de pronóstico
    params_optimal_v1 <- params_optimal
    
  } else {
    
    message("  Ventana de pronóstico: reutilizando thresholds óptimos de la ventana de validación")
    if (is.null(params_optimal_v1)) {
      stop("No hay params_optimal_v1 disponible — la ventana de validación debe correr primero")
    }
    params_optimal <- params_optimal_v1
  }
  
  R2_MIN  <- params_optimal$R2_MIN
  LAM_MIN <- params_optimal$LAM_MIN
  LAM_MAX <- params_optimal$LAM_MAX
  
  # -----------------------------------------------------------------
  # 2.4 JS shrinkage + bounds -> lambda_final para ESTA ventana
  #     (siempre se recalcula con el lambda_raw de la ventana, aplicando
  #      los thresholds correspondientes)
  # -----------------------------------------------------------------
  message("  Aplicando shrinkage JS y bounds...")
  
  subclass_js <- lambda_raw %>%
    filter(apply_D, est_ok, !is.na(lambda)) %>%
    group_by(subclase_ipc, ciudad) %>%
    summarise(
      k_city    = n_distinct(articulo),
      sigma2_sc = mean(sigma2, na.rm = TRUE),
      Q_sc      = sum((lambda - 1)^2, na.rm = TRUE),
      .groups   = "drop")
  
  lambda_final <- lambda_raw %>%
    left_join(subclass_js, by = c("subclase_ipc", "ciudad")) %>%
    mutate(
      delta = case_when(
        !apply_D | !est_ok  ~ NA_real_,
        k == 1              ~ 1.0,
        k >= 3 ~ pmax(0, 1 - (k_city - 2) * sigma2_sc / Q_sc),
        k == 2 ~ {
          dev <- (lambda - 1)^2
          if_else((dev + sigma2) > 0, dev / (dev + sigma2), 0)},
        TRUE ~ NA_real_),
      lambda_shrunk = case_when(
        !apply_D                 ~ 1.0,
        !est_ok | is.na(lambda)  ~ 1.0,
        TRUE                     ~ 1 + delta * (lambda - 1)),
      lambda_shrunk = if_else(!is.finite(lambda_shrunk), 1.0, lambda_shrunk),
      reversion_reason = case_when(
        !apply_D                   ~ "single_item",
        !est_ok | is.na(lambda)    ~ "est_failed",
        r2 < R2_MIN                ~ "low_R2",
        lambda_shrunk < LAM_MIN    ~ "low_lambda",
        lambda_shrunk > LAM_MAX    ~ "high_lambda",
        TRUE                       ~ "none"),
      lambda_final = if_else(reversion_reason == "none", lambda_shrunk, 1.0),
      method_label = case_when(
        !apply_D                          ~ "BASE",
        reversion_reason == "est_failed"  ~ "BASE_fallback",
        reversion_reason == "low_R2"      ~ "BASE_lowR2",
        reversion_reason == "low_lambda"  ~ "BASE_lowlambda",
        reversion_reason == "high_lambda" ~ "BASE_highlambda",
        TRUE                              ~ "D_city"))
  
  shrink_diag <- lambda_final %>%
    filter(apply_D, est_ok, !is.na(lambda)) %>%
    summarise(
      n              = n(),
      delta_mean     = mean(delta, na.rm = TRUE),
      mean_abs_raw   = mean(abs(lambda - 1), na.rm = TRUE),
      mean_abs_final = mean(abs(lambda_final - 1), na.rm = TRUE),
      pct_shrinkage  = (1 - mean_abs_final / mean_abs_raw) * 100)
  
  message(sprintf("    Shrinkage: |lambda-1| %.3f -> %.3f (%.1f%%)",
                  shrink_diag$mean_abs_raw, shrink_diag$mean_abs_final, shrink_diag$pct_shrinkage))
  
  lambda_final_all[[vent$nombre]] <- lambda_final %>% mutate(ventana = vent$nombre)
  
  if (vent$tipo == "validacion") lambda_final_v1 <- lambda_final
  
  # ---- Figuras de distribución lambda (ambas ventanas) ----
  lambda_dcity <- lambda_final %>%
    filter(apply_D, est_ok, method_label == "D_city") %>%
    mutate(ciudad_lbl = CITY_LABELS[ciudad])
  
  if (nrow(lambda_dcity) > 0) {
    p_dist <- ggplot(lambda_dcity, aes(x = lambda_final, fill = ciudad_lbl)) +
      geom_histogram(bins = 30, alpha = 0.75, position = "identity", color = "white") +
      geom_vline(xintercept = 1, linetype = "dashed", color = "#C0392B", linewidth = 0.8) +
      geom_vline(xintercept = c(LAM_MIN, LAM_MAX), linetype = "dotted", color = "grey40", linewidth = 0.6) +
      scale_fill_manual(values = CITY_COLORS) +
      labs(
        title = paste0("Metodología A2 — Lambda JS-shrunk (", vent$nombre, ")"),
        subtitle = sprintf("Media=%.3f | DE=%.3f | R2>=%.2f | lambda en [%.2f,%.2f]",
                           mean(lambda_dcity$lambda_final, na.rm = TRUE),
                           sd(lambda_dcity$lambda_final, na.rm = TRUE),
                           R2_MIN, LAM_MIN, LAM_MAX),
        x = "Lambda (JS-shrunk)", y = "Conteo", fill = "Ciudad") +
      theme_bw(base_size = 11) +
      theme(legend.position = "bottom")
    
    ggsave(file.path(vent_dir, "fig_lambda_distribution.png"), p_dist, width = 8, height = 5, dpi = 200)
  }
  
  p_shrink <- lambda_final %>%
    filter(apply_D, est_ok, !is.na(lambda)) %>%
    mutate(ciudad_lbl = CITY_LABELS[ciudad]) %>%
    ggplot(aes(x = lambda, y = lambda_final, color = ciudad_lbl)) +
    geom_point(alpha = 0.4, size = 1.5) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black", linewidth = 0.6) +
    geom_hline(yintercept = c(LAM_MIN, LAM_MAX), linetype = "dotted", color = "grey50") +
    geom_vline(xintercept = 1, linetype = "dashed", color = "#C0392B", linewidth = 0.5) +
    scale_color_manual(values = CITY_COLORS) +
    labs(
      title = paste0("Metodología A2 — Efecto shrinkage (", vent$nombre, ")"),
      x = "Lambda OLS (crudo)", y = "Lambda JS-shrunk (final)", color = "Ciudad") +
    theme_bw(base_size = 11) +
    theme(legend.position = "bottom")
  
  ggsave(file.path(vent_dir, "fig_shrinkage_effect.png"), p_shrink, width = 7, height = 6, dpi = 200)
  
  # -----------------------------------------------------------------
  # 2.5 Construcción de precios para esta ventana
  # -----------------------------------------------------------------
  message("  Construyendo precios estimados...")
  
  obs_dane_full <- dane %>%
    select(ciudad = nombre_ciudad, articulo, fecha, price_obs = precio_500g)
  
  all_series <- list()
  
  for (i in seq_len(nrow(lambda_final))) {
    
    city_name    <- lambda_final$ciudad[i]
    food_name    <- lambda_final$articulo[i]
    lambda_use   <- lambda_final$lambda_final[i]
    method_use   <- lambda_final$method_label[i]
    subclase_ipc <- lambda_final$subclase_ipc[i]
    
    if (!is.finite(lambda_use)) lambda_use <- 1.0
    if (is.na(subclase_ipc)) next
    
    subclase_code <- substr(paste0(subclase_ipc, "00"), 1, 8)
    
    anchor <- anchor_prices %>%
      filter(ciudad == city_name, articulo == food_name) %>%
      pull(precio_anchor)
    if (length(anchor) == 0) next
    anchor <- anchor[1]
    
    df_ipc <- ipc %>%
      filter(ciudad == city_name, cod_subclase == subclase_code) %>%
      select(fecha, ipc) %>% arrange(fecha)
    if (nrow(df_ipc) == 0) next
    
    ipc_t0 <- df_ipc %>% filter(fecha == vent$T0) %>% pull(ipc)
    if (length(ipc_t0) == 0 || is.na(ipc_t0)) next
    ipc_t0 <- ipc_t0[1]
    
    obs_item <- obs_dane_full %>% filter(ciudad == city_name, articulo == food_name)
    
    pred <- df_ipc %>%
      filter(fecha >= vent$T0, fecha <= vent$EVAL_END) %>%
      mutate(
        ciudad       = city_name,
        articulo     = food_name,
        method       = method_use,
        subclase_ipc = subclase_ipc,
        ipc_ratio    = ipc / ipc_t0,
        price_BASE   = anchor * ipc_ratio,
        price_S5     = anchor * ipc_ratio ^ lambda_use
      ) %>%
      left_join(obs_item %>% select(fecha, price_obs), by = "fecha")
    
    all_series[[length(all_series) + 1]] <- pred
  }
  
  series_df <- bind_rows(all_series)
  message(sprintf("    %d filas item x ciudad x mes", nrow(series_df)))
  
  prices_window <- series_df %>%
    mutate(ventana = vent$nombre, tipo_ventana = vent$tipo) %>%
    select(ventana, tipo_ventana, ciudad, articulo, fecha, subclase_ipc, method,
           price_BASE, price_S5, price_obs) %>%
    arrange(ciudad, articulo, fecha)
  
  prices_all_windows[[vent$nombre]] <- prices_window
  
  # -----------------------------------------------------------------
  # 2.6 Figuras: observado vs. estimado (precio_S5), un PDF multi-página por ciudad
  # -----------------------------------------------------------------
  message("  Generando figuras observado vs. estimado...")
  
  plots_by_city <- setNames(vector("list", length(CITIES_USE)), CITIES_USE)
  
  items_in_window <- prices_window %>% distinct(ciudad, articulo) %>% arrange(ciudad, articulo)
  
  for (i in seq_len(nrow(items_in_window))) {
    
    city_name <- items_in_window$ciudad[i]
    food_name <- items_in_window$articulo[i]
    
    df_plot <- prices_window %>% filter(ciudad == city_name, articulo == food_name)
    
    p <- ggplot(df_plot, aes(x = fecha)) +
      geom_line(aes(y = price_S5, color = "Precio estimado (A2)"), linewidth = 0.4, linetype = "dashed") +
      geom_line(aes(y = price_BASE, color = "Precio base (lambda=1)"), linewidth = 0.3, linetype = "dotted") +
      geom_line(
        data = df_plot %>% filter(!is.na(price_obs)),
        aes(y = price_obs, color = "Precio observado"),
        linewidth = 0.4
      ) +
      scale_color_manual(values = c(
        "Precio observado" = "black",
        "Precio estimado (A2)" = "red",
        "Precio base (lambda=1)" = "grey60"
      )) +
      labs(
        title = paste0("Metodología A2 — ", vent$nombre),
        subtitle = paste0(city_name, " — ", food_name),
        x = NULL, y = "Precio (500g)", color = ""
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
lambda_raw_all_df     <- bind_rows(lambda_raw_all)
lambda_final_all_df   <- bind_rows(lambda_final_all)
grid_results_all_df   <- bind_rows(grid_results_all)
params_optimal_all_df <- bind_rows(params_optimal_all)

saveRDS(prices_all_windows_df, file.path(out_dir, "metA2_precios_dos_ventanas.rds"))

write_xlsx(
  split(lambda_raw_all_df, lambda_raw_all_df$ventana),
  file.path(out_tablas, "metA2_lambda_raw_por_ventana.xlsx")
)

write_xlsx(
  split(lambda_final_all_df, lambda_final_all_df$ventana),
  file.path(out_tablas, "metA2_lambda_final_por_ventana.xlsx")
)

write_xlsx(
  list(grid_search = grid_results_all_df),
  file.path(out_tablas, "metA2_grid_search_ventana1.xlsx")
)

write_xlsx(
  list(params_optimal = params_optimal_all_df),
  file.path(out_tablas, "metA2_params_optimal.xlsx")
)

message("\nListo. Outputs en: ", out_dir)
message(sprintf("  %d filas en el panel de precios (2 ventanas)", nrow(prices_all_windows_df)))