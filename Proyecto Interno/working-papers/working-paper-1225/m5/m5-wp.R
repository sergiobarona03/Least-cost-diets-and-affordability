############################################################
## M5: ECM (Error-Correction Model) + cointegration test   ##
## Cointegration: aTSA::coint.test (Engle–Granger EG test) ##
## ECM: Δlog_ipc = c + γ*ECT_{t-1} + β*Δlog_sipsa + month FE
## + Forecast (levels) + metrics + grouped plots (3x3)     ##
############################################################

# -----------------------
# 0) Packages
# -----------------------
library(tidyverse)
library(readxl)
library(Metrics)
library(ggplot2)
library(lubridate)
library(readr)
library(ggforce)   # facet_wrap_paginate
library(aTSA)      # coint.test

# -----------------------
# 1) Paths / working dir
# -----------------------
base_dir <- "C:/Users/Portatil/Desktop/Least-cost-diets-and-affordability/Proyecto Interno"
setwd(base_dir)

date_tag <- "121225"

infile <- file.path(
  base_dir,
  "working-papers", "working-paper-1225",
  "mapeo ipc-sipsa", "output",
  paste0(date_tag, "_dataset_ipc_sipsa.xlsx")
)

out_dir <- file.path(base_dir, "working-papers", "working-paper-1225", "m5", "output_ecm")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# -----------------------
# 2) Load dataset
# -----------------------
dataset <- read_excel(infile)

# -----------------------
# 3) Helpers
# -----------------------
rmse2 <- function(y, yhat) sqrt(mean((y - yhat)^2, na.rm = TRUE))
mape2 <- function(y, yhat) mean(abs((y - yhat) / y), na.rm = TRUE) 

safe_name <- function(x) {
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  x <- gsub("[^A-Za-z0-9_]+", "_", x)
  x <- gsub("_+", "_", x)
  x <- gsub("^_|_$", "", x)
  x
}

# -----------------------
# 4) Containers
# -----------------------
food.vector <- sort(unique(dataset$alimento_sipsa))

cointegration_table <- tibble()
ecm_params_table <- tibble()
metrics_table <- tibble()
plot_store <- list()

# -----------------------
# 5) Main loop (per food)
# -----------------------
for (i in seq_along(food.vector)) {

  food.x <- food.vector[i]
  message("Procesando: ", food.x)
  
  data.food <- dataset %>%
    filter(alimento_sipsa == food.x) %>%
    drop_na(precio_ipc, precio_sipsa) %>%
    mutate(
      fecha     = as.Date(paste(Year, Month, 1, sep = "-")),
      log_ipc   = log(precio_ipc),
      log_sipsa = log(precio_sipsa),
      mes       = factor(month(fecha), levels = 1:12, labels = month.abb)
    ) %>%
    arrange(fecha)
  
  # Need enough obs for cointegration + ECM
  if (nrow(data.food) < 36) next
  
  # -----------------------
  # Train/test split (temporal, 70/30)
  # -----------------------
  n <- nrow(data.food)
  cut <- floor(0.7 * n)
  train <- data.food[1:cut, ]
  test  <- data.food[(cut + 1):n, ]
  if (nrow(test) < 6) next
  
  # ==========================================================
  # (A) Cointegration test (aTSA::coint.test) on TRAIN
  # NOTE: coint.test runs EG residual-based test for y ~ X.
  # ==========================================================
  # Choose lag order for EG ADF on residuals:
  # - nlag = NULL lets aTSA choose default
  # - or set e.g., nlag = 1,2,...
  eg_nlag <- 1
  
  ct <- tryCatch(
    aTSA::coint.test(
      y = train$log_ipc,
      X = train$log_sipsa
    ),
    error = function(e) NULL
  )
  
  if (is.null(ct)) next
  
  # ct is a 3x3 matrix: rows type1/type2/type3; cols lag/EG/p.value
  ct_mat <- as.matrix(ct)
  
  cointegration_table <- bind_rows(
    cointegration_table,
    tibble(
      alimento_sipsa = food.x,
      n_train = nrow(train),
      EG_type1 = as.numeric(ct_mat[1,2]),
      p_type1  = as.numeric(ct_mat[1,3]),
      EG_type2 = as.numeric(ct_mat[2,2]),
      p_type2  = as.numeric(ct_mat[2,2]),
      EG_type3 = as.numeric(ct_mat[3,2]),
      p_type3  = as.numeric(ct_mat[3,3])
    )
  )
  
  # ==========================================================
  # (B) Long-run equation (levels) to build ECT
  # We keep your seasonal control in the long-run relation:
  # log_ipc_t = a + b*log_sipsa_t + month FE + u_t
  # ==========================================================
  lr_model <- lm(log_ipc ~ log_sipsa + mes, data = train)
  
  # Build combined data to compute ECT_{t-1} consistently for TEST too
  comb <- bind_rows(
    train %>% mutate(split = "train"),
    test  %>% mutate(split = "test")
  ) %>%
    arrange(fecha) %>%
    mutate(
      lr_fit = predict(lr_model, newdata = .),
      ect    = log_ipc - lr_fit,
      ect_l1 = lag(ect),
      dlog_sipsa = log_sipsa - lag(log_sipsa),
      dlog_ipc   = log_ipc   - lag(log_ipc)
    )
  
  # ECM training sample (needs Δ and lagged ECT)
  train_ecm <- comb %>%
    filter(split == "train") %>%
    drop_na(dlog_ipc, dlog_sipsa, ect_l1)
  
  if (nrow(train_ecm) < 18) next
  
  # ==========================================================
  # (C) ECM estimation
  # Δlog_ipc = c + β*Δlog_sipsa + γ*ECT_{t-1} + month FE + e
  # ==========================================================
  ecm_model <- lm(dlog_ipc ~ dlog_sipsa + ect_l1 + mes, data = train_ecm)
  
  cs <- summary(ecm_model)$coefficients
  beta  <- cs["dlog_sipsa", "Estimate"]
  gamma <- cs["ect_l1", "Estimate"]
  
  ecm_params_table <- bind_rows(
    ecm_params_table,
    tibble(
      alimento_sipsa = food.x,
      n_train_ecm = nrow(train_ecm),
      beta_dlog = beta,
      beta_se   = cs["dlog_sipsa", "Std. Error"],
      gamma_ect = gamma,
      gamma_se  = cs["ect_l1", "Std. Error"]
    )
  )
  
  # ==========================================================
  # (D) Forecast on TEST (levels) + CI
  # Predict Δlog_ipc, then cumulate to log-levels.
  # ==========================================================
  test_ecm <- comb %>%
    filter(split == "test") %>%
    drop_na(dlog_sipsa, ect_l1)
  
  if (nrow(test_ecm) < 3) next
  
  preds <- predict(ecm_model, newdata = test_ecm, 
                   interval = "confidence", level = 0.95)
  
  test_ecm <- test_ecm %>%
    mutate(
      dlog_fit = preds[, "fit"],
      dlog_lwr = preds[, "lwr"],
      dlog_upr = preds[, "upr"]
    )
  
  y0 <- tail(train$log_ipc, 1)
  
  test_ecm <- test_ecm %>%
    mutate(
      yhat_log     = y0 + cumsum(dlog_fit),
      yhat_log_lwr = y0 + cumsum(dlog_lwr),
      yhat_log_upr = y0 + cumsum(dlog_upr),
      obs_level = exp(log_ipc),
      fit_level = exp(yhat_log),
      lwr_level = exp(yhat_log_lwr),
      upr_level = exp(yhat_log_upr)
    )
  
  # Metrics (test)
  rmse_log   <- rmse2(test_ecm$log_ipc, test_ecm$yhat_log)
  mape_level <- mape2(test_ecm$obs_level, test_ecm$fit_level)
  
  metrics_table <- bind_rows(
    metrics_table,
    tibble(
      alimento_sipsa = food.x,
      n_test = nrow(test_ecm),
      RMSE_log = rmse_log,
      MAPE_level = mape_level,
      coint_p_type1 = as.numeric(ct_mat[1, 3]),
      coint_p_type2 = as.numeric(ct_mat[2, 3]),
      coint_p_type3 = as.numeric(ct_mat[3, 3])
    )
  )
  
  # ==========================================================
  # (E) Store plot data for grouped pages
  # ==========================================================
  plot_df <- comb %>%
    transmute(
      alimento_sipsa = food.x,
      fecha,
      obs_level_full = exp(log_ipc)
    ) %>%
    left_join(
      test_ecm %>% select(fecha, fit_level, lwr_level, upr_level),
      by = "fecha"
    )
  
  plot_store[[length(plot_store) + 1]] <- plot_df
}

# -----------------------
# 6) Save outputs
# -----------------------
write_csv(cointegration_table, file.path(out_dir, "m5_cointegration_coint_test.csv"))
write_csv(ecm_params_table,    file.path(out_dir, "m5_ecm_parameters.csv"))
write_csv(metrics_table,       file.path(out_dir, "m5_ecm_metrics_test.csv"))

write_xlsx(
  list(
    cointegration = cointegration_table,
    ecm_params    = ecm_params_table,
    metrics_test  = metrics_table
  ),
  file.path(out_dir, "m5_ecm_outputs.xlsx")
)

# -----------------------
# 7) Grouped plots in pages (3x3)
# -----------------------
plot_all <- if (length(plot_store) == 0) tibble() else bind_rows(plot_store)

if (nrow(plot_all) > 0) {
  
  foods_ok <- sort(unique(plot_all$alimento_sipsa))
  n_per_page <- 9
  n_pages <- ceiling(length(foods_ok) / n_per_page)
  
  for (p in seq_len(n_pages)) {

    gg_food <- ggplot(plot_all, aes(x = fecha)) +
      # geom_ribbon(aes(ymin = lwr_level, ymax = upr_level), alpha = 0.20) +
      geom_line(aes(y = obs_level_full), linewidth = 0.7) +
      geom_line(aes(y = fit_level), linetype = "dashed", linewidth = 0.7,
                col = "red") +
      facet_wrap_paginate(
        ~ alimento_sipsa,
        scales = "free_y",
        ncol = 3, nrow = 3,
        page = p
      ) +
      labs(
        title = "M5 (ECM): Observed vs Predicted Retail Price (levels)",
        subtitle = paste("Cointegration test: aTSA::coint.test (EG) | Page", p, "of", n_pages),
        x = NULL,
        y = "Precio (nivel)"
      ) +
      theme_classic(base_size = 11) +
      theme(strip.text = element_text(face = "bold", size = 9))
    
    ggsave(
      filename = file.path(out_dir, paste0("m5_ecm_grouped_page_", sprintf("%02d", p), ".png")),
      plot = gg_food,
      width = 13,
      height = 9,
      dpi = 300
    )
  }
  
  message("Listo. Outputs (tablas + plots) en: ", out_dir)
  
} else {
  message("No se generaron plots (plot_store vacío). Revisa filtros / mínimos de observaciones.")
}
