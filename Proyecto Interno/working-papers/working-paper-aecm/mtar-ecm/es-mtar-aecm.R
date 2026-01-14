############################################################
## ecm_es_nonlinear_run.R
## Enders–Siklos style non-linear models (TAR / M-TAR / ECM M-TAR)
## applied to retail (IPC) vs wholesale (SIPSA) prices
############################################################

rm(list = ls())

library(tidyverse)
library(readxl)
library(lubridate)
library(aTSA)
library(purrr)

#-----------------------------------------------------------------
# 0. Paths, date tag, and source auxiliary functions
#-----------------------------------------------------------------

setwd("C:\\Users\\sergio.barona\\Desktop\\Least-cost-diets-and-affordability\\Proyecto Interno\\")
date_tag <- "261225"

path.in <- paste0("working-papers\\working-paper-aecm\\input\\",
                  date_tag, "_selected_foods_dataset.xlsx")

out_dir <- "working-papers\\working-paper-aecm\\output\\ts-ecm\\"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

## Symmetric ECM helpers (if needed later)
source("working-papers\\working-paper-aecm\\ecm-aecm\\ecm_aux.R")

## Non-linear Enders–Siklos helpers (you already have these)
source("working-papers\\working-paper-aecm\\literature\\enders_siklos\\aux-functions\\aux-utils.R")
source("working-papers\\working-paper-aecm\\literature\\enders_siklos\\aux-functions\\aux-tar-level.R")
source("working-papers\\working-paper-aecm\\literature\\enders_siklos\\aux-functions\\aux-tar-momentum.R")
source("working-papers\\working-paper-aecm\\literature\\enders_siklos\\aux-functions\\aux-mtar-threshold.R")
source("working-papers\\working-paper-aecm\\literature\\enders_siklos\\aux-functions\\aux-ecm-mtar.R")

#-----------------------------------------------------------------
# 1. Mapping and data prep (copied from ecm_run.R)
#-----------------------------------------------------------------

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
    tibble(cod_mun = city_code, articulo_ipc = art, alimento_sipsa = sipsa_vec)
  })
}

map_df <- bind_rows(
  mapping_to_df("76001", cali.mapping),
  mapping_to_df("11001", bogota.mapping),
  mapping_to_df("05001", medellin.mapping)
) %>%
  mutate(
    cod_mun = sprintf("%05d", as.integer(cod_mun)),
    articulo_ipc = str_squish(as.character(articulo_ipc)),
    alimento_sipsa = str_squish(as.character(alimento_sipsa))
  )

city_labs <- c("76001" = "Cali", "11001" = "Bogotá", "05001" = "Medellín")

# -----------------------
# Load + prepare data (collapsed monthly)
# -----------------------
raw <- read_excel(path.in) %>%
  mutate(
    cod_mun = sprintf("%05d", as.integer(cod_mun)),
    Year = as.integer(Year),
    Month = as.integer(Month),
    fecha = as.Date(sprintf("%d-%02d-01", Year, Month)),
    articulo_ipc = str_squish(as.character(articulo_ipc)),
    alimento_sipsa = str_squish(as.character(alimento_sipsa)),
    precio_ipc = as.numeric(precio_ipc),
    precio_sipsa = as.numeric(precio_sipsa),
    mes = factor(month(fecha), levels = 1:12, labels = month.abb)
  ) %>%
  filter(!is.na(fecha), !is.na(precio_ipc), !is.na(precio_sipsa)) %>%
  filter(precio_ipc > 0, precio_sipsa > 0)

dat <- raw %>%
  semi_join(map_df, by = c("cod_mun", "articulo_ipc", "alimento_sipsa")) %>%
  group_by(cod_mun, fecha, articulo_ipc, alimento_sipsa, mes) %>%
  summarise(
    precio_ipc   = mean(precio_ipc, na.rm = TRUE),
    precio_sipsa = mean(precio_sipsa, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    log_ipc   = log(precio_ipc),
    log_sipsa = log(precio_sipsa)
  ) %>%
  arrange(cod_mun, articulo_ipc, alimento_sipsa, fecha)

# Nos quedamos con los artículos donde ya hay evidencia de cointegración
dat <- dat %>% filter(articulo_ipc %in% c("ARROZ PARA SECO",
                                          "PAPA",
                                          "PLÁTANO",
                                          "YUCA"))

#-----------------------------------------------------------------
# 2. Colecciones para resultados no lineales
#-----------------------------------------------------------------

tar_level_long   <- tibble()
tar_mom_long     <- tibble()
mtar_long        <- tibble()
ecm_mtar_coef    <- tibble()
ecm_mtar_Ftests  <- tibble()
coint_long_nl    <- tibble()  # opcional: re-guardar coint tests

keys <- dat %>% distinct(cod_mun, articulo_ipc, alimento_sipsa) %>%
  arrange(cod_mun, articulo_ipc, alimento_sipsa)

#-----------------------------------------------------------------
# 3. Loop sobre cada par ciudad–artículo–alimento
#-----------------------------------------------------------------

for (i in seq_len(nrow(keys))) {
  
  city_i <- keys$cod_mun[i]
  art_i  <- keys$articulo_ipc[i]
  ali_i  <- keys$alimento_sipsa[i]
  
  message("Procesando (no lineal): ", city_i, " | ", art_i, " <- ", ali_i)
  
  data.food <- dat %>%
    filter(cod_mun == city_i,
           articulo_ipc == art_i,
           alimento_sipsa == ali_i) %>%
    drop_na(log_ipc, log_sipsa, mes) %>%
    arrange(fecha)
  
  # exigir longitud mínima
  if (nrow(data.food) < 36) next
  
  #------------------------------------------------------
  # 3.1 Cointegration test (opcional, similar a ecm_run.R)
  #------------------------------------------------------
  
  ct <- tryCatch(aTSA::coint.test(y = data.food$log_ipc,
                                  X = data.food$log_sipsa),
                 error = function(e) NULL)
  if (!is.null(ct)) {
    m <- as.matrix(ct)
    coint_long_nl <- bind_rows(
      coint_long_nl,
      tibble(
        city = city_labs[[city_i]],
        cod_mun = city_i,
        articulo_ipc = art_i,
        alimento_sipsa = ali_i,
        p_type1 = as.numeric(m[1, 3]),
        p_type2 = as.numeric(m[2, 3]),
        p_type3 = as.numeric(m[3, 3])
      )
    )
  }
  
  #------------------------------------------------------
  # 3.2 Long-run: log_ipc ~ log_sipsa  (SIN mes)
  #     Residuals = mu_hat (como μ̂_t en Enders-Siklos)
  #------------------------------------------------------
  
  reg_coint <- lm(log_ipc ~ log_sipsa, data = data.food)
  mu_hat    <- resid(reg_coint)     # μ̂_t
  Tn        <- length(mu_hat)
  
  #------------------------------------------------------
  # 3.3 Linear-attractor TAR (nivel) sobre mu_hat
  #     Usamos p = 0 y p = 2 como en tu réplica financiera
  #------------------------------------------------------
  
  tarL_p0 <- tryCatch(estimate_linear_attractor_level(mu_hat, p = 0),
                      error = function(e) NULL)
  tarL_p2 <- tryCatch(estimate_linear_attractor_level(mu_hat, p = 2),
                      error = function(e) NULL)
  
  if (!is.null(tarL_p0)) {
    tar_level_long <- bind_rows(
      tar_level_long,
      tibble(
        city = city_labs[[city_i]],
        cod_mun = city_i,
        articulo_ipc = art_i,
        alimento_sipsa = ali_i,
        p     = 0,
        rho1  = tarL_p0$rho1,
        t_rho1 = tarL_p0$t_rho1,
        rho2  = tarL_p0$rho2,
        t_rho2 = tarL_p0$t_rho2,
        Phi_mu = tarL_p0$Phi_mu,
        F_equal = tarL_p0$F_equal,
        p_equal = tarL_p0$p_equal,
        AIC    = tarL_p0$AIC,
        BIC    = tarL_p0$BIC,
        Q4     = tarL_p0$Q4,
        p_Q4   = tarL_p0$p_Q4
      )
    )
  }
  
  if (!is.null(tarL_p2)) {
    tar_level_long <- bind_rows(
      tar_level_long,
      tibble(
        city = city_labs[[city_i]],
        cod_mun = city_i,
        articulo_ipc = art_i,
        alimento_sipsa = ali_i,
        p     = 2,
        rho1  = tarL_p2$rho1,
        t_rho1 = tarL_p2$t_rho1,
        rho2  = tarL_p2$rho2,
        t_rho2 = tarL_p2$t_rho2,
        Phi_mu = tarL_p2$Phi_mu,
        F_equal = tarL_p2$F_equal,
        p_equal = tarL_p2$p_equal,
        AIC    = tarL_p2$AIC,
        BIC    = tarL_p2$BIC,
        Q4     = tarL_p2$Q4,
        p_Q4   = tarL_p2$p_Q4
      )
    )
  }
  
  #------------------------------------------------------
  # 3.4 Linear-attractor TAR (momentum) sobre mu_hat
  #     I_t basado en Δμ̂_{t-1}
  #------------------------------------------------------
  
  tarM_p0 <- tryCatch(estimate_linear_attractor_momentum(mu_hat, p = 0),
                      error = function(e) NULL)
  tarM_p2 <- tryCatch(estimate_linear_attractor_momentum(mu_hat, p = 2),
                      error = function(e) NULL)
  
  if (!is.null(tarM_p0)) {
    tar_mom_long <- bind_rows(
      tar_mom_long,
      tibble(
        city = city_labs[[city_i]],
        cod_mun = city_i,
        articulo_ipc = art_i,
        alimento_sipsa = ali_i,
        p     = 0,
        rho1  = tarM_p0$rho1,
        t_rho1 = tarM_p0$t_rho1,
        rho2  = tarM_p0$rho2,
        t_rho2 = tarM_p0$t_rho2,
        Phi_mu = tarM_p0$Phi_mu,
        F_equal = tarM_p0$F_equal,
        p_equal = tarM_p0$p_equal,
        AIC    = tarM_p0$AIC,
        BIC    = tarM_p0$BIC,
        Q4     = tarM_p0$Q4,
        p_Q4   = tarM_p0$p_Q4
      )
    )
  }
  
  if (!is.null(tarM_p2)) {
    tar_mom_long <- bind_rows(
      tar_mom_long,
      tibble(
        city = city_labs[[city_i]],
        cod_mun = city_i,
        articulo_ipc = art_i,
        alimento_sipsa = ali_i,
        p     = 2,
        rho1  = tarM_p2$rho1,
        t_rho1 = tarM_p2$t_rho1,
        rho2  = tarM_p2$rho2,
        t_rho2 = tarM_p2$t_rho2,
        Phi_mu = tarM_p2$Phi_mu,
        F_equal = tarM_p2$F_equal,
        p_equal = tarM_p2$p_equal,
        AIC    = tarM_p2$AIC,
        BIC    = tarM_p2$BIC,
        Q4     = tarM_p2$Q4,
        p_Q4   = tarM_p2$p_Q4
      )
    )
  }
  
  #------------------------------------------------------
  # 3.5 M-TAR con búsqueda de umbral por AIC + Ljung–Box
  #     sobre mu_hat (como en tu replicación financiera)
  #------------------------------------------------------
  
  mtar_p0 <- tryCatch(
    search_tau_AIC_LB(mu_hat, p = 0, trim = 0.15, alpha_Q = 0.05),
    error = function(e) NULL
  )
  mtar_p2 <- tryCatch(
    search_tau_AIC_LB(mu_hat, p = 2, trim = 0.15, alpha_Q = 0.05),
    error = function(e) NULL
  )
  
  if (!is.null(mtar_p0)) {
    mtar_long <- bind_rows(
      mtar_long,
      tibble(
        city = city_labs[[city_i]],
        cod_mun = city_i,
        articulo_ipc = art_i,
        alimento_sipsa = ali_i,
        p     = 0,
        tau   = mtar_p0$tau,
        rho1  = mtar_p0$rho1,
        t_rho1 = mtar_p0$t_rho1,
        rho2  = mtar_p0$rho2,
        t_rho2 = mtar_p0$t_rho2,
        Phi_mu = mtar_p0$Phi_mu,
        F_equal = mtar_p0$F_equal,
        p_equal = mtar_p0$p_equal,
        AIC    = mtar_p0$AIC,
        BIC    = mtar_p0$BIC,
        Q4     = mtar_p0$Q4,
        p_Q4   = mtar_p0$p_Q4
      )
    )
  }
  
  if (!is.null(mtar_p2)) {
    mtar_long <- bind_rows(
      mtar_long,
      tibble(
        city = city_labs[[city_i]],
        cod_mun = city_i,
        articulo_ipc = art_i,
        alimento_sipsa = ali_i,
        p     = 2,
        tau   = mtar_p2$tau,
        rho1  = mtar_p2$rho1,
        t_rho1 = mtar_p2$t_rho1,
        rho2  = mtar_p2$rho2,
        t_rho2 = mtar_p2$t_rho2,
        Phi_mu = mtar_p2$Phi_mu,
        F_equal = mtar_p2$F_equal,
        p_equal = mtar_p2$p_equal,
        AIC    = mtar_p2$AIC,
        BIC    = mtar_p2$BIC,
        Q4     = mtar_p2$Q4,
        p_Q4   = mtar_p2$p_Q4
      )
    )
  }
  
  #------------------------------------------------------
  # 3.6 ECM M-TAR asimétrico (tipo Enders–Siklos)
  #     Usamos el τ̂ del modelo M-TAR con p = 2 (puedes cambiarlo a p=0 si quieres)
  #------------------------------------------------------
  
  if (is.null(mtar_p2)) {
    # si M-TAR p=2 no converge, intenta con p=0
    if (!is.null(mtar_p0)) {
      tau.x <- mtar_p0$tau
    } else {
      next  # no hay tau razonable → saltar el ECM asimétrico
    }
  } else {
    tau.x <- mtar_p2$tau
  }
  
  df_ecm_mtar <- tryCatch(
    build_ecm_mtar_df(
      mu_hat = mu_hat,
      r_f    = data.food$log_ipc,   # corto = retail
      r_10   = data.food$log_sipsa, # largo = wholesale
      tau_x  = tau.x,
      p_lags = 1                    # A_ij(L) de orden 1, como en ES
    ),
    error = function(e) NULL
  )
  
  if (is.null(df_ecm_mtar) || nrow(df_ecm_mtar) < 20) next
  
  ecm_fit <- tryCatch(
    estimate_ecm_mtar(df_ecm_mtar, p_lags = 1),
    error = function(e) NULL
  )
  if (is.null(ecm_fit)) next
  
  # Coeficientes de corrección de error asimétrica
  # ecm_fit$ecm_results tiene equation, regressor, estimate, t_value
  ecm_mtar_coef <- bind_rows(
    ecm_mtar_coef,
    ecm_fit$ecm_results %>%
      mutate(
        city = city_labs[[city_i]],
        cod_mun = city_i,
        articulo_ipc = art_i,
        alimento_sipsa = ali_i,
        tau_x = tau.x
      )
  )
  
  # F-tests A_ij(L) = 0 (guardamos con tags)
  # F-tests los vamos a guardar en formato largo: F_11, F_12, F_21, F_22
  Ft <- ecm_fit$F_tests
  # asumimos que Ft tiene columnas F_11, p_11, F_12, p_12, etc.
  ecm_mtar_Ftests <- bind_rows(
    ecm_mtar_Ftests,
    Ft %>%
      mutate(
        city = city_labs[[city_i]],
        cod_mun = city_i,
        articulo_ipc = art_i,
        alimento_sipsa = ali_i,
        tau_x = tau.x
      )
  )
}

#-----------------------------------------------------------------
# 4. Guardar objeto RDS con resultados no lineales
#-----------------------------------------------------------------

saveRDS(
  list(
    date_tag        = date_tag,
    mapping_used    = map_df,
    coint_long_nl   = coint_long_nl,
    tar_level_long  = tar_level_long,
    tar_mom_long    = tar_mom_long,
    mtar_long       = mtar_long,
    ecm_mtar_coef   = ecm_mtar_coef,
    ecm_mtar_Ftests = ecm_mtar_Ftests
  ),
  file.path(out_dir, paste0(date_tag, "_ecm_es_nonlinear_raw.rds"))
)

cat("\nSaved non-linear Enders–Siklos-style results to:\n",
    file.path(out_dir, paste0(date_tag, "_ecm_es_nonlinear_raw.rds")), "\n")
