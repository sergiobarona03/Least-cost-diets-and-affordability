###############################################################
## Monte Carlo para replicar el Panel A – Table 1
## Enders & Granger (1998)
##
## Panel A: Estadístico Φ (TAR)
## Caso: No estimated deterministic components
##
## Nula: y_t es una caminata aleatoria (unit root)
##       y_t = y_{t-1} + ε_t ,  ε_t ~ N(0,1)
##
## Para cada T ∈ {50, 100, 250, 1000}:
##   - Se generan 108,000 caminatas aleatorias
##   - Cada caminata usa T + 100 innovaciones
##   - Se descartan las primeras 100 observaciones (burn-in)
##   - Se exige que la serie cruce y=0
##   - Se calcula el estadístico Φ del modelo TAR
##   - Se obtienen los percentiles 90%, 95%, 99%
###############################################################

## =============================================================
## 1. PARÁMETROS DEL EXPERIMENTO
## =============================================================

T_vec   <- c(50, 100, 250, 1000)  # tamaños muestrales del Panel A
N_mc    <- 108000                 # número de replicaciones Monte Carlo
burn    <- 100                    # burn-in como en el paper
max_lag <- 12                     # máximo de rezagos ADF
ic_used <- "AIC"                  # criterio de información (AIC o BIC)
seed    <- 123                    # semilla para reproducibilidad

set.seed(seed)

## =============================================================
## 2. GENERADOR DE CAMINATAS ALEATORIAS
## =============================================================

simulate_random_walk <- function(T, burn = 100) {
  
  # Genera T + burn innovaciones normales iid
  eps <- rnorm(T + burn, mean = 0, sd = 1)
  
  # y0 = 0 implícito; caminata aleatoria
  y_full <- cumsum(eps)
  
  # Eliminar burn-in
  y <- y_full[(burn + 1):(burn + T)]
  
  return(y)
}

## =============================================================
## 3. CONDICIÓN: LA SERIE DEBE CRUZAR y = 0
## =============================================================

crosses_zero <- function(y) {
  any(y >= 0) && any(y < 0)
}

## =============================================================
## 4. CONSTRUCCIÓN DE DATOS PARA EL MODELO TAR
## =============================================================

build_tar_df <- function(y, p = 0) {
  
  Tn <- length(y)
  
  dy   <- c(NA, diff(y))              # Δy_t
  y_l1 <- c(NA, y[-Tn])               # y_{t-1}
  
  # Indicador TAR: I_t = 1{y_{t-1} ≥ 0}
  I <- ifelse(is.na(y_l1), NA, as.integer(y_l1 >= 0))
  
  z1 <- I * y_l1
  z2 <- (1 - I) * y_l1
  
  df <- data.frame(dy = dy, z1 = z1, z2 = z2)
  
  # ADF augmentation: rezagos de Δy
  if (p > 0) {
    for (i in 1:p) {
      df[[paste0("dy_l", i)]] <- c(rep(NA, i), dy[1:(length(dy) - i)])
    }
  }
  
  df <- df[complete.cases(df), ]
  
  if (nrow(df) < 12) return(NULL)
  return(df)
}

## =============================================================
## 5. CRITERIO DE INFORMACIÓN (AIC / BIC)
## =============================================================

ic_value <- function(ssr, Tn, k, ic = c("AIC", "BIC")) {
  
  ic <- match.arg(ic)
  base <- Tn * log(ssr / Tn)
  
  if (ic == "AIC") return(base + 2 * k)
  return(base + log(Tn) * k)
}

## =============================================================
## 6. CÁLCULO DEL ESTADÍSTICO Φ PARA UN p DADO
## =============================================================

phi_for_p <- function(df, p, ic) {
  
  # Modelo no restringido:
  # Δy_t ~ z1 + z2 + Δy_{t-1} + ... + Δy_{t-p}
  rhs_u <- c("z1", "z2")
  if (p > 0) rhs_u <- c(rhs_u, paste0("dy_l", 1:p))
  
  f_u <- as.formula(paste("dy ~ 0 +", paste(rhs_u, collapse = " + ")))
  m_u <- lm(f_u, data = df)
  
  ssr_u <- sum(residuals(m_u)^2)
  df_u  <- df.residual(m_u)
  k_u   <- length(coef(m_u))
  
  # Modelo restringido: ρ1 = ρ2 = 0
  if (p > 0) {
    f_r <- as.formula(paste("dy ~ 0 +", paste0("dy_l", 1:p, collapse = " + ")))
    m_r <- lm(f_r, data = df)
    ssr_r <- sum(residuals(m_r)^2)
  } else {
    ssr_r <- sum(df$dy^2)
  }
  
  # Estadístico Φ (F-test con q = 2 restricciones)
  Phi <- ((ssr_r - ssr_u) / 2) / (ssr_u / df_u)
  
  icv <- ic_value(ssr_u, nrow(df), k_u, ic)
  
  list(Phi = Phi, ic = icv)
}

## =============================================================
## 7. Φ CON SELECCIÓN ÓPTIMA DE p
## =============================================================

phi_tar <- function(y, max_lag, ic) {
  
  best_ic  <- Inf
  best_phi <- NA
  
  for (p in 0:max_lag) {
    df <- build_tar_df(y, p)
    if (is.null(df)) next
    
    tmp <- phi_for_p(df, p, ic)
    
    if (tmp$ic < best_ic) {
      best_ic  <- tmp$ic
      best_phi <- tmp$Phi
    }
  }
  
  return(best_phi)
}

## =============================================================
## 8. MONTE CARLO PARA UN T DADO (PANEL A)
## =============================================================

mc_panelA <- function(T, N, burn, max_lag, ic) {
  
  Phi_vals <- numeric(N)
  kept <- 0
  draws <- 0
  
  while (kept < N) {
    
    draws <- draws + 1
    y <- simulate_random_walk(T, burn)
    
    # Condición clave del paper
    if (!crosses_zero(y)) next
    
    phi <- phi_tar(y, max_lag, ic)
    if (!is.finite(phi)) next
    
    kept <- kept + 1
    Phi_vals[kept] <- phi
    
    if (kept %% 5000 == 0)
      message("T = ", T, " | aceptadas: ", kept, "/", N)
  }
  
  c(
    q90 = quantile(Phi_vals, 0.90),
    q95 = quantile(Phi_vals, 0.95),
    q99 = quantile(Phi_vals, 0.99)
  )
}

## =============================================================
## 9. EJECUCIÓN FINAL – PANEL A
## =============================================================

panelA_results <- data.frame(
  Sample_size = T_vec,
  `90%` = NA,
  `95%` = NA,
  `99%` = NA
)

for (i in seq_along(T_vec)) {
  
  T <- T_vec[i]
  message("\n=== Monte Carlo Panel A: T = ", T, " ===")
  
  q <- mc_panelA(
    T = T,
    N = N_mc,
    burn = burn,
    max_lag = max_lag,
    ic = ic_used
  )
  
  panelA_results[i, 2:4] <- q
}

print(panelA_results)

write.csv(panelA_results, "working-papers/working-paper-aecm/literature/enders_granger/panelA_montecarlo_results.csv", row.names = FALSE)
