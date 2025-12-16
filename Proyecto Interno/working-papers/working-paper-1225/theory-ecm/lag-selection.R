
########################################################
## Ejemplo: selección de rezagos óptimos en un ECM
## (datos simulados cointegrados + búsqueda AIC/BIC)
########################################################

# -----------------------------
# 1) Simulación de datos cointegrados
# -----------------------------
set.seed(123)

T <- 240

# x_t ~ random walk
eps_x <- rnorm(T, sd = 0.05)
x <- cumsum(eps_x)

# y_t cointegrado con x_t
# y_t = a + b x_t + u_t, donde u_t es AR(1) estable
a <- 0.5
b <- 1.2
u <- numeric(T)
eta <- rnorm(T, sd = 0.05)
rho_u <- 0.7
for (t in 2:T) u[t] <- rho_u * u[t-1] + eta[t]
y <- a + b * x + u

# convertir a "data frame" y crear diferencias
df <- tibble(
  t = 1:T,
  y = y,
  x = x,
  dy = c(NA, diff(y)),
  dx = c(NA, diff(x))
)

# -----------------------------
# 2) Funciones auxiliares ECM
# -----------------------------

# Construir data para ECM con rezagos p (dy) y q (dx)
build_ecm_data <- function(df, p = 0, q = 0) {

  # Ecuación de largo plazo (EG)
  lr <- lm(y ~ x, data = df)
  df2 <- df %>%
    mutate(
      ect = resid(lr),
      ect_l1 = lag(ect)
    )
  
  # Crear rezagos de dy: dy_l1 ... dy_lp
  if (p > 0) {
    for (i in 1:p) {
      df2[[paste0("dy_l", i)]] <- dplyr::lag(df2$dy, i)
    }
  }
  
  # Crear rezagos de dx: dx_0 ... dx_lq (incluye contemporáneo j=0)
  for (j in 0:q) {
    df2[[paste0("dx_l", j)]] <- dplyr::lag(df2$dx, j)
  }
  
  # Quitar NA por diferencias + rezagos + ect_l1
  df2 <- df2 %>% drop_na(dy, ect_l1)
  
  list(data = df2, lr = lr)
}

# Ajustar ECM dado p,q y devolver info criterio
fit_ecm <- function(df, p = 0, q = 0, 
                    criterion = c("AIC", "BIC")) {
  
  criterion <- match.arg(criterion)
  
  tmp <- build_ecm_data(df, p, q)
  d <- tmp$data
  
  # armar fórmula: dy ~ ect_l1 + dy_l1..dy_lp + dx_l0..dx_lq
  rhs <- c("ect_l1")
  if (p > 0) rhs <- c(rhs, paste0("dy_l", 1:p))
  rhs <- c(rhs, paste0("dx_l", 0:q))
  
  fml <- as.formula(paste("dy ~", paste(rhs, collapse = " + ")))
  
  m <- lm(fml, data = d)
  
  xeq <- xtr <- df[c('x')]
  model1 <- ecm(df$y, xeq, xtr, includeIntercept=TRUE)
  
  crit <- if (criterion == "AIC") AIC(m) else BIC(m)
  
  list(
    model = m,
    lr = tmp$lr,
    data = d,
    p = p,
    q = q,
    criterion = criterion,
    value = crit
  )
}

# Buscar p,q óptimos en una grilla
select_ecm_lags <- function(df, max_p = 6, max_q = 6,
                            criterion = c("AIC", "BIC")) {
  criterion <- match.arg(criterion)
  
  grid <- expand.grid(p = 0:max_p, q = 0:max_q)
  results <- vector("list", nrow(grid))
  
  for (k in seq_len(nrow(grid))) {
    p <- grid$p[k]
    q <- grid$q[k]
    
    # tryCatch por si algún modelo queda singular
    results[[k]] <- tryCatch(
      fit_ecm(df, p = p, q = q, criterion = criterion),
      error = function(e) NULL
    )
  }
  
  results <- results[!sapply(results, is.null)]
  
  tab <- tibble(
    p = sapply(results, `[[`, "p"),
    q = sapply(results, `[[`, "q"),
    crit = sapply(results, `[[`, "value")
  ) %>% arrange(crit)
  
  best <- results[[which.min(sapply(results, `[[`, "value"))]]
  
  list(
    table = tab,
    best = best
  )
}

# -----------------------------
# 3) Ejecutar selección de rezagos
# -----------------------------
library(tidyverse)

sel_aic <- select_ecm_lags(df, max_p = 6, max_q = 6, criterion = "AIC")
sel_bic <- select_ecm_lags(df, max_p = 6, max_q = 6, criterion = "BIC")

# Ver mejores combinaciones
head(sel_aic$table, 10)
head(sel_bic$table, 10)

# Mejor modelo AIC
best_aic <- sel_aic$best$model
summary(best_aic)

# (opcional) ver parámetros clave
coef(summary(best_aic))[c("(Intercept)", "ect_l1"), , drop = FALSE]
