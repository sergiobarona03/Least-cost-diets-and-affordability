############################################################
## TAR (tau = 0) sobre residuales EG: mu_hat
## - Modelo TAR de "linear attractor"
## - Indicador en el nivel: I_t = 1{ mu_{t-1} >= 0 }
## - Tests: Phi_mu (rho1=rho2=0), Equality (rho1=rho2)
## - IC: AIC/BIC Enders; Ljung-Box Q(4)
############################################################

#-----------------------------------------------------------
# AIC/BIC como Enders–Granger:
#   AIC = T log(SSR) + 2n
#   BIC = T log(SSR) + n log(T)
#-----------------------------------------------------------
ic_enders <- function(ssr, Tn, n_par) {
  aic <- Tn * log(ssr) + 2 * n_par
  bic <- Tn * log(ssr) + n_par * log(Tn)
  list(AIC = aic, BIC = bic)
}

#-----------------------------------------------------------
# Construye dataset TAR (nivel) con tau=0:
#   Δmu_t = rho1 * I_t * mu_{t-1} + rho2 * (1-I_t) * mu_{t-1}
#           + Σ_{j=1}^p γ_j Δmu_{t-j} + e_t
#   I_t = 1{ mu_{t-1} >= 0 }
#-----------------------------------------------------------
build_tar_df_tau0 <- function(mu_hat, p = 0) {
  mu_hat <- as.numeric(mu_hat)
  Tn <- length(mu_hat)
  
  dmu   <- c(NA, diff(mu_hat))        # Δmu_t
  mu_l1 <- c(NA, mu_hat[-Tn])         # mu_{t-1}
  
  I <- ifelse(mu_l1 >= 0, 1, 0)       # tau = 0
  z1 <- I       * mu_l1               # I_t * mu_{t-1}
  z2 <- (1 - I) * mu_l1               # (1-I_t) * mu_{t-1}
  
  dfm <- data.frame(dmu = dmu, z1 = z1, z2 = z2)
  
  if (p > 0) {
    for (j in 1:p) {
      dfm[[paste0("dmu_l", j)]] <- c(rep(NA, j), dmu[1:(Tn - j)])
    }
  }
  
  dfm <- dfm[complete.cases(dfm), , drop = FALSE]
  if (nrow(dfm) < 20) stop("Insufficient observations after lagging.")
  dfm
}

#-----------------------------------------------------------
# Estima TAR (tau=0) y calcula:
# - rho1, rho2 (y t-stats)
# - Phi_mu: H0 rho1=rho2=0
# - F_equal: H0 rho1=rho2
# - AIC/BIC Enders
# - Ljung-Box Q(4) en residuales
#-----------------------------------------------------------
estimate_tar_tau0 <- function(mu_hat, p = 0, lb_lag = 4) {
  
  dfm <- build_tar_df_tau0(mu_hat, p = p)
  
  rhs <- c("z1", "z2")
  if (p > 0) rhs <- c(rhs, paste0("dmu_l", 1:p))
  fml <- as.formula(paste("dmu ~ 0 +", paste(rhs, collapse = " + ")))
  
  mod <- lm(fml, data = dfm)
  s   <- summary(mod)
  
  SSR_U <- sum(resid(mod)^2)
  Tn_m  <- nrow(dfm)
  kU    <- length(coef(mod))
  
  # (A) Phi_mu: H0 rho1 = rho2 = 0
  if (p > 0) {
    rhs_R <- paste0("dmu_l", 1:p, collapse = " + ")
    fml_R <- as.formula(paste("dmu ~ 0 +", rhs_R))
  } else {
    fml_R <- dmu ~ 0
  }
  mod_R <- lm(fml_R, data = dfm)
  SSR_R <- sum(resid(mod_R)^2)
  
  q <- 2
  Phi_mu <- ((SSR_R - SSR_U) / q) / (SSR_U / (Tn_m - kU))
  
  # (B) Equality test: H0 rho1 = rho2
  dfm_eq <- dfm
  dfm_eq$z_sum <- dfm_eq$z1 + dfm_eq$z2
  rhs_eq <- c("z_sum")
  if (p > 0) rhs_eq <- c(rhs_eq, paste0("dmu_l", 1:p))
  fml_eq <- as.formula(paste("dmu ~ 0 +", paste(rhs_eq, collapse = " + ")))
  
  mod_eq <- lm(fml_eq, data = dfm_eq)
  an <- anova(mod_eq, mod)
  F_equal  <- an$F[2]
  pF_equal <- an$`Pr(>F)`[2]
  
  # Coefs
  coefs <- coef(s)
  rho1  <- coefs["z1", "Estimate"]
  trho1 <- coefs["z1", "t value"]
  rho2  <- coefs["z2", "Estimate"]
  trho2 <- coefs["z2", "t value"]
  
  # IC Enders
  ic <- ic_enders(SSR_U, Tn_m, kU)
  
  # Ljung–Box Q(lag) en residuales
  Q <- Box.test(resid(mod), lag = lb_lag, type = "Ljung-Box", fitdf = kU)
  
  list(
    model   = mod,
    p       = p,
    tau     = 0,
    rho1    = rho1,
    t_rho1  = trho1,
    rho2    = rho2,
    t_rho2  = trho2,
    Phi_mu  = Phi_mu,
    F_equal = F_equal,
    p_equal = pF_equal,
    AIC     = ic$AIC,
    BIC     = ic$BIC,
    Q4      = as.numeric(Q$statistic),
    p_Q4    = Q$p.value
  )
}

#-----------------------------------------------------------
# Selección de p por IC (AIC o BIC), con filtro opcional
# de Ljung-Box (ruido blanco).
#-----------------------------------------------------------
select_p_tar_tau0 <- function(mu_hat,
                              p_max = 6,
                              ic = c("BIC","AIC"),
                              alpha_Q = 0.05,
                              lb_lag = 4) {
  
  ic <- match.arg(ic)
  
  fits <- vector("list", p_max + 1)
  tab <- tibble(
    p = 0:p_max,
    rho1 = NA_real_, t_rho1 = NA_real_,
    rho2 = NA_real_, t_rho2 = NA_real_,
    Phi_mu = NA_real_,
    F_equal = NA_real_, p_equal = NA_real_,
    AIC = NA_real_, BIC = NA_real_,
    Q4 = NA_real_, p_Q4 = NA_real_
  )
  
  for (p in 0:p_max) {
    fit_p <- estimate_tar_tau0(mu_hat, p = p, lb_lag = lb_lag)
    fits[[p + 1]] <- fit_p
    
    tab[p + 1, c("rho1","t_rho1","rho2","t_rho2","Phi_mu",
                 "F_equal","p_equal","AIC","BIC","Q4","p_Q4")] <-
      list(fit_p$rho1, fit_p$t_rho1, fit_p$rho2, fit_p$t_rho2,
           fit_p$Phi_mu, fit_p$F_equal, fit_p$p_equal,
           fit_p$AIC, fit_p$BIC, fit_p$Q4, fit_p$p_Q4)
  }
  
  ok <- tab %>% filter(p_Q4 >= alpha_Q)
  if (nrow(ok) > 0) {
    p_star <- ok$p[which.min(ok[[ic]])]
    note <- paste0("Selected p* by ", ic,
                   " among models with Ljung-Box p_Q4 >= ", alpha_Q, ".")
  } else {
    p_star <- tab$p[which.min(tab[[ic]])]
    note <- paste0("WARNING: no model passed Ljung-Box (p_Q4 >= ", alpha_Q,
                   "); selected p* by ", ic, " anyway.")
  }
  
  list(
    p_star = p_star,
    best   = fits[[p_star + 1]],
    table  = tab,
    note   = note
  )
}



############################################################
mu_hat <- res$mu_hat
out <- select_p_tar_tau0(mu_hat, p_max=6, ic="BIC")
## out$note
## out$p_star
## out$table
############################################################

t_stars <- function(t) {
  ifelse(abs(t) >= 2.58, "***",
         ifelse(abs(t) >= 1.96, "**",
                ifelse(abs(t) >= 1.65, "*", "")))
}

tar_table_print <- out$table %>%
  mutate(
    p_star = if_else(p == out$p_star, "p*", ""),
    rho1_fmt = sprintf("%.4f%s", rho1, t_stars(t_rho1)),
    trho1_fmt = sprintf("(%.3f)", t_rho1),
    rho2_fmt = sprintf("%.4f%s", rho2, t_stars(t_rho2)),
    trho2_fmt = sprintf("(%.3f)", t_rho2),
    Phi_mu_fmt = sprintf("%.3f", Phi_mu),
    Feq_fmt    = sprintf("%.3f", F_equal),
    peq_fmt    = sprintf("%.3f", p_equal),
    AIC_fmt    = sprintf("%.2f", AIC),
    BIC_fmt    = sprintf("%.2f", BIC),
    Q4_fmt     = sprintf("%.3f", Q4),
    pQ4_fmt    = sprintf("%.3f", p_Q4)
  ) %>%
  select(
    p, p_star,
    rho1_fmt, trho1_fmt,
    rho2_fmt, trho2_fmt,
    Phi_mu_fmt, Feq_fmt, peq_fmt,
    AIC_fmt, BIC_fmt, Q4_fmt, pQ4_fmt
  ) %>%
  rename(
    `p` = p,
    `Selected` = p_star,
    `rho1` = rho1_fmt,
    `t(rho1)` = trho1_fmt,
    `rho2` = rho2_fmt,
    `t(rho2)` = trho2_fmt,
    `Phi_mu` = Phi_mu_fmt,
    `F_equal` = Feq_fmt,
    `p_equal` = peq_fmt,
    `AIC` = AIC_fmt,
    `BIC` = BIC_fmt,
    `Q(4)` = Q4_fmt,
    `p_Q(4)` = pQ4_fmt
  )

kable(
  tar_table_print,
  booktabs = TRUE,
  caption = paste0(
    "TAR (tau = 0) lag selection over p = 0..", max(out$table$p),
    ". Selected p* by ", "BIC",
    " subject to Ljung–Box p_Q(4) >= ", "0.05",
    " (white-noise residuals)."
  )
) %>%
  kable_styling(latex_options = c("hold_position", "scale_down"), font_size = 8)