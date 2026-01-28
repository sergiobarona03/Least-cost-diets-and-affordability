############################################################
## M-TAR MOMENTUM (tau = 0) on EG residuals: mu_hat
## - Momentum TAR (Enders & Siklos / Enders & Granger style)
## - Indicator based on change: I_t = 1{ Δmu_{t-1} >= 0 }
## - Tests: Phi_mu (rho1=rho2=0), Equality (rho1=rho2)
## - IC: AIC/BIC Enders
## - Ljung-Box: (compute Q(1)..Q(h_max), report Q(h_max))
############################################################

library(tidyverse)
library(knitr)
library(kableExtra)

##-----------------------------------------------------------
## AIC/BIC as in Enders–Granger:
##   AIC = T log(SSR) + 2n
##   BIC = T log(SSR) + n log(T)
##-----------------------------------------------------------
ic_enders <- function(ssr, Tn, n_par) {
  aic <- Tn * log(ssr) + 2 * n_par
  bic <- Tn * log(ssr) + n_par * log(Tn)
  list(AIC = aic, BIC = bic)
}

##-----------------------------------------------------------
## Ljung–Box diagnostics (EViews-style)
## - Compute Q(h) for h = 1..h_max
## - Report Q(h_max) and p-value at h_max
## NOTE: We do NOT use fitdf here to avoid NaNs (lag - fitdf <= 0).
##-----------------------------------------------------------
ljung_box_eviews <- function(e, h_max = 8) {
  out <- tibble(h = 1:h_max, Q = NA_real_, p = NA_real_)
  for (hh in 1:h_max) {
    bt <- Box.test(e, lag = hh, type = "Ljung-Box")
    out$Q[out$h == hh] <- as.numeric(bt$statistic)
    out$p[out$h == hh] <- bt$p.value
  }
  list(
    table  = out,
    Q_last = out$Q[out$h == h_max],
    p_last = out$p[out$h == h_max]
  )
}

##-----------------------------------------------------------
## Build M-TAR MOMENTUM regression dataset (tau = 0):
##   Δmu_t = rho1 * I_t * mu_{t-1} + rho2 * (1-I_t) * mu_{t-1}
##           + Σ_{j=1}^p γ_j Δmu_{t-j} + e_t
##   I_t = 1{ Δmu_{t-1} >= 0 }
##-----------------------------------------------------------
build_mtar_mom_df_tau0 <- function(mu_hat, p = 0) {
  mu_hat <- as.numeric(mu_hat)
  Tn <- length(mu_hat)
  
  dmu   <- c(NA, diff(mu_hat))        # Δmu_t
  mu_l1 <- c(NA, mu_hat[-Tn])         # mu_{t-1}
  
  dmu_l1 <- c(NA, diff(mu_l1))        # Δmu_{t-1} (momentum threshold variable)
  
  I <- ifelse(dmu_l1 >= 0, 1, 0)      # tau = 0
  
  z1 <- I * mu_l1
  z2 <- (1 - I) * mu_l1
  
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

##-----------------------------------------------------------
## Estimate M-TAR MOMENTUM (tau=0) and compute stats
##-----------------------------------------------------------
estimate_mtar_mom_tau0 <- function(mu_hat, p = 0, h_max = 8) {
  
  dfm <- build_mtar_mom_df_tau0(mu_hat, p = p)
  
  rhs <- c("z1", "z2")
  if (p > 0) rhs <- c(rhs, paste0("dmu_l", 1:p))
  fml <- as.formula(paste("dmu ~ 0 +", paste(rhs, collapse = " + ")))
  
  mod <- lm(fml, data = dfm)
  s   <- summary(mod)
  
  SSR_U <- sum(resid(mod)^2)
  Tn_m  <- nrow(dfm)
  kU    <- length(coef(mod))
  
  ## (A) Phi_mu: H0 rho1 = rho2 = 0
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
  
  ## (B) Equality test: H0 rho1 = rho2
  dfm_eq <- dfm
  dfm_eq$z_sum <- dfm_eq$z1 + dfm_eq$z2
  rhs_eq <- c("z_sum")
  if (p > 0) rhs_eq <- c(rhs_eq, paste0("dmu_l", 1:p))
  fml_eq <- as.formula(paste("dmu ~ 0 +", paste(rhs_eq, collapse = " + ")))
  mod_eq <- lm(fml_eq, data = dfm_eq)
  
  an <- anova(mod_eq, mod)
  F_equal  <- an$F[2]
  pF_equal <- an$`Pr(>F)`[2]
  
  ## Coefs
  coefs <- coef(s)
  rho1  <- coefs["z1", "Estimate"]
  trho1 <- coefs["z1", "t value"]
  rho2  <- coefs["z2", "Estimate"]
  trho2 <- coefs["z2", "t value"]
  
  ## IC Enders
  ic <- ic_enders(SSR_U, Tn_m, kU)
  
  ## Ljung–Box EViews-style
  lb <- ljung_box_eviews(resid(mod), h_max = h_max)
  
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
    Qh      = lb$Q_last,
    p_Qh    = lb$p_last,
    lb_grid = lb$table
  )
}

##-----------------------------------------------------------
## Select p by IC subject to Ljung-Box p_Q(h_max) >= alpha_Q
##-----------------------------------------------------------
select_p_mtar_mom_tau0 <- function(mu_hat,
                                   p_max = 6,
                                   ic = c("BIC","AIC"),
                                   alpha_Q = 0.05,
                                   h_max = 8) {
  
  ic <- match.arg(ic)
  
  fits <- vector("list", p_max + 1)
  
  tab <- tibble(
    p = 0:p_max,
    rho1 = NA_real_, t_rho1 = NA_real_,
    rho2 = NA_real_, t_rho2 = NA_real_,
    Phi_mu = NA_real_,
    F_equal = NA_real_, p_equal = NA_real_,
    AIC = NA_real_, BIC = NA_real_,
    Qh = NA_real_, p_Qh = NA_real_
  )
  
  for (p in 0:p_max) {
    fit_p <- estimate_mtar_mom_tau0(mu_hat, p = p, h_max = h_max)
    fits[[p + 1]] <- fit_p
    
    tab[p + 1, c("rho1","t_rho1","rho2","t_rho2","Phi_mu",
                 "F_equal","p_equal","AIC","BIC","Qh","p_Qh")] <-
      list(fit_p$rho1, fit_p$t_rho1,
           fit_p$rho2, fit_p$t_rho2,
           fit_p$Phi_mu,
           fit_p$F_equal, fit_p$p_equal,
           fit_p$AIC, fit_p$BIC,
           fit_p$Qh, fit_p$p_Qh)
  }
  
  ok <- tab %>% filter(p_Qh >= alpha_Q)
  if (nrow(ok) > 0) {
    p_star <- ok$p[which.min(ok[[ic]])]
    note <- paste0("Selected p* by ", ic,
                   " among models with Ljung–Box p_Q(", h_max, ") >= ", alpha_Q, ".")
  } else {
    p_star <- tab$p[which.min(tab[[ic]])]
    note <- paste0("WARNING: no model passed Ljung–Box (p_Q(", h_max, ") >= ", alpha_Q,
                   "); selected p* by ", ic, " anyway.")
  }
  
  list(
    p_star = p_star,
    best   = fits[[p_star + 1]],
    table  = tab,
    note   = note
  )
}

##-----------------------------------------------------------
## Table printing (kable) like your TAR table
##-----------------------------------------------------------
t_stars <- function(t) {
  ifelse(abs(t) >= 2.58, "***",
         ifelse(abs(t) >= 1.96, "**",
                ifelse(abs(t) >= 1.65, "*", "")))
}

print_mtar_mom_kable <- function(out, ic_label = "BIC", alpha_Q = 0.05, h_max = 8) {
  
  tab_print <- out$table %>%
    mutate(
      Selected = if_else(p == out$p_star, "p*", ""),
      rho1_fmt = sprintf("%.4f%s", rho1, t_stars(t_rho1)),
      trho1_fmt = sprintf("(%.3f)", t_rho1),
      rho2_fmt = sprintf("%.4f%s", rho2, t_stars(t_rho2)),
      trho2_fmt = sprintf("(%.3f)", t_rho2),
      Phi_mu_fmt = sprintf("%.3f", Phi_mu),
      Feq_fmt    = sprintf("%.3f", F_equal),
      peq_fmt    = sprintf("%.3f", p_equal),
      AIC_fmt    = sprintf("%.2f", AIC),
      BIC_fmt    = sprintf("%.2f", BIC),
      Qh_fmt     = sprintf("%.3f", Qh),
      pQh_fmt    = sprintf("%.3f", p_Qh)
    ) %>%
    select(
      p, Selected,
      rho1_fmt, trho1_fmt,
      rho2_fmt, trho2_fmt,
      Phi_mu_fmt, Feq_fmt, peq_fmt,
      AIC_fmt, BIC_fmt, Qh_fmt, pQh_fmt
    ) %>%
    rename(
      `p` = p,
      `Selected` = Selected,
      `rho1` = rho1_fmt,
      `t(rho1)` = trho1_fmt,
      `rho2` = rho2_fmt,
      `t(rho2)` = trho2_fmt,
      `Phi_mu` = Phi_mu_fmt,
      `F_equal` = Feq_fmt,
      `p_equal` = peq_fmt,
      `AIC` = AIC_fmt,
      `BIC` = BIC_fmt,
      Qh = Qh_fmt,
      p_Qh = pQh_fmt
    )
  
  ## Dynamic column names (this is the fix)
  names(tab_print)[names(tab_print) == "Qh"]   <- paste0("Q(", h_max, ")")
  names(tab_print)[names(tab_print) == "p_Qh"] <- paste0("p_Q(", h_max, ")")
  
  kable(
    tab_print,
    booktabs = TRUE,
    caption = paste0(
      "M-TAR momentum (tau = 0) lag selection over p = 0..", max(out$table$p),
      ". Selected p* by ", ic_label,
      " subject to Ljung–Box p_Q(", h_max, ") >= ", alpha_Q, "."
    )
  ) %>%
    kable_styling(latex_options = c("hold_position", "scale_down"), font_size = 8)
}

############################################################
## Example run
############################################################
mu_hat <- res$mu_hat

out_mtar_mom <- select_p_mtar_mom_tau0(
  mu_hat,
  p_max = 6,
  ic = "BIC",
  alpha_Q = 0.05,
  h_max = 8
)

out_mtar_mom$note
print_mtar_mom_kable(out_mtar_mom, ic_label = "BIC", alpha_Q = 0.05, h_max = 8)

# Ljung-Box grid for the selected p*
out_mtar_mom$best$lb_grid

