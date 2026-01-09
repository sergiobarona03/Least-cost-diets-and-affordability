############################################################
## Enders & Granger (1998)
## Asymmetric Error-Correction M-TAR (Table 6 style)
## Δr_L,t and Δr_S,t with z_plus,t-1 and z_minus,t-1
## 2 lags in A_ij(L), intercept included
############################################################

library(tidyverse)
library(lubridate)
library(readxl)

##----------------------------------------------------------
## 0. Cargar datos y definir r_L,t y r_S,t
##----------------------------------------------------------

setwd("C:/Users/Portatil/Desktop/Least-cost-diets-and-affordability/Proyecto Interno/")

dataset <- read_excel(
  "working-papers/working-paper-aecm/literature/enders_granger/dataset_enders.xlsx"
)

n_obs <- nrow(dataset)

dates_q <- seq.Date(
  from = as.Date("1958-01-01"),
  by   = "quarter",
  length.out = n_obs
)

df <- dataset %>%
  mutate(
    date = dates_q,
    rL   = r_10,      # tasa larga r_L,t
    rS   = r_short    # tasa corta r_S,t
  )

rL <- df$rL
rS <- df$rS
Tn <- length(rL)

##----------------------------------------------------------
## 1. Construir spread, indicador M-TAR e instrumentos z_plus, z_minus
##----------------------------------------------------------

# Spread en niveles: rS - rL (como en el paper)
spread_SL <- rS - rL

# Δ(rS - rL)
d_spread_SL <- c(NA, diff(spread_SL))
d_spread_SL_l1 <- c(NA, d_spread_SL[-length(d_spread_SL)])  # Δspread_{t-1}

# Indicador momentum I_t (puedes cambiar >= 0 por < 0 si quieres la otra convención)
I_t <- ifelse(d_spread_SL_l1 >= 0, 1, 0)

# Niveles en t-1
rL_l1 <- c(NA, rL[-Tn])
rS_l1 <- c(NA, rS[-Tn])

# Umbral consistente reportado por EG en rS - rL
tau_rD <- 2.64

# Término de corrección de error (exactamente como en la figura):
# z_plus,t-1  = I_t (rS,t-1 - rL,t-1 + 2.64)
# z_minus,t-1 = (1 - I_t)(rS,t-1 - rL,t-1 + 2.64)
ec_term <- rS_l1 - rL_l1 + tau_rD

z_plus_l1  <- I_t       * ec_term
z_minus_l1 <- (1 - I_t) * ec_term

##----------------------------------------------------------
## 2. Diferencias de rL y rS y rezagos (A_ij(L))
##----------------------------------------------------------

# Diferencias primeras
drL <- c(NA, diff(rL))
drS <- c(NA, diff(rS))

# EXACTAMENTE 2 rezagos
p_lags <- 2

df_ecm <- tibble(
  drL = drL,
  drS = drS,
  z_plus_l1  = z_plus_l1,
  z_minus_l1 = z_minus_l1
)

# Agregar rezagos de ΔrL,t y ΔrS,t hasta 2
for (j in 1:p_lags) {
  df_ecm[[paste0("drL_l", j)]] <- c(rep(NA, j), drL[1:(Tn - j)])
  df_ecm[[paste0("drS_l", j)]] <- c(rep(NA, j), drS[1:(Tn - j)])
}

# Eliminar NA's por rezagos e instrumentos
df_ecm <- df_ecm %>% drop_na()

##----------------------------------------------------------
## 3. Estimar las dos ecuaciones ECM asimétricas (con intercepto)
##    ΔrL,t  y  ΔrS,t
##----------------------------------------------------------

rhs_lags <- c(paste0("drL_l", 1:p_lags),
              paste0("drS_l", 1:p_lags))
rhs_all <- c(rhs_lags, "z_plus_l1", "z_minus_l1")

# Ecuación para ΔrL,t (intercepto incluido con "1 +")
fml_L <- as.formula(
  paste("drL ~ 1 +", paste(rhs_all, collapse = " + "))
)
mod_L <- lm(fml_L, data = df_ecm)
cat("\n=== ECM para ΔrL,t ===\n")
print(summary(mod_L))

# Ecuación para ΔrS,t (intercepto incluido)
fml_S <- as.formula(
  paste("drS ~ 1 +", paste(rhs_all, collapse = " + "))
)
mod_S <- lm(fml_S, data = df_ecm)
cat("\n=== ECM para ΔrS,t ===\n")
print(summary(mod_S))

##----------------------------------------------------------
## 4. F-tests tipo F_ij sobre A_ij(L) (dos rezagos)
##    F_11: rezagos de ΔrL,t en eq. de ΔrL,t
##    F_12: rezagos de ΔrS,t en eq. de ΔrL,t
##    F_21: rezagos de ΔrL,t en eq. de ΔrS,t
##    F_22: rezagos de ΔrS,t en eq. de ΔrS,t
##----------------------------------------------------------

# F_11: A_11(L) = 0 en ecuación de ΔrL,t (quitar drL_l1, drL_l2)
fml_L_noL <- update(
  fml_L,
  paste(". ~ . -", paste(paste0("drL_l", 1:p_lags), collapse = " - "))
)
mod_L_noL <- lm(fml_L_noL, data = df_ecm)
an_L_11   <- anova(mod_L_noL, mod_L)
F_11      <- an_L_11$F[2]
p_11      <- an_L_11$`Pr(>F)`[2]

# F_12: A_12(L) = 0 en ecuación de ΔrL,t (quitar drS_l1, drS_l2)
fml_L_noS <- update(
  fml_L,
  paste(". ~ . -", paste(paste0("drS_l", 1:p_lags), collapse = " - "))
)
mod_L_noS <- lm(fml_L_noS, data = df_ecm)
an_L_12   <- anova(mod_L_noS, mod_L)
F_12      <- an_L_12$F[2]
p_12      <- an_L_12$`Pr(>F)`[2]

# F_21: A_21(L) = 0 en ecuación de ΔrS,t (quitar drL_l1, drL_l2)
fml_S_noL <- update(
  fml_S,
  paste(". ~ . -", paste(paste0("drL_l", 1:p_lags), collapse = " - "))
)
mod_S_noL <- lm(fml_S_noL, data = df_ecm)
an_S_21   <- anova(mod_S_noL, mod_S)
F_21      <- an_S_21$F[2]
p_21      <- an_S_21$`Pr(>F)`[2]

# F_22: A_22(L) = 0 en ecuación de ΔrS,t (quitar drS_l1, drS_l2)
fml_S_noS <- update(
  fml_S,
  paste(". ~ . -", paste(paste0("drS_l", 1:p_lags), collapse = " - "))
)
mod_S_noS <- lm(fml_S_noS, data = df_ecm)
an_S_22   <- anova(mod_S_noS, mod_S)
F_22      <- an_S_22$F[2]
p_22      <- an_S_22$`Pr(>F)`[2]

cat("\n=== F-tests A_ij(L) = 0 con 2 rezagos ===\n")
cat("F_11 (ΔrL lags in ΔrL eq.) =", round(F_11, 3),
    "p =", round(p_11, 4), "\n")
cat("F_12 (ΔrS lags in ΔrL eq.) =", round(F_12, 3),
    "p =", round(p_12, 4), "\n")
cat("F_21 (ΔrL lags in ΔrS eq.) =", round(F_21, 3),
    "p =", round(p_21, 4), "\n")
cat("F_22 (ΔrS lags in ΔrS eq.) =", round(F_22, 3),
    "p =", round(p_22, 4), "\n")

##----------------------------------------------------------
## 5. Resumen compacto de z_plus y z_minus (para comparar con Tabla 6)
##----------------------------------------------------------

coef_L <- summary(mod_L)$coefficients
coef_S <- summary(mod_S)$coefficients

ecm_results <- tibble(
  equation   = c("ΔrL_t", "ΔrL_t", "ΔrS_t", "ΔrS_t"),
  regressor  = c("z_plus_l1", "z_minus_l1", "z_plus_l1", "z_minus_l1"),
  estimate   = c(coef_L["z_plus_l1","Estimate"],
                 coef_L["z_minus_l1","Estimate"],
                 coef_S["z_plus_l1","Estimate"],
                 coef_S["z_minus_l1","Estimate"]),
  t_value    = c(coef_L["z_plus_l1","t value"],
                 coef_L["z_minus_l1","t value"],
                 coef_S["z_plus_l1","t value"],
                 coef_S["z_minus_l1","t value"])
)

cat("\n=== Coeficientes de corrección de error asimétrica ===\n")
print(ecm_results, digits = 4)
