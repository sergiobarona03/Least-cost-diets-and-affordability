############################################################
## ecm_es_nonlinear_tables.R
## Tablas para modelos no lineales Enders–Siklos style:
##  - TAR nivel
##  - TAR momentum
##  - M-TAR
##  - ECM M-TAR (coeficientes y F-tests)
############################################################

rm(list = ls())

library(tidyverse)
library(writexl)

#-----------------------------------------------------------
# 0. Paths and load nonlinear RDS
#-----------------------------------------------------------

setwd("C:\\Users\\sergio.barona\\Desktop\\Least-cost-diets-and-affordability\\Proyecto Interno\\")
date_tag <- "261225"

out_dir  <- "working-papers\\working-paper-aecm\\output\\ts-ecm\\"
rds_path <- file.path(out_dir, paste0(date_tag, "_ecm_es_nonlinear_raw.rds"))

obj <- readRDS(rds_path)

tar_level_long   <- obj$tar_level_long
tar_mom_long     <- obj$tar_mom_long
mtar_long        <- obj$mtar_long
ecm_mtar_coef    <- obj$ecm_mtar_coef
ecm_mtar_Ftests  <- obj$ecm_mtar_Ftests

pair_lab <- function(art, ali) paste0(art, " / ", ali)

#===========================================================
# 1. Tabla TAR nivel
#===========================================================

tab_tar_level <- tar_level_long %>%
  dplyr::mutate(
    pair = pair_lab(articulo_ipc, alimento_sipsa)
  ) %>%
  dplyr::select(
    city,
    articulo_ipc,
    alimento_sipsa,
    pair,
    p,
    rho1,  t_rho1,
    rho2,  t_rho2,
    Phi_mu,
    F_equal, p_equal,
    AIC, BIC,
    Q4, p_Q4
  ) %>%
  dplyr::arrange(articulo_ipc, city, alimento_sipsa, p)

#===========================================================
# 2. Tabla TAR momentum
#===========================================================

tab_tar_mom <- tar_mom_long %>%
  dplyr::mutate(
    pair = pair_lab(articulo_ipc, alimento_sipsa)
  ) %>%
  dplyr::select(
    city,
    articulo_ipc,
    alimento_sipsa,
    pair,
    p,
    rho1,  t_rho1,
    rho2,  t_rho2,
    Phi_mu,
    F_equal, p_equal,
    AIC, BIC,
    Q4, p_Q4
  ) %>%
  dplyr::arrange(articulo_ipc, city, alimento_sipsa, p)

#===========================================================
# 3. Tabla M-TAR
#===========================================================

tab_mtar <- mtar_long %>%
  dplyr::mutate(
    pair = pair_lab(articulo_ipc, alimento_sipsa)
  ) %>%
  dplyr::select(
    city,
    articulo_ipc,
    alimento_sipsa,
    pair,
    p,
    tau,
    rho1,  t_rho1,
    rho2,  t_rho2,
    Phi_mu,
    F_equal, p_equal,
    AIC, BIC,
    Q4, p_Q4
  ) %>%
  dplyr::arrange(articulo_ipc, city, alimento_sipsa, p)

#===========================================================
# 4. Tablas ECM M-TAR
#===========================================================

pretty_equation <- function(eq) {
  if (eq == "Δr_f,t")   return("Δlog_ipc,t")
  if (eq == "Δr_10,t")  return("Δlog_sipsa,t")
  eq
}

pretty_regressor <- function(reg) {
  if (reg == "mu_minus_l1") return("μ_minus,t-1")
  if (reg == "mu_plus_l1")  return("μ_plus,t-1")
  reg
}

tab_ecm_mtar_coef <- ecm_mtar_coef %>%
  dplyr::mutate(
    pair      = pair_lab(articulo_ipc, alimento_sipsa),
    equation2 = vapply(equation,  pretty_equation,  character(1)),
    reg2      = vapply(regressor, pretty_regressor, character(1))
  ) %>%
  dplyr::select(
    city,
    articulo_ipc,
    alimento_sipsa,
    pair,
    tau_x,
    equation   = equation2,
    regressor  = reg2,
    estimate,
    t_value
  ) %>%
  dplyr::arrange(articulo_ipc, city, alimento_sipsa, equation, regressor)

tab_ecm_mtar_F <- ecm_mtar_Ftests %>%
  dplyr::mutate(
    pair = pair_lab(articulo_ipc, alimento_sipsa)
  ) %>%
  dplyr::rename(
    F_stat  = F,
    p_value = p
  ) %>%
  dplyr::select(
    city,
    articulo_ipc,
    alimento_sipsa,
    pair,
    tau_x,
    test,
    F_stat,
    p_value
  ) %>%
  dplyr::arrange(articulo_ipc, city, alimento_sipsa, test)

#===========================================================
# 5. Exportar a Excel
#===========================================================

write_xlsx(
  list("TAR_level" = tab_tar_level),
  file.path(out_dir, paste0(date_tag, "_nl_table1_tar_level.xlsx"))
)

write_xlsx(
  list("TAR_momentum" = tab_tar_mom),
  file.path(out_dir, paste0(date_tag, "_nl_table2_tar_momentum.xlsx"))
)

write_xlsx(
  list("M_TAR" = tab_mtar),
  file.path(out_dir, paste0(date_tag, "_nl_table3_mtar.xlsx"))
)

write_xlsx(
  list("ECM_M_TAR_coef"   = tab_ecm_mtar_coef,
       "ECM_M_TAR_Ftests" = tab_ecm_mtar_F),
  file.path(out_dir, paste0(date_tag, "_nl_table4_ecm_mtar.xlsx"))
)

cat("\nSaved nonlinear ES tables in:\n", out_dir, "\n")
