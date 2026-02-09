############################################################
## A-ECM M-TAR ITERATED (SIPSA vs IPC)
## - Runs by (cod_mun, articulo_ipc) for 4 foods (IPC)
## - Uses tau_x per group from tables_12.rds (mtar_consistent$tau_hat)
############################################################

library(tidyverse)
library(readxl)
library(knitr)
library(kableExtra)

##----------------------------------------------------------
## 0) Paths
##----------------------------------------------------------
ROOT <- "C:\\Users\\danie\\OneDrive\\Escritorio\\Least-cost-diets-and-affordability\\Proyecto Interno\\"

IN_FOODS <- file.path(ROOT, "working-papers/working-paper-aecm/input/261225_selected_foods_dataset.xlsx")

OUT_DIR  <- file.path(ROOT, "working-papers/working-paper-aecm/output-paper")
if (!dir.exists(OUT_DIR)) dir.create(OUT_DIR, recursive = TRUE)

RDS_TABLE7 <- file.path(OUT_DIR, "tables/tables_12.rds")        
RDS_AECM   <- file.path(OUT_DIR, "tables/aecm_results_12.rds")   

##----------------------------------------------------------
## 1) Load foods (our base)
##----------------------------------------------------------
foods <- read_excel(IN_FOODS) %>%
  mutate(
    date = as.Date(sprintf("%04d-%02d-01", as.integer(Year), as.integer(Month))),
    precio_ipc   = as.numeric(precio_ipc),
    precio_sipsa = as.numeric(precio_sipsa),
    log_ipc      = log(precio_ipc),
    log_sipsa    = log(precio_sipsa)
  ) %>%
  arrange(cod_mun, articulo_ipc, alimento_sipsa, date)

##----------------------------------------------------------
## 2) Foods to keep (articulo_ipc)
##----------------------------------------------------------
foods_keep_ipc <- c("ARROZ PARA SECO", "CEBOLLA CABEZONA", "PAPA", "PLÁTANO")

foods_sub <- foods %>%
  filter(articulo_ipc %in% foods_keep_ipc) %>%
  drop_na(log_sipsa, log_ipc)

##----------------------------------------------------------
## 3) Load tau_x per group from tables_12.rds
##----------------------------------------------------------
if (!file.exists(RDS_TABLE7)) {
  stop("tables_12.rds not found. Save it first. Path:\n", RDS_TABLE7)
}
tables_12 <- readRDS(RDS_TABLE7)

parse_group_name <- function(nm) {
  parts <- strsplit(nm, "__", fixed = TRUE)[[1]]
  kv <- strsplit(parts, "=", fixed = TRUE)
  out <- setNames(lapply(kv, `[`, 2), sapply(kv, `[`, 1))
  tibble(
    cod_mun = as.integer(out[["cod_mun"]]),
    articulo_ipc = out[["articulo_ipc"]]
  )
}

tau_map <- imap_dfr(tables_12, ~{
  nm <- .y
  obj <- .x
  key <- parse_group_name(nm)
  tibble(
    cod_mun = key$cod_mun,
    articulo_ipc = key$articulo_ipc,
    tau_x = as.numeric(obj$mtar_consistent$tau_hat),
    name_key = nm
  )
}) %>%
  filter(articulo_ipc %in% foods_keep_ipc)

if (nrow(tau_map) == 0) stop("No tau_x found for the selected foods in tables_12.rds. Check the naming keys.")

##----------------------------------------------------------
## 4) A-ECM function for ONE group (city-food)
##   Cointegration: log_sipsa ~ 1 + log_ipc
##   M_t: 1{ Δmu_{t-1} >= tau_x }
##   Two equations: ΔpW and ΔpR, with intercept
##----------------------------------------------------------
get_Fp <- function(mod_restricted, mod_full) {
  an <- anova(mod_restricted, mod_full)
  list(F = as.numeric(an$F[2]), p = as.numeric(an$`Pr(>F)`[2]))
}

run_aecm_mtar_one_group <- function(df_g, tau_x, p_lags = 1) {
  df_g <- df_g %>% arrange(date) %>% drop_na(log_sipsa, log_ipc)
  Tn <- nrow(df_g)
  if (Tn < 50) return(NULL)
  
  # (1) Cointegration and residuals
  reg_coint <- lm(log_sipsa ~ 1 + log_ipc, data = df_g)
  mu_hat <- resid(reg_coint)
  
  # (2) M_t and asymmetric EC terms (lagged)
  d_mu    <- c(NA, diff(mu_hat))               # Δμ_t
  d_mu_l1 <- c(NA, d_mu[-length(d_mu)])        # Δμ_{t-1}
  mu_l1   <- c(NA, mu_hat[-length(mu_hat)])    # μ_{t-1}
  
  M_t <- ifelse(d_mu_l1 >= tau_x, 1, 0)
  mu_minus_l1 <- M_t * mu_l1
  mu_plus_l1  <- (1 - M_t) * mu_l1
  
  # (3) First differences of prices (logs)
  dpW <- c(NA, diff(df_g$log_sipsa))  # ΔpW
  dpR <- c(NA, diff(df_g$log_ipc))    # ΔpR
  
  df_ecm <- tibble(
    dpW = dpW,
    dpR = dpR,
    mu_minus_l1 = mu_minus_l1,
    mu_plus_l1  = mu_plus_l1
  )
  
  # Lags for A_ij(L)
  for (j in 1:p_lags) {
    df_ecm[[paste0("dpW_l", j)]] <- c(rep(NA, j), dpW[1:(Tn - j)])
    df_ecm[[paste0("dpR_l", j)]] <- c(rep(NA, j), dpR[1:(Tn - j)])
  }
  
  df_ecm <- df_ecm %>% drop_na()
  if (nrow(df_ecm) < 30) return(NULL)
  
  rhs_lags <- c(paste0("dpW_l", 1:p_lags),
                paste0("dpR_l", 1:p_lags))
  rhs_all  <- c(rhs_lags, "mu_minus_l1", "mu_plus_l1")
  
  # (4) Estimate ECMs
  fml_W <- as.formula(paste("dpW ~ 1 +", paste(rhs_all, collapse = " + ")))
  fml_R <- as.formula(paste("dpR ~ 1 +", paste(rhs_all, collapse = " + ")))
  
  mod_W <- lm(fml_W, data = df_ecm)
  mod_R <- lm(fml_R, data = df_ecm)
  
  # (5) F-tests A_ij(L)=0, keep intercept + EC terms
  fml_W_noW <- update(
    fml_W,
    paste(". ~ 1 +",
          paste(c(paste0("dpR_l", 1:p_lags), "mu_minus_l1", "mu_plus_l1"),
                collapse = " + "))
  )
  Fp_11 <- get_Fp(lm(fml_W_noW, data = df_ecm), mod_W)
  
  fml_W_noR <- update(
    fml_W,
    paste(". ~ 1 +",
          paste(c(paste0("dpW_l", 1:p_lags), "mu_minus_l1", "mu_plus_l1"),
                collapse = " + "))
  )
  Fp_12 <- get_Fp(lm(fml_W_noR, data = df_ecm), mod_W)
  
  fml_R_noW <- update(
    fml_R,
    paste(". ~ 1 +",
          paste(c(paste0("dpR_l", 1:p_lags), "mu_minus_l1", "mu_plus_l1"),
                collapse = " + "))
  )
  Fp_21 <- get_Fp(lm(fml_R_noW, data = df_ecm), mod_R)
  
  fml_R_noR <- update(
    fml_R,
    paste(". ~ 1 +",
          paste(c(paste0("dpW_l", 1:p_lags), "mu_minus_l1", "mu_plus_l1"),
                collapse = " + "))
  )
  Fp_22 <- get_Fp(lm(fml_R_noR, data = df_ecm), mod_R)
  
  # (6) Extract asymmetric EC coefficients
  coef_W <- summary(mod_W)$coefficients
  coef_R <- summary(mod_R)$coefficients
  
  ecm_terms <- tibble(
    equation  = c("ΔpW_t", "ΔpW_t", "ΔpR_t", "ΔpR_t"),
    regressor = c("mu_minus_l1", "mu_plus_l1", "mu_minus_l1", "mu_plus_l1"),
    estimate  = c(coef_W["mu_minus_l1","Estimate"],
                  coef_W["mu_plus_l1", "Estimate"],
                  coef_R["mu_minus_l1","Estimate"],
                  coef_R["mu_plus_l1", "Estimate"]),
    t_value   = c(coef_W["mu_minus_l1","t value"],
                  coef_W["mu_plus_l1", "t value"],
                  coef_R["mu_minus_l1","t value"],
                  coef_R["mu_plus_l1", "t value"])
  )
  
  list(
    reg_coint = reg_coint,
    tau_x = tau_x,
    p_lags = p_lags,
    mod_W = mod_W,
    mod_R = mod_R,
    F_tests = tibble(
      test = c("F_11","F_12","F_21","F_22"),
      meaning = c("ΔpW lags in ΔpW eq.",
                  "ΔpR lags in ΔpW eq.",
                  "ΔpW lags in ΔpR eq.",
                  "ΔpR lags in ΔpR eq."),
      F = c(Fp_11$F, Fp_12$F, Fp_21$F, Fp_22$F),
      p = c(Fp_11$p, Fp_12$p, Fp_21$p, Fp_22$p)
    ),
    ecm_terms = ecm_terms
  )
}

##----------------------------------------------------------
## 5) Iterate: (cod_mun, articulo_ipc) using tau_map
##----------------------------------------------------------
p_lags_ecm <- 1

groups_12 <- tau_map %>%
  distinct(cod_mun, articulo_ipc, tau_x, name_key) %>%
  arrange(cod_mun, articulo_ipc)

aecm_results_12 <- pmap(
  list(groups_12$cod_mun, groups_12$articulo_ipc, groups_12$tau_x, groups_12$name_key),
  function(cod_mun, articulo_ipc, tau_x, name_key) {
    
    df_g <- foods_sub %>%
      filter(cod_mun == cod_mun, articulo_ipc == articulo_ipc) %>%
      arrange(date)
    
    fit <- run_aecm_mtar_one_group(df_g, tau_x = tau_x, p_lags = p_lags_ecm)
    if (is.null(fit)) return(NULL)
    
    list(
      cod_mun = cod_mun,
      articulo_ipc = articulo_ipc,
      name_key = name_key,
      fit = fit
    )
  }
)

aecm_results_12 <- aecm_results_12[!map_lgl(aecm_results_12, is.null)]
if (length(aecm_results_12) == 0) stop("No A-ECM results produced. Check data availability and tau_map keys.")

saveRDS(aecm_results_12, RDS_AECM)
cat("\nSaved A-ECM results to:\n", RDS_AECM, "\n")

##----------------------------------------------------------
## 6) Summary table for RMD
##----------------------------------------------------------
fmt_bt <- function(x) ifelse(is.na(x), NA_character_, sprintf("%.4f", x))
fmt_p  <- function(x) ifelse(is.na(x), NA_character_, sprintf("%.3f", x))

summary_tbl <- map_dfr(aecm_results_12, function(obj) {
  fit <- obj$fit
  e <- fit$ecm_terms
  
  W_minus <- e %>% filter(equation == "ΔpW_t", regressor == "mu_minus_l1")
  W_plus  <- e %>% filter(equation == "ΔpW_t", regressor == "mu_plus_l1")
  R_minus <- e %>% filter(equation == "ΔpR_t", regressor == "mu_minus_l1")
  R_plus  <- e %>% filter(equation == "ΔpR_t", regressor == "mu_plus_l1")
  
  Ft <- fit$F_tests %>% select(test, F, p) %>% pivot_wider(names_from = test, values_from = c(F, p))
  
  tibble(
    cod_mun = obj$cod_mun,
    articulo_ipc = obj$articulo_ipc,
    tau_x = fit$tau_x,
    p_lags = fit$p_lags,
    
    aW_minus = W_minus$estimate, tW_minus = W_minus$t_value,
    aW_plus  = W_plus$estimate,  tW_plus  = W_plus$t_value,
    aR_minus = R_minus$estimate, tR_minus = R_minus$t_value,
    aR_plus  = R_plus$estimate,  tR_plus  = R_plus$t_value,
    
    F_11 = Ft$F_F_11, p_11 = Ft$p_F_11,
    F_12 = Ft$F_F_12, p_12 = Ft$p_F_12,
    F_21 = Ft$F_F_21, p_21 = Ft$p_F_21,
    F_22 = Ft$F_F_22, p_22 = Ft$p_F_22
  )
}) %>%
  mutate(
    group = paste0("cod_mun=", cod_mun, " | articulo_ipc=", articulo_ipc),
    tau_x = round(tau_x, 5),
    
    `ΔpW: μ-` = sprintf("%s (%s)", fmt_bt(aW_minus), fmt_bt(tW_minus)),
    `ΔpW: μ+` = sprintf("%s (%s)", fmt_bt(aW_plus),  fmt_bt(tW_plus)),
    `ΔpR: μ-` = sprintf("%s (%s)", fmt_bt(aR_minus), fmt_bt(tR_minus)),
    `ΔpR: μ+` = sprintf("%s (%s)", fmt_bt(aR_plus),  fmt_bt(tR_plus)),
    
    `F_11 (p)` = sprintf("%s (%s)", fmt_bt(F_11), fmt_p(p_11)),
    `F_12 (p)` = sprintf("%s (%s)", fmt_bt(F_12), fmt_p(p_12)),
    `F_21 (p)` = sprintf("%s (%s)", fmt_bt(F_21), fmt_p(p_21)),
    `F_22 (p)` = sprintf("%s (%s)", fmt_bt(F_22), fmt_p(p_22))
  ) %>%
  select(group, tau_x, p_lags,
         `ΔpW: μ-`, `ΔpW: μ+`, `ΔpR: μ-`, `ΔpR: μ+`,
         `F_11 (p)`, `F_12 (p)`, `F_21 (p)`, `F_22 (p)`)

kable(
  summary_tbl,
  booktabs = TRUE,
  caption = "A-ECM M-TAR summary by city-food group (SIPSA vs IPC). EC terms shown as estimate (t). F-tests shown as F (p)."
) %>%
  kable_styling(full_width = FALSE) %>%
  scroll_box(height = "450px")
