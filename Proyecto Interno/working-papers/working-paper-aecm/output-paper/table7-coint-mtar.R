############################################################
##                  A-ECM M-TAR (ONLY RETAIL EQUATION)
##                 SIPSA (mayorista) -> IPC (minorista)
## - Groups: (cod_mun, articulo_ipc) for 4 foods
## - tau_x and (rho1,rho2) come from tables_12.rds (mtar_consistent)
## - A-ECM equation estimated: Δp_min,t
## - Lag length p selected by IC (BIC/AIC) subject to Ljung-Box p-value
############################################################

library(tidyverse)
library(readxl)
library(knitr)
library(kableExtra)

##----------------------------------------------------------
## 0) Paths
##----------------------------------------------------------
root <- "C:\\Users\\danie\\OneDrive\\Escritorio\\Least-cost-diets-and-affordability\\Proyecto Interno\\"

in_foods   <- file.path(root, "working-papers/working-paper-aecm/input/261225_selected_foods_dataset.xlsx")
out_dir    <- file.path(root, "working-papers/working-paper-aecm/output-paper")
dir_tables <- file.path(out_dir, "tables")

if (!dir.exists(dir_tables)) dir.create(dir_tables, recursive = TRUE)

rds_table7 <- file.path(dir_tables, "tables_12_ipc_dep.rds")
rds_aecm   <- file.path(dir_tables, "aecm_retail_results_12.rds")

##----------------------------------------------------------
## 1) Load foods (our base)
##----------------------------------------------------------
foods <- read_excel(in_foods) %>%
  mutate(
    date = as.Date(sprintf("%04d-%02d-01", as.integer(Year), as.integer(Month))),
    precio_ipc   = as.numeric(precio_ipc),
    precio_sipsa = as.numeric(precio_sipsa),
    log_ipc      = log(precio_ipc),
    log_sipsa    = log(precio_sipsa)
  ) %>%
  arrange(cod_mun, articulo_ipc, alimento_sipsa, date)

foods_keep_ipc <- c("ARROZ PARA SECO", "CEBOLLA CABEZONA", "PAPA", "PLÁTANO")

foods_sub <- foods %>%
  filter(articulo_ipc %in% foods_keep_ipc) %>%
  drop_na(log_sipsa, log_ipc)

##----------------------------------------------------------
## 2) Load tau_x (and rho1,rho2) per group from tables_12.rds
##----------------------------------------------------------
if (!file.exists(rds_table7)) {
  stop("tables_12.rds not found. Save it first at:\n", rds_table7)
}
tables_12 <- readRDS(rds_table7)

parse_group_name <- function(nm) {
  parts <- strsplit(nm, "__", fixed = TRUE)[[1]]
  kv <- strsplit(parts, "=", fixed = TRUE)
  out <- setNames(lapply(kv, `[`, 2), sapply(kv, `[`, 1))
  tibble(
    cod_mun      = as.integer(out[["cod_mun"]]),
    articulo_ipc = out[["articulo_ipc"]]
  )
}

tau_map <- imap_dfr(tables_12, ~{
  nm  <- .y
  obj <- .x
  key <- parse_group_name(nm)
  
  tau_x  <- as.numeric(obj$mtar_consistent$tau_hat)
  rho1   <- as.numeric(obj$mtar_consistent$best$rho1)
  rho2   <- as.numeric(obj$mtar_consistent$best$rho2)
  p_mtar <- as.integer(obj$mtar_consistent$p_star)
  
  tibble(
    cod_mun      = key$cod_mun,
    articulo_ipc = key$articulo_ipc,
    name_key     = nm,
    tau_x        = tau_x,
    rho1_mtar    = rho1,
    rho2_mtar    = rho2,
    p_mtar       = p_mtar
  )
}) %>%
  filter(articulo_ipc %in% foods_keep_ipc)

if (nrow(tau_map) == 0) stop("No tau_x found for selected foods in tables_12.rds.")

##----------------------------------------------------------
## 3) Helpers: Enders IC + Ljung-Box
##----------------------------------------------------------
ic_enders <- function(ssr, Tn, n_par) {
  aic <- Tn * log(ssr) + 2 * n_par
  bic <- Tn * log(ssr) + n_par * log(Tn)
  list(AIC = aic, BIC = bic)
}

lb_pvalue <- function(e, h = 8) {
  bt <- Box.test(e, lag = h, type = "Ljung-Box")
  as.numeric(bt$p.value)
}

##----------------------------------------------------------
## 4) Build A-ECM dataset for ONE group, given tau_x and p lags
##    FIXED: use seq_len(p_lags) and dplyr::lag() to avoid j=0
##----------------------------------------------------------
build_aecm_df <- function(df_g, tau_x, p_lags = 1) {
  
  df_g <- df_g %>% arrange(date) %>% drop_na(log_sipsa, log_ipc)
  Tn <- nrow(df_g)
  if (Tn < 50) return(NULL)
  
  # cointegration (your current convention)
  reg_coint <- lm(log_sipsa ~ 1 + log_ipc, data = df_g)
  mu_hat <- resid(reg_coint)
  
  d_mu    <- c(NA, diff(mu_hat))
  d_mu_l1 <- c(NA, d_mu[-length(d_mu)])
  mu_l1   <- c(NA, mu_hat[-length(mu_hat)])
  
  M_t <- ifelse(d_mu_l1 >= tau_x, 1, 0)
  mu_minus_l1 <- M_t * mu_l1
  mu_plus_l1  <- (1 - M_t) * mu_l1
  
  dpW <- c(NA, diff(df_g$log_sipsa))  # Δp^may
  dpR <- c(NA, diff(df_g$log_ipc))    # Δp^min
  
  df_ecm <- tibble(
    dpW = dpW,
    dpR = dpR,
    mu_minus_l1 = mu_minus_l1,
    mu_plus_l1  = mu_plus_l1
  )
  
  # ✅ IMPORTANT FIX: no 1:0, no l0 columns
  if (p_lags > 0) {
    for (j in seq_len(p_lags)) {
      df_ecm[[paste0("dpW_l", j)]] <- dplyr::lag(dpW, j)
      df_ecm[[paste0("dpR_l", j)]] <- dplyr::lag(dpR, j)
    }
  }
  
  df_ecm <- df_ecm %>% drop_na()
  if (nrow(df_ecm) < 30) return(NULL)
  
  list(df_ecm = df_ecm, reg_coint = reg_coint)
}

##----------------------------------------------------------
## 5) Fit retail equation for given p, return IC + LB
##    FIXED: rhs terms built safely so p=0 -> empty lag set
##----------------------------------------------------------
fit_retail_eq <- function(df_g, tau_x, p_lags = 1, h_lb = 8) {
  
  built <- build_aecm_df(df_g, tau_x, p_lags = p_lags)
  if (is.null(built)) return(NULL)
  
  df_ecm <- built$df_ecm
  
  rhs_lags <- if (p_lags > 0) {
    c(paste0("dpR_l", seq_len(p_lags)),
      paste0("dpW_l", seq_len(p_lags)))
  } else character(0)
  
  rhs_all <- c(rhs_lags, "mu_minus_l1", "mu_plus_l1")
  
  fml_R <- as.formula(paste("dpR ~ 1 +", paste(rhs_all, collapse = " + ")))
  mod_R <- lm(fml_R, data = df_ecm)
  
  SSR <- sum(resid(mod_R)^2)
  Tn_m <- nrow(df_ecm)
  k    <- length(coef(mod_R))
  
  ic <- ic_enders(SSR, Tn_m, k)
  p_lb <- lb_pvalue(resid(mod_R), h = h_lb)
  
  list(
    p_lags = p_lags,
    mod_R  = mod_R,
    reg_coint = built$reg_coint,
    AIC = ic$AIC,
    BIC = ic$BIC,
    p_LB = p_lb,
    h_LB = h_lb
  )
}

##----------------------------------------------------------
## 6) Select p* by IC subject to Ljung-Box on retail equation
##----------------------------------------------------------
select_p_retail <- function(df_g, tau_x,
                            p_max = 6,
                            ic = c("BIC","AIC"),
                            alpha_LB = 0.05,
                            h_lb = 8) {
  
  ic <- match.arg(ic)
  
  fits <- map(0:p_max, ~fit_retail_eq(df_g, tau_x, p_lags = .x, h_lb = h_lb))
  
  keep <- !map_lgl(fits, is.null)
  fits <- fits[keep]
  pset <- (0:p_max)[keep]
  
  if (length(fits) == 0) return(NULL)
  
  tab <- tibble(
    p = pset,
    AIC = map_dbl(fits, "AIC"),
    BIC = map_dbl(fits, "BIC"),
    p_LB = map_dbl(fits, "p_LB")
  )
  
  ok <- tab %>% filter(p_LB >= alpha_LB)
  
  if (nrow(ok) > 0) {
    p_star <- ok$p[which.min(ok[[ic]])]
    note <- paste0("Selected p* by ", ic, " among models with Ljung-Box p(Q(", h_lb, ")) >= ", alpha_LB, ".")
  } else {
    p_star <- tab$p[which.min(tab[[ic]])]
    note <- paste0("WARNING: no model passed Ljung-Box; selected p* by ", ic, " anyway.")
  }
  
  best_fit <- fits[[which(pset == p_star)]]
  
  list(p_star = p_star, best = best_fit, table = tab, note = note)
}

##----------------------------------------------------------
## 7) Iterate all groups and save
##----------------------------------------------------------
groups_12 <- tau_map %>%
  distinct(cod_mun, articulo_ipc, tau_x, rho1_mtar, rho2_mtar, p_mtar, name_key) %>%
  arrange(cod_mun, articulo_ipc)

aecm_retail_results <- pmap(
  list(groups_12$cod_mun, groups_12$articulo_ipc, groups_12$tau_x,
       groups_12$rho1_mtar, groups_12$rho2_mtar, groups_12$p_mtar, groups_12$name_key),
  function(cod_mun_i, articulo_i, tau_x_i, rho1_i, rho2_i, p_mtar_i, name_key_i) {
    
    df_g <- foods_sub %>%
      filter(cod_mun == cod_mun_i, articulo_ipc == articulo_i) %>%
      arrange(date)
    
    sel <- select_p_retail(
      df_g, tau_x = tau_x_i,
      p_max = 6,
      ic = "BIC",
      alpha_LB = 0.05,
      h_lb = 8
    )
    if (is.null(sel)) return(NULL)
    
    list(
      cod_mun = cod_mun_i,
      articulo_ipc = articulo_i,
      name_key = name_key_i,
      
      # from MTAR-consistent (Table 7 step)
      tau_x = tau_x_i,
      rho1_mtar = rho1_i,
      rho2_mtar = rho2_i,
      p_mtar = p_mtar_i,
      
      # from A-ECM retail eq selection
      p_ecm = sel$p_star,
      ic_table = sel$table,
      note = sel$note,
      mod_retail = sel$best$mod_R,
      reg_coint  = sel$best$reg_coint,
      lb_h = sel$best$h_LB,
      lb_p = sel$best$p_LB
    )
  }
)

aecm_retail_results <- aecm_retail_results[!map_lgl(aecm_retail_results, is.null)]
if (length(aecm_retail_results) == 0) stop("No A-ECM retail results produced.")

saveRDS(aecm_retail_results, rds_aecm)
cat("\nSaved A-ECM RETAIL results to:\n", rds_aecm, "\n")

##----------------------------------------------------------
## 8) One summary table (for RMD)
##----------------------------------------------------------
fmt_et <- function(b, t) ifelse(is.na(b) | is.na(t), NA_character_, sprintf("%.4f (%.3f)", b, t))

extract_retail_coefs <- function(mod, p_ecm) {
  cc <- summary(mod)$coefficients
  rn <- rownames(cc)
  
  lag_terms <- if (p_ecm > 0) {
    c(paste0("dpR_l", seq_len(p_ecm)),
      paste0("dpW_l", seq_len(p_ecm)))
  } else character(0)
  
  wanted <- c("(Intercept)", lag_terms, "mu_plus_l1", "mu_minus_l1")
  wanted <- wanted[wanted %in% rn]
  
  tibble(
    term = wanted,
    value = map_chr(wanted, ~fmt_et(cc[.x,"Estimate"], cc[.x,"t value"]))
  ) %>% pivot_wider(names_from = term, values_from = value)
}

summary_tbl <- map_dfr(aecm_retail_results, function(obj) {
  coefs_wide <- extract_retail_coefs(obj$mod_retail, obj$p_ecm)
  
  tibble(
    group = obj$name_key,
    cod_mun = obj$cod_mun,
    articulo_ipc = obj$articulo_ipc,
    tau_x = round(obj$tau_x, 5),
    `rho1 (MTAR)` = round(obj$rho1_mtar, 4),
    `rho2 (MTAR)` = round(obj$rho2_mtar, 4),
    p_mtar = obj$p_mtar,
    p_ecm  = obj$p_ecm,
    `LB p(Q)` = round(obj$lb_p, 3)
  ) %>%
    bind_cols(coefs_wide)
})

kable(
  summary_tbl,
  booktabs = TRUE,
  caption = "A-ECM (Δp^min_t) con selección de rezagos por BIC y filtro Ljung-Box. Incluye tau_x y (rho1,rho2) del M-TAR"
) %>%
  kable_styling(full_width = FALSE) %>%
  scroll_box(height = "450px")
