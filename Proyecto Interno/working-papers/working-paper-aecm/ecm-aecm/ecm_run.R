############################################################
## ecm_run.R
## Runs: cointegration, long-run, ECM (full sample) for each mapped pair
## Enders restriction: k>=1 and same lag length for Δy and Δx (p=q=k)
## NO contemporaneous Δx_t
## Lags selected by IC using select_ecm_lag_k_full()
## Saves: one raw .rds in output/ts-ecm/
############################################################

library(tidyverse)
library(readxl)
library(lubridate)
library(aTSA)
library(purrr)

setwd("C:\\Users\\sergio.barona\\Desktop\\Least-cost-diets-and-affordability\\Proyecto Interno\\")
date_tag <- "261225"

path.in <- paste0("working-papers\\working-paper-aecm\\input\\",
                  date_tag, "_selected_foods_dataset.xlsx")

out_dir <- "working-papers\\working-paper-aecm\\output\\ts-ecm\\"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# ---- choose information criterion here
criterion_ecm <- "BIC"   # change to "AIC" if desired
max_k <- 6
min_ecm_obs <- 24

# source helpers (UPDATED)
source("working-papers\\working-paper-aecm\\ecm-aecm\\ecm_aux.R")

# -----------------------
# Mapping (LIST, city-specific)
# -----------------------
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

# De acuerdo con las pruebas de cointegración
dat = dat %>% filter(articulo_ipc %in% c("ARROZ PARA SECO",
                                         "PAPA",
                                         "PLÁTANO",
                                         "YUCA"))


# -----------------------
# Collectors
# -----------------------
coint_long <- tibble()
lr_long    <- tibble()
ecm_long   <- tibble()
lags_long  <- tibble()

keys <- dat %>% distinct(cod_mun, articulo_ipc, alimento_sipsa) %>%
  arrange(cod_mun, articulo_ipc, alimento_sipsa)

for (i in seq_len(nrow(keys))) {
  
  city_i <- keys$cod_mun[i]
  art_i  <- keys$articulo_ipc[i]
  ali_i  <- keys$alimento_sipsa[i]
  
  message("Procesando: ", city_i, " | ", art_i, " <- ", ali_i)
  
  data.food <- dat %>%
    filter(cod_mun == city_i,
           articulo_ipc == art_i,
           alimento_sipsa == ali_i) %>%
    drop_na(log_ipc, log_sipsa, mes) %>%
    arrange(fecha)
  
  if (nrow(data.food) < 36) next
  
  # 1) Cointegration p-values
  ct <- tryCatch(aTSA::coint.test(y = data.food$log_ipc, X = data.food$log_sipsa),
                 error = function(e) NULL)
  if (!is.null(ct)) {
    m <- as.matrix(ct)
    coint_long <- bind_rows(
      coint_long,
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
  
  # 2) Long-run estimates (beta on log_sipsa) [NO month dummies]
  lr_model <- lm(log_ipc ~ log_sipsa, data = data.food)
  cs_lr <- summary(lr_model)$coefficients
  if ("log_sipsa" %in% rownames(cs_lr)) {
    lr_long <- bind_rows(
      lr_long,
      tibble(
        city = city_labs[[city_i]],
        cod_mun = city_i,
        articulo_ipc = art_i,
        alimento_sipsa = ali_i,
        b = cs_lr["log_sipsa", "Estimate"],
        se = cs_lr["log_sipsa", "Std. Error"],
        p_value = cs_lr["log_sipsa", "Pr(>|t|)"]
      )
    )
  }
  
  # 3) ECM with Enders restriction: k>=1, same lag length, and NO Δx_t
  sel <- select_ecm_lag_k_full(
    data.food,
    max_k = max_k,
    criterion = criterion_ecm,
    min_ecm_obs = min_ecm_obs
  )
  if (is.null(sel)) next
  
  # Save top-5 k values
  lags_long <- bind_rows(
    lags_long,
    sel$table %>%
      slice(1:min(5, n())) %>%
      mutate(
        city = city_labs[[city_i]],
        cod_mun = city_i,
        articulo_ipc = art_i,
        alimento_sipsa = ali_i,
        criterion = criterion_ecm
      )
  )
  
  best <- sel$best
  ecm_model <- best$model
  cs <- summary(ecm_model)$coefficients
  
  ecm_long <- bind_rows(
    ecm_long,
    tibble(
      term = rownames(cs),
      b = cs[, "Estimate"],
      se = cs[, "Std. Error"],
      p_value = cs[, "Pr(>|t|)"]
    ) %>%
      mutate(
        city = city_labs[[city_i]],
        cod_mun = city_i,
        articulo_ipc = art_i,
        alimento_sipsa = ali_i,
        k_star = best$k,
        criterion = criterion_ecm
      )
  )
}

saveRDS(
  list(
    date_tag = date_tag,
    criterion_ecm = criterion_ecm,
    mapping_used = map_df,
    coint_long = coint_long,
    lr_long = lr_long,
    ecm_long = ecm_long,
    lags_long = lags_long
  ),
  file.path(out_dir, paste0(date_tag, "_ecm_raw.rds"))
)

cat("\nSaved:\n", file.path(out_dir, paste0(date_tag, "_ecm_raw.rds")), "\n")
