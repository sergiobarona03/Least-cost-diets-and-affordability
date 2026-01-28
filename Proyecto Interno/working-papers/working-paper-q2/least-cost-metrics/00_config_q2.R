########################################################
## 00_config_q2.R
## Config global + helpers + objetos FoodpriceR
## (Ligero: NO carga datasets pesados)
########################################################

suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(readxl)
  library(readr)
  library(writexl)
  library(FoodpriceR)
})

# -----------------------------
# Parámetros
# -----------------------------
base_dir  <- "C:/Users/Portatil/Desktop/Least-cost-diets-and-affordability/Proyecto Interno"
date_tag  <- "121225"

cities_use <- c("CALI", "BOGOTÁ D.C.", "MEDELLÍN")

# -----------------------------
# Rutas de trabajo (Q2 project)
# -----------------------------
q2_out_dir <- file.path(base_dir, "working-papers", "working-paper-q2", "output")
dir.create(q2_out_dir, recursive = TRUE, showWarnings = FALSE)

tcac_dir <- file.path(q2_out_dir, "tcac")
dir.create(tcac_dir, recursive = TRUE, showWarnings = FALSE)

q2_fullsample_dir <- file.path(q2_out_dir, "q2_fullsample")
dir.create(q2_fullsample_dir, recursive = TRUE, showWarnings = FALSE)

# -----------------------------
# Inputs
# -----------------------------
# 1) Predicciones IPC desde SIPSA (DEBE existir, generado por tu script de márgenes)
#    Usamos la versión "wholesale_by_city" (SIPSA completa + predicción)
in_prices_pred <- file.path(
  q2_fullsample_dir,
  paste0(date_tag, "_ipc_pred_from_sipsa_full_by_city.rds")
)

# 2) TCAC mapping file
in_tcac_map <- file.path(
  base_dir,
  "composicion-nut/1823_mapeo_sipsa_tcac v1.0_2025.xlsx"
)

# -----------------------------
# Helpers
# -----------------------------
safe_name <- function(x) {
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  x <- gsub("[^A-Za-z0-9_]+", "_", x)
  x <- gsub("_+", "_", x)
  x <- gsub("^_|_$", "", x)
  x
}

# Cargar objetos FoodpriceR si no existen aún
load_foodpricer_obj <- function(obj_name) {
  if (exists(obj_name, envir = .GlobalEnv)) return(invisible(TRUE))
  ok <- tryCatch({
    data(list = obj_name, package = "FoodpriceR", envir = .GlobalEnv)
    TRUE
  }, error = function(e) FALSE)
  if (!ok) warning("Could not load FoodpriceR object: ", obj_name)
  invisible(ok)
}

invisible(load_foodpricer_obj("EER"))
invisible(load_foodpricer_obj("EER_LL"))
invisible(load_foodpricer_obj("UL"))
invisible(load_foodpricer_obj("Household"))

message("Config Q2 loaded.")
message("base_dir: ", base_dir)
message("q2_out_dir: ", q2_out_dir)
