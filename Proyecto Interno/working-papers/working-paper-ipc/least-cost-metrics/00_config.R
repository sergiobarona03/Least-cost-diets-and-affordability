########################################################
## 00_config.R
## Global config + helpers + requirement objects
########################################################

suppressPackageStartupMessages({
  library(tidyverse)
  library(readxl)
  library(janitor)
  library(lubridate)
  library(writexl)
  library(FoodpriceR)
})

# -----------------------------
# Paths (EDIT ONLY THIS BLOCK)
# -----------------------------
base_dir <- "C:\\Users\\danie\\OneDrive\\Escritorio\\Least-cost-diets-and-affordability\\Proyecto Interno\\"

# Extended prices (from IPC script)
in_prices_ext <- file.path(
  base_dir,
  "working-papers/working-paper-ipc/output/forecasting_fullsample/prices_extended_city_article_month.rds"
)

# Original retail file (for mapping articulo -> codigo_articulo if needed)
in_retail_99_18 <- file.path(
  base_dir,
  "Precios DANE/OUTPUT_DANE/precios_unadj_DANE_1999_2018.xlsx"
)

# Nutrient mapping file (your TCAC join file)
in_tcac_map <- file.path(
  base_dir,
  "composicion-nut/Copia_DANE_4_DIC_2025act.xlsx"
)

# Outputs requested
out_dir <- file.path(base_dir, "working-papers/working-paper-ipc/output/least_cost_metrics")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

tmp_dir <- file.path(out_dir, "tmp")
dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)

plot_dir <- file.path(out_dir, "plots")
dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)

# Cities
cities_use <- c("CALI", "BOGOTÁ D.C.", "MEDELLÍN")

# Price scenario(s)
escenarios <- c("precio_100g")  # keep extensible

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

mode1 <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA)
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Load FoodpriceR requirement objects if not already in global env
load_foodpricer_obj <- function(obj_name) {
  if (exists(obj_name, envir = .GlobalEnv)) return(invisible(TRUE))
  ok <- tryCatch({
    data(list = obj_name, package = "FoodpriceR", envir = .GlobalEnv)
    TRUE
  }, error = function(e) FALSE)
  if (!ok) warning("Could not load FoodpriceR object: ", obj_name)
  invisible(ok)
}

# These are commonly needed in your scripts
invisible(load_foodpricer_obj("EER"))
invisible(load_foodpricer_obj("EER_LL"))
invisible(load_foodpricer_obj("UL"))

message("Config loaded.")
message("out_dir: ", out_dir)
