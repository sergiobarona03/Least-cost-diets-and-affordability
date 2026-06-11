########################################################
## 00_config.R — Central configuration
## Source this file at the top of every script:
##   source(file.path(dirname(rstudioapi::getSourceEditorContext()$path),
##                    "00_config.R"))
## or hardcode the path if running non-interactively.
########################################################

# -----------------------------------------------------------------------
# 1. Base directory (project root — edit only here)
# -----------------------------------------------------------------------
BASE_DIRS <- c(
  "C:\\Users\\sergio.barona\\Desktop\\Least-cost-diets-and-affordability\\Proyecto Interno",
  "C:/Users/Portatil/Desktop/Least-cost-diets-and-affordability/Proyecto Interno",
  "C:/Users/danie/OneDrive/Escritorio/Least-cost-diets-and-affordability/Proyecto Interno"
)
BASE_DIR <- BASE_DIRS[dir.exists(BASE_DIRS)][1]
if (is.na(BASE_DIR)) stop("No base directory found. Edit BASE_DIRS in 00_config.R")

# -----------------------------------------------------------------------
# 2. Raw input paths (read-only — original data)
# -----------------------------------------------------------------------
IN_PRICES   <- file.path(BASE_DIR,
                         "Precios DANE/OUTPUT_DANE/precios_unadj_DANE_1999_2018.xlsx")
IN_IPC      <- file.path(BASE_DIR, "var-ipc/IPC.xls")
IN_IPC2     <- file.path(BASE_DIR, "var-ipc/IPC_2.xls")
IN_IPC3     <- file.path(BASE_DIR, "var-ipc/IPC_3.xls")
IN_IPC4     <- file.path(BASE_DIR, "var-ipc/IPC_4.xls")
IN_CORR1    <- file.path(BASE_DIR, "var-ipc/correlativa_ipc.xlsx")
IN_CORR2    <- file.path(BASE_DIR, "var-ipc/correlativa_ipc_articulos.xlsx")

IN_PANEL_FULL <- file.path(BASE_DIR,
                           "review-food-security-paper/output/tcac_food_table/panel_city_month_food_1999_2025.rds")

IN_EER      <- file.path(BASE_DIR,
                         "review-food-security-paper/input/eer/220326_agg_eer.xlsx")

IN_AUX_DIR  <- file.path(BASE_DIR,
                         "review-food-security-paper/02_models/aux-functions")


# -----------------------------------------------------------------------
# 3. Review paper root
# -----------------------------------------------------------------------
REVIEW_DIR  <- file.path(BASE_DIR, "review-food-security-paper")

# -----------------------------------------------------------------------
# 4. Output subdirectories (all inside REVIEW_DIR)
# -----------------------------------------------------------------------

# 00_pipeline — price forecasting
PIPE_DIR    <- file.path(REVIEW_DIR, "00_pipeline")
CACHE_DIR   <- file.path(PIPE_DIR,   "cache")
FORECAST_DIR<- file.path(PIPE_DIR,   "price_forecasting")
ROBUST_DIR  <- file.path(PIPE_DIR,   "robust")
VAL_PLOT_DIR<- file.path(ROBUST_DIR, "validation_plots")

# 01_data_preparation
PREP_DIR    <- file.path(REVIEW_DIR, "01_data_preparation")

# 02_models
MODELS_DIR  <- file.path(REVIEW_DIR, "02_models")
COCA_DIR    <- file.path(MODELS_DIR, "coca")
CONA_DIR    <- file.path(MODELS_DIR, "cona")
CORD_DIR    <- file.path(MODELS_DIR, "cord")
CCONA_DIR   <- file.path(MODELS_DIR, "ccona")
HCOST_DIR   <- file.path(MODELS_DIR, "hcost")

# 03_income
INCOME_DIR  <- file.path(REVIEW_DIR, "03_income")
AFFORD_DIR  <- file.path(REVIEW_DIR, "03_income", "affordability")

# Auxiliary functions (moved to review folder)
IN_AFFORD_AUX <- file.path(REVIEW_DIR, "03_income/aux-functions", "Afford_Expansion.R")

# GEIH poverty microdata (moved to review folder)
GEIH_PERSONAS_DIR <- file.path(REVIEW_DIR, "Pobreza", "personas")
GEIH_HOGARES_DIR  <- file.path(REVIEW_DIR, "Pobreza", "hogares")

# GEIH years to process
GEIH_YEARS <- 2019:2024

# 04_validation
VALID_DIR   <- file.path(REVIEW_DIR, "04_validation")

# 05_manuscript
MANU_DIR    <- file.path(REVIEW_DIR, "05_manuscript")

# 06_figures / 07_tables
FIG_DIR     <- file.path(REVIEW_DIR, "06_figures")
TAB_DIR     <- file.path(REVIEW_DIR, "07_tables")

# -----------------------------------------------------------------------
# 5. Create all output directories
# -----------------------------------------------------------------------
dirs_to_create <- c(
  CACHE_DIR, FORECAST_DIR, ROBUST_DIR, VAL_PLOT_DIR,
  PREP_DIR,
  COCA_DIR, CONA_DIR, CORD_DIR, CCONA_DIR, HCOST_DIR,
  INCOME_DIR, AFFORD_DIR,
  VALID_DIR,
  file.path(MANU_DIR, "sections"),
  file.path(MANU_DIR, "appendix"),
  file.path(FIG_DIR,  "final"),
  file.path(FIG_DIR,  "drafts"),
  file.path(TAB_DIR,  "final"),
  file.path(TAB_DIR,  "drafts")
)
invisible(lapply(dirs_to_create, dir.create,
                 recursive = TRUE, showWarnings = FALSE))

# -----------------------------------------------------------------------
# 6. Shared parameters
# -----------------------------------------------------------------------
TRAIN_END    <- as.Date("2015-12-01")
T0           <- as.Date("2016-01-01")   # anchor date
VAL_START    <- as.Date("2016-02-01")   # validation start (IPC ratio ≠ 1)
VAL_END      <- as.Date("2018-03-01")   # validation end
PAPER_START  <- as.Date("2019-01-01")   # paper period start
PAPER_END    <- as.Date("2024-12-01")   # paper period end
EXTRAP_START <- as.Date("2018-04-01")   # extrapolation start

CITIES_USE   <- c("CALI", "BOGOTÁ D.C.", "MEDELLÍN")
MIN_OBS      <- 24L

# Validity bounds (from Script 03 grid search — update after running)
R2_MIN  <- 0.10
LAM_MIN <- 0.20
LAM_MAX <- 4.00

# Colours (shared across all figures)
COL_BOGOTA   <- "#2E5FA3"
COL_CALI     <- "#1A7A4A"
COL_MEDELLIN <- "#C0392B"
COL_OBS      <- "#2C3E50"
COL_BASE     <- "#95A5A6"
COL_S5       <- "#E74C3C"
COL_VLINE    <- "#2E5FA3"

CITY_LABELS <- c(
  "BOGOTÁ D.C." = "Bogotá",
  "CALI"        = "Cali",
  "MEDELLÍN"    = "Medellín",
  "BOGOTA"      = "Bogotá",
  "MEDELLIN"    = "Medellín"
)

CITY_COLORS <- c(
  "Bogotá"   = COL_BOGOTA,
  "Cali"     = COL_CALI,
  "Medellín" = COL_MEDELLIN
)

message("Config loaded. Review dir: ", REVIEW_DIR)