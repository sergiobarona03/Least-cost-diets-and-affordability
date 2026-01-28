
# Cálculo del ingreso para cada ciudad

#----------------------------------------------------------------------
# Paquetes
#----------------------------------------------------------------------
library(devtools)
library(tidyverse)
library(FoodpriceR)

#----------------------------------------------------------------------
# Directorios
#----------------------------------------------------------------------
base_dir <- "C:/Users/Portatil/Desktop/Least-cost-diets-and-affordability/Proyecto Interno"

out_dir <- file.path(base_dir, "working-papers/working-paper-ipc/output/least_cost_metrics")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

tmp_dir <- file.path(out_dir, "tmp")
dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)

# panel en tmp_dir (ajusta a rds o csv según lo que uses)
# panel <- readRDS(file.path(tmp_dir, "panel_city_month_food_1999_2025.rds"))
panel <- read.csv(file.path(tmp_dir, "panel_city_month_food_1999_2025.csv"))

city.x = levels(as.factor(panel$ciudad))[1]

if (city.x == "BOGOTÁ D.C.") {
  city.x = "Bogotá"
}

fecha.x = levels(as.factor(panel$fecha))[278]

library(reshape2)
income.x = FoodpriceR::IncomeCol(Month = month(fecha.x), Year = year(fecha.x),
                                 City = city.x
                                  )
