########################################################
## Household diet cost comparison
## CoCA, CoNA and CoRD — representative household
## Full panel: all cities × dates
########################################################

library(tidyverse)
library(readxl)
library(lubridate)

##----------------------------------------------------------
## Directories
##----------------------------------------------------------

base_dir <- "C:\\Users\\Portatil\\Desktop\\Least-cost-diets-and-affordability\\Proyecto Interno"

out_coca  <- file.path(base_dir, "food-security-paper", "output", "coca")
out_cona  <- file.path(base_dir, "food-security-paper", "output", "cona")
out_cord  <- file.path(base_dir, "food-security-paper", "output", "cord")
out_hcost <- file.path(base_dir, "food-security-paper", "output", "hcost")

##----------------------------------------------------------
## 1. Load model results
##----------------------------------------------------------

# CoCA
df.coca <- read_excel(file.path(out_coca, "230326_coca_results.xlsx")) %>%
  mutate(
    fecha  = as.Date(fecha),
    year   = year(fecha),
    member = case_when(
      Sex == 0 & Demo_Group == "[31,51)"  ~ "Adult male",
      Sex == 1 & Demo_Group == "[31,51)"  ~ "Adult female",
      Sex == 1 & Demo_Group == "[10, 14)" ~ "Female child"
    )
  )

# CoNA
df.cona <- read_excel(file.path(out_cona, "230326_cona_full.xlsx"),
                      sheet = "cost") %>%
  mutate(fecha = as.Date(fecha), year = year(fecha))

# CoRD
df.cord <- readRDS(file.path(out_cord, "230326_cord_full.rds"))$cost %>%
  mutate(fecha = as.Date(fecha), year = year(fecha))

##----------------------------------------------------------
## 2. Helper: compute household cost metrics for one city × date
##----------------------------------------------------------

household_metrics <- function(df, dominio, fecha_sel) {
  df %>%
    filter(ciudad == dominio, fecha == fecha_sel) %>%
    mutate(
      Person           = row_number(),
      n                = n(),
      total_household  = sum(as.numeric(cost_day)),
      per_capita       = total_household / n,
      per_capita_month = per_capita * 30,
      per_capita_year  = per_capita * 365
    ) %>%
    select(Demo_Group, Sex, Person, cost_day,
           total_household, per_capita, per_capita_month, per_capita_year)
}

##----------------------------------------------------------
## 3. Full panel: all cities × dates
##----------------------------------------------------------

dominios <- levels(as.factor(df.coca$ciudad))
fechas   <- levels(as.factor(df.coca$fecha))

hcost_full <- map_dfr(dominios, function(i) {
  map_dfr(fechas, function(t) {
    
    coca_i <- tryCatch(
      household_metrics(df.coca, i, t) %>% mutate(model = "CoCA"),
      error = function(e) NULL
    )
    cona_i <- tryCatch(
      household_metrics(df.cona, i, t) %>% mutate(model = "CoNA"),
      error = function(e) NULL
    )
    cord_i <- tryCatch(
      household_metrics(df.cord, i, t) %>% mutate(model = "CoRD"),
      error = function(e) NULL
    )
    
    bind_rows(coca_i, cona_i, cord_i) %>%
      mutate(ciudad = i, fecha = as.Date(t), year = year(as.Date(t)))
  })
})

# Reorder columns
hcost_full <- hcost_full %>%
  select(model, ciudad, fecha, year,
         Demo_Group, Sex, Person,
         cost_day, total_household,
         per_capita, per_capita_month, per_capita_year)

##----------------------------------------------------------
## 4. Save
##----------------------------------------------------------

saveRDS(hcost_full, file.path(out_hcost, "230326_hcost_full.rds"))

writexl::write_xlsx(
  list(
    full       = hcost_full,
    CoCA       = hcost_full %>% filter(model == "CoCA"),
    CoNA       = hcost_full %>% filter(model == "CoNA"),
    CoRD       = hcost_full %>% filter(model == "CoRD")
  ),
  file.path(out_hcost, "230326_hcost_full.xlsx")
)

  message("Done. Rows: ", nrow(hcost_full))
message("Saved to: ", out_hcost)