########################################################
## Summary table: Per capita household income by city and year
## Replaces ridgeline plot
## Statistics (separate columns): Mean, SD, Median, P25, P75
## Weighted by fex_c (GEIH expansion factor)
## Nominal COP per capita per month
########################################################

library(tidyverse)
library(lubridate)
library(janitor)
library(Hmisc)
library(writexl)

##----------------------------------------------------------
## Directories
##----------------------------------------------------------

base_dir <- "C:\\Users\\Portatil\\Desktop\\Least-cost-diets-and-affordability\\Proyecto Interno\\"
setwd(base_dir)

out_dir    <- file.path(base_dir, "food-security-paper", "output")
income_dir <- file.path(out_dir,  "income_col")
tab_dir    <- file.path(out_dir,  "tables")

dir.create(tab_dir, recursive = TRUE, showWarnings = FALSE)

##----------------------------------------------------------
## 1. Load income data
##----------------------------------------------------------

income_data <- read.csv(file.path(income_dir, "deciles_food_income.csv")) %>%
  select(year, mes, dominio, id_hogar, nug,
         income, per_capita_income, fex_c) %>%
  distinct() %>%
  mutate(
    ciudad = case_when(
      dominio == "MEDELLIN" ~ "Medellín",
      dominio == "BOGOTA"   ~ "Bogotá",
      dominio == "CALI"     ~ "Cali",
      TRUE                  ~ dominio
    )
  )

##----------------------------------------------------------
## 2. Compute weighted statistics by city × year
##    Weighted by fex_c (GEIH expansion factor)
##    Unit: nominal COP per capita per month
##----------------------------------------------------------

income_summary <- income_data %>%
  group_by(ciudad, year) %>%
  dplyr::summarize(
    Mean   = round(wtd.mean(per_capita_income, fex_c, na.rm = TRUE), 0),
    SD     = round(sqrt(wtd.var(per_capita_income, fex_c, na.rm = TRUE)), 0),
    Median = round(wtd.quantile(per_capita_income, fex_c,
                                probs = 0.50, na.rm = TRUE), 0),
    P10    = round(wtd.quantile(per_capita_income, fex_c,
                                probs = 0.1, na.rm = TRUE), 0),
    P90    = round(wtd.quantile(per_capita_income, fex_c,
                                probs = 0.9, na.rm = TRUE), 0),
    N      = n(),
    .groups = "drop"
  ) %>%
  arrange(ciudad, year)



##----------------------------------------------------------
## 3. Long format — one row per city × year
##    Columns: City | Year | Mean | SD | Median | P25 | P75
##    This is the cleanest format for the paper
##----------------------------------------------------------

table_long <- income_summary %>%
  rename(City = ciudad, Year = year)

##----------------------------------------------------------
## 4. Wide format for LaTeX
##    Rows: city × statistic
##    Columns: years 2018–2024
##    Each statistic gets its own row
##----------------------------------------------------------

table_wide <- income_summary %>%
  select(-N) %>%
  pivot_longer(cols      = c(Mean, SD, Median, P10, P90),
               names_to  = "Statistic",
               values_to = "value") %>%
  mutate(
    Statistic = factor(Statistic,
                       levels = c("Mean", "SD", "Median", "P10", "P90"))
  ) %>%
  pivot_wider(names_from  = year,
              values_from = value) %>%
  arrange(ciudad, Statistic) %>%
  rename(City = ciudad)

##----------------------------------------------------------
## 5. Save
##----------------------------------------------------------
library(writexl)
write_xlsx(
  list(
    `Long`             = table_long,
    `Wide (for LaTeX)` = table_wide
  ),
  file.path(tab_dir, "table_income_summary.xlsx")
)

##----------------------------------------------------------
## 6. Print for inspection
##----------------------------------------------------------

cat("\n=== Per capita household income — nominal COP per month ===\n\n")
print(table_long, n = Inf)

message("Table saved to: ", tab_dir)