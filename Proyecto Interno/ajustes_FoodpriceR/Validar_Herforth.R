
##########################################
##########################################
## Prueba: robustez de la medida CoRD   ##
##########################################
##########################################

library(FoodpriceR)

# DataCol
data.cali = FoodpriceR::DataCol(Month = 9, Year = 2022, City = "Cali",
                                Percentile = 0.25) %>% 
  filter(!Food %in% c("Sal yodada", "Queso campesino", "Mayonesa doy pack"))

# Ingresos 2022-09
income.cali = FoodpriceR::IncomeCol(Month = 9,
                                    Year = 2022, City = "Cali")

##---------------------------------##
## 1. Según Yoshioka et al. (2025) ##
##---------------------------------##


# Estimación CoRD
cord_yoshioka = FoodpriceR::CoRD(data = data.cali, serv = FoodpriceR::serv2, 
                                 diverse = FoodpriceR::diverse)$cost

cord_yoshioka = merge(Household, cord_yoshioka)
cord_yoshioka$total_household = sum(cord_yoshioka$cost_day)
cord_yoshioka$per_capita = cord_yoshioka$total_household/3
cord_yoshioka$per_capita_year = cord_yoshioka$per_capita*365
cord_yoshioka$per_capita_month = cord_yoshioka$per_capita*30

# HCost
hcost_yoshioka = list(Model_CoRD = data.frame(cord_yoshioka))
  
afford_yoshioka = FoodpriceR::Afford(Hexpense = income.cali, 
                                     Model_CoRD = cord_yoshioka)

##----------------------------------##
## 2. Según Dizon & Herforth (2022) ##
##----------------------------------##

# Estimación CoRD
cord_herforth = CoRD_Herforth(data = data.cali, serv = FoodpriceR::serv2, 
                                 diverse = FoodpriceR::diverse)$cost

cord_herforth = merge(Household, cord_herforth)
cord_herforth$total_household = sum(cord_herforth$cost_day)
cord_herforth$per_capita = cord_herforth$total_household/3
cord_herforth$per_capita_year = cord_herforth$per_capita*365
cord_herforth$per_capita_month = cord_herforth$per_capita*30

# HCost
hcost_herforth = list(Model_CoRD = data.frame(cord_herforth))

afford_herforth = FoodpriceR::Afford(Hexpense = income.cali, 
                                     Model_CoRD = cord_herforth)
