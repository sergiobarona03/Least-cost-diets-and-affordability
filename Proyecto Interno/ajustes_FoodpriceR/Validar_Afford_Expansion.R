

#------------------------------------------------------#
#           Validar: función Afford y Afford_Expansion #
#------------------------------------------------------#

setwd("C:\\Users\\Portatil\\Desktop\\Least-cost-diets-and-affordability\\Proyecto Interno\\ajustes_FoodpriceR\\input")

# Datos de ingresos
library(dplyr)
library(reshape2)
library(FoodpriceR)

# writexl::write_xlsx(data_enfoque1, "data_enfoque1.xlsx")
# writexl::write_xlsx(Data_income_household, "data_enfoque2.xlsx")

data_enfoque1 = readxl::read_excel("data_enfoque1.xlsx")
data_enfoque2 = readxl::read_excel("data_enfoque2.xlsx")
  

# Datos de costos (no cambian)
models = FoodpriceR::HCost(Month = 9, Year = 2022, City = "Cali",
                           Household = Household, EER_LL = EER_LL,
                           UL = UL, Serv = serv2, Diverse = diverse,
                           ERR = EER)

Model_CoCA = models$Model_CoCA
Model_CoNA = models$Model_CoNA
Model_CoRD = models$Model_CoRD

##--------------------------##
## Afford: versión 1        ##
##--------------------------##

output_v1 = Afford(Hexpense = data_enfoque1, 
                   Model_CoCA = Model_CoCA, 
                   Model_CoNA = Model_CoNA,
                   Model_CoRD = Model_CoRD)
output_v1$Poverty_outcome

##--------------------------##
## Afford: versión 2        ##
##--------------------------##

output_v2 = Afford(Hexpense = data_enfoque2, 
                   Model_CoCA = Model_CoCA, 
                   Model_CoNA = Model_CoNA,
                   Model_CoRD = Model_CoRD)
output_v2$Poverty_outcome
