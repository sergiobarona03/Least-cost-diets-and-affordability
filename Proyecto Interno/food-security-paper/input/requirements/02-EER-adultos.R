
########################################################
## Procesamiento de los requerimientos energéticos
## en adultos
## Nota: el procedimiento utiliza ENSIN y SABE
########################################################

# Librerías
library(tidyverse)
library(readxl)

# Directorio base
base_dir = "C:\\Users\\Portatil\\Desktop\\Least-cost-diets-and-affordability\\Proyecto Interno"

# ------------------------------------------------------------
# 1. Cargar RIEN para niños y niñas
# ------------------------------------------------------------
eer_div1 = read_csv(file.path(base_dir,
                               "food-security-paper\\input\\requirements\\tabla_final_eer_div1.csv"))

eer_div2 = read_csv(file.path(base_dir,
                              "food-security-paper\\input\\requirements\\tabla_final_eer_div2.csv"))

out_eer = file.path(base_dir,
                    "food-security-paper\\output\\eer\\")
# Guardar la base de datos
writexl::write_xlsx(eer_div2, 
                    paste0(out_eer, "\\220326_adult_eer.xlsx"))

