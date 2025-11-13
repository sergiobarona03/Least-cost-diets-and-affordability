## =====================================================================
## Comparar versión 1 vs versión 2: costos y composición de la dieta
## =====================================================================

# Paquetes
library(readxl)
library(dplyr)
library(writexl)

## ---------------------------------------------------------------------
## 1. Cargar datos
## ---------------------------------------------------------------------

# Costos
v1_cona_cost <- read_excel("estimadores-banrep/CALI/DANE/input/v1/v1_dane_cona_cali.xlsx")
v2_cona_cost <- read_excel("estimadores-banrep/CALI/DANE/input/v2/v2_dane_cona_cali.xlsx")

# Composición
v1_cona_comp <- read_excel("estimadores-banrep/CALI/DANE/input/v1/v1_dane_cona_cali_comp.xlsx")
v2_cona_comp <- read_excel("estimadores-banrep/CALI/DANE/input/v2/v2_dane_cona_cali_comp.xlsx")

## ---------------------------------------------------------------------
## 2. Elegir agente representativo (misma lógica que tu código)
## ---------------------------------------------------------------------

i <- 5                          # índice del agente
agents <- levels(as.factor(v1_cona_cost$Demo_Group))

## ---------------------------------------------------------------------
## 3. Comparar costos diarios v1 vs v2
## ---------------------------------------------------------------------

v1_cost <- v1_cona_cost %>% 
  filter(Sex == 0, Demo_Group == agents[i]) %>% 
  select(fecha, Sex, Demo_Group, cost_day) %>% 
  rename(cost_v1 = cost_day)

v2_cost <- v2_cona_cost %>% 
  filter(Sex == 0, Demo_Group == agents[i]) %>% 
  select(fecha, Sex, Demo_Group, cost_day) %>% 
  rename(cost_v2 = cost_day)

# Unir por fecha, sexo y grupo demográfico, y calcular diferencia
cost_compare <- v1_cost %>% 
  inner_join(v2_cost, by = c("fecha", "Sex", "Demo_Group")) %>% 
  mutate(diff = cost_v2 - cost_v1) %>% 
  arrange(fecha)

# Ver tabla de comparación de costos
print(cost_compare)

# Resumen rápido de las diferencias (v2 - v1)
print(summary(cost_compare$diff))

# Gráfico simple (línea negra v1, roja v2)
plot(cost_compare$fecha, cost_compare$cost_v1, type = "l",
     ylab = "Costo diario", xlab = "Fecha")
lines(cost_compare$fecha, cost_compare$cost_v2, col = "red")
legend("topleft", legend = c("v1", "v2"), lty = 1, col = c("black", "red"))

## ---------------------------------------------------------------------
## 4. Comparar composición de la dieta óptima (alimentos v1 vs v2)
## ---------------------------------------------------------------------

# NOTA: si tienes una variable de cantidad óptima (por ejemplo q_opt),
# puedes filtrar antes de select():
#   ... %>% filter(q_opt > 0) %>% select(...)

v1_comp <- v1_cona_comp %>% 
  filter(Sex == 0, Demo_Group == agents[i]) %>% 
  select(fecha, Sex, Demo_Group, Food) %>% 
  mutate(in_v1 = 1)

v2_comp <- v2_cona_comp %>% 
  filter(Sex == 0, Demo_Group == agents[i]) %>% 
  select(fecha, Sex, Demo_Group, Food) %>% 
  mutate(in_v2 = 1)

# Unir ambas versiones alimento a alimento por fecha
foods_compare <- v1_comp %>% 
  full_join(v2_comp, by = c("fecha", "Sex", "Demo_Group", "Food")) %>% 
  mutate(
    in_v1 = ifelse(is.na(in_v1), 0, in_v1),
    in_v2 = ifelse(is.na(in_v2), 0, in_v2)
  )

# Alimentos que están en v1 pero no en v2
foods_only_v1 <- foods_compare %>% 
  filter(in_v1 == 1, in_v2 == 0) %>% 
  mutate(diff_type = "v1_not_in_v2")

# Alimentos que están en v2 pero no en v1
foods_only_v2 <- foods_compare %>% 
  filter(in_v1 == 0, in_v2 == 1) %>% 
  mutate(diff_type = "v2_not_in_v1")

# Juntar diferencias y ordenar
final_list_foods <- bind_rows(foods_only_v1, foods_only_v2) %>% 
  arrange(fecha, diff_type, Food) %>% 
  mutate(metric = "cona") %>% 
  select(metric, Sex, agent = Demo_Group, fecha, Food, diff_type)

# Ver una muestra en consola
print(head(final_list_foods, 20))

# Exportar a Excel
write_xlsx(final_list_foods,
           "estimadores-banrep/CALI/131125_clean_validar_foods.xlsx")

## ---------------------------------------------------------------------
## Fin del script
## ---------------------------------------------------------------------
