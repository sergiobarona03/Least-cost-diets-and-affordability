
# Contrastar versiones de las estimaciones
v1_cona_cost = readxl::read_excel("estimadores-banrep/CALI/DANE/input/v1/v1_dane_cona_cali.xlsx")
v1_cona_comp = readxl::read_excel("estimadores-banrep/CALI/DANE/input/v1/v1_dane_cona_cali_comp.xlsx")

v2_cona_cost = readxl::read_excel("estimadores-banrep/CALI/DANE/input/v2/v2_dane_cona_cali.xlsx")
v2_cona_comp = readxl::read_excel("estimadores-banrep/CALI/DANE/input/v2/v2_dane_cona_cali_comp.xlsx")

# Contrastar diferencias en el costo
# Agente representativo
i = 5
agents = levels(as.factor(v1_cona_cost$Demo_Group))

v1_cost = v1_cona_cost %>% filter(Sex == 0 &
                                    Demo_Group == agents[i]) %>%
  mutate(version = "version 1")

v2_cost = v2_cona_cost %>% filter(Sex == 0 &
                                    Demo_Group == agents[i])%>%
  mutate(version = "version 2")

plot(v1_cost$cost_day, type = "l")
lines(v2_cost$cost_day, col = "red")

# Validación en la composición

v1_comp = v1_cona_comp %>% filter(Sex == 0 &
                                    Demo_Group == agents[i]) %>%
  mutate(version = "version 1")

v2_comp = v2_cona_comp %>% filter(Sex == 0 &
                                   Demo_Group == agents[i]) %>%
  mutate(version = "version 2")


fechas_vec = v1_comp$fecha
list_foods = vector(mode = "list", 
                    length = length(fechas_vec))

for (k in 1:length(fechas_vec)) {
  v1_aux = v1_comp %>% filter(fecha == fechas_vec[k])
  v2_aux = v2_comp %>% filter(fecha == fechas_vec[k])
  
  if (length(v2_aux$Food) > length(v1_aux$Food)) {
    diff_vec = setdiff(v2_aux$Food,v1_aux$Food)
  } else {
    diff_vec = setdiff(v1_aux$Food,v2_aux$Food)
  }
  
  print(diff_vec)
  
  
  list_foods[[k]] = data.frame(metric = "cona",
                               sex = unique(v1_aux$Sex),
                               agent = unique(v1_aux$Demo_Group),
                               fecha = fechas_vec[i],
                               foods = diff_vec
                               )
  
}

final_list_foods = do.call(rbind,list_foods)
writexl::write_xlsx(final_list_foods,
                    "estimadores-banrep/CALI/131125_validar_foods.xlsx")


