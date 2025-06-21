
cali_data = overall_tcac_sipsa %>% filter(municipio == "Cali")


length(levels(as.factor(cali_data$alimento)))

# Cambiar grupos:
cali_data$grupo = toupper(cali_data$grupo)
cali_data$grupo[cali_data$grupo == "TUBERCULOS, RAICES Y PLATANOS"] = "TUBÉRCULOS, RAÍCES Y PLÁTANOS"
cali_data$grupo[cali_data$grupo == "LACTEOS Y HUEVOS"] = "LÁCTEOS Y HUEVOS"
grupos = levels(as.factor(cali_data$grupo))
grupos

for (k in grupos) {
print(k)
# Dataframe alimentos:
df.aux = cali_data %>% filter(grupo == k)
alimentos = data.frame(alimento = levels(as.factor(df.aux$alimento)))

library(dplyr)

# Suponiendo que tu data frame original se llama `alimentos`
# y tiene una sola columna: alimento
tiendas <- c("Éxito Valle del Lili", "Éxito Calipso", "Éxito Unicentro")

# Columnas a crear: una para unidad y otra para precio por tienda
cols <- unlist(lapply(tiendas, function(t) c(paste0(t, "_unidad"), paste0(t, "_precio"))))

# Crear una fila vacía con las mismas columnas (NA en todo excepto alimento)
fila_vacia <- as.data.frame(matrix(NA, nrow = 1, ncol = length(cols) + 1))
colnames(fila_vacia) <- c("alimento", cols)

# Inicializar lista para almacenar los bloques
lista_filas <- list()


# Sacar un excel por grupo


for (i in seq_len(nrow(alimentos))) {
  fila_alimento <- fila_vacia
  fila_alimento$alimento <- alimentos$alimento[i]
  
  # Crear bloque: 1 fila con nombre del alimento + 3 vacías
  bloque <- bind_rows(fila_alimento, fila_vacia, fila_vacia, fila_vacia)
  
  lista_filas[[i]] <- bloque
}

# Unir todo
df_resultado <- bind_rows(lista_filas)

# Resultado
print(df_resultado)

writexl::write_xlsx(df_resultado, paste0("Mapeo SIPSA-TCAC\\trabajo_campo\\", 
                                         k,"_1324_campo_planilla.xlsx"))


}

