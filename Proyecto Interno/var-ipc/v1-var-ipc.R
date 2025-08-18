
########################################################
## Prueba: estimación según variación mensual del IPC ##
## Fecha: 22 de junio de 2025                         ##
########################################################

# Cargar librerías
library(lubridate)
library(tidyverse)

# Definir directorio de trabajo
setwd("C:\\Users\\danie\\OneDrive\\Escritorio\\Least-cost-diets-and-affordability\\Proyecto Interno\\")

######################################
###--------------------------------###
### Datos sobre precios minoristas ###
###--------------------------------###
######################################

# Cargar datos de precios minoristas
dane_99_18 = readxl::read_excel("Precios DANE\\OUTPUT_DANE\\precios_unadj_DANE_1999_2018.xlsx")

# Filtrar para las tres ciudades principales
ciudad.input = c("CALI")
dane_99_18 = dane_99_18 %>% filter(nombre_ciudad %in% ciudad.input)

# Recodificar fecha
dane_99_18$fecha = as.Date(paste(dane_99_18$ano, 
                                 dane_99_18$mes_num, 
                                 "01", sep = "-"))

# Crear la variable "código subclase"
dane_99_18$cod_subclase = paste0("0",substr(dane_99_18$codigo_articulo, 1, 6), "0")

# Eliminamos > 2018-3
dane_99_18 = dane_99_18 %>% filter(fecha < "2018-04-01")

# Prueba con una ciudad y una alimento
city_i = levels(as.factor(dane_99_18$nombre_ciudad))
food_j = levels(as.factor(dane_99_18$articulo))

# Base de datos fallidos
list.fail = vector(mode = "list")

# Bucle
for (i in 1:length(city_i)) {
for (j in 1:length(food_j)) {
  
  i = 1
  j = 4
  
print(paste0(city_i[i], " - ", food_j[j]))
  
# Base de datos aux.
df.aux = dane_99_18 %>% filter(nombre_ciudad == city_i[i] &
                                articulo == food_j[j])

if (length(df.aux$precio_500g) == 0) {
  df.aux2 = dane_99_18 %>% filter(articulo == food_j[j])
  list.fail[[length(list.fail) + 1]] <- data.frame(
    ciudad = city_i[i],
    cod_articulo = unique(df.aux2$codigo_articulo),
    cod_subclase = unique(df.aux2$cod_subclase),
    articulo = food_j[j]
  )
} else {
  
  # Recuperar código en tablas correlativas
  # Tabla correlativa
  correlativa = readxl::read_excel("var-ipc\\correlativa_ipc.xlsx")
  # Llenar por celdas combinadas
  correlativa <- correlativa %>%
    fill(subclase, ipc, .direction = "down") %>%
    mutate(cod_subclase = paste0("0",
                                 gasto_basico, "00"))
  
  # Recuperar código ipc
  df.aux = df.aux %>% left_join(correlativa[c("cod_subclase",
                                                      "subclase")],
                                        by = "cod_subclase")
  
  # Código de subclase (formato)
  df.aux$subclase = paste0(df.aux$subclase,"00")
  
  # Filtrar fechas_75
  train.df = df.aux %>% dplyr::filter(fecha <= "2015-01-01")
  test.df = df.aux %>% filter(fecha >= "2015-01-01")
  

#######################################
###---------------------------------###
### Variación del ÍPC: datos > 2015 ###
###---------------------------------###
#######################################

# Cargar variación del IPC
var_ipc = readxl::read_excel("var-ipc\\IPC.xls") %>% janitor::clean_names()

# Recodificar ciudades
var_ipc$ciudad[var_ipc$ciudad == "CARTAGENA DE INDIAS"] = "CARTAGENA"
var_ipc$ciudad[var_ipc$ciudad == "BOGOTÁ, D.C."] = "BOGOTÁ D.C."

# Crear variable codigo_subclase
var_ipc$cod_subclase = paste0(substr(var_ipc$subclase, 1, 8))

# Filtrar para la ciudad de interés
var_ipc = var_ipc %>% filter(ciudad == city_i[i] & 
                               as.numeric(cod_subclase) == as.numeric(unique(df.aux$subclase))) 


if (length(var_ipc$numero_indice) == 0) {
  list.fail[[length(list.fail) + 1]] <- data.frame(
    ciudad = city_i[i],
    cod_articulo = unique(df.aux$codigo_articulo),
    cod_subclase = unique(df.aux$cod_subclase),
    articulo = unique(df.aux$articulo)
  )
} else {

# Formato long
library(tidyverse)
meses_esp <- c("Ene", "Feb", "Mar", "Abr", "May", "Jun", 
               "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")
df_ipc <- var_ipc %>%
  mutate(
    mes_num = match(mes, meses_esp),
    ipc = numero_indice,
    ano = as.numeric(ano)
  ) %>%
  select(ano, mes_num, ipc) %>%
  arrange(ano, mes_num)

# Merge
ipc_df = test.df %>% full_join(df_ipc, by = c("ano", "mes_num")) %>%
  select(fecha, ano, mes_num, ciudad, nombre_ciudad, 
         cod_subclase,
         codigo_articulo, articulo, precio_500g,ipc)%>%
  arrange(fecha) %>%
  mutate(precio_hat = NA)

# Llenar precios hacia adelante usando la variación del IPC
ipc_df$precio_hat[ipc_df$fecha == "2015-01-01" &
                    !is.na(ipc_df$fecha)] = ipc_df$precio_500g[ipc_df$fecha == "2015-01-01" &
                                                                 !is.na(ipc_df$fecha)] 
for (k in 2:nrow(ipc_df)) {
  if (is.na(ipc_df$precio_hat[k])) {
    ipc_df$precio_hat[k] = ipc_df$precio_hat[k - 1] * (ipc_df$ipc[k] / ipc_df$ipc[k - 1])
  }
}

# Unir con la base de datos
test.df2 = test.df %>% left_join(ipc_df[c("fecha", "ipc",
                                          "precio_hat")],
                                 by = "fecha") %>%
  select(fecha, nombre_ciudad, articulo, precio_500g,
         precio_hat)

# Unir con la base de datos de entrenamiento
test.df2 = bind_rows(train.df, test.df2)

# Validar la estimación:
plot_aux = test.df2 %>%
  ggplot(aes(x = fecha)) +
  geom_line(aes(y = precio_500g, color = "Precio real"), size = 1) +
  geom_line(aes(y = precio_hat, color = "Precio estimado"), linetype = "dashed", size = 1) +
  labs(
    title = "Comparación entre precio real y estimado",
    subtitle = paste0(unique(test.df2$nombre_ciudad),
                      " - ",
                      unique(test.df2$articulo)),
    x = "Fecha",
    y = "Precio (500g)",
    color = " "
  ) +
  scale_color_manual(values = c("Precio real" = "black",
                                "Precio estimado" = "red")) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold")
  )

plot_aux

ggsave(plot = plot_aux, 
       filename = paste0("var-ipc/output/",
                         city_i[i],"_",food_j[j],".png"),
       dpi = 300, height = 10, width = 13)
}

}

}
}
