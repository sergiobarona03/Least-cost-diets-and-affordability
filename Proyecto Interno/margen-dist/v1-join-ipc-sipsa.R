
##################################################################
## Prueba: análisis sobre margen de comercialización (subclase) ##
## Fecha: 31 de julio de 2025                                   ##
##################################################################

# Cargar librerías
library(lubridate)
library(tidyverse)

# Definir directorio de trabajo
setwd("C:\\Users\\Portatil\\Desktop\\Least-cost-diets-and-affordability\\Proyecto Interno\\")

##-------------------------------------------##
## Cargar datos de precios minoristas (2018) ##
##-------------------------------------------##

# Cargar datos (13 ciudades principales)
retail_99_18 = readxl::read_excel("Precios DANE\\OUTPUT_DANE\\precios_IPC_1999_2018.xlsx")

# Eliminar alimentos que son ultraprocesados o preparaciones
alimentos_excluir <- c(
  # Ultraprocesados
  "AREPAS  PRECOCIDAS", "AREPAS RELLENAS CON ALGO", "BOCADILLOS",
  "CEREAL ALIMENTO PARA BEBÉ", "CEREAL PARA DESAYUNO", "CHOCOLATE INSTANTANEO",
  "CHORIZO", "GALLETAS DE SAL", "GALLETAS DULCES", "GALLETAS INTEGRALES",
  "GASEOSAS", "GELATINA O FLAN", "HARINA PARA TORTAS", "HELADOS DE CREMA",
  "JAMÓN", "JUGOS INSTANTANEOS O EN POLVO", "JUGOS PROCESADOS",
  "MALTAS", "MARGARINA", "MAYONESA", "MERMELADA", "MORTADELA",
  "PIZZA", "SALCHICHAS","SALCHICHÓN", "SALSA DE TOMATE", 
  "SOPAS", "YOGOURT", "CREMA DE LECHE", "PAPAS FRITAS",
  
  # Condimentos y hierbas
  "CILANTRO", "COLOR", "COMINOS", "LAUREL", "MOSTAZA",
  "PIMIENTA", "TOMILLO", "REVUELTO VERDE",
  
  # Preparaciones y alimentos compuestos
  "ALMUERZO CORRIENTE O EJECUTIVO", "ALMUERZO ESPECIAL O A LA CARTA",
  "CHOCOLATE EN PASTA", "CAFÉ INSTANTANEO", "COMBOS", 
  "CREMAS", "ENSALADA  DE FRUTAS", "QUESO CREMA", "TINTO", 
  "HAMBURGUESA", "KUMIS", "JUGOS NATURALES", "SUERO"
)

# Excluir alimentos
retail_99_18 <- retail_99_18 %>% filter(!articulo %in% alimentos_excluir)

# Armonizar los nombres de las ciudades (código DIVIPOLA)
retail_99_18 <- retail_99_18 %>%
  mutate(cod_mun = case_when(
    nombre_ciudad == "BARRANQUILLA"   ~ "08001",
    nombre_ciudad == "BOGOTÁ D.C."    ~ "11001",
    nombre_ciudad == "BUCARAMANGA"    ~ "68001",
    nombre_ciudad == "CALI"           ~ "76001",
    nombre_ciudad == "CARTAGENA"      ~ "13001",
    nombre_ciudad == "CÚCUTA"         ~ "54001",
    nombre_ciudad == "MANIZALES"      ~ "17001",
    nombre_ciudad == "MEDELLÍN"       ~ "05001",
    nombre_ciudad == "MONTERÍA"       ~ "23001",
    nombre_ciudad == "NEIVA"          ~ "41001",
    nombre_ciudad == "PASTO"          ~ "52001",
    nombre_ciudad == "PEREIRA"        ~ "66001",
    nombre_ciudad == "VILLAVICENCIO"  ~ "50001",
    TRUE ~ NA_character_
  ))

# Recodificar año
retail_99_18 <- retail_99_18 %>%
  mutate(ano = as.integer(ano))

# Recodificar mes
retail_99_18 <- retail_99_18 %>%
  mutate(
    mes_num = recode(mes,
                     "enero" = 1,
                     "febrero" = 2,
                     "marzo" = 3,
                     "abril" = 4,
                     "mayo" = 5,
                     "junio" = 6,
                     "julio" = 7,
                     "agosto" = 8,
                     "septiembre" = 9,
                     "octubre" = 10,
                     "noviembre" = 11,
                     "diciembre" = 12
    )
  )

# Conversión de las unidades (precio_500g)
retail_99_18 <- retail_99_18 %>%
  mutate(
    unidad = str_replace_all(tolower(unidad), "\\s", ""),
    precio = as.numeric(precio),
    precio_500g = case_when(
      unidad == "125grs." ~ precio * 4,
      unidad == "250grs." ~ precio * 2,
      unidad %in% c("400grs.", "400grs") ~ precio * (500 / 400),
      unidad %in% c("500grs.", "500grs") ~ precio,
      unidad %in% c("600grs.", "600grs") ~ precio * (500 / 600),
      unidad %in% c("1und.", "1und") & articulo == "HUEVOS" ~ precio * 8.33,
      unidad %in% c("1000c.c.", "1000cc") & str_detect(articulo, regex("aceite", ignore_case = TRUE)) ~ precio * (500 / 920),
      unidad %in% c("1000c.c.", "1000cc") & str_detect(articulo, regex("leche", ignore_case = TRUE)) ~ precio * 0.5,
      unidad %in% c("250c.c.", "250cc") & str_detect(articulo, regex("mantequilla", ignore_case = TRUE)) ~ precio * 2,
      TRUE ~ NA_real_
    )
  )


##------------------------------------##
## Cargar datos de precios mayoristas ##
##------------------------------------##

# Lista output
whole_list = vector(mode = "list", length = length(2013:2018))

# Cargar series de sipsa
for (k in 2013:2018) {
  whole_list[[k]] = readRDS(paste0("Precios al por mayor\\Bases historicas\\", k,".rds"))
}

# whole_18 significa whole hasta 2018
whole_18 <- do.call(rbind, whole_list)

# Identificar los mercados de las principales ciudades
whole_18 <- whole_18 %>%
  mutate(nombre_ciudad = case_when(
    str_detect(Mercado, regex("Barranquilla", ignore_case = TRUE)) ~ "BARRANQUILLA",
    str_detect(Mercado, regex("Bogotá", ignore_case = TRUE)) ~ "BOGOTÁ D.C.",
    str_detect(Mercado, regex("Bucaramanga", ignore_case = TRUE)) ~ "BUCARAMANGA",
    str_detect(Mercado, regex("Cali", ignore_case = TRUE)) ~ "CALI",
    str_detect(Mercado, regex("Cartagena", ignore_case = TRUE)) ~ "CARTAGENA",
    str_detect(Mercado, regex("Cúcuta", ignore_case = TRUE)) ~ "CÚCUTA",
    str_detect(Mercado, regex("Manizales", ignore_case = TRUE)) ~ "MANIZALES",
    str_detect(Mercado, regex("Medellín", ignore_case = TRUE)) ~ "MEDELLÍN",
    str_detect(Mercado, regex("Montería", ignore_case = TRUE)) ~ "MONTERÍA",
    str_detect(Mercado, regex("Neiva", ignore_case = TRUE)) ~ "NEIVA",
    str_detect(Mercado, regex("Pasto", ignore_case = TRUE)) ~ "PASTO",
    str_detect(Mercado, regex("Pereira", ignore_case = TRUE)) ~ "PEREIRA",
    str_detect(Mercado, regex("Villavicencio", ignore_case = TRUE)) ~ "VILLAVICENCIO",
    TRUE ~ NA_character_
  ))

# Filtrar para las 13 ciudades principales
whole_18 <- whole_18 %>% filter(!is.na(nombre_ciudad))

# Armonizar los nombres de las ciudades (código DIVIPOLA)
whole_18 <- whole_18 %>%
  mutate(cod_mun = case_when(
    nombre_ciudad == "BARRANQUILLA"   ~ "08001",
    nombre_ciudad == "BOGOTÁ D.C."    ~ "11001",
    nombre_ciudad == "BUCARAMANGA"    ~ "68001",
    nombre_ciudad == "CALI"           ~ "76001",
    nombre_ciudad == "CARTAGENA"      ~ "13001",
    nombre_ciudad == "CÚCUTA"         ~ "54001",
    nombre_ciudad == "MANIZALES"      ~ "17001",
    nombre_ciudad == "MEDELLÍN"       ~ "05001",
    nombre_ciudad == "MONTERÍA"       ~ "23001",
    nombre_ciudad == "NEIVA"          ~ "41001",
    nombre_ciudad == "PASTO"          ~ "52001",
    nombre_ciudad == "PEREIRA"        ~ "66001",
    nombre_ciudad == "VILLAVICENCIO"  ~ "50001",
    TRUE ~ NA_character_
  ))

# Antes de calcular el precio medio, se armonizan las unidad (P500g)
# El precio 
whole_18 <- whole_18 %>%
  mutate(
    precio_500g = case_when(
      str_detect(Alimento, regex("aceite", ignore_case = TRUE)) ~ Precio_kg * (500 / 920),
      
      Alimento %in% c("Huevo blanco A", "Huevo rojo A") ~ Precio_kg * (500 / 50),
      Alimento %in% c("Huevo blanco AA", "Huevo rojo AA") ~ Precio_kg * (500 / 60),
      Alimento %in% c("Huevo blanco extra", "Huevo rojo extra") ~ Precio_kg * (500 / 67),
      
      TRUE ~ Precio_kg / 2  # Por defecto, mitad del precio por kg
    )
  ) %>%
  filter(!Alimento %in% c("Jugo de frutas", "Bocadillo veleño", "Vinagre",
                          "Huevo blanco B", "Huevo rojo B"))


# Crear el precio promedio para cada alimento según: año, mes, ciudad, alimento
whole_18_mean <- whole_18 %>% group_by(Year, Month, cod_mun, Alimento) %>%
  summarise(precio_medio = mean(precio_500g, na.rm = TRUE))

##------------------------------------##
## Cargar mapeo: DANE (IPC) - SIPSA   ##
##------------------------------------##

# Cargar el mapeo de ambas bases:
ipc_sipsa = readxl::read_excel("Time-series\\mapeo_retail_sipsa.xlsx")

# Añadir al mayorista las denominaciones de sipsa
whole_18_mean = whole_18_mean %>% left_join(ipc_sipsa, by = c("Alimento" = "sipsa"))

# Añadir los precios retail
retail_whole_18 <- whole_18_mean[c("Year", "Month", "cod_mun", "Alimento", "retail", "precio_medio")] %>%
  left_join(
    retail_99_18,
    by = c(
       "Year" = "ano",
      "Month" = "mes_num",
      "cod_mun" = "cod_mun",
       "retail" = "articulo"
    )
  ) %>% filter(Year >= 2013)


