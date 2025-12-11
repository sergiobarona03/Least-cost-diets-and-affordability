##################################################################
## Prueba: análisis sobre margen de comercialización (subclase) ##
## Fecha: 04 de diciembre de 2025                               ##
##################################################################

# Cargar librerías
library(lubridate)
library(tidyverse)

# Definir directorio de trabajo
setwd("C:\\Users\\sergio.barona\\Desktop\\Least-cost-diets-and-affordability\\Proyecto Interno\\")

##-------------------------------------------##
## Cargar datos de precios minoristas (2018) ##
##-------------------------------------------##

# Cargar datos (13 ciudades principales)
retail_99_18 = readxl::read_excel("Precios DANE\\OUTPUT_DANE\\precios_IPC_1999_2018.xlsx")

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
  dplyr::mutate(
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

readr::write_csv(retail_99_18,
                 "estimadores-banrep/CALI/DANE/input/v7/v7_retail_price_data.csv")


# Cargar datos de precios estimados
input_cali_hat = retail_99_18

##-------------------------------------------------##
## Cargar base de datos de composición nutricional ##
##-------------------------------------------------##

# Se garantiza el mapeo 1:1
# cuartiles_margen = read.csv("margen-dist/output-ciudades/CALI/111225_margen_mediana_con_IC.csv")

# Recuperar el mapeo
# Cargar datos de composición nutricional
# sipsa_tcac <- readxl::read_excel("composicion-nut/1823_mapeo_sipsa_tcac v1.0_2025.xlsx") %>%
 # janitor::clean_names() %>%
  # rename(food_sipsa = alimento_nombre_sipsa)

# Cargar mapeo IPC - SIPSA
# ipc_sipsa = readxl::read_excel("composicion-nut\\Copia_DANE_4_DIC_2025act.xlsx") %>%
 # janitor::clean_names() %>% mutate(retail = articulo_dane) %>%
 #  select(retail, mapeo_sipsa, codigo_tcac)

# Recuperar el nombre de sipsa
# ipc_sipsa = merge(ipc_sipsa, sipsa_tcac[c("codigo_tcac", "food_sipsa")], by = "codigo_tcac") %>%
  # rename(sipsa = food_sipsa) %>% select(c("retail", "sipsa"))

# cuartiles_ipc_sipsa = merge(cuartiles_margen, ipc_sipsa,
                            # by.x = "alimento_sipsa", by.y = "sipsa") 
# filter_foods = levels(as.factor(cuartiles_ipc_sipsa$retail))

# Filtrar input
# input_cali_hat = input_cali_hat %>% filter(articulo %in% filter_foods)

# Cargar datos de composición nutricional
dane_tcac = readxl::read_excel("composicion-nut\\Copia_DANE_4_DIC_2025act.xlsx") %>%
  janitor::clean_names() %>% rename(articulo = articulo_dane)


# Unir ambas bases de datos
input_cali_hat$codigo_articulo = as.numeric(input_cali_hat$codigo_articulo)
dane_tcac$codigo_articulo = as.numeric(dane_tcac$codigo_articulo)
input_cali_hat = input_cali_hat %>% left_join(dane_tcac %>% dplyr::select(-any_of("nombre_ciudad")),
                                              by = c("codigo_articulo", "articulo"))

# Conversión a 100 gramos en parte comestible
input_cali_hat2 = input_cali_hat %>%
  mutate(
    pc = parte_comestible_percent,
    precio_100g = precio_500g*(100/(5*pc))) %>%
  distinct()

readr::write_csv(input_cali_hat2,
                 "estimadores-banrep/CALI/DANE/input/v7/v7_comp_price_data_cali.csv")
