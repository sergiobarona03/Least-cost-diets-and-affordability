##--------------------------------------------------##
## Análisis conjunto preliminar de series de tiempo ##
## Fecha: 15 de junio de 2025                       ##
##--------------------------------------------------##

# Cargar librerías
library(tidyverse)

# Definir directorio de trabajo
setwd("C:\\Users\\danie\\OneDrive\\Escritorio\\Least-cost-diets-and-affordability\\Proyecto Interno\\")

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

# Guardar retail 1999 - 2018
writexl::write_xlsx(retail_99_18, "Precios DANE\\OUTPUT_DANE\\precios_unadj_DANE_1999_2018.xlsx")

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

# Crear price_500g
whole_18 <- whole_18 %>%
  mutate(precio_500g = case_when(
    str_detect(Alimento, regex("aceite", ignore_case = TRUE)) ~ Precio_kg * (500 / 920),
    Alimento %in% c("Huevo blanco A", "Huevo rojo A") ~ Precio_kg * (500 / 50),
    Alimento %in% c("Huevo blanco AA", "Huevo rojo AA") ~ Precio_kg * (500 / 60),
    Alimento %in% c("Huevo blanco extra", "Huevo rojo extra") ~ Precio_kg * (500 / 67),
    TRUE ~ Precio_kg / 2
  ))

##-------------------------------------------------------------##
## Integrar base de Limón (1996–2011) con SIPSA (2013–2018)    ##
##-------------------------------------------------------------##

# 1. Cargar la base del limón
limon_9611 <- readRDS("C:\\Users\\danie\\Downloads\\limon_convertido.rds")

# 2. Homogeneizar formato
limon_9611 <- limon_9611 %>%
  mutate(
    cod_mun = case_when(
      Mercado == "Cali"     ~ "76001",
      Mercado == "Medellín" ~ "05001",
      Mercado == "Bogotá"   ~ "11001",
      TRUE ~ NA_character_
    ),
    precio_500g = Precio_kg / 2,
    Alimento = as.character(Alimento),
    Year = as.integer(Year),
    Month = as.integer(Month),
    cod_mun = as.character(cod_mun),
    precio_500g = as.numeric(precio_500g)
  ) %>%
  select(Year, Month, cod_mun, Alimento, precio_500g)

# Cargar bases SIPSA 2013–2018
whole_list <- list()
for (k in 2013:2018) {
  whole_list[[as.character(k)]] <- readRDS(paste0("Precios al por mayor/Bases historicas/", k, ".rds")) %>%
    mutate(
      nombre_ciudad = case_when(
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
      ),
      cod_mun = case_when(
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
      ),
      precio_500g = case_when(
        str_detect(Alimento, regex("aceite", ignore_case = TRUE)) ~ Precio_kg * (500 / 920),
        Alimento %in% c("Huevo blanco A", "Huevo rojo A") ~ Precio_kg * (500 / 50),
        Alimento %in% c("Huevo blanco AA", "Huevo rojo AA") ~ Precio_kg * (500 / 60),
        Alimento %in% c("Huevo blanco extra", "Huevo rojo extra") ~ Precio_kg * (500 / 67),
        TRUE ~ Precio_kg / 2
      )
    ) %>%
    filter(!is.na(cod_mun)) %>%
    select(Year, Month, cod_mun, Alimento, precio_500g)
}

whole_18 <- whole_18 %>%
  mutate(
    Year = as.integer(Year),
    Month = as.integer(Month),
    cod_mun = as.character(cod_mun),
    Alimento = as.character(Alimento),
    precio_500g = as.numeric(precio_500g)
  )

whole_18 <- bind_rows(whole_18, limon_9611)

# Precios de julio y variaciones
precios_julio <- tribble(
  ~cod_mun, ~Alimento,        ~precio_julio, ~variacion_pct,
  "11001",  "Limón Tahití",   1075,          -2.10,  # Bogotá
  "76001",  "Limón Tahití",    918,           2.36,  # Cali
  "05001",  "Limón Tahití",    821,           0.67   # Medellín
)

# Calcular junio 
precios_junio <- precios_julio %>%
  mutate(
    Year = 2012,
    Month = 6,
    precio_500g = round(precio_julio / (1 + (variacion_pct / 100)), 0)
  ) %>%
  select(Year, Month, cod_mun, Alimento, precio_500g)

# precios de julio a octubre directamente
precios_jul_oct <- tribble(
  ~Year, ~Month, ~cod_mun, ~Alimento,        ~precio_500g,
  2012,     7,   "11001",  "Limón Tahití",   1075,  # Bogotá
  2012,     8,   "11001",  "Limón Tahití",   1502,
  2012,     9,   "11001",  "Limón Tahití",   2325,
  2012,    10,   "11001",  "Limón Tahití",   1121,
  
  2012,     7,   "76001",  "Limón Tahití",    918,  # Cali
  2012,     8,   "76001",  "Limón Tahití",   1257,
  2012,     9,   "76001",  "Limón Tahití",   1793,
  2012,    10,   "76001",  "Limón Tahití",   1472,
  
  2012,     7,   "05001",  "Limón Tahití",    821,  # Medellín
  2012,     8,   "05001",  "Limón Tahití",   1176,
  2012,     9,   "05001",  "Limón Tahití",   2039,
  2012,    10,   "05001",  "Limón Tahití",   1717
)

# Precios semanales de nov y dic
precios_semanales <- tribble(
  ~mes,  ~semana,       ~bogota, ~cali_santa_helena, ~cali_cavasa, ~medellin,
  "nov", "3–9 nov",      1055,     933,                 838,          856,
  "nov", "10–16 nov",     857,     833,                 719,          900,
  "nov", "17–23 nov",     907,     775,                 688,          700,
  "nov", "24–30 nov",     650,     683,                 588,          793,
  "dic", "1–7 dic",       943,     613,                 675,          786,
  "dic", "8–14 dic",      964,     692,                 710,          922,
  "dic", "15–21 dic",     817,     708,                 738,          856,
  "dic", "22–28 dic",    1000,     733,                 863,          883
)

# Calcular promedios mensuales
precios_mensuales <- precios_semanales %>%
  mutate(
    cali_promedio = (cali_santa_helena + cali_cavasa) / 2
  ) %>%
  group_by(mes) %>%
  summarise(
    precio_bog = round(mean(bogota, na.rm = TRUE)),
    precio_cali = round(mean(cali_promedio, na.rm = TRUE)),
    precio_med = round(mean(medellin, na.rm = TRUE)),
    .groups = "drop"
  )

# Extraer los valores numéricos de los promedios de noviembre y diciembre
precio_bog_nov  <- precios_mensuales$precio_bog[precios_mensuales$mes == "nov"]
precio_cali_nov <- precios_mensuales$precio_cali[precios_mensuales$mes == "nov"]
precio_med_nov  <- precios_mensuales$precio_med[precios_mensuales$mes == "nov"]

precio_bog_dic  <- precios_mensuales$precio_bog[precios_mensuales$mes == "dic"]
precio_cali_dic <- precios_mensuales$precio_cali[precios_mensuales$mes == "dic"]
precio_med_dic  <- precios_mensuales$precio_med[precios_mensuales$mes == "dic"]

# Convertir a tabla nov-dic con los valores ya evaluados
precios_nov_dic <- tribble(
  ~Year, ~Month, ~cod_mun, ~Alimento,        ~precio_500g,
  2012,    11,   "11001",  "Limón Tahití",   precio_bog_nov,
  2012,    11,   "76001",  "Limón Tahití",   precio_cali_nov,
  2012,    11,   "05001",  "Limón Tahití",   precio_med_nov,
  2012,    12,   "11001",  "Limón Tahití",   precio_bog_dic,
  2012,    12,   "76001",  "Limón Tahití",   precio_cali_dic,
  2012,    12,   "05001",  "Limón Tahití",   precio_med_dic
)

# Unir con las demás observaciones de 2012
precios_2012 <- bind_rows(precios_junio, precios_jul_oct, precios_nov_dic)

# Añadir a whole_18
whole_18 <- bind_rows(whole_18, precios_2012) %>%
  arrange(cod_mun, Alimento, Year, Month)

whole_18$Alimento[whole_18$Alimento == "Limón tahití"] = "Limón Tahití"

# Recalcular precios medios
whole_18_mean <- whole_18 %>%
  group_by(Year, Month, cod_mun, Alimento) %>%
  summarise(precio_medio = mean(precio_500g, na.rm = TRUE), .groups = "drop")


##------------------------------------##
## Cargar mapeo: DANE (IPC) - SIPSA   ##
##------------------------------------##

# Cargar mapeo retail–SIPSA
ipc_sipsa <- readxl::read_excel("Time-series\\mapeo_retail_sipsa.xlsx")

# Unir nombres de SIPSA a la base del DANE (usar mapeo)
retail_99_18 <- retail_99_18 %>%
  left_join(ipc_sipsa, by = c("articulo" = "retail"))

# Hacer left_join 
retail_whole_18 <- retail_99_18 %>%
  left_join(
    whole_18_mean %>%
      select(Year, Month, cod_mun, Alimento, precio_medio),
    by = c(
      "ano" = "Year",
      "mes_num" = "Month",
      "cod_mun" = "cod_mun",
      "sipsa" = "Alimento"
    )
  )

# Filtro para Bogotá, Medellín y Cali
whole_tres <- retail_whole_18 %>%
  filter(nombre_ciudad %in% c("CALI", "BOGOTÁ D.C.", "MEDELLÍN"))

whole_tres$precio_medio = as.numeric(whole_tres$precio_medio)

whole_lemon <- whole_tres %>% filter(nombre_ciudad == "CALI" & sipsa == "Limón Tahití")

# 5. Guardar para revisión
writexl::write_xlsx(whole_lemon, "Time-series\\V4_ipc_sipsa.xlsx")

##------------------------------------##
## Visualización: Limón Tahití en Cali ##
##------------------------------------##

# Filtrar por sipsa == "Limón Tahití" en Cali
limon_cali <- whole_tres %>%
  filter(nombre_ciudad == "CALI", sipsa == "Limón Tahití")

# Crear fecha
limon_cali <- limon_cali %>%
  mutate(fecha = as.Date(paste(ano, mes_num, "01", sep = "-")))


limon_cali_long <- limon_cali %>%
  select(fecha, precio_500g, precio_medio) %>%
  pivot_longer(cols = c(precio_500g, precio_medio),
               names_to = "fuente", values_to = "precio") %>%
  mutate(fuente = recode(fuente,
                         "precio_500g" = "Precio IPC",
                         "precio_medio" = "Precio SIPSA"))

# Gráfico
ggplot(limon_cali_long, aes(x = fecha, y = precio, color = fuente)) +
  geom_line(size = 1.2, na.rm = TRUE) +
  geom_point(size = 0.9, na.rm = TRUE) +
  scale_color_manual(values = c("Precio IPC" = "red", "Precio SIPSA" = "cyan4")) +
  labs(
    title = "Comparación de precios Retail (IPC) y Mayorista (SIPSA)",
    subtitle = "Ciudad: CALI – Artículo: LIMÓN TAHITÍ",
    x = "Fecha",
    y = "Precio (COP)",
    color = ""
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
