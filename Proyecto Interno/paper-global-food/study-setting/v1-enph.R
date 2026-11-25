# =============================================================================
# Participación del gasto en alimentos en el gasto corriente de los hogares
# 13 ciudades y áreas metropolitanas (dominios GEIH), ENPH 2016-2017
# Fuente: DANE, ENPH 2016-2017, Cuadro 14 (gasto corriente mensual de la
#         unidad de gasto según división CCIF, por ciudad)
# Salida: un archivo Excel con tres hojas
# =============================================================================

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(writexl)

ruta_datos  <- "C:\\Users\\Portatil\\Desktop\\Least-cost-diets-and-affordability\\Proyecto Interno\\paper-global-food\\study-setting\\data\\ciudades-enph-2017.xls"
ruta_salida <- "C:\\Users\\Portatil\\Desktop\\Least-cost-diets-and-affordability\\Proyecto Interno\\paper-global-food\\study-setting\\output"
dir.create(ruta_salida, showWarnings = FALSE)

# -----------------------------------------------------------------------------
# 1. Lectura del Cuadro 14
#    Cada ciudad es un bloque: una fila con el nombre de la ciudad, una fila
#    de encabezados ("Clasificación del gasto por división"), una fila con el
#    total de la ciudad y doce filas, una por división CCIF.
# -----------------------------------------------------------------------------
encabezado <- "Clasificación del gasto por división"

c14 <- read_excel(ruta_datos, sheet = "Cuadro 14", col_names = FALSE, col_types = "text") %>%
  select(1:5) %>%
  setNames(c("rubro", "hogares_miles", "cve_hogares", "gasto_millones", "cve_gasto")) %>%
  mutate(rubro  = str_squish(rubro),
         ciudad = if_else(lead(rubro) %in% encabezado, rubro, NA_character_)) %>%
  fill(ciudad) %>%
  filter(!is.na(ciudad), !is.na(rubro), rubro != encabezado)

# -----------------------------------------------------------------------------
# 2. Gasto total, alimentos en el hogar (división 01) y restaurantes y
#    hoteles (división 11), por ciudad
# -----------------------------------------------------------------------------
todas_ciudades <- c14 %>%
  mutate(tipo = case_when(rubro == ciudad              ~ "gasto_total",
                          str_detect(rubro, "^01 ")    ~ "gasto_alimentos_hogar",
                          str_detect(rubro, "^11 ")    ~ "gasto_restaurantes_hoteles",
                          TRUE                         ~ NA_character_),
         gasto_millones = as.numeric(gasto_millones)) %>%
  filter(!is.na(tipo), !is.na(gasto_millones)) %>%
  select(ciudad, tipo, gasto_millones) %>%
  pivot_wider(names_from = tipo, values_from = gasto_millones) %>%
  mutate(part_alimentos_hogar_pct       = 100 * gasto_alimentos_hogar / gasto_total,
         part_restaurantes_hoteles_pct  = 100 * gasto_restaurantes_hoteles / gasto_total)

cat("Ciudades leídas:", nrow(todas_ciudades), "(se esperan 38)\n")

# -----------------------------------------------------------------------------
# 3. Dominios de la GEIH
#    Barranquilla A.M. = Barranquilla + Soledad; Cali A.M. = Cali + Yumbo.
#    Las demás áreas metropolitanas ya vienen agregadas en la ENPH.
#    Nota: la ENPH no separa Puerto Santander dentro de Cúcuta A.M.;
#    se usa el agregado que publica el DANE.
# -----------------------------------------------------------------------------
dominio_13_enph <- tribble(
  ~ciudad_enph,           ~dominio,
  "Bogotá",               "Bogotá",
  "Medellín y A.M.",      "Medellín A.M.",
  "Cali",                 "Cali A.M.",
  "Yumbo",                "Cali A.M.",
  "Barranquilla",         "Barranquilla A.M.",
  "Soledad",              "Barranquilla A.M.",
  "Bucaramanga y A.M.",   "Bucaramanga A.M.",
  "Manizales y A.M.",     "Manizales A.M.",
  "Pereira y A.M.",       "Pereira A.M.",
  "Cúcuta y A.M.",        "Cúcuta A.M.",
  "Pasto",                "Pasto",
  "Ibagué",               "Ibagué",
  "Montería",             "Montería",
  "Cartagena",            "Cartagena",
  "Villavicencio",        "Villavicencio"
)

control <- dominio_13_enph %>%
  left_join(todas_ciudades, by = c("ciudad_enph" = "ciudad"))
if (any(is.na(control$gasto_total))) stop("Alguna ciudad del dominio no aparece en el Cuadro 14.")

por_dominio <- control %>%
  group_by(dominio) %>%
  summarise(across(c(gasto_total, gasto_alimentos_hogar, gasto_restaurantes_hoteles), sum),
            .groups = "drop")

total_13 <- por_dominio %>%
  summarise(dominio = "Total 13 ciudades y A.M.",
            across(c(gasto_total, gasto_alimentos_hogar, gasto_restaurantes_hoteles), sum))

dominio_13 <- bind_rows(por_dominio, total_13) %>%
  mutate(part_alimentos_hogar_pct      = 100 * gasto_alimentos_hogar / gasto_total,
         part_restaurantes_hoteles_pct = 100 * gasto_restaurantes_hoteles / gasto_total,
         part_alimentos_total_pct      = part_alimentos_hogar_pct + part_restaurantes_hoteles_pct) %>%
  arrange(dominio == "Total 13 ciudades y A.M.", desc(part_alimentos_hogar_pct))

print(dominio_13, n = Inf)

# -----------------------------------------------------------------------------
# 4. Salida
# -----------------------------------------------------------------------------
write_xlsx(
  list(dominio_13 = dominio_13, todas_ciudades = todas_ciudades, control = control),
  file.path(ruta_salida, "participacion_gasto_alimentos_enph_2017.xlsx")
)