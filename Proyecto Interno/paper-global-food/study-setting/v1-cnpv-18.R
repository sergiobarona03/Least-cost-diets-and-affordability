# =============================================================================
# Participación de las 13 ciudades y áreas metropolitanas (dominio GEIH)
# en la población nacional, 2025
# Fuente: DANE, Proyecciones de población municipales por área geográfica,
#         2018-2042 (base CNPV 2018), hoja 3
# Salida: un archivo Excel con dos hojas (resultado y control)
# =============================================================================

library(readxl)
library(dplyr)
library(tidyr)
library(writexl)

ruta_datos  <- "C:\\Users\\Portatil\\Desktop\\Least-cost-diets-and-affordability\\Proyecto Interno\\paper-global-food\\study-setting\\data\\PPED-AreaMun-2018-2042_VP.xlsx"
ruta_salida <- "C:\\Users\\Portatil\\Desktop\\Least-cost-diets-and-affordability\\Proyecto Interno\\paper-global-food\\study-setting\\output"
dir.create(ruta_salida, showWarnings = FALSE)

anio_ref <- 2025

# -----------------------------------------------------------------------------
# 1. Lectura
#    Los encabezados están en la sexta fila de la hoja; la fila siguiente
#    está vacía. Se renombran las columnas por posición para evitar problemas
#    con las tildes de "AÑO" y "ÁREA GEOGRÁFICA".
# -----------------------------------------------------------------------------
pob <- read_excel(ruta_datos, sheet = 3, skip = 5, col_types = "text")
pob <- setNames(pob, c("dp", "dp_nom", "mpio", "mpio_nom", "anio", "area", "total"))

pob <- pob %>%
  filter(!is.na(mpio)) %>%
  mutate(anio  = as.integer(anio),
         total = as.numeric(total))

# Valores de área esperados: "Cabecera Municipal",
# "Centros Poblados y Rural Disperso" y "Total"
print(count(pob, area))
cat("Municipios distintos:", n_distinct(pob$mpio), "\n")

pob_anio <- pob %>% filter(anio == anio_ref)

# -----------------------------------------------------------------------------
# 2. Composición de los dominios de la GEIH
#    VERIFICAR con la ficha metodológica de la GEIH antes de usar la cifra.
# -----------------------------------------------------------------------------
dominio_13 <- tribble(
  ~ciudad,              ~mpio,
  "Bogotá",             "11001",
  "Medellín A.M.",      "05001",  # Medellín
  "Medellín A.M.",      "05079",  # Barbosa
  "Medellín A.M.",      "05088",  # Bello
  "Medellín A.M.",      "05129",  # Caldas
  "Medellín A.M.",      "05212",  # Copacabana
  "Medellín A.M.",      "05266",  # Envigado
  "Medellín A.M.",      "05308",  # Girardota
  "Medellín A.M.",      "05360",  # Itagüí
  "Medellín A.M.",      "05380",  # La Estrella
  "Medellín A.M.",      "05631",  # Sabaneta
  "Cali A.M.",          "76001",  # Cali
  "Cali A.M.",          "76892",  # Yumbo
  "Barranquilla A.M.",  "08001",  # Barranquilla
  "Barranquilla A.M.",  "08758",  # Soledad
  "Bucaramanga A.M.",   "68001",  # Bucaramanga
  "Bucaramanga A.M.",   "68276",  # Floridablanca
  "Bucaramanga A.M.",   "68307",  # Girón
  "Bucaramanga A.M.",   "68547",  # Piedecuesta
  "Manizales A.M.",     "17001",  # Manizales
  "Manizales A.M.",     "17873",  # Villamaría
  "Pereira A.M.",       "66001",  # Pereira
  "Pereira A.M.",       "66170",  # Dosquebradas
  "Pereira A.M.",       "66400",  # La Virginia
  "Cúcuta A.M.",        "54001",  # Cúcuta
  "Cúcuta A.M.",        "54261",  # El Zulia
  "Cúcuta A.M.",        "54405",  # Los Patios
  "Cúcuta A.M.",        "54874",  # Villa del Rosario
  "Pasto",              "52001",
  "Ibagué",             "73001",
  "Montería",           "23001",
  "Cartagena",          "13001",
  "Villavicencio",      "50001"
)

# Control: que todos los códigos existan y que los nombres sean los esperados
control <- dominio_13 %>%
  left_join(distinct(pob_anio, mpio, mpio_nom, dp_nom), by = "mpio")
print(control, n = Inf)
if (any(is.na(control$mpio_nom))) stop("Hay códigos de municipio que no aparecen en la base.")

# -----------------------------------------------------------------------------
# 3. Población por ciudad y participación en el total nacional
# -----------------------------------------------------------------------------
nacional <- pob_anio %>%
  group_by(area) %>%
  summarise(pob = sum(total), .groups = "drop") %>%
  pivot_wider(names_from = area, values_from = pob)

pob_nacional_total     <- nacional[["Total"]]
pob_nacional_cabeceras <- nacional[["Cabecera Municipal"]]

por_ciudad <- pob_anio %>%
  inner_join(dominio_13, by = "mpio") %>%
  filter(area %in% c("Cabecera Municipal", "Total")) %>%
  group_by(ciudad, area) %>%
  summarise(pob = sum(total), .groups = "drop") %>%
  pivot_wider(names_from = area, values_from = pob) %>%
  rename(pob_cabecera = `Cabecera Municipal`, pob_total = Total)

total_13 <- por_ciudad %>%
  summarise(ciudad       = "Total 13 ciudades y A.M.",
            pob_cabecera = sum(pob_cabecera),
            pob_total    = sum(pob_total))

resultado <- bind_rows(por_ciudad, total_13) %>%
  mutate(
    anio                          = anio_ref,
    part_nacional_cabecera_pct    = 100 * pob_cabecera / pob_nacional_total,
    part_nacional_total_pct       = 100 * pob_total / pob_nacional_total,
    part_cabeceras_nacionales_pct = 100 * pob_cabecera / pob_nacional_cabeceras
  ) %>%
  select(anio, ciudad, pob_cabecera, pob_total,
         part_nacional_cabecera_pct, part_nacional_total_pct,
         part_cabeceras_nacionales_pct)

resultado <- bind_rows(
  resultado,
  tibble(anio = anio_ref, ciudad = "Nacional",
         pob_cabecera = pob_nacional_cabeceras, pob_total = pob_nacional_total)
)

print(resultado, n = Inf)

# -----------------------------------------------------------------------------
# 4. Salida
# -----------------------------------------------------------------------------
write_xlsx(
  list(resultado = resultado, control_municipios = control),
  file.path(ruta_salida, "poblacion_13_ciudades_2025.xlsx")
)