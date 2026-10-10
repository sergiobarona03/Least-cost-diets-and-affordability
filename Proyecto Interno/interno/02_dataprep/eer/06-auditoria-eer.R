########################################################
## SCRIPT 02_dataprep/eer/06-auditoria-eer.R
## Auditoria de los requerimientos de energia (EER) de los miembros de los hogares
## de 3, 4 y 5 personas.
##
## Para cada miembro y ciudad reune la antropometria, la actividad fisica y el EER
## que usan los modelos; recalcula el EER de los miembros con ecuaciones IOM/NASEM
## desde la antropometria y comprueba que coincide; y compara con la energia de
## referencia de FoodpriceR y con la recomendacion energetica de las GABA.
##
## Reads:  output_dir/230726_adult_eer.xlsx
##         output_dir/230726_agg_eer.xlsx
##         output_dir/041026_gestantes_incremento.rds
##         household_dir/household_eer{,_4m,_4m_t3,_5m}.rds
##         FoodpriceR::EER_LL
##         interno/output/gabas/gaba_energy_base.rds
## Writes: output_dir/081026_auditoria_eer.xlsx
##         output_dir/081026_auditoria_eer.rds
########################################################

library(tidyverse)
library(readxl)
library(writexl)
library(FoodpriceR)

# ============================================================
# Rutas
# ============================================================

dirs <- c(
  "C:/Users/Portatil/Desktop/Least-cost-diets-and-affordability/Proyecto Interno",
  "C:/Users/danie/OneDrive/Escritorio/Least-cost-diets-and-affordability/Proyecto Interno"
)

base_dir <- dirs[dir.exists(dirs)][1]

if (is.na(base_dir)) {
  stop("Ninguno de los directorios existe")
}

output_dir    <- file.path(base_dir, "interno/02_dataprep/eer/output")
household_dir <- file.path(base_dir, "interno/02_dataprep/household eer")
gaba_dir      <- file.path(base_dir, "interno/output/gabas")

source(file.path(base_dir, "interno/02_dataprep/aux-functions/eer_iom.R"), encoding = "UTF-8")
source(file.path(base_dir, "interno/02_dataprep/aux-functions/eer_hogar.R"), encoding = "UTF-8")

# -----------------------------------------------------------------------
# Insumos
# -----------------------------------------------------------------------

adulto <- read_excel(file.path(output_dir, "230726_adult_eer.xlsx"))
gest   <- readRDS(file.path(output_dir, "041026_gestantes_incremento.rds"))

leer_hogar <- function(sufijo) {
  readRDS(file.path(household_dir, paste0("household_eer", sufijo, ".rds"))) %>%
    transmute(ciudad, sex, rango, eer)
}
usado <- bind_rows(
  leer_hogar("")    %>% mutate(hogar = "3"),
  leer_hogar("_4m") %>% mutate(hogar = "4"),
  leer_hogar("_5m") %>% mutate(hogar = "5")
)
t3 <- leer_hogar("_4m_t3") %>% filter(rango == "[31,51) gest") %>% rename(eer_t3 = eer)

ciudad_dominio <- c(
  BARRANQUILLA = "Barranquilla", BOGOTA = "Bogotá", BUCARAMANGA = "Bucaramanga",
  CALI = "Cali", CARTAGENA = "Cartagena", CUCUTA = "Cúcuta", IBAGUE = "Ibagué",
  MANIZALES = "Manizales", MEDELLIN = "Medellín", MONTERIA = "Montería",
  PASTO = "Pasto", PEREIRA = "Pereira", VILLAVICENCIO = "Villavicencio"
)

# Energia de referencia de FoodpriceR por grupo de edad y sexo, y de las GABA
ref_fp <- FoodpriceR::EER_LL %>%
  transmute(sex = if_else(Sex == 0, "Masculino", "Femenino"), edad_foodpricer = Age, eer_foodpricer = Energy)

map_gaba <- tribble(
  ~rango,         ~edad,
  "[10, 14)",     "10 to 13",
  "[14,19)",      "14 to 17",
  "[31,51)",      "19 to 59",
  "[31,51) gest", "19 to 59",
  "[51,70)",      "> 60"
)
gaba <- readRDS(file.path(gaba_dir, "gaba_energy_base.rds")) %>%
  filter(tipo_fila == "recomendacion") %>%
  transmute(edad, sex = if_else(sexo == "male", "Masculino", "Femenino"), eer_gaba = energia_kcal)

# -----------------------------------------------------------------------
# Tabla por miembro y ciudad
# -----------------------------------------------------------------------

miembros <- MIEMBROS_EER %>%
  mutate(fuente = if_else(rango_eer == "[10, 14)", "RIEN, actividad moderada", "IOM/NASEM 2023"))

tabla <- usado %>%
  distinct(ciudad, sex, rango, eer) %>%
  inner_join(miembros, by = c("sex" = "sex", "rango" = "rango_hogar")) %>%
  mutate(dominio_geo = unname(ciudad_dominio[ciudad])) %>%
  left_join(adulto %>% select(dominio_geo, sexo, grupo_edad, edad_rep, peso_prom, talla_prom,
                              AF_predominante, pal_iom),
            by = c("dominio_geo", "sex" = "sexo", "rango_eer" = "grupo_edad")) %>%
  left_join(ref_fp, by = c("sex", "edad_foodpricer")) %>%
  left_join(map_gaba, by = "rango") %>%
  left_join(gaba, by = c("edad", "sex")) %>%
  mutate(
    eer_recalculado = pmap_dbl(list(edad_rep, talla_prom, peso_prom, sex, pal_iom),
                               ~ eer_iom_2023(age = ..1, height = ..2, weight = ..3, sexo = ..4, pal_cat = ..5)),
    eer_base = if_else(gestante, eer - gest$incremento, eer),
    dif_recalculo = if_else(fuente == "IOM/NASEM 2023", eer_base - eer_recalculado, NA_real_)
  ) %>%
  select(miembro, ciudad, rango, fuente, edad_rep, peso_prom, talla_prom, AF_predominante, pal_iom,
         eer, eer_base, eer_recalculado, dif_recalculo, eer_foodpricer, eer_gaba) %>%
  distinct() %>%
  arrange(miembro, ciudad)

# Controles: el EER que usan los modelos coincide con el recalculado desde la antropometria
ga <- tabla %>% filter(miembro == "Mujer gestante (31-50)") %>% arrange(ciudad)
mu <- tabla %>% filter(miembro == "Mujer adulta (31-50)") %>% arrange(ciudad)
controles <- tibble(
  control = c("miembros x ciudades", "max |EER usado - EER recalculado| (kcal)",
              "ciudades sin EER", "gestante = mujer 31-50 + incremento (max dif, kcal)"),
  valor = c(nrow(tabla),
            max(abs(tabla$dif_recalculo), na.rm = TRUE),
            sum(is.na(tabla$eer)),
            max(abs(ga$eer_base - mu$eer)))
)
stopifnot(nrow(tabla) == nrow(miembros) * length(ciudad_dominio),
          controles$valor[2] < 1e-6, controles$valor[3] == 0, controles$valor[4] < 1e-6)

# -----------------------------------------------------------------------
# Resumen por miembro (promedio de las 13 ciudades) y gestante
# -----------------------------------------------------------------------

resumen <- tabla %>%
  group_by(miembro, fuente) %>%
  summarise(eer_min = min(eer), eer_max = max(eer),
            across(c(edad_rep, peso_prom, talla_prom, eer, eer_foodpricer, eer_gaba),
                   ~ mean(.x, na.rm = TRUE)),
            ciudades_ligera = sum(AF_predominante == "Ligera", na.rm = TRUE),
            ciudades_moderada = sum(AF_predominante == "Moderada", na.rm = TRUE),
            .groups = "drop") %>%
  mutate(razon_foodpricer = eer / eer_foodpricer, razon_gaba = eer / eer_gaba)

gestante <- tabla %>%
  filter(miembro == "Mujer gestante (31-50)") %>%
  select(ciudad, eer_mujer_no_gestante = eer_base, eer_ponderado = eer) %>%
  left_join(t3 %>% select(ciudad, eer_tercer_trimestre = eer_t3), by = "ciudad")

trimestres <- gest$por_trimestre %>%
  select(trimestre, mujeres, participacion, incremento_kcal)

salida <- list(miembros_ciudad = tabla, resumen = resumen, gestante = gestante,
               trimestres = trimestres, controles = controles)
saveRDS(salida, file.path(output_dir, "081026_auditoria_eer.rds"))
write_xlsx(salida, file.path(output_dir, "081026_auditoria_eer.xlsx"))

message("Auditoria lista: ", nrow(tabla), " combinaciones miembro x ciudad.")
print(as.data.frame(resumen %>% mutate(across(where(is.numeric), ~ round(.x, 2)))), width = 250)
print(as.data.frame(controles))
