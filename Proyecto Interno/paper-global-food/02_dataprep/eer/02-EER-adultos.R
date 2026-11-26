########################################################
## SCRIPT 02_dataprep/eer/02-EER-adultos.R
## Requerimientos energéticos estimados (EER) para adultos y
## adolescentes 14+ usando las ecuaciones IOM/NASEM 2023, con
## antropometría y actividad física de ENSIN y SABE por ciudad.
##
## Reads:  requirements_dir/AF_ADOLESCENTES.rds
##         requirements_dir/AF_ADULTOS.rds
##         requirements_dir/ANTROPOMETRIA.rds
##         requirements_dir/Cap1Parte1.rds
##         requirements_dir/Cap4.rds
##         requirements_dir/Cap7.rds
##         requirements_dir/Cap10.rds
## Writes: output_dir/230726_adult_eer.xlsx
########################################################

library(tidyverse)
library(writexl)
library(haven)

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

requirements_dir <- file.path(base_dir, "interno/02_dataprep/eer")
output_dir       <- file.path(base_dir, "interno/02_dataprep/eer/output")

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Cargar bases
af_ado <- readRDS(file.path(requirements_dir, "AF_ADOLESCENTES.rds"))
af_adu <- readRDS(file.path(requirements_dir, "AF_ADULTOS.rds"))
antro  <- readRDS(file.path(requirements_dir, "ANTROPOMETRIA.rds"))

cap1   <- readRDS(file.path(requirements_dir, "Cap1Parte1.rds"))
cap4   <- readRDS(file.path(requirements_dir, "Cap4.rds"))
cap7   <- readRDS(file.path(requirements_dir, "Cap7.rds"))
cap10  <- readRDS(file.path(requirements_dir, "Cap10.rds"))

# Rango plausible de peso (kg) y talla (cm). La encuesta SABE codifica "no sabe /
# no responde" como 777 en peso y talla, y la ENSIN trae algunos valores fuera de
# rango; sin este filtro entran al promedio como si fueran mediciones.
PESO_MAX  <- 250
TALLA_MIN <- 100
TALLA_MAX <- 250

# Función numérica
to_num <- function(x) {
  suppressWarnings(as.numeric(haven::zap_labels(x)))
}

# Funciones auxiliares
w_mean <- function(x, w) {
  ok <- !is.na(x) & !is.na(w) & w > 0
  if (!any(ok)) return(NA_real_)
  weighted.mean(x[ok], w[ok])
}

grupo_edad <- function(edad) {
  case_when(
    edad >= 14 & edad < 19 ~ "[14,19)",
    edad >= 19 & edad < 31 ~ "[19,31)",
    edad >= 31 & edad < 51 ~ "[31,51)",
    edad >= 51 & edad < 70 ~ "[51,70)",
    edad >= 70             ~ "[70,Inf)",
    TRUE ~ NA_character_
  )
}

# Ecuaciones IOM/NASEM 2023 y mapeo de actividad fisica (ENSIN a PAL)
source(file.path(base_dir, "interno/02_dataprep/aux-functions/eer_iom.R"), encoding = "UTF-8")

dominios_geo <- c(
  "Barranquilla", "Bogotá", "Bucaramanga", "Cali", "Cartagena",
  "Cúcuta", "Ibagué", "Manizales", "Medellín", "Montería",
  "Pasto", "Pereira", "Villavicencio", "Colombia"
)

# Agregar dominios antropometría
agregar_dominios_antropo <- function(df) {
  base <- df %>%
    mutate(
      Region = as.character(haven::zap_labels(Region)),
      Subregion = as.character(haven::zap_labels(Subregion))
    )
  
  bind_rows(
    base %>% filter(Subregion == "Barranquilla A. M.")                   %>% mutate(dominio_geo = "Barranquilla"),
    base %>% filter(Subregion == "Bogota")                               %>% mutate(dominio_geo = "Bogotá"),
    base %>% filter(Subregion == "Santanderes")                          %>% mutate(dominio_geo = "Bucaramanga"),
    base %>% filter(Subregion == "Cali A.M.")                            %>% mutate(dominio_geo = "Cali"),
    base %>% filter(Subregion == "Atlantico, San Andres, Bolivar Norte") %>% mutate(dominio_geo = "Cartagena"),
    base %>% filter(Subregion == "Santanderes")                          %>% mutate(dominio_geo = "Cúcuta"),
    base %>% filter(Subregion == "Tolima, Huila, Caqueta")               %>% mutate(dominio_geo = "Ibagué"),
    base %>% filter(Subregion == "Caldas, Risaralda, Quindio")           %>% mutate(dominio_geo = "Manizales"),
    base %>% filter(Subregion == "Medellin A.M.")                        %>% mutate(dominio_geo = "Medellín"),
    base %>% filter(Subregion == "Bolivar Sur, Sucre, Cordoba")          %>% mutate(dominio_geo = "Montería"),
    base %>% filter(Subregion == "Cauca y Nari+\xa6o sin Litoral")       %>% mutate(dominio_geo = "Pasto"),
    base %>% filter(Subregion == "Caldas, Risaralda, Quindio")           %>% mutate(dominio_geo = "Pereira"),
    base %>% filter(Subregion == "Orinoquia y Amazonia")                 %>% mutate(dominio_geo = "Villavicencio"),
    base %>% mutate(dominio_geo = "Colombia")
  )
}

# Agregar dominios actividad física
agregar_dominios_af <- function(df) {
  base <- df %>%
    mutate(
      Region = as.character(haven::zap_labels(Region)),
      Subregion = as.character(haven::zap_labels(Subregion))
    )
  
  bind_rows(
    base %>% filter(Subregion == "4")  %>% mutate(dominio_geo = "Barranquilla"),
    base %>% filter(Subregion == "5")  %>% mutate(dominio_geo = "Bogotá"),
    base %>% filter(Subregion == "14") %>% mutate(dominio_geo = "Bucaramanga"),
    base %>% filter(Subregion == "9")  %>% mutate(dominio_geo = "Cali"),
    base %>% filter(Subregion == "3")  %>% mutate(dominio_geo = "Cartagena"),
    base %>% filter(Subregion == "14") %>% mutate(dominio_geo = "Cúcuta"),
    base %>% filter(Subregion == "15") %>% mutate(dominio_geo = "Ibagué"),
    base %>% filter(Subregion == "8")  %>% mutate(dominio_geo = "Manizales"),
    base %>% filter(Subregion == "12") %>% mutate(dominio_geo = "Medellín"),
    base %>% filter(Subregion == "6")  %>% mutate(dominio_geo = "Montería"),
    base %>% filter(Subregion == "1")  %>% mutate(dominio_geo = "Pasto"),
    base %>% filter(Subregion == "8")  %>% mutate(dominio_geo = "Pereira"),
    base %>% filter(Subregion == "13") %>% mutate(dominio_geo = "Villavicencio"),
    base %>% mutate(dominio_geo = "Colombia")
  )
}

# Antropometría ENSIN
antropo_ensin <- agregar_dominios_antropo(antro) %>%
  mutate(
    ANTSEXO = to_num(ANTSEXO),
    AN_EDAD = to_num(AN_EDAD),
    AN_7 = to_num(AN_7),
    AN_8 = to_num(AN_8),
    FactorExpansionPer = to_num(FactorExpansionPer)
  ) %>%
  transmute(
    dominio_geo,
    sexo = case_when(
      ANTSEXO == 1 ~ "Masculino",
      ANTSEXO == 2 ~ "Femenino",
      TRUE ~ NA_character_
    ),
    edad = AN_EDAD,
    peso = AN_7,
    talla = AN_8,
    factor_exp = FactorExpansionPer
  ) %>%
  filter(
    dominio_geo %in% dominios_geo,
    !is.na(sexo), !is.na(edad), !is.na(peso), !is.na(talla), !is.na(factor_exp),
    edad >= 14, factor_exp > 0,
    peso > 0, peso < PESO_MAX, talla > TALLA_MIN, talla < TALLA_MAX
  )

# Antropometría SABE
antropo_sabe <- cap1 %>%
  mutate(
    P121 = to_num(P121),
    P122EDAD = to_num(P122EDAD)
  ) %>%
  transmute(
    NumIdentificador,
    sexo = case_when(
      P121 == 1 ~ "Masculino",
      P121 == 2 ~ "Femenino",
      TRUE ~ NA_character_
    ),
    edad = P122EDAD
  ) %>%
  left_join(
    cap10 %>%
      mutate(
        P1005PESO = to_num(P1005PESO),
        P1006TALLA = to_num(P1006TALLA)
      ) %>%
      transmute(
        NumIdentificador,
        peso = P1005PESO,
        talla = P1006TALLA
      ),
    by = "NumIdentificador"
  ) %>%
  transmute(
    dominio_geo = "Colombia",
    sexo,
    edad,
    peso,
    talla,
    factor_exp = 1
  ) %>%
  filter(
    !is.na(sexo), !is.na(edad), !is.na(peso), !is.na(talla),
    edad >= 60,
    peso > 0, peso < PESO_MAX, talla > TALLA_MIN, talla < TALLA_MAX
  )

# Base antropométrica final
antropo_base <- bind_rows(
  antropo_ensin %>% filter(!(dominio_geo == "Colombia" & edad >= 60)),
  antropo_sabe
)

insumos_antropo <- antropo_base %>%
  mutate(grupo_edad = grupo_edad(edad)) %>%
  filter(!is.na(grupo_edad)) %>%
  group_by(dominio_geo, sexo, grupo_edad) %>%
  summarise(
    edad_rep   = w_mean(edad, factor_exp),
    peso_prom  = w_mean(peso, factor_exp),
    talla_prom = w_mean(talla, factor_exp),
    .groups = "drop"
  )

# Actividad física adolescentes ENSIN
af_ado_limpia <- agregar_dominios_af(af_ado) %>%
  mutate(
    AFSEXO = to_num(AFSEXO),
    AFEDAD = to_num(AFEDAD),
    FactorExpansionAF = to_num(FactorExpansionAF),
    activof = to_num(activof)
  ) %>%
  transmute(
    dominio_geo,
    sexo = case_when(
      AFSEXO == 1 ~ "Masculino",
      AFSEXO == 2 ~ "Femenino",
      TRUE ~ NA_character_
    ),
    edad = AFEDAD,
    factor_exp = FactorExpansionAF,
    af_cat = case_when(
      activof == 1 ~ "Moderada",
      activof == 0 ~ "Ligera",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(
    dominio_geo %in% dominios_geo,
    !is.na(sexo), !is.na(edad), !is.na(factor_exp), !is.na(af_cat),
    edad >= 14, edad < 19, factor_exp > 0
  )

# Actividad física adultos ENSIN
af_adu_limpia <- agregar_dominios_af(af_adu) %>%
  mutate(
    AFSEXO = to_num(AFSEXO),
    AFEDAD = to_num(AFEDAD),
    FactorExpansionAF = to_num(FactorExpansionAF),
    meetcombinatt = to_num(meetcombinatt)
  ) %>%
  transmute(
    dominio_geo,
    sexo = case_when(
      AFSEXO == 1 ~ "Masculino",
      AFSEXO == 2 ~ "Femenino",
      TRUE ~ NA_character_
    ),
    edad = AFEDAD,
    factor_exp = FactorExpansionAF,
    af_cat = case_when(
      meetcombinatt == 1 ~ "Moderada",
      meetcombinatt == 0 ~ "Ligera",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(
    dominio_geo %in% dominios_geo,
    !is.na(sexo), !is.na(edad), !is.na(factor_exp), !is.na(af_cat),
    edad >= 19, factor_exp > 0
  )

af_ensin <- bind_rows(af_ado_limpia, af_adu_limpia)

insumos_af_ensin <- af_ensin %>%
  mutate(grupo_edad = grupo_edad(edad)) %>%
  filter(!is.na(grupo_edad)) %>%
  group_by(dominio_geo, sexo, grupo_edad, af_cat) %>%
  summarise(freq = sum(factor_exp, na.rm = TRUE), .groups = "drop") %>%
  group_by(dominio_geo, sexo, grupo_edad) %>%
  slice_max(order_by = freq, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  transmute(
    dominio_geo,
    sexo,
    grupo_edad,
    AF_predominante = af_cat
  )

# Actividad física SABE mayores
af_sabe <- cap1 %>%
  mutate(
    P121 = to_num(P121),
    P122EDAD = to_num(P122EDAD)
  ) %>%
  transmute(
    NumIdentificador,
    sexo = case_when(
      P121 == 1 ~ "Masculino",
      P121 == 2 ~ "Femenino",
      TRUE ~ NA_character_
    ),
    edad = P122EDAD
  ) %>%
  left_join(
    cap4 %>%
      mutate(
        P407B_2 = to_num(P407B_2),
        P409_11 = to_num(P409_11)
      ) %>%
      transmute(
        NumIdentificador,
        P407B_2,
        P409_11
      ),
    by = "NumIdentificador"
  ) %>%
  left_join(
    cap7 %>%
      mutate(
        P719 = to_num(P719),
        P720 = to_num(P720),
        P721 = to_num(P721)
      ) %>%
      transmute(
        NumIdentificador,
        P719,
        P720,
        P721
      ),
    by = "NumIdentificador"
  ) %>%
  filter(!is.na(sexo), !is.na(edad), edad >= 60) %>%
  transmute(
    dominio_geo = "Colombia",
    sexo,
    edad,
    factor_exp = 1,
    af_cat = case_when(
      P407B_2 == 1 | P409_11 == 1 | P719 == 1 | P720 == 1 | P721 == 1 ~ "Moderada",
      TRUE ~ "Ligera"
    )
  )

insumos_af_sabe <- af_sabe %>%
  mutate(grupo_edad = grupo_edad(edad)) %>%
  filter(!is.na(grupo_edad)) %>%
  group_by(dominio_geo, sexo, grupo_edad, af_cat) %>%
  summarise(freq = sum(factor_exp, na.rm = TRUE), .groups = "drop") %>%
  group_by(dominio_geo, sexo, grupo_edad) %>%
  slice_max(order_by = freq, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  transmute(
    dominio_geo,
    sexo,
    grupo_edad,
    AF_predominante = af_cat
  )

# Actividad física final
insumos_af <- bind_rows(
  insumos_af_ensin %>% filter(!(dominio_geo == "Colombia" & grupo_edad == "[70,Inf)")),
  insumos_af_sabe  %>% filter(dominio_geo == "Colombia", grupo_edad == "[70,Inf)")
) %>%
  mutate(
    pal_iom = map_pal_iom(AF_predominante)
  )

# Tabla final
tabla_eer <- insumos_antropo %>%
  left_join(insumos_af, by = c("dominio_geo", "sexo", "grupo_edad")) %>%
  mutate(
    eer_kcal_dia = pmap_dbl(
      list(edad_rep, talla_prom, peso_prom, sexo, pal_iom),
      ~ eer_iom_2023(
        age     = ..1,
        height  = ..2,
        weight  = ..3,
        sexo    = ..4,
        pal_cat = ..5
      )
    )
  ) %>%
  select(
    dominio_geo,
    sexo,
    grupo_edad,
    edad_rep,
    peso_prom,
    talla_prom,
    AF_predominante,
    pal_iom,
    eer_kcal_dia
  ) %>%
  arrange(dominio_geo, sexo, grupo_edad)

# Guardar base
writexl::write_xlsx(
  tabla_eer,
  file.path(output_dir, "230726_adult_eer.xlsx")
)

tabla_eer
