# ====================================================================
# ESTIMACIÓN DE VARIABLES PARA ECUACIONES IOM (ENSIN 2015 + SABE 2015)
# ====================================================================

library(haven)
library(dplyr)
library(readr)

# Directorios
base_dir  <- "C:/Users/danie/OneDrive/Documentos/least-cost/EER"
ensin_dir <- file.path(base_dir, "ensin-2015")
sabe_dir  <- file.path(base_dir, "base-de-datos-SABE-2015", "Base de datos y diccionario", "Base de datos - Capitulos")

# Factor de expansión y grupos etarios
w_mean <- function(x, w){
  ok <- !is.na(x) & !is.na(w) & w > 0
  if(!any(ok)) return(NA_real_)
  weighted.mean(x[ok], w[ok])
}

grupo_edad_iom <- function(edad){
  case_when(
    edad >= 14 & edad < 18 ~ "[14,18)",
    edad >= 18 & edad < 31 ~ "[18,31)",
    edad >= 31 & edad < 51 ~ "[31,51)",
    edad >= 51 & edad < 70 ~ "[51,70)",
    edad >= 70             ~ "[70,Inf)",
    TRUE ~ NA_character_
  )
}

# Antropometría (ENSIN)
antropo <- read_dta(file.path(ensin_dir, "Formato_Stata", "ANTROPOMETRIA.DTA"))

antropo_base <- antropo %>%
  transmute(
    dominio_geo = case_when(
      Subregion == "Cali A.M."     ~ "Cali",
      Subregion == "Bogota"        ~ "Bogotá",
      Subregion == "Medellin A.M." ~ "Medellín",
      TRUE ~ "Colombia"
    ),
    sexo = case_when(
      ANTSEXO == 1 ~ "Masculino",
      ANTSEXO == 2 ~ "Femenino",
      TRUE ~ NA_character_
    ),
    edad = as.numeric(AN_EDAD),
    peso = as.numeric(AN_7),
    talla = as.numeric(AN_8),
    factor_exp = as.numeric(FactorExpansionPer)
  ) %>%
  filter(
    !is.na(sexo), !is.na(edad), !is.na(peso), !is.na(talla), !is.na(factor_exp),
    edad >= 14, peso > 0, talla > 0, factor_exp > 0
  )

insumos_antropo <- antropo_base %>%
  mutate(grupo_edad = grupo_edad_iom(edad)) %>%
  filter(!is.na(grupo_edad)) %>%
  group_by(dominio_geo, sexo, grupo_edad) %>%
  summarise(
    edad_rep   = w_mean(edad, factor_exp),
    peso_prom  = w_mean(peso, factor_exp),
    talla_prom = w_mean(talla, factor_exp),
    .groups = "drop"
  )

# Actividad física (ENSIN)
af_ado <- read_dta(file.path(ensin_dir, "Formato_Stata", "AF_ADOLESCENTES.dta"))
af_adu <- read_dta(file.path(ensin_dir, "Formato_Stata", "AF_ADULTOS.dta"))

af_ensin <- bind_rows(
  af_ado %>%
    transmute(
      dominio_geo = case_when(
        Subregion == "9"  ~ "Cali",
        Subregion == "5"  ~ "Bogotá",
        Subregion == "12" ~ "Medellín",
        TRUE ~ "Colombia"
      ),
      sexo = case_when(
        AFSEXO == 1 ~ "Masculino",
        AFSEXO == 2 ~ "Femenino",
        TRUE ~ NA_character_
      ),
      edad = as.numeric(AFEDAD),
      factor_exp = as.numeric(`FactorExpansiónAF`),
      af_cat = case_when(
        activof == 1 ~ "Moderada",
        activof == 0 ~ "Ligera",
        TRUE ~ NA_character_
      )
    ),
  af_adu %>%
    transmute(
      dominio_geo = case_when(
        Subregion == "9"  ~ "Cali",
        Subregion == "5"  ~ "Bogotá",
        Subregion == "12" ~ "Medellín",
        TRUE ~ "Colombia"
      ),
      sexo = case_when(
        AFSEXO == 1 ~ "Masculino",
        AFSEXO == 2 ~ "Femenino",
        TRUE ~ NA_character_
      ),
      edad = as.numeric(AFEDAD),
      factor_exp = as.numeric(`FactorExpansiónAF`),
      af_cat = case_when(
        meetcombinatt == 1 ~ "Moderada",
        meetcombinatt == 0 ~ "Ligera",
        TRUE ~ NA_character_
      )
    )
) %>%
  filter(
    !is.na(sexo), !is.na(edad), !is.na(factor_exp), !is.na(af_cat),
    edad >= 14, factor_exp > 0
  )

# Actividad física (SABE) para mayores
cap1 <- read_tsv(file.path(sabe_dir, "Cap1Parte1.txt"), show_col_types = FALSE)
cap4 <- read_tsv(file.path(sabe_dir, "Cap4.txt"), show_col_types = FALSE)
cap7 <- read_tsv(file.path(sabe_dir, "Cap7.txt"), show_col_types = FALSE)

af_sabe <- cap1 %>%
  transmute(
    NumIdentificador,
    sexo = case_when(
      P121 == 1 ~ "Masculino",
      P121 == 2 ~ "Femenino",
      TRUE ~ NA_character_
    ),
    edad = as.numeric(P122EDAD)
  ) %>%
  left_join(
    cap4 %>% select(NumIdentificador, P407B_2, P409_11),
    by = "NumIdentificador"
  ) %>%
  left_join(
    cap7 %>% select(NumIdentificador, P719, P720, P721),
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

# Resumen de actividad física
resumen_af <- function(data){
  data %>%
    mutate(grupo_edad = grupo_edad_iom(edad)) %>%
    filter(!is.na(grupo_edad)) %>%
    group_by(dominio_geo, sexo, grupo_edad, af_cat) %>%
    summarise(freq = sum(factor_exp, na.rm = TRUE), .groups = "drop") %>%
    group_by(dominio_geo, sexo, grupo_edad) %>%
    slice_max(order_by = freq, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    rename(af_predominante = af_cat)
}

insumos_af_ensin <- resumen_af(af_ensin)
insumos_af_sabe  <- resumen_af(af_sabe)

insumos_af <- bind_rows(
  insumos_af_ensin %>% filter(!(dominio_geo == "Colombia" & grupo_edad == "[70,Inf)")),
  insumos_af_sabe  %>% filter(dominio_geo == "Colombia", grupo_edad == "[70,Inf)")
)

# Tabla final
variables_iom <- insumos_antropo %>%
  left_join(insumos_af, by = c("dominio_geo", "sexo", "grupo_edad")) %>%
  arrange(dominio_geo, sexo, grupo_edad)

variables_iom