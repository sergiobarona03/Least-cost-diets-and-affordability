# =========================================================
# EER IOM / NASEM 2023
# =========================================================

library(tidyverse)

# Directorios
input_dir  <- "C:/Users/danie/OneDrive/Escritorio/Least-cost-diets-and-affordability/Proyecto Interno/food-security-paper/input/requirements"
output_dir <- "C:/Users/danie/OneDrive/Escritorio/Least-cost-diets-and-affordability/Proyecto Interno/food-security-paper/output/eer"

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Cargar bases
af_ado <- readRDS(file.path(input_dir, "AF_ADOLESCENTES.rds"))
af_adu <- readRDS(file.path(input_dir, "AF_ADULTOS.rds"))
antro  <- readRDS(file.path(input_dir, "ANTROPOMETRIA.rds"))

cap1   <- readRDS(file.path(input_dir, "Cap1Parte1.rds"))
cap4   <- readRDS(file.path(input_dir, "Cap4.rds"))
cap7   <- readRDS(file.path(input_dir, "Cap7.rds"))
cap10  <- readRDS(file.path(input_dir, "Cap10.rds"))

# Funciones 
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

map_pal_iom <- function(AF_predominante) {
  case_when(
    AF_predominante == "Ligera"   ~ "Low active",
    AF_predominante == "Moderada" ~ "Active",
    AF_predominante == "Inactiva" ~ "Inactive",
    AF_predominante == "Alta"     ~ "Very active",
    TRUE ~ NA_character_
  )
}

eer_iom_2023 <- function(age, height, weight, sexo, pal_cat) {
  
  if (any(is.na(c(age, height, weight))) || is.na(sexo) || is.na(pal_cat)) {
    return(NA_real_)
  }
  
  if (age >= 14 && age < 19) {
    if (sexo == "Masculino" && pal_cat == "Inactive")   return(-447.51 + 3.68 * age + 13.01 * height + 13.15 * weight + 20)
    if (sexo == "Masculino" && pal_cat == "Low active") return(19.12 + 3.68 * age + 8.62 * height + 20.28 * weight + 20)
    if (sexo == "Masculino" && pal_cat == "Active")     return(-388.19 + 3.68 * age + 12.66 * height + 20.46 * weight + 20)
    if (sexo == "Masculino" && pal_cat == "Very active") return(-671.75 + 3.68 * age + 15.38 * height + 23.25 * weight + 20)
    
    if (sexo == "Femenino" && pal_cat == "Inactive")    return(55.59 - 22.25 * age + 8.43 * height + 17.07 * weight + 20)
    if (sexo == "Femenino" && pal_cat == "Low active")  return(-297.54 - 22.25 * age + 12.77 * height + 14.73 * weight + 20)
    if (sexo == "Femenino" && pal_cat == "Active")      return(-189.55 - 22.25 * age + 11.74 * height + 18.34 * weight + 20)
    if (sexo == "Femenino" && pal_cat == "Very active") return(-709.59 - 22.25 * age + 18.22 * height + 14.25 * weight + 20)
  }
  
  if (age >= 19) {
    if (sexo == "Masculino" && pal_cat == "Inactive")   return(753.07 - 10.83 * age + 6.50 * height + 14.10 * weight)
    if (sexo == "Masculino" && pal_cat == "Low active") return(581.47 - 10.83 * age + 8.30 * height + 14.94 * weight)
    if (sexo == "Masculino" && pal_cat == "Active")     return(1004.82 - 10.83 * age + 6.52 * height + 15.91 * weight)
    if (sexo == "Masculino" && pal_cat == "Very active") return(-517.88 - 10.83 * age + 15.61 * height + 19.11 * weight)
    
    if (sexo == "Femenino" && pal_cat == "Inactive")    return(584.90 - 7.01 * age + 5.72 * height + 11.71 * weight)
    if (sexo == "Femenino" && pal_cat == "Low active")  return(575.77 - 7.01 * age + 6.60 * height + 12.14 * weight)
    if (sexo == "Femenino" && pal_cat == "Active")      return(710.25 - 7.01 * age + 6.54 * height + 12.34 * weight)
    if (sexo == "Femenino" && pal_cat == "Very active") return(511.83 - 7.01 * age + 9.07 * height + 12.56 * weight)
  }
  
  NA_real_
}

# Agregar dominios 
agregar_dominios_antropo <- function(df) {
  base <- df %>%
    mutate(
      Region = as.character(Region),
      Subregion = as.character(Subregion)
    )
  
  bind_rows(
    base %>% filter(Subregion == "Cali A.M.")     %>% mutate(dominio_geo = "Cali"),
    base %>% filter(Subregion == "Bogota")        %>% mutate(dominio_geo = "Bogotá"),
    base %>% filter(Subregion == "Medellin A.M.") %>% mutate(dominio_geo = "Medellín"),
    base %>% mutate(dominio_geo = "Colombia")
  )
}

agregar_dominios_af <- function(df) {
  base <- df %>%
    mutate(
      Region = as.character(Region),
      Subregion = as.character(Subregion)
    )
  
  bind_rows(
    base %>% filter(Subregion == "9")  %>% mutate(dominio_geo = "Cali"),
    base %>% filter(Subregion == "5")  %>% mutate(dominio_geo = "Bogotá"),
    base %>% filter(Subregion == "12") %>% mutate(dominio_geo = "Medellín"),
    base %>% mutate(dominio_geo = "Colombia")
  )
}

# Antropometría ENSIN
antropo_ensin <- agregar_dominios_antropo(antro) %>%
  transmute(
    dominio_geo,
    sexo = case_when(
      ANTSEXO %in% c(1, "1") ~ "Masculino",
      ANTSEXO %in% c(2, "2") ~ "Femenino",
      TRUE ~ NA_character_
    ),
    edad = suppressWarnings(as.numeric(AN_EDAD)),
    peso = suppressWarnings(as.numeric(AN_7)),
    talla = suppressWarnings(as.numeric(AN_8)),
    factor_exp = suppressWarnings(as.numeric(FactorExpansionPer))
  ) %>%
  filter(
    dominio_geo %in% c("Cali", "Bogotá", "Medellín", "Colombia"),
    !is.na(sexo), !is.na(edad), !is.na(peso), !is.na(talla), !is.na(factor_exp),
    edad >= 14, peso > 0, talla > 0, factor_exp > 0
  )

# Antropometría SABE mayores con cap1 + cap10
antropo_sabe <- cap1 %>%
  transmute(
    NumIdentificador,
    sexo = case_when(
      P121 %in% c(1, "1") ~ "Masculino",
      P121 %in% c(2, "2") ~ "Femenino",
      TRUE ~ NA_character_
    ),
    edad = suppressWarnings(as.numeric(P122EDAD))
  ) %>%
  left_join(
    cap10 %>%
      transmute(
        NumIdentificador,
        peso  = suppressWarnings(as.numeric(P1005PESO)),
        talla = suppressWarnings(as.numeric(P1006TALLA))
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
    edad >= 60, peso > 0, talla > 0
  )

# ENSIN para ciudades y Colombia < 60, SABE para Colombia 60+
antropo_base <- bind_rows(
  antropo_ensin %>% filter(!(dominio_geo == "Colombia" & edad >= 60)),
  antropo_sabe
)

insumos_antropo <- antropo_base %>%
  dplyr::mutate(grupo_edad = grupo_edad(edad)) %>%
  dplyr::filter(!is.na(grupo_edad)) %>%
  dplyr::group_by(dominio_geo, sexo, grupo_edad) %>%
  dplyr::summarise(
    edad_rep   = w_mean(edad, factor_exp),
    peso_prom  = w_mean(peso, factor_exp),
    talla_prom = w_mean(talla, factor_exp),
    .groups = "drop"
  )

# Actividad física ENSIN
af_ado_limpia <- agregar_dominios_af(af_ado) %>%
  transmute(
    dominio_geo,
    sexo = case_when(
      AFSEXO %in% c(1, "1") ~ "Masculino",
      AFSEXO %in% c(2, "2") ~ "Femenino",
      TRUE ~ NA_character_
    ),
    edad = suppressWarnings(as.numeric(AFEDAD)),
    factor_exp = suppressWarnings(as.numeric(FactorExpansionAF)),
    af_cat = case_when(
      activof %in% c(1, "1") ~ "Moderada",
      activof %in% c(0, "0") ~ "Ligera",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(
    dominio_geo %in% c("Cali", "Bogotá", "Medellín", "Colombia"),
    !is.na(sexo), !is.na(edad), !is.na(factor_exp), !is.na(af_cat),
    edad >= 14, edad < 19, factor_exp > 0
  )

af_adu_limpia <- agregar_dominios_af(af_adu) %>%
  transmute(
    dominio_geo,
    sexo = case_when(
      AFSEXO %in% c(1, "1") ~ "Masculino",
      AFSEXO %in% c(2, "2") ~ "Femenino",
      TRUE ~ NA_character_
    ),
    edad = suppressWarnings(as.numeric(AFEDAD)),
    factor_exp = suppressWarnings(as.numeric(FactorExpansionAF)),
    af_cat = case_when(
      meetcombinatt %in% c(1, "1") ~ "Moderada",
      meetcombinatt %in% c(0, "0") ~ "Ligera",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(
    dominio_geo %in% c("Cali", "Bogotá", "Medellín", "Colombia"),
    !is.na(sexo), !is.na(edad), !is.na(factor_exp), !is.na(af_cat),
    edad >= 19, factor_exp > 0
  )

af_ensin <- bind_rows(af_ado_limpia, af_adu_limpia)

insumos_af_ensin <- af_ensin %>%
  dplyr::mutate(grupo_edad = grupo_edad(edad)) %>%
  dplyr::filter(!is.na(grupo_edad)) %>%
  dplyr::group_by(dominio_geo, sexo, grupo_edad, af_cat) %>%
  dplyr::summarise(freq = sum(factor_exp, na.rm = TRUE), .groups = "drop") %>%
  dplyr::group_by(dominio_geo, sexo, grupo_edad) %>%
  slice_max(order_by = freq, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  transmute(
    dominio_geo,
    sexo,
    grupo_edad,
    AF_predominante = af_cat
  )

# Actividad física SABE para mayores
af_sabe <- cap1 %>%
  transmute(
    NumIdentificador,
    sexo = case_when(
      P121 %in% c(1, "1") ~ "Masculino",
      P121 %in% c(2, "2") ~ "Femenino",
      TRUE ~ NA_character_
    ),
    edad = suppressWarnings(as.numeric(P122EDAD))
  ) %>%
  left_join(
    cap4 %>%
      transmute(
        NumIdentificador,
        P407B_2 = suppressWarnings(as.numeric(P407B_2)),
        P409_11 = suppressWarnings(as.numeric(P409_11))
      ),
    by = "NumIdentificador"
  ) %>%
  left_join(
    cap7 %>%
      transmute(
        NumIdentificador,
        P719 = suppressWarnings(as.numeric(P719)),
        P720 = suppressWarnings(as.numeric(P720)),
        P721 = suppressWarnings(as.numeric(P721))
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
  dplyr::mutate(grupo_edad = grupo_edad(edad)) %>%
  dplyr::filter(!is.na(grupo_edad)) %>%
  dplyr::group_by(dominio_geo, sexo, grupo_edad, af_cat) %>%
  dplyr::summarise(freq = sum(factor_exp, na.rm = TRUE), .groups = "drop") %>%
  dplyr::group_by(dominio_geo, sexo, grupo_edad) %>%
  slice_max(order_by = freq, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  transmute(
    dominio_geo,
    sexo,
    grupo_edad,
    AF_predominante = af_cat
  )

# ENSIN para todo salvo Colombia 70+, SABE para Colombia 70+
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
        age    = ..1,
        height = ..2,
        weight = ..3,
        sexo   = ..4,
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
writexl::write_xlsx(tabla_eer, file.path(output_dir, "270326_adult_eer.xlsx"))

tabla_eer
