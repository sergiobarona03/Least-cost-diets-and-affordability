########################################################
## SCRIPT 02_dataprep/02_requerimientos_hogares.R
##
## Prepara los requerimientos de energia (EER) y los limites de
## nutrientes de los tres hogares y de sus variantes, en una sola
## corrida. Reune lo que antes hacian 02_eer_3members.R,
## 02_eer_4members.R, 02_eer_5members.R y aux-functions/eer_hogar.R,
## sin cambiar el calculo ni los nombres de los archivos de salida.
##
## Hogares
##   3 miembros: hombre adulto 31-50, mujer adulta 31-50, nina 9-13
##   4 miembros: hombre adulto 31-50, mujer gestante 31-50,
##               adolescente mujer 14-18, nina 9-13
##   5 miembros: hombre adulto 31-50, mujer adulta 31-50,
##               adolescente mujer 14-18, nina 9-13, mujer 51-70
##
## Variantes (pruebas de sensibilidad)
##   base: minimos y maximos de macronutrientes de FoodpriceR en gramos
##   esc:  los gramos de proteina, lipidos y carbohidratos se escalan a
##         la energia de cada miembro
##   t3:   solo hogar de 4; incremento de la gestacion del tercer
##         trimestre (452 kcal/dia), como en Yoshioka et al. (2025)
##
## Nina 9-13: el EER se promedia sobre las edades de 10 a 13 anos (RIEN
## ponderado por el CNPV 2018); los limites de nutrientes son los de 9 a
## 13 anos de FoodpriceR.
## Gestante: EER de la mujer no gestante de 31 a 50 anos de la ciudad mas
## el incremento de la gestacion; limites de "gestantes 31 a 50 anos".
##
## Reads:  eer_output_dir/230726_agg_eer.xlsx
##         eer_output_dir/041026_gestantes_incremento.rds
##         FoodpriceR::EER_LL, FoodpriceR::UL
## Writes: household_dir/household_eer<sufijo>.rds
##         household_dir/household_eer_ll<sufijo>.rds
##         household_dir/household_ul<sufijo>.rds
##   con sufijo "", "_esc", "_4m", "_4m_t3", "_4m_esc", "_5m", "_5m_esc"
########################################################

library(tidyverse)
library(readxl)
library(FoodpriceR)

# ============================================================
# Rutas
# ============================================================

base_dir <- "C:/Users/Portatil/Desktop/Least-cost-diets-and-affordability/Proyecto Interno/paper-global-food"

eer_output_dir <- file.path(base_dir, "02_dataprep/eer/output")
household_dir  <- file.path(base_dir, "02_dataprep/household eer")

dir.create(household_dir, recursive = TRUE, showWarnings = FALSE)

# ============================================================
# Miembros posibles y ciudades
# ============================================================
# sex, rango_eer: clave del EER por ciudad en 230726_agg_eer.xlsx
# edad_foodpricer: grupo de edad de FoodpriceR::EER_LL y FoodpriceR::UL
# La gestante lleva el rango "[31,51) gest" para distinguirla de la
# mujer no gestante.

MIEMBROS_EER <- tribble(
  ~miembro,                       ~sex,        ~rango_eer, ~rango_hogar,   ~edad_foodpricer,         ~gestante,
  "Hombre adulto (31-50)",        "Masculino", "[31,51)",  "[31,51)",      "31 a 50 años",           FALSE,
  "Mujer adulta (31-50)",         "Femenino",  "[31,51)",  "[31,51)",      "31 a 50 años",           FALSE,
  "Mujer gestante (31-50)",       "Femenino",  "[31,51)",  "[31,51) gest", "gestantes 31 a 50 años", TRUE,
  "Niña (9-13)",                  "Femenino",  "[10, 14)", "[10, 14)",     "9 a 13 años",            FALSE,
  "Adolescente mujer (14-18)",    "Femenino",  "[14,19)",  "[14,19)",      "14 a 18 años",           FALSE,
  "Mujer adulta mayor (51-70)",   "Femenino",  "[51,70)",  "[51,70)",      "51 a 70 años",           FALSE
)

CIUDADES_EER <- c(
  "08001" = "BARRANQUILLA", "11001" = "BOGOTA", "68001" = "BUCARAMANGA",
  "76001" = "CALI", "13001" = "CARTAGENA", "54001" = "CUCUTA",
  "73001" = "IBAGUE", "17001" = "MANIZALES", "05001" = "MEDELLIN",
  "23001" = "MONTERIA", "52001" = "PASTO", "66001" = "PEREIRA",
  "50001" = "VILLAVICENCIO"
)

HOGARES <- list(
  "3" = c("Hombre adulto (31-50)", "Mujer adulta (31-50)", "Niña (9-13)"),
  "4" = c("Hombre adulto (31-50)", "Mujer gestante (31-50)",
          "Adolescente mujer (14-18)", "Niña (9-13)"),
  "5" = c("Hombre adulto (31-50)", "Mujer adulta (31-50)",
          "Adolescente mujer (14-18)", "Niña (9-13)", "Mujer adulta mayor (51-70)")
)

# Combinaciones de hogar y variante que se generan
CORRIDAS <- tribble(
  ~n_miembros, ~variante,
  "3",         "",
  "3",         "esc",
  "4",         "",
  "4",         "t3",
  "4",         "esc",
  "5",         "",
  "5",         "esc"
) %>%
  mutate(sufijo = paste0(if_else(n_miembros == "3", "", paste0("_", n_miembros, "m")),
                         if_else(nzchar(variante), paste0("_", variante), "")))

# ============================================================
# Funcion: EER, minimos y maximos de un hogar
# ============================================================
# escalar_macros: FoodpriceR da los minimos y maximos de proteina,
# lipidos y carbohidratos en gramos, calculados como porcentaje de la
# energia de referencia de su grupo de edad (por ejemplo, 1.916 kcal
# para una mujer de 31 a 50 anos). Con FALSE se usan esos gramos tal
# cual; con TRUE se multiplican por EER del miembro / energia de
# referencia, de modo que conserven el mismo porcentaje de su energia.

insumos_eer_hogar <- function(miembros, agg_eer, incremento_gestacion, eer_ll_base, ul_base,
                              escalar_macros = FALSE) {

  m <- MIEMBROS_EER[match(miembros, MIEMBROS_EER$miembro), ]
  stopifnot(!anyNA(m$miembro))

  household_eer <- map_dfr(seq_len(nrow(m)), function(k) {
    agg_eer %>%
      filter(sex == m$sex[k], rango == m$rango_eer[k], cod_mun %in% names(CIUDADES_EER)) %>%
      mutate(
        ciudad = unname(CIUDADES_EER[cod_mun]),
        eer    = eer + if (m$gestante[k]) incremento_gestacion else 0,
        rango  = m$rango_hogar[k]
      )
  }) %>%
    select(cod_mun, sex, rango, eer, ciudad) %>%
    as.data.frame()

  stopifnot(nrow(household_eer) == nrow(m) * length(CIUDADES_EER))

  # Claves de FoodpriceR llevadas al rango del hogar
  claves <- setNames(m$rango_hogar, m$edad_foodpricer)
  limites <- function(base) {
    base %>%
      filter(Age %in% names(claves)) %>%
      mutate(Age = unname(claves[Age]))
  }

  household_eer_renamed <- household_eer %>%
    rename(Age = rango, Sex = sex, Energy = eer) %>%
    mutate(Sex = if_else(Sex == "Masculino", 0L, 1L))

  household_eer_ll <- merge(household_eer_renamed,
                            limites(eer_ll_base) %>% select(-Energy),
                            by = c("Sex", "Age"))

  household_ul <- merge(household_eer_renamed,
                        limites(ul_base) %>% select(-Energy),
                        by = c("Sex", "Age"))

  if (escalar_macros) {
    en_ref <- limites(eer_ll_base) %>% select(Sex, Age, Energia_ref = Energy)
    macros <- c("Protein", "Lipids", "Carbohydrates")
    escalar <- function(tab) {
      f <- tab %>% left_join(en_ref, by = c("Sex", "Age")) %>%
        mutate(f = Energy / Energia_ref) %>% pull(f)
      stopifnot(!anyNA(f))
      tab[macros] <- tab[macros] * f
      tab
    }
    household_eer_ll <- escalar(household_eer_ll)
    household_ul <- escalar(household_ul)
  }

  household_ul <- household_ul %>%
    select(-Energy) %>%
    mutate(across(where(is.numeric), ~ replace_na(.x, 9999999)))

  stopifnot(nrow(household_eer_ll) == nrow(household_eer),
            nrow(household_ul) == nrow(household_eer))

  list(household_eer = household_eer,
       household_eer_ll = household_eer_ll,
       household_ul = household_ul)
}

# ============================================================
# Insumos
# ============================================================

agg_eer <- read_excel(file.path(eer_output_dir, "230726_agg_eer.xlsx")) %>%
  mutate(cod_mun = as.character(cod_mun))

gest <- readRDS(file.path(eer_output_dir, "041026_gestantes_incremento.rds"))

# ============================================================
# Generar todas las combinaciones
# ============================================================

for (k in seq_len(nrow(CORRIDAS))) {

  n_m      <- CORRIDAS$n_miembros[k]
  variante <- CORRIDAS$variante[k]
  sufijo   <- CORRIDAS$sufijo[k]

  # Incremento de la gestacion: solo el hogar de 4 tiene gestante
  incremento <- case_when(
    n_m != "4"       ~ 0,
    variante == "t3" ~ 452,
    TRUE             ~ gest$incremento
  )

  insumos <- insumos_eer_hogar(HOGARES[[n_m]], agg_eer, incremento,
                               eer_ll_base    = FoodpriceR::EER_LL,
                               ul_base        = FoodpriceR::UL,
                               escalar_macros = variante == "esc")

  saveRDS(insumos$household_eer,    file.path(household_dir, paste0("household_eer", sufijo, ".rds")))
  saveRDS(insumos$household_eer_ll, file.path(household_dir, paste0("household_eer_ll", sufijo, ".rds")))
  saveRDS(insumos$household_ul,     file.path(household_dir, paste0("household_ul", sufijo, ".rds")))

  message(sprintf("  hogar de %s%s: %d miembros x %d ciudades | incremento de gestacion: %.0f kcal/dia",
                  n_m, if (nzchar(variante)) paste0(" (", variante, ")") else "",
                  length(HOGARES[[n_m]]), n_distinct(insumos$household_eer$ciudad), incremento))
}

message("Done. Archivos guardados en ", household_dir)
