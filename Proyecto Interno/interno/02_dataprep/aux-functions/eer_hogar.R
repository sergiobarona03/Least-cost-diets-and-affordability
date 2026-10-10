########################################################
## 02_dataprep/aux-functions/eer_hogar.R
##
## Miembros posibles de un hogar y armado de los insumos de requerimientos
## de energia y nutrientes (EER, minimos y maximos) de un hogar de 4 o 5
## personas, con las mismas tablas que usa 02_eer_3members.R.
##
## Gestante: EER de la mujer no gestante de 31 a 50 anos de la ciudad mas el
## incremento de energia de la gestacion (05-EER-gestantes-mayores.R), y
## minimos y maximos de nutrientes de "gestantes 31 a 50 anos" de FoodpriceR
## (FoodpriceR trae gestantes de < 18, 19 a 30 y 31 a 50 anos, como Yoshioka et al., 2025).
##
## Adolescente: mujer de 14 a 18 anos, con las ecuaciones IOM/NASEM 2023 y la
## antropometria de la ENSIN (02-EER-adultos.R), como el resto de los mayores de 14.
## La nina de 9 a 13 anos viene de RIEN ponderado por el CNPV 2018 (01-EER-ninos.R).
##
## Cada miembro se identifica con (sex, rango_hogar). La gestante lleva el
## rango "[31,51) gest" para distinguirla de la mujer no gestante.
########################################################

# sex, rango_eer: clave del EER por ciudad en 230726_agg_eer.xlsx
# edad_foodpricer: grupo de edad de FoodpriceR::EER_LL y FoodpriceR::UL
MIEMBROS_EER <- tibble::tribble(
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

# Devuelve household_eer, household_eer_ll y household_ul con las mismas
# columnas que los de 02_eer_3members.R.
#   miembros: nombres de MIEMBROS_EER que forman el hogar
#   agg_eer: EER por ciudad, sexo y rango (230726_agg_eer.xlsx)
#   incremento_gestacion: kcal/dia que se suman a la gestante
#   eer_ll_base, ul_base: FoodpriceR::EER_LL y FoodpriceR::UL
#   escalar_macros: FoodpriceR da los minimos y maximos de proteina, lipidos y
#     carbohidratos en gramos, calculados como porcentaje de la energia de referencia
#     de su grupo de edad (por ejemplo 1.916 kcal para una mujer de 31 a 50 anos). Con
#     FALSE (version base) se usan esos gramos tal cual; con TRUE se multiplican por
#     EER del miembro en su ciudad / energia de referencia de FoodpriceR, de modo que
#     conserven el mismo porcentaje de la energia del miembro.
insumos_eer_hogar <- function(miembros, agg_eer, incremento_gestacion, eer_ll_base, ul_base,
                              escalar_macros = FALSE) {

  m <- MIEMBROS_EER[match(miembros, MIEMBROS_EER$miembro), ]
  stopifnot(!anyNA(m$miembro))

  household_eer <- purrr::map_dfr(seq_len(nrow(m)), function(k) {
    agg_eer %>%
      dplyr::filter(sex == m$sex[k], rango == m$rango_eer[k], cod_mun %in% names(CIUDADES_EER)) %>%
      dplyr::mutate(
        ciudad = unname(CIUDADES_EER[cod_mun]),
        eer    = eer + if (m$gestante[k]) incremento_gestacion else 0,
        rango  = m$rango_hogar[k]
      )
  }) %>%
    dplyr::select(cod_mun, sex, rango, eer, ciudad) %>%
    as.data.frame()

  stopifnot(nrow(household_eer) == nrow(m) * length(CIUDADES_EER))

  # Claves de FoodpriceR llevadas al rango del hogar
  claves <- stats::setNames(m$rango_hogar, m$edad_foodpricer)
  limites <- function(base) {
    base %>%
      dplyr::filter(Age %in% names(claves)) %>%
      dplyr::mutate(Age = unname(claves[Age]))
  }

  household_eer_renamed <- household_eer %>%
    dplyr::rename(Age = rango, Sex = sex, Energy = eer) %>%
    dplyr::mutate(Sex = dplyr::if_else(Sex == "Masculino", 0L, 1L))

  household_eer_ll <- merge(
    household_eer_renamed,
    limites(eer_ll_base) %>% dplyr::select(-Energy),
    by = c("Sex", "Age"))

  household_ul <- merge(
    household_eer_renamed,
    limites(ul_base) %>% dplyr::select(-Energy),
    by = c("Sex", "Age"))

  if (escalar_macros) {
    en_ref <- limites(eer_ll_base) %>% dplyr::select(Sex, Age, Energia_ref = Energy)
    macros <- c("Protein", "Lipids", "Carbohydrates")
    escalar <- function(tab) {
      f <- tab %>% dplyr::left_join(en_ref, by = c("Sex", "Age")) %>%
        dplyr::mutate(f = Energy / Energia_ref) %>% dplyr::pull(f)
      stopifnot(!anyNA(f))
      tab[macros] <- tab[macros] * f
      tab
    }
    household_eer_ll <- escalar(household_eer_ll)
    household_ul <- escalar(household_ul)
  }

  household_ul <- household_ul %>%
    dplyr::select(-Energy) %>%
    dplyr::mutate(dplyr::across(where(is.numeric), ~ tidyr::replace_na(.x, 9999999)))

  stopifnot(nrow(household_eer_ll) == nrow(household_eer),
            nrow(household_ul) == nrow(household_eer))

  list(household_eer = household_eer,
       household_eer_ll = household_eer_ll,
       household_ul = household_ul)
}
