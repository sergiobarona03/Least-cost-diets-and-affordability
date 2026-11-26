########################################################
## 02_dataprep/aux-functions/eer_iom.R
##
## Ecuaciones de requerimiento estimado de energia (EER) de las Dietary
## Reference Intakes for Energy (NASEM, 2023, doi:10.17226/26818):
##   - 14 a 18,99 anos: ecuaciones por sexo y nivel de actividad fisica (PAL),
##     con 20 kcal/dia de deposito de energia por crecimiento (tabla de 14 a 18,99 anos).
##   - 19 anos o mas: ecuaciones por sexo y PAL (tabla S-3).
## EER = b0 + b_edad * edad + b_talla * talla + b_peso * peso + b_crec
## con edad en anos, talla en cm y peso en kg. Devuelve kcal/dia.
##
## Los coeficientes estan transcritos en
## "EER act/ECUACIONES ENERGIA ACTUALIZACION.xlsx" (hoja Nasem 2023).
## Embarazo (IOM, 2006): EER de la mujer no gestante + 0, 340 o 452 kcal/dia en el
## primero, segundo y tercer trimestre (hoja IOM 2006 del mismo archivo).
##
## Usado por eer/02-EER-adultos.R (calculo) y eer/06-auditoria-eer.R (verificacion);
## el working paper imprime EER_IOM_COEF.
########################################################

EER_IOM_COEF <- tibble::tribble(
  ~edad_grupo,  ~sexo,       ~pal,          ~b0,      ~b_edad, ~b_talla, ~b_peso, ~b_crec,
  "14 a 18",    "Masculino", "Inactive",    -447.51,   3.68,    13.01,    13.15,   20,
  "14 a 18",    "Masculino", "Low active",    19.12,   3.68,     8.62,    20.28,   20,
  "14 a 18",    "Masculino", "Active",      -388.19,   3.68,    12.66,    20.46,   20,
  "14 a 18",    "Masculino", "Very active", -671.75,   3.68,    15.38,    23.25,   20,
  "14 a 18",    "Femenino",  "Inactive",      55.59, -22.25,     8.43,    17.07,   20,
  "14 a 18",    "Femenino",  "Low active",  -297.54, -22.25,    12.77,    14.73,   20,
  "14 a 18",    "Femenino",  "Active",      -189.55, -22.25,    11.74,    18.34,   20,
  "14 a 18",    "Femenino",  "Very active", -709.59, -22.25,    18.22,    14.25,   20,
  "19 o más", "Masculino", "Inactive",     753.07, -10.83,     6.50,    14.10,    0,
  "19 o más", "Masculino", "Low active",   581.47, -10.83,     8.30,    14.94,    0,
  "19 o más", "Masculino", "Active",      1004.82, -10.83,     6.52,    15.91,    0,
  "19 o más", "Masculino", "Very active", -517.88, -10.83,    15.61,    19.11,    0,
  "19 o más", "Femenino",  "Inactive",     584.90,  -7.01,     5.72,    11.71,    0,
  "19 o más", "Femenino",  "Low active",   575.77,  -7.01,     6.60,    12.14,    0,
  "19 o más", "Femenino",  "Active",       710.25,  -7.01,     6.54,    12.34,    0,
  "19 o más", "Femenino",  "Very active",  511.83,  -7.01,     9.07,    12.56,    0
)

# Actividad fisica de la ENSIN a categoria PAL de NASEM
map_pal_iom <- function(AF_predominante) {
  dplyr::case_when(
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

  grupo <- if (age >= 14 && age < 19) "14 a 18" else if (age >= 19) "19 o más" else return(NA_real_)

  k <- EER_IOM_COEF[EER_IOM_COEF$edad_grupo == grupo & EER_IOM_COEF$sexo == sexo & EER_IOM_COEF$pal == pal_cat, ]
  if (nrow(k) != 1) return(NA_real_)

  k$b0 + k$b_edad * age + k$b_talla * height + k$b_peso * weight + k$b_crec
}
