# =========================================
# Ecuaciones IOM / NASEM 2023 para EER
# =========================================

library(dplyr)
library(tidyr)
library(tidyverse)

# Ecuación en rmd:
# age   = edad en años
# height = talla/altura en cm
# weight = peso en kg
# sexo  = "Masculino" o "Femenino"
# pal_cat = "Inactive", "Low active", "Active", "Very active"

# Convertir categorías de AF a PAL de IOM
map_pal_iom <- function(af_predominante) {
  dplyr::case_when(
    af_predominante == "Ligera"   ~ "Low active",
    af_predominante == "Moderada" ~ "Active",
    af_predominante == "Inactiva" ~ "Inactive",
    af_predominante == "Alta"     ~ "Very active",
    TRUE ~ NA_character_
  )
}

eer_iom_2023 <- function(age, height, weight, sexo, pal_cat) {
  
  if (any(is.na(c(age, height, weight))) || is.na(sexo) || is.na(pal_cat)) {
    return(NA_real_)
  }
  
  # Adolescentes: 14 a <19 años
  if (age >= 14 && age < 19) {
    
    if (sexo == "Masculino" && pal_cat == "Inactive") {
      return(-447.51 + 3.68 * age + 13.01 * height + 13.15 * weight + 20)
    }
    if (sexo == "Masculino" && pal_cat == "Low active") {
      return(19.12 + 3.68 * age + 8.62 * height + 20.28 * weight + 20)
    }
    if (sexo == "Masculino" && pal_cat == "Active") {
      return(-388.19 + 3.68 * age + 12.66 * height + 20.46 * weight + 20)
    }
    if (sexo == "Masculino" && pal_cat == "Very active") {
      return(-671.75 + 3.68 * age + 15.38 * height + 23.25 * weight + 20)
    }
    
    if (sexo == "Femenino" && pal_cat == "Inactive") {
      return(55.59 - 22.25 * age + 8.43 * height + 17.07 * weight + 20)
    }
    if (sexo == "Femenino" && pal_cat == "Low active") {
      return(-297.54 - 22.25 * age + 12.77 * height + 14.73 * weight + 20)
    }
    if (sexo == "Femenino" && pal_cat == "Active") {
      return(-189.55 - 22.25 * age + 11.74 * height + 18.34 * weight + 20)
    }
    if (sexo == "Femenino" && pal_cat == "Very active") {
      return(-709.59 - 22.25 * age + 18.22 * height + 14.25 * weight + 20)
    }
  }
  
  # Adultos: 19+ años
  if (age >= 19) {
    
    if (sexo == "Masculino" && pal_cat == "Inactive") {
      return(753.07 - 10.83 * age + 6.50 * height + 14.10 * weight)
    }
    if (sexo == "Masculino" && pal_cat == "Low active") {
      return(581.47 - 10.83 * age + 8.30 * height + 14.94 * weight)
    }
    if (sexo == "Masculino" && pal_cat == "Active") {
      return(1004.82 - 10.83 * age + 6.52 * height + 15.91 * weight)
    }
    if (sexo == "Masculino" && pal_cat == "Very active") {
      return(-517.88 - 10.83 * age + 15.61 * height + 19.11 * weight)
    }
    
    if (sexo == "Femenino" && pal_cat == "Inactive") {
      return(584.90 - 7.01 * age + 5.72 * height + 11.71 * weight)
    }
    if (sexo == "Femenino" && pal_cat == "Low active") {
      return(575.77 - 7.01 * age + 6.60 * height + 12.14 * weight)
    }
    if (sexo == "Femenino" && pal_cat == "Active") {
      return(710.25 - 7.01 * age + 6.54 * height + 12.34 * weight)
    }
    if (sexo == "Femenino" && pal_cat == "Very active") {
      return(511.83 - 7.01 * age + 9.07 * height + 12.56 * weight)
    }
  }
  
  return(NA_real_)
}