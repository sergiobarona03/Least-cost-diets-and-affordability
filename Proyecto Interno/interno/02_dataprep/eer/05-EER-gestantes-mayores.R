########################################################
## SCRIPT 02_dataprep/eer/05-EER-gestantes-mayores.R
## Informacion antropometrica de las mujeres gestantes (ENSIN 2015) y
## de las personas mayores (ENSIN y SABE), y el requerimiento de energia
## adicional de la gestacion.
##
## Gestantes: la ENSIN trae el trimestre de embarazo de cada mujer. El EER
## de una gestante es el de una mujer no gestante de su edad mas un
## incremento por trimestre (IOM 2005; NASEM 2023): 0 kcal/dia en el
## primero, 340 en el segundo y 452 en el tercero. El incremento que se usa
## es el promedio de esos valores ponderado por la distribucion de
## trimestres de las gestantes de la ENSIN.
## La muestra de gestantes por ciudad es pequena, asi que la distribucion
## de trimestres es nacional.
##
## Mayores: se toman de la tabla de adultos (02-EER-adultos.R), que ya
## combina ENSIN (hasta 64 anos) y SABE (60 anos o mas).
##
## Reads:  requirements_dir/ANTROPOMETRIA.rds
##         output_dir/230726_adult_eer.xlsx
## Writes: output_dir/041026_gestantes_mayores.xlsx
##         output_dir/041026_gestantes_incremento.rds
########################################################

library(tidyverse)
library(readxl)
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

# Incremento de energia por trimestre de embarazo (kcal/dia)
INCREMENTO_TRIMESTRE <- c(`1` = 0, `2` = 340, `3` = 452)

to_num <- function(x) suppressWarnings(as.numeric(haven::zap_labels(x)))

# -----------------------------------------------------------------------
# Gestantes
# -----------------------------------------------------------------------

antro <- readRDS(file.path(requirements_dir, "ANTROPOMETRIA.rds"))

gestantes <- antro %>%
  transmute(
    trimestre   = to_num(trimestreEmbarazo),
    semanas     = to_num(semanasGest),
    edad        = to_num(AN_EDAD),
    peso        = to_num(AN_7),
    talla       = to_num(AN_8),
    estado_peso = as.character(haven::as_factor(estadoPesoGestantes)),
    factor_exp  = to_num(FactorExpansionPer)
  ) %>%
  filter(trimestre %in% 1:3, !is.na(factor_exp), factor_exp > 0) %>%
  mutate(imc = if_else(peso > 0 & talla > 0, peso / (talla / 100)^2, NA_real_))

w_mean <- function(x, w) {
  ok <- !is.na(x) & !is.na(w)
  if (!any(ok)) return(NA_real_)
  weighted.mean(x[ok], w[ok])
}

gestantes_trimestre <- gestantes %>%
  group_by(trimestre) %>%
  summarise(
    mujeres      = n(),
    poblacion    = sum(factor_exp),
    edad_prom    = w_mean(edad, factor_exp),
    semanas_prom = w_mean(semanas, factor_exp),
    peso_prom    = w_mean(peso, factor_exp),
    talla_prom   = w_mean(talla, factor_exp),
    imc_prom     = w_mean(imc, factor_exp),
    .groups = "drop"
  ) %>%
  mutate(
    participacion = poblacion / sum(poblacion),
    incremento_kcal = INCREMENTO_TRIMESTRE[as.character(trimestre)]
  )

incremento_gestacion <- sum(gestantes_trimestre$participacion * gestantes_trimestre$incremento_kcal)

gestantes_estado_peso <- gestantes %>%
  filter(!is.na(estado_peso)) %>%
  group_by(estado_peso) %>%
  summarise(mujeres = n(), poblacion = sum(factor_exp), .groups = "drop") %>%
  mutate(participacion = poblacion / sum(poblacion))

message(sprintf("  %d gestantes | incremento ponderado = %.0f kcal/dia",
                nrow(gestantes), incremento_gestacion))

# -----------------------------------------------------------------------
# Personas mayores
# -----------------------------------------------------------------------

mayores <- read_excel(file.path(output_dir, "230726_adult_eer.xlsx")) %>%
  filter(grupo_edad %in% c("[51,70)", "[70,Inf)")) %>%
  arrange(grupo_edad, sexo, dominio_geo)

# -----------------------------------------------------------------------
# Save
# -----------------------------------------------------------------------

write_xlsx(
  list(
    gestantes_trimestre   = gestantes_trimestre,
    gestantes_estado_peso = gestantes_estado_peso,
    gestantes_incremento  = tibble(incremento_kcal_dia = incremento_gestacion),
    mayores               = mayores
  ),
  file.path(output_dir, "041026_gestantes_mayores.xlsx")
)

saveRDS(list(incremento = incremento_gestacion,
             por_trimestre = gestantes_trimestre),
        file.path(output_dir, "041026_gestantes_incremento.rds"))

message("Done.")
