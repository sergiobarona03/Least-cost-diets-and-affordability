########################################################
## SCRIPT 03_models/09_sensibilidad_requerimientos.R
## Junta los resultados de las dos pruebas de sensibilidad de los
## requerimientos de los hogares:
##
##   esc  Los minimos y maximos de proteina, lipidos y carbohidratos de FoodpriceR
##        estan en gramos, calculados con la energia de referencia de su grupo de edad;
##        con "esc" se escalan a la energia de cada miembro en su ciudad, de modo que
##        conserven el mismo porcentaje de su energia. Solo afecta al CoNA y al CC-CoNA.
##   t3   El hogar de 4 con el incremento de energia del tercer trimestre de la gestacion
##        (452 kcal/dia, como Yoshioka et al., 2025) en lugar del promedio ponderado por
##        el trimestre de las gestantes de la ENSIN (311 kcal/dia). Afecta a las cinco dietas.
##
## Se corren antes, con la variable de entorno VARIANTE_HOGAR (y N_MIEMBROS) y los
## scripts de 02_dataprep y 03_models: 02_eer_*members.R y 02_cona.R, 04_cc_cona.R (esc);
## 02_eer_4members.R, 03_gaba_ajustado.R y los cinco modelos (t3).
##
## Reads:  coca_dir, cona_dir, cord_dir, coahd_dir, ccona_dir (resultados con sufijo)
## Writes: requerimientos_dir/requerimientos_results.rds
##         requerimientos_dir/requerimientos_results.xlsx
########################################################

library(tidyverse)
library(writexl)

dirs <- c(
  "C:/Users/Portatil/Desktop/Least-cost-diets-and-affordability/Proyecto Interno",
  "C:/Users/danie/OneDrive/Escritorio/Least-cost-diets-and-affordability/Proyecto Interno"
)

base_dir <- dirs[dir.exists(dirs)][1]

if (is.na(base_dir)) {
  stop("Ninguno de los directorios existe")
}

models_dir <- file.path(base_dir, "interno/03_models")
req_dir    <- file.path(models_dir, "requerimientos")
dir.create(req_dir, recursive = TRUE, showWarnings = FALSE)

source(file.path(models_dir, "aux-functions/alfa_referencia.R"))

# -----------------------------------------------------------------------
# Lectura de un modelo de un hogar y una variante
# -----------------------------------------------------------------------

sufijo <- function(hogar, variante) {
  paste0(if (hogar == 3L) "" else paste0("_", hogar, "m"), if (nzchar(variante)) paste0("_", variante))
}

leer <- function(modelo, hogar, variante) {
  s <- sufijo(hogar, variante)
  f <- function(carpeta, nombre) file.path(models_dir, carpeta, paste0(nombre, "_results", s, ".rds"))
  switch(modelo,
    CoCA = readRDS(f("coca", "coca")) %>% distinct(ciudad, fecha, Demo_Group, Sex, costo = cost_day),
    CoNA = readRDS(f("cona", "cona"))$cost %>% select(ciudad, fecha, Demo_Group, Sex, costo = cona_cost),
    "CC-CoNA" = readRDS(f("ccona", "ccona"))$cost %>% filter(alpha_val == ALFA_REFERENCIA) %>%
      select(ciudad, fecha, Demo_Group, Sex, costo = cost_day),
    CoRD = readRDS(f("cord", "cord"))$cost %>% select(ciudad, fecha, Demo_Group, Sex, costo = cost_day),
    CoAHD = readRDS(f("coahd", "coahd"))$cost %>% select(ciudad, fecha, Demo_Group, Sex, costo = cost_day)
  ) %>% mutate(modelo = modelo, hogar = hogar, variante = if (nzchar(variante)) variante else "base",
               fecha = as.Date(fecha))
}

# -----------------------------------------------------------------------
# Costos por miembro, ciudad y mes de cada combinacion
# -----------------------------------------------------------------------

combinaciones <- bind_rows(
  expand_grid(hogar = 3:5, variante = c("", "esc"), modelo = c("CoNA", "CC-CoNA")),
  expand_grid(hogar = 4L, variante = c("", "t3"), modelo = c("CoCA", "CoNA", "CC-CoNA", "CoRD", "CoAHD"))
) %>% distinct()

costos <- pmap_dfr(combinaciones, function(hogar, variante, modelo) leer(modelo, hogar, variante))

# -----------------------------------------------------------------------
# Restricciones de macronutrientes en el CoNA: cuanto pesan y con que frecuencia ligan
# -----------------------------------------------------------------------

spe <- map_dfr(c("", "esc"), function(v) {
  x <- readRDS(file.path(models_dir, "cona", paste0("cona_results", sufijo(3L, v), ".rds")))$spe
  x %>% mutate(variante = if (nzchar(v)) v else "base")
}) %>%
  group_by(variante, Nutrients, constraint) %>%
  summarise(activa = mean(abs(SPE) > 1e-9), spe = mean(SPE), .groups = "drop")

salida <- list(costos = costos, spe_hogar3 = spe)
saveRDS(salida, file.path(req_dir, "requerimientos_results.rds"))
write_xlsx(salida, file.path(req_dir, "requerimientos_results.xlsx"))

message("Done. ", nrow(costos), " filas de costos en ", req_dir)
