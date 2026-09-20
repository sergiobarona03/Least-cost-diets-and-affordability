########################################################
## 03_models/aux-functions/gaba_insumos.R
##
## Grupos GABA, numero de alimentos por grupo y armado de los
## insumos que comparten 03_cord.R y 031_coahd.R.
########################################################

GRUPOS_GABA <- c(
  "Cereales, raíces, tubérculos y plátanos",
  "Frutas",
  "Verduras",
  "Leche y productos lácteos",
  "Carnes, huevos, leguminosas, frutos secos y semillas",
  "Grasas",
  "Azúcares"
)

# Alimentos a elegir por grupo (Herforth et al.), en el orden de GRUPOS_GABA
DIVERSIDAD_GABA <- tibble::tibble(
  Group  = GRUPOS_GABA,
  Number = c(3, 2, 2, 1, 2, 1, 1)
)

# Ajo y ají quedan fuera de CoRD y CoAHD: Herforth et al. (2024), tabla
# suplementaria 2, los listan como "Excluded".
EXCLUIDOS_GABA <- c("Ajo", "Ajo importado", "Ají topito dulce")

# El panel y gaba_exchanges_adj escriben distinto los mismos grupos
MAPA_GRUPOS_GABA <- c(
  "AZUCARES"                                                    = "Azúcares",
  "AZÚCARES"                                                    = "Azúcares",
  "CARNES, HUEVOS, LEGUMINOSAS SECAS, FRUTOS SECOS Y SEMILLAS"  = "Carnes, huevos, leguminosas, frutos secos y semillas",
  "CARNES, HUEVOS, LEGUMINOSAS, FRUTOS SECOS Y SEMILLAS"        = "Carnes, huevos, leguminosas, frutos secos y semillas",
  "CEREALES, RAÍCES, TUBÉRCULOS Y PLÁTANOS"                     = "Cereales, raíces, tubérculos y plátanos",
  "FRUTAS"                                                      = "Frutas",
  "GRASAS"                                                      = "Grasas",
  "LECHE Y PRODUCTOS LACTEOS"                                   = "Leche y productos lácteos",
  "LECHE Y PRODUCTOS LÁCTEOS"                                   = "Leche y productos lácteos",
  "VERDURAS"                                                    = "Verduras"
)

# Alimentos del panel con su grupo GABA y los gramos de un intercambio.
# El panel no trae esos gramos (el TCAC los llama gramos_g_1_intercambio).
alimentos_gaba <- function(panel, tcac) {
  gramos <- tcac %>%
    dplyr::transmute(articulo = sipsa_name, Serving_g = gramos_g_1_intercambio) %>%
    dplyr::distinct(articulo, .keep_all = TRUE)

  panel %>%
    dplyr::mutate(
      Group = dplyr::if_else(subgrupos_gabas %in% c("FRUTAS", "VERDURAS"),
                             subgrupos_gabas, grupos_gabas),
      Group = dplyr::recode(Group, !!!MAPA_GRUPOS_GABA)
    ) %>%
    dplyr::filter(Group %in% GRUPOS_GABA) %>%
    dplyr::left_join(gramos, by = "articulo")
}

# Requerimientos GABA del hogar representativo: hombre y mujer de
# 31-50 y niña de 10-13 (los mismos miembros de CoCA y CoNA).
requerimientos_gaba <- function(gaba_exchanges_adj) {
  gaba_exchanges_adj %>%
    dplyr::filter(
      (sex == "Masculino" & rango == "[31,51)") |
        (sex == "Femenino" & rango %in% c("[31,51)", "[10, 14)"))
    ) %>%
    dplyr::transmute(
      ciudad,
      Age     = rango,
      Sex     = dplyr::if_else(sex == "Masculino", 0L, 1L),
      Group   = dplyr::recode(grupo_principal, !!!MAPA_GRUPOS_GABA),
      Serving = n_exchanges_adj,
      Kcal    = e_kcal_adj
    ) %>%
    dplyr::filter(Group %in% GRUPOS_GABA)
}
