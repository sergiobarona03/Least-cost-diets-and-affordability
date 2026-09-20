########################################################
## FUNCION lista_por_umbral_grupo()
## 01_webscrap_prep/aux-functions/umbral_fechas_grupo.R
##
## Decide, por ciudad, que alimentos quedan en el panel segun su
## cobertura de fechas.
##
## Un alimento queda si aparece al menos `cobertura` (85%) de los
## dias en que su GRUPO tiene datos en esa ciudad. Los alimentos sin
## grupo se comparan contra todos los dias de la ciudad.
## Contra los dias de la ciudad, las frutas de Ibague (de temporada)
## quedaban todas por debajo del umbral.
##
## Parametros
##   panel:  city, sipsa_name, fecha (una fila por alimento-ciudad-fecha)
##   grupos: sipsa_name, grupo (NA = sin grupo)
##
## Devuelve: city, sipsa_name, grupo, n_fechas, fechas_disponibles,
##   dias_grupo, umbral (solo los alimentos que cumplen)
########################################################

lista_por_umbral_grupo <- function(panel, grupos, cobertura = 0.85) {

  fechas_ciudad <- panel %>%
    dplyr::group_by(city) %>%
    dplyr::summarise(fechas_disponibles = dplyr::n_distinct(fecha), .groups = "drop")

  dias_grupo <- panel %>%
    dplyr::left_join(grupos, by = "sipsa_name") %>%
    dplyr::filter(!is.na(grupo)) %>%
    dplyr::group_by(city, grupo) %>%
    dplyr::summarise(dias_grupo = dplyr::n_distinct(fecha), .groups = "drop")

  panel %>%
    dplyr::group_by(city, sipsa_name) %>%
    dplyr::summarise(n_fechas = dplyr::n_distinct(fecha), .groups = "drop") %>%
    dplyr::left_join(fechas_ciudad, by = "city") %>%
    dplyr::left_join(grupos, by = "sipsa_name") %>%
    dplyr::left_join(dias_grupo, by = c("city", "grupo")) %>%
    dplyr::mutate(umbral = floor(cobertura * dplyr::coalesce(dias_grupo, fechas_disponibles))) %>%
    dplyr::filter(n_fechas >= umbral) %>%
    dplyr::select(city, sipsa_name, grupo, n_fechas, fechas_disponibles, dias_grupo, umbral)
}
