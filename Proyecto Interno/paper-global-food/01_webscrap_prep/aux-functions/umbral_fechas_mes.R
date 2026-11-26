########################################################
## FUNCION lista_por_umbral_mensual()
## 01_webscrap_prep/aux-functions/umbral_fechas_mes.R
##
## Decide, por ciudad, que alimentos quedan en el panel segun su
## cobertura de fechas.
##
## Un alimento queda si tiene precio en al menos `min_fechas` fechas
## de cada mes con datos de la ciudad.
##
## Parametros
##   panel: city, sipsa_name, fecha (una fila por alimento-ciudad-fecha)
##
## Devuelve: city, sipsa_name, n_fechas, fechas_mes_min (solo los
##   alimentos que cumplen)
########################################################

lista_por_umbral_mensual <- function(panel, min_fechas = 2) {

  meses_ciudad <- panel %>%
    dplyr::mutate(mes = format(fecha, "%Y-%m")) %>%
    dplyr::distinct(city, mes)

  panel %>%
    dplyr::mutate(mes = format(fecha, "%Y-%m")) %>%
    dplyr::distinct(city, sipsa_name, fecha, mes) %>%
    dplyr::count(city, sipsa_name, mes) %>%
    dplyr::full_join(
      dplyr::distinct(panel, city, sipsa_name) %>% dplyr::inner_join(meses_ciudad, by = "city", relationship = "many-to-many"),
      by = c("city", "sipsa_name", "mes")
    ) %>%
    dplyr::mutate(n = dplyr::coalesce(n, 0L)) %>%
    dplyr::group_by(city, sipsa_name) %>%
    dplyr::summarise(n_fechas = sum(n), fechas_mes_min = min(n), .groups = "drop") %>%
    dplyr::filter(fechas_mes_min >= min_fechas)
}
