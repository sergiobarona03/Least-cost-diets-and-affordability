########################################################
## FUNCION seleccionar_sku_mediano()
## 01_webscrap_prep/aux-functions/seleccionar_sku_mediano.R
##
## Elige, por alimento-ciudad, el sku representativo que se usa
## para estandarizar el precio (en 01_construccion_panel.R).
##
## Regla:
##  1. Compiten los skus con al menos `min_fechas_mes` fechas con
##     precio en cada mes del panel. Si ninguno lo cumple, compiten
##     todos.
##  2. Se descartan los skus cuyo precio por gramo queda fuera de
##     [Q1 - 1.5 IQR, Q3 + 1.5 IQR], con los cuartiles de todos los
##     skus del alimento en todas las ciudades. Solo se aplica si el
##     alimento tiene al menos `min_skus_atipicos` skus; si todos los
##     de una ciudad son atipicos, se conservan.
##  3. Entre los que quedan, los que no pasan del umbral de tamano:
##     `umbral_gramos` para los solidos y `umbral_ml` para los liquidos
##     (el tamano de un liquido es su contenido en ml); si ninguno lo
##     cumple, todos. Con un umbral en NULL no se filtra ese tipo.
##  4. Se elige el sku con el precio por gramo mas cercano a la
##     mediana; en un empate, el de menor precio.
##  Los alimentos-ciudad sin ningun sku con medida en el nombre (se
##  venden por unidad, kg o g) no se filtran: se devuelven todos sus
##  skus para que 01_construccion_panel.R los resuelva.
##
## Parametros
##   datos: una fila por sku (alimento-ciudad-sku_code) con sipsa_name,
##          city, gramos_sku (NA sin medida), densidad (1 en los
##          solidos), objetivo_base (500 o 1000), precio_gramo_sku y
##          fechas_mes_min (menor numero de fechas del sku en un mes)
##
## Devuelve: el sku ganador de cada alimento-ciudad y los skus de los
##   alimentos sin medida, con n_candidatos y n_atipicos
########################################################

seleccionar_sku_mediano <- function(datos, umbral_gramos = 500, umbral_ml = 1000,
                                    min_fechas_mes = 2, min_skus_atipicos = 4) {

  con_medida_flag <- datos %>%
    dplyr::group_by(sipsa_name, city) %>%
    dplyr::mutate(tiene_medida = any(!is.na(gramos_sku))) %>%
    dplyr::ungroup()

  sin_medida <- con_medida_flag %>%
    dplyr::filter(!tiene_medida) %>%
    dplyr::select(-tiene_medida)

  candidatos <- con_medida_flag %>%
    dplyr::filter(tiene_medida, !is.na(gramos_sku), !is.na(precio_gramo_sku)) %>%
    dplyr::select(-tiene_medida) %>%
    dplyr::group_by(sipsa_name, city) %>%
    dplyr::filter(fechas_mes_min >= min_fechas_mes | !any(fechas_mes_min >= min_fechas_mes)) %>%
    dplyr::ungroup()

  candidatos <- candidatos %>%
    dplyr::group_by(sipsa_name) %>%
    dplyr::mutate(
      q1 = stats::quantile(precio_gramo_sku, 0.25),
      q3 = stats::quantile(precio_gramo_sku, 0.75),
      atipico = dplyr::n() >= min_skus_atipicos &
        (precio_gramo_sku < q1 - 1.5 * (q3 - q1) | precio_gramo_sku > q3 + 1.5 * (q3 - q1))
    ) %>%
    dplyr::group_by(sipsa_name, city) %>%
    dplyr::mutate(n_atipicos = sum(atipico)) %>%
    dplyr::filter(!atipico | all(atipico)) %>%
    dplyr::ungroup()

  limite_g  <- if (is.null(umbral_gramos)) Inf else umbral_gramos
  limite_ml <- if (is.null(umbral_ml)) Inf else umbral_ml

  candidatos <- candidatos %>%
    dplyr::mutate(
      cumple_tamano = gramos_sku / densidad <= dplyr::if_else(objetivo_base == 1000, limite_ml, limite_g)
    ) %>%
    dplyr::group_by(sipsa_name, city) %>%
    dplyr::filter(cumple_tamano | !any(cumple_tamano)) %>%
    dplyr::mutate(
      n_candidatos = dplyr::n(),
      distancia = abs(precio_gramo_sku - stats::median(precio_gramo_sku))
    ) %>%
    dplyr::arrange(distancia, precio_gramo_sku, .by_group = TRUE) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::select(-q1, -q3, -atipico, -distancia, -cumple_tamano)

  dplyr::bind_rows(sin_medida, candidatos)
}
