########################################################
## FUNCION seleccionar_sku_umbral()
## 01_webscrap_prep/aux-functions/seleccionar_sku_umbral.R
##
## Elige, por alimento-ciudad, el sku representativo que se usa
## para estandarizar el precio (en 01_construccion_panel.R).
##
## Regla:
##  0. Solo compiten los skus con medida conocida que cumplen el
##     umbral de fechas de su ciudad (85% de sus dias, igual que
##     02a_lista_alimentos.R). Si ninguno cumple, compiten todos.
##     Sin este paso, el sku mas barato podia ser una marca que el
##     scraper vio pocos dias y el alimento entero se caia de la
##     ciudad (leche en polvo en Manizales, Ibague y Villavicencio).
##  1. Se calcula el precio por gramo de cada sku.
##  2. Se consideran los que pesan <= umbral_gramos; si ninguno,
##     todos los que pasaron el paso 0.
##  3. Se elige el de menor precio por gramo.
##  4. Si el alimento-ciudad no tiene ningun sku con medida en el
##     nombre (se vende por unidad o por kg/g), no se elige nada y
##     se devuelven todos sus skus para que el resto del proceso
##     los resuelva con la formula de kilogramo, gramo o unidad.
##
## umbral_gramos = NULL: sin umbral de tamano.
##
## Parametros
##   datos:         una fila por sku (alimento-ciudad-sku_code) con
##                  sipsa_name, city, n_fechas (dias en que aparecio
##                  el sku), gramos_sku (NA si el nombre no trae
##                  medida) y precio_gramo_sku
##   umbral_gramos: tamano maximo en gramos (500 por defecto)
##   umbral_fechas_ciudad: city y umbral_fechas (minimo de dias que
##                  necesita un sku para competir en esa ciudad)
##
## Devuelve: una fila por sku ganador de cada alimento-ciudad, mas
##   los skus completos de los alimentos-ciudad sin medida (paso 4)
########################################################

seleccionar_sku_umbral <- function(datos, umbral_gramos = 500, 
                                   umbral_fechas_ciudad) {
  
  # Por alimento-ciudad: ¿hay al menos un sku con medida conocida?
  grupos_con_medida <- datos %>%
    dplyr::group_by(sipsa_name, city) %>%
    dplyr::summarise(tiene_medida = any(!is.na(gramos_sku)),
                     .groups = "drop")

  datos <- datos %>%
    dplyr::left_join(grupos_con_medida, by = c("sipsa_name", "city"))

  # Punto 5: alimento-ciudad sin ningun sku medido -> se devuelven
  # todos sus skus tal cual, sin pasar por el criterio de umbral.
  sin_medida <- datos %>%
    dplyr::filter(!tiene_medida) %>%
    dplyr::select(-tiene_medida)

  con_medida <- datos %>%
    dplyr::filter(tiene_medida, !is.na(gramos_sku), !is.na(precio_gramo_sku)) %>%
    dplyr::select(-tiene_medida)

  # Punto 0: cobertura de fechas ANTES que precio
  con_medida <- con_medida %>%
    dplyr::left_join(umbral_fechas_ciudad, by = "city") %>%
    dplyr::mutate(cumple_cobertura = n_fechas >= umbral_fechas)

  hay_cobertura <- con_medida %>%
    dplyr::group_by(sipsa_name, city) %>%
    dplyr::summarise(algun_cumple_cobertura = any(cumple_cobertura), .groups = "drop")

  con_medida <- con_medida %>%
    dplyr::left_join(hay_cobertura, by = c("sipsa_name", "city")) %>%
    # solo compiten los que cumplen cobertura; si ninguno cumple,
    # compiten todos (para no perder el alimento del todo)
    dplyr::filter(cumple_cobertura | !algun_cumple_cobertura) %>%
    dplyr::select(-umbral_fechas, -cumple_cobertura, -algun_cumple_cobertura)

  # Puntos 1-4: umbral de tamano, dentro de los que ya pasaron cobertura
  con_medida <- con_medida %>%
    dplyr::mutate(
      cumple_umbral = if (is.null(umbral_gramos)) TRUE else gramos_sku <= umbral_gramos
    )

  hay_bajo_umbral <- con_medida %>%
    dplyr::group_by(sipsa_name, city) %>%
    dplyr::summarise(algun_bajo_umbral = any(cumple_umbral), .groups = "drop")

  ganadores_con_medida <- con_medida %>%
    dplyr::left_join(hay_bajo_umbral, by = c("sipsa_name", "city")) %>%
    # se compite dentro del umbral; si nadie lo cumple, se compite sin umbral
    dplyr::filter(cumple_umbral | !algun_bajo_umbral) %>%
    dplyr::group_by(sipsa_name, city) %>%
    dplyr::slice_min(precio_gramo_sku, n = 1, with_ties = FALSE) %>%
    dplyr::ungroup() %>%
    dplyr::select(-cumple_umbral, -algun_bajo_umbral)

  dplyr::bind_rows(sin_medida, ganadores_con_medida)
}
