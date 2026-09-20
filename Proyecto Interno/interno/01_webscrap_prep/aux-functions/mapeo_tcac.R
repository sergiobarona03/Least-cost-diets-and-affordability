########################################################
## FUNCIONES mapeo_tcac.R
## 01_webscrap_prep/aux-functions/mapeo_tcac.R
##
## Cruce de los alimentos SIPSA con el mapeo a TCAC (composicion
## nutricional y grupo GABA). Lo usan 02a_lista_alimentos.R (grupo de
## cada alimento) y 02_dataprep/00_tcac_composicion.R (composicion).
########################################################

normalizar_nombre <- function(x) {
  x %>%
    stringr::str_to_upper() %>%
    stringi::stri_trans_general("Latin-ASCII") %>%
    stringr::str_squish()
}

leer_mapeo_tcac <- function(ruta) {
  openxlsx::read.xlsx(ruta, sheet = "Imputada") %>%
    dplyr::rename(sipsa_name = `Alimento.(Nombre.sipsa)`) %>%
    dplyr::mutate(sipsa_name = stringr::str_squish(as.character(sipsa_name)))
}

# lista: data frame con sipsa_name. Devuelve la lista con las columnas del mapeo.
unir_mapeo_tcac <- function(lista, mapeo) {
  # Alimentos que el mapeo nombra distinto
  lista <- lista %>%
    dplyr::mutate(
      sipsa_name_join = dplyr::case_when(
        sipsa_name == "Ajo importado"                    ~ "Ajo",
        sipsa_name == "Almejas con concha"               ~ "Almejas",
        sipsa_name == "Bagre rayado en postas congelado" ~ "Bagre rayado",
        sipsa_name == "Carne de cerdo, lomo sin hueso"   ~ "Carne de cerdo, lomo",
        sipsa_name == "Carne de cerdo, pernil sin hueso" ~ "Carne de cerdo, lomo",
        sipsa_name == "Trucha en corte mariposa"         ~ "Trucha",
        sipsa_name == "Uva roja"                         ~ "Uva comun",
        sipsa_name == "Yuca ICA"                         ~ "Yuca",
        TRUE ~ sipsa_name
      ),
      sipsa_name_norm = normalizar_nombre(sipsa_name_join)
    )

  mapeo_unico <- mapeo %>%
    dplyr::mutate(sipsa_name_norm = normalizar_nombre(sipsa_name)) %>%
    dplyr::distinct(sipsa_name_norm, .keep_all = TRUE)

  lista %>%
    dplyr::left_join(mapeo_unico %>% dplyr::select(-sipsa_name), by = "sipsa_name_norm") %>%
    dplyr::select(-sipsa_name_norm, -sipsa_name_join) %>%
    dplyr::relocate(sipsa_name)
}
