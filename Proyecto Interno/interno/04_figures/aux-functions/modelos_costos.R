########################################################
## 04_figures/aux-functions/modelos_costos.R
##
## Lectura de los resultados de los cuatro modelos y costo per capita,
## para las figuras que comparan CoCA, CoNA, CoRD y CoAHD.
## Requiere 00_fig_config.R cargado (coca_dir, cona_dir, cord_dir, coahd_dir).
########################################################

MODELOS <- c("CoCA", "CoNA", "CoRD", "CoAHD")

# Costo diario y costo por 1000 kcal por miembro y fecha, con las mismas columnas en los cuatro
leer_costos_modelos <- function() {
  lista <- list(
    CoCA  = readRDS(file.path(coca_dir,  "coca_results.rds")) %>%
      dplyr::distinct(ciudad, fecha, Demo_Group, Sex, costo = cost_day, kcal1000 = Cost_1000kcal),
    CoNA  = readRDS(file.path(cona_dir,  "cona_results.rds"))$cost %>%
      dplyr::select(ciudad, fecha, Demo_Group, Sex, costo = cona_cost, kcal1000 = Cost_1000kcal),
    CoRD  = readRDS(file.path(cord_dir,  "cord_results.rds"))$cost %>%
      dplyr::select(ciudad, fecha, Demo_Group, Sex, costo = cost_day, kcal1000 = Cost_1000kcal),
    CoAHD = readRDS(file.path(coahd_dir, "coahd_results.rds"))$cost %>%
      dplyr::select(ciudad, fecha, Demo_Group, Sex, costo = cost_day, kcal1000 = Cost_1000kcal)
  )
  dplyr::bind_rows(lista, .id = "modelo") %>%
    dplyr::mutate(fecha  = as.Date(fecha),
                  modelo = factor(modelo, levels = MODELOS))
}

# Costo per capita: suma de los miembros / numero de miembros, promedio del trimestre
costo_percapita <- function(costos) {
  n_miembros <- dplyr::n_distinct(costos$Demo_Group, costos$Sex)
  costos %>%
    dplyr::group_by(modelo, ciudad, fecha) %>%
    dplyr::summarise(hogar = sum(costo), .groups = "drop") %>%
    dplyr::group_by(modelo, ciudad) %>%
    dplyr::summarise(pc = mean(hogar) / n_miembros, .groups = "drop")
}
