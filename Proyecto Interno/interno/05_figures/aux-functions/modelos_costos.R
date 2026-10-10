########################################################
## 05_figures/aux-functions/modelos_costos.R
##
## Lectura de los resultados de las cinco dietas y costo per capita,
## para las figuras que comparan CoCA, CoNA, CC-CoNA, CoRD y CoAHD.
## Requiere 00_fig_config.R cargado (coca_dir, cona_dir, ccona_dir,
## cord_dir, coahd_dir y ALFA_REFERENCIA).
########################################################

MODELOS <- c("CoCA", "CoNA", "CC-CoNA", "CoRD", "CoAHD")

# Costo diario y costo por 1000 kcal por miembro y fecha, con las mismas columnas
# en las cinco dietas. `n_miembros` es 3, 4 o 5 (hogar de 02_eer_*members.R) y
# `alfa` el alpha del CC-CoNA.
leer_costos_modelos <- function(n_miembros = 3L, alfa = ALFA_REFERENCIA) {
  sufijo <- if (n_miembros == 3L) "" else paste0("_", n_miembros, "m")
  lista <- list(
    CoCA  = readRDS(file.path(coca_dir,  paste0("coca_results", sufijo, ".rds"))) %>%
      dplyr::distinct(ciudad, fecha, Demo_Group, Sex, costo = cost_day, kcal1000 = Cost_1000kcal),
    CoNA  = readRDS(file.path(cona_dir,  paste0("cona_results", sufijo, ".rds")))$cost %>%
      dplyr::select(ciudad, fecha, Demo_Group, Sex, costo = cona_cost, kcal1000 = Cost_1000kcal),
    "CC-CoNA" = readRDS(file.path(ccona_dir, paste0("ccona_results", sufijo, ".rds")))$cost %>%
      dplyr::filter(alpha_val == alfa) %>%
      dplyr::select(ciudad, fecha, Demo_Group, Sex, costo = cost_day, kcal1000 = Cost_1000kcal),
    CoRD  = readRDS(file.path(cord_dir,  paste0("cord_results", sufijo, ".rds")))$cost %>%
      dplyr::select(ciudad, fecha, Demo_Group, Sex, costo = cost_day, kcal1000 = Cost_1000kcal),
    CoAHD = readRDS(file.path(coahd_dir, paste0("coahd_results", sufijo, ".rds")))$cost %>%
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
