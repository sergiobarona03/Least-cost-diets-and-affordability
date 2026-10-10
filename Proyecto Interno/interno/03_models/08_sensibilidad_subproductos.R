########################################################
## SCRIPT 03_models/08_sensibilidad_subproductos.R
## CoNA sin los alimentos que el mapeo cruza con subproductos o cortes
## de poco aprovechamiento:
##   sin_cogote:       sale "Carne de res, cogote", que el mapeo cruza con
##                     "hueso de res para sustancia"
##   sin_subproductos: salen ademas menudencias de pollo, cabezas de pescado
##                     y rabadillas de pollo
##
## No cambia ningun resultado del flujo principal: solo mide cuanto
## dependen el CoNA y sus ciudades de esos alimentos.
##
## Reads:  output_dir/paneles/panel_mensual_cities_tcac.rds
##         household_dir/household_eer_ll.rds, household_ul.rds
##         cona_dir/cona_results.rds
## Writes: sub_dir/subproductos_results.rds
##         sub_dir/subproductos_results.xlsx
########################################################

library(tidyverse)
library(writexl)
library(FoodpriceR)

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

output_dir    <- file.path(base_dir, "interno/output")
aux_dir       <- file.path(base_dir, "interno/03_models/aux-functions")
household_dir <- file.path(base_dir, "interno/02_dataprep/household eer")
cona_dir      <- file.path(base_dir, "interno/03_models/cona")
sub_dir       <- file.path(base_dir, "interno/03_models/subproductos")

dir.create(sub_dir, recursive = TRUE, showWarnings = FALSE)

source(file.path(aux_dir, "CoNA_paper.R"))

ESCENARIOS <- list(
  con_todo         = NULL,
  sin_cogote       = "Carne de res, cogote",
  sin_subproductos = c("Carne de res, cogote", "Menudencias de pollo",
                       "Pescado cabezas", "Rabadillas de pollo")
)

normalise_eer <- function(df) {
  if ("rango" %in% names(df)) names(df)[names(df) == "rango"] <- "Age"
  if ("sex"   %in% names(df)) names(df)[names(df) == "sex"]   <- "Sex"
  if ("eer"   %in% names(df)) names(df)[names(df) == "eer"]   <- "Energy"
  if ("Sex"   %in% names(df) && is.character(df$Sex))
    df$Sex <- if_else(df$Sex == "Masculino", 0L, 1L)
  df
}

# -----------------------------------------------------------------------
# Insumos
# -----------------------------------------------------------------------

message("Loading inputs...")

panel <- readRDS(file.path(output_dir, "paneles/panel_mensual_cities_tcac.rds")) %>%
  mutate(ciudad_norm = toupper(iconv(ciudad, from = "", to = "ASCII//TRANSLIT")))
household_eer_ll <- readRDS(file.path(household_dir, "household_eer_ll.rds"))
household_ul     <- readRDS(file.path(household_dir, "household_ul.rds"))

dominios <- sort(unique(household_eer_ll$ciudad))
fechas   <- sort(unique(panel$fecha))

# -----------------------------------------------------------------------
# Loop: escenario x ciudad x fecha
# -----------------------------------------------------------------------

costos <- list(); comps <- list()

for (esc in names(ESCENARIOS)) {
  message(sprintf("Estimating CoNA - %s...", esc))
  for (i in dominios) {
    eer.aux <- household_eer_ll %>%
      filter(ciudad == i) %>% select(-ciudad, -any_of("cod_mun")) %>%
      as.data.frame() %>% normalise_eer()
    ul.aux <- household_ul %>%
      filter(ciudad == i) %>% select(-ciudad, -any_of("cod_mun")) %>%
      as.data.frame() %>% normalise_eer()

    for (t in fechas) {
      data.aux <- panel %>%
        filter(ciudad_norm == i, fecha == t, !is.na(precio_100g)) %>%
        filter(articulo != "ARROZ PARA SOPA") %>%
        dplyr::rename(
          Food = articulo, Price_100g = precio_100g, Energy = energia_kcal,
          Protein = proteina_g, Lipids = lipidos_g,
          Carbohydrates = carbohidratos_totales_g, VitaminC = vitamina_c_mg,
          Folate = folatos_mcg, VitaminA = vitamina_a_er, Thiamine = tiamina_mg,
          Riboflavin = riboflavina_mg, Niacin = niacina_mg,
          VitaminB12 = vitamina_b12_mcg, Magnesium = magnesio_mg,
          Phosphorus = fosforo_mg, Sodium = sodio_mg, Calcium = calcio_mg,
          Iron = hierro_mg, Zinc = zinc_mg
        ) %>%
        as.data.frame()

      res <- tryCatch(
        CoNA_paper(data = data.aux, EER_LL = eer.aux, UL = ul.aux, exclude = ESCENARIOS[[esc]]),
        error = function(e) {
          warning(esc, " - ", i, " | ", t, " | ", conditionMessage(e)); NULL })

      if (!is.null(res) && !is.null(res$cost)) {
        costos[[length(costos) + 1]] <- res$cost %>%
          mutate(escenario = esc, ciudad = i, fecha = t)
        comps[[length(comps) + 1]] <- res$comp %>%
          mutate(escenario = esc, ciudad = i, fecha = t)
      }
    }
  }
}

costos <- bind_rows(costos) %>% mutate(fecha = as.Date(fecha), Sex = as.character(Sex))
comp   <- bind_rows(comps)  %>% mutate(fecha = as.Date(fecha))

# -----------------------------------------------------------------------
# Validacion y resumen
# -----------------------------------------------------------------------

principal <- readRDS(file.path(cona_dir, "cona_results.rds"))$cost %>%
  mutate(fecha = as.Date(fecha), Sex = as.character(Sex)) %>%
  select(ciudad, fecha, Demo_Group, Sex, costo_principal = cona_cost)

validacion <- costos %>%
  filter(escenario == "con_todo") %>%
  inner_join(principal, by = c("ciudad", "fecha", "Demo_Group", "Sex")) %>%
  summarise(filas = n(), dif_maxima = max(abs(cost_day - costo_principal)))
stopifnot(validacion$filas == 117, validacion$dif_maxima < 1e-6)

per_capita <- costos %>%
  group_by(escenario, ciudad, fecha) %>%
  summarise(hogar = sum(cost_day), .groups = "drop") %>%
  group_by(escenario, ciudad) %>%
  summarise(pc = mean(hogar) / 3, .groups = "drop")

resumen <- per_capita %>%
  group_by(escenario) %>%
  summarise(pc = mean(pc), .groups = "drop") %>%
  mutate(razon_con_todo = pc / pc[escenario == "con_todo"])

# -----------------------------------------------------------------------
# Save
# -----------------------------------------------------------------------

salida <- list(costos = costos, comp = comp, per_capita = per_capita,
               resumen = resumen, validacion = validacion)
saveRDS(salida, file.path(sub_dir, "subproductos_results.rds"))
write_xlsx(list(resumen = resumen, per_capita = per_capita, costos = costos),
           file.path(sub_dir, "subproductos_results.xlsx"))

message("Done.")
