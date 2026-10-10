########################################################
## SCRIPT 03_models/06_sensibilidad_azucar.R
## Sensibilidad de CoNA, CC-CoNA, CoRD y CoAHD a los azucares.
##
## Cada modelo se estima dos veces, con las mismas ciudades, fechas,
## precios y requerimientos que el flujo principal:
##   con_azucar: panel completo (reproduce los resultados del proyecto)
##   sin_azucar: sin los alimentos del grupo AZUCARES del panel
##               (azucar morena, refinada y sulfitada, panela cuadrada
##               y redonda). En CoRD y CoAHD tambien sale el grupo
##               "Azucares" de la dieta, que pasa de 7 grupos y 12
##               alimentos a 6 grupos y 11 alimentos. En el CC-CoNA (con el
##               alpha de referencia) sale ademas la subclase del IPC de
##               azucar y panela, y las demas se reescalan.
##
## CoCA no se repite: solo usa el grupo de cereales, raices, tuberculos
## y platanos, que no incluye azucares, y su costo no cambia.
##
## Reads:  output_dir/paneles/panel_mensual_cities_tcac.rds
##         output_dir/tcac/composicion_270726.rds
##         output_dir/gabas/gaba_exchanges_adj.rds
##         output_dir/ipc/mapeo_ipc_subclase.csv, ipc_participaciones.rds
##         household_dir/household_eer_ll.rds, household_ul.rds
##         cona_dir, ccona_dir, cord_dir, coahd_dir (resultados del flujo principal)
##         aux_dir/CoNA_paper.R, CoNA_IPC.R, CoRD_Herforth.R,
##                 CoAHD_Herforth.R, gaba_insumos.R, alfa_referencia.R
##
## Writes: azucar_dir/azucar_results.rds
##         azucar_dir/azucar_results.xlsx
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
ccona_dir     <- file.path(base_dir, "interno/03_models/ccona")
cord_dir      <- file.path(base_dir, "interno/03_models/cord")
coahd_dir     <- file.path(base_dir, "interno/03_models/coahd")
azucar_dir    <- file.path(base_dir, "interno/03_models/azucar")

dir.create(azucar_dir, recursive = TRUE, showWarnings = FALSE)

source(file.path(aux_dir, "CoNA_paper.R"))
source(file.path(aux_dir, "CoNA_IPC.R"))
source(file.path(aux_dir, "alfa_referencia.R"))
source(file.path(aux_dir, "CoRD_Herforth.R"), encoding = "UTF-8")
source(file.path(aux_dir, "CoAHD_Herforth.R"), encoding = "UTF-8")
source(file.path(aux_dir, "gaba_insumos.R"),   encoding = "UTF-8")

GRUPO_AZUCAR   <- "Azúcares"
ESCENARIOS     <- c("con_azucar", "sin_azucar")
N_MIEMBROS     <- 3

# -----------------------------------------------------------------------
# Insumos
# -----------------------------------------------------------------------

message("Loading inputs...")

panel <- readRDS(file.path(output_dir, "paneles/panel_mensual_cities_tcac.rds")) %>%
  mutate(ciudad_norm = toupper(iconv(ciudad, from = "", to = "ASCII//TRANSLIT")))

tcac <- readRDS(file.path(output_dir, "tcac/composicion_270726.rds"))

alimentos_azucar <- panel %>%
  filter(grupos_gabas == "AZUCARES") %>%
  distinct(articulo) %>%
  pull(articulo)

household_eer_ll <- readRDS(file.path(household_dir, "household_eer_ll.rds"))
household_ul     <- readRDS(file.path(household_dir, "household_ul.rds"))

serv_adj <- readRDS(file.path(output_dir, "gabas/gaba_exchanges_adj.rds")) %>%
  requerimientos_gaba()

panel_gaba <- alimentos_gaba(panel, tcac)

# Subclases del IPC de cada alimento y su participacion (CC-CoNA)
mapeo <- read_csv(file.path(output_dir, "ipc/mapeo_ipc_subclase.csv"),
                  col_types = cols(.default = col_character())) %>%
  select(articulo, Group = subclase_codigo)

ipc_shares <- readRDS(file.path(output_dir, "ipc/ipc_participaciones.rds")) %>%
  transmute(Group = subclase_codigo, share) %>%
  as.data.frame()

panel_ipc <- panel %>% left_join(mapeo, by = "articulo")
stopifnot(!anyNA(panel_ipc$Group))

message(sprintf("  Alimentos del grupo AZUCARES: %s",
                paste(alimentos_azucar, collapse = ", ")))

normalise_eer <- function(df) {
  if ("rango" %in% names(df)) names(df)[names(df) == "rango"] <- "Age"
  if ("sex"   %in% names(df)) names(df)[names(df) == "sex"]   <- "Sex"
  if ("eer"   %in% names(df)) names(df)[names(df) == "eer"]   <- "Energy"
  if ("Sex"   %in% names(df) && is.character(df$Sex))
    df$Sex <- if_else(df$Sex == "Masculino", 0L, 1L)
  df
}

dominios <- sort(unique(household_eer_ll$ciudad))
fechas   <- sort(unique(panel$fecha))

# -----------------------------------------------------------------------
# CoNA
# -----------------------------------------------------------------------

estimar_cona <- function(escenario) {
  datos <- panel
  if (escenario == "sin_azucar") datos <- filter(datos, !articulo %in% alimentos_azucar)

  costos <- list(); comps <- list()

  for (i in dominios) {
    eer.aux <- household_eer_ll %>%
      filter(ciudad == i) %>% select(-ciudad, -any_of("cod_mun")) %>%
      as.data.frame() %>% normalise_eer()
    ul.aux <- household_ul %>%
      filter(ciudad == i) %>% select(-ciudad, -any_of("cod_mun")) %>%
      as.data.frame() %>% normalise_eer()

    for (t in fechas) {
      data.aux <- datos %>%
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

      if (nrow(data.aux) == 0) next

      res <- tryCatch(
        CoNA_paper(data = data.aux, EER_LL = eer.aux, UL = ul.aux),
        error = function(e) {
          warning("CoNA ", escenario, " - ", i, " | ", t, " | ", conditionMessage(e))
          NULL
        })

      if (!is.null(res) && !is.null(res$cost)) {
        costos[[length(costos) + 1]] <- res$cost %>% mutate(ciudad = i, fecha = t)
        comps[[length(comps) + 1]]   <- res$comp %>% mutate(ciudad = i, fecha = t)
      }
    }
  }

  list(
    cost = bind_rows(costos) %>% mutate(fecha = as.Date(fecha)) %>%
      dplyr::rename(costo = cost_day),
    comp = bind_rows(comps) %>% mutate(fecha = as.Date(fecha))
  )
}

# -----------------------------------------------------------------------
# CC-CoNA (alpha de referencia)
# -----------------------------------------------------------------------

estimar_ccona <- function(escenario) {
  datos <- panel_ipc
  if (escenario == "sin_azucar") datos <- filter(datos, !articulo %in% alimentos_azucar)

  costos <- list(); comps <- list()

  for (i in dominios) {
    eer.aux <- household_eer_ll %>%
      filter(ciudad == i) %>% select(-ciudad, -any_of("cod_mun")) %>%
      as.data.frame() %>% normalise_eer()
    ul.aux <- household_ul %>%
      filter(ciudad == i) %>% select(-ciudad, -any_of("cod_mun")) %>%
      as.data.frame() %>% normalise_eer()

    for (t in fechas) {
      data.aux <- datos %>%
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

      if (nrow(data.aux) == 0) next

      res <- tryCatch(
        CoNA_IPC(data = data.aux, EER_LL = eer.aux, UL = ul.aux,
                 IPC_shares = ipc_shares, alpha = ALFA_REFERENCIA),
        error = function(e) {
          warning("CC-CoNA ", escenario, " - ", i, " | ", t, " | ", conditionMessage(e))
          NULL
        })

      if (!is.null(res)) {
        costos[[length(costos) + 1]] <- res$cost %>% mutate(ciudad = i, fecha = t)
        comps[[length(comps) + 1]]   <- res$comp %>% mutate(ciudad = i, fecha = t)
      }
    }
  }

  list(
    cost = bind_rows(costos) %>% mutate(fecha = as.Date(fecha)) %>%
      dplyr::rename(costo = cost_day),
    comp = bind_rows(comps) %>% mutate(fecha = as.Date(fecha))
  )
}

# -----------------------------------------------------------------------
# CoRD y CoAHD
# -----------------------------------------------------------------------

diversidad_escenario <- function(escenario) {
  if (escenario == "sin_azucar") filter(DIVERSIDAD_GABA, Group != GRUPO_AZUCAR)
  else DIVERSIDAD_GABA
}

datos_gaba_escenario <- function(escenario) {
  if (escenario == "sin_azucar") filter(panel_gaba, Group != GRUPO_AZUCAR) else panel_gaba
}

estimar_cord <- function(escenario) {
  diverse <- diversidad_escenario(escenario)
  datos   <- datos_gaba_escenario(escenario) %>%
    mutate(Price_serving = precio_100g * Serving_g / 100)
  req <- serv_adj %>% filter(Group %in% diverse$Group)

  costos <- list(); comps <- list()

  for (i in sort(unique(req$ciudad))) {
    serv.aux <- req %>% filter(ciudad == i) %>% select(Age, Sex, Group, Serving)

    for (t in fechas) {
      data.aux <- datos %>%
        filter(ciudad_norm == i, fecha == t, !is.na(Price_serving)) %>%
        rename(Food = articulo, Energy = energia_kcal) %>%
        as.data.frame()

      if (nrow(data.aux) == 0 || length(setdiff(diverse$Group, data.aux$Group)) > 0) next

      res <- tryCatch(
        CoRD_Herforth(data = data.aux, serv = serv.aux, diverse = diverse,
                      exclude = EXCLUIDOS_GABA),
        error = function(e) {
          warning("CoRD ", escenario, " - ", i, " | ", t, " | ", conditionMessage(e))
          NULL
        })

      if (!is.null(res)) {
        costos[[length(costos) + 1]] <- res$cost %>% mutate(ciudad = i, fecha = t)
        comps[[length(comps) + 1]]   <- res$comp %>% mutate(ciudad = i, fecha = t)
      }
    }
  }

  list(
    cost = bind_rows(costos) %>% mutate(fecha = as.Date(fecha)) %>%
      dplyr::rename(costo = cost_day),
    comp = bind_rows(comps) %>% mutate(fecha = as.Date(fecha))
  )
}

estimar_coahd <- function(escenario) {
  diverse <- diversidad_escenario(escenario)
  datos   <- datos_gaba_escenario(escenario)
  req     <- serv_adj %>% filter(Group %in% diverse$Group)

  costos <- list(); comps <- list()

  for (i in sort(unique(req$ciudad))) {
    req.aux <- req %>% filter(ciudad == i) %>% select(Age, Sex, Group, Kcal)

    for (t in fechas) {
      data.aux <- datos %>%
        filter(ciudad_norm == i, fecha == t) %>%
        select(Food = articulo, Group, Price_100g = precio_100g, Energy = energia_kcal) %>%
        as.data.frame()

      if (nrow(data.aux) == 0) next

      res <- tryCatch(
        CoAHD_Herforth(data = data.aux, req = req.aux, diverse = diverse,
                       exclude = EXCLUIDOS_GABA),
        error = function(e) {
          warning("CoAHD ", escenario, " - ", i, " | ", t, " | ", conditionMessage(e))
          NULL
        })

      if (!is.null(res)) {
        costos[[length(costos) + 1]] <- res$cost %>% mutate(ciudad = i, fecha = t)
        comps[[length(comps) + 1]]   <- res$comp %>% mutate(ciudad = i, fecha = t)
      }
    }
  }

  list(
    cost = bind_rows(costos) %>% mutate(fecha = as.Date(fecha)) %>%
      dplyr::rename(costo = cost_day),
    comp = bind_rows(comps) %>% mutate(fecha = as.Date(fecha))
  )
}

# -----------------------------------------------------------------------
# Correr los seis casos
# -----------------------------------------------------------------------

modelos <- list(CoNA = estimar_cona, "CC-CoNA" = estimar_ccona,
                CoRD = estimar_cord, CoAHD = estimar_coahd)

resultados <- list()
for (m in names(modelos)) {
  for (e in ESCENARIOS) {
    message(sprintf("Estimating %s - %s...", m, e))
    resultados[[paste(m, e, sep = "|")]] <- modelos[[m]](e)
  }
}

unir <- function(campo) {
  imap_dfr(resultados, function(r, nombre) {
    p <- strsplit(nombre, "|", fixed = TRUE)[[1]]
    r[[campo]] %>% mutate(Sex = as.character(Sex), modelo = p[1], escenario = p[2], .before = 1)
  })
}

costos <- unir("cost") %>%
  mutate(kcal_dieta = costo / Cost_1000kcal * 1000)
comp <- unir("comp")

# -----------------------------------------------------------------------
# Razones sin azucar / con azucar
# -----------------------------------------------------------------------

por_ciudad_fecha <- costos %>%
  group_by(modelo, escenario, ciudad, fecha) %>%
  summarise(hogar = sum(costo), kcal_hogar = sum(kcal_dieta), .groups = "drop")

per_capita <- por_ciudad_fecha %>%
  group_by(modelo, escenario, ciudad) %>%
  summarise(pc = mean(hogar) / N_MIEMBROS,
            kcal_pc = mean(kcal_hogar) / N_MIEMBROS, .groups = "drop") %>%
  mutate(pc_1000kcal = pc / kcal_pc * 1000)

razones_ciudad <- per_capita %>%
  pivot_wider(names_from = escenario, values_from = c(pc, kcal_pc, pc_1000kcal)) %>%
  transmute(
    modelo, ciudad,
    pc_con = pc_con_azucar, pc_sin = pc_sin_azucar,
    razon = pc_sin_azucar / pc_con_azucar,
    kcal_con = kcal_pc_con_azucar, kcal_sin = kcal_pc_sin_azucar,
    pc1000_con = pc_1000kcal_con_azucar, pc1000_sin = pc_1000kcal_sin_azucar,
    razon_1000kcal = pc_1000kcal_sin_azucar / pc_1000kcal_con_azucar
  )

razones_modelo <- razones_ciudad %>%
  group_by(modelo) %>%
  summarise(pc_con = mean(pc_con), pc_sin = mean(pc_sin),
            razon_promedio = pc_sin / pc_con,
            razon_min = min(razon), razon_max = max(razon),
            razon_1000kcal = mean(pc1000_sin) / mean(pc1000_con),
            .groups = "drop")

# Razon por ciudad y mes (para ver la dispersion)
razones_mes <- por_ciudad_fecha %>%
  select(-kcal_hogar) %>%
  pivot_wider(names_from = escenario, values_from = hogar) %>%
  mutate(razon = sin_azucar / con_azucar)

# -----------------------------------------------------------------------
# Validacion: con_azucar reproduce el flujo principal
# -----------------------------------------------------------------------

principal <- bind_rows(
  CoNA  = readRDS(file.path(cona_dir,  "cona_results.rds"))$cost %>%
    select(ciudad, fecha, Demo_Group, Sex, costo_principal = cona_cost),
  "CC-CoNA" = readRDS(file.path(ccona_dir, "ccona_results.rds"))$cost %>%
    filter(alpha_val == ALFA_REFERENCIA) %>%
    select(ciudad, fecha, Demo_Group, Sex, costo_principal = cost_day),
  CoRD  = readRDS(file.path(cord_dir,  "cord_results.rds"))$cost %>%
    select(ciudad, fecha, Demo_Group, Sex, costo_principal = cost_day),
  CoAHD = readRDS(file.path(coahd_dir, "coahd_results.rds"))$cost %>%
    select(ciudad, fecha, Demo_Group, Sex, costo_principal = cost_day),
  .id = "modelo"
) %>% mutate(fecha = as.Date(fecha), Sex = as.character(Sex))

validacion <- costos %>%
  filter(escenario == "con_azucar") %>%
  select(modelo, ciudad, fecha, Demo_Group, Sex, costo) %>%
  inner_join(principal, by = c("modelo", "ciudad", "fecha", "Demo_Group", "Sex")) %>%
  group_by(modelo) %>%
  summarise(filas = n(), dif_maxima = max(abs(costo - costo_principal)), .groups = "drop")

stopifnot(all(validacion$dif_maxima < 1e-6))

# -----------------------------------------------------------------------
# Uso del azucar en las dietas con azucar
# -----------------------------------------------------------------------

precios_azucar <- panel %>%
  filter(articulo %in% alimentos_azucar) %>%
  group_by(ciudad_norm, articulo) %>%
  summarise(precio_100g = mean(precio_100g, na.rm = TRUE),
            kcal_100g = first(energia_kcal),
            meses = n_distinct(fecha), .groups = "drop") %>%
  mutate(cop_por_100kcal = precio_100g / kcal_100g * 100)

uso_cona <- comp %>%
  filter(modelo == "CoNA", escenario == "con_azucar", Food %in% alimentos_azucar, quantity > 0) %>%
  left_join(panel %>% select(Food = articulo, ciudad = ciudad_norm, fecha, precio_100g, energia_kcal),
            by = c("Food", "ciudad", "fecha")) %>%
  mutate(gramos = quantity, kcal = gramos / 100 * energia_kcal,
         costo = gramos / 100 * precio_100g)

uso_cord_ahd <- comp %>%
  filter(modelo %in% c("CoRD", "CoAHD"), escenario == "con_azucar", Group == GRUPO_AZUCAR)

# Alimentos que entran o suben en CoNA al quitar el azucar
cambio_cona <- comp %>%
  filter(modelo == "CoNA") %>%
  group_by(escenario, ciudad, fecha, Food) %>%
  summarise(gramos = sum(quantity) / N_MIEMBROS, .groups = "drop") %>%
  group_by(escenario, Food) %>%
  summarise(gramos = sum(gramos) / (length(dominios) * length(fechas)), .groups = "drop") %>%
  pivot_wider(names_from = escenario, values_from = gramos, values_fill = 0) %>%
  mutate(cambio_g = sin_azucar - con_azucar) %>%
  arrange(desc(abs(cambio_g)))

# -----------------------------------------------------------------------
# Save
# -----------------------------------------------------------------------

salida <- list(
  alimentos_azucar = alimentos_azucar,
  costos           = costos,
  razones_modelo   = razones_modelo,
  razones_ciudad   = razones_ciudad,
  razones_mes      = razones_mes,
  validacion       = validacion,
  precios_azucar   = precios_azucar,
  uso_cona         = uso_cona,
  uso_cord_ahd     = uso_cord_ahd,
  cambio_cona      = cambio_cona,
  comp             = comp
)

saveRDS(salida, file.path(azucar_dir, "azucar_results.rds"))

write_xlsx(
  list(razones_modelo = razones_modelo,
       razones_ciudad = razones_ciudad,
       razones_mes    = razones_mes,
       validacion     = validacion,
       precios_azucar = precios_azucar,
       cambio_cona    = cambio_cona,
       costos         = costos),
  file.path(azucar_dir, "azucar_results.xlsx"))

message("Done.")
