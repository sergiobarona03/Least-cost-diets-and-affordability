########################################################
## SCRIPT 03_models/03_cord.R
## Cost of Recommended Diet (CoRD)
## Loop over cities x dates for the 13-city trimester panel.
##
## Reads:  output_dir/paneles/panel_mensual_cities_tcac.rds
##         output_dir/tcac/composicion_270726.rds
##         output_dir/gabas/gaba_exchanges_adj.rds
##         aux_dir/CoRD_Herforth.R
##         aux_dir/gaba_insumos.R
##
## Writes: cord_dir/cord_results.rds
##         cord_dir/cord_results.xlsx
########################################################

library(tidyverse)
library(writexl)

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

output_dir <- file.path(base_dir, "interno/output")
aux_dir    <- file.path(base_dir, "interno/03_models/aux-functions")
cord_dir   <- file.path(base_dir, "interno/03_models/cord")

dir.create(cord_dir, recursive = TRUE, showWarnings = FALSE)

source(file.path(aux_dir, "CoRD_Herforth.R"), encoding = "UTF-8")
source(file.path(aux_dir, "gaba_insumos.R"),  encoding = "UTF-8")

# -----------------------------------------------------------------------
# Load inputs
# -----------------------------------------------------------------------

message("Loading inputs...")

tcac <- readRDS(file.path(output_dir, "tcac/composicion_270726.rds"))

data_paper <- readRDS(file.path(output_dir, "paneles/panel_mensual_cities_tcac.rds")) %>%
  mutate(ciudad_norm = toupper(iconv(ciudad, from = "", to = "ASCII//TRANSLIT"))) %>%
  alimentos_gaba(tcac) %>%
  mutate(Price_serving = precio_100g * Serving_g / 100)

serv_adj <- readRDS(file.path(output_dir, "gabas/gaba_exchanges_adj.rds")) %>%
  requerimientos_gaba()

message(sprintf("  data_paper: %d rows | serv_adj: %d rows",
                nrow(data_paper), nrow(serv_adj)))

# -----------------------------------------------------------------------
# Loop: CoRD for each city x date
# -----------------------------------------------------------------------

dominios <- sort(unique(serv_adj$ciudad))
fechas   <- sort(unique(data_paper$fecha))

message(sprintf("Estimating CoRD: %d cities x %d dates...",
                length(dominios), length(fechas)))

out_cost <- list()
out_comp <- list()
n_ok <- 0L; n_fail <- 0L

for (i in dominios) {

  serv.aux <- serv_adj %>%
    filter(ciudad == i) %>%
    select(Age, Sex, Group, Serving)

  for (t in fechas) {

    data.aux <- data_paper %>%
      filter(ciudad_norm == i, fecha == t, !is.na(Price_serving)) %>%
      rename(Food = articulo, Energy = energia_kcal) %>%
      as.data.frame()

    if (nrow(data.aux) == 0) next

    # CoRD_Herforth() arma la dieta con los grupos que encuentre; sin un
    # grupo completo la dieta no cumple la GABA, asi que se salta la corrida.
    sin_grupo <- setdiff(DIVERSIDAD_GABA$Group, data.aux$Group)
    if (length(sin_grupo) > 0) {
      warning("Sin alimentos en ", paste(sin_grupo, collapse = ", "),
              " - ", i, " | ", t)
      n_fail <- n_fail + 1L
      next
    }

    result <- tryCatch(
      CoRD_Herforth(data    = data.aux,
                    serv    = serv.aux,
                    diverse = DIVERSIDAD_GABA,
                    exclude = EXCLUIDOS_GABA),
      error = function(e) {
        warning("Error - ", i, " | ", t, " | ", conditionMessage(e))
        NULL
      })

    if (!is.null(result)) {
      out_cost[[length(out_cost) + 1]] <- result$cost %>%
        mutate(ciudad = i, fecha = t)
      out_comp[[length(out_comp) + 1]] <- result$comp %>%
        mutate(ciudad = i, fecha = t)
      n_ok <- n_ok + 1L
    } else {
      n_fail <- n_fail + 1L
    }
  }
}

df.cost <- bind_rows(out_cost) %>%
  mutate(fecha = as.Date(fecha),
         year  = year(fecha),
         mes   = month(fecha))

df.comp <- bind_rows(out_comp) %>%
  mutate(fecha = as.Date(fecha))

message(sprintf("  Done. %d OK | %d failed | %d cost rows",
                n_ok, n_fail, nrow(df.cost)))

# -----------------------------------------------------------------------
# Save
# -----------------------------------------------------------------------

saveRDS(list(cost = df.cost, comp = df.comp),
        file.path(cord_dir, "cord_results.rds"))

write_xlsx(list(cost = df.cost, comp = df.comp),
           file.path(cord_dir, "cord_results.xlsx"))

message("Done. Run 04_cc_cona.R next.")
