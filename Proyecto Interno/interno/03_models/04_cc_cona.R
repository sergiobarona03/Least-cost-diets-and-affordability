########################################################
## SCRIPT 03_models/04_cc_cona.R
## CoNA restringido a la composicion del IPC (CC-CoNA).
## Loop sobre alpha x ciudades x fechas del panel de 13 ciudades.
##   alpha = 0    : CoNA sin restriccion de composicion
##   alpha = 1    : la dieta reproduce las participaciones del IPC (sin
##                  solucion en este panel; ver docs/workingpaper_041026)
##
## Hogar de 3 miembros por defecto; con N_MIEMBROS = 4 o 5 usa el hogar de
## 02_eer_4members.R o 02_eer_5members.R y los archivos llevan el sufijo _4m o _5m.
##
## Reads:  output_dir/paneles/panel_mensual_cities_tcac.rds
##         output_dir/ipc/mapeo_ipc_subclase.csv
##         output_dir/ipc/ipc_participaciones.rds
##         household_dir/household_eer_ll.rds, household_ul.rds
##         aux_dir/CoNA_IPC.R
##
## Writes: ccona_dir/ccona_results.rds
##         ccona_dir/ccona_results.xlsx
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

output_dir    <- file.path(base_dir, "interno/output")
aux_dir       <- file.path(base_dir, "interno/03_models/aux-functions")
household_dir <- file.path(base_dir, "interno/02_dataprep/household eer")
ccona_dir     <- file.path(base_dir, "interno/03_models/ccona")

dir.create(ccona_dir, recursive = TRUE, showWarnings = FALSE)

# Tamano del hogar: 3 (base), 4 o 5 miembros. La variable de entorno N_MIEMBROS lo
# cambia; los archivos de 4 y 5 miembros llevan el sufijo _4m o _5m.
n_miembros   <- as.integer(Sys.getenv("N_MIEMBROS", unset = "3"))
stopifnot(n_miembros %in% 3:5)
# VARIANTE_HOGAR pruebas de sensibilidad de los requerimientos; vacia es la version base:
#   "t3"  hogar de 4 con el incremento de energia del tercer trimestre de la gestacion
#   "esc" minimos y maximos de macronutrientes escalados a la energia de cada miembro
variante_hogar <- Sys.getenv("VARIANTE_HOGAR", unset = "")
stopifnot(variante_hogar %in% c("", "t3", "esc"), !(variante_hogar == "t3" && n_miembros != 4L))
sufijo_hogar <- paste0(if (n_miembros == 3L) "" else paste0("_", n_miembros, "m"),
                       if (nzchar(variante_hogar)) paste0("_", variante_hogar))

source(file.path(aux_dir, "CoNA_IPC.R"))

ALPHAS <- c(0, 0.25, 0.50, 0.75, 0.90, 1.00)

normalise_eer <- function(df) {
  if ("rango" %in% names(df)) names(df)[names(df) == "rango"] <- "Age"
  if ("sex"   %in% names(df)) names(df)[names(df) == "sex"]   <- "Sex"
  if ("eer"   %in% names(df)) names(df)[names(df) == "eer"]   <- "Energy"
  if ("Sex"   %in% names(df) && is.character(df$Sex))
    df$Sex <- if_else(df$Sex == "Masculino", 0L, 1L)
  df
}

# -----------------------------------------------------------------------
# Load inputs
# -----------------------------------------------------------------------

message("Loading inputs...")

mapeo <- read_csv(file.path(output_dir, "ipc/mapeo_ipc_subclase.csv"),
                  col_types = cols(.default = col_character())) %>%
  select(articulo, Group = subclase_codigo)

ipc_shares <- readRDS(file.path(output_dir, "ipc/ipc_participaciones.rds")) %>%
  transmute(Group = subclase_codigo, share) %>%
  as.data.frame()

data_paper <- readRDS(file.path(output_dir, "paneles/panel_mensual_cities_tcac.rds")) %>%
  mutate(ciudad_norm = toupper(iconv(ciudad, from = "", to = "ASCII//TRANSLIT"))) %>%
  left_join(mapeo, by = "articulo")

stopifnot(!anyNA(data_paper$Group))

household_eer_ll <- readRDS(file.path(household_dir, paste0("household_eer_ll", sufijo_hogar, ".rds")))
household_ul     <- readRDS(file.path(household_dir, paste0("household_ul", sufijo_hogar, ".rds")))

dominios <- sort(unique(household_eer_ll$ciudad))
fechas   <- sort(unique(data_paper$fecha))

message(sprintf("  data_paper: %d rows | %d IPC subclasses | %d alphas x %d cities x %d dates",
                nrow(data_paper), nrow(ipc_shares), length(ALPHAS),
                length(dominios), length(fechas)))

# -----------------------------------------------------------------------
# Loop: alpha x ciudad x fecha
# -----------------------------------------------------------------------

out_cost <- list(); out_comp <- list(); out_shares <- list()
n_ok <- 0L; n_fail <- 0L

for (alp in ALPHAS) {
  for (i in dominios) {

    eer.aux <- household_eer_ll %>%
      filter(ciudad == i) %>% select(-ciudad, -any_of("cod_mun")) %>%
      as.data.frame() %>% normalise_eer()
    ul.aux <- household_ul %>%
      filter(ciudad == i) %>% select(-ciudad, -any_of("cod_mun")) %>%
      as.data.frame() %>% normalise_eer()

    for (t in fechas) {

      data.aux <- data_paper %>%
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
                 IPC_shares = ipc_shares, alpha = alp),
        error = function(e) {
          warning("Error - ", i, " | ", t, " | alpha=", alp, " | ", conditionMessage(e))
          NULL
        })

      if (!is.null(res)) {
        out_cost[[length(out_cost) + 1]] <- res$cost %>%
          mutate(ciudad = i, fecha = t, alpha_val = alp)
        out_comp[[length(out_comp) + 1]] <- res$comp %>%
          mutate(ciudad = i, fecha = t, alpha_val = alp)
        out_shares[[length(out_shares) + 1]] <- res$shares %>%
          mutate(ciudad = i, fecha = t, alpha_val = alp)
        n_ok <- n_ok + 1L
      } else {
        n_fail <- n_fail + 1L
      }
    }
  }
  message(sprintf("  alpha = %.2f done", alp))
}

df.cost <- bind_rows(out_cost) %>%
  mutate(fecha = as.Date(fecha), year = year(fecha), mes = month(fecha))
df.comp   <- bind_rows(out_comp)   %>% mutate(fecha = as.Date(fecha))
df.shares <- bind_rows(out_shares) %>% mutate(fecha = as.Date(fecha))

message(sprintf("  Done. %d OK | %d failed | %d cost rows (%d without solution)",
                n_ok, n_fail, nrow(df.cost), sum(is.na(df.cost$cost_day))))

# -----------------------------------------------------------------------
# Save
# -----------------------------------------------------------------------

saveRDS(list(cost = df.cost, comp = df.comp, shares = df.shares),
        file.path(ccona_dir, paste0("ccona_results", sufijo_hogar, ".rds")))

write_xlsx(list(cost = df.cost, comp = df.comp, shares = df.shares),
           file.path(ccona_dir, paste0("ccona_results", sufijo_hogar, ".xlsx")))

message("Done.")
