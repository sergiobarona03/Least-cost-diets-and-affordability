########################################################
## SCRIPT 02_dataprep/05_mapeo_ipc.R
## Asigna a cada alimento del panel la subclase del IPC (DANE) a la
## que pertenece y calcula la participacion de cada subclase, que
## usa el CoNA restringido al IPC (CC-CoNA, 03_models/04_cc_cona.R).
##
## El mapeo se hace por el nombre del alimento con las reglas de
## `reglas_ipc`, en orden: gana la primera que coincide. Si un alimento
## del panel no coincide con ninguna regla el script se detiene.
##
## Reads:  output_dir/paneles/panel_mensual_cities_tcac.rds
##         ipc_dir/cpi-weights-2024.xlsx  (ponderaciones del IPC, DANE)
##
## Writes: output_dir/ipc/mapeo_ipc_subclase.csv
##         output_dir/ipc/ipc_participaciones.csv
##         output_dir/ipc/ipc_participaciones.rds
########################################################

library(tidyverse)
library(readxl)
library(stringi)

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
ipc_dir    <- file.path(base_dir, "interno/02_dataprep/ipc")
salida_dir <- file.path(output_dir, "ipc")

dir.create(salida_dir, recursive = TRUE, showWarnings = FALSE)

# -----------------------------------------------------------------------
# Reglas: patron sobre el nombre del alimento (minusculas, sin tildes)
# y codigo de subclase del IPC
# -----------------------------------------------------------------------

reglas_ipc <- tribble(
  ~patron,                                                                 ~codigo,
  "^arroz",                                                                "01110100",
  "^pastas",                                                               "01110200",
  "^avena",                                                                "01110300",
  "^harina de trigo",                                                      "01110500",
  "^fecula",                                                               "01119800",
  "^maiz enlatado",                                                        "01170300",
  "^(harina precocida|cuchuco|maiz (amarillo|blanco|pira)|chocolo)",       "01110600",
  "^carne de res",                                                         "01120100",
  "^carne de cerdo, tocineta",                                             "01120500",
  "^carne de cerdo",                                                       "01120200",
  "pollo|rabadilla|menudencias|^pierna pernil|^piernas",                   "01120300",
  paste0("^(almejas|bagre|basa|bocachico|cachama|calamar|corvina|lomitos ",
         "de atun|palmitos de mar|pargo|pescado|robalo|salmon|sierra|",
         "tilapia|trucha|camaron|langostino|cazuela de mariscos|sardinas)"), "01130300",
  "^(huevo|blanquillo)",                                                   "01140500",
  "^leche en polvo",                                                       "01140100",
  "^queso",                                                                "01140200",
  "^aceite",                                                               "01150100",
  "^naranja",                                                              "01160500",
  "^banano",                                                               "01160600",
  "^tomate de arbol",                                                      "01160700",
  "^mora",                                                                 "01160800",
  paste0("^(aguacate|coco|borojo|ciruela|curuba|durazno|fresa|guanabana|",
         "guayaba|gulupa|higo|kiwi|limon|lulo|mandarina|mango|manzana|",
         "maracuya|melon|papaya|patilla|pera|pitahaya|tangelo|uchuva|uva)"), "01160900",
  "^(frijol (bolon|cabeza|calima|cargamanto)|garbanzo|arveja amarilla)",   "01170200",
  "^(frijol enlatado|arveja enlatada)",                                    "01170300",
  "^platano",                                                              "01170400",
  "^papa",                                                                 "01170500",
  "^yuca",                                                                 "01170600",
  "^(arracacha|ulluco)",                                                   "01171200",
  "^tomate (chonto|larga)",                                                "01171300",
  "^(cebolla|cebollin)",                                                   "01171400",
  "^zanahoria",                                                            "01171500",
  "^(ajo|cilantro)",                                                       "01190800",
  paste0("^(acelga|ahuyama|aji|apio|arveja verde|berenjena|brocoli|",
         "calabacin|cidra|coles|coliflor|espinaca|frijol verde|habichuela|",
         "lechuga|pepino|perejil|pimenton|remolacha|repollo|rabano)"),     "01171600",
  "^azucar",                                                               "01180100",
  "^panela",                                                               "01180200",
  "^sal ",                                                                 "01190200",
  "^cafe",                                                                 "01210100"
)

normalizar <- function(x) stri_trans_tolower(stri_trans_general(x, "Latin-ASCII"))

# -----------------------------------------------------------------------
# Alimentos del panel y subclases del IPC
# -----------------------------------------------------------------------

alimentos <- readRDS(file.path(output_dir, "paneles/panel_mensual_cities_tcac.rds")) %>%
  distinct(articulo) %>%
  mutate(nombre_norm = normalizar(articulo))

ponderaciones <- read_excel(file.path(ipc_dir, "cpi-weights-2024.xlsx"),
                            .name_repair = "minimal")
names(ponderaciones) <- c("nivel", "codigo", "periodicidad", "nombre",
                          "caracteristicas", "total_ingresos", "pobres",
                          "vulnerables", "clase_media", "ingresos_altos")

subclases <- ponderaciones %>%
  filter(nivel == "Subclase", grepl("^01[12]", codigo)) %>%
  transmute(subclase_codigo = codigo, subclase_nombre = nombre,
            ponderacion = as.numeric(total_ingresos))

asignar <- function(nombre) {
  hit <- which(vapply(reglas_ipc$patron, grepl, logical(1), x = nombre))
  if (length(hit) == 0) return(NA_integer_)
  hit[1]
}

mapeo <- alimentos %>%
  mutate(regla = map_int(nombre_norm, asignar),
         subclase_codigo = reglas_ipc$codigo[regla],
         patron = reglas_ipc$patron[regla]) %>%
  left_join(subclases, by = "subclase_codigo")

sin_subclase <- mapeo %>% filter(is.na(subclase_codigo) | is.na(subclase_nombre))
if (nrow(sin_subclase) > 0) {
  stop("Alimentos sin subclase del IPC: ",
       paste(sin_subclase$articulo, collapse = ", "))
}

# -----------------------------------------------------------------------
# Participaciones: la ponderacion de cada subclase con alimentos en el
# panel, reescalada para que sumen 1
# -----------------------------------------------------------------------

participaciones <- mapeo %>%
  distinct(subclase_codigo, subclase_nombre, ponderacion) %>%
  mutate(share = ponderacion / sum(ponderacion)) %>%
  arrange(desc(share))

stopifnot(abs(sum(participaciones$share) - 1) < 1e-9)

message(sprintf("  %d alimentos | %d subclases del IPC | cubren %.1f%% de la ponderacion de alimentos",
                nrow(mapeo), nrow(participaciones),
                100 * sum(participaciones$ponderacion) /
                  sum(filter(subclases, grepl("^011", subclase_codigo))$ponderacion)))

# -----------------------------------------------------------------------
# Save
# -----------------------------------------------------------------------

write_csv(mapeo %>% select(articulo, subclase_codigo, subclase_nombre, patron),
          file.path(salida_dir, "mapeo_ipc_subclase.csv"))
write_csv(participaciones, file.path(salida_dir, "ipc_participaciones.csv"))
saveRDS(participaciones, file.path(salida_dir, "ipc_participaciones.rds"))

message("Done.")
