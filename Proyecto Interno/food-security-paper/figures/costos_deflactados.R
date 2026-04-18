# =========================
# DEFLACTAR COSTOS
# =========================

# Librerías
library(dplyr)
library(readxl)
library(openxlsx)
library(stringi)


# Directorios base
dirs <- c(
  "C:/Users/Portatil/Desktop/Least-cost-diets-and-affordability/Proyecto Interno",
  "C:/Users/danie/OneDrive/Escritorio/Least-cost-diets-and-affordability/Proyecto Interno"
)

base_dir <- dirs[dir.exists(dirs)][1]

if (is.na(base_dir)) {
  stop("Ninguno de los directorios existe")
}

ipc_path  <- file.path(base_dir, "food-security-paper", "input", "prices","IPC.xls")

coca_path <- file.path(base_dir, "food-security-paper", "output", "coca", "230326_coca_results.xlsx")
cona_path <- file.path(base_dir, "food-security-paper", "output", "cona", "230326_cona_results.xlsx")
cord_path <- file.path(base_dir, "food-security-paper", "output", "cord", "230326_cord_full.xlsx")

output_path <- file.path(base_dir, "output", "real")
dir.create(output_path, recursive = TRUE, showWarnings = FALSE)

# Funciones
meses_es <- c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic")

normalizar_ciudad <- function(x) {
  x <- stringi::stri_trans_general(as.character(x), "Latin-ASCII")
  x <- toupper(trimws(x))
  
  case_when(
    grepl("BOGOTA", x)   ~ "BOGOTA",
    grepl("MEDELLIN", x) ~ "MEDELLIN",
    grepl("CALI", x)     ~ "CALI",
    TRUE ~ x
  )
}

# IPC
ipc <- read_excel(ipc_path) %>%
  dplyr::mutate(
    ciudad = normalizar_ciudad(Ciudad),
    fecha = as.Date(paste(Año, match(Mes, meses_es), "01", sep = "-"))
  ) %>%
  dplyr::select(ciudad, fecha, ipc = `Número Índice`) %>%
  distinct()

ipc_base <- ipc %>%
  dplyr::filter(fecha == as.Date("2018-12-01")) %>%
  dplyr::select(ciudad, ipc_base = ipc)

# Deflactar costos
deflactar_costos <- function(df) {
  df %>%
    mutate(
      ciudad = normalizar_ciudad(ciudad),
      fecha = as.Date(fecha)
    ) %>%
    left_join(ipc, by = c("ciudad", "fecha")) %>%
    left_join(ipc_base, by = "ciudad") %>%
    mutate(
      cost_day_real = cost_day / ipc * ipc_base,
      Cost_1000kcal_real = Cost_1000kcal / ipc * ipc_base
    )
}

# COCA
coca <- read.xlsx(coca_path)

coca_real <- deflactar_costos(coca)

write.xlsx(
  coca_real,
  file.path(output_path, "coca_real.xlsx"),
  overwrite = TRUE
  )

# CONA
cona_cost <- read.xlsx(cona_path)
cona_cost_real <- deflactar_costos(cona_cost)

write.xlsx(
  cona_cost_real,
  file.path(output_path, "cona_real.xlsx"),
  overwrite = TRUE
  )

# CORD
cord_cost <- read.xlsx(cord_path)
cord_cost_real <- deflactar_costos(cord_cost)

write.xlsx(
  cord_cost_real,
  file.path(output_path, "cord_real.xlsx"),
  overwrite = TRUE
  )
