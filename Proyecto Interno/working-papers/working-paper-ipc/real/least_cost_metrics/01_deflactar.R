# =========================
# DEFLACTAR COSTOS
# =========================

library(dplyr)
library(readxl)
library(openxlsx)
library(stringi)

# =========================
# 0. DIRECTORIO BASE
# =========================

base_dir <- "C:/Users/danie/OneDrive/Escritorio/Least-cost-diets-and-affordability/Proyecto Interno/working-papers/working-paper-ipc"

# =========================
# 1. RUTAS
# =========================

ipc_path    <- file.path(base_dir, "real", "IPC.xls")
input_path  <- file.path(base_dir, "output", "least_cost_metrics")
output_path <- file.path(base_dir, "output", "real", "least_cost_metrics")

dir.create(output_path, recursive = TRUE, showWarnings = FALSE)

# =========================
# 2. HELPERS
# =========================

meses_es <- c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic")

normalizar_ciudad <- function(x) {
  x <- as.character(x)
  x <- stringi::stri_trans_general(x, "Latin-ASCII")
  x <- toupper(trimws(x))
  x <- gsub("\\.", "", x)
  x <- gsub(",", "", x)
  x <- gsub("\\s+", " ", x)
  
  dplyr::case_when(
    x %in% c("BOGOTA", "BOGOTA DC", "BOGOTA D C", "BOGOTA D.C", "BOGOTA D.C.") ~ "BOGOTA",
    x %in% c("MEDELLIN") ~ "MEDELLIN",
    x %in% c("CALI") ~ "CALI",
    TRUE ~ x
  )
}

parse_fecha_base <- function(x) {
  as.Date(x, origin = "1970-01-01")
}

# =========================
# 3. IPC
# =========================

ipc <- read_excel(ipc_path)

ipc_all <- ipc %>%
  mutate(
    mes_num   = match(Mes, meses_es),
    fecha_ipc = as.Date(paste(Año, mes_num, "01", sep = "-")),
    ciudad_std = normalizar_ciudad(Ciudad)
  ) %>%
  select(ciudad_std, fecha_ipc, ipc = `Número Índice`) %>%
  distinct()

ipc_clean <- ipc_all %>%
  filter(fecha_ipc >= as.Date("2019-01-01"))

ipc_base <- ipc_all %>%
  filter(fecha_ipc == as.Date("2018-12-01")) %>%
  select(ciudad_std, ipc_base = ipc)

# =========================
# 4. FUNCIONES
# =========================

deflactar <- function(df) {
  df %>%
    mutate(
      ciudad_std = normalizar_ciudad(ciudad),
      fecha_aux  = parse_fecha_base(fecha)
    ) %>%
    filter(fecha_aux >= as.Date("2019-01-01")) %>%
    left_join(ipc_clean, by = c("ciudad_std", "fecha_aux" = "fecha_ipc")) %>%
    left_join(ipc_base,  by = "ciudad_std") %>%
    mutate(
      cost_day_real      = cost_day / ipc * ipc_base,
      Cost_1000kcal_real = Cost_1000kcal / ipc * ipc_base
    ) %>%
    select(-ciudad_std, -fecha_aux, -ipc_base)
}

limpiar_comp <- function(df) {
  df %>%
    mutate(
      ciudad_std = normalizar_ciudad(ciudad),
      fecha_aux  = parse_fecha_base(fecha)
    ) %>%
    filter(fecha_aux >= as.Date("2019-01-01")) %>%
    select(-ciudad_std, -fecha_aux)
}

# =========================
# 5. COCA
# =========================

coca <- readRDS(file.path(input_path, "coca_fullsample.rds"))
coca_real <- deflactar(coca)

saveRDS(coca_real, file.path(output_path, "coca_fullsample.rds"))

wb_coca <- createWorkbook()

addWorksheet(wb_coca, "coca")

writeData(wb_coca, "coca", coca_real)

saveWorkbook(
  wb_coca,
  file.path(output_path, "coca_fullsample_real.xlsx"),
  overwrite = TRUE
)

# =========================
# 6. CONA
# =========================

cona_cost <- readRDS(file.path(input_path, "cona_cost_fullsample.rds"))
cona_comp <- readRDS(file.path(input_path, "cona_comp_fullsample.rds"))

cona_cost_real <- deflactar(cona_cost)
cona_comp_real <- limpiar_comp(cona_comp)

saveRDS(cona_cost_real, file.path(output_path, "cona_cost_fullsample.rds"))
saveRDS(cona_comp_real, file.path(output_path, "cona_comp_fullsample.rds"))

wb_cona <- createWorkbook()
addWorksheet(wb_cona, "cost")
addWorksheet(wb_cona, "comp")
writeData(wb_cona, "cost", cona_cost_real)
writeData(wb_cona, "comp", cona_comp_real)

saveWorkbook(
  wb_cona,
  file.path(output_path, "cona_fullsample_real.xlsx"),
  overwrite = TRUE
)

# =========================
# 7. CORD
# =========================

cord_cost <- readRDS(file.path(input_path, "cord_cost_fullsample.rds"))
cord_comp <- readRDS(file.path(input_path, "cord_comp_fullsample.rds"))

cord_cost_real <- deflactar(cord_cost)
cord_comp_real <- limpiar_comp(cord_comp)

saveRDS(cord_cost_real, file.path(output_path, "cord_cost_fullsample.rds"))
saveRDS(cord_comp_real, file.path(output_path, "cord_comp_fullsample.rds"))

wb_cord <- createWorkbook()
addWorksheet(wb_cord, "cost")
addWorksheet(wb_cord, "comp")
writeData(wb_cord, "cost", cord_cost_real)
writeData(wb_cord, "comp", cord_comp_real)

saveWorkbook(
  wb_cord,
  file.path(output_path, "cord_fullsample_real.xlsx"),
  overwrite = TRUE
)
