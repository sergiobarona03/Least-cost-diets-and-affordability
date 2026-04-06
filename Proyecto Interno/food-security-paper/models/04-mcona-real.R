########################################################
## Deflactar costos CoNA-IPC
## Misma metodología que CoCA, CoNA y CoRD
## Base: diciembre 2018
########################################################

library(dplyr)
library(readxl)
library(openxlsx)
library(stringi)
library(lubridate)

##----------------------------------------------------------
## Directorios
##----------------------------------------------------------

base_dir    <- "C:\\Users\\sergio.barona\\Desktop\\Least-cost-diets-and-affordability\\Proyecto Interno\\food-security-paper"

ipc_path    <- file.path(base_dir, "input", "prices", "IPC.xls")
ipc_in_path <- file.path(base_dir, "output", "cona-ipc",
                         "230326_cona_ipc_full.rds")
out_real    <- file.path(base_dir, "output", "real")

##----------------------------------------------------------
## Funciones auxiliares
##----------------------------------------------------------

meses_es <- c("Ene","Feb","Mar","Abr","May","Jun",
              "Jul","Ago","Sep","Oct","Nov","Dic")

normalizar_ciudad <- function(x) {
  x <- stringi::stri_trans_general(as.character(x), "Latin-ASCII")
  x <- toupper(trimws(x))
  case_when(
    grepl("BOGOTA",   x) ~ "BOGOTA",
    grepl("MEDELLIN", x) ~ "MEDELLIN",
    grepl("CALI",     x) ~ "CALI",
    TRUE ~ x
  )
}

##----------------------------------------------------------
## IPC — índice de precios al consumidor
##----------------------------------------------------------

ipc <- read_excel(ipc_path) %>%
  mutate(
    ciudad = normalizar_ciudad(Ciudad),
    fecha  = as.Date(paste(Año, match(Mes, meses_es), "01", sep = "-"))
  ) %>%
  select(ciudad, fecha, ipc = `Número Índice`) %>%
  distinct()

# Base: diciembre 2018
ipc_base <- ipc %>%
  filter(fecha == as.Date("2018-12-01")) %>%
  select(ciudad, ipc_base = ipc)

##----------------------------------------------------------
## Función de deflactación
##----------------------------------------------------------

deflactar_costos <- function(df) {
  df %>%
    mutate(
      ciudad = normalizar_ciudad(ciudad),
      fecha  = as.Date(fecha)
    ) %>%
    left_join(ipc,      by = c("ciudad", "fecha")) %>%
    left_join(ipc_base, by = "ciudad") %>%
    mutate(
      cost_day_real      = cost_day      / ipc * ipc_base,
      Cost_1000kcal_real = Cost_1000kcal / ipc * ipc_base
    )
}

##----------------------------------------------------------
## Cargar CoNA-IPC y deflactar
##----------------------------------------------------------

ipc_full <- readRDS(ipc_in_path)

cona_ipc_cost <- ipc_full$cost %>%
  mutate(fecha = as.Date(fecha)) %>% distinct()

cona_ipc_real <- deflactar_costos(cona_ipc_cost)

##----------------------------------------------------------
## Per capita real cost: mean across 3 members, by city × date × alpha
##----------------------------------------------------------

cona_ipc_pc_real <- cona_ipc_real %>%
  group_by(ciudad, fecha, alpha_val) %>%
  summarize(
    hh_total_nom   = sum(cost_day,  na.rm = TRUE),
    hh_total_real   = sum(cost_day_real,  na.rm = TRUE),
    n_members  = n(),
    cost_pc_real = hh_total_real/ n_members,
    cost_pc_nominal    = hh_total_nom/n_members,
    Cost_1000kcal_real = mean(Cost_1000kcal_real, 
                              na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    year = year(fecha),
    mes  = month(fecha)
  )

##----------------------------------------------------------
## Guardar
##----------------------------------------------------------

# Archivo completo con todos los miembros (por si se necesita después)
library(writexl)
write.xlsx(
  cona_ipc_real,
  file.path(out_real, "cona_ipc_real_full.xlsx"),
  overwrite = TRUE
)

# Archivo per cápita — este es el que usan las figuras de la Sección 5
write.xlsx(
  cona_ipc_pc_real,
  file.path(out_real, "cona_ipc_real_percapita.xlsx"),
  overwrite = TRUE
)

saveRDS(
  list(full       = cona_ipc_real,
       per_capita = cona_ipc_pc_real),
  file.path(out_real, "cona_ipc_real.rds")
)

