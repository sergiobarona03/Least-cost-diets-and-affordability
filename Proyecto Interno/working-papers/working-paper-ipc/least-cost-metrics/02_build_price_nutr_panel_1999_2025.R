########################################################
## 02_build_price_nutr_panel_1999_2025.R
## Merge extended prices + tcac nutrition + edible portion,
## for 3 cities, full monthly range up to last IPC month.
########################################################

source("working-papers/working-paper-ipc/least-cost-metrics/00_config.R")

suppressPackageStartupMessages({
  library(tidyverse)
  library(readxl)
  library(janitor)
  library(lubridate)
})

# ------------------------------------------------------------
# Helper: first non-missing character value
# ------------------------------------------------------------
first_non_na <- function(x) {
  x <- x[!is.na(x) & x != ""]
  if (length(x) == 0) return(NA_character_)
  x[1]
}

norm_city <- function(x) {
  x <- as.character(x)
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  x <- toupper(trimws(x))
  
  dplyr::case_when(
    x %in% c("BOGOTA D.C.") ~ "BOGOTA",
    x %in% c("MEDELLIN") ~ "MEDELLIN",
    x %in% c("CALI") ~ "CALI",
    TRUE ~ x
  )
}
# ------------------------------------------------------------
# 1) Load extended prices (from IPC)
# ------------------------------------------------------------
prices_ext <- readRDS(in_prices_ext)

stopifnot(all(c("ciudad", "articulo", "fecha", "precio_final") %in% names(prices_ext)))

cities_use_std <- c("CALI", "BOGOTA", "MEDELLIN")

prices_ext <- prices_ext %>%
  mutate(
    fecha = as.Date(fecha),
    articulo = as.character(articulo),
    ciudad = norm_city(ciudad)
  ) %>%
  filter(ciudad %in% cities_use_std)

message(
  "Extended price date range: ",
  min(prices_ext$fecha, na.rm = TRUE), " to ",
  max(prices_ext$fecha, na.rm = TRUE)
)

# ------------------------------------------------------------
# 2) Ensure codigo_articulo exists in price panel
# ------------------------------------------------------------
if (!("codigo_articulo" %in% names(prices_ext))) {
  
  message("prices_ext has no codigo_articulo. Recovering mapping from original DANE file...")
  
  dane_map <- read_excel(in_retail_99_18) %>%
    mutate(
      codigo_articulo = suppressWarnings(as.numeric(codigo_articulo)),
      articulo = as.character(articulo)
    ) %>%
    group_by(articulo) %>%
    dplyr::summarise(codigo_articulo = mode1(codigo_articulo), .groups = "drop")
  
  prices_ext <- prices_ext %>%
    left_join(dane_map, by = "articulo")
}

# ------------------------------------------------------------
# 3) Load tcac master
#    Debe incluir Group, Subgroup y gramos_g_1_intercambio_1_intercambio
# ------------------------------------------------------------
tcac_master <- readRDS(file.path(tmp_dir, "tcac_master.rds"))

# Si tcac_master todavía trae grupos_gabas/subgrupos_gabas, normalizar nombres
if (all(c("grupos_gabas", "subgrupos_gabas") %in% names(tcac_master)) &&
    !all(c("Group", "Subgroup") %in% names(tcac_master))) {
  tcac_master <- tcac_master %>%
    rename(
      Group = grupos_gabas,
      Subgroup = subgrupos_gabas
    )
}

# Sanity: ensure expected columns exist
need_tcac_cols <- c(
  "codigo_articulo", "articulo",
  "parte_comestible_percent",
  "Group", "Subgroup",
  "gramos_g_1_intercambio_1_intercambio"
)

miss_tcac <- setdiff(need_tcac_cols, names(tcac_master))
if (length(miss_tcac) > 0) {
  stop("tcac_master missing required columns: ", paste(miss_tcac, collapse = ", "))
}

# ------------------------------------------------------------
# 4) Merge prices + tcac
# ------------------------------------------------------------
panel <- prices_ext %>%
  mutate(codigo_articulo = suppressWarnings(as.numeric(codigo_articulo))) %>%
  left_join(tcac_master, by = c("codigo_articulo", "articulo"))

# ------------------------------------------------------------
# 4.1) Diagnostics on nutrition merge
# ------------------------------------------------------------
diag <- panel %>%
  dplyr::summarise(
    pct_missing_pc = mean(is.na(parte_comestible_percent)) * 100,
    pct_missing_energy = if ("energia_kcal" %in% names(panel)) mean(is.na(energia_kcal)) * 100 else NA_real_,
    pct_missing_serving = if ("gramos_g_1_intercambio_1_intercambio" %in% names(panel)) {
      mean(is.na(gramos_g_1_intercambio_1_intercambio)) * 100
    } else NA_real_
  )

message("Missing parte_comestible_percent (%): ", round(diag$pct_missing_pc, 2))
if (!is.na(diag$pct_missing_energy)) {
  message("Missing energia_kcal (%): ", round(diag$pct_missing_energy, 2))
}
if (!is.na(diag$pct_missing_serving)) {
  message("Missing gramos_g_1_intercambio_1_intercambio (%): ", round(diag$pct_missing_serving, 2))
}

# ------------------------------------------------------------
# 5) Build price variables
# precio_final is per 500g
# Convert to 100g: /5
# Adjust to edible portion: divide by pc
# ------------------------------------------------------------
panel <- panel %>%
  mutate(
    precio_500g = precio_final,
    pc_raw = parte_comestible_percent,
    pc = case_when(
      is.na(pc_raw) ~ NA_real_,
      pc_raw > 1 ~ pc_raw / 100,
      TRUE ~ pc_raw
    ),
    precio_100g = (precio_500g / 5) / pc,
    ano = year(fecha),
    mes_num = month(fecha),
    
    # importante: asegurar numérica la columna de intercambio
    gramos_g_1_intercambio_1_intercambio =
      suppressWarnings(as.numeric(gramos_g_1_intercambio_1_intercambio))
  )

# ------------------------------------------------------------
# Optional: exclude ultraprocessed / condiments / composites
# ------------------------------------------------------------
alimentos_excluir <- c(
  "AREPAS  PRECOCIDAS", "AREPAS RELLENAS CON ALGO", "BOCADILLOS",
  "CEREAL ALIMENTO PARA BEBÉ", "CEREAL PARA DESAYUNO", "CHOCOLATE INSTANTANEO",
  "CHORIZO", "GALLETAS DE SAL", "GALLETAS DULCES", "GALLETAS INTEGRALES",
  "GASEOSAS", "GELATINA O FLAN", "HARINA PARA TORTAS", "HELADOS DE CREMA",
  "JAMÓN", "JUGOS INSTANTANEOS O EN POLVO", "JUGOS PROCESADOS",
  "MALTAS", "MARGARINA", "MAYONESA", "MERMELADA", "MORTADELA",
  "PIZZA", "SALCHICHAS", "SALCHICHÓN", "SALSA DE TOMATE",
  "SOPAS", "YOGOURT", "CREMA DE LECHE", "PAPAS FRITAS",
  "CILANTRO", "COLOR", "COMINOS", "LAUREL", "MOSTAZA",
  "PIMIENTA", "TOMILLO", "REVUELTO VERDE",
  "ALMUERZO CORRIENTE O EJECUTIVO", "ALMUERZO ESPECIAL O A LA CARTA",
  "CHOCOLATE EN PASTA", "CAFÉ INSTANTANEO", "CAFÉ MOLIDO", "COMBOS",
  "CREMAS", "ENSALADA  DE FRUTAS", "QUESO CREMA", "TINTO",
  "HAMBURGUESA", "KUMIS", "JUGOS NATURALES", "SUERO"
)

panel <- panel %>%
  filter(!articulo %in% alimentos_excluir)

# ------------------------------------------------------------
# 6) Collapse duplicates per city-month-food
# ------------------------------------------------------------
panel <- panel %>%
  group_by(ciudad, fecha, ano, mes_num, articulo, codigo_articulo) %>%
  dplyr::summarise(
    Group = first_non_na(Group),
    Subgroup = first_non_na(Subgroup),
    
    precio_100g = mean(precio_100g, na.rm = TRUE),
    precio_500g = mean(precio_500g, na.rm = TRUE),
    pc = mean(pc, na.rm = TRUE),
    
    across(
      where(is.numeric) & !any_of(c("precio_100g", "precio_500g", "pc")),
      ~ mean(.x, na.rm = TRUE)
    ),
    .groups = "drop"
  )

# ------------------------------------------------------------
# 7) Save outputs
# ------------------------------------------------------------
saveRDS(panel, file.path(tmp_dir, "panel_city_month_food_1999_2025.rds"))
write_csv(panel, file.path(tmp_dir, "panel_city_month_food_1999_2025.csv"))