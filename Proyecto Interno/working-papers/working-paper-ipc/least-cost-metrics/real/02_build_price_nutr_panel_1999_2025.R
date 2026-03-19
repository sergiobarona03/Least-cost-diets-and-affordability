########################################################
## 02_build_price_nutr_panel_1999_2025.R
## Merge extended REAL prices + tcac nutrition + edible portion,
## for 3 cities, full monthly range up to last IPC month.
########################################################

source("working-papers/working-paper-ipc/least-cost-metrics/real/00_config.R")

suppressPackageStartupMessages({
  library(tidyverse)
  library(readxl)
  library(janitor)
  library(lubridate)
})

# ------------------------------------------------------------
# Helpers
# ------------------------------------------------------------
first_non_na <- function(x) {
  x <- x[!is.na(x) & x != ""]
  if (length(x) == 0) return(NA_character_)
  x[1]
}

mode1 <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA)
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

safe_mean <- function(x) {
  if (all(is.na(x))) return(NA_real_)
  mean(x, na.rm = TRUE)
}

norm_city <- function(x) {
  x <- as.character(x)
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  x <- toupper(trimws(x))
  
  dplyr::case_when(
    x %in% c("BOGOTA D.C.") ~ "BOGOTA",
    x %in% c("BOGOTA") ~ "BOGOTA",
    x %in% c("MEDELLIN") ~ "MEDELLIN",
    x %in% c("CALI") ~ "CALI",
    TRUE ~ x
  )
}

# ------------------------------------------------------------
# 1) Load extended REAL prices panel
# ------------------------------------------------------------
prices_ext <- readRDS(in_prices_ext)

stopifnot(all(c("ciudad", "articulo", "fecha", "precio_final") %in% names(prices_ext)))

prices_ext <- prices_ext %>%
  dplyr::mutate(
    fecha = as.Date(fecha),
    articulo = as.character(articulo),
    ciudad = norm_city(ciudad)
  ) %>%
  dplyr::filter(ciudad %in% norm_city(cities_use))

message(
  "Extended real price date range: ",
  min(prices_ext$fecha, na.rm = TRUE), " to ",
  max(prices_ext$fecha, na.rm = TRUE)
)

# ------------------------------------------------------------
# 2) Ensure codigo_articulo exists in prices_ext
# ------------------------------------------------------------
if (!("codigo_articulo" %in% names(prices_ext))) {
  
  message("prices_ext has no codigo_articulo. Recovering mapping from original DANE file...")
  
  dane_map <- read_excel(in_retail_99_18) %>%
    clean_names() %>%
    dplyr::mutate(
      codigo_articulo = suppressWarnings(as.numeric(codigo_articulo)),
      articulo = as.character(articulo)
    ) %>%
    dplyr::group_by(articulo) %>%
    dplyr::summarise(codigo_articulo = mode1(codigo_articulo), .groups = "drop")
  
  prices_ext <- prices_ext %>%
    dplyr::left_join(dane_map, by = "articulo")
}

prices_ext <- prices_ext %>%
  dplyr::mutate(
    codigo_articulo = suppressWarnings(as.numeric(codigo_articulo))
  )

# ------------------------------------------------------------
# 3) Keep ONLY real prices
# precio_final in this object is already the extended REAL price
# ------------------------------------------------------------
prices_ext <- prices_ext %>%
  dplyr::mutate(
    precio_500g = suppressWarnings(as.numeric(precio_final))
  )

diag_price <- prices_ext %>%
  dplyr::summarise(
    n_total = n(),
    n_real = sum(!is.na(precio_500g)),
    pct_real = 100 * mean(!is.na(precio_500g))
  )

message("Rows with real precio_500g available (%): ", round(diag_price$pct_real, 2))

# ------------------------------------------------------------
# 4) Load tcac master
# ------------------------------------------------------------
tcac_master <- readRDS(file.path(tmp_dir, "tcac_master.rds"))

if (all(c("grupos_gabas", "subgrupos_gabas") %in% names(tcac_master)) &&
    !all(c("Group", "Subgroup") %in% names(tcac_master))) {
  tcac_master <- tcac_master %>%
    dplyr::rename(
      Group = grupos_gabas,
      Subgroup = subgrupos_gabas
    )
}

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
# 5) Merge prices + tcac
# ------------------------------------------------------------
panel <- prices_ext %>%
  dplyr::left_join(tcac_master, by = c("codigo_articulo", "articulo"))

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
# 6) Build price variables
# Only REAL prices
# ------------------------------------------------------------
panel <- panel %>%
  dplyr::mutate(
    pc_raw = parte_comestible_percent,
    pc = dplyr::case_when(
      is.na(pc_raw) ~ NA_real_,
      pc_raw > 1 ~ pc_raw / 100,
      TRUE ~ pc_raw
    ),
    precio_100g = (precio_500g / 5) / pc,
    ano = lubridate::year(fecha),
    mes_num = lubridate::month(fecha),
    gramos_g_1_intercambio_1_intercambio =
      suppressWarnings(as.numeric(gramos_g_1_intercambio_1_intercambio))
  )

# ------------------------------------------------------------
# 7) Exclude foods
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
  dplyr::filter(!articulo %in% alimentos_excluir)

# ------------------------------------------------------------
# 8) Collapse duplicates
# Only REAL price variables remain
# ------------------------------------------------------------
panel <- panel %>%
  dplyr::group_by(ciudad, fecha, ano, mes_num, articulo, codigo_articulo) %>%
  dplyr::summarise(
    Group = first_non_na(Group),
    Subgroup = first_non_na(Subgroup),
    
    precio_100g = safe_mean(precio_100g),
    precio_500g = safe_mean(precio_500g),
    pc = safe_mean(pc),
    
    across(
      where(is.numeric) & !any_of(c("precio_100g", "precio_500g", "pc")),
      safe_mean
    ),
    .groups = "drop"
  )

# ------------------------------------------------------------
# 9) Final diagnostics
# ------------------------------------------------------------
diag_final <- panel %>%
  dplyr::summarise(
    n = n(),
    pct_precio_500g = 100 * mean(!is.na(precio_500g)),
    pct_precio_100g = 100 * mean(!is.na(precio_100g)),
    pct_pc = 100 * mean(!is.na(pc))
  )

message("Final non-missing precio_500g (%): ", round(diag_final$pct_precio_500g, 2))
message("Final non-missing precio_100g (%): ", round(diag_final$pct_precio_100g, 2))
message("Final non-missing pc (%): ", round(diag_final$pct_pc, 2))

# ------------------------------------------------------------
# 10) Save outputs
# ------------------------------------------------------------
saveRDS(panel, file.path(tmp_dir, "panel_city_month_food_1999_2025.rds"))
write_csv(panel, file.path(tmp_dir, "panel_city_month_food_1999_2025.csv"))

message("Saved REAL panel to tmp_dir (incluye Group/Subgroup).")