########################################################
## 02_build_price_nutr_panel_1999_2025_quarterly.R
## Merge extended prices + tcac nutrition + edible portion,
## for 3 cities, full QUARTERLY range up to last IPC month.
########################################################

source("working-papers\\working-paper-ipc\\least-cost-metrics\\00_config.R")

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

# 1) Load extended prices (from IPC)
prices_ext <- readRDS(in_prices_ext)

stopifnot(all(c("ciudad","articulo","fecha","precio_final") %in% names(prices_ext)))

prices_ext <- prices_ext %>%
  mutate(
    fecha = as.Date(fecha),
    articulo = as.character(articulo),
    ciudad = as.character(ciudad)
  ) %>%
  filter(ciudad %in% cities_use)

message("Extended price date range: ",
        min(prices_ext$fecha, na.rm = TRUE), " to ",
        max(prices_ext$fecha, na.rm = TRUE))

# 2) Ensure codigo_articulo exists in price panel (IPC script might have dropped it)
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
    dplyr::left_join(dane_map, by = "articulo")
}

# 3) Load tcac master (must already include Group + Subgroup)
tcac_master <- readRDS(file.path(tmp_dir, "tcac_master.rds"))

# If tcac_master still has grupos_gabas/subgrupos_gabas, normalize names
if (all(c("grupos_gabas","subgrupos_gabas") %in% names(tcac_master)) &&
    !all(c("Group","Subgroup") %in% names(tcac_master))) {
  tcac_master <- tcac_master %>%
    rename(Group = grupos_gabas, Subgroup = subgrupos_gabas)
}

# Sanity: ensure expected columns exist
need_tcac_cols <- c("codigo_articulo","articulo","parte_comestible_percent","Group","Subgroup")
miss_tcac <- setdiff(need_tcac_cols, names(tcac_master))
if (length(miss_tcac) > 0) {
  stop("tcac_master missing required columns: ", paste(miss_tcac, collapse = ", "))
}

# 4) Merge prices + tcac
panel <- prices_ext %>%
  mutate(codigo_articulo = suppressWarnings(as.numeric(codigo_articulo))) %>%
  left_join(tcac_master, by = c("codigo_articulo", "articulo"))

# Diagnostics on nutrition merge
diag <- panel %>%
  summarise(
    pct_missing_pc = mean(is.na(parte_comestible_percent)) * 100,
    pct_missing_energy = if ("energia_kcal" %in% names(panel)) mean(is.na(energia_kcal)) * 100 else NA_real_
  )

message("Missing parte_comestible_percent (%): ", round(diag$pct_missing_pc, 2))
if (!is.na(diag$pct_missing_energy)) {
  message("Missing energia_kcal (%): ", round(diag$pct_missing_energy, 2))
}

# 5) Build price variables:
# precio_final is per 500g (from your retail normalization).
# Convert to 100g: /5
# Adjust to edible portion: divide by pc (pc is either 0-1 or 0-100)
panel <- panel %>%
  mutate(
    precio_500g = precio_final,
    pc_raw = parte_comestible_percent,
    pc = case_when(
      is.na(pc_raw) ~ NA_real_,
      pc_raw > 1 ~ pc_raw / 100,   # if percent (e.g., 80)
      TRUE ~ pc_raw               # if already proportion (e.g., 0.80)
    ),
    precio_100g = (precio_500g / 5) / pc,
    ano = year(fecha),
    trimestre = quarter(fecha),
    # representative quarter date (start of quarter)
    q_date = as.Date(paste0(ano, "-", (trimestre - 1) * 3 + 1, "-01"))
  )

# Optional: exclude ultraprocessed / condiments / composites (your list)
alimentos_excluir <- c(
  # --- AZUCARES (cambio metodológico CoRD)
  "AZÚCAR NATURAL O MORENA",
  "AZÚCAR REFINADA",
  
  # --- Lista original
  "AREPAS  PRECOCIDAS", "AREPAS RELLENAS CON ALGO", "BOCADILLOS",
  "CEREAL ALIMENTO PARA BEBÉ", "CEREAL PARA DESAYUNO", "CHOCOLATE INSTANTANEO",
  "CHORIZO", "GALLETAS DE SAL", "GALLETAS DULCES", "GALLETAS INTEGRALES",
  "GASEOSAS", "GELATINA O FLAN", "HARINA PARA TORTAS", "HELADOS DE CREMA",
  "JAMÓN", "JUGOS INSTANTANEOS O EN POLVO", "JUGOS PROCESADOS",
  "MALTAS", "MARGARINA", "MAYONESA", "MERMELADA", "MORTADELA",
  "PIZZA", "SALCHICHAS","SALCHICHÓN", "SALSA DE TOMATE",
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

# 6) Collapse duplicates per city-quarter-food (keep Group/Subgroup)
panel <- panel %>%
  group_by(ciudad, ano, trimestre, q_date, articulo, codigo_articulo) %>%
  dplyr::summarise(
    # Keep GABAS classifications
    Group = first_non_na(Group),
    Subgroup = first_non_na(Subgroup),
    
    # Price variables
    precio_100g = mean(precio_100g, na.rm = TRUE),
    precio_500g = mean(precio_500g, na.rm = TRUE),
    pc = mean(pc, na.rm = TRUE),
    
    # All other numeric columns (nutrients etc.)
    across(
      where(is.numeric) & !any_of(c("precio_100g","precio_500g","pc")),
      ~ mean(.x, na.rm = TRUE)
    ),
    .groups = "drop"
  )

# Optional: order
panel <- panel %>%
  arrange(ciudad, ano, trimestre, articulo)

saveRDS(panel, file.path(tmp_dir, "panel_city_quarter_food_1999_2025.rds"))
write_csv(panel, file.path(tmp_dir, "panel_city_quarter_food_1999_2025.csv"))

message("Saved quarterly panel to tmp_dir (incluye Group/Subgroup).")