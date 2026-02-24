########################################################
## 01_prepare_tcac_master.R
## Build nutrient master from Copia_DANE_4_DIC_2025act.xlsx
## + EXCLUDE AZUCARES from Grupos_GABAS
########################################################

source("working-papers/working-paper-ipc/least-cost-metrics/00_config.R")

tcac_raw <- read_excel(in_tcac_map) %>%
  clean_names()

# ------------------------------------------------------------
# FILTRO: excluir grupo AZUCARES (columna: grupos_gabas)
# ------------------------------------------------------------
if ("grupos_gabas" %in% names(tcac_raw)) {
  tcac_raw <- tcac_raw %>%
    filter(toupper(trimws(grupos_gabas)) != "AZUCARES")
} else {
  warning("No encuentro la columna 'grupos_gabas' en el Excel. Revisa el nombre tras clean_names().")
}

tcac <- tcac_raw %>%
  dplyr::rename(articulo = articulo_dane) %>%
  mutate(
    codigo_articulo = suppressWarnings(as.numeric(codigo_articulo)),
    articulo = as.character(articulo),
    grupos_gabas = as.character(grupos_gabas),
    subgrupos_gabas = as.character(subgrupos_gabas)
  )

# Keep only needed columns (keep many nutrients; safe if extras exist)
keep_cols <- c(
  "codigo_articulo","articulo",
  "parte_comestible_percent",
  # energy + macro + micros (names from your earlier CoNA rename block)
  "energia_kcal","proteina_g","lipidos_g","carbohidratos_totales_g",
  "vitamina_c_mg","folatos_mcg","vitamina_a_er","tiamina_mg","riboflavina_mg",
  "niacina_mg","vitamina_b12_mcg","magnesio_mg","fosforo_mg","sodio_mg",
  "calcio_mg","hierro_mg","zinc_mg",
  # group CoRD
  "grupos_gabas", "subgrupos_gabas"
)

tcac <- tcac %>%
  select(any_of(keep_cols)) %>%
  distinct()

# Helper: first non-NA value
first_non_na <- function(x) {
  x <- x[!is.na(x) & x != ""]
  if (length(x) == 0) return(NA_character_)
  x[1]
}

tcac_master <- tcac %>%
  group_by(codigo_articulo, articulo) %>%
  dplyr::summarise(
    # conservar categorías (texto)
    Group = first_non_na(grupos_gabas),
    Subgroup = first_non_na(subgrupos_gabas),
    
    # colapsar numéricas
    across(where(is.numeric), ~ mean(.x, na.rm = TRUE)),
    .groups = "drop"
  )

saveRDS(tcac_master, file.path(tmp_dir, "tcac_master.rds"))
write_csv(tcac_master, file.path(tmp_dir, "tcac_master.csv"))

message("Saved tcac_master (sin AZUCARES, con Group/Subgroup) to tmp_dir.")
