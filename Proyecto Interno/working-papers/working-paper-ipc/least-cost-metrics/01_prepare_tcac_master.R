########################################################
## 01_prepare_tcac_master.R
## Build nutrient master from Copia_DANE_4_DIC_2025act.xlsx
########################################################

source("working-papers/working-paper-ipc/least-cost-metrics/00_config.R")

tcac_raw <- read_excel(in_tcac_map) %>%
  clean_names()

# In your code you used: rename(articulo = articulo_dane)
# Also the file includes codigo_articulo and parte_comestible_percent + nutrients
tcac <- tcac_raw %>%
  rename(articulo = articulo_dane) %>%
  mutate(
    codigo_articulo = suppressWarnings(as.numeric(codigo_articulo)),
    articulo = as.character(articulo)
  )

# Keep only needed columns (keep many nutrients; safe if extras exist)
keep_cols <- c(
  "codigo_articulo","articulo",
  "parte_comestible_percent",
  # energy + macro + micros (names from your earlier CoNA rename block)
  "energia_kcal","proteina_g","lipidos_g","carbohidratos_totales_g",
  "vitamina_c_mg","folatos_mcg","vitamina_a_er","tiamina_mg","riboflavina_mg",
  "niacina_mg","vitamina_b12_mcg","magnesio_mg","fosforo_mg","sodio_mg",
  "calcio_mg","hierro_mg","zinc_mg"
)

tcac <- tcac %>%
  select(any_of(keep_cols)) %>%
  distinct()

# If multiple rows per (codigo_articulo, articulo), collapse numeric columns by mean (safe)
tcac_master <- tcac %>%
  group_by(codigo_articulo, articulo) %>%
  summarise(
    across(where(is.numeric), ~ mean(.x, na.rm = TRUE)),
    .groups = "drop"
  )

saveRDS(tcac_master, file.path(tmp_dir, "tcac_master.rds"))
write_csv(tcac_master, file.path(tmp_dir, "tcac_master.csv"))

message("Saved tcac_master to tmp_dir.")
