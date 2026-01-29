########################################################
## 01_build_tcac_master.R
## Construye tcac_master (nutrientes + parte comestible)
########################################################

source("working-papers/working-paper-q2/least-cost-metrics/00_config_q2.R")

suppressPackageStartupMessages({
  library(janitor)
})

# -----------------------------
# 1) Leer TCAC raw
# -----------------------------
tcac_raw <- read_excel(in_tcac_map) %>% clean_names()

# -----------------------------
# 2) Normalizar nombres clave
# -----------------------------
tcac_master <- tcac_raw %>%
  rename(alimento_sipsa = alimento_nombre_sipsa) %>%
  mutate(
    alimento_sipsa = as.character(alimento_sipsa)
  )

# -----------------------------
# 3) Seleccionar columnas necesarias (si existen)
# -----------------------------
keep_cols <- c(
  "codigo_tcac", "alimento_sipsa",
  "parte_comestible_percent",
  "energia_kcal", "proteina_g", "lipidos_g", "carbohidratos_totales_g",
  "vitamina_c_mg", "folatos_mcg", "vitamina_a_er", "tiamina_mg", "riboflavina_mg",
  "niacina_mg", "vitamina_b12_mcg", "magnesio_mg", "fosforo_mg", "sodio_mg",
  "calcio_mg", "hierro_mg", "zinc_mg"
)

tcac_master <- tcac_master %>%
  select(any_of(keep_cols)) %>%
  distinct()

# -----------------------------
# 4) Guardar
# -----------------------------
out_rds <- file.path(tcac_dir, "tcac_master.rds")
out_csv <- file.path(tcac_dir, "tcac_master.csv")

saveRDS(tcac_master, out_rds)
write_csv(tcac_master, out_csv)

message("Saved tcac_master:")
message(" - ", out_rds)
message(" - ", out_csv)
