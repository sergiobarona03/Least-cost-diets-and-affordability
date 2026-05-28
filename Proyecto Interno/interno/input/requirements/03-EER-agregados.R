########################################################
## Procesamiento de los requerimientos energéticos
## agregados (niñxs y adultos) - CIUDADES
########################################################

library(tidyverse)
library(readxl)
library(writexl)
library(kableExtra)

# Directorio base
dirs <- c(
  "C:/Users/Portatil/Desktop/Least-cost-diets-and-affordability/Proyecto Interno",
  "C:/Users/danie/OneDrive/Escritorio/Least-cost-diets-and-affordability/Proyecto Interno"
)

base_dir <- dirs[dir.exists(dirs)][1]

if (is.na(base_dir)) {
  stop("Ninguno de los directorios existe")
}

# Carpeta EER
out_eer <- file.path(base_dir, "interno", "output", "eer")

# Mapa códigos / nombres de ciudades
map_ciudades <- tibble::tribble(
  ~dominio_geo,      ~cod_mun,    ~city_en,
  "Barranquilla",    "08001",     "Barranquilla",
  "Bogotá",          "11001",     "Bogotá",
  "Bucaramanga",     "68001",     "Bucaramanga",
  "Cali",            "76001",     "Cali",
  "Cartagena",       "13001",     "Cartagena",
  "Cúcuta",          "54001",     "Cúcuta",
  "Ibagué",          "73001",     "Ibagué",
  "Manizales",       "17001",     "Manizales",
  "Medellín",        "05001",     "Medellín",
  "Montería",        "23001",     "Montería",
  "Pasto",           "52001",     "Pasto",
  "Pereira",         "66001",     "Pereira",
  "Villavicencio",   "50001",     "Villavicencio",
  "Colombia",        "Nacional",  "National"
)

# ======================================================
# Cargar EER de niños
# ======================================================

child_eer <- readxl::read_excel(
  file.path(out_eer, "250526_child_eer.xlsx")
) %>%
  mutate(
    sex = case_when(
      sex == "female" ~ "Femenino",
      sex == "male"   ~ "Masculino",
      TRUE ~ sex
    ),
    cod_mun = as.character(cod_mun)
  )

# ======================================================
# Cargar EER de adultos
# ======================================================

adult_eer <- readxl::read_excel(
  file.path(out_eer, "250526_adult_eer.xlsx")
) %>%
  filter(grupo_edad != "[14,18)") %>%
  left_join(map_ciudades, by = "dominio_geo") %>%
  mutate(
    rango = grupo_edad,
    eer   = eer_kcal_dia,
    sex   = sexo
  ) %>%
  select(cod_mun, sex, rango, eer)

# ======================================================
# Unir niños y adultos
# ======================================================

agg_eer <- bind_rows(child_eer, adult_eer) %>%
  mutate(
    cod_mun = as.character(cod_mun),
    sex = case_when(
      sex == "female"    ~ "Femenino",
      sex == "male"      ~ "Masculino",
      sex == "Female"    ~ "Femenino",
      sex == "Male"      ~ "Masculino",
      TRUE ~ sex
    )
  )

writexl::write_xlsx(
  agg_eer,
  file.path(out_eer, "250526_agg_eer.xlsx")
)

# ======================================================
# Preparar apéndice
# ======================================================

agg_eer_wide <- agg_eer %>%
  left_join(
    map_ciudades %>% select(cod_mun, city_en),
    by = "cod_mun"
  ) %>%
  mutate(
    city_en = if_else(cod_mun == "Nacional", "National", city_en),
    sex = case_when(
      sex == "Femenino"  ~ "Female",
      sex == "Masculino" ~ "Male",
      TRUE ~ sex
    )
  ) %>%
  filter(!is.na(city_en)) %>%
  group_by(rango, sex, city_en) %>%
  summarise(
    eer = mean(eer, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    id_cols     = rango,
    names_from  = c(sex, city_en),
    values_from = eer,
    names_glue  = "{sex}_{city_en}"
  ) %>%
  arrange(rango) %>%
  mutate(across(where(is.numeric), ~ round(.x, 1)))

# Orden de columnas

ciudades_orden <- c(
  "Barranquilla", "Bogotá", "Bucaramanga", "Cali", "Cartagena",
  "Cúcuta", "Ibagué", "Manizales", "Medellín", "Montería",
  "Pasto", "Pereira", "Villavicencio", "National"
)

cols_female <- paste0("Female_", ciudades_orden)
cols_male   <- paste0("Male_", ciudades_orden)

cols_finales <- c(
  "rango",
  cols_female[cols_female %in% names(agg_eer_wide)],
  cols_male[cols_male %in% names(agg_eer_wide)]
)

agg_eer_wide <- agg_eer_wide %>%
  select(all_of(cols_finales))

# Nombres columnas 

col_names <- c(
  "Age group",
  ciudades_orden[cols_female %in% names(agg_eer_wide)],
  ciudades_orden[cols_male %in% names(agg_eer_wide)]
)

n_female <- sum(cols_female %in% names(agg_eer_wide))
n_male   <- sum(cols_male %in% names(agg_eer_wide))

# ======================================================
# Tabla LaTeX
# ======================================================

latex_eer <- agg_eer_wide %>%
  kbl(
    format    = "latex",
    booktabs  = TRUE,
    col.names = col_names,
    align     = c("l", rep("r", ncol(agg_eer_wide) - 1)),
    caption   = "Weighted estimated energy requirements (EER) by age group, sex, and city. Values in kcal/day. Source: authors' calculations based on ENSIN (2015) and IOM (2023).",
    label     = "tab:appendix_eer",
    escape    = FALSE
  ) %>%
  kable_styling(
    latex_options = c("hold_position", "striped", "scale_down"),
    font_size     = 6
  ) %>%
  add_header_above(
    c(" " = 1, "Female" = n_female, "Male" = n_male),
    bold = TRUE
  )

writeLines(
  as.character(latex_eer),
  file.path(out_eer, "appendix_B_eer.tex")
)

agg_eer_wide
