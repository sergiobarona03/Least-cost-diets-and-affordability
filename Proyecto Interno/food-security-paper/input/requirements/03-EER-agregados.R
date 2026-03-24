
########################################################
## Procesamiento de los requerimientos energéticos
## agregados (niñxs y adultos)
########################################################

# Librerías
library(tidyverse)
library(readxl)

# Directorio base
base_dir = "C:\\Users\\Portatil\\Desktop\\Least-cost-diets-and-affordability\\Proyecto Interno"

# Cargar archivos
out_eer = file.path(base_dir,
                    "food-security-paper\\output\\eer\\")

# Cargar EER de niños
child_eer = readxl::read_excel(paste0(out_eer, 
                                      "\\220326_child_eer.xlsx"))
child_eer$sex[child_eer$sex == "female"] = "Femenino"
child_eer$sex[child_eer$sex == "male"] = "Masculino"

# Cargar EER de adultos
adult_eer = readxl::read_excel(paste0(out_eer, 
                                      "\\220326_adult_eer.xlsx")) %>%
  filter(grupo_edad  != "[14,18)")

adult_eer$cod_mun = adult_eer$dominio_geo
adult_eer$cod_mun[adult_eer$cod_mun == "Bogotá"] = "11001"
adult_eer$cod_mun[adult_eer$cod_mun == "Cali"] = "76001"
adult_eer$cod_mun[adult_eer$cod_mun == "Medellín"] = "05001"
adult_eer$cod_mun[adult_eer$cod_mun == "Colombia"] = "Nacional"

adult_eer = adult_eer %>% mutate(
  rango = grupo_edad,
  eer = eer_kcal_dia,
  sex = sexo,
) %>% select(cod_mun, sex, rango, eer)

# Unir child_eer y adult_eer
agg_eer = bind_rows(child_eer, adult_eer)
writexl::write_xlsx(agg_eer, 
                    paste0(out_eer, "\\220326_agg_eer.xlsx"))

# Preparar apéndice
# Pivot a wide
agg_eer_wide <- agg_eer %>%
  mutate(
    cod_mun = case_when(
      cod_mun == "Nacional" ~ "National",
      cod_mun == "05001"    ~ "Medellín",
      cod_mun == "11001"    ~ "Bogotá",
      cod_mun == "76001"    ~ "Cali",
      TRUE ~ cod_mun
    ),
    sex = case_when(
      sex == "Femenino"  ~ "Female",
      sex == "Masculino" ~ "Male",
      TRUE ~ sex
    )
  ) %>%
  pivot_wider(
    names_from  = c(sex, cod_mun),
    values_from = eer,
    names_glue  = "{sex}_{cod_mun}"
  ) %>%
  arrange(rango) %>%
  mutate(across(where(is.numeric), ~ round(.x, 1)))

# Nombres de columnas
agg_eer_wide = agg_eer_wide %>%
  select(c("rango", 
           "Female_Bogotá", "Female_Cali", 
           "Female_Medellín", "Female_National",
           "Male_Bogotá", "Male_Cali", 
           "Male_Medellín", "Male_National"))
col_names <- c(
  "Age group",
  "Bogotá", "Cali", "Medellín", "National",
  "Bogotá", "Cali", "Medellín", "National"
)

# Tabla LaTeX
latex_eer <- agg_eer_wide %>%
  kbl(
    format    = "latex",
    booktabs  = TRUE,
    col.names = col_names,
    align     = c("l", rep("r", 8)),
    caption   = "Weighted estimated energy requirements (EER) by age group, sex, and city. Values in kcal/day. Source: authors' calculations based on ENSIN (2015) and IOM (2023).",
    label     = "tab:appendix_eer",
    escape    = FALSE
  ) %>%
  kable_styling(
    latex_options = c("hold_position", "striped"),
    font_size     = 8
  ) %>%
  add_header_above(
    c(" " = 1,
      "Female" = 4,
      "Male"   = 4),
    bold = TRUE
  )

writeLines(
  as.character(latex_eer),
  file.path(out_eer, "appendix_B_eer.tex")
)

