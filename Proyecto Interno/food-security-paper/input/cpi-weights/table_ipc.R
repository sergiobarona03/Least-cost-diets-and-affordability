library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(janitor)
library(writexl)
library(kableExtra)
library(tibble)

# ============================================================
# 1) Rutas
# ============================================================
ipc_path <- "C:/Users/danie/OneDrive/Escritorio/Least-cost-diets-and-affordability/Proyecto Interno/food-security-paper/input/cpi-weights/cpi-weights-2024.xlsx"

appendix_path <- "C:/Users/danie/OneDrive/Escritorio/Least-cost-diets-and-affordability/Proyecto Interno/food-security-paper/output/tcac_food_table/appendix_S1_food_items.xlsx"

out_dir <- "C:/Users/danie/OneDrive/Escritorio/Least-cost-diets-and-affordability/Proyecto Interno/food-security-paper/output/tables"

# ============================================================
# 2) Función para normalizar texto
# ============================================================
norm_txt <- function(x) {
  x %>%
    as.character() %>%
    str_squish() %>%
    iconv(from = "", to = "ASCII//TRANSLIT") %>%
    str_to_lower()
}

# ============================================================
# 3) Leer Nivel, Código y Nombre del primer Excel
# ============================================================
ipc_raw <- read_excel(
  ipc_path,
  col_types = "text"
) %>%
  select(1, 2, 4) %>%
  setNames(c("nivel", "codigo", "nombre")) %>%
  mutate(
    nivel  = norm_txt(nivel),
    codigo = str_pad(str_extract(codigo, "\\d+"), width = 8, side = "left", pad = "0"),
    nombre = str_squish(nombre)
  )

# ============================================================
# 4) Construir jerarquía arrastrando hacia abajo
# ============================================================
ipc_hierarchy <- ipc_raw %>%
  mutate(
    division = if_else(nivel == "division", nombre, NA_character_),
    grupo    = if_else(nivel == "grupo", nombre, NA_character_),
    clase    = if_else(nivel == "clase", nombre, NA_character_),
    subclase = if_else(nivel == "subclase", nombre, NA_character_)
  ) %>%
  fill(division, grupo, clase, subclase, .direction = "down")

# ============================================================
# 5) Crear diccionario de artículos
# ============================================================
ipc_articulos <- ipc_hierarchy %>%
  filter(nivel == "articulo") %>%
  transmute(
    Item         = str_to_upper(str_squish(nombre)),
    COICOP       = codigo,
    subclase_ipc = str_sub(codigo, 1, 6),
    Division     = division,
    Group        = grupo,
    Class        = clase,
    Subclass     = subclase
  ) %>%
  distinct()

# ============================================================
# 6) Leer segundo Excel
# ============================================================
appendix <- read_excel(
  appendix_path,
  col_types = "text"
) %>%
  clean_names() %>%
  transmute(
    Item         = str_to_upper(str_squish(articulo)),
    subclase_ipc = str_pad(str_extract(subclase_ipc, "\\d+"), width = 6, side = "left", pad = "0")
  ) %>%
  distinct()

# ============================================================
# 7) Base final
# ============================================================
base_final <- appendix %>%
  left_join(ipc_articulos, by = c("Item", "subclase_ipc")) %>%
  select(Division, Group, Class, Subclass, Item, COICOP) %>%
  distinct() %>%
  arrange(Division, Group, Class, Subclass, Item)

# ============================================================
# 8) Revisar no match
# ============================================================
no_match <- appendix %>%
  left_join(ipc_articulos, by = c("Item", "subclase_ipc")) %>%
  filter(is.na(COICOP)) %>%
  distinct(subclase_ipc, Item)

print(no_match, n = 100)

# ============================================================
# 9) Guardar Excel completo
# ============================================================
write_xlsx(
  base_final,
  file.path(out_dir, "ipc_hierarchy_food_items.xlsx")
)

# ============================================================
# 10) Preparar tabla para LaTeX
# ============================================================
appendix_table <- base_final %>%
  filter(
    !is.na(Division),
    !is.na(Group),
    !is.na(Class),
    !is.na(Subclass),
    !is.na(Item),
    !is.na(COICOP)
  ) %>%
  distinct() %>%
  arrange(Division, Group, Class, Subclass, Item)

appendix_table_display <- appendix_table

col_names <- c(
  "Division", "Group", "Class", "Subclass", "Item", "COICOP"
)

print(length(col_names) == ncol(appendix_table_display))

# ============================================================
# 11) Índice para agrupar por Class
# ============================================================
class_index <- appendix_table %>%
  dplyr::count(Class, name = "n") %>%
  filter(!is.na(Class), n > 0) %>%
  tibble::deframe()

# ============================================================
# 12) Crear tabla LaTeX
# ============================================================
latex_table <- appendix_table_display %>%
  kbl(
    format    = "latex",
    booktabs  = TRUE,
    longtable = TRUE,
    col.names = col_names,
    align     = c("l", "l", "l", "l", "l", "c"),
    caption   = "Food items classified according to the CPI hierarchy.",
    label     = "tab:ipc_hierarchy_food_items",
    escape    = FALSE
  ) %>%
  kable_styling(
    latex_options = c("repeat_header", "striped"),
    font_size     = 7
  ) %>%
  add_header_above(
    c("CPI hierarchy" = 6),
    bold   = TRUE,
    escape = FALSE
  )

if (length(class_index) > 0) {
  latex_table <- latex_table %>%
    pack_rows(
      index           = class_index,
      bold            = TRUE,
      latex_gap_space = "0.5em"
    )
}

# ============================================================
# 13) Guardar .tex
# ============================================================
writeLines(
  c(
    "\\begingroup\\fontsize{7}{9}\\selectfont",
    as.character(latex_table),
    "\\endgroup"
  ),
  file.path(out_dir, "ipc_hierarchy_food_items.tex")
)