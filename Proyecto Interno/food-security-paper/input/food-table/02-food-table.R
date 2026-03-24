
########################################################
## Construir el panel con la composición nutricional
## Merge extended prices + tcac nutrition + edible portion,
## for 3 cities, full monthly range up to last IPC month.
########################################################

# ------------------------------------------------------------
# 1. Directorio y librerías
# ------------------------------------------------------------
library(tidyverse)
library(readxl)

# Directorio base
base_dir <- "C:\\Users\\Portatil\\Desktop\\Least-cost-diets-and-affordability\\Proyecto Interno\\"

# Directorio de precios
price_dir   <- file.path(base_dir, "food-security-paper/output/forecasting_fullsample")
prices_extended = read_csv(file.path(price_dir, 
                                     "prices_extended_city_article_month.csv"))

# Output path
out_dir   <- file.path(base_dir, "food-security-paper/output/tcac_food_table")

# Cargar TCAC
tcac_master = read_csv(file.path(out_dir, "tcac_master.csv"))

# Original retail file
# Este archivo sólo sirve para recuperar el código de los artículos
in_retail_99_18 <- file.path(
  base_dir,
  "Precios DANE/OUTPUT_DANE/precios_unadj_DANE_1999_2018.xlsx"
)


# ------------------------------------------------------------
# 2. Funciones auxiliares
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

mode1 <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_character_)
  ux <- unique(x)
  
  if(length(ux) > 0){print("No hay código único")}
  
  return(ux[which.max(tabulate(match(x, ux)))])
}
# ------------------------------------------------------------
# 3. Cargar precios estimados
# ------------------------------------------------------------
prices_ext <- prices_extended %>%
  mutate(
    fecha = as.Date(fecha),
    articulo = as.character(articulo),
    ciudad = norm_city(ciudad)
  ) %>%
  filter(ciudad %in% cities_use_std)

# ------------------------------------------------------------
# 3) Leer TCAC y hacer merge (price-tcac)
#    Debe incluir Group, Subgroup y gramos_g_1_intercambio_1_intercambio
# ------------------------------------------------------------
tcac_master2 <- tcac_master %>%
  select(-c("codigo_articulo")) %>% distinct()

panel <- prices_ext %>%
  left_join(tcac_master2, by = c("articulo"))


# ------------------------------------------------------------
# 5) Variables de precios
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
  "AREPAS RELLENAS CON ALGO", "BOCADILLOS",
  "CEREAL ALIMENTO PARA BEBÉ", "CEREAL PARA DESAYUNO", "CHOCOLATE INSTANTANEO",
  "CHORIZO", "GALLETAS DE SAL", "GALLETAS DULCES", "GALLETAS INTEGRALES",
  "GASEOSAS", "GELATINA O FLAN", "HARINA PARA TORTAS", "HELADOS DE CREMA",
  "JAMÓN", "JUGOS INSTANTANEOS O EN POLVO", "JUGOS PROCESADOS",
  "MALTAS", "MAYONESA", "MERMELADA", "MORTADELA",
  "PIZZA", "SALCHICHAS", "SALCHICHÓN", "SALSA DE TOMATE",
  "SOPAS", "YOGOURT", "CREMA DE LECHE", "PAPAS FRITAS",
  "CILANTRO", "COLOR", "COMINOS", "LAUREL", "MOSTAZA",
  "PIMIENTA", "TOMILLO", "REVUELTO VERDE",
  "ALMUERZO CORRIENTE O EJECUTIVO", "ALMUERZO ESPECIAL O A LA CARTA",
  "CHOCOLATE EN PASTA", "CAFÉ INSTANTANEO", "CAFÉ MOLIDO", "COMBOS",
  "CREMAS", "TINTO",
  "HAMBURGUESA", "KUMIS", "JUGOS NATURALES", "SUERO"
)

panel <- panel %>%
  filter(!articulo %in% alimentos_excluir)

# ------------------------------------------------------------
# 7) Save outputs
# ------------------------------------------------------------
saveRDS(panel, file.path(out_dir, "panel_city_month_food_1999_2025.rds"))
write_csv(panel, file.path(out_dir, "panel_city_month_food_1999_2025.csv"))

# ------------------------------------------------------------
# 8) Organizar tabla para presentar la tabla
# ------------------------------------------------------------

appendix_table = panel %>%
  group_by(articulo) %>%
  slice(1) %>%
  ungroup() %>%
  select(
    articulo,
    subclase_ipc,
    grupos_gabas,
    subgrupos_gabas,
    parte_comestible_percent,
    energia_kcal,
    proteina_g,
    lipidos_g,
    carbohidratos_totales_g,
    calcio_mg,
    hierro_mg,
    zinc_mg,
    magnesio_mg,
    fosforo_mg,
    vitamina_c_mg,
    tiamina_mg,
    riboflavina_mg,
    niacina_mg,
    folatos_mcg,
    vitamina_b12_mcg,
    vitamina_a_er,
    sodio_mg
  ) %>%
  arrange(grupos_gabas, subgrupos_gabas, articulo)


library(kableExtra)

appendix_table_display = appendix_table %>%
  select(-grupos_gabas)

col_names <- c(
  "Food item", "COICOP", "GABA subgroup", "EP (\\%)",
  "Energy (kcal)", "Protein (g)", "Lipids (g)", "Carb. (g)",
  "Ca (mg)", "Fe (mg)", "Zn (mg)", "Mg (mg)", "P (mg)",
  "Vit. C (mg)", "Thi. (mg)", "Rib. (mg)", "Nia. (mg)",
  "Fol. (\\textmu g)", "B12 (\\textmu g)", "Vit. A (\\textmu g RE)", "Na (mg)"
)

length(col_names) == ncol(appendix_table_display) # debe ser TRUE

latex_table = appendix_table_display %>%
  mutate(across(all_of(numeric_vars),
                ~ format(round(.x, 2), nsmall = 2))) %>%
  kbl(
    format      = "latex",
    booktabs    = TRUE,
    longtable   = TRUE,
    col.names   = col_names,
    align       = c("l", "c", "l", "c", rep("r", 17)),
    caption     = "Food items, nutritional composition, and GABA classification. Values per 100 g of edible portion. EP: edible portion. Source: Colombian Food Composition Table (TCAC).",
    label       = "tab:appendix_food_items",
    escape      = FALSE
  ) %>%
  kable_styling(
    latex_options = c("repeat_header", "striped"),
    font_size     = 7
  ) %>%
  add_header_above(
    c(" " = 3,
      "Macronutrients" = 4,
      "Minerals"       = 6,
      "Vitamins"       = 7,
      " "              = 1),
    bold   = TRUE,
    escape = FALSE
  ) %>%
  pack_rows(
    index           = table(appendix_table$grupos_gabas),
    bold            = TRUE,
    latex_gap_space = "0.5em"
  )

writeLines(
  as.character(latex_table),
  file.path(out_dir, "appendix_S1_food_items.tex")
)

library(writexl)

appendix_table_display %>%
  mutate(across(all_of(numeric_vars),
                ~ round(.x, 2))) %>%
  write_xlsx(file.path(out_dir, "appendix_S1_food_items.xlsx"))
