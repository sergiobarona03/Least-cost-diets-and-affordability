########################################################
## SCRIPT 03_models/10_zoom_alimentos.R
## Revision alimento por alimento de los productos que mas mueven
## o que mas dudas dejan en los resultados:
##   - maiz blanco trillado y maiz blanco con cascara (el CoCA);
##   - carne de res, cogote; menudencias de pollo; cabezas de pescado y
##     rabadillas de pollo (el CoNA).
##
## Para cada uno reune el sku que toma el panel, su precio original en el
## scraper (con el descuento que registra), el precio ya convertido a 100 g de
## parte comestible, su composicion, cuanto aporta a la dieta de minimo costo
## del CoNA, como se compara su precio con el de otras fuentes del mismo nutriente
## y cuanto sube el CoNA si se saca ese alimento.
##
## No cambia ningun resultado del flujo principal.
##
## Reads:  01_webscrap_prep/data/*.rds (precios originales del scraper)
##         output_dir/lista_alimentos/lista_alimentos.xlsx
##         output_dir/paneles/panel_mensual_cities_tcac.rds
##         cona_dir/cona_results.rds
##         household_dir/household_eer_ll.rds, household_ul.rds
## Writes: zoom_dir/zoom_alimentos.rds
##         zoom_dir/zoom_alimentos.xlsx
########################################################

library(tidyverse)
library(readxl)
library(writexl)
library(FoodpriceR)

# ============================================================
# Rutas
# ============================================================

dirs <- c(
  "C:/Users/Portatil/Desktop/Least-cost-diets-and-affordability/Proyecto Interno",
  "C:/Users/danie/OneDrive/Escritorio/Least-cost-diets-and-affordability/Proyecto Interno"
)

base_dir <- dirs[dir.exists(dirs)][1]

if (is.na(base_dir)) {
  stop("Ninguno de los directorios existe")
}

output_dir    <- file.path(base_dir, "interno/output")
raw_dir       <- file.path(base_dir, "interno/01_webscrap_prep/data")
aux_dir       <- file.path(base_dir, "interno/03_models/aux-functions")
household_dir <- file.path(base_dir, "interno/02_dataprep/household eer")
cona_dir      <- file.path(base_dir, "interno/03_models/cona")
zoom_dir      <- file.path(base_dir, "interno/03_models/zoom")

dir.create(zoom_dir, recursive = TRUE, showWarnings = FALSE)

source(file.path(aux_dir, "CoNA_paper.R"))

norm_ciudad <- function(x) toupper(iconv(trimws(x), from = "", to = "ASCII//TRANSLIT"))

MAICES   <- c("Maíz blanco trillado", "Maíz blanco cáscara")
SUBPROD  <- c("Carne de res, cogote", "Menudencias de pollo", "Pescado cabezas", "Rabadillas de pollo")
NUTRIENTES <- c(energia_kcal = "Energía", proteina_g = "Proteína", calcio_mg = "Calcio", hierro_mg = "Hierro",
                zinc_mg = "Zinc", vitamina_b12_mcg = "Vitamina B12", vitamina_a_er = "Vitamina A")

# ============================================================
# Insumos
# ============================================================

message("Loading inputs...")

panel <- readRDS(file.path(output_dir, "paneles/panel_mensual_cities_tcac.rds")) %>%
  mutate(ciudad_norm = norm_ciudad(ciudad))

elegidos <- read_excel(file.path(output_dir, "lista_alimentos/lista_alimentos.xlsx")) %>%
  mutate(ciudad_norm = norm_ciudad(city), sku_code = as.character(sku_code))

# Tamano del empaque (g o ml) de cada sku; los que se venden por kilo o por unidad no lo traen
tamanos <- readRDS(file.path(output_dir, "lista_alimentos/skus_todos.rds")) %>%
  transmute(sipsa_name, ciudad_norm = norm_ciudad(city), sku_code = as.character(sku_code),
            gramos_sku = suppressWarnings(as.numeric(gramos_sku))) %>%
  distinct(sipsa_name, ciudad_norm, sku_code, .keep_all = TRUE)

raw <- map_dfr(c("julio", "agosto", "septiembre"), ~ readRDS(file.path(raw_dir, paste0(.x, ".rds")))) %>%
  transmute(sipsa_name = as.character(sipsa_name),
            ciudad_norm = norm_ciudad(sub(", D\\.c\\.$|, D\\.C\\.$| De Indias$| de Indias$", "", city, ignore.case = TRUE)),
            sku_code = as.character(sku_code),
            price = suppressWarnings(as.numeric(price)),
            discount = suppressWarnings(as.numeric(discount)))

# ============================================================
# 1. El producto que toma el panel y su precio original
# ============================================================

original <- raw %>%
  filter(!is.na(price), price > 0) %>%
  group_by(sipsa_name, ciudad_norm, sku_code) %>%
  summarise(observaciones = n(), precio_original_mediano = median(price),
            precio_original_min = min(price), precio_original_max = max(price),
            obs_con_descuento = sum(!is.na(discount) & discount > 0),
            descuento_mediano = median(discount[!is.na(discount) & discount > 0]),
            .groups = "drop")

producto <- elegidos %>%
  filter(sipsa_name %in% c(MAICES, SUBPROD)) %>%
  select(sipsa_name, ciudad_norm, sku_code, exito_name) %>%
  left_join(tamanos, by = c("sipsa_name", "ciudad_norm", "sku_code")) %>%
  left_join(original, by = c("sipsa_name", "ciudad_norm", "sku_code")) %>%
  left_join(panel %>% group_by(articulo, ciudad_norm) %>%
              summarise(precio_base = mean(precio_base), precio_100g = mean(precio_100g),
                        codigo_tcac = first(codigo_tcac), parte_comestible = first(parte_comestible_percent),
                        across(all_of(names(NUTRIENTES)), first), .groups = "drop"),
            by = c("sipsa_name" = "articulo", "ciudad_norm")) %>%
  mutate(cop_100kcal = precio_100g / energia_kcal * 100,
         cop_g_proteina = precio_100g / proteina_g) %>%
  arrange(sipsa_name, ciudad_norm)

# ============================================================
# 2. Los cereales: precio original, convertido, por gramo y por kilocaloria
# ============================================================

cereales <- panel %>%
  filter(grepl("CEREALES", grupos_gabas), !is.na(precio_100g)) %>%
  group_by(ciudad_norm, articulo) %>%
  summarise(precio_base = mean(precio_base), precio_100g = mean(precio_100g),
            energia_kcal = first(energia_kcal), parte_comestible = first(parte_comestible_percent), .groups = "drop") %>%
  left_join(elegidos %>% select(articulo = sipsa_name, ciudad_norm, exito_name, sku_code), by = c("articulo", "ciudad_norm")) %>%
  left_join(tamanos, by = c("articulo" = "sipsa_name", "ciudad_norm", "sku_code")) %>%
  left_join(original, by = c("articulo" = "sipsa_name", "ciudad_norm", "sku_code")) %>%
  mutate(cop_100kcal = precio_100g / energia_kcal * 100) %>%
  group_by(ciudad_norm) %>% arrange(cop_100kcal, .by_group = TRUE) %>% mutate(orden = row_number()) %>% ungroup()

# ============================================================
# 3. Que aportan los subproductos a la dieta del CoNA
# ============================================================

cona <- readRDS(file.path(cona_dir, "cona_results.rds"))
comp <- cona$comp %>%
  mutate(ciudad_norm = norm_ciudad(ciudad), fecha = as.Date(fecha)) %>%
  left_join(panel %>% mutate(fecha = as.Date(fecha)) %>%
              select(Food = articulo, ciudad_norm, fecha, precio_100g, all_of(names(NUTRIENTES))),
            by = c("Food", "ciudad_norm", "fecha")) %>%
  mutate(costo = quantity / 100 * precio_100g)

aporte_celda <- comp %>%
  group_by(ciudad_norm, fecha, Demo_Group, Sex) %>%
  summarise(costo_total = sum(costo), across(all_of(names(NUTRIENTES)), ~ sum(.x * quantity / 100), .names = "tot_{.col}"), .groups = "drop")

uso <- comp %>%
  filter(Food %in% SUBPROD) %>%
  left_join(aporte_celda, by = c("ciudad_norm", "fecha", "Demo_Group", "Sex")) %>%
  mutate(pct_costo = 100 * costo / costo_total,
         across(all_of(names(NUTRIENTES)), ~ .x * quantity / 100, .names = "ap_{.col}")) %>%
  group_by(Food) %>%
  summarise(ciudades = n_distinct(ciudad_norm), celdas = n(),
            gramos_dia = mean(quantity), pct_costo = mean(pct_costo),
            across(all_of(names(NUTRIENTES)),
                   ~ 100 * sum(get(paste0("ap_", cur_column()))) / sum(get(paste0("tot_", cur_column()))), .names = "pct_{.col}"),
            .groups = "drop")

# ============================================================
# 4. Su precio frente a otras fuentes del mismo nutriente
#    (precio por unidad de nutriente, entre los alimentos de carnes, huevos y leguminosas)
# ============================================================

grupo_carnes <- panel %>% filter(grepl("CARNES", grupos_gabas), !is.na(precio_100g))
precio_nutriente <- map_dfr(c("proteina_g", "hierro_mg", "zinc_mg", "vitamina_b12_mcg", "vitamina_a_er"), function(n) {
  grupo_carnes %>%
    filter(.data[[n]] > 0) %>%
    mutate(p = precio_100g / .data[[n]], nutriente = NUTRIENTES[[n]]) %>%
    group_by(nutriente, ciudad_norm, fecha) %>%
    mutate(rango = rank(p), n_alimentos = n()) %>%
    ungroup() %>%
    filter(articulo %in% SUBPROD) %>%
    group_by(nutriente, articulo) %>%
    summarise(p_alimento = mean(p), rango_medio = mean(rango), n_alimentos = mean(n_alimentos), .groups = "drop") %>%
    left_join(
      grupo_carnes %>% filter(.data[[n]] > 0) %>% mutate(p = precio_100g / .data[[n]]) %>%
        group_by(ciudad_norm, fecha) %>% summarise(p_mediana = median(p), p_min = min(p), .groups = "drop") %>%
        summarise(p_mediana = mean(p_mediana), p_min = mean(p_min)) %>% mutate(nutriente = NUTRIENTES[[n]]),
      by = "nutriente")
})

# ============================================================
# 5. Cuanto sube el CoNA si se saca cada alimento
# ============================================================

normalise_eer <- function(df) {
  if ("rango" %in% names(df)) names(df)[names(df) == "rango"] <- "Age"
  if ("sex"   %in% names(df)) names(df)[names(df) == "sex"]   <- "Sex"
  if ("eer"   %in% names(df)) names(df)[names(df) == "eer"]   <- "Energy"
  if ("Sex"   %in% names(df) && is.character(df$Sex))
    df$Sex <- if_else(df$Sex == "Masculino", 0L, 1L)
  df
}

ll <- readRDS(file.path(household_dir, "household_eer_ll.rds"))
ul <- readRDS(file.path(household_dir, "household_ul.rds"))
dominios <- sort(unique(ll$ciudad)); fechas <- sort(unique(panel$fecha))

correr <- function(excluir) {
  map_dfr(dominios, function(i) {
    ll_i <- ll %>% filter(ciudad == i) %>% select(-ciudad, -any_of("cod_mun")) %>% as.data.frame() %>% normalise_eer()
    ul_i <- ul %>% filter(ciudad == i) %>% select(-ciudad, -any_of("cod_mun")) %>% as.data.frame() %>% normalise_eer()
    map_dfr(fechas, function(t) {
      d <- panel %>% filter(ciudad_norm == i, fecha == t, !is.na(precio_100g), articulo != "ARROZ PARA SOPA") %>%
        rename(Food = articulo, Price_100g = precio_100g, Energy = energia_kcal, Protein = proteina_g, Lipids = lipidos_g,
               Carbohydrates = carbohidratos_totales_g, VitaminC = vitamina_c_mg, Folate = folatos_mcg, VitaminA = vitamina_a_er,
               Thiamine = tiamina_mg, Riboflavin = riboflavina_mg, Niacin = niacina_mg, VitaminB12 = vitamina_b12_mcg,
               Magnesium = magnesio_mg, Phosphorus = fosforo_mg, Sodium = sodio_mg, Calcium = calcio_mg, Iron = hierro_mg,
               Zinc = zinc_mg) %>% as.data.frame()
      r <- suppressMessages(suppressWarnings(CoNA_paper(data = d, EER_LL = ll_i, UL = ul_i, exclude = excluir)))
      r$cost %>% mutate(ciudad = i, fecha = t)
    })
  })
}

message("Costo del CoNA sin cada alimento...")
sin_ciudad <- map_dfr(c(list(NULL), as.list(SUBPROD)), function(a) {
  message("  sin ", if (is.null(a)) "nada" else a)
  correr(a) %>% group_by(ciudad, fecha) %>% summarise(hogar = sum(cost_day), .groups = "drop") %>%
    group_by(ciudad) %>% summarise(pc = mean(hogar) / 3, .groups = "drop") %>%
    mutate(escenario = if (is.null(a)) "Panel completo" else paste("Sin", a))
}) %>%
  group_by(ciudad) %>% mutate(cambio = pc / pc[escenario == "Panel completo"] - 1) %>% ungroup()
sin_alimento <- sin_ciudad %>% group_by(escenario) %>%
  summarise(pc = mean(pc), cambio_medio = mean(cambio), cambio_max = max(cambio),
            ciudad_max = ciudad[which.max(cambio)], .groups = "drop")

# ============================================================
# Guardar
# ============================================================

salida <- list(producto = producto, cereales = cereales, uso = uso,
               precio_nutriente = precio_nutriente, sin_alimento = sin_alimento, sin_ciudad = sin_ciudad)
saveRDS(salida, file.path(zoom_dir, "zoom_alimentos.rds"))
write_xlsx(salida, file.path(zoom_dir, "zoom_alimentos.xlsx"))

message("Done. Resultados en ", zoom_dir)
