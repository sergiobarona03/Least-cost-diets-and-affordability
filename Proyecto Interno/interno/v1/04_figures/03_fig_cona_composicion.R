########################################################
## 04_figures/03_fig_cona_composicion.R
##
## Composicion de la dieta CoNA (adecuacion nutricional) para
## el hogar representativo en las 13 ciudades principales de
## Colombia, trimestre 3 de 2025 (julio, agosto, septiembre).
##
## Figura 1: mapa de calor — cantidad media per capita
##           (g/dia) de cada alimento seleccionado, por ciudad.
## Figura 2: contribucion al costo per capita del CoNA por
##           alimento (agrupado segun GABA), por ciudad, con
##           el costo per capita del CoNA (hcost) como linea
##           de validacion.
##
## Reads:  cona_dir/cona_results.rds        (elemento $comp, $cost)
##         output_dir/paneles/panel_mensual_cities_tcac.rds  (precios)
##
## Writes: fig_dir/02_composicion/fig03_heatmap_alimentos.png/.pdf
##         fig_dir/02_composicion/fig04_contribucion_costo.png/.pdf
########################################################

source("C:/Users/danie/OneDrive/Escritorio/Least-cost-diets-and-affordability/Proyecto Interno/interno/v1/04_figures/00_fig_config.R")
library(tidyverse)
library(scales)

# -----------------------------------------------------------------------
# 1. Cargar composicion CoNA (alimentos seleccionados por miembro)
# -----------------------------------------------------------------------
cona_full <- readRDS(file.path(cona_dir, "cona_results.rds"))

comp <- cona_full$comp %>%
  mutate(fecha = as.Date(fecha)) %>%
  filter(quantity > 0)

cona_cost <- cona_full$cost %>%
  mutate(fecha = as.Date(fecha))

N_MEMBERS <- comp %>% distinct(Demo_Group, Sex) %>% nrow()

all_foods <- sort(unique(comp$Food))

message(sprintf("Composicion CoNA: %d filas | %d alimentos distintos | %d ciudades",
                nrow(comp), length(all_foods), n_distinct(comp$ciudad)))

# -----------------------------------------------------------------------
# 2. Cantidad per capita por ciudad x alimento x fecha
##   (suma de los 3 miembros del hogar / 3)
# -----------------------------------------------------------------------
qty_percapita <- comp %>%
  group_by(ciudad, Food, fecha) %>%
  summarise(sum_qty = sum(quantity, na.rm = TRUE), .groups = "drop") %>%
  mutate(qty_pc = sum_qty / N_MEMBERS)

# -----------------------------------------------------------------------
# 3. FIGURA 1 — heatmap: cantidad media per capita (g/dia),
##   promedio del trimestre, por ciudad x alimento
# -----------------------------------------------------------------------
qty_heatmap <- qty_percapita %>%
  group_by(ciudad, Food) %>%
  summarise(mean_qty = mean(qty_pc, na.rm = TRUE), .groups = "drop") %>%
  recode_city()

food_order <- qty_heatmap %>%
  group_by(Food) %>%
  summarise(overall = mean(mean_qty, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(overall)) %>%
  pull(Food)

qty_heatmap <- qty_heatmap %>%
  mutate(Food = factor(Food, levels = rev(food_order)))

fig1 <- ggplot(qty_heatmap, aes(x = ciudad_lbl, y = Food, fill = mean_qty)) +
  geom_tile(color = "white", linewidth = 0.3) +
  geom_text(aes(label = round(mean_qty, 0)),
            size = 2.3, family = "serif", color = "grey20") +
  scale_fill_gradient(low = "#EFF3FF", high = "#2166AC",
                      name = "g/día\n(per cápita)",
                      na.value = "grey95") +
  labs(
    title = "Alimentos seleccionados en la dieta CoNA",
    subtitle = "Cantidad media per cápita (g/día)",
    x = NULL, y = NULL,
    caption = "Fuente: cálculos propios. Celdas vacías: alimento no seleccionado en esa ciudad."
  ) +
  paper_theme() +
  theme(
    axis.text.x   = element_text(angle = 45, hjust = 1, size = 8),
    axis.text.y   = element_text(size = 8),
    panel.grid    = element_blank(),
    legend.position   = "right",
    legend.key.height = unit(1.8, "cm")
  )

fig_h <- max(8, length(all_foods) * 0.32)
ggsave(file.path(fig_dir, "02_composicion", "fig03_heatmap_alimentos.png"),
       fig1, width = 10, height = fig_h, dpi = 300, bg = "white")
ggsave(file.path(fig_dir, "02_composicion", "fig03_heatmap_alimentos.pdf"),
       fig1, width = 10, height = fig_h)
message("Figura 3 (heatmap) guardada.")

# -----------------------------------------------------------------------
# 4. Precios: panel de precios usado por 02_cona.R
##   (mismo insumo, mismo criterio de normalizacion de ciudad)
# -----------------------------------------------------------------------
data_paper <- readRDS(file.path(output_dir, "paneles/panel_mensual_cities_tcac.rds")) %>%
  mutate(fecha = as.Date(fecha),
         ciudad_norm = toupper(iconv(ciudad, from = "", to = "ASCII//TRANSLIT"))) %>%
  filter(articulo %in% all_foods, !is.na(precio_100g)) %>%
  group_by(ciudad_norm, articulo, fecha) %>%
  summarise(precio_100g = mean(precio_100g, na.rm = TRUE), .groups = "drop") %>%
  rename(ciudad = ciudad_norm, Food = articulo)

# -----------------------------------------------------------------------
# 5. Contribucion al costo per capita = precio_100g/100 * cantidad per capita
# -----------------------------------------------------------------------
cost_contrib <- qty_percapita %>%
  left_join(data_paper, by = c("ciudad", "Food", "fecha")) %>%
  mutate(contrib = precio_100g * qty_pc / 100)

n_sin_precio <- sum(is.na(cost_contrib$contrib))
if (n_sin_precio > 0) {
  warning(sprintf(
    "%d combinaciones ciudad-alimento-fecha sin precio emparejado (%.1f%% del total); se excluyen de la Figura 4.",
    n_sin_precio, 100 * n_sin_precio / nrow(cost_contrib)))
}
cost_contrib <- cost_contrib %>% filter(!is.na(contrib), contrib > 0)

# -----------------------------------------------------------------------
# 6. Grupos GABA por alimento (compartido con la version en
##   produccion, no depende del metodo de precio -- ver
##   04_figures/aux-functions/food_groups_gaba.R)
# -----------------------------------------------------------------------
source("C:/Users/danie/OneDrive/Escritorio/Least-cost-diets-and-affordability/Proyecto Interno/interno/04_figures/aux-functions/food_groups_gaba.R")

foods_sin_grupo <- setdiff(all_foods, food_groups$Food)
if (length(foods_sin_grupo) > 0) {
  warning("Alimentos sin grupo GABA asignado (agregarlos a 04_figures/aux-functions/food_groups_gaba.R): ",
         paste(foods_sin_grupo, collapse = ", "))
}

## Solo los alimentos que de verdad aparecen en ESTA composicion --
## food_groups ahora es una tabla compartida con muchos mas
## alimentos de los que selecciona cualquier version puntual, asi
## que hay que filtrar antes de usarla como niveles del factor (si
## no, la leyenda sale con alimentos que ni siquiera se usaron).
food_order_contrib <- food_groups$Food[food_groups$Food %in% all_foods]

# -----------------------------------------------------------------------
# 8. Agregar contribucion al trimestre y ordenar ciudades
# -----------------------------------------------------------------------
contrib_trim <- cost_contrib %>%
  left_join(food_groups, by = "Food") %>%
  mutate(Food = factor(Food, levels = food_order_contrib)) %>%
  group_by(ciudad, Food) %>%
  summarise(contrib = mean(contrib, na.rm = TRUE), .groups = "drop") %>%
  recode_city()

orden_ciudad_costo <- contrib_trim %>%
  group_by(ciudad_lbl) %>%
  summarise(total = sum(contrib, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(total)) %>%
  pull(ciudad_lbl)

contrib_trim <- contrib_trim %>%
  mutate(ciudad_lbl = factor(ciudad_lbl, levels = orden_ciudad_costo))

# Linea de validacion: CoNA per capita real (hcost = suma miembros / 3)
cona_pc_valid <- cona_cost %>%
  group_by(ciudad, fecha) %>%
  summarise(cost_hogar = sum(cona_cost, na.rm = TRUE), .groups = "drop") %>%
  mutate(cona_pc = cost_hogar / N_MEMBERS) %>%
  group_by(ciudad) %>%
  summarise(cona_pc = mean(cona_pc, na.rm = TRUE), .groups = "drop") %>%
  recode_city() %>%
  mutate(ciudad_lbl = factor(ciudad_lbl, levels = orden_ciudad_costo))

# -----------------------------------------------------------------------
# 9. FIGURA 2 — contribucion al costo per capita del CoNA, por alimento
# -----------------------------------------------------------------------
fig2 <- ggplot(contrib_trim, aes(x = ciudad_lbl, y = contrib, fill = Food)) +
  geom_col(width = 0.7, alpha = 0.95) +
  geom_point(data = cona_pc_valid, aes(x = ciudad_lbl, y = cona_pc),
            inherit.aes = FALSE, shape = 21, fill = "white",
            color = "black", size = 2, stroke = 0.8) +
  scale_fill_manual(values = food_palette, name = "Alimento") +
  scale_y_continuous(labels = cop_format(suffix = " COP")) +
  labs(
    title = "Contribución de cada alimento al costo per cápita del CoNA",
    x = NULL,
    y = "COP / día per cápita",
    caption = "Fuente: cálculos propios, con panel de precios interno/output/paneles."
  ) +
  paper_theme() +
  theme(
    axis.text.x     = element_text(angle = 45, hjust = 1, size = 9),
    legend.position = "right",
    legend.text     = element_text(size = 7),
    legend.key.size = unit(0.4, "cm")
  ) +
  guides(fill = guide_legend(ncol = 2))

## Altura dinamica: con la tabla GABA compartida, el numero de
## alimentos seleccionados varia segun la version (v1/v2), asi que
## la leyenda (2 columnas) puede necesitar mas alto que un valor fijo.
fig2_h <- max(7, length(food_order_contrib) / 2 * 0.28)

ggsave(file.path(fig_dir, "02_composicion", "fig04_contribucion_costo.png"),
       fig2, width = 13, height = fig2_h, dpi = 300, bg = "white")
ggsave(file.path(fig_dir, "02_composicion", "fig04_contribucion_costo.pdf"),
       fig2, width = 13, height = fig2_h)
message("Figura 4 (contribucion al costo) guardada.")

message("Listo. Figuras en: ", file.path(fig_dir, "02_composicion"))
