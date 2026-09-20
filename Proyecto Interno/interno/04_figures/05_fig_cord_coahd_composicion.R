########################################################
## 04_figures/05_fig_cord_coahd_composicion.R
## Composicion de CoRD y CoAHD. Estos modelos siempre eligen el mismo
## numero de alimentos por grupo GABA, asi que en vez de las cantidades
## (como en CoNA) se muestra cuanto pesa cada grupo en el costo y que
## alimentos gana cada ciudad.
##
## Figura 7: costo per capita por grupo GABA (COP/dia).
## Figura 8: participacion de cada grupo en el costo (%).
## Figura 9: alimentos elegidos por ciudad (% de los meses del trimestre).
##
## Reads:  cord_dir/cord_results.rds, coahd_dir/coahd_results.rds
##         output_dir/paneles/panel_mensual_cities_tcac.rds
##         output_dir/tcac/composicion_270726.rds
##
## Writes: fig_dir/02_composicion/fig07_costo_por_grupo.png/.pdf
##         fig_dir/02_composicion/fig08_participacion_grupo.png/.pdf
##         fig_dir/02_composicion/fig09_alimentos_elegidos.png/.pdf
########################################################

source("C:/Users/danie/OneDrive/Escritorio/Least-cost-diets-and-affordability/Proyecto Interno/interno/04_figures/00_fig_config.R")
library(tidyverse)
library(scales)
source(file.path(base_dir, "interno/03_models/aux-functions/gaba_insumos.R"), encoding = "UTF-8")

GRUPO_LAB <- c(
  "Cereales, raíces, tubérculos y plátanos"              = "Cereales, raíces y plátanos",
  "Frutas"                                               = "Frutas",
  "Verduras"                                             = "Verduras",
  "Leche y productos lácteos"                            = "Lácteos",
  "Carnes, huevos, leguminosas, frutos secos y semillas" = "Carnes, huevos y leguminosas",
  "Grasas"                                               = "Grasas",
  "Azúcares"                                             = "Azúcares"
)

GRUPO_COLS <- c(
  "Cereales, raíces y plátanos" = "#D9A441",
  "Frutas"                      = "#C0392B",
  "Verduras"                    = "#1A7A4A",
  "Lácteos"                     = "#2E86C1",
  "Carnes, huevos y leguminosas" = "#7B4B3A",
  "Grasas"                      = "#8E44AD",
  "Azúcares"                    = "#95A5A6"
)

recode_grupo <- function(df) {
  df %>% mutate(grupo = factor(GRUPO_LAB[Group], levels = unname(GRUPO_LAB)))
}

# -----------------------------------------------------------------------
# 1. Costo por alimento elegido, en los dos modelos
# -----------------------------------------------------------------------
tcac <- readRDS(file.path(output_dir, "tcac/composicion_270726.rds")) %>%
  distinct(sipsa_name, .keep_all = TRUE)

precios <- readRDS(file.path(output_dir, "paneles/panel_mensual_cities_tcac.rds")) %>%
  transmute(ciudad = toupper(iconv(ciudad, from = "", to = "ASCII//TRANSLIT")),
            fecha = as.Date(fecha), Food = articulo, precio_100g)

cord_res  <- readRDS(file.path(cord_dir,  "cord_results.rds"))
coahd_res <- readRDS(file.path(coahd_dir, "coahd_results.rds"))

# CoRD no guarda el costo por alimento: intercambios x gramos x precio por gramo
cord_comp <- cord_res$comp %>%
  mutate(fecha = as.Date(fecha), Sex = as.integer(Sex)) %>%
  left_join(tcac %>% select(Food = sipsa_name, gramos_intercambio = gramos_g_1_intercambio), by = "Food") %>%
  left_join(precios, by = c("ciudad", "fecha", "Food")) %>%
  mutate(Cost = Number_Serving * gramos_intercambio * precio_100g / 100)

# El costo por alimento debe sumar el costo del modelo
chequeo <- cord_comp %>%
  group_by(ciudad, fecha, Demo_Group, Sex) %>% summarise(suma = sum(Cost), .groups = "drop") %>%
  left_join(cord_res$cost %>% mutate(fecha = as.Date(fecha)), by = c("ciudad", "fecha", "Demo_Group", "Sex"))
stopifnot(max(abs(chequeo$suma - chequeo$cost_day)) < 1e-6)

comp <- bind_rows(
  CoRD  = cord_comp %>% select(ciudad, fecha, Demo_Group, Sex, Food, Group, Cost),
  CoAHD = coahd_res$comp %>% mutate(fecha = as.Date(fecha)) %>%
    select(ciudad, fecha, Demo_Group, Sex, Food, Group, Cost),
  .id = "modelo"
) %>%
  mutate(modelo = factor(modelo, levels = c("CoRD", "CoAHD"))) %>%
  recode_grupo()

N_MIEMBROS <- n_distinct(comp$Demo_Group, comp$Sex)

# -----------------------------------------------------------------------
# 2. Costo per capita por grupo
# -----------------------------------------------------------------------
por_grupo <- comp %>%
  group_by(modelo, ciudad, fecha, grupo) %>%
  summarise(costo = sum(Cost), .groups = "drop") %>%
  group_by(modelo, ciudad, grupo) %>%
  summarise(pc = mean(costo) / N_MIEMBROS, .groups = "drop") %>%
  recode_city()

orden_ciudad <- por_grupo %>%
  filter(modelo == "CoAHD") %>%
  group_by(ciudad_lbl) %>% summarise(total = sum(pc), .groups = "drop") %>%
  arrange(total) %>% pull(ciudad_lbl) %>% as.character()

por_grupo <- por_grupo %>% mutate(ciudad_lbl = factor(ciudad_lbl, levels = orden_ciudad))

leyenda_grupos <- guides(fill = guide_legend(nrow = 1, byrow = TRUE))

# -----------------------------------------------------------------------
# Figura 7: COP por grupo
# -----------------------------------------------------------------------
fig7 <- ggplot(por_grupo, aes(x = pc, y = ciudad_lbl, fill = grupo)) +
  geom_col(width = 0.75) +
  facet_wrap(~ modelo, nrow = 1) +
  scale_fill_manual(values = GRUPO_COLS, name = NULL) +
  scale_x_continuous(labels = cop_format(), breaks = seq(0, 6000, 2000), expand = expansion(mult = c(0, 0.03))) +
  labs(
    title    = "Costo per cápita de CoRD y CoAHD por grupo de alimentos",
    subtitle = "COP por día, promedio del trimestre",
    x = NULL, y = NULL,
    caption  = "Fuente: cálculos propios. Grupos de las Guías Alimentarias Basadas en Alimentos (GABA)."
  ) +
  leyenda_grupos +
  paper_theme() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5),
        legend.text = element_text(size = 8))

guardar_fig(fig7, "fig07_costo_por_grupo", "02_composicion", ancho = 12, alto = 6.75)

# -----------------------------------------------------------------------
# Figura 8: participacion (%)
# -----------------------------------------------------------------------
fig8 <- ggplot(por_grupo, aes(x = pc, y = ciudad_lbl, fill = grupo)) +
  geom_col(position = "fill", width = 0.75) +
  facet_wrap(~ modelo, nrow = 1) +
  scale_fill_manual(values = GRUPO_COLS, name = NULL) +
  scale_x_continuous(labels = percent_format(accuracy = 1), breaks = c(0, 0.25, 0.5, 0.75), expand = expansion(mult = c(0, 0.01))) +
  labs(
    title    = "Participación de cada grupo en el costo de CoRD y CoAHD",
    subtitle = "Porcentaje del costo diario, promedio del trimestre",
    x = NULL, y = NULL,
    caption  = "Fuente: cálculos propios."
  ) +
  leyenda_grupos +
  paper_theme() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5),
        legend.text = element_text(size = 8))

guardar_fig(fig8, "fig08_participacion_grupo", "02_composicion", ancho = 12, alto = 6.75)

# -----------------------------------------------------------------------
# Figura 9: alimentos elegidos por ciudad
#   Los alimentos se eligen por precio, asi que son los mismos para todos
#   los miembros del hogar; se cuenta en cuantos meses lo gana cada ciudad.
# -----------------------------------------------------------------------
n_meses <- n_distinct(comp$fecha)

elegidos <- comp %>%
  distinct(modelo, ciudad, fecha, Food, grupo) %>%
  count(modelo, ciudad, Food, grupo, name = "meses") %>%
  mutate(pct = meses / n_meses) %>%
  recode_city()

orden_alimentos <- elegidos %>%
  group_by(grupo, Food) %>% summarise(peso = sum(pct), .groups = "drop") %>%
  arrange(grupo, peso) %>% pull(Food) %>% unique()

# Todas las combinaciones para que las celdas no elegidas aparezcan vacias
malla <- elegidos %>%
  distinct(Food, grupo) %>%
  crossing(modelo = factor(c("CoRD", "CoAHD"), levels = c("CoRD", "CoAHD")),
           ciudad_lbl = factor(CITY_LABS[CITY_ORDER], levels = CITY_LABS[CITY_ORDER])) %>%
  left_join(elegidos %>% select(modelo, ciudad_lbl, Food, pct), by = c("modelo", "ciudad_lbl", "Food")) %>%
  mutate(Food = factor(Food, levels = orden_alimentos))

fig9 <- ggplot(malla, aes(x = ciudad_lbl, y = Food, fill = pct)) +
  geom_tile(color = "white", linewidth = 0.4) +
  facet_grid(grupo ~ modelo, scales = "free_y", space = "free_y") +
  scale_fill_gradient(low = "#D6E9DD", high = "#1A7A4A", labels = percent_format(accuracy = 1),
                      limits = c(0, 1), na.value = "grey96", name = "% de los meses") +
  labs(
    title    = "Alimentos elegidos por CoRD y CoAHD en cada ciudad",
    subtitle = "Porcentaje de los tres meses del trimestre en que el alimento entra a la dieta",
    x = NULL, y = NULL,
    caption  = "Fuente: cálculos propios. Celdas vacías: el alimento no fue elegido en esa ciudad."
  ) +
  paper_theme() +
  theme(
    axis.text.y      = element_text(size = 8),
    axis.text.x      = element_text(angle = 45, hjust = 1, size = 8),
    panel.grid       = element_blank(),
    strip.text.y     = element_text(angle = 0, size = 8),
    legend.position  = "right",
    legend.key.height = unit(1.4, "cm"),
    legend.title     = element_text(size = 9)
  )

n_alimentos <- n_distinct(malla$Food)
guardar_fig(fig9, "fig09_alimentos_elegidos", "02_composicion", ancho = 12, alto = max(8, n_alimentos * 0.3))

message("Listo. Figuras 7, 8 y 9 en: ", file.path(fig_dir, "02_composicion"))
