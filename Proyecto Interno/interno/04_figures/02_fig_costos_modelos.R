########################################################
## 04_figures/02_fig_costos_modelos.R
## Costo de CoCA, CoNA, CoRD y CoAHD para el hogar representativo
## (hombre adulto 31-50, mujer adulta 31-50 y niña 9-13) en las 13
## ciudades, promedio del trimestre (jul-sep 2025). Barras
## horizontales, una por ciudad, ordenadas por costo.
##
## Figura 1:  costo diario por miembro del hogar
## Figura 1c: costo por 1000 kcal por miembro
## Figura 2:  costo per capita del hogar
## Figura 2c: costo por 1000 kcal del hogar (promedio de los miembros)
##
## Reads:  coca_dir, cona_dir, cord_dir, coahd_dir (resultados)
##
## Writes: fig_dir/01_costos/fig01_costo_miembro_barras.png/.pdf
##         fig_dir/01_costos/fig01c_costo_miembro_1000kcal_barras.png/.pdf
##         fig_dir/01_costos/fig02_costo_percapita_barras.png/.pdf
##         fig_dir/01_costos/fig02c_costo_hogar_1000kcal_barras.png/.pdf
########################################################

source("C:/Users/danie/OneDrive/Escritorio/Least-cost-diets-and-affordability/Proyecto Interno/interno/04_figures/00_fig_config.R")
library(tidyverse)
library(scales)
source(file.path(base_dir, "interno/04_figures/aux-functions/modelos_costos.R"))

costos <- leer_costos_modelos() %>%
  recode_member() %>%
  recode_city()

# Mismo orden de ciudades en todas las figuras: costo medio de los cuatro
# modelos, de mayor a menor
orden_ciudades <- costos %>%
  group_by(ciudad_lbl) %>%
  summarise(costo = mean(costo), .groups = "drop") %>%
  arrange(desc(costo)) %>%
  pull(ciudad_lbl)

barras_ciudad <- function(df, valor, facetas, x_lab, titulo, nota, tam_texto = 3.2) {
  df %>%
    mutate(ciudad_lbl = factor(ciudad_lbl, levels = rev(as.character(orden_ciudades)))) %>%
    ggplot(aes(x = {{ valor }}, y = ciudad_lbl, fill = modelo)) +
    geom_col(width = 0.72) +
    geom_text(aes(label = comma({{ valor }}, accuracy = 1, big.mark = ".", decimal.mark = ",")),
              hjust = -0.1, size = tam_texto, family = "serif", color = "grey20") +
    facetas +
    scale_fill_manual(values = MODEL_COLS, guide = "none") +
    scale_x_continuous(labels = cop_format(), expand = expansion(mult = c(0, 0.22))) +
    labs(title = titulo, x = x_lab, y = NULL, caption = nota) +
    paper_theme(base_size = 13) +
    theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size = 9),
          panel.grid.major.y = element_blank())
}

# -----------------------------------------------------------------------
# Figura 1: costo diario por miembro
# -----------------------------------------------------------------------
por_miembro <- costos %>%
  group_by(modelo, ciudad_lbl, member) %>%
  summarise(dia = mean(costo), kcal = mean(kcal1000), .groups = "drop")

fig1 <- barras_ciudad(
  por_miembro, dia, facet_grid(member ~ modelo),
  x_lab = "COP / día", titulo = "Costo diario de la dieta por miembro del hogar",
  nota = "Fuente: cálculos propios. Promedio del trimestre (jul-sep 2025).", tam_texto = 2.7)
guardar_fig(fig1, "fig01_costo_miembro_barras", "01_costos", alto = 9.6)

# -----------------------------------------------------------------------
# Figura 1c: costo por 1000 kcal por miembro
# -----------------------------------------------------------------------
fig1c <- barras_ciudad(
  por_miembro, kcal, facet_grid(member ~ modelo),
  x_lab = "COP / 1000 kcal", titulo = "Costo de la dieta por 1000 kcal, por miembro del hogar",
  nota = "Fuente: cálculos propios. Costo normalizado por las kilocalorías de la dieta de cada miembro.",
  tam_texto = 2.7)
guardar_fig(fig1c, "fig01c_costo_miembro_1000kcal_barras", "01_costos", alto = 9.6)

# -----------------------------------------------------------------------
# Figura 2: costo per capita del hogar
# -----------------------------------------------------------------------
per_capita <- costo_percapita(costos) %>% recode_city()

fig2 <- barras_ciudad(
  per_capita, pc, facet_wrap(~ modelo, nrow = 1),
  x_lab = "COP / día per cápita", titulo = "Costo per cápita del hogar representativo",
  nota = "Fuente: cálculos propios. Costo del hogar (tres miembros) dividido entre 3, promedio del trimestre.")
guardar_fig(fig2, "fig02_costo_percapita_barras", "01_costos")

# -----------------------------------------------------------------------
# Figura 2c: costo por 1000 kcal del hogar
# -----------------------------------------------------------------------
hogar_kcal <- por_miembro %>%
  group_by(modelo, ciudad_lbl) %>%
  summarise(kcal = mean(kcal), .groups = "drop")

fig2c <- barras_ciudad(
  hogar_kcal, kcal, facet_wrap(~ modelo, nrow = 1),
  x_lab = "COP / 1000 kcal", titulo = "Costo de la dieta por 1000 kcal, hogar representativo",
  nota = "Fuente: cálculos propios. Promedio simple de los tres miembros (mide el precio de la energía, no el gasto total).")
guardar_fig(fig2c, "fig02c_costo_hogar_1000kcal_barras", "01_costos")

message("Listo. Figuras en: ", file.path(fig_dir, "01_costos"))
