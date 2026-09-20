########################################################
## 04_figures/04_fig_modelos_costos.R
## Comparacion de los cuatro modelos por ciudad.
##
## Figura 5: costo per capita de CoCA, CoNA, CoRD y CoAHD.
## Figura 6: primas, es decir, cuantas veces cuesta cada dieta frente
##           al CoCA y frente al CoNA.
##
## Reads:  coca_dir, cona_dir, cord_dir, coahd_dir (resultados)
##
## Writes: fig_dir/01_costos/fig05_costo_4modelos_percapita.png/.pdf
##         fig_dir/01_costos/fig06_primas_modelos.png/.pdf
########################################################

source("C:/Users/danie/OneDrive/Escritorio/Least-cost-diets-and-affordability/Proyecto Interno/interno/04_figures/00_fig_config.R")
library(tidyverse)
library(scales)
library(patchwork)
source(file.path(base_dir, "interno/04_figures/aux-functions/modelos_costos.R"))

pc <- costo_percapita(leer_costos_modelos()) %>% recode_city()

# Mismo orden de ciudades que 02_fig_costos_modelos.R: costo medio de los cuatro modelos
orden_ciudades <- pc %>%
  group_by(ciudad_lbl) %>%
  summarise(costo = mean(pc), .groups = "drop") %>%
  arrange(desc(costo)) %>%
  pull(ciudad_lbl) %>%
  as.character()

# -----------------------------------------------------------------------
# Figura 5: costo per capita por modelo
# -----------------------------------------------------------------------
fig5 <- pc %>%
  mutate(ciudad_lbl = factor(ciudad_lbl, levels = rev(orden_ciudades)),
         modelo = factor(modelo, levels = rev(MODELOS))) %>%
  ggplot(aes(x = pc, y = ciudad_lbl, fill = modelo)) +
  geom_col(position = position_dodge(width = 0.85), width = 0.8) +
  scale_fill_manual(values = MODEL_COLS, breaks = MODELOS, name = NULL) +
  scale_x_continuous(labels = cop_format(), expand = expansion(mult = c(0, 0.03))) +
  labs(
    title    = "Costo per cápita de la dieta, por modelo",
    subtitle = "COP por día, promedio del trimestre (jul-sep 2025)",
    x = NULL, y = NULL,
    caption  = "Fuente: cálculos propios. Ciudades ordenadas por el costo medio de los cuatro modelos."
  ) +
  paper_theme(base_size = 14) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5),
        panel.grid.major.y = element_blank(),
        legend.position = "top")

guardar_fig(fig5, "fig05_costo_4modelos_percapita", "01_costos")

# -----------------------------------------------------------------------
# Figura 6: primas. Cada celda es el costo de un modelo dividido entre el
# de otro, con el valor impreso. Dos bloques: frente al CoCA y frente al CoNA.
# -----------------------------------------------------------------------
razones <- pc %>%
  select(ciudad_lbl, modelo, pc) %>%
  pivot_wider(names_from = modelo, values_from = pc) %>%
  mutate(ciudad_lbl = factor(ciudad_lbl, levels = rev(orden_ciudades)))

mapa_primas <- function(df, columnas, base, titulo, alto_color, mostrar_ciudades) {
  d <- df %>%
    select(ciudad_lbl, all_of(columnas), base = all_of(base)) %>%
    pivot_longer(all_of(columnas), names_to = "modelo", values_to = "costo") %>%
    mutate(razon = costo / base,
           modelo = factor(modelo, levels = columnas))
  centro <- min(d$razon) + 0.65 * diff(range(d$razon))

  ggplot(d, aes(x = modelo, y = ciudad_lbl, fill = razon)) +
    geom_tile(color = "white", linewidth = 1) +
    geom_text(aes(label = paste0(format(round(razon, 2), nsmall = 2, decimal.mark = ","), "\u00d7"),
                  color = razon > centro),
              size = 5, family = "serif") +
    scale_fill_gradient(low = "#F3F6F6", high = alto_color) +
    scale_color_manual(values = c("TRUE" = "white", "FALSE" = "grey15"), guide = "none") +
    scale_x_discrete(position = "top") +
    labs(title = titulo, x = NULL, y = NULL) +
    paper_theme(base_size = 14) +
    theme(panel.grid.major = element_blank(), panel.border = element_blank(),
          legend.position = "none",
          plot.title = element_text(hjust = 0.5, size = 14),
          axis.text.x = element_text(angle = 0, hjust = 0.5, face = "bold", size = 13),
          axis.text.y = if (mostrar_ciudades) element_text(size = 12) else element_blank(),
          axis.ticks = element_blank(), axis.ticks.length = unit(0, "pt"))
}

fig6 <- (
  mapa_primas(razones, c("CoNA", "CoRD", "CoAHD"), "CoCA", "Frente al CoCA", MODEL_COLS[["CoNA"]], TRUE) |
  mapa_primas(razones, c("CoRD", "CoAHD"), "CoNA", "Frente al CoNA", MODEL_COLS[["CoRD"]], FALSE)
) +
  plot_layout(widths = c(3, 2)) +
  plot_annotation(
    title    = "Cuántas veces cuesta cada dieta frente a otra",
    subtitle = "Costo per cápita de la dieta de cada columna dividido entre el de CoCA (izquierda) o el de CoNA (derecha)",
    caption  = "Fuente: cálculos propios. Un valor de 2,50\u00d7 quiere decir que la dieta cuesta 2,5 veces la de referencia en esa ciudad.\nCiudades ordenadas por el costo medio de los cuatro modelos.",
    theme = theme(text = element_text(family = "serif"),
                  plot.title = element_text(face = "bold", size = 16),
                  plot.subtitle = element_text(size = 13, color = "grey40"),
                  plot.caption = element_text(size = 10, color = "grey50", hjust = 0))
  )

guardar_fig(fig6, "fig06_primas_modelos", "01_costos")

message("Listo. Figuras 5 y 6 en: ", file.path(fig_dir, "01_costos"))
