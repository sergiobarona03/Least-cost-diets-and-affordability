########################################################
## SCRIPT 05_figures/13_fig_umbral.R
## Sensibilidad al umbral de tamano del producto (500 g en los solidos y
## 1.000 ml en los liquidos) frente al caso sin umbral, trimestre jul-sep 2025.
##
## Figura 25: costo per capita de cada dieta con y sin el umbral
## Figura 26: cambio del costo al quitar el umbral, por ciudad
## Figura 27: alimentos cuyo precio por 100 g mas cambia al quitar el umbral
##
## Reads:  output_dir/sensibilidad_umbral/tablas.rds
##         output_dir/paneles/panel_mensual_cities_tcac.rds
##         output_dir/sensibilidad_umbral/sin_umbral_tamano/insumos/paneles/panel_mensual_cities_tcac.rds
## Writes: fig_dir/07_robustez/fig25..fig27 (.png/.pdf)
########################################################

source("C:/Users/danie/OneDrive/Escritorio/Least-cost-diets-and-affordability/Proyecto Interno/interno/05_figures/00_fig_config.R")
library(tidyverse)
library(scales)

ETQ_CON <- "Con umbral de tamaño (500 g / 1.000 ml)"
ETQ_SIN <- "Sin umbral de tamaño"

sens <- readRDS(file.path(output_dir, "sensibilidad_umbral/tablas.rds"))$costos %>%
  filter(regla %in% c("actual", "sin_umbral_tamano")) %>%
  mutate(modelo = factor(modelo, levels = names(MODEL_COLS)),
         regla = factor(regla, levels = c("actual", "sin_umbral_tamano"), labels = c(ETQ_CON, ETQ_SIN)))

# -----------------------------------------------------------------------
# Figura 25: costo con y sin umbral (un panel por dieta, con su color)
# -----------------------------------------------------------------------
medio <- sens %>% group_by(modelo, regla) %>% summarise(pc = mean(pc), .groups = "drop") %>%
  mutate(regla_corta = factor(if_else(regla == ETQ_CON, "Con umbral", "Sin umbral"), levels = c("Con umbral", "Sin umbral")))

fig25 <- ggplot(medio, aes(x = regla_corta, y = pc, fill = modelo, alpha = regla_corta)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = comma(pc, accuracy = 1, big.mark = ".", decimal.mark = ",")),
            vjust = -0.5, size = 3.4, family = "serif", alpha = 1) +
  facet_wrap(~ modelo, nrow = 1) +
  scale_fill_manual(values = MODEL_COLS, guide = "none") +
  scale_alpha_manual(values = c("Con umbral" = 1, "Sin umbral" = 0.45), guide = "none") +
  scale_y_continuous(labels = cop_format(), expand = expansion(mult = c(0, 0.12))) +
  labs(title = "Costo de la dieta con y sin umbral de tamaño del producto",
       subtitle = subtitulo_modelos(names(MODEL_COLS), "COP por persona y día, promedio de las 13 ciudades y del trimestre jul-sep 2025"),
       x = NULL, y = NULL,
       caption = paste0("Fuente: cálculos propios. Con umbral, el producto representativo se elige entre los de hasta 500 g (1.000 ml en líquidos); ",
                        "sin umbral, entre todos los tamaños. CC-CoNA con alpha = ", ALFA_TXT, ".")) +
  paper_theme(base_size = 12) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size = 9),
        panel.grid.major.x = element_blank())
guardar_fig(fig25, "fig25_costo_con_sin_umbral", "07_robustez", ancho = 12, alto = 7.5)

# -----------------------------------------------------------------------
# Figura 26: razon por ciudad
# -----------------------------------------------------------------------
razon <- sens %>%
  pivot_wider(names_from = regla, values_from = pc) %>%
  mutate(cambio = .data[[ETQ_SIN]] / .data[[ETQ_CON]] - 1) %>%
  rename(ciudad_cod = ciudad) %>%
  mutate(ciudad = ciudad_cod) %>%
  recode_city()

orden <- razon %>% filter(modelo == "CoNA") %>% arrange(cambio) %>% pull(ciudad_lbl) %>% as.character()

fig26 <- razon %>%
  mutate(ciudad_lbl = factor(ciudad_lbl, levels = rev(orden))) %>%
  ggplot(aes(x = cambio, y = ciudad_lbl, fill = modelo)) +
  geom_col(width = 0.72) +
  geom_vline(xintercept = 0, color = "grey35", linewidth = 0.4) +
  geom_text(aes(label = paste0(ifelse(cambio >= 0, "+", "-"), format(round(100 * abs(cambio), 1), nsmall = 1, decimal.mark = ","), "%"),
                hjust = ifelse(cambio >= 0, -0.12, 1.12)), size = 2.8, family = "serif") +
  facet_wrap(~ modelo, nrow = 1) +
  scale_fill_manual(values = MODEL_COLS, guide = "none") +
  scale_x_continuous(labels = function(x) paste0(ifelse(x > 0, "+", ""), format(round(100 * x, 0), decimal.mark = ",", trim = TRUE), "%"),
                     expand = expansion(mult = 0.3)) +
  labs(title = "Cambio en el costo de la dieta al quitar el umbral de tamaño",
       subtitle = subtitulo_modelos(names(MODEL_COLS), "Costo sin umbral / costo con umbral menos 1, por ciudad, promedio del trimestre jul-sep 2025"),
       x = NULL, y = NULL,
       caption = paste0("Fuente: cálculos propios. Una barra a la derecha de cero indica que la dieta sin umbral cuesta más. CC-CoNA con alpha = ", ALFA_TXT, ".")) +
  paper_theme(base_size = 11) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size = 8),
        panel.grid.major.y = element_blank())
guardar_fig(fig26, "fig26_cambio_sin_umbral", "07_robustez", ancho = 14, alto = 7.5)

# -----------------------------------------------------------------------
# Figura 27: los alimentos cuyo precio cambia
# -----------------------------------------------------------------------
p_con <- readRDS(file.path(output_dir, "paneles/panel_mensual_cities_tcac.rds")) %>%
  select(articulo, ciudad, fecha, p_con = precio_100g)
p_sin <- readRDS(file.path(output_dir, "sensibilidad_umbral/sin_umbral_tamano/insumos/paneles/panel_mensual_cities_tcac.rds")) %>%
  select(articulo, ciudad, fecha, p_sin = precio_100g)

cambio_alim <- inner_join(p_con, p_sin, by = c("articulo", "ciudad", "fecha")) %>%
  filter(!is.na(p_con), !is.na(p_sin), p_con > 0) %>%
  group_by(articulo) %>%
  summarise(cambio = mean(p_sin / p_con - 1), n_cambian = sum(abs(p_sin / p_con - 1) > 1e-6), n = n(), .groups = "drop") %>%
  filter(n_cambian > 0)

top <- bind_rows(cambio_alim %>% slice_max(cambio, n = 10), cambio_alim %>% slice_min(cambio, n = 10)) %>%
  distinct(articulo, .keep_all = TRUE) %>%
  mutate(articulo = fct_reorder(articulo, cambio), sube = cambio > 0)

saveRDS(top, file.path(fig_dir, "07_robustez/fig27_datos.rds"))

fig27 <- ggplot(top, aes(x = cambio, y = articulo, fill = sube)) +
  geom_col(width = 0.72) +
  geom_vline(xintercept = 0, color = "grey35", linewidth = 0.4) +
  geom_text(aes(label = paste0(ifelse(cambio >= 0, "+", "-"), format(round(100 * abs(cambio), 0), decimal.mark = ",", trim = TRUE), "%"),
                hjust = ifelse(cambio >= 0, -0.12, 1.12)), size = 3, family = "serif") +
  scale_fill_manual(values = c(`TRUE` = MODEL_COLS[["CC-CoNA"]], `FALSE` = MODEL_COLS[["CoRD"]]), guide = "none") +
  scale_x_continuous(labels = function(x) paste0(ifelse(x > 0, "+", ""), format(round(100 * x, 0), decimal.mark = ",", trim = TRUE), "%"),
                     expand = expansion(mult = 0.15)) +
  labs(title = "Los alimentos cuyo precio cambia al quitar el umbral de tamaño",
       subtitle = "Cambio del precio por 100 g, promedio de las ciudades y los meses; los 10 que más suben y los 10 que más bajan",
       x = NULL, y = NULL,
       caption = paste0("Fuente: cálculos propios. Solo se muestran alimentos con al menos una ciudad y mes en que el producto elegido cambia; ",
                        "la mayoría de los alimentos del panel no cambia.")) +
  paper_theme(base_size = 11) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5), panel.grid.major.y = element_blank())
guardar_fig(fig27, "fig27_alimentos_cambian_umbral", "07_robustez", ancho = 12, alto = 7)

message("Figuras 25 a 27 guardadas en: ", file.path(fig_dir, "07_robustez"))
