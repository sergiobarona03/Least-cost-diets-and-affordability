########################################################
## SCRIPT 05_figures/08_fig_asequibilidad.R
## Asequibilidad de las cinco dietas por decil de ingreso y ciudad,
## trimestre jul-sep 2025.
##
## Figura 13: porcentaje de la poblacion que no puede pagar la dieta,
##            por decil y ciudad (hogar de tres personas como referencia)
## Figura 14: brecha de asequibilidad de los deciles 1 a 3
## Figura 15: porcentaje que no puede pagar la dieta segun el tamano del
##            hogar, con el costo del hogar de tres personas y con el costo
##            que corresponde al tamano
## Figura 16: contrafactual, el hogar dedica todo su ingreso a alimentos
##
## Falta el insumo de ingreso (microdatos de pobreza monetaria 2025 en 04_income/input): sin correr
## 04_income/01 y 02 no hay datos para estas figuras.
##
## Reads:  afford_dir/asequibilidad.rds
## Writes: fig_dir/05_asequibilidad/fig13..fig16 (.png/.pdf)
########################################################

source("C:/Users/danie/OneDrive/Escritorio/Least-cost-diets-and-affordability/Proyecto Interno/interno/05_figures/00_fig_config.R")
library(tidyverse)
library(scales)

asequibilidad <- readRDS(file.path(afford_dir, "asequibilidad.rds"))

indicadores <- asequibilidad$indicadores %>%
  mutate(model = factor(model, levels = names(MODEL_COLS))) %>%
  recode_city(col = "ciudad")

# Promedio de los tres meses
trimestre <- function(df, ...) {
  df %>%
    group_by(caso, ciudad_lbl, model, ...) %>%
    summarise(across(c(rate, gap, severity), mean), .groups = "drop")
}

orden_ciudades <- indicadores %>%
  filter(caso == "base", model == "CoNA") %>%
  group_by(ciudad_lbl) %>%
  summarise(r = mean(rate), .groups = "drop") %>%
  arrange(r) %>%
  pull(ciudad_lbl) %>%
  as.character()

nota_unidad <- paste0(
  "Fuente: cálculos propios con la GEIH 2025. El porcentaje es de la población que vive en hogares cuyo gasto ",
  "en alimentos per cápita es menor que el costo mensual per cápita de la dieta.")

# -----------------------------------------------------------------------
# Figura 13: tasa por decil y ciudad
# -----------------------------------------------------------------------
tasa_decil <- trimestre(filter(indicadores, caso == "base"), grupo) %>%
  mutate(grupo = factor(grupo, levels = paste0("Decil ", 1:10), labels = 1:10),
         ciudad_lbl = factor(ciudad_lbl, levels = orden_ciudades))

fig13 <- ggplot(tasa_decil, aes(x = grupo, y = ciudad_lbl, fill = rate)) +
  geom_tile(color = "white", linewidth = 0.3) +
  geom_text(aes(label = round(rate)), size = 2.2, family = "serif", color = "grey15") +
  facet_wrap(~ model, nrow = 1) +
  scale_fill_gradient(low = "#FFF7EC", high = "#B03A2E", limits = c(0, 100), oob = scales::squish,
                      name = "% de la población\nque no puede pagarla") +
  labs(title = "¿Quién no puede pagar la dieta?",
       subtitle = subtitulo_modelos(names(MODEL_COLS),
                                    "Porcentaje de la población de cada decil de ingreso, promedio jul-sep 2025"),
       x = "Decil de ingreso per cápita", y = NULL, caption = nota_unidad) +
  paper_theme(base_size = 11) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5), panel.grid = element_blank(), legend.title = element_text(size = 9),
        legend.key.height = unit(0.4, "cm"), legend.key.width = unit(1.6, "cm"))
guardar_fig(fig13, "fig13_asequibilidad_deciles", "05_asequibilidad", ancho = 14, alto = 7.6)

# -----------------------------------------------------------------------
# Figura 14: brecha de los deciles 1 a 3
# -----------------------------------------------------------------------
brecha <- trimestre(filter(indicadores, caso == "base", grupo %in% paste0("Decil ", 1:3)), grupo) %>%
  mutate(grupo = factor(grupo, levels = paste0("Decil ", 1:3), labels = paste("Decil", 1:3)),
         ciudad_lbl = factor(ciudad_lbl, levels = orden_ciudades))

fig14 <- ggplot(brecha, aes(x = grupo, y = ciudad_lbl, fill = 100 * gap)) +
  geom_tile(color = "white", linewidth = 0.3) +
  geom_text(aes(label = sprintf("%.0f", 100 * gap)), size = 2.6, family = "serif", color = "grey15") +
  facet_wrap(~ model, nrow = 1) +
  scale_fill_gradient(low = "#FDEDEC", high = "#7B241C", name = "Brecha (% del costo)") +
  labs(title = "¿Cuánto les falta a los hogares más pobres?",
       subtitle = subtitulo_modelos(names(MODEL_COLS),
                                    "Brecha de asequibilidad (FGT1) de los deciles 1 a 3, promedio jul-sep 2025"),
       x = NULL, y = NULL,
       caption = paste0("Fuente: cálculos propios con la GEIH 2025. La brecha es lo que le falta al gasto en alimentos para llegar al costo de la dieta,\n",
                        "como porcentaje de ese costo, promediada sobre toda la población del decil (cero para quien sí puede pagarla).")) +
  paper_theme(base_size = 11) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5), panel.grid = element_blank(), legend.title = element_text(size = 9),
        legend.key.width = unit(1.6, "cm"))
guardar_fig(fig14, "fig14_brecha_asequibilidad", "05_asequibilidad", ancho = 12, alto = 6.75)

# -----------------------------------------------------------------------
# Figura 15: tasa segun el tamano del hogar
# -----------------------------------------------------------------------
por_tamano <- indicadores %>%
  filter(caso %in% c("base_segun_hogar", "por_tamano_segun_hogar")) %>%
  group_by(caso, model, grupo) %>%
  summarise(rate = mean(rate), .groups = "drop") %>%
  mutate(grupo = factor(grupo, levels = c("3 o menos", "4", "5 o más")),
         umbral = recode(caso,
                         base_segun_hogar = "Costo del hogar de tres personas",
                         por_tamano_segun_hogar = "Costo del hogar del mismo tamaño"))

fig15 <- ggplot(por_tamano, aes(x = grupo, y = rate, fill = umbral)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.75) +
  geom_text(aes(label = sprintf("%.0f%%", rate)), position = position_dodge(width = 0.8),
            vjust = -0.4, size = 2.8, family = "serif") +
  facet_wrap(~ model, nrow = 1) +
  scale_fill_manual(values = c("Costo del hogar de tres personas" = "#B0BEC5",
                               "Costo del hogar del mismo tamaño" = "#264653"), name = NULL) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), expand = expansion(mult = c(0, 0.12))) +
  labs(title = "Pagar la dieta según el tamaño del hogar",
       subtitle = subtitulo_modelos(names(MODEL_COLS),
                                    "Porcentaje de la población que no puede pagarla, promedio de las 13 ciudades y jul-sep 2025"),
       x = "Personas en el hogar", y = NULL,
       caption = paste0(nota_unidad,
                        "\nEl costo del hogar del mismo tamaño sale de los hogares de 3, 4 y 5 personas.")) +
  paper_theme(base_size = 11) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))
guardar_fig(fig15, "fig15_asequibilidad_tamano_hogar", "05_asequibilidad", ancho = 12, alto = 6.75)

# -----------------------------------------------------------------------
# Figura 16: contrafactual, todo el ingreso a alimentos
# -----------------------------------------------------------------------
contra <- trimestre(filter(indicadores, caso == "contrafactual",
                           grupo %in% c("Decil 1", "Decil 2", "Decil 3")), grupo) %>%
  mutate(ciudad_lbl = factor(ciudad_lbl, levels = orden_ciudades))

fig16 <- ggplot(contra, aes(x = rate, y = ciudad_lbl, color = model)) +
  geom_point(size = 2.6) +
  facet_wrap(~ grupo, nrow = 1) +
  scale_color_manual(values = MODEL_COLS, name = NULL) +
  scale_x_continuous(labels = function(x) paste0(x, "%")) +
  labs(title = "Aun dedicando todo el ingreso a alimentos",
       subtitle = subtitulo_modelos(names(MODEL_COLS),
                                    "Porcentaje de la población de los deciles 1 a 3 cuyo ingreso per cápita no alcanza para la dieta"),
       x = NULL, y = NULL,
       caption = "Fuente: cálculos propios con la GEIH 2025. Contrafactual: el hogar destina todo su ingreso per cápita a alimentos.") +
  paper_theme(base_size = 11) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))
guardar_fig(fig16, "fig16_contrafactual", "05_asequibilidad", ancho = 12, alto = 6.75)

message("Figuras de asequibilidad guardadas en: ", file.path(fig_dir, "05_asequibilidad"))
