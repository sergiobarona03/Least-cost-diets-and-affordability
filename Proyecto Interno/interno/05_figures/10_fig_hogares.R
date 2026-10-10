########################################################
## SCRIPT 05_figures/10_fig_hogares.R
## Costo de la dieta segun el tamano y la composicion del hogar (3, 4 y 5
## personas), trimestre jul-sep 2025.
##
## Figura 17: costo diario del hogar y por persona, por tamano y dieta
## Figura 18: costo diario de cada miembro, por dieta
## Figura 19: costo por 1000 kcal de cada tipo de miembro
##
## Reads:  coca_dir, cona_dir, ccona_dir, cord_dir, coahd_dir (resultados de
##         los hogares de 3, 4 y 5 miembros)
## Writes: fig_dir/06_hogares/fig17..fig19 (.png/.pdf)
########################################################

source("C:/Users/danie/OneDrive/Escritorio/Least-cost-diets-and-affordability/Proyecto Interno/interno/05_figures/00_fig_config.R")
library(tidyverse)
library(scales)
source(file.path(base_dir, "interno/05_figures/aux-functions/modelos_costos.R"))

ETIQUETA_HOGAR <- c("3" = "3 personas", "4" = "4 personas", "5" = "5 personas")

NOTA_HOGARES <- paste0("Hogar de 3: hombre, mujer y niña. Hogar de 4: hombre, mujer gestante, adolescente y niña. ",
                       "Hogar de 5: hombre, mujer, adolescente, niña y mujer mayor.")

costos <- map_dfr(3:5, function(n) leer_costos_modelos(n) %>% mutate(hogar_miembros = n)) %>%
  recode_member() %>%
  mutate(hogar_lbl = factor(ETIQUETA_HOGAR[as.character(hogar_miembros)], levels = ETIQUETA_HOGAR))

stopifnot(!anyNA(costos$member))

# -----------------------------------------------------------------------
# Figura 17: hogar y per capita
# -----------------------------------------------------------------------
por_hogar <- costos %>%
  group_by(hogar_lbl, modelo, ciudad, fecha) %>%
  summarise(total = sum(costo), personas = n(), .groups = "drop") %>%
  group_by(hogar_lbl, modelo) %>%
  summarise(`Costo diario del hogar` = mean(total),
            `Costo diario por persona` = mean(total / personas), .groups = "drop") %>%
  pivot_longer(starts_with("Costo"), names_to = "medida", values_to = "cop")

fig17 <- ggplot(por_hogar, aes(x = hogar_lbl, y = cop, fill = modelo)) +
  geom_col(position = position_dodge(width = 0.85), width = 0.8) +
  facet_wrap(~ medida, ncol = 1, scales = "free_y") +
  scale_fill_manual(values = MODEL_COLS, name = NULL) +
  scale_y_continuous(labels = cop_format(), expand = expansion(mult = c(0, 0.08))) +
  labs(title = "El costo de la dieta según el tamaño y la composición del hogar",
       subtitle = subtitulo_modelos(MODELOS, "COP por día, promedio de las 13 ciudades y del trimestre jul-sep 2025"),
       x = NULL, y = NULL,
       caption = paste0("Fuente: cálculos propios. ", NOTA_HOGARES, "\nCada persona se costea con su propio requerimiento de energía y nutrientes. CC-CoNA con alpha = ",
                        ALFA_TXT, ".")) +
  paper_theme(base_size = 11) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size = 8))
guardar_fig(fig17, "fig17_costo_composicion_hogar", "06_hogares", ancho = 13, alto = 8.4)

# -----------------------------------------------------------------------
# Figura 18: costo diario de cada miembro
# -----------------------------------------------------------------------
por_miembro <- costos %>%
  group_by(hogar_lbl, modelo, member) %>%
  summarise(cop = mean(costo), .groups = "drop")

fig18 <- ggplot(por_miembro, aes(x = hogar_lbl, y = cop, fill = member)) +
  geom_col(width = 0.7) +
  facet_wrap(~ modelo, nrow = 1) +
  scale_fill_manual(values = MEMBER_COLS, name = NULL) +
  scale_y_continuous(labels = cop_format(), expand = expansion(mult = c(0, 0.05))) +
  guides(fill = guide_legend(nrow = 2)) +
  labs(title = "Qué miembro pesa cuánto en el costo de la dieta del hogar",
       subtitle = subtitulo_modelos(MODELOS, "COP por día, promedio de las 13 ciudades y del trimestre jul-sep 2025"),
       x = NULL, y = NULL,
       caption = paste0("Fuente: cálculos propios. ", NOTA_HOGARES, "\nLa mujer gestante usa los requerimientos de nutrientes de FoodpriceR para gestantes de 31 a 50 años.")) +
  paper_theme(base_size = 11) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size = 7))
guardar_fig(fig18, "fig18_costo_por_miembro_hogar", "06_hogares", ancho = 14, alto = 7.5)

# -----------------------------------------------------------------------
# Figura 19: costo por 1000 kcal de cada tipo de miembro
# -----------------------------------------------------------------------
por_kcal <- costos %>%
  group_by(member, modelo) %>%
  summarise(kcal1000 = mean(kcal1000), .groups = "drop") %>%
  mutate(member = factor(member, levels = rev(MEMBER_ORDER)))

fig19 <- ggplot(por_kcal, aes(x = kcal1000, y = member, fill = modelo)) +
  geom_col(position = position_dodge(width = 0.85), width = 0.8) +
  scale_fill_manual(values = MODEL_COLS, name = NULL) +
  scale_x_continuous(labels = cop_format(), expand = expansion(mult = c(0, 0.05))) +
  labs(title = "Cuánto cuesta cubrir 1000 kcal a cada miembro del hogar",
       subtitle = subtitulo_modelos(MODELOS, "COP por 1000 kcal, promedio de las 13 ciudades y del trimestre jul-sep 2025"),
       x = NULL, y = NULL,
       caption = "Fuente: cálculos propios. El costo por 1000 kcal mide el precio de la energía de la dieta, no el gasto total.") +
  paper_theme(base_size = 11) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))
guardar_fig(fig19, "fig19_costo_1000kcal_por_miembro", "06_hogares", ancho = 12, alto = 6.75)

message("Figuras de hogares guardadas en: ", file.path(fig_dir, "06_hogares"))
