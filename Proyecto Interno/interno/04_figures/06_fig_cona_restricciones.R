########################################################
## 04_figures/06_fig_cona_restricciones.R
## Restricciones nutricionales del CoNA: cuales se activan y cuanto
## cuesta cada una.
##
## Figura 10: frecuencia con que cada restriccion nutricional es activa
##            (el requerimiento se cumple justo en el limite) por ciudad
##            y miembro del hogar.
## Figura 11: precio sombra (SPE) medio de los nutrientes mas activos.
##
## Reads:  cona_dir/cona_results.rds (elementos $limit y $spe)
##
## Writes: fig_dir/03_restricciones/fig10_restricciones_activas.png/.pdf
##         fig_dir/03_restricciones/fig11_precios_sombra.png/.pdf
########################################################

source("C:/Users/danie/OneDrive/Escritorio/Least-cost-diets-and-affordability/Proyecto Interno/interno/04_figures/00_fig_config.R")
library(tidyverse)
library(scales)

NUTRIENTES_ES <- c(
  Protein = "Proteína", Lipids = "Lípidos", Carbohydrates = "Carbohidratos",
  Calcium = "Calcio", Iron = "Hierro", Sodium = "Sodio", Phosphorus = "Fósforo",
  Zinc = "Zinc", Magnesium = "Magnesio", Thiamine = "Tiamina",
  Riboflavin = "Riboflavina", Niacin = "Niacina", Folate = "Folato",
  VitaminB12 = "Vitamina B12", VitaminC = "Vitamina C", VitaminA = "Vitamina A"
)

cona <- readRDS(file.path(cona_dir, "cona_results.rds"))

limit <- cona$limit %>%
  mutate(fecha = as.Date(fecha)) %>%
  recode_member(sex_col = "Sex", age_col = "Age") %>%
  recode_city()

spe <- cona$spe %>%
  filter(constraint == "Min") %>%
  mutate(fecha = as.Date(fecha)) %>%
  recode_member(sex_col = "Sex", age_col = "Age") %>%
  recode_city()

# Nutrientes ordenados por frecuencia total de activacion; los que nunca
# se activan salen del grafico
frecuencia <- limit %>%
  group_by(Nutrients) %>%
  summarise(freq = mean(Limiting == 1, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(freq))

activos <- frecuencia %>% filter(freq > 0) %>% pull(Nutrients)

etiqueta_nutriente <- function(x, orden) factor(NUTRIENTES_ES[x], levels = rev(NUTRIENTES_ES[orden]))

# -----------------------------------------------------------------------
# Figura 10: frecuencia de activacion
# -----------------------------------------------------------------------
binding <- limit %>%
  filter(Nutrients %in% activos) %>%
  group_by(Nutrients, member, ciudad_lbl) %>%
  summarise(pct = mean(Limiting == 1, na.rm = TRUE), .groups = "drop") %>%
  mutate(Nutrients = etiqueta_nutriente(Nutrients, activos))

fig10 <- ggplot(binding, aes(x = ciudad_lbl, y = Nutrients, fill = pct)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = percent(pct, accuracy = 1)), size = 3.4, family = "serif", color = "grey15") +
  facet_grid(member ~ .) +
  scale_fill_gradient(low = "#F3F6F6", high = MODEL_COLS[["CoRD"]], limits = c(0, 1),
                      labels = percent_format(accuracy = 1), name = "% de los meses") +
  labs(
    title    = "Restricciones nutricionales activas en el CoNA",
    subtitle = "Porcentaje de los tres meses en que el requerimiento se cumple exactamente en el límite",
    x = NULL, y = NULL,
    caption  = "Fuente: cálculos propios. Una restricción está activa cuando la dieta de mínimo costo cumple justo el límite.\nSe omiten los nutrientes que nunca se activan."
  ) +
  paper_theme(base_size = 13) +
  theme(
    panel.grid        = element_blank(),
    axis.text.x       = element_text(angle = 45, hjust = 1, size = 11),
    legend.position   = "right",
    legend.key.height = unit(1.6, "cm"),
    legend.title      = element_text(size = 11),
    strip.text.y      = element_text(angle = 0, size = 11)
  )

guardar_fig(fig10, "fig10_restricciones_activas", "03_restricciones", alto = 9.6)

# -----------------------------------------------------------------------
# Figura 11: precio sombra medio de los nutrientes mas activos
# Un mapa de calor por miembro: nutrientes en filas, ciudades en columnas
# -----------------------------------------------------------------------
UMBRAL_SPE <- 0.02

spe_medio <- spe %>%
  filter(Nutrients %in% activos) %>%
  group_by(Nutrients, member, ciudad_lbl) %>%
  summarise(spe = mean(SPE, na.rm = TRUE), .groups = "drop")

# Nutrientes con un SPE de al menos UMBRAL_SPE en alguna ciudad y miembro,
# del que mas al que menos pesa en promedio
top <- spe_medio %>%
  group_by(Nutrients) %>%
  summarise(medio = mean(spe), maximo = max(spe), .groups = "drop") %>%
  filter(maximo >= UMBRAL_SPE) %>%
  arrange(desc(medio)) %>%
  pull(Nutrients)

spe_medio <- spe_medio %>%
  filter(Nutrients %in% top) %>%
  mutate(Nutrients = etiqueta_nutriente(Nutrients, top),
         color_texto = spe > 0.6 * max(spe))

fig11 <- ggplot(spe_medio, aes(x = ciudad_lbl, y = Nutrients, fill = spe)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = format(round(spe, 2), nsmall = 2, decimal.mark = ","), color = color_texto),
            size = 3.6, family = "serif") +
  facet_grid(member ~ .) +
  scale_fill_gradient(low = "#F3F6F6", high = MODEL_COLS[["CoNA"]], name = "SPE") +
  scale_color_manual(values = c("TRUE" = "white", "FALSE" = "grey15"), guide = "none") +
  labs(
    title    = "Cuánto pesa cada nutriente en el costo del CoNA",
    subtitle = "Precio sombra (SPE) medio del trimestre, de los nutrientes que encarecen la dieta",
    x = NULL, y = NULL,
    caption  = paste0("Fuente: cálculos propios. El SPE es el precio sombra como proporción del costo diario de la dieta:\n",
                      "un valor mayor implica un mayor aumento del costo si el requerimiento fuera más exigente.\n",
                      "Se omiten los nutrientes con un SPE menor a ", format(UMBRAL_SPE, decimal.mark = ","),
                      " en todas las ciudades y miembros.")
  ) +
  paper_theme(base_size = 13) +
  theme(
    panel.grid        = element_blank(),
    axis.text.x       = element_text(angle = 45, hjust = 1, size = 11),
    legend.position   = "right",
    legend.key.height = unit(1.6, "cm"),
    strip.text.y      = element_text(angle = 0, size = 11)
  )

guardar_fig(fig11, "fig11_precios_sombra", "03_restricciones", alto = 8.4)

message("Listo. Figuras 10 y 11 en: ", file.path(fig_dir, "03_restricciones"))
