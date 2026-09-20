########################################################
## 04_figures/07_fig_adecuacion.R
## Adecuacion nutricional de las dietas de cada modelo: aporte de la
## dieta / requerimiento minimo, para cada nutriente y miembro del hogar.
##
## Figura 12: adecuacion media (% del requerimiento) por modelo.
##
## Reads:  coca_dir, cona_dir, cord_dir, coahd_dir (resultados)
##         output_dir/tcac/composicion_270726.rds (nutrientes por 100 g)
##         02_dataprep/household eer/household_eer_ll.rds (requerimientos)
##
## Writes: fig_dir/04_adecuacion/fig12_adecuacion_nutricional.png/.pdf
########################################################

source("C:/Users/danie/OneDrive/Escritorio/Least-cost-diets-and-affordability/Proyecto Interno/interno/04_figures/00_fig_config.R")
library(tidyverse)
library(scales)
source(file.path(base_dir, "interno/04_figures/aux-functions/modelos_costos.R"))

household_dir <- file.path(base_dir, "interno/02_dataprep/household eer")

# Nombre en el modelo -> columna del TCAC / etiqueta del grafico
NUTRIENTES <- tribble(
  ~nutriente,      ~col_tcac,                  ~etiqueta,
  "Energy",        "energia_kcal",             "Energía",
  "Protein",       "proteina_g",               "Proteína",
  "Lipids",        "lipidos_g",                "Lípidos",
  "Carbohydrates", "carbohidratos_totales_g",  "Carbohidratos",
  "Calcium",       "calcio_mg",                "Calcio",
  "Iron",          "hierro_mg",                "Hierro",
  "Zinc",          "zinc_mg",                  "Zinc",
  "Magnesium",     "magnesio_mg",              "Magnesio",
  "Phosphorus",    "fosforo_mg",               "Fósforo",
  "VitaminA",      "vitamina_a_er",            "Vitamina A",
  "VitaminC",      "vitamina_c_mg",            "Vitamina C",
  "Thiamine",      "tiamina_mg",               "Tiamina",
  "Riboflavin",    "riboflavina_mg",           "Riboflavina",
  "Niacin",        "niacina_mg",               "Niacina",
  "Folate",        "folatos_mcg",              "Folato",
  "VitaminB12",    "vitamina_b12_mcg",         "Vitamina B12"
)

# -----------------------------------------------------------------------
# 1. Gramos de cada alimento en la dieta de cada modelo
# -----------------------------------------------------------------------
tcac <- readRDS(file.path(output_dir, "tcac/composicion_270726.rds")) %>%
  distinct(sipsa_name, .keep_all = TRUE)

unificar <- function(df) {
  df %>% transmute(ciudad, fecha = as.Date(fecha), Demo_Group, Sex = as.integer(Sex), Food, gramos)
}

gramos <- bind_rows(
  CoCA = readRDS(file.path(coca_dir, "coca_results.rds")) %>%
    mutate(gramos = quantity) %>% unificar(),
  CoNA = readRDS(file.path(cona_dir, "cona_results.rds"))$comp %>%
    mutate(gramos = quantity) %>% unificar(),
  CoRD = readRDS(file.path(cord_dir, "cord_results.rds"))$comp %>%
    left_join(tcac %>% select(Food = sipsa_name, g_intercambio = gramos_g_1_intercambio), by = "Food") %>%
    mutate(gramos = Number_Serving * g_intercambio) %>% unificar(),
  CoAHD = readRDS(file.path(coahd_dir, "coahd_results.rds"))$comp %>%
    mutate(gramos = Cantidad_g) %>% unificar(),
  .id = "modelo"
) %>%
  mutate(modelo = factor(modelo, levels = MODELOS))

# -----------------------------------------------------------------------
# 2. Aporte de nutrientes y adecuacion
# -----------------------------------------------------------------------
densidad <- tcac %>%
  select(Food = sipsa_name, all_of(setNames(NUTRIENTES$col_tcac, NUTRIENTES$nutriente))) %>%
  pivot_longer(-Food, names_to = "nutriente", values_to = "por_100g")

aporte <- gramos %>%
  inner_join(densidad, by = "Food", relationship = "many-to-many") %>%
  group_by(modelo, ciudad, fecha, Demo_Group, Sex, nutriente) %>%
  summarise(aporte = sum(gramos * por_100g / 100, na.rm = TRUE), .groups = "drop")

requerimientos <- readRDS(file.path(household_dir, "household_eer_ll.rds")) %>%
  select(ciudad, Demo_Group = Age, Sex, all_of(NUTRIENTES$nutriente)) %>%
  mutate(Sex = as.integer(Sex)) %>%
  pivot_longer(-c(ciudad, Demo_Group, Sex), names_to = "nutriente", values_to = "requerimiento")

adecuacion <- aporte %>%
  inner_join(requerimientos, by = c("ciudad", "Demo_Group", "Sex", "nutriente")) %>%
  mutate(ratio = 100 * aporte / requerimiento) %>%
  group_by(modelo, Demo_Group, Sex, nutriente) %>%
  summarise(ratio = mean(ratio), .groups = "drop") %>%
  recode_member(sex_col = "Sex", age_col = "Demo_Group") %>%
  left_join(NUTRIENTES %>% select(nutriente, etiqueta), by = "nutriente") %>%
  mutate(etiqueta = factor(etiqueta, levels = rev(NUTRIENTES$etiqueta)))

# El CoNA debe llegar a 100% en los nutrientes cuya restriccion se activa siempre
message("Chequeo CoNA (calcio, debe ser ~100%): ",
        paste(round(adecuacion$ratio[adecuacion$modelo == "CoNA" & adecuacion$nutriente == "Calcium"]), collapse = ", "))

# -----------------------------------------------------------------------
# Figura 12
# -----------------------------------------------------------------------
fig12 <- ggplot(adecuacion, aes(x = modelo, y = etiqueta, fill = pmin(ratio, 300))) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = paste0(round(ratio), "%")), size = 2.8, family = "serif", color = "grey10") +
  facet_wrap(~ member, nrow = 1) +
  scale_fill_gradient2(low = "#C0392B", mid = "white", high = "#2C3E6B", midpoint = 100,
                       limits = c(0, 300), name = "% del requerimiento") +
  labs(
    title    = "Adecuación nutricional de la dieta de cada modelo",
    subtitle = "Aporte de la dieta como porcentaje del requerimiento mínimo, promedio de las 13 ciudades y el trimestre",
    x = NULL, y = NULL,
    caption  = "Fuente: cálculos propios. Por debajo de 100% la dieta no cubre el requerimiento; el color se satura en 300%.\nNo se muestra el sodio, porque tiene límite máximo y no mínimo."
  ) +
  paper_theme() +
  theme(
    panel.grid        = element_blank(),
    axis.text.x       = element_text(angle = 0, hjust = 0.5, size = 9),
    legend.position   = "right",
    legend.key.height = unit(1.6, "cm"),
    legend.title      = element_text(size = 9)
  )

guardar_fig(fig12, "fig12_adecuacion_nutricional", "04_adecuacion", ancho = 12, alto = 6.75)

message("Listo. Figura 12 en: ", file.path(fig_dir, "04_adecuacion"))
