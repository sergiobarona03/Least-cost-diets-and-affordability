########################################################
## 04_figures/aux-functions/food_groups_gaba.R
##
## Clasificacion GABA (Guias Alimentarias Basadas en Alimentos)
## de cada alimento que puede aparecer en la composicion CoNA.
## Es un metadato del alimento -- no depende de que metodo de
## precio se uso (v1 o v2), asi que este archivo es compartido
## entre 04_figures/03_fig_cona_composicion.R y
## v1/04_figures/03_fig_cona_composicion.R.
##
## Si un alimento nuevo aparece seleccionado en el CoNA (de
## cualquiera de las dos versiones) y no esta en food_groups de
## abajo, el script que lo usa avisa con un warning
## ("Alimentos sin grupo GABA asignado") -- agregarlo aqui una
## sola vez arregla ambos scripts.
##
## Expone: food_groups, grupo_order, grupo_hue_pair, food_palette
########################################################

library(dplyr)
library(tibble)

food_groups <- tribble(
  ~Food,                             ~grupo,
  "Leche en polvo",                  "Lácteos",
  "Queso campesino",                 "Lácteos",
  "Queso cuajada",                   "Lácteos",
  "Arveja enlatada",                 "Cárnicos, huevos y leguminosas",
  "Arveja amarilla seca importada",  "Cárnicos, huevos y leguminosas",
  "Blanquillo entero fresco",        "Cárnicos, huevos y leguminosas",
  "Camarón tití precocido seco",     "Cárnicos, huevos y leguminosas",
  "Carne de cerdo, tocino papada",   "Cárnicos, huevos y leguminosas",
  "Carne de cerdo, espinazo",        "Cárnicos, huevos y leguminosas",
  "Carne de cerdo, tocino barriga",  "Cárnicos, huevos y leguminosas",
  "Carne de res, cogote",            "Cárnicos, huevos y leguminosas",
  "Carne de res, murillo",           "Cárnicos, huevos y leguminosas",
  "Carne de res molida, murillo",    "Cárnicos, huevos y leguminosas",
  "Fríjol cabeza negra nacional",    "Cárnicos, huevos y leguminosas",
  "Garbanzo importado",              "Cárnicos, huevos y leguminosas",
  "Huevo rojo A",                    "Cárnicos, huevos y leguminosas",
  "Menudencias de pollo",            "Cárnicos, huevos y leguminosas",
  "Pescado cabezas",                 "Cárnicos, huevos y leguminosas",
  "Arracacha blanca",                "Cereales, raíces, tubérculos y plátanos",
  "Arroz de segunda",                "Cereales, raíces, tubérculos y plátanos",
  "Arroz excelso",                   "Cereales, raíces, tubérculos y plátanos",
  "Avena en hojuelas",               "Cereales, raíces, tubérculos y plátanos",
  "Harina de trigo",                 "Cereales, raíces, tubérculos y plátanos",
  "Harina precocida de maíz",        "Cereales, raíces, tubérculos y plátanos",
  "Maíz amarillo cáscara",           "Cereales, raíces, tubérculos y plátanos",
  "Maíz amarillo trillado",          "Cereales, raíces, tubérculos y plátanos",
  "Maíz blanco cáscara",             "Cereales, raíces, tubérculos y plátanos",
  "Maíz blanco trillado",            "Cereales, raíces, tubérculos y plátanos",
  "Maíz pira",                       "Cereales, raíces, tubérculos y plátanos",
  "Papa capira",                     "Cereales, raíces, tubérculos y plátanos",
  "Plátano hartón verde",            "Cereales, raíces, tubérculos y plátanos",
  "Acelga",                          "Frutas y verduras",
  "Ahuyama",                         "Frutas y verduras",
  "Ajo importado",                   "Frutas y verduras",
  "Cebolla cabezona blanca",         "Frutas y verduras",
  "Cebolla cabezona roja",           "Frutas y verduras",
  "Cebolla junca",                   "Frutas y verduras",
  "Ciruela roja",                    "Frutas y verduras",
  "Espinaca",                        "Frutas y verduras",
  "Guanábana",                       "Frutas y verduras",
  "Guayaba manzana",                 "Frutas y verduras",
  "Guayaba pera",                    "Frutas y verduras",
  "Habichuela",                      "Frutas y verduras",
  "Limón Tahití",                    "Frutas y verduras",
  "Mandarina Oneco",                 "Frutas y verduras",
  "Papaya Paulina",                  "Frutas y verduras",
  "Perejil",                         "Frutas y verduras",
  "Pimentón",                        "Frutas y verduras",
  "Pimentón verde",                  "Frutas y verduras",
  "Tangelo",                         "Frutas y verduras",
  "Tomate de árbol",                 "Frutas y verduras",
  "Zanahoria",                       "Frutas y verduras",
  "Aceite girasol",                  "Grasas",
  "Aceite soya",                     "Grasas",
  "Aceite vegetal mezcla",           "Grasas",
  "Azúcar morena",                   "Azúcares",
  "Azúcar refinada",                 "Azúcares",
  "Azúcar sulfitada",                "Azúcares",
  "Bocadillo veleño",                "Azúcares",
  "Sal yodada",                      "Otro"
)

grupo_order <- c("Lácteos", "Cárnicos, huevos y leguminosas",
                 "Cereales, raíces, tubérculos y plátanos",
                 "Frutas y verduras", "Grasas", "Azúcares", "Otro")

grupo_hue_pair <- list(
  "Lácteos"                                 = c("#D6EAF8", "#1A5276"),
  "Cárnicos, huevos y leguminosas"          = c("#F5B7B1", "#7B241C"),
  "Cereales, raíces, tubérculos y plátanos" = c("#FDEBD0", "#CA6F1E"),
  "Frutas y verduras"                       = c("#D5F5E3", "#1E8449"),
  "Grasas"                                  = c("#FCF3CF", "#B7950B"),
  "Azúcares"                                = c("#E8DAEF", "#6C3483"),
  "Otro"                                    = c("#D5D8DC", "#5D6D7E")
)

food_groups <- food_groups %>%
  mutate(grupo = factor(grupo, levels = grupo_order)) %>%
  arrange(grupo, Food)

food_palette <- food_groups %>%
  group_by(grupo) %>%
  group_map(~ {
    pal <- grupo_hue_pair[[as.character(.y$grupo)]]
    n   <- nrow(.x)
    if (n == 1) tibble(Food = .x$Food, color = pal[2])
    else tibble(Food = .x$Food, color = colorRampPalette(pal)(n))
  }) %>%
  bind_rows() %>%
  deframe()
