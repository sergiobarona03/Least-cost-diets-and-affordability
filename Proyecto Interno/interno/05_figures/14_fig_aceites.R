########################################################
## SCRIPT 05_figures/14_fig_aceites.R
## Precio por litro de los aceites segun marca y tamano de la presentacion, Bogota.
## Soporte de docs/prueba_aceites.Rmd.
##
## Figura 28: precio por litro de cada presentacion y producto elegido con cada regla
##            (umbral de 500 g, umbral de 1.000 ml y sin umbral)
##
## Reads:  output_dir/lista_alimentos/skus_todos.rds
##         output_dir/lista_alimentos/lista_alimentos.xlsx
##         output_dir/sensibilidad_umbral/sin_umbral_tamano/insumos/lista_alimentos/lista_alimentos.xlsx
##         01_webscrap_prep/aux-functions/seleccionar_sku_mediano.R
## Writes: fig_dir/07_robustez/fig28_aceites_precio_tamano (.png/.pdf)
########################################################

source(file.path(Sys.getenv("PROYECTO_DIR", unset = "C:/Users/danie/OneDrive/Escritorio/Least-cost-diets-and-affordability/Proyecto Interno"),
                 "interno/05_figures/00_fig_config.R"))
library(tidyverse)
library(openxlsx)
source(file.path(base_dir, "interno/01_webscrap_prep/aux-functions/seleccionar_sku_mediano.R"))

ACEITES <- c("Aceite girasol", "Aceite soya", "Aceite vegetal mezcla")
DENSIDAD_ACEITE <- 0.92   # g/ml, la misma que usa el panel
PRECIO_ALTO <- 14000      # COP por litro: separa las marcas economicas de las caras

skus   <- readRDS(file.path(output_dir, "lista_alimentos/skus_todos.rds"))
actual <- read.xlsx(file.path(output_dir, "lista_alimentos/lista_alimentos.xlsx")) %>% mutate(sku_code = as.character(sku_code))
sin_um <- read.xlsx(file.path(output_dir, "sensibilidad_umbral/sin_umbral_tamano/insumos/lista_alimentos/lista_alimentos.xlsx")) %>%
  mutate(sku_code = as.character(sku_code))
sel_500 <- seleccionar_sku_mediano(skus %>% filter(sipsa_name %in% ACEITES),
                                   umbral_gramos = 500, umbral_ml = 500 / DENSIDAD_ACEITE) %>%
  mutate(sku_code = as.character(sku_code))

llave <- function(d) paste(d$sipsa_name, d$sku_code)

productos <- skus %>%
  filter(sipsa_name %in% ACEITES, city == "Bogot\u00e1", fechas_mes_min >= 2) %>%
  mutate(ml = gramos_sku / densidad,
         litro = precio_mediano / ml * 1000,
         marca = str_to_title(str_extract(exito_name, "[A-Z\u00c1\u00c9\u00cd\u00d3\u00da\u00d1]{4,}")),
         marca = recode(marca, Exito = "\u00c9xito"),
         aceite = str_remove(sipsa_name, "Aceite "),
         aceite = factor(aceite, levels = c("girasol", "soya", "vegetal mezcla"),
                         labels = c("Girasol", "Soya", "Vegetal mezcla")))

elegidos <- bind_rows(
  productos %>% filter(llave(.) %in% llave(sel_500 %>% filter(city == "Bogot\u00e1"))) %>% mutate(regla = "Umbral de 500 g"),
  productos %>% filter(llave(.) %in% llave(actual %>% filter(city == "Bogot\u00e1"))) %>% mutate(regla = "Umbral de 1.000 ml"),
  productos %>% filter(llave(.) %in% llave(sin_um %>% filter(city == "Bogot\u00e1"))) %>% mutate(regla = "Sin umbral")
) %>% mutate(regla = factor(regla, levels = c("Umbral de 500 g", "Umbral de 1.000 ml", "Sin umbral")))

MARCA_COLS <- c("Frescampo" = "#1B7F5C", "\u00c9xito" = "#8DBF7E",
                "Diana" = "#C0392B", "Oleocali" = "#8E5A2B", "Premier" = "#6C4AA0", "Gourmet" = "#D98E04")

fig28 <- ggplot(productos, aes(x = ml, y = litro)) +
  geom_hline(yintercept = PRECIO_ALTO, linetype = "dotted", color = "grey55", linewidth = 0.5) +
  geom_vline(xintercept = 1000, linetype = "dashed", color = "grey55", linewidth = 0.5) +
  geom_point(aes(color = marca), size = 3.4, alpha = 0.9) +
  geom_point(data = elegidos, aes(shape = regla, size = regla), stroke = 1.1, color = "black", fill = NA) +
  facet_wrap(~ aceite, nrow = 1) +
  scale_color_manual(values = MARCA_COLS, name = "Marca") +
  scale_shape_manual(values = c("Umbral de 500 g" = 5, "Umbral de 1.000 ml" = 0, "Sin umbral" = 2), name = "Producto elegido con") +
  scale_size_manual(values = c("Umbral de 500 g" = 9, "Umbral de 1.000 ml" = 7, "Sin umbral" = 5.5), name = "Producto elegido con") +
  scale_x_log10(breaks = c(500, 1000, 2000, 3000), labels = function(x) format(x, big.mark = ".", decimal.mark = ","),
                expand = expansion(mult = 0.12)) +
  scale_y_continuous(labels = function(x) format(x, big.mark = ".", decimal.mark = ","),
                     expand = expansion(mult = c(0.05, 0.08))) +
  labs(title = "Precio por litro de los aceites, por marca y tama\u00f1o de la presentaci\u00f3n",
       subtitle = stringr::str_wrap(paste0("Bogot\u00e1, productos con al menos dos fechas de precio por mes. La l\u00ednea punteada horizontal marca ",
                         format(PRECIO_ALTO, big.mark = ".", decimal.mark = ","), " COP por litro; la vertical, 1.000 ml."),
                         width = round(150 * FIG_ESCALA)),
       x = "Mililitros de la presentaci\u00f3n (escala logar\u00edtmica)", y = "COP por litro",
       caption = "Fuente: cálculos propios con los precios del scraping. Cada punto es una presentación; las figuras negras marcan el producto que elige cada regla.") +
  paper_theme(base_size = 12) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5),
        legend.box = "vertical", legend.spacing.y = unit(0.1, "cm"),
        legend.title = element_text(face = "bold", size = 11),
        panel.grid.minor = element_blank()) +
  guides(color = guide_legend(order = 1, nrow = 1, override.aes = list(size = 4), title.position = "left"),
         shape = guide_legend(order = 2, nrow = 1, title.position = "left"),
         size = guide_legend(order = 2, nrow = 1, title.position = "left"))

guardar_fig(fig28, "fig28_aceites_precio_tamano", "07_robustez", ancho = 13, alto = 7.8)
message("fig28 guardada")
