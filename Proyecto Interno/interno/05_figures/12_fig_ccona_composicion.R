########################################################
## SCRIPT 05_figures/12_fig_ccona_composicion.R
## Composicion de la dieta del CC-CoNA frente a la del IPC.
##
## Figura 24: participacion en las cantidades de la dieta de cada subclase
##            del IPC (las 12 mas pesadas), en el IPC y en la dieta del CoNA
##            (alpha = 0) y del CC-CoNA con alpha de 0,5 y 0,75.
##
## Reads:  ccona_dir/ccona_results.rds
##         output_dir/ipc/ipc_participaciones.rds
## Writes: fig_dir/07_robustez/fig24_ccona_composicion.png/.pdf
########################################################

source("C:/Users/danie/OneDrive/Escritorio/Least-cost-diets-and-affordability/Proyecto Interno/interno/05_figures/00_fig_config.R")
library(tidyverse)
library(scales)

cc <- readRDS(file.path(ccona_dir, "ccona_results.rds"))$shares
ipc <- readRDS(file.path(output_dir, "ipc/ipc_participaciones.rds"))

ALFAS <- c(0, 0.5, 0.75)

# Participacion observada de cada subclase en la dieta, promedio de miembros,
# ciudades y meses con solucion (la participacion de las subclases del panel
# suma 1 en cada dieta)
observada <- cc %>%
  filter(alpha_val %in% ALFAS) %>%
  group_by(alpha_val, Group) %>%
  summarise(share = mean(share_observada), .groups = "drop") %>%
  rename(subclase_codigo = Group)

principales <- ipc %>% slice_max(share, n = 12) %>% pull(subclase_codigo)

datos <- bind_rows(
  ipc %>% transmute(subclase_codigo, subclase_nombre, serie = "IPC", share),
  observada %>% left_join(select(ipc, subclase_codigo, subclase_nombre), by = "subclase_codigo") %>%
    transmute(subclase_codigo, subclase_nombre,
              serie = if_else(alpha_val == 0, "CoNA (alpha = 0)",
                              paste0("CC-CoNA (alpha = ", sub(".", ",", as.character(alpha_val), fixed = TRUE), ")")),
              share)
) %>%
  filter(subclase_codigo %in% principales) %>%
  mutate(serie = factor(serie, levels = c("IPC", "CoNA (alpha = 0)", "CC-CoNA (alpha = 0,5)", "CC-CoNA (alpha = 0,75)")),
         subclase_nombre = str_wrap(str_remove(subclase_nombre, " \\(.*"), 28),
         subclase_nombre = factor(subclase_nombre, levels = rev(unique(subclase_nombre[order(match(subclase_codigo, principales))]))))

colores <- c("IPC" = "grey70", "CoNA (alpha = 0)" = MODEL_COLS[["CoNA"]],
             "CC-CoNA (alpha = 0,5)" = MODEL_COLS[["CC-CoNA"]], "CC-CoNA (alpha = 0,75)" = "#7A2E12")

fig24 <- ggplot(datos, aes(x = share, y = subclase_nombre, fill = serie)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.75) +
  scale_fill_manual(values = colores, name = NULL) +
  scale_x_continuous(labels = percent_format(accuracy = 1, decimal.mark = ","), expand = expansion(mult = c(0, 0.05))) +
  guides(fill = guide_legend(reverse = FALSE, nrow = 1)) +
  labs(title = "Qué parte de la dieta ocupa cada subclase del IPC",
       subtitle = subtitulo_modelos(c("CoNA", "CC-CoNA"),
                                    "Participación en las cantidades de la dieta, promedio de miembros, ciudades y meses; las 12 subclases con más peso en el IPC"),
       x = NULL, y = NULL,
       caption = "Fuente: cálculos propios. Las participaciones del IPC son la ponderación del gasto reescalada sobre las subclases que tienen alimentos en el panel.") +
  paper_theme(base_size = 11) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5), panel.grid.major.y = element_blank())
guardar_fig(fig24, "fig24_ccona_composicion", "07_robustez", ancho = 12, alto = 7.5)

message("Figura 24 guardada en: ", file.path(fig_dir, "07_robustez"))
