########################################################
## SCRIPT 05_figures/11_fig_robustez.R
## Pruebas de robustez, trimestre jul-sep 2025:
##
## Figura 20: costo con y sin azucares, por dieta
## Figura 21: razon del costo sin azucares entre el costo con azucares, por
##            ciudad, total y por 1000 kcal
## Figura 22: prima del CC-CoNA frente al CoNA segun alpha
## Figura 23: razon del CoAHD entre el CoRD por ciudad
##
## Reads:  azucar_dir/azucar_results.rds
##         ccona_dir/ccona_results.rds
##         coahd_dir, cord_dir (via leer_costos_modelos)
## Writes: fig_dir/07_robustez/fig20..fig23 (.png/.pdf)
########################################################

source("C:/Users/danie/OneDrive/Escritorio/Least-cost-diets-and-affordability/Proyecto Interno/interno/05_figures/00_fig_config.R")
library(tidyverse)
library(scales)
source(file.path(base_dir, "interno/05_figures/aux-functions/modelos_costos.R"))

azucar_dir <- file.path(base_dir, "interno/03_models/azucar")

MODELOS_AZUCAR <- c("CoNA", "CC-CoNA", "CoRD", "CoAHD")

# -----------------------------------------------------------------------
# Figura 20: costo con y sin azucares
# -----------------------------------------------------------------------
azucar <- readRDS(file.path(azucar_dir, "azucar_results.rds"))

costos_azucar <- azucar$razones_modelo %>%
  select(modelo, `Con azúcares` = pc_con, `Sin azúcares` = pc_sin) %>%
  pivot_longer(-modelo, names_to = "escenario", values_to = "pc") %>%
  mutate(modelo = factor(modelo, levels = MODELOS_AZUCAR),
         escenario = factor(escenario, levels = c("Con azúcares", "Sin azúcares")))

# Un panel por dieta, con su color; la barra sin azúcares es la misma tinta más clara.
# Así no hace falta leyenda y el color siempre es el de la dieta.
fig20 <- ggplot(costos_azucar, aes(x = escenario, y = pc, fill = modelo, alpha = escenario)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = comma(pc, accuracy = 1, big.mark = ".", decimal.mark = ",")),
            vjust = -0.5, size = 3.6, family = "serif", alpha = 1) +
  facet_wrap(~ modelo, nrow = 1) +
  scale_fill_manual(values = MODEL_COLS, guide = "none") +
  scale_alpha_manual(values = c("Con azúcares" = 1, "Sin azúcares" = 0.45), guide = "none") +
  scale_y_continuous(labels = cop_format(), expand = expansion(mult = c(0, 0.12))) +
  labs(title = "Costo de la dieta con y sin azúcares",
       subtitle = subtitulo_modelos(MODELOS_AZUCAR, "COP por persona y día, promedio de las 13 ciudades y del trimestre jul-sep 2025"),
       x = NULL, y = NULL,
       caption = paste0("Fuente: cálculos propios. Sin azúcares: salen azúcar morena, refinada y sulfitada y panela; en CoRD y CoAHD sale además el grupo de azúcares.\n",
                        "El CoCA no cambia porque solo usa cereales, raíces, tubérculos y plátanos. CC-CoNA con alpha = ", ALFA_TXT, ".")) +
  paper_theme(base_size = 12) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size = 10),
        panel.grid.major.x = element_blank())
guardar_fig(fig20, "fig20_costo_con_sin_azucar", "07_robustez", ancho = 11, alto = 7)

# -----------------------------------------------------------------------
# Figura 21: razon sin / con azucares por ciudad
# -----------------------------------------------------------------------
razones <- azucar$razones_ciudad %>%
  select(modelo, ciudad, `Costo total` = razon, `Costo por 1000 kcal` = razon_1000kcal) %>%
  pivot_longer(starts_with("Costo"), names_to = "medida", values_to = "razon") %>%
  mutate(modelo = factor(modelo, levels = MODELOS_AZUCAR),
         medida = factor(medida, levels = c("Costo total", "Costo por 1000 kcal"))) %>%
  recode_city()

orden_azucar <- razones %>%
  filter(medida == "Costo total", modelo == "CoNA") %>%
  arrange(razon) %>% pull(ciudad_lbl) %>% as.character()

fig21 <- razones %>%
  mutate(ciudad_lbl = factor(ciudad_lbl, levels = rev(orden_azucar))) %>%
  ggplot(aes(x = razon - 1, y = ciudad_lbl, fill = modelo)) +
  geom_col(width = 0.72) +
  geom_vline(xintercept = 0, color = "grey35", linewidth = 0.4) +
  geom_text(aes(label = paste0(ifelse(razon >= 1, "+", "-"), format(round(100 * abs(razon - 1), 1), nsmall = 1, decimal.mark = ","), "%"),
                hjust = ifelse(razon >= 1, -0.12, 1.12)), size = 2.9, family = "serif") +
  facet_grid(medida ~ modelo) +
  scale_fill_manual(values = MODEL_COLS, guide = "none") +
  scale_x_continuous(labels = function(x) paste0(ifelse(x > 0, "+", ""), format(round(100 * x, 0), decimal.mark = ",", trim = TRUE), "%"),
                     breaks = c(-0.05, 0, 0.05, 0.1), expand = expansion(mult = 0.28)) +
  labs(title = "Cambio en el costo de la dieta al quitar los azúcares",
       subtitle = subtitulo_modelos(MODELOS_AZUCAR,
                                    "Razón sin azúcares / con azúcares menos 1, por ciudad, promedio del trimestre jul-sep 2025"),
       x = NULL, y = NULL,
       caption = paste0("Fuente: cálculos propios. Una barra a la derecha de cero indica que la dieta sin azúcares cuesta más. ",
                        "El costo por 1000 kcal descuenta que CoRD y CoAHD pierden las calorías del grupo de azúcares.\n",
                        "CC-CoNA con alpha = ", ALFA_TXT, ".")) +
  paper_theme(base_size = 11) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size = 8),
        panel.grid.major.y = element_blank())
guardar_fig(fig21, "fig21_razon_sin_azucar", "07_robustez", ancho = 13, alto = 8.4)

# -----------------------------------------------------------------------
# Figura 22: prima del CC-CoNA
# -----------------------------------------------------------------------
cc <- readRDS(file.path(ccona_dir, "ccona_results.rds"))$cost

prima <- cc %>%
  group_by(alpha_val, ciudad, fecha) %>%
  summarise(hogar = sum(cost_day), completo = all(!is.na(cost_day)), .groups = "drop") %>%
  filter(completo) %>%
  group_by(alpha_val, ciudad) %>%
  summarise(pc = mean(hogar) / 3, meses = n(), .groups = "drop") %>%
  filter(meses == 3) %>%
  group_by(ciudad) %>%
  mutate(prima = pc / pc[alpha_val == 0]) %>%
  ungroup() %>%
  recode_city()

media_prima <- prima %>% group_by(alpha_val) %>% summarise(prima = mean(prima), n = n(), .groups = "drop")

fig22 <- ggplot(prima, aes(x = alpha_val, y = prima)) +
  geom_line(aes(group = ciudad_lbl), color = "grey75", linewidth = 0.5) +
  geom_line(data = media_prima, color = MODEL_COLS[["CC-CoNA"]], linewidth = 1.3) +
  geom_point(data = media_prima, color = MODEL_COLS[["CC-CoNA"]], size = 3) +
  geom_text(data = media_prima, aes(label = paste0(format(round(prima, 2), decimal.mark = ","), "×")),
            color = MODEL_COLS[["CC-CoNA"]], vjust = -1.1, size = 3.4, family = "serif") +
  scale_x_continuous(breaks = c(0, 0.25, 0.5, 0.75, 0.9)) +
  scale_y_continuous(labels = number_format(accuracy = 0.1, decimal.mark = ",", suffix = "×"),
                     expand = expansion(mult = c(0.03, 0.1))) +
  labs(title = "Cuánto cuesta que la dieta se parezca a la composición del IPC",
       subtitle = subtitulo_modelos(c("CoNA", "CC-CoNA"),
                                    "Costo per cápita del CC-CoNA dividido entre el del CoNA, según alpha (línea de color: promedio de las ciudades; en gris, cada ciudad)"),
       x = "alpha (fracción de la composición del IPC que se exige)", y = NULL,
       caption = paste0("Fuente: cálculos propios. Con alpha = 0 el CC-CoNA es el CoNA. ",
                        "Con alpha = 0,9 hay solución en ", media_prima$n[media_prima$alpha_val == 0.9],
                        " de las 13 ciudades y con alpha = 1 en ninguna.")) +
  paper_theme(base_size = 11) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))
guardar_fig(fig22, "fig22_prima_ccona", "07_robustez", ancho = 12, alto = 6.75)

# -----------------------------------------------------------------------
# Figura 23: CoAHD frente al CoRD
# -----------------------------------------------------------------------
pc <- costo_percapita(leer_costos_modelos()) %>%
  filter(modelo %in% c("CoRD", "CoAHD")) %>%
  pivot_wider(names_from = modelo, values_from = pc) %>%
  mutate(razon = CoAHD / CoRD) %>%
  recode_city() %>%
  mutate(ciudad_lbl = fct_reorder(ciudad_lbl, razon))

fig23 <- ggplot(pc, aes(x = razon - 1, y = ciudad_lbl)) +
  geom_col(fill = MODEL_COLS[["CoAHD"]], width = 0.72) +
  geom_vline(xintercept = 0, color = "grey35", linewidth = 0.4) +
  geom_text(aes(label = paste0(format(round(razon, 3), nsmall = 3, decimal.mark = ",")),
                hjust = ifelse(razon < 1, 1.15, -0.15)), size = 3.2, family = "serif") +
  scale_x_continuous(labels = function(x) paste0(ifelse(x > 0, "+", ""), format(round(100 * x, 0), decimal.mark = ",", trim = TRUE), "%"),
                     expand = expansion(mult = 0.15)) +
  labs(title = "CoAHD frente a CoRD",
       subtitle = subtitulo_modelos(c("CoRD", "CoAHD"),
                                    "Razón CoAHD / CoRD menos 1, costo per cápita, promedio del trimestre jul-sep 2025"),
       x = NULL, y = NULL,
       caption = "Fuente: cálculos propios. Una barra a la izquierda de cero indica que la dieta saludable de la FAO cuesta menos que la dieta recomendada; el número es la razón.") +
  paper_theme(base_size = 11) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5), panel.grid.major.y = element_blank())
guardar_fig(fig23, "fig23_razon_coahd_cord", "07_robustez", ancho = 12, alto = 6.75)

message("Figuras de robustez guardadas en: ", file.path(fig_dir, "07_robustez"))
