########################################################
## v1/04_figures/02_fig_costos_coca_cona.R
## Version v1: mismo script, usando v1/04_figures/00_fig_config.R
## (que apunta a v1/03_models y v1/output).
##
## Costo trimestral (promedio 3T 2025: julio, agosto,
## septiembre) de las dietas CoCA (adecuacion calorica) y CoNA
## (adecuacion nutricional) para el hogar representativo
## (hombre adulto 31-50, mujer adulta 31-50, nina 9-13) en las
## 13 ciudades principales de Colombia.
##
## Con 13 ciudades, las series de linea por ciudad se saturan y
## no se distinguen bien; se usan barras horizontales ordenadas
## de mayor a menor costo, una por ciudad, en su lugar.
##
## Figura 1: costo diario promedio del trimestre, individualizado
##           por miembro del hogar (filas) y por dieta (columnas).
## Figura 2: costo per capita promedio del trimestre del hogar
##           representativo, individualizado por dieta.
## Figura 3: razon CoNA/CoCA per capita, promedio del trimestre,
##           por ciudad.
##
## Reads:  coca_dir/coca_results.rds
##         cona_dir/cona_results.rds  (elemento $cost)
##
## Writes: fig_dir/01_costos/fig01_costo_miembro_barras.png/.pdf
##         fig_dir/01_costos/fig02_costo_percapita_barras.png/.pdf
##         fig_dir/01_costos/fig03_razon_cona_coca_barras.png/.pdf
########################################################

source("C:/Users/danie/OneDrive/Escritorio/Least-cost-diets-and-affordability/Proyecto Interno/interno/v1/04_figures/00_fig_config.R")
library(tidyverse)
library(scales)

# -----------------------------------------------------------------------
# 1. Cargar datos
# -----------------------------------------------------------------------
df_coca <- readRDS(file.path(coca_dir, "coca_results.rds")) %>%
  mutate(fecha = as.Date(fecha), model = "CoCA") %>%
  select(model, ciudad, fecha, year, mes, Demo_Group, Sex,
         cost_day, Cost_1000kcal)

df_cona <- readRDS(file.path(cona_dir, "cona_results.rds"))$cost %>%
  mutate(fecha = as.Date(fecha), model = "CoNA") %>%
  rename(cost_day = cona_cost) %>%
  select(model, ciudad, fecha, year, mes, Demo_Group, Sex,
         cost_day, Cost_1000kcal)

df_costos <- bind_rows(df_coca, df_cona) %>%
  mutate(model = factor(model, levels = c("CoCA", "CoNA"))) %>%
  recode_member() %>%
  recode_city()

message(sprintf("Costos cargados: %d filas | %d ciudades | %d fechas | modelos: %s",
                nrow(df_costos), n_distinct(df_costos$ciudad),
                n_distinct(df_costos$fecha),
                paste(levels(df_costos$model), collapse = ", ")))

# -----------------------------------------------------------------------
# 2. Costo diario promedio del trimestre, por miembro x ciudad x modelo
# -----------------------------------------------------------------------
avg_miembro <- df_costos %>%
  group_by(model, ciudad, ciudad_lbl, member) %>%
  summarise(cost_avg = mean(cost_day, na.rm = TRUE), .groups = "drop")

# Orden de ciudades: costo promedio general (todas las dietas y miembros),
# de mayor a menor. Mismo orden en las tres figuras para poder comparar.
orden_ciudades <- avg_miembro %>%
  group_by(ciudad_lbl) %>%
  summarise(overall = mean(cost_avg, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(overall)) %>%
  pull(ciudad_lbl)

avg_miembro <- avg_miembro %>%
  mutate(ciudad_lbl = factor(ciudad_lbl, levels = rev(orden_ciudades)))

# -----------------------------------------------------------------------
# 3. Figura 1 — barras horizontales: costo diario promedio del trimestre,
##   por miembro (filas) x dieta (columnas), una barra por ciudad
# -----------------------------------------------------------------------
fig1 <- ggplot(avg_miembro,
              aes(x = cost_avg, y = ciudad_lbl, fill = ciudad_lbl)) +
  geom_col(width = 0.72) +
  geom_text(aes(label = comma(round(cost_avg), big.mark = ".")),
            hjust = -0.12, size = 2.5, family = "serif", color = "grey20") +
  facet_grid(member ~ model) +
  city_scale_fill(guide = "none") +
  scale_x_continuous(labels = cop_format(),
                     expand = expansion(mult = c(0, 0.18))) +
  labs(
    title = "Costo diario de la dieta por miembro del hogar",
    x = "COP / día", y = NULL,
    caption = "Fuente: cálculos propios. Hogar representativo: hombre adulto, mujer adulta y niña."
  ) +
  paper_theme() +
  theme(
    axis.text.y = element_text(size = 8),
    strip.text  = element_text(size = 9),
    legend.position = "none"
  )

ggsave(file.path(fig_dir, "01_costos", "fig01_costo_miembro_barras.png"),
       fig1, width = 10, height = 8, dpi = 300, bg = "white")
ggsave(file.path(fig_dir, "01_costos", "fig01_costo_miembro_barras.pdf"),
       fig1, width = 10, height = 8)
message("Figura 1 guardada.")

# -----------------------------------------------------------------------
# 4. Costo per capita del hogar representativo, promedio del trimestre
##   (suma de los 3 miembros / 3, promediada sobre jul-ago-sep)
# -----------------------------------------------------------------------
N_MEMBERS <- df_costos %>% distinct(Demo_Group, Sex) %>% nrow()

percapita_mes <- df_costos %>%
  group_by(model, ciudad, ciudad_lbl, fecha) %>%
  summarise(cost_hogar = sum(cost_day, na.rm = TRUE),
            n_miembros = n(),
            .groups = "drop") %>%
  mutate(cost_percapita = cost_hogar / n_miembros)

percapita_avg <- percapita_mes %>%
  group_by(model, ciudad, ciudad_lbl) %>%
  summarise(cost_percapita = mean(cost_percapita, na.rm = TRUE),
            .groups = "drop")

orden_percapita <- percapita_avg %>%
  filter(model == "CoNA") %>%
  arrange(desc(cost_percapita)) %>%
  pull(ciudad_lbl)

percapita_avg <- percapita_avg %>%
  mutate(ciudad_lbl = factor(ciudad_lbl, levels = rev(orden_percapita)))

# -----------------------------------------------------------------------
# 5. Figura 2 — barras horizontales: costo per capita promedio del
##   trimestre, individualizado por dieta (columnas)
# -----------------------------------------------------------------------
fig2 <- ggplot(percapita_avg,
              aes(x = cost_percapita, y = ciudad_lbl, fill = ciudad_lbl)) +
  geom_col(width = 0.72) +
  geom_text(aes(label = comma(round(cost_percapita), big.mark = ".")),
            hjust = -0.12, size = 2.6, family = "serif", color = "grey20") +
  facet_wrap(~ model, nrow = 1) +
  city_scale_fill(guide = "none") +
  scale_x_continuous(labels = cop_format(),
                     expand = expansion(mult = c(0, 0.18))) +
  labs(
    title = "Costo per cápita del hogar representativo",
    x = "COP / día per cápita", y = NULL,
    caption = "Fuente: cálculos propios. Costo per cápita = costo total del hogar representativo / 3 miembros."
  ) +
  paper_theme() +
  theme(
    axis.text.y = element_text(size = 9),
    strip.text  = element_text(size = 10),
    legend.position = "none"
  )

ggsave(file.path(fig_dir, "01_costos", "fig02_costo_percapita_barras.png"),
       fig2, width = 10, height = 6, dpi = 300, bg = "white")
ggsave(file.path(fig_dir, "01_costos", "fig02_costo_percapita_barras.pdf"),
       fig2, width = 10, height = 6)
message("Figura 2 guardada.")

# -----------------------------------------------------------------------
# 6. Figura 3 — barras horizontales: razon CoNA / CoCA per capita,
##   promedio del trimestre, por ciudad
# -----------------------------------------------------------------------
razon <- percapita_avg %>%
  select(model, ciudad_lbl, cost_percapita) %>%
  mutate(ciudad_lbl = as.character(ciudad_lbl)) %>%
  pivot_wider(names_from = model, values_from = cost_percapita) %>%
  mutate(razon_cona_coca = CoNA / CoCA) %>%
  arrange(razon_cona_coca) %>%
  mutate(ciudad_lbl = factor(ciudad_lbl, levels = ciudad_lbl))

fig3 <- ggplot(razon, aes(x = razon_cona_coca, y = ciudad_lbl, fill = ciudad_lbl)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = sprintf("%.1fx", razon_cona_coca)),
            hjust = -0.15, size = 2.8, family = "serif", color = "grey20") +
  city_scale_fill(guide = "none") +
  scale_x_continuous(labels = function(x) sprintf("%.1fx", x),
                     expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "Razón entre el costo del CoNA/CoCA",
    x = "CoNA / CoCA",
    y = NULL,
    caption = "Fuente: cálculos propios. Un valor de 2.0x indica que el CoNA cuesta el doble que el CoCA en esa ciudad."
  ) +
  paper_theme() +
  theme(
    axis.text.y = element_text(size = 9),
    legend.position = "none"
  )

ggsave(file.path(fig_dir, "01_costos", "fig03_razon_cona_coca_barras.png"),
       fig3, width = 9, height = 6, dpi = 300, bg = "white")
ggsave(file.path(fig_dir, "01_costos", "fig03_razon_cona_coca_barras.pdf"),
       fig3, width = 9, height = 6)
message("Figura 3 guardada.")


# -----------------------------------------------------------------------
# 7. Costo por 1000 kcal, por miembro x ciudad x modelo
##   A diferencia de cost_day, Cost_1000kcal ya viene normalizado por el
##   requerimiento energetico (EER) de cada miembro -- aisla que tan
##   eficiente es el precio de la dieta, sin el efecto de que el hombre
##   adulto simplemente necesita mas kcal que la nina. Mismo orden de
##   ciudades que las figuras anteriores, para poder comparar entre si.
# -----------------------------------------------------------------------
avg_miembro_kcal <- df_costos %>%
  group_by(model, ciudad, ciudad_lbl, member) %>%
  summarise(cost_avg = mean(Cost_1000kcal, na.rm = TRUE), .groups = "drop") %>%
  mutate(ciudad_lbl = factor(ciudad_lbl, levels = rev(orden_ciudades)))

fig1_kcal <- ggplot(avg_miembro_kcal,
              aes(x = cost_avg, y = ciudad_lbl, fill = ciudad_lbl)) +
  geom_col(width = 0.72) +
  geom_text(aes(label = comma(round(cost_avg), big.mark = ".")),
            hjust = -0.12, size = 2.5, family = "serif", color = "grey20") +
  facet_grid(member ~ model) +
  city_scale_fill(guide = "none") +
  scale_x_continuous(labels = cop_format(),
                     expand = expansion(mult = c(0, 0.18))) +
  labs(
    title = "Costo de la dieta por 1000 kcal, por miembro del hogar",
    x = "COP / 1000 kcal", y = NULL,
    caption = "Fuente: cálculos propios. Costo normalizado por el requerimiento energético (EER) de cada miembro."
  ) +
  paper_theme() +
  theme(
    axis.text.y = element_text(size = 8),
    strip.text  = element_text(size = 9),
    legend.position = "none"
  )

ggsave(file.path(fig_dir, "01_costos", "fig01c_costo_miembro_1000kcal_barras.png"),
       fig1_kcal, width = 10, height = 8, dpi = 300, bg = "white")
ggsave(file.path(fig_dir, "01_costos", "fig01c_costo_miembro_1000kcal_barras.pdf"),
       fig1_kcal, width = 10, height = 8)
message("Figura 1c (1000 kcal) guardada.")

# -----------------------------------------------------------------------
# 8. Costo por 1000 kcal, promedio del hogar representativo x modelo
##   Promedio simple entre los 3 miembros -- Cost_1000kcal ya es una
##   medida de intensidad (precio por caloria), no de gasto total, asi
##   que aqui NO se suma ni se divide por n_miembros como en la figura
##   de costo per capita en COP/dia.
# -----------------------------------------------------------------------
hogar_kcal <- df_costos %>%
  group_by(model, ciudad, ciudad_lbl) %>%
  summarise(cost_1000kcal = mean(Cost_1000kcal, na.rm = TRUE), .groups = "drop") %>%
  mutate(ciudad_lbl = factor(ciudad_lbl, levels = rev(orden_percapita)))

fig2_kcal <- ggplot(hogar_kcal,
              aes(x = cost_1000kcal, y = ciudad_lbl, fill = ciudad_lbl)) +
  geom_col(width = 0.72) +
  geom_text(aes(label = comma(round(cost_1000kcal), big.mark = ".")),
            hjust = -0.12, size = 2.6, family = "serif", color = "grey20") +
  facet_wrap(~ model, nrow = 1) +
  city_scale_fill(guide = "none") +
  scale_x_continuous(labels = cop_format(),
                     expand = expansion(mult = c(0, 0.18))) +
  labs(
    title = "Costo de la dieta por 1000 kcal, hogar representativo",
    x = "COP / 1000 kcal", y = NULL,
    caption = "Fuente: cálculos propios. Promedio simple entre los 3 miembros del hogar (medida de intensidad, no de gasto total)."
  ) +
  paper_theme() +
  theme(
    axis.text.y = element_text(size = 9),
    strip.text  = element_text(size = 10),
    legend.position = "none"
  )

ggsave(file.path(fig_dir, "01_costos", "fig02c_costo_hogar_1000kcal_barras.png"),
       fig2_kcal, width = 10, height = 6, dpi = 300, bg = "white")
ggsave(file.path(fig_dir, "01_costos", "fig02c_costo_hogar_1000kcal_barras.pdf"),
       fig2_kcal, width = 10, height = 6)
message("Figura 2c (1000 kcal) guardada.")

# -----------------------------------------------------------------------
# 9. Verificacion: la razon CoNA/CoCA no cambia entre COP/dia y
##   COP/1000kcal, porque ambos modelos se resuelven sujetos al MISMO
##   EER (restriccion de igualdad) -- el EER se cancela en la razon.
##   No se genera una figura nueva para esto; solo se deja el chequeo
##   en el log para que quede documentado.
# -----------------------------------------------------------------------
razon_kcal_check <- hogar_kcal %>%
  select(model, ciudad_lbl, cost_1000kcal) %>%
  mutate(ciudad_lbl = as.character(ciudad_lbl)) %>%
  pivot_wider(names_from = model, values_from = cost_1000kcal) %>%
  mutate(razon_1000kcal = CoNA / CoCA) %>%
  left_join(razon %>% mutate(ciudad_lbl = as.character(ciudad_lbl)) %>%
              select(ciudad_lbl, razon_cona_coca),
            by = "ciudad_lbl")

diff_max <- max(abs(razon_kcal_check$razon_1000kcal - razon_kcal_check$razon_cona_coca), na.rm = TRUE)
message(sprintf(
  "Chequeo: diferencia maxima entre razon (COP/dia) y razon (COP/1000kcal) = %.4f (deberia ser ~0)",
  diff_max))

message("Listo. Figuras en: ", file.path(fig_dir, "01_costos"))
