########################################################
## 04_figures/02_fig_costos_coca_cona.R
##
## Costo diario de las dietas CoCA (adecuacion calorica) y
## CoNA (adecuacion nutricional) para el hogar representativo
## (hombre adulto 31-50, mujer adulta 31-50, nina 9-13) en las
## 13 ciudades principales de Colombia, trimestre 3 de 2025
## (julio, agosto, septiembre).
##
## Figura 1: series de linea del costo diario, individualizadas
##           por miembro del hogar (filas) y por dieta (columnas).
## Figura 2: series de linea del costo per capita del hogar
##           representativo, individualizadas por dieta.
## Figura 3: razon CoNA/CoCA per capita en el tiempo, por ciudad.
##
## Reads:  coca_dir/coca_results.rds
##         cona_dir/cona_results.rds  (elemento $cost)
##
## Writes: fig_dir/01_costos/fig01_costo_lineas_miembro.png/.pdf
##         fig_dir/01_costos/fig02_costo_percapita_lineas.png/.pdf
##         fig_dir/01_costos/fig03_razon_cona_coca.png/.pdf
########################################################

source("interno/04_figures/00_fig_config.R")
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

fecha_breaks <- sort(unique(df_costos$fecha))

# -----------------------------------------------------------------------
# 2. Figura 1 — series de linea: costo diario por miembro (filas) x
##   dieta (columnas), una linea por ciudad
# -----------------------------------------------------------------------
fig1 <- ggplot(df_costos,
              aes(x = fecha, y = cost_day, color = ciudad_lbl)) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 1.6) +
  facet_grid(member ~ model) +
  city_scale_color(name = NULL) +
  scale_x_date(breaks = fecha_breaks, date_labels = "%b") +
  scale_y_continuous(labels = cop_format()) +
  labs(
    title = "Costo diario de la dieta por miembro del hogar",
    subtitle = "13 ciudades · 3T 2025",
    x = NULL, y = "COP / día",
    caption = "Fuente: cálculos propios. Hogar representativo: hombre adulto, mujer adulta y niña."
  ) +
  paper_theme() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 8),
    strip.text  = element_text(size = 9),
    legend.position = "right"
  ) +
  guides(color = guide_legend(ncol = 1))

ggsave(file.path(fig_dir, "01_costos", "fig01_costo_lineas_miembro.png"),
       fig1, width = 10, height = 8, dpi = 300, bg = "white")
ggsave(file.path(fig_dir, "01_costos", "fig01_costo_lineas_miembro.pdf"),
       fig1, width = 10, height = 8)
message("Figura 1 guardada.")

# -----------------------------------------------------------------------
# 3. Costo per capita del hogar representativo
##   (suma de los 3 miembros / 3)
# -----------------------------------------------------------------------
N_MEMBERS <- df_costos %>% distinct(Demo_Group, Sex) %>% nrow()

percapita <- df_costos %>%
  group_by(model, ciudad, ciudad_lbl, fecha) %>%
  summarise(cost_hogar = sum(cost_day, na.rm = TRUE),
            n_miembros = n(),
            .groups = "drop") %>%
  mutate(cost_percapita = cost_hogar / n_miembros)

# -----------------------------------------------------------------------
# 4. Figura 2 — series de linea: costo per capita, individualizado por
##   dieta (columnas), una linea por ciudad
# -----------------------------------------------------------------------
fig2 <- ggplot(percapita,
              aes(x = fecha, y = cost_percapita, color = ciudad_lbl)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.8) +
  facet_wrap(~ model, nrow = 1) +
  city_scale_color(name = NULL) +
  scale_x_date(breaks = fecha_breaks, date_labels = "%b") +
  scale_y_continuous(labels = cop_format()) +
  labs(
    title = "Costo per cápita del hogar representativo",
    subtitle = "13 ciudades · 3T 2025",
    x = NULL, y = "COP / día per cápita",
    caption = "Fuente: cálculos propios. Costo per cápita = costo total del hogar representativo / 3 miembros."
  ) +
  paper_theme() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 8),
    strip.text  = element_text(size = 10),
    legend.position = "right"
  ) +
  guides(color = guide_legend(ncol = 1))

ggsave(file.path(fig_dir, "01_costos", "fig02_costo_percapita_lineas.png"),
       fig2, width = 11, height = 6, dpi = 300, bg = "white")
ggsave(file.path(fig_dir, "01_costos", "fig02_costo_percapita_lineas.pdf"),
       fig2, width = 11, height = 6)
message("Figura 2 guardada.")

# -----------------------------------------------------------------------
# 5. Figura 3 — razon CoNA / CoCA per capita, por ciudad, en el tiempo
# -----------------------------------------------------------------------
razon <- percapita %>%
  select(model, ciudad, ciudad_lbl, fecha, cost_percapita) %>%
  pivot_wider(names_from = model, values_from = cost_percapita) %>%
  mutate(razon_cona_coca = CoNA / CoCA)

fig3 <- ggplot(razon,
              aes(x = fecha, y = razon_cona_coca, color = ciudad_lbl)) +
  geom_hline(yintercept = 1, color = "grey50", linetype = "dashed",
            linewidth = 0.4) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.8) +
  city_scale_color(name = NULL) +
  scale_x_date(breaks = fecha_breaks, date_labels = "%b") +
  scale_y_continuous(labels = function(x) sprintf("%.1fx", x)) +
  labs(
    title = "Razón entre el costo del CoNA y el costo del CoCA",
    subtitle = "Costo per cápita del hogar representativo · 13 ciudades · 3T 2025",
    x = NULL,
    y = "CoNA / CoCA",
    caption = "Fuente: cálculos propios. Un valor de 2.0x indica que el CoNA cuesta el doble que el CoCA en esa ciudad y mes."
  ) +
  paper_theme() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size = 8))

ggsave(file.path(fig_dir, "01_costos", "fig03_razon_cona_coca.png"),
       fig3, width = 10, height = 6, dpi = 300, bg = "white")
ggsave(file.path(fig_dir, "01_costos", "fig03_razon_cona_coca.pdf"),
       fig3, width = 10, height = 6)
message("Figura 3 guardada.")

message("Listo. Figuras en: ", file.path(fig_dir, "01_costos"))
