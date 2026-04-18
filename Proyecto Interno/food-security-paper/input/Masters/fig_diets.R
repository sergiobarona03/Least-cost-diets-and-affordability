# =========================================================
# Plot year-on-year variation of CoCA, CoNA, and
#          CoRD against 8 food CPI classes
# =========================================================

library(readxl)
library(dplyr)
library(lubridate)
library(ggplot2)
library(janitor)
library(stringr)
library(tidyr)

# =========================
# 1. PATHS
# =========================

ipc_path  <- "C:/Users/danie/OneDrive/Escritorio/Least-cost-diets-and-affordability/Proyecto Interno/food-security-paper/input/Masters/IPC-clase.xls"
diet_path <- "C:/Users/danie/OneDrive/Escritorio/Least-cost-diets-and-affordability/Proyecto Interno/food-security-paper/output/real"
out_path  <- dirname(ipc_path)

# =========================
# 2. IPC CLASSES TO KEEP
# =========================

clases_ipc <- c(
  "01110000 - Pan Y Cereales",
  "01120000 - Carnes",
  "01130000 - Pescado",
  "01140000 - Leche, Queso Y Huevos",
  "01150000 - Aceites Y Grasas",
  "01160000 - Frutas",
  "01170000 - Legumbres",
  "01180000 - Azúcar, Mermelada, Miel, Chocolate Y Dulces De Azúcar"
)

# =========================
# 3. TRANSLATIONS
# =========================

traductor <- c(
  "Pan Y Cereales" = "Bread and cereals",
  "Carnes" = "Meat",
  "Pescado" = "Fish",
  "Leche, Queso Y Huevos" = "Milk, cheese and eggs",
  "Aceites Y Grasas" = "Oils and fats",
  "Frutas" = "Fruits",
  "Legumbres" = "Legumes",
  "Azúcar, Mermelada, Miel, Chocolate Y Dulces De Azúcar" = "Sugar, jam, honey, chocolate and sweets"
)

# =========================
# 4. COLORS AND LINE TYPES
# =========================

color_values <- c(
  "CoCA" = "#1B1B1B",
  "CoNA" = "#1B1B1B",
  "CoRD" = "#1B1B1B",
  "Bread and cereals" = "#1F3A5F",
  "Meat" = "#8C2D04",
  "Fish" = "#2B6C8E",
  "Milk, cheese and eggs" = "#6A4C93",
  "Oils and fats" = "#B07D00",
  "Fruits" = "#2E6F40",
  "Legumes" = "#7A3E2B",
  "Sugar, jam, honey, chocolate and sweets" = "#6B7280"
)

linetype_values <- c(
  "CoCA" = "solid",
  "CoNA" = "solid",
  "CoRD" = "solid",
  "Bread and cereals" = "dashed",
  "Meat" = "dotted",
  "Fish" = "dotdash",
  "Milk, cheese and eggs" = "longdash",
  "Oils and fats" = "twodash",
  "Fruits" = "dashed",
  "Legumes" = "dotted",
  "Sugar, jam, honey, chocolate and sweets" = "dotdash"
)

# =========================
# 5. READ IPC
# =========================

ipc <- read_excel(ipc_path) %>%
  clean_names() %>%
  mutate(
    mes = trimws(as.character(mes)),
    mes_num = case_when(
      mes == "Ene" ~ 1L,
      mes == "Feb" ~ 2L,
      mes == "Mar" ~ 3L,
      mes == "Abr" ~ 4L,
      mes == "May" ~ 5L,
      mes == "Jun" ~ 6L,
      mes == "Jul" ~ 7L,
      mes == "Ago" ~ 8L,
      mes == "Sep" ~ 9L,
      mes == "Oct" ~ 10L,
      mes == "Nov" ~ 11L,
      mes == "Dic" ~ 12L,
      TRUE ~ NA_integer_
    ),
    ano = as.integer(ano),
    fecha = make_date(ano, mes_num, 1)
  )

ipc_yoy <- ipc %>%
  filter(clase %in% clases_ipc) %>%
  select(ciudad, fecha, clase, numero_indice) %>%
  filter(!is.na(fecha), !is.na(numero_indice), !is.na(ciudad), !is.na(clase)) %>%
  arrange(ciudad, clase, fecha) %>%
  group_by(ciudad, clase) %>%
  mutate(
    ipc_yoy = (numero_indice / lag(numero_indice, 12) - 1) * 100
  ) %>%
  ungroup() %>%
  mutate(
    ciudad = case_when(
      ciudad == "BOGOTÁ, D.C." ~ "BOGOTA",
      ciudad == "MEDELLÍN" ~ "MEDELLIN",
      ciudad == "CALI" ~ "CALI",
      TRUE ~ ciudad
    ),  
    grupo_nombre = str_replace(clase, "^\\d+\\s*-\\s*", ""),
    grupo_nombre = recode(grupo_nombre, !!!traductor)
  ) %>%
  filter(!is.na(ipc_yoy), !is.na(grupo_nombre)) %>%
  select(ciudad, fecha, grupo_nombre, ipc_yoy)

# =========================
# 6. COCA
# =========================

coca <- read_excel(file.path(diet_path, "coca_real.xlsx")) %>%
  clean_names() %>%
  mutate(fecha = as.Date(fecha))

coca_yoy <- coca %>%
  dplyr::group_by(ciudad, fecha) %>%
  dplyr::summarise(cost_day_real = mean(cost_day_real, na.rm = TRUE), .groups = "drop") %>%
  dplyr::filter(!is.na(ciudad), !is.na(fecha), !is.na(cost_day_real)) %>%
  arrange(ciudad, fecha) %>%
  dplyr::group_by(ciudad) %>%
  dplyr::mutate(
    diet_yoy = (cost_day_real / lag(cost_day_real, 12) - 1) * 100
  ) %>%
  ungroup() %>%
  filter(!is.na(diet_yoy))

coca_plot <- coca_yoy %>%
  left_join(ipc_yoy, by = c("ciudad", "fecha")) %>%
  filter(!is.na(grupo_nombre), !is.na(ipc_yoy))

p_coca <- ggplot(coca_plot, aes(x = fecha)) +
  geom_line(
    aes(y = ipc_yoy, color = grupo_nombre, linetype = grupo_nombre),
    linewidth = 0.8
  ) +
  geom_line(
    aes(y = diet_yoy, color = "CoCA", linetype = "CoCA"),
    linewidth = 1.1
  ) +
  facet_wrap(~ciudad, nrow = 1, scales = "free_y") +
  scale_color_manual(values = color_values, breaks = c(
    "CoCA", "Bread and cereals", "Meat", "Fish", "Milk, cheese and eggs",
    "Oils and fats", "Fruits", "Legumes", "Sugar, jam, honey, chocolate and sweets"
  )) +
  scale_linetype_manual(values = linetype_values, breaks = c(
    "CoCA", "Bread and cereals", "Meat", "Fish", "Milk, cheese and eggs",
    "Oils and fats", "Fruits", "Legumes", "Sugar, jam, honey, chocolate and sweets"
  )) +
  labs(
    x = NULL,
    y = "Year-on-year variation (%)",
    color = NULL,
    linetype = NULL,
    title = "Cost of Caloric Adequacy and food CPI classes"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom",
    legend.box = "vertical",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold")
  )

ggsave(
  filename = file.path(out_path, "coca_yoy.png"),
  plot = p_coca,
  width = 15,
  height = 4.8,
  dpi = 300
)

# =========================
# 7. CONA
# =========================

cona <- read_excel(file.path(diet_path, "cona_real.xlsx")) %>%
  clean_names() %>%
  dplyr::mutate(fecha = as.Date(fecha))

cona_yoy <- cona %>%
  dplyr::group_by(ciudad, fecha) %>%
  dplyr::summarise(cost_day_real = mean(cost_day_real, na.rm = TRUE), .groups = "drop") %>%
  dplyr::filter(!is.na(ciudad), !is.na(fecha), !is.na(cost_day_real)) %>%
  dplyr::arrange(ciudad, fecha) %>%
  dplyr::group_by(ciudad) %>%
  dplyr::mutate(
    diet_yoy = (cost_day_real / lag(cost_day_real, 12) - 1) * 100
  ) %>%
  ungroup() %>%
  filter(!is.na(diet_yoy))

cona_plot <- cona_yoy %>%
  left_join(ipc_yoy, by = c("ciudad", "fecha")) %>%
  filter(!is.na(grupo_nombre), !is.na(ipc_yoy)) 

p_cona <- ggplot(cona_plot, aes(x = fecha)) +
  geom_line(
    aes(y = ipc_yoy, color = grupo_nombre, linetype = grupo_nombre),
    linewidth = 0.8
  ) +
  geom_line(
    aes(y = diet_yoy, color = "CoNA", linetype = "CoNA"),
    linewidth = 1.1
  ) +
  facet_wrap(~ciudad, nrow = 1, scales = "free_y") +
  scale_color_manual(values = color_values, breaks = c(
    "CoNA", "Bread and cereals", "Meat", "Fish", "Milk, cheese and eggs",
    "Oils and fats", "Fruits", "Legumes", "Sugar, jam, honey, chocolate and sweets"
  )) +
  scale_linetype_manual(values = linetype_values, breaks = c(
    "CoNA", "Bread and cereals", "Meat", "Fish", "Milk, cheese and eggs",
    "Oils and fats", "Fruits", "Legumes", "Sugar, jam, honey, chocolate and sweets"
  )) +
  labs(
    x = NULL,
    y = "Year-on-year variation (%)",
    color = NULL,
    linetype = NULL,
    title = "Cost of Nutritional Adequacy and food CPI classes"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom",
    legend.box = "vertical",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold")
  )

ggsave(
  filename = file.path(out_path, "cona_yoy.png"),
  plot = p_cona,
  width = 15,
  height = 4.8,
  dpi = 300
)

# =========================
# 8. CORD
# =========================

cord <- read_excel(file.path(diet_path, "cord_real.xlsx")) %>%
  clean_names() %>%
  dplyr::mutate(fecha = as.Date(fecha))

cord_yoy <- cord %>%
  dplyr::group_by(ciudad, fecha) %>%
  dplyr::summarise(cost_day_real = mean(cost_day_real, na.rm = TRUE), .groups = "drop") %>%
  dplyr::filter(!is.na(ciudad), !is.na(fecha), !is.na(cost_day_real)) %>%
  arrange(ciudad, fecha) %>%
  dplyr::group_by(ciudad) %>%
  mutate(
    diet_yoy = (cost_day_real / lag(cost_day_real, 12) - 1) * 100
  ) %>%
  ungroup() %>%
  filter(!is.na(diet_yoy))

cord_plot <- cord_yoy %>%
  left_join(ipc_yoy, by = c("ciudad", "fecha")) %>%
  filter(!is.na(grupo_nombre), !is.na(ipc_yoy))

p_cord <- ggplot(cord_plot, aes(x = fecha)) +
  geom_line(
    aes(y = ipc_yoy, color = grupo_nombre, linetype = grupo_nombre),
    linewidth = 0.8
  ) +
  geom_line(
    aes(y = diet_yoy, color = "CoRD", linetype = "CoRD"),
    linewidth = 1.1
  ) +
  facet_wrap(~ciudad, nrow = 1, scales = "free_y") +
  scale_color_manual(values = color_values, breaks = c(
    "CoRD", "Bread and cereals", "Meat", "Fish", "Milk, cheese and eggs",
    "Oils and fats", "Fruits", "Legumes", "Sugar, jam, honey, chocolate and sweets"
  )) +
  scale_linetype_manual(values = linetype_values, breaks = c(
    "CoRD", "Bread and cereals", "Meat", "Fish", "Milk, cheese and eggs",
    "Oils and fats", "Fruits", "Legumes", "Sugar, jam, honey, chocolate and sweets"
  )) +
  labs(
    x = NULL,
    y = "Year-on-year variation (%)",
    color = NULL,
    linetype = NULL,
    title = "Cost of Recommended Diet and food CPI classes"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom",
    legend.box = "vertical",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold")
  )

ggsave(
  filename = file.path(out_path, "cord_yoy.png"),
  plot = p_cord,
  width = 15,
  height = 4.8,
  dpi = 300
)