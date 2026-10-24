########################################################
## Estimación de los precios minoristas usando IPC subclase
## Full sample (DANE observed 1999-01..2018-03)
## Forward-fill to latest IPC month available (e.g., 2025)
## Cities: CALI, BOGOTÁ D.C., MEDELLÍN
########################################################

# -----------------------------
# 0. Packages
# -----------------------------
library(tidyverse)
library(readxl)
library(janitor)
library(lubridate)
library(writexl)

# -----------------------------
# 1. Directorios
# -----------------------------

# Definir directorio base
base_dir <- "C:/Users/Portatil/Desktop/Least-cost-diets-and-affordability/Proyecto Interno"

# Definir directorio de input
out_dir   <- file.path(base_dir, "food-security-paper/output/forecasting_fullsample")
prices_extended = read_csv(file.path(out_dir, 
                                     "prices_extended_city_article_month.csv"))

# -----------------------------------------------
# 2. Preparación del IPC por subclase
# -----------------------------------------------
# Filtrar datos
price_data = prices_extended %>% 
  # add date as date
  mutate(date = ymd(fecha)) %>%
  filter(date >= "2019-01-01",
         date < "2025-01-01")

# Definir clase
price_data$cod_clase = paste0(substr(price_data$subclase_ipc, 1,
                                          4), "0000")

# Seleccionar variables y agregar
plot_price_data = price_data %>%
  group_by(date, ciudad, cod_clase, subclase_ipc) %>%
  summarise(ipc = unique(ipc))



# Vector de meses
meses_esp <- c("Ene","Feb","Mar","Abr",
               "May","Jun","Jul","Ago","Sep","Oct","Nov","Dic")
# Recuperar el IPC por clase
ipc_clase_raw = readxl::read_excel(file.path(base_dir, 
                                         "var-ipc/IPC-clase.xls")) %>%
  clean_names()

ipc_clase <- ipc_clase_raw %>%
  mutate(
    ciudad = case_when(
      ciudad == "CARTAGENA DE INDIAS" ~ "CARTAGENA",
      ciudad == "BOGOTÁ, D.C." ~ "BOGOTÁ D.C.",
      TRUE ~ ciudad
    ),
    cod_clase = substr(clase, 1, 8),
    mes_num = match(mes, meses_esp),
    ano = as.integer(ano),
    ipc_clase = as.numeric(numero_indice),
    date = make_date(ano, mes_num)
  ) %>%
  select(ciudad, clase, cod_clase, date, ano, mes_num, ipc_clase) %>%
  filter(!is.na(date), !is.na(ipc_clase), 
         !is.na(cod_clase), 
         !is.na(ciudad)) %>%
  arrange(ciudad, cod_clase, date)

# Agregar la información del IPC por clase
plot_price_data = merge(plot_price_data, ipc_clase, by = c("date",
                                                           "ciudad",
                                                           "cod_clase"))
# Nombre de la clase 
clase_labels <- c(
  "01110000 - Pan Y Cereales"          = "01110000 - Bread & cereals",
  "01120000 - Carnes"                                                = "01120000 - Meat",
  "01130000 - Pescado"                                               = "01130000 - Fish & seafood",
  "01140000 - Leche, Queso Y Huevos"                                 = "01140000 - Dairy, cheese & eggs",
  "01150000 - Aceites Y Grasas"                                      = "01150000 - Oils & fats",
  "01160000 - Frutas"                                                = "01160000 - Fruits",
  "01170000 - Legumbres"                                             = "01170000 - Vegetables",
  "01180000 - Azúcar, Mermelada, Miel, Chocolate Y Dulces De Azúcar" = "01180000 - Sugar, jam & confectionery",
  "01190000 - Productos Alimenticios No Incluidos Anteriormente"     = "01190000 - Other food products",
  "01210000 - Café, Té Y Cacao"                                      = "01210000 - Coffee, tea & cocoa"
)


# -----------------------------------------------
# 3. Figure paper: IPC index (2018-01 - 2024-12)
# -----------------------------------------------

# Opción 1: Usando valor de la clase
clase_plot_data = plot_price_data %>% select(date, ciudad, clase,
                                             cod_clase, ipc_clase) %>%
  distinct()

city_labels <- c(
  "BOGOTÁ D.C." = "Bogotá",
  "MEDELLÍN"    = "Medellín",
  "CALI"        = "Cali"
)

clase_plot_data = plot_price_data %>% 
  select(date, ciudad, clase, cod_clase, ipc_clase) %>%
  distinct() %>%
  arrange(ciudad, cod_clase, date) %>%
  group_by(ciudad, cod_clase) %>%
  mutate(ipc_yoy = (ipc_clase / lag(ipc_clase, 12) - 1) * 100) %>%
  ungroup() %>%
  filter(!is.na(ipc_yoy))

p_clase1 = clase_plot_data %>%
  mutate(clase_en = clase_labels[clase],
         ciudad_en = city_labels[ciudad]) %>%
  ggplot(aes(x = date, y = ipc_yoy, 
             color    = ciudad_en,
             linetype = ciudad_en)) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 0, linewidth = 0.4, 
             linetype = "solid", color = "black") +
  facet_wrap(~clase_en, ncol = 4, 
             scales = "free") +
  scale_color_manual(
    name   = NULL,
    values = c("Bogotá"   = "#E63946",
               "Medellín" = "#457B9D",
               "Cali"     = "#2A9D8F")
  ) +
  scale_linetype_manual(
    name   = NULL,
    values = c("Bogotá"   = "solid",
               "Medellín" = "dashed",
               "Cali"     = "dotdash")
  ) +
  labs(x = NULL, 
       y = "Year-on-year price change (%)") +
  theme_bw(base_size = 11) +
  theme(
    legend.position       = "top",
    strip.text            = element_text(face = "bold"),
    panel.grid.minor      = element_blank(),
    legend.background     = element_rect(fill = "white", color = "black", 
                                         linewidth = 0.5),
    legend.box.background = element_rect(color = "black", linewidth = 0.5),
    legend.margin         = margin(5, 10, 5, 10)
  )

ggsave(file.path(out_dir, 
                 "210326_figure_clase_yoy.png"),
       p_clase1, width = 12, height = 8, 
       dpi = 300)

ggsave(file.path(out_dir, 
                 "210326_figure_clase.png"),
       p_clase1, width = 12, height = 8, 
       dpi = 300)

