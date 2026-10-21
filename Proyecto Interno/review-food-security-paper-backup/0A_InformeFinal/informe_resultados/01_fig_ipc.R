########################################################
## informe_resultados/fig_ipc_yoy_clase.R
##
## Figura: variación interanual del IPC por clase
##   ipc_yoy_t = (IPC_t / IPC_{t-12} - 1) × 100
##
## Organización: facet por clase (10 clases División 01)
##               una línea por ciudad
## Insumo: IPC-clase.xls (directo, sin depender del
##         pipeline de precios extendidos)
##
## Output:
##   output/figuras/fig_ipc_yoy_clase.png
########################################################

library(tidyverse)
library(readxl)
library(janitor)
library(lubridate)

# -------------------------------------------------------
# PATHS
# -------------------------------------------------------
base_dir <- "C:/Users/Portatil/Desktop/Least-cost-diets-and-affordability/Proyecto Interno/"

in_ipc_clase <- file.path(base_dir, "var-ipc/IPC-clase.xls")

out_dir     <- file.path(base_dir, "review-food-security-paper/0A_InformeFinal/informe_resultados/output")
out_figuras <- file.path(out_dir, "figuras")
out_tablas  <- file.path(out_dir, "tablas")

dir.create(out_figuras, showWarnings = FALSE, recursive = TRUE)
dir.create(out_tablas,  showWarnings = FALSE, recursive = TRUE)

# -------------------------------------------------------
# PARÁMETROS
# -------------------------------------------------------
CITY_COLORS <- c("Bogotá" = "#E63946", "Medellín" = "#457B9D", "Cali" = "#27AE60")
CITY_LINES  <- c("Bogotá" = "solid",   "Medellín" = "dashed",  "Cali" = "dotdash")

meses_esp <- c("Ene","Feb","Mar","Abr","May","Jun",
               "Jul","Ago","Sep","Oct","Nov","Dic")

make_date <- function(ano, mes_num) {
  as.Date(sprintf("%04d-%02d-01", as.integer(ano), as.integer(mes_num)))
}

# Etiquetas de clase en español (División 01 COICOP)
clase_labels <- c(
  "01110000 - Pan Y Cereales"                                        = "Pan y cereales",
  "01120000 - Carnes"                                                = "Carnes",
  "01130000 - Pescado"                                               = "Pescado y mariscos",
  "01140000 - Leche, Queso Y Huevos"                                 = "Lácteos, queso y huevos",
  "01150000 - Aceites Y Grasas"                                      = "Aceites y grasas",
  "01160000 - Frutas"                                                = "Frutas",
  "01170000 - Legumbres"                                             = "Verduras y legumbres",
  "01180000 - Azúcar, Mermelada, Miel, Chocolate Y Dulces De Azúcar" = "Azúcar y confitería",
  "01190000 - Productos Alimenticios No Incluidos Anteriormente"     = "Otros alimentos",
  "01210000 - Café, Té Y Cacao"                                      = "Café, té y cacao"
)

# -------------------------------------------------------
# 1. CARGAR IPC POR CLASE
# -------------------------------------------------------
message("Cargando IPC por clase...")

ipc_clase <- read_excel(in_ipc_clase) %>%
  clean_names() %>%
  mutate(
    ciudad = case_when(
      ciudad == "CARTAGENA DE INDIAS" ~ "CARTAGENA",
      ciudad == "BOGOTÁ, D.C."        ~ "BOGOTÁ D.C.",
      TRUE ~ ciudad
    ),
    ciudad_label = case_when(
      ciudad == "BOGOTÁ D.C." ~ "Bogotá",
      ciudad == "MEDELLÍN"    ~ "Medellín",
      ciudad == "CALI"        ~ "Cali",
      TRUE ~ NA_character_
    ),
    cod_clase   = substr(clase, 1, 8),
    mes_num     = match(mes, meses_esp),
    ano         = as.integer(ano),
    ipc_clase   = as.numeric(numero_indice),
    fecha       = make_date(ano, mes_num)
  ) %>%
  select(ciudad, ciudad_label, clase, cod_clase, fecha, ipc_clase) %>%
  filter(
    !is.na(fecha), !is.na(ipc_clase), !is.na(ciudad_label),
    # Solo División 01 (alimentos y bebidas no alcohólicas)
    str_starts(cod_clase, "011") | str_starts(cod_clase, "012")
  ) %>%
  distinct(ciudad, clase, fecha, .keep_all = TRUE) %>%
  arrange(ciudad, clase, fecha)

message(sprintf("  %d filas | %d ciudades | %d clases | %s a %s",
                nrow(ipc_clase), n_distinct(ipc_clase$ciudad),
                n_distinct(ipc_clase$clase),
                min(ipc_clase$fecha), max(ipc_clase$fecha)))

# -------------------------------------------------------
# 2. CALCULAR VARIACIÓN INTERANUAL
# -------------------------------------------------------
message("Calculando variación interanual...")

plot_data <- ipc_clase %>%
  group_by(ciudad, clase) %>%
  arrange(fecha) %>%
  mutate(ipc_yoy = (ipc_clase / lag(ipc_clase, 12) - 1) * 100) %>%
  ungroup() %>%
  filter(!is.na(ipc_yoy)) %>%
  mutate(
    clase_label  = clase_labels[clase],
    ciudad_label = factor(ciudad_label, levels = names(CITY_COLORS))
  ) %>%
  filter(!is.na(clase_label))   # solo clases con etiqueta asignada

message(sprintf("  %d filas con variación interanual | período: %s a %s",
                nrow(plot_data), min(plot_data$fecha), max(plot_data$fecha)))

# -------------------------------------------------------
# 3. FIGURA
# -------------------------------------------------------
message("Generando figura...")

p <- ggplot(plot_data,
            aes(x = fecha, y = ipc_yoy,
                color    = ciudad_label,
                linetype = ciudad_label)) +
  geom_line(linewidth = 0.9) +
  geom_hline(yintercept = 0, linewidth = 0.4,
             linetype = "solid", color = "black") +
  facet_wrap(~ clase_label, ncol = 4, scales = "free_y") +
  scale_color_manual(name = NULL, values = CITY_COLORS) +
  scale_linetype_manual(name = NULL, values = CITY_LINES) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  labs(
    x = NULL,
    y = "Variación interanual (%)"
  ) +
  theme_bw(base_size = 11) +
  theme(
    legend.position       = "top",
    legend.background     = element_rect(fill = "white", color = "black",
                                         linewidth = 0.5),
    legend.margin         = margin(5, 10, 5, 10),
    strip.background      = element_rect(fill = "grey92"),
    strip.text            = element_text(face = "bold", size = 9),
    panel.grid.minor      = element_blank(),
    axis.text.x           = element_text(angle = 45, hjust = 1, size = 8)
  )

out_path <- file.path(out_figuras, "fig_ipc_yoy_clase.png")
ggsave(out_path, p, width = 14, height = 9, dpi = 300)
message("  Figura guardada: ", out_path)

# -------------------------------------------------------
# 4. TABLA RESUMEN: variación promedio anual por clase y ciudad
# -------------------------------------------------------
tab_resumen <- plot_data %>%
  mutate(ano = year(fecha)) %>%
  group_by(clase_label, ciudad_label, ano) %>%
  summarise(var_promedio = mean(ipc_yoy, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = ciudad_label, values_from = var_promedio) %>%
  arrange(clase_label, ano)

writexl::write_xlsx(
  list(ipc_yoy_clase = tab_resumen),
  file.path(out_tablas, "tab_ipc_yoy_clase.xlsx")
)

message("  Tabla guardada: tab_ipc_yoy_clase.xlsx")
message("\nListo. Outputs en: ", out_dir)