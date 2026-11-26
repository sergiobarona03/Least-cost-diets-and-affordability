# =============================================================================
# Mapa del contexto del estudio: 13 ciudades y áreas metropolitanas
#
# Panel izquierdo: pobreza monetaria 2025 (color, en clases) y población (tamaño)
# Panel derecho:   encarecimiento relativo acumulado de los alimentos,
#                  diciembre de 2021 a septiembre de 2025 (color, en clases)
#
# Formato pensado para una revista Elsevier a doble columna (180 mm de ancho).
# Sin título, subtítulo ni nota dentro de la figura: van en el caption de LaTeX.
# =============================================================================

library(sf)
library(dplyr)
library(ggplot2)
library(ggspatial)
library(ggrepel)      # versión 0.9.0 o superior (para el halo blanco de las etiquetas)
library(readxl)
library(cowplot)

dir_shapes <- "C:/Users/Portatil/Desktop/multilevel-listings/data/ShapeFiles"
dir_study  <- "C:/Users/Portatil/Desktop/Least-cost-diets-and-affordability/Proyecto Interno/paper-global-food/study-setting"
dir_output <- file.path(dir_study, "output")

# -----------------------------------------------------------------------------
# 0. Parámetros de diseño (se ajustan aquí sin tocar el resto del código)
# -----------------------------------------------------------------------------
ancho_mm <- 180
alto_mm  <- 120

col_oceano    <- "#E8F1F7"   # mar
col_vecinos   <- "#E2E2E2"   # países vecinos
col_colombia  <- "#FBFAF6"   # departamentos de Colombia
col_dptos     <- "gray70"    # límites departamentales
col_pais      <- "gray25"    # límite nacional

# Paletas secuenciales (ColorBrewer), legibles también en escala de grises
pal_pobreza <- c("#FFFFB2", "#FECC5C", "#FD8D3C", "#F03B20", "#BD0026")   # YlOrRd, 5 clases
pal_precio  <- c("#F1EEF6", "#BDC9E1", "#74A9CF", "#0570B0")              # PuBu, 4 clases

# Clases de las leyendas (los cortes se pueden cambiar aquí)
cortes_pobreza   <- c(-Inf, 15, 20, 25, 30, Inf)
etiq_pobreza     <- c("< 15", "15\u201320", "20\u201325", "25\u201330", "\u2265 30")
cortes_precio    <- c(-Inf, 5, 7.5, 10, Inf)
etiq_precio      <- c("< 5", "5\u20137.5", "7.5\u201310", "\u2265 10")
etiq_sin_dato    <- "Not available"

# -----------------------------------------------------------------------------
# 1. Capas geográficas (se leen de la carpeta del otro proyecto)
# -----------------------------------------------------------------------------
col_mpios <- st_read(file.path(dir_shapes, "DANE_geodata/MGN_ANM_MPIOS.shp"), quiet = TRUE) %>%
  filter(DPTO_CCDGO != "88") %>%                      # sin San Andrés
  st_transform(3116)

col_deptos <- col_mpios %>%
  group_by(DPTO_CCDGO) %>%
  summarise(.groups = "drop")

col_contorno <- st_union(col_deptos)

latin_america <- st_read(file.path(dir_shapes, "World/world.shp"), quiet = TRUE) %>%
  filter(region_wb == "Latin America & Caribbean", admin != "Colombia") %>%
  st_transform(3116)

# Extensión del mapa: Colombia continental con un margen pequeño
bb     <- st_bbox(col_deptos)
margen <- 0.05
xr <- bb[["xmax"]] - bb[["xmin"]]
yr <- bb[["ymax"]] - bb[["ymin"]]
lim_x <- c(bb[["xmin"]] - xr * margen, bb[["xmax"]] + xr * margen)
lim_y <- c(bb[["ymin"]] - yr * margen, bb[["ymax"]] + yr * margen)

bb_crop <- st_bbox(c(xmin = lim_x[1], xmax = lim_x[2], ymin = lim_y[1], ymax = lim_y[2]),
                   crs = st_crs(col_deptos))
latin_america <- suppressWarnings(st_crop(st_make_valid(latin_america), bb_crop))

# Rótulos de referencia (países y mares), en coordenadas geográficas
rotulos_ref <- tribble(
  ~texto,             ~lat,   ~lon,   ~tipo,
  "Caribbean Sea",    12.3,  -75.8,   "mar",   # movido al este para no chocar con la flecha del norte
  "Pacific Ocean",     3.2,  -78.6,   "mar",
  "Panama",            8.4,  -78.7,   "pais",
  "Venezuela",         6.6,  -67.6,   "pais",
  "Brazil",           -1.6,  -68.2,   "pais",
  "Peru",             -3.4,  -74.6,   "pais",
  "Ecuador",          -1.2,  -78.1,   "pais"
) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  st_transform(3116)
xy_ref <- st_coordinates(rotulos_ref)
rotulos_ref$X <- xy_ref[, 1]
rotulos_ref$Y <- xy_ref[, 2]

# -----------------------------------------------------------------------------
# 2. Datos de las ciudades
#    pobreza_2025: DANE, pobreza monetaria 2025 (serie A.M. para las siete
#                  áreas metropolitanas; serie de capitales para las demás)
#    lat, lon:     centro urbano de la ciudad principal
#    La población y el precio relativo se leen de los Excel ya generados.
# -----------------------------------------------------------------------------
ciudades <- tribble(
  ~ciudad,              ~ciudad_ipc,            ~lat,    ~lon,     ~pobreza_2025,
  "Bogotá",             "BOGOTÁ, D.C.",          4.711,  -74.072,  17.8,
  "Medellín A.M.",      "MEDELLÍN",              6.244,  -75.581,  14.4,
  "Cali A.M.",          "CALI",                  3.452,  -76.532,  19.9,
  "Barranquilla A.M.",  "BARRANQUILLA",         10.964,  -74.796,  25.6,
  "Bucaramanga A.M.",   "BUCARAMANGA",           7.119,  -73.123,  26.7,
  "Manizales A.M.",     "MANIZALES",             5.069,  -75.517,  13.0,
  "Pereira A.M.",       "PEREIRA",               4.814,  -75.694,  20.4,
  "Cúcuta A.M.",        "CÚCUTA",                7.894,  -72.508,  31.5,
  "Pasto",              "PASTO",                 1.214,  -77.281,  29.3,
  "Ibagué",             "IBAGUÉ",                4.439,  -75.232,  25.6,
  "Montería",           "MONTERÍA",              8.748,  -75.881,  30.7,
  "Cartagena",          "CARTAGENA DE INDIAS",  10.391,  -75.479,  34.6,
  "Villavicencio",      "VILLAVICENCIO",         4.142,  -73.627,  23.4
)

poblacion <- read_excel(file.path(dir_output, "poblacion_13_ciudades_2025.xlsx"), sheet = "resultado") %>%
  select(ciudad, pob_cabecera)

ipc <- read_excel(file.path(dir_output, "ipc_acumulado_2021_2025.xlsx"), sheet = "acumulado") %>%
  select(ciudad_ipc = cobertura, precio_relativo_alim)

datos <- ciudades %>%
  left_join(poblacion, by = "ciudad") %>%
  left_join(ipc, by = "ciudad_ipc") %>%
  mutate(
    clase_pobreza = cut(pobreza_2025, breaks = cortes_pobreza, labels = etiq_pobreza, right = FALSE),
    clase_precio  = cut(precio_relativo_alim, breaks = cortes_precio, labels = etiq_precio, right = FALSE),
    clase_precio  = factor(ifelse(is.na(clase_precio), etiq_sin_dato, as.character(clase_precio)),
                           levels = c(etiq_precio, etiq_sin_dato)),
    etiqueta      = sub(" A.M.", "", ciudad, fixed = TRUE)
  )

if (any(is.na(datos$pob_cabecera))) stop("Falta la población de alguna ciudad: revisar los nombres.")

# Control: cuántas ciudades caen en cada clase
print(datos %>% select(ciudad, pobreza_2025, clase_pobreza, precio_relativo_alim, clase_precio), n = Inf)
print(count(datos, clase_pobreza))
print(count(datos, clase_precio))

puntos <- st_as_sf(datos, coords = c("lon", "lat"), crs = 4326) %>%
  st_transform(3116)
xy <- st_coordinates(puntos)
puntos$X <- xy[, 1]
puntos$Y <- xy[, 2]

# Tabla sin geometría para dibujar los puntos con geom_point (coordenadas
# proyectadas). Las ciudades más grandes van primero para que las pequeñas
# queden encima.
puntos_df <- puntos %>%
  st_drop_geometry() %>%
  arrange(desc(pob_cabecera))

# Clave de leyenda en forma de rectángulo con borde negro
draw_key_caja <- function(data, params, size) {
  grid::rectGrob(
    width  = grid::unit(0.92, "npc"),
    height = grid::unit(0.85, "npc"),
    gp = grid::gpar(col = "black", fill = data$fill, lwd = 0.6)
  )
}

# -----------------------------------------------------------------------------
# 3. Elementos comunes de los dos paneles
# -----------------------------------------------------------------------------
mapa_base <- ggplot() +
  geom_sf(data = latin_america, fill = col_vecinos, color = "white", linewidth = 0.25) +
  geom_sf(data = col_deptos, fill = col_colombia, color = col_dptos, linewidth = 0.12) +
  geom_sf(data = col_contorno, fill = NA, color = col_pais, linewidth = 0.35) +
  geom_text(data = filter(rotulos_ref, tipo == "mar"), aes(x = X, y = Y, label = texto),
            size = 2.2, fontface = "italic", color = "#6B8BA4") +
  geom_text(data = filter(rotulos_ref, tipo == "pais"), aes(x = X, y = Y, label = texto),
            size = 2.2, color = "gray45")

coordenadas <- coord_sf(xlim = lim_x, ylim = lim_y, expand = FALSE, crs = st_crs(3116))

tema_mapa <- theme_void(base_size = 8, base_family = "sans") +
  theme(
    panel.background  = element_rect(fill = col_oceano, colour = NA),
    panel.grid.major  = element_line(color = "white", linewidth = 0.3),
    panel.border      = element_rect(colour = "gray20", fill = NA, linewidth = 0.4),
    legend.position   = "bottom",
    legend.box        = "vertical",
    legend.title      = element_text(size = 7.5),
    legend.text       = element_text(size = 7),
    legend.key.width  = unit(7, "mm"),
    legend.key.height = unit(3.5, "mm"),
    legend.spacing.y  = unit(1, "mm"),
    legend.margin     = margin(2, 0, 0, 0),
    plot.margin       = margin(2, 3, 2, 3)
  )

etiquetas <- geom_text_repel(
  data = puntos_df, aes(x = X, y = Y, label = etiqueta),
  size = 2.3, family = "sans", color = "gray10",
  bg.color = "white", bg.r = 0.12,
  box.padding = 0.4, point.padding = 0.4,
  min.segment.length = 0.2, segment.size = 0.2, segment.color = "gray40",
  seed = 42
)

# -----------------------------------------------------------------------------
# 4. Panel izquierdo: pobreza monetaria (clases) y población (tamaño)
# -----------------------------------------------------------------------------
# Escala de tamaño común a los dos paneles (misma población, mismo tamaño)
escala_tamano <- function(mostrar = TRUE) {
  scale_size_area(max_size = 7, breaks = c(5e5, 2e6, 8e6), labels = c("0.5", "2", "8"),
                  name = "Population (millions)",
                  guide = if (mostrar) "legend" else "none")
}

p_pobreza <- mapa_base +
  # Puntos visibles: aportan solo la leyenda de tamaño
  geom_point(data = puntos_df, aes(x = X, y = Y, size = pob_cabecera, fill = clase_pobreza),
             shape = 21, color = "gray15", stroke = 0.3,
             show.legend = c(size = TRUE, fill = FALSE)) +
  # Capa invisible: aporta la leyenda de color con rectángulos de borde negro
  geom_point(data = puntos_df, aes(x = X, y = Y, fill = clase_pobreza),
             size = 0, stroke = 0, alpha = 0,
             show.legend = c(fill = TRUE), key_glyph = draw_key_caja) +
  etiquetas +
  scale_fill_manual(values = pal_pobreza, drop = FALSE,
                    name = "Monetary poverty incidence, 2025 (%)") +
  escala_tamano(mostrar = TRUE) +
  annotation_scale(location = "br", width_hint = 0.22, style = "ticks",
                   text_cex = 0.55, line_width = 0.4, line_col = "gray20", text_col = "gray20") +
  annotation_north_arrow(location = "tl", height = unit(0.7, "cm"), width = unit(0.7, "cm")) +
  coordenadas +
  tema_mapa +
  guides(
    fill = guide_legend(title.position = "top", nrow = 1, order = 1),
    size = guide_legend(title.position = "top", nrow = 1, order = 2,
                        override.aes = list(fill = "gray80", color = "gray15"))
  )

# -----------------------------------------------------------------------------
# 5. Panel derecho: encarecimiento relativo de los alimentos (clases)
#    Ibagué aparece como "Not available" (punto blanco)
# -----------------------------------------------------------------------------
p_precios <- mapa_base +
  # Puntos visibles: tama\u00f1o seg\u00fan la poblaci\u00f3n, igual que en el panel izquierdo
  geom_point(data = puntos_df, aes(x = X, y = Y, size = pob_cabecera, fill = clase_precio),
             shape = 21, color = "gray15", stroke = 0.3,
             show.legend = c(size = FALSE, fill = FALSE)) +
  # Capa invisible: aporta la leyenda de color con rect\u00e1ngulos de borde negro
  geom_point(data = puntos_df, aes(x = X, y = Y, fill = clase_precio),
             size = 0, stroke = 0, alpha = 0,
             show.legend = c(fill = TRUE), key_glyph = draw_key_caja) +
  etiquetas +
  scale_fill_manual(values = c(pal_precio, "white"), drop = FALSE,
                    name = "Cumulative increase in the relative price of food, Dec 2021\u2013Sep 2025 (%)") +
  escala_tamano(mostrar = FALSE) +
  coordenadas +
  tema_mapa +
  guides(
    fill = guide_legend(title.position = "top", nrow = 1)
  )

# -----------------------------------------------------------------------------
# 6. Figura final y salida
# -----------------------------------------------------------------------------
figura <- plot_grid(p_pobreza, p_precios, ncol = 2, align = "h", axis = "tb")

ggsave(file.path(dir_output, "mapa_contexto_13_ciudades.pdf"), figura,
       width = ancho_mm, height = alto_mm, units = "mm", device = cairo_pdf)
ggsave(file.path(dir_output, "mapa_contexto_13_ciudades.png"), figura,
       width = ancho_mm, height = alto_mm, units = "mm", dpi = 600, bg = "white")