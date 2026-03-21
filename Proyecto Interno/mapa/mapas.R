# =========================================================
# FLOW MAP FOR PAPER
# =========================================================

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(ggplot2)
  library(readxl)
  library(stringr)
  library(scales)
  library(patchwork)
  library(purrr)
  library(tidyr)
  library(ggspatial)
  library(grid)
})

# =========================================================
# 1. Paths
# =========================================================
base_dir  <- "C:/Users/danie/OneDrive/Documentos/least-cost/shp.files"

path_col   <- file.path(base_dir, "colombia", "COLOMBIA.shp")
path_muni  <- file.path(base_dir, "DANE_geodata", "MGN_ANM_MPIOS_WGS84.shp")
path_world <- file.path(base_dir, "world", "world.shp")
path_dist  <- "C:/Users/danie/OneDrive/Escritorio/Least-cost-diets-and-affordability/Proyecto Interno/mapa/distancia_cantidad.xlsx"

out_dir  <- "C:/Users/danie/OneDrive/Escritorio/Least-cost-diets-and-affordability/Proyecto Interno/mapa"
out_file <- file.path(out_dir, "food_supply_flows.png")

if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# =========================================================
# 2. Read data
# =========================================================
colombia <- st_read(path_col, quiet = TRUE)
muni     <- st_read(path_muni, quiet = TRUE)
world    <- st_read(path_world, quiet = TRUE)
dist_raw <- read_excel(path_dist)

# =========================================================
# 3. CRS
# =========================================================
if (is.na(st_crs(colombia))) st_crs(colombia) <- 4326
if (is.na(st_crs(muni)))     st_crs(muni)     <- 4326
if (is.na(st_crs(world)))    st_crs(world)    <- 4326

colombia <- st_transform(colombia, 4326)
muni     <- st_transform(muni, 4326)
world    <- st_transform(world, 4326)

# =========================================================
# 4. Clean codes
# =========================================================
std_code <- function(x) {
  x <- as.character(x)
  x <- str_trim(x)
  x <- str_remove(x, "\\.0$")
  x <- str_pad(x, width = 5, side = "left", pad = "0")
  x
}

muni <- muni %>%
  mutate(
    MPIO_CDPMP = std_code(MPIO_CDPMP)
  )

dist <- dist_raw %>%
  mutate(
    from     = std_code(from),
    to       = std_code(to),
    year     = as.integer(year),
    quantity = as.numeric(quantity),
    Dist_km  = as.numeric(Dist_km),
    group    = str_to_title(group)
  )

# =========================================================
# 5. Background layer: Latin America & Caribbean
# =========================================================
if (!"region_wb" %in% names(world)) {
  stop("No existe la columna 'region_wb' en el shapefile world.")
}

latin_america <- world %>%
  filter(region_wb == "Latin America & Caribbean")

# =========================================================
# 6. Municipality coordinates
# Prefer LONGITUD/LATITUD if available; otherwise point_on_surface
# =========================================================
if (all(c("LONGITUD", "LATITUD") %in% names(muni))) {
  mun_xy <- muni %>%
    st_drop_geometry() %>%
    transmute(
      mun_code = MPIO_CDPMP,
      x = as.numeric(LONGITUD),
      y = as.numeric(LATITUD)
    )
} else {
  muni_pts <- muni %>%
    st_make_valid() %>%
    st_point_on_surface()
  
  coords <- st_coordinates(muni_pts)
  
  mun_xy <- muni_pts %>%
    st_drop_geometry() %>%
    transmute(
      mun_code = MPIO_CDPMP,
      x = coords[, 1],
      y = coords[, 2]
    )
}

# =========================================================
# 7. Destination cities
# =========================================================
cities_keep <- c("05001", "11001", "76001")

city_labels <- c(
  "05001" = "Medellín",
  "11001" = "Bogotá",
  "76001" = "Cali"
)

# =========================================================
# 8. Aggregate flows
# =========================================================
flow0 <- dist %>%
  filter(to %in% cities_keep, from != to) %>%
  group_by(to, from) %>%
  summarise(
    qty_total = sum(quantity, na.rm = TRUE),
    dist_mean = weighted.mean(Dist_km, w = quantity, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    tkm = qty_total * dist_mean
  )

# =========================================================
# 9. Join coordinates
# =========================================================
flow1 <- flow0 %>%
  left_join(
    mun_xy %>% rename(from = mun_code, x_from = x, y_from = y),
    by = "from"
  ) %>%
  left_join(
    mun_xy %>% rename(to = mun_code, x_to = x, y_to = y),
    by = "to"
  ) %>%
  filter(
    !is.na(x_from), !is.na(y_from),
    !is.na(x_to), !is.na(y_to)
  ) %>%
  mutate(
    city = recode(to, !!!city_labels)
  )

# =========================================================
# 10. Top flows
# =========================================================
top_n <- 35

flow_top <- flow1 %>%
  arrange(city, desc(tkm)) %>%
  group_by(city) %>%
  slice_head(n = top_n) %>%
  ungroup()

# =========================================================
# 11. Convert flows to sf lines
# =========================================================
make_lines_sf <- function(df) {
  geoms <- purrr::map(
    seq_len(nrow(df)),
    function(i) {
      st_linestring(
        matrix(
          c(df$x_from[i], df$x_to[i],
            df$y_from[i], df$y_to[i]),
          ncol = 2,
          byrow = FALSE
        )
      )
    }
  )
  st_sf(df, geometry = st_sfc(geoms, crs = 4326))
}

lines_top <- make_lines_sf(flow_top)

dest_pts <- flow_top %>%
  distinct(city, to, x_to, y_to) %>%
  st_as_sf(coords = c("x_to", "y_to"), crs = 4326)

orig_pts <- flow_top %>%
  distinct(city, from, x_from, y_from) %>%
  st_as_sf(coords = c("x_from", "y_from"), crs = 4326)

# =========================================================
# 12. Zoom: Colombia completa con margen pequeño
# =========================================================
bb_col <- st_bbox(colombia)

xpad <- (bb_col["xmax"] - bb_col["xmin"]) * 0.08
ypad <- (bb_col["ymax"] - bb_col["ymin"]) * 0.08

bbox_col_zoom <- c(
  xmin = bb_col["xmin"] - xpad,
  xmax = bb_col["xmax"] + xpad,
  ymin = bb_col["ymin"] - ypad,
  ymax = bb_col["ymax"] + ypad
)

# recorte del fondo regional con ese mismo encuadre ampliado
bbox_bg <- st_bbox(c(
  xmin = bbox_col_zoom["xmin"] - 2.0,
  xmax = bbox_col_zoom["xmax"] + 2.0,
  ymin = bbox_col_zoom["ymin"] - 1.5,
  ymax = bbox_col_zoom["ymax"] + 1.5
), crs = st_crs(world))

latin_america_bg <- st_crop(latin_america, bbox_bg)

# =========================================================
# 13. Legend breaks
# =========================================================
rng_tkm <- range(flow_top$tkm, na.rm = TRUE)
col_breaks <- pretty(rng_tkm, n = 3)
col_breaks <- col_breaks[col_breaks > 0]

if (length(col_breaks) < 2) {
  col_breaks <- c(min(flow_top$tkm, na.rm = TRUE), max(flow_top$tkm, na.rm = TRUE))
}

# =========================================================
# 14. Theme
# =========================================================
theme_paper_map <- function() {
  theme_minimal(base_size = 12) +
    theme(
      panel.grid = element_blank(),
      panel.background = element_rect(fill = "aliceblue", color = NA),
      plot.background  = element_rect(fill = "white", color = NA),
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.title = element_text(size = 12, face = "bold"),
      legend.text = element_text(size = 10),
      panel.border = element_rect(color = "grey35", fill = NA, linewidth = 0.55),
      plot.margin = margin(6, 6, 6, 6)
    )
}

# =========================================================
# 15. Plot function
# =========================================================
plot_city <- function(city_name, show_legend = FALSE) {
  
  lines_city <- lines_top %>% filter(city == city_name)
  dest_city  <- dest_pts  %>% filter(city == city_name)
  orig_city  <- orig_pts  %>% filter(city == city_name)
  
  ggplot() +
    geom_sf(
      data = latin_america_bg,
      fill = "grey97",
      color = "grey80",
      linewidth = 0.20
    ) +
    geom_sf(
      data = colombia,
      fill = "white",
      color = "grey35",
      linewidth = 0.48
    ) +
    
    geom_sf(
      data = lines_city,
      aes(color = tkm),
      linewidth = 0.85,
      alpha = 0.90,
      show.legend = show_legend
    ) +
    
    geom_sf(
      data = orig_city,
      color = "#2E6F8E",
      size = 1.8,
      alpha = 0.95,
      show.legend = FALSE
    ) +
    
    geom_sf(
      data = dest_city,
      shape = 21,
      size = 4.6,
      stroke = 0.55,
      fill = "#0B3954",
      color = "#0B3954",
      show.legend = FALSE
    ) +
    
    coord_sf(
      xlim = c(bbox_col_zoom["xmin"], bbox_col_zoom["xmax"]),
      ylim = c(bbox_col_zoom["ymin"], bbox_col_zoom["ymax"]),
      expand = FALSE,
      clip = "on"
    ) +
    
    annotation_scale(
      location = "bl",
      width_hint = 0.15,
      text_cex = 0.72,
      line_width = 0.40
    ) +
    
    annotation_north_arrow(
      location = "tr",
      which_north = "true",
      pad_x = unit(0.10, "cm"),
      pad_y = unit(0.10, "cm"),
      height = unit(0.85, "cm"),
      width  = unit(0.85, "cm"),
      style = north_arrow_fancy_orienteering
    ) +
    
    scale_color_gradientn(
      colors = c("#DCEAF2", "#9CC3D5", "#4A90A4", "#1F5D75", "#0B3954"),
      name   = "Flow intensity\n(ton-km)",
      breaks = col_breaks,
      labels = label_number(scale_cut = cut_short_scale()),
      guide  = guide_colorbar(
        title.position = "top",
        title.hjust = 0.5,
        barwidth  = unit(4.2, "cm"),
        barheight = unit(0.60, "cm")
      )
    ) +
    
    labs(title = city_name) +
    theme_paper_map() +
    theme(
      legend.position = if (show_legend) "bottom" else "none"
    )
}

# =========================================================
# 16. Final figure
# =========================================================
p_med <- plot_city("Medellín", FALSE)
p_bog <- plot_city("Bogotá",   TRUE)
p_cal <- plot_city("Cali",     FALSE)

final_map <- (p_med | p_bog | p_cal) +
  plot_layout(guides = "collect") &
  theme(
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.justification = "center"
  )

print(final_map)

# =========================================================
# 17. Save
# =========================================================
ggsave(
  filename = out_file,
  plot = final_map,
  width = 15.2,
  height = 6.3,
  dpi = 700,
  bg = "white"
)

cat("Mapa guardado en:\n", out_file, "\n")