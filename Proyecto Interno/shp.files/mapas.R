# =========================================================
# FLOW MAP FOR PAPER: food supply origins -> destination city
# Using DANE municipal shapefile
# Full Colombia extent in all three panels
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
})

# =========================================================
# 1. Paths
# =========================================================
base_dir <- "C:/Users/danie/OneDrive/Escritorio/Least-cost-diets-and-affordability/Proyecto Interno/shp.files"

path_col  <- file.path(base_dir, "colombia", "COLOMBIA.shp")
path_muni <- "C:/Users/danie/OneDrive/Escritorio/Least-cost-diets-and-affordability/Proyecto Interno/shp.files/DANE_geodata/MGN_ANM_MPIOS_WGS84.shp"
path_dist <- "C:/Users/danie/OneDrive/Escritorio/Least-cost-diets-and-affordability/Proyecto Interno/shp.files/distancia_cantidad.xlsx"

# =========================================================
# 2. Read data
# =========================================================
colombia <- st_read(path_col, quiet = TRUE)
muni     <- st_read(path_muni, quiet = TRUE)
dist_raw <- read_excel(path_dist)

# =========================================================
# 3. CRS
# =========================================================
if (is.na(st_crs(colombia))) st_crs(colombia) <- 4326
if (is.na(st_crs(muni)))     st_crs(muni)     <- 4326

colombia <- st_transform(colombia, 4326)
muni     <- st_transform(muni, 4326)

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

colombia$DPTO_CCDGO <- as.character(colombia$DPTO_CCDGO)
colombia$DPTO_CCDGO <- str_trim(colombia$DPTO_CCDGO)
colombia$DPTO_CCDGO <- str_remove(colombia$DPTO_CCDGO, "\\.0$")
colombia$DPTO_CCDGO <- str_pad(colombia$DPTO_CCDGO, width = 2, side = "left", pad = "0")

muni <- muni %>%
  mutate(
    MPIO_CDPMP = std_code(MPIO_CDPMP),
    DPTO_CCDGO = str_pad(as.character(DPTO_CCDGO), width = 2, side = "left", pad = "0"),
    MPIO_CCDGO = str_pad(as.character(MPIO_CCDGO), width = 3, side = "left", pad = "0")
  )

dist <- dist_raw %>%
  mutate(
    from = std_code(from),
    to   = std_code(to),
    year = as.integer(year),
    quantity = as.numeric(quantity),
    Dist_km  = as.numeric(Dist_km),
    group = str_to_title(group)
  )

# =========================================================
# 5. Municipality coordinates from DANE shapefile
# =========================================================
if (all(c("LONGITUD", "LATITUD") %in% names(muni))) {
  mun_xy <- muni %>%
    st_drop_geometry() %>%
    transmute(
      mun_code = MPIO_CDPMP,
      mun_name = MPIO_CNMBR,
      depto    = DPTO_CCDGO,
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
      mun_name = MPIO_CNMBR,
      depto    = DPTO_CCDGO,
      x = coords[, 1],
      y = coords[, 2]
    )
}

# =========================================================
# 6. Keep study cities
# =========================================================
cities_keep <- c("05001", "11001", "76001")

city_labels <- c(
  "05001" = "Medellín",
  "11001" = "Bogotá",
  "76001" = "Cali"
)

# =========================================================
# 7. Aggregate flows
# =========================================================
use_period <- TRUE
year_selected <- 2024

if (use_period) {
  flow0 <- dist %>%
    dplyr::filter(to %in% cities_keep) %>%
    dplyr::group_by(to, from, NOM_DPTO.proc, NOM_MPIO.proc, NOM_DPTO.des, NOM_MPIO.des) %>%
    dplyr::summarise(
      qty_total = sum(quantity, na.rm = TRUE),
      dist_mean = weighted.mean(Dist_km, w = quantity, na.rm = TRUE),
      .groups = "drop"
    )
} else {
  flow0 <- dist %>%
    dplyr::filter(to %in% cities_keep, year == year_selected) %>%
    dplyr::group_by(to, from, NOM_DPTO.proc, NOM_MPIO.proc, NOM_DPTO.des, NOM_MPIO.des) %>%
    dplyr::summarise(
      qty_total = sum(quantity, na.rm = TRUE),
      dist_mean = weighted.mean(Dist_km, w = quantity, na.rm = TRUE),
      .groups = "drop"
    )
}

flow0 <- flow0 %>%
  dplyr::filter(from != to)

# =========================================================
# 8. Join coordinates
# =========================================================
flow1 <- flow0 %>%
  dplyr::left_join(
    mun_xy %>% dplyr::rename(from = mun_code, x_from = x, y_from = y),
    by = "from"
  ) %>%
  dplyr::left_join(
    mun_xy %>% dplyr::rename(to = mun_code, x_to = x, y_to = y),
    by = "to"
  ) %>%
  dplyr::filter(
    !is.na(x_from), !is.na(y_from),
    !is.na(x_to), !is.na(y_to)
  ) %>%
  dplyr::mutate(
    city = recode(to, !!!city_labels)
  )

# limpieza espacial básica: Colombia continental
flow1 <- flow1 %>%
  dplyr::filter(
    x_from >= -79.5, x_from <= -66.5,
    x_to   >= -79.5, x_to   <= -66.5,
    y_from >=  -4.5, y_from <=  13.8,
    y_to   >=  -4.5, y_to   <=  13.8
  )

cat("Flows after matching coordinates:", nrow(flow1), "\n")

# =========================================================
# 9. Top flows to highlight
# =========================================================
top_n <- 35

flow_main <- flow1 %>%
  dplyr::mutate(qty_total = as.numeric(qty_total)) %>%
  dplyr::arrange(city, desc(qty_total)) %>%
  dplyr::group_by(city) %>%
  dplyr::mutate(rank_city = row_number()) %>%
  dplyr::ungroup()

flow_top <- flow_main %>%
  dplyr::filter(rank_city <= top_n)

# =========================================================
# 10. Convert to sf lines
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

lines_all <- make_lines_sf(flow_main)
lines_top <- make_lines_sf(flow_top)

dest_pts <- flow_main %>%
  dplyr::distinct(city, to, x_to, y_to) %>%
  st_as_sf(coords = c("x_to", "y_to"), crs = 4326)

orig_top_pts <- flow_top %>%
  dplyr::group_by(city) %>%
  dplyr::slice_max(order_by = qty_total, n = 12, with_ties = FALSE) %>%
  dplyr::ungroup() %>%
  dplyr::distinct(city, from, NOM_MPIO.proc, x_from, y_from, qty_total) %>%
  st_as_sf(coords = c("x_from", "y_from"), crs = 4326)

# =========================================================
# 11. Full-country bbox for all panels
# =========================================================
bb <- st_bbox(colombia)

xpad <- (bb["xmax"] - bb["xmin"]) * 0.02
ypad <- (bb["ymax"] - bb["ymin"]) * 0.02

bbox_full <- c(
  xmin = bb["xmin"] - xpad,
  xmax = bb["xmax"] + xpad,
  ymin = bb["ymin"] - ypad,
  ymax = bb["ymax"] + ypad
)

# escala común para los tres mapas
lw_breaks <- pretty_breaks(n = 5)(flow_top$qty_total)
lw_breaks <- lw_breaks[lw_breaks > 0]

# =========================================================
# 12. Theme
# =========================================================
theme_paper_map <- function() {
  theme_minimal(base_size = 11) +
    theme(
      panel.grid = element_blank(),
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      plot.title = element_text(face = "bold", hjust = 0.5, size = 15),
      plot.subtitle = element_blank(),
      legend.position = "bottom",
      legend.title = element_text(size = 12, face = "bold"),
      legend.text = element_text(size = 10),
      panel.border = element_rect(color = "grey35", fill = NA, linewidth = 0.45),
      plot.margin = margin(6, 6, 6, 6)
    )
}

# =========================================================
# 13. Plot function
# =========================================================
plot_flow_city <- function(city_name, show_legend = FALSE) {
  
  lines_bg_city  <- lines_all %>% dplyr::filter(city == city_name)
  lines_top_city <- lines_top %>% dplyr::filter(city == city_name)
  dest_city      <- dest_pts %>% dplyr::filter(city == city_name)
  orig_city      <- orig_top_pts %>% dplyr::filter(city == city_name)
  
  ggplot() +
    geom_sf(data = colombia, fill = "grey96", color = "grey80", linewidth = 0.16) +
    
    geom_sf(
      data = lines_bg_city,
      color = "grey60",
      linewidth = 0.14,
      alpha = 0.08,
      show.legend = FALSE
    ) +
    
    geom_sf(
      data = lines_top_city,
      aes(linewidth = qty_total),
      color = "#0C7C7C",
      alpha = 0.88,
      show.legend = show_legend   
    ) +
    
    geom_sf(
      data = orig_city,
      color = "#0C7C7C",
      size = 0.8,
      alpha = 0.9,
      show.legend = FALSE
    ) +
    
    geom_sf(
      data = dest_city,
      shape = 21,
      size = 2.8,
      stroke = 0.4,
      fill = "#163A5F",
      color = "#163A5F",
      show.legend = FALSE
    ) +
    
    coord_sf(
      xlim = c(bbox_full["xmin"], bbox_full["xmax"]),
      ylim = c(bbox_full["ymin"], bbox_full["ymax"]),
      expand = FALSE,
      clip = "on"
    ) +
    
    scale_linewidth_continuous(
      name = "Quantity",
      range = c(0.35, 1.45),
      breaks = lw_breaks,
      labels = label_number(big.mark = ",", accuracy = 1)
    ) +
    
    labs(title = city_name) +
    theme_paper_map() +
    theme(
      legend.position = ifelse(show_legend, "bottom", "none"),
      legend.title = element_text(size = 13, face = "bold"),
      legend.text  = element_text(size = 11)
    )
}

# =========================================================
# 14. Final figure
# =========================================================
p_med <- plot_flow_city("Medellín", show_legend = FALSE)
p_bog <- plot_flow_city("Bogotá",   show_legend = TRUE)   
p_cal <- plot_flow_city("Cali",     show_legend = FALSE)

final_map <- (p_med | p_bog | p_cal) +
  plot_layout(guides = "collect") &
  theme(
    legend.position = "bottom",
    legend.box = "horizontal"
  )

print(final_map)

# =========================================================
# 15. Save
# =========================================================
out_file <- file.path(base_dir, "food_supply_flows_paper_dane.png")

ggsave(
  filename = out_file,
  plot = final_map,
  width = 14.5,
  height = 6.2,
  dpi = 700,
  bg = "white"
)

cat("Mapa guardado en:\n", out_file, "\n")