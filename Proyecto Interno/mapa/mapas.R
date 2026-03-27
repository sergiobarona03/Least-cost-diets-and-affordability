# =========================================================
# FLOW MAP FOR PAPER
# =========================================================

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(ggplot2)
  library(readxl)
  library(stringr)
  library(cowplot)
  library(purrr)
  library(ggspatial)
  library(grid)
  library(scales)
})

# =========================================================
# 1. PATHS
# =========================================================
base_dir  <- "C:/Users/danie/OneDrive/Documentos/least-cost/shp.files"

path_col   <- file.path(base_dir, "colombia", "COLOMBIA.shp")
path_muni  <- file.path(base_dir, "DANE_geodata", "MGN_ANM_MPIOS_WGS84.shp")
path_world <- file.path(base_dir, "world", "world.shp")
path_dist  <- "C:/Users/danie/OneDrive/Escritorio/Least-cost-diets-and-affordability/Proyecto Interno/mapa/distancia_cantidad.xlsx"

out_dir  <- "C:/Users/danie/OneDrive/Escritorio/Least-cost-diets-and-affordability/Proyecto Interno/mapa"
out_file <- file.path(out_dir, "food_supply_flows_panel_paper_final.png")

if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# =========================================================
# 2. READ DATA
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
# 4. HELPERS
# =========================================================
std_code <- function(x) {
  x <- as.character(x)
  x <- stringr::str_trim(x)
  x <- stringr::str_remove(x, "\\.0$")
  stringr::str_pad(x, width = 5, side = "left", pad = "0")
}

pick_name_col <- function(df) {
  nms <- names(df)
  candidates <- c(
    "MPIO_CNMBR", "NOM_MPIO", "NOMBRE_MPIO", "MPIO_NOMBRE",
    "MUNICIPIO", "NOMB_MPIO", "NOMMUNICI", "NAME_2"
  )
  hit <- candidates[candidates %in% nms]
  if (length(hit) > 0) hit[1] else NA_character_
}

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

round_to_base <- function(x, base = 10000) {
  round(x / base) * base
}

# =========================================================
# 5. CLEAN DATA
# =========================================================
muni <- muni %>%
  dplyr::mutate(MPIO_CDPMP = std_code(MPIO_CDPMP))

dist <- dist_raw %>%
  dplyr::mutate(
    from     = std_code(from),
    to       = std_code(to),
    quantity = suppressWarnings(as.numeric(quantity))
  )

# =========================================================
# 6. BACKGROUND
# =========================================================
latin_america <- world %>%
  dplyr::filter(region_wb == "Latin America & Caribbean")

# =========================================================
# 7. MUNICIPAL CENTROIDS
# =========================================================
name_col <- pick_name_col(muni)

muni_centroids <- muni %>%
  st_make_valid() %>%
  st_point_on_surface()

coords <- st_coordinates(muni_centroids)

mun_xy <- muni_centroids %>%
  st_drop_geometry() %>%
  dplyr::transmute(
    mun_code = MPIO_CDPMP,
    mun_name = if (!is.na(name_col)) as.character(.data[[name_col]]) else MPIO_CDPMP,
    x = coords[, 1],
    y = coords[, 2]
  ) %>%
  dplyr::filter(is.finite(x), is.finite(y), !is.na(mun_code))

# =========================================================
# 8. MAIN CITIES
# =========================================================
cities_keep <- c("05001", "11001", "76001")

city_labels <- c(
  "05001" = "Medellín",
  "11001" = "Bogotá",
  "76001" = "Cali"
)

# =========================================================
# 9. AGGREGATE FLOWS
# =========================================================
flow1 <- dist %>%
  dplyr::filter(
    to %in% cities_keep,
    from != to,
    is.finite(quantity),
    quantity > 0
  ) %>%
  dplyr::group_by(to, from) %>%
  dplyr::summarise(
    qty_total = sum(quantity, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::left_join(
    mun_xy %>%
      dplyr::rename(
        from = mun_code,
        mun_name_from = mun_name,
        x_from = x,
        y_from = y
      ),
    by = "from"
  ) %>%
  dplyr::left_join(
    mun_xy %>%
      dplyr::rename(
        to = mun_code,
        mun_name_to = mun_name,
        x_to = x,
        y_to = y
      ),
    by = "to"
  ) %>%
  dplyr::mutate(city = dplyr::recode(to, !!!city_labels)) %>%
  dplyr::filter(
    is.finite(x_from), is.finite(y_from),
    is.finite(x_to),   is.finite(y_to),
    !is.na(city)
  )

if (nrow(flow1) == 0) {
  stop("No hay flujos válidos luego de unir con centroides municipales.")
}

# =========================================================
# 10. TOP ORIGINS
# =========================================================
top_n <- 35

flow_top <- flow1 %>%
  dplyr::group_by(city) %>%
  dplyr::slice_max(order_by = qty_total, n = top_n, with_ties = FALSE) %>%
  dplyr::ungroup()

if (nrow(flow_top) == 0) {
  stop("No quedaron flujos válidos luego de seleccionar top_n.")
}

# =========================================================
# 11. SUPPLY LEVELS
# =========================================================
cuts_tbl <- flow_top %>%
  dplyr::summarise(
    low_max_raw = quantile(qty_total, probs = 1/3, na.rm = TRUE),
    med_max_raw = quantile(qty_total, probs = 2/3, na.rm = TRUE)
  )

round_to <- 10000

low_max <- round_to_base(as.numeric(cuts_tbl$low_max_raw), base = round_to)
med_max <- round_to_base(as.numeric(cuts_tbl$med_max_raw), base = round_to)

if (med_max <= low_max) {
  med_max <- low_max + round_to
}

flow_top <- flow_top %>%
  dplyr::mutate(
    supply_level = dplyr::case_when(
      qty_total < low_max ~ "Low supply",
      qty_total < med_max ~ "Medium supply",
      TRUE ~ "High supply"
    ),
    supply_level = factor(
      supply_level,
      levels = c("Low supply", "Medium supply", "High supply")
    )
  )

# =========================================================
# 12. SF OBJECTS
# =========================================================
lines_top <- make_lines_sf(flow_top)

orig_pts <- flow_top %>%
  dplyr::distinct(city, from, mun_name_from, qty_total, supply_level, x_from, y_from) %>%
  st_as_sf(coords = c("x_from", "y_from"), crs = 4326)

dest_pts <- flow_top %>%
  dplyr::distinct(city, to, mun_name_to, x_to, y_to) %>%
  st_as_sf(coords = c("x_to", "y_to"), crs = 4326)

# =========================================================
# 13. ZOOM TO COLOMBIA
# =========================================================
bb_col <- st_bbox(colombia)

xpad <- (bb_col["xmax"] - bb_col["xmin"]) * 0.03
ypad <- (bb_col["ymax"] - bb_col["ymin"]) * 0.03

xlim_col <- c(bb_col["xmin"] - xpad, bb_col["xmax"] + xpad)
ylim_col <- c(bb_col["ymin"] - ypad, bb_col["ymax"] + ypad)

# =========================================================
# 14. THEME
# =========================================================
theme_map <- function() {
  theme_minimal(base_size = 12) +
    theme(
      panel.grid = element_blank(),
      panel.background = element_rect(fill = "#EAF2F8", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
      panel.border = element_rect(color = "grey45", fill = NA, linewidth = 0.55),
      plot.margin = margin(2, 2, 2, 2)
    )
}

# =========================================================
# 15. PLOT FUNCTION
# =========================================================
plot_city <- function(city_name) {
  
  lines_city <- lines_top %>% dplyr::filter(city == city_name)
  orig_city  <- orig_pts  %>% dplyr::filter(city == city_name)
  dest_city  <- dest_pts  %>% dplyr::filter(city == city_name)
  
  ggplot() +
    geom_sf(
      data = latin_america,
      fill = "grey95",
      color = "grey82",
      linewidth = 0.18
    ) +
    geom_sf(
      data = colombia,
      fill = "white",
      color = "grey40",
      linewidth = 0.60
    ) +
    geom_sf(
      data = lines_city,
      color = "#A7B3BC",
      linewidth = 0.60,
      alpha = 0.85,
      lineend = "round",
      show.legend = FALSE
    ) +
    geom_sf(
      data = orig_city,
      aes(size = supply_level, fill = supply_level),
      shape = 21,
      color = "#4F6774",
      stroke = 0.22,
      alpha = 0.95,
      show.legend = FALSE
    ) +
    geom_sf(
      data = dest_city,
      shape = 21,
      size = 4.9,
      stroke = 0.55,
      fill = "#0B3954",
      color = "#0B3954",
      show.legend = FALSE
    ) +
    coord_sf(
      xlim = xlim_col,
      ylim = ylim_col,
      expand = FALSE
    ) +
    annotation_scale(
      location = "bl",
      width_hint = 0.12,
      text_cex = 0.72,
      line_width = 0.35
    ) +
    annotation_north_arrow(
      location = "tr",
      which_north = "true",
      pad_x = unit(0.08, "cm"),
      pad_y = unit(0.08, "cm"),
      height = unit(0.75, "cm"),
      width  = unit(0.75, "cm"),
      style = north_arrow_fancy_orienteering
    ) +
    scale_size_manual(
      values = c(
        "Low supply" = 1.9,
        "Medium supply" = 3.2,
        "High supply" = 4.8
      ),
      drop = FALSE
    ) +
    scale_fill_manual(
      values = c(
        "Low supply" = "#D6E3EA",
        "Medium supply" = "#8FB1C0",
        "High supply" = "#3E7691"
      ),
      drop = FALSE
    ) +
    labs(title = city_name) +
    theme_map()
}

# =========================================================
# 16. SMALL LEGEND PANEL
# =========================================================
legend_df <- data.frame(
  supply_level = factor(
    c("Low supply", "Medium supply", "High supply"),
    levels = c("Low supply", "Medium supply", "High supply")
  ),
  x = 1,
  y = c(3, 2, 1),
  label = c(
    paste0("Low: under ", scales::comma(low_max), " tonnes"),
    paste0("Medium: ", scales::comma(low_max), "–", scales::comma(med_max), " tonnes"),
    paste0("High: over ", scales::comma(med_max), " tonnes")
  )
)

legend_panel <- ggplot() +
  geom_point(
    data = legend_df,
    aes(x = x, y = y, fill = supply_level, size = supply_level),
    shape = 21,
    color = "#4F6774",
    stroke = 0.22,
    alpha = 0.95,
    show.legend = FALSE
  ) +
  geom_text(
    data = legend_df,
    aes(x = 1.22, y = y, label = label),
    hjust = 0,
    size = 5.0,
    color = "grey20"
  ) +
  annotate(
    "text",
    x = 1.22,
    y = 3.72,
    label = "Supply level",
    hjust = 0,
    fontface = "bold",
    size = 6.2
  ) +
  scale_size_manual(
    values = c(
      "Low supply" = 2.2,
      "Medium supply" = 3.7,
      "High supply" = 5.3
    )
  ) +
  scale_fill_manual(
    values = c(
      "Low supply" = "#D6E3EA",
      "Medium supply" = "#8FB1C0",
      "High supply" = "#3E7691"
    )
  ) +
  coord_cartesian(
    xlim = c(0.9, 4.6),
    ylim = c(0.55, 3.95),
    expand = FALSE
  ) +
  theme_void() +
  theme(
    plot.margin = margin(0, 0, 0, 0)
  )

# =========================================================
# 17. MAPS
# =========================================================
p_bog <- plot_city("Bogotá")
p_med <- plot_city("Medellín")
p_cal <- plot_city("Cali")

# =========================================================
# 18. BUILD PANEL
# =========================================================
final_map <- cowplot::ggdraw() +
  cowplot::draw_plot(p_bog, x = 0.03, y = 0.50, width = 0.44, height = 0.47) +
  cowplot::draw_plot(p_med, x = 0.50, y = 0.50, width = 0.44, height = 0.47) +
  cowplot::draw_plot(p_cal, x = 0.03, y = 0.03, width = 0.44, height = 0.47) +
  cowplot::draw_plot(legend_panel, x = 0.54, y = 0.06, width = 0.34, height = 0.18)

print(final_map)

# =========================================================
# 19. SAVE
# =========================================================
ggsave(
  filename = out_file,
  plot = final_map,
  width = 13.2,
  height = 10.6,
  dpi = 700,
  bg = "white"
)

cat("Saved map at:\n", out_file, "\n")