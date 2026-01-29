
########################################################
## 06_plots_coca_cona.R
## Plots for CoCA and CoNA (full sample, 3 cities)
## Inputs (created by scripts 00–04):
##  - out_dir/coca_fullsample.rds  (or .csv/.xlsx)
##  - out_dir/cona_cost_fullsample.rds and cona_comp_fullsample.rds
## Outputs:
##  - out_dir/plots/coca/*.png
##  - out_dir/plots/cona/*.png
##  - out_dir/plots/aggregates/*.png
########################################################

# -----------------------------
# 0) Load config + packages
# -----------------------------
source("working-papers/working-paper-ipc/least-cost-metrics/00_config.R")


suppressPackageStartupMessages({
  library(scales)
})

# Ensure plot dirs
plot_dir_coca <- file.path(out_dir, "plots", "coca")
plot_dir_cona <- file.path(out_dir, "plots", "cona")
plot_dir_agg  <- file.path(out_dir, "plots", "aggregates")

dir.create(plot_dir_coca, recursive = TRUE, showWarnings = FALSE)
dir.create(plot_dir_cona, recursive = TRUE, showWarnings = FALSE)
dir.create(plot_dir_agg,  recursive = TRUE, showWarnings = FALSE)

# -----------------------------
# 1) Load results
# -----------------------------
coca_path <- file.path(out_dir, "coca_fullsample.rds")
cona_cost_path <- file.path(out_dir, "cona_cost_fullsample.rds")
cona_comp_path <- file.path(out_dir, "cona_comp_fullsample.rds")

if (!file.exists(coca_path)) stop("Missing: ", coca_path)
if (!file.exists(cona_cost_path)) stop("Missing: ", cona_cost_path)
if (!file.exists(cona_comp_path)) stop("Missing: ", cona_comp_path)

coca <- readRDS(coca_path)
cona_cost <- readRDS(cona_cost_path)
cona_comp <- readRDS(cona_comp_path)

# Basic checks (column names depend on FoodpriceR outputs)
# We expect: fecha, ciudad, Sex, Demo_Group, cost_day, Cost_1000kcal (often)
required_coca <- c("fecha", "ciudad", "Sex", "Demo_Group", "cost_day")
required_cona <- c("fecha", "ciudad", "Sex", "Demo_Group", "cost_day")

miss_coca <- setdiff(required_coca, names(coca))
miss_cona <- setdiff(required_cona, names(cona_cost))
if (length(miss_coca) > 0) warning("CoCA missing columns: ", paste(miss_coca, collapse = ", "))
if (length(miss_cona) > 0) warning("CoNA cost missing columns: ", paste(miss_cona, collapse = ", "))

# Standardize types
coca <- coca %>%
  mutate(
    fecha = as.Date(fecha),
    ciudad = as.character(ciudad),
    Demo_Group = as.factor(Demo_Group),
    Sex = as.factor(Sex)
  )

cona_cost <- cona_cost %>%
  mutate(
    fecha = as.Date(fecha),
    ciudad = as.character(ciudad),
    Demo_Group = as.factor(Demo_Group),
    Sex = as.factor(Sex)
  )

# If Sex is coded 1/2, label (optional)
# Keep safe: only relabel if levels look numeric
relabel_sex <- function(x) {
  lv <- levels(x)
  if (all(lv %in% c("1","2"))) {
    factor(x, levels = c("1","2"), labels = c("Hombres","Mujeres"))
  } else x
}

coca$Sex <- relabel_sex(coca$Sex)
cona_cost$Sex <- relabel_sex(cona_cost$Sex)

# -----------------------------
# 2) Utilities for plotting
# -----------------------------
save_plot <- function(p, filename, width = 15, height = 9, dpi = 300) {
  ggsave(filename = filename, plot = p, width = width, height = height, dpi = dpi)
}

date_scale_monthly <- function() {
  scale_x_date(date_labels = "%Y-%m", date_breaks = "12 months")
}

theme_ts <- function(base_size = 12) {
  theme_bw(base_size = base_size) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid.minor = element_blank(),
      legend.position = "bottom"
    )
}

# -----------------------------
# 3) CoCA plots
# -----------------------------

# 3.1) City x Sex: facet by Demo_Group (cost_day)
plot_coca_city_sex <- function(df, city, sex) {
  d <- df %>% filter(ciudad == city, Sex == sex)
  
  ggplot(d, aes(x = fecha, y = cost_day)) +
    geom_line(linewidth = 0.45) +
    facet_wrap(~ Demo_Group, ncol = 3, scales = "free_y") +
    date_scale_monthly() +
    labs(
      title = paste0("CoCA — ", city, " — ", sex),
      subtitle = "Costo diario de una dieta suficiente en energía (cost_day)",
      x = "Fecha", y = "Costo diario (COP)"
    ) +
    theme_ts(12)
}

# 3.2) City aggregate: mean Cost_1000kcal across Sex & Demo_Group (if exists)
plot_coca_city_1000kcal <- function(df, city) {
  if (!("Cost_1000kcal" %in% names(df))) return(NULL)
  
  d <- df %>%
    filter(ciudad == city) %>%
    group_by(fecha) %>%
    summarise(mean_1000kcal = mean(Cost_1000kcal, na.rm = TRUE), .groups = "drop")
  
  ggplot(d, aes(x = fecha, y = mean_1000kcal)) +
    geom_line(linewidth = 0.55) +
    date_scale_monthly() +
    labs(
      title = paste0("CoCA — ", city, " — Costo promedio por 1000 kcal"),
      x = "Fecha", y = "COP por 1000 kcal"
    ) +
    theme_ts(12)
}

# 3.3) Compare cities: overall mean cost_day (averaged over Sex & Demo_Group)
plot_coca_compare_cities <- function(df) {
  d <- df %>%
    group_by(ciudad, fecha) %>%
    summarise(mean_cost_day = mean(cost_day, na.rm = TRUE), .groups = "drop")
  
  ggplot(d, aes(x = fecha, y = mean_cost_day, group = ciudad, linetype = ciudad)) +
    geom_line(linewidth = 0.55) +
    date_scale_monthly() +
    labs(
      title = "CoCA — Comparación entre ciudades",
      subtitle = "Promedio mensual de cost_day (promediado sobre Sex y Demo_Group)",
      x = "Fecha", y = "Costo diario (COP)", linetype = "Ciudad"
    ) +
    theme_ts(12)
}

# Save CoCA plots
cities <- sort(unique(coca$ciudad))
sexes  <- sort(unique(as.character(coca$Sex)))

for (cc in cities) {
  for (sx in sexes) {
    p <- plot_coca_city_sex(coca, cc, sx)
    out_png <- file.path(plot_dir_coca, paste0("coca__", safe_name(cc), "__", safe_name(sx), "__facet_demo.png"))
    save_plot(p, out_png, width = 15, height = 9, dpi = 300)
  }
  
  p1000 <- plot_coca_city_1000kcal(coca, cc)
  if (!is.null(p1000)) {
    out_png <- file.path(plot_dir_coca, paste0("coca__", safe_name(cc), "__mean_1000kcal.png"))
    save_plot(p1000, out_png, width = 15, height = 7, dpi = 300)
  }
}

p_comp <- plot_coca_compare_cities(coca)
save_plot(p_comp, file.path(plot_dir_agg, "coca_compare_cities_mean_cost_day.png"), width = 15, height = 7, dpi = 300)

# -----------------------------
# 4) CoNA plots
# -----------------------------

# 4.1) City x Sex: facet by Demo_Group (cost_day)
plot_cona_city_sex <- function(df, city, sex) {
  d <- df %>% filter(ciudad == city, Sex == sex)
  
  ggplot(d, aes(x = fecha, y = cost_day)) +
    geom_line(linewidth = 0.45) +
    facet_wrap(~ Demo_Group, ncol = 3, scales = "free_y") +
    date_scale_monthly() +
    labs(
      title = paste0("CoNA — ", city, " — ", sex),
      subtitle = "Costo diario de una dieta suficiente en nutrientes (cost_day)",
      x = "Fecha", y = "Costo diario (COP)"
    ) +
    theme_ts(12)
}

# 4.2) City aggregate: mean Cost_1000kcal across Sex & Demo_Group (if exists)
plot_cona_city_1000kcal <- function(df, city) {
  if (!("Cost_1000kcal" %in% names(df))) return(NULL)
  
  d <- df %>%
    filter(ciudad == city) %>%
    group_by(fecha) %>%
    summarise(mean_1000kcal = mean(Cost_1000kcal, na.rm = TRUE), .groups = "drop")
  
  ggplot(d, aes(x = fecha, y = mean_1000kcal)) +
    geom_line(linewidth = 0.55) +
    date_scale_monthly() +
    labs(
      title = paste0("CoNA — ", city, " — Costo promedio por 1000 kcal"),
      x = "Fecha", y = "COP por 1000 kcal"
    ) +
    theme_ts(12)
}

# 4.3) Compare cities: overall mean cost_day (averaged over Sex & Demo_Group)
plot_cona_compare_cities <- function(df) {
  d <- df %>%
    group_by(ciudad, fecha) %>%
    summarise(mean_cost_day = mean(cost_day, na.rm = TRUE), .groups = "drop")
  
  ggplot(d, aes(x = fecha, y = mean_cost_day, group = ciudad, linetype = ciudad)) +
    geom_line(linewidth = 0.55) +
    date_scale_monthly() +
    labs(
      title = "CoNA — Comparación entre ciudades",
      subtitle = "Promedio mensual de cost_day (promediado sobre Sex y Demo_Group)",
      x = "Fecha", y = "Costo diario (COP)", linetype = "Ciudad"
    ) +
    theme_ts(12)
}

# Save CoNA plots
cities2 <- sort(unique(cona_cost$ciudad))
sexes2  <- sort(unique(as.character(cona_cost$Sex)))

for (cc in cities2) {
  for (sx in sexes2) {
    p <- plot_cona_city_sex(cona_cost, cc, sx)
    out_png <- file.path(plot_dir_cona, paste0("cona__", safe_name(cc), "__", safe_name(sx), "__facet_demo.png"))
    save_plot(p, out_png, width = 15, height = 9, dpi = 300)
  }
  
  p1000 <- plot_cona_city_1000kcal(cona_cost, cc)
  if (!is.null(p1000)) {
    out_png <- file.path(plot_dir_cona, paste0("cona__", safe_name(cc), "__mean_1000kcal.png"))
    save_plot(p1000, out_png, width = 15, height = 7, dpi = 300)
  }
}

p_comp2 <- plot_cona_compare_cities(cona_cost)
save_plot(p_comp2, file.path(plot_dir_agg, "cona_compare_cities_mean_cost_day.png"), width = 15, height = 7, dpi = 300)

# -----------------------------
# 5) Optional: CoNA composition diagnostics plots (top binding nutrients)
# -----------------------------
# cona_comp usually contains which nutrients bind / constraints.
# Structure varies by FoodpriceR version; we keep a safe diagnostic:
if (nrow(cona_comp) > 0) {
  cona_comp2 <- cona_comp %>%
    mutate(
      fecha = as.Date(fecha),
      ciudad = as.character(ciudad)
    )
  
  # Try to find a "Nutrient" or similar column
  nutr_col <- intersect(names(cona_comp2), c("Nutrient","nutrient","constraint","Constraint"))
  if (length(nutr_col) >= 1) {
    nutr_col <- nutr_col[1]
    
    # Count frequency by city of binding constraints (top 10)
    diag_df <- cona_comp2 %>%
      filter(ciudad %in% cities_use) %>%
      count(ciudad, .data[[nutr_col]], sort = TRUE) %>%
      group_by(ciudad) %>%
      slice_max(n, n = 10, with_ties = FALSE) %>%
      ungroup()
    
    p_diag <- ggplot(diag_df, aes(x = reorder(.data[[nutr_col]], n), y = n)) +
      geom_col() +
      facet_wrap(~ ciudad, scales = "free_y") +
      coord_flip() +
      labs(
        title = "CoNA — Diagnóstico de restricciones más frecuentes (top 10)",
        subtitle = paste0("Columna detectada: ", nutr_col),
        x = "Restricción / nutriente", y = "Frecuencia"
      ) +
      theme_ts(12)
    
    save_plot(p_diag, file.path(plot_dir_agg, "cona_comp_top_constraints_by_city.png"),
              width = 14, height = 8, dpi = 300)
  }
}

message("DONE. Plots saved in: ", file.path(out_dir, "plots"))
