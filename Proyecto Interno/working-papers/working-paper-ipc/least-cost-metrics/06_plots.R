########################################################
## 06_plots_coca_cona_cord.R
## Plots for CoCA, CoNA and CoRD (full sample, 3 cities)
## Inputs (created by scripts 00–04):
##  - out_dir/coca_fullsample.rds
##  - out_dir/cona_cost_fullsample.rds  (+ cona_comp_fullsample.rds optional)
##  - out_dir/cord_cost_fullsample.rds  (+ cord_comp_fullsample.rds optional)
## Outputs:
##  - out_dir/plots/<metric>/*.png
##  - out_dir/plots/aggregates/*.png
########################################################

# -----------------------------
# 0) Load config + packages
# -----------------------------
source("working-papers/working-paper-ipc/least-cost-metrics/00_config.R")

suppressPackageStartupMessages({
  library(tidyverse)
  library(scales)
})

# -----------------------------
# 0.1) Helpers
# -----------------------------
safe_name <- function(x) {
  x %>%
    as.character() %>%
    stringr::str_to_lower() %>%
    stringr::str_replace_all("[^a-z0-9]+", "_") %>%
    stringr::str_replace_all("^_|_$", "")
}

save_plot <- function(p, filename, width = 15, height = 9, dpi = 300) {
  ggsave(filename = filename, plot = p, width = width, height = height, dpi = dpi, bg = "white")
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

# If Sex is coded 1/2, label (optional)
relabel_sex <- function(x) {
  lv <- levels(x)
  if (all(lv %in% c("1","2"))) {
    factor(x, levels = c("1","2"), labels = c("Hombres","Mujeres"))
  } else x
}

# -----------------------------
# 0.2) Plot directories
# -----------------------------
plot_dir_base <- file.path(out_dir, "plots")
plot_dir_agg  <- file.path(plot_dir_base, "aggregates")
dir.create(plot_dir_agg, recursive = TRUE, showWarnings = FALSE)

metric_dirs <- function(metric) {
  d <- file.path(plot_dir_base, metric)
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  d
}

# -----------------------------
# 1) Input paths
# -----------------------------
paths <- list(
  CoCA = list(
    cost = file.path(out_dir, "coca_fullsample.rds"),
    comp = NULL
  ),
  CoNA = list(
    cost = file.path(out_dir, "cona_cost_fullsample.rds"),
    comp = file.path(out_dir, "cona_comp_fullsample.rds")
  ),
  CoRD = list(
    cost = file.path(out_dir, "cord_fullsample.rds"),        
    comp = file.path(out_dir, "cord_comp_fullsample.rds")    
  )
)

# -----------------------------
# 2) Loaders
# -----------------------------
load_metric <- function(metric) {
  p_cost <- paths[[metric]]$cost
  p_comp <- paths[[metric]]$comp
  
  if (is.null(p_cost) || !file.exists(p_cost)) stop("Missing cost file for ", metric, ": ", p_cost)
  
  cost <- readRDS(p_cost)
  
  # comp is optional
  comp <- NULL
  if (!is.null(p_comp) && file.exists(p_comp)) {
    comp <- readRDS(p_comp)
  }
  
  # Basic checks (FoodpriceR outputs vary)
  required <- c("fecha", "ciudad", "Sex", "Demo_Group", "cost_day")
  miss <- setdiff(required, names(cost))
  if (length(miss) > 0) warning(metric, " missing columns: ", paste(miss, collapse = ", "))
  
  # Standardize types
  cost <- cost %>%
    mutate(
      fecha      = as.Date(fecha),
      ciudad     = as.character(ciudad),
      Demo_Group = as.factor(Demo_Group),
      Sex        = as.factor(Sex)
    )
  
  cost$Sex <- relabel_sex(cost$Sex)
  
  if (!is.null(comp) && nrow(comp) > 0 && "fecha" %in% names(comp)) {
    comp <- comp %>%
      mutate(
        fecha  = as.Date(fecha),
        ciudad = as.character(ciudad)
      )
  }
  
  list(cost = cost, comp = comp)
}

# -----------------------------
# 3) Generic plot functions (work for ANY metric)
# -----------------------------

# 3.1) City x Sex: facet by Demo_Group (cost_day)
plot_city_sex_facet_demo <- function(df, metric, city, sex) {
  d <- df %>% filter(ciudad == city, Sex == sex)
  
  ggplot(d, aes(x = fecha, y = cost_day)) +
    geom_line(linewidth = 0.45) +
    facet_wrap(~ Demo_Group, ncol = 3, scales = "free_y") +
    date_scale_monthly() +
    labs(
      title = paste0(metric, " — ", city, " — ", sex),
      subtitle = "Daily cost (cost_day)",
      x = "Date", y = "Daily cost (COP)"
    ) +
    theme_ts(12)
}

# 3.2) City aggregate: mean Cost_1000kcal across Sex & Demo_Group (if exists)
plot_city_mean_1000kcal <- function(df, metric, city) {
  if (!("Cost_1000kcal" %in% names(df))) return(NULL)
  
  d <- df %>%
    filter(ciudad == city) %>%
    group_by(fecha) %>%
    summarise(mean_1000kcal = mean(Cost_1000kcal, na.rm = TRUE), .groups = "drop")
  
  ggplot(d, aes(x = fecha, y = mean_1000kcal)) +
    geom_line(linewidth = 0.55) +
    date_scale_monthly() +
    labs(
      title = paste0(metric, " — ", city, " — Mean cost per 1,000 kcal"),
      x = "Date", y = "COP per 1,000 kcal"
    ) +
    theme_ts(12)
}

# 3.3) Compare cities: overall mean cost_day (averaged over Sex & Demo_Group)
plot_compare_cities_mean_cost_day <- function(df, metric) {
  d <- df %>%
    group_by(ciudad, fecha) %>%
    summarise(mean_cost_day = mean(cost_day, na.rm = TRUE), .groups = "drop")
  
  ggplot(d, aes(x = fecha, y = mean_cost_day, group = ciudad, linetype = ciudad)) +
    geom_line(linewidth = 0.55) +
    date_scale_monthly() +
    labs(
      title = paste0(metric, " — City comparison"),
      subtitle = "Monthly mean of cost_day (averaged over Sex and Demo_Group)",
      x = "Date", y = "Daily cost (COP)", linetype = "City"
    ) +
    theme_ts(12)
}

# -----------------------------
# 4) Optional: composition diagnostics (for metrics with *_comp_fullsample)
# -----------------------------
plot_comp_top_constraints_by_city <- function(comp_df, metric, out_png) {
  if (is.null(comp_df) || nrow(comp_df) == 0) return(invisible(NULL))
  
  nutr_col <- intersect(names(comp_df), c("Nutrient","nutrient","constraint","Constraint"))
  if (length(nutr_col) == 0) return(invisible(NULL))
  nutr_col <- nutr_col[1]
  
  cities <- sort(unique(comp_df$ciudad))
  
  diag_df <- comp_df %>%
    filter(ciudad %in% cities) %>%
    count(ciudad, .data[[nutr_col]], sort = TRUE) %>%
    group_by(ciudad) %>%
    slice_max(n, n = 10, with_ties = FALSE) %>%
    ungroup()
  
  p <- ggplot(diag_df, aes(x = reorder(.data[[nutr_col]], n), y = n)) +
    geom_col() +
    facet_wrap(~ ciudad, scales = "free_y") +
    coord_flip() +
    labs(
      title = paste0(metric, " — Most frequent binding constraints (top 10)"),
      subtitle = paste0("Detected column: ", nutr_col),
      x = "Constraint / nutrient", y = "Frequency"
    ) +
    theme_ts(12)
  
  save_plot(p, out_png, width = 14, height = 8, dpi = 300)
  p
}

# -----------------------------
# 5) Run everything for each metric
# -----------------------------
metrics <- c("CoCA", "CoNA", "CoRD")

for (metric in metrics) {
  message("Running plots for: ", metric)
  
  obj <- load_metric(metric)
  df_cost <- obj$cost
  df_comp <- obj$comp
  
  metric_dir <- metric_dirs(tolower(metric))
  
  cities <- sort(unique(df_cost$ciudad))
  sexes  <- sort(unique(as.character(df_cost$Sex)))
  
  # City x Sex facet Demo_Group
  for (cc in cities) {
    for (sx in sexes) {
      p <- plot_city_sex_facet_demo(df_cost, metric, cc, sx)
      out_png <- file.path(metric_dir, paste0(tolower(metric), "__", safe_name(cc), "__", safe_name(sx), "__facet_demo.png"))
      save_plot(p, out_png, width = 15, height = 9, dpi = 300)
    }
    
    # Mean 1000kcal (if exists)
    p1000 <- plot_city_mean_1000kcal(df_cost, metric, cc)
    if (!is.null(p1000)) {
      out_png <- file.path(metric_dir, paste0(tolower(metric), "__", safe_name(cc), "__mean_1000kcal.png"))
      save_plot(p1000, out_png, width = 15, height = 7, dpi = 300)
    }
  }
  
  # Compare cities mean cost_day
  p_comp <- plot_compare_cities_mean_cost_day(df_cost, metric)
  save_plot(
    p_comp,
    file.path(plot_dir_agg, paste0(tolower(metric), "_compare_cities_mean_cost_day.png")),
    width = 15, height = 7, dpi = 300
  )
  
  # Optional comp diagnostics
  if (!is.null(df_comp) && nrow(df_comp) > 0) {
    plot_comp_top_constraints_by_city(
      df_comp, metric,
      out_png = file.path(plot_dir_agg, paste0(tolower(metric), "_comp_top_constraints_by_city.png"))
    )
  }
}

message("DONE. Plots saved in: ", plot_dir_base)