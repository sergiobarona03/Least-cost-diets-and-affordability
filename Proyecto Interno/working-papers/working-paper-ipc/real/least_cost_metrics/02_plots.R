########################################################
## 02_plots_real.R
########################################################

# -----------------------------
# 0) Packages
# -----------------------------
suppressPackageStartupMessages({
  library(tidyverse)
  library(scales)
})

# -----------------------------
# 0.1) DIRECTORIO BASE
# -----------------------------
base_dir <- "C:/Users/danie/OneDrive/Escritorio/Least-cost-diets-and-affordability/Proyecto Interno/working-papers/working-paper-ipc"

out_dir <- file.path(base_dir, "output/real/least_cost_metrics")

# -----------------------------
# 0.2) Helpers
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

relabel_sex <- function(x) {
  lv <- levels(x)
  if (all(lv %in% c("1","2"))) {
    factor(x, levels = c("1","2"), labels = c("Hombres","Mujeres"))
  } else x
}

# -----------------------------
# 0.3) Plot directories
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
# 1) Input paths (REAL DATA)
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
    cost = file.path(out_dir, "cord_cost_fullsample.rds"),
    comp = file.path(out_dir, "cord_comp_fullsample.rds")
  )
)

# -----------------------------
# 2) Loaders
# -----------------------------
load_metric <- function(metric) {
  
  p_cost <- paths[[metric]]$cost
  p_comp <- paths[[metric]]$comp
  
  cost <- readRDS(p_cost)
  
  comp <- NULL
  if (!is.null(p_comp) && file.exists(p_comp)) {
    comp <- readRDS(p_comp)
  }
  
  cost <- cost %>%
    mutate(
      fecha      = as.Date(fecha),   
      ciudad     = as.character(ciudad),
      Demo_Group = as.factor(Demo_Group),
      Sex        = as.factor(Sex)
    )
  
  cost$Sex <- relabel_sex(cost$Sex)
  
  if (!is.null(comp)) {
    comp <- comp %>%
      mutate(
        fecha = as.Date(fecha),
        ciudad = as.character(ciudad)
      )
  }
  
  list(cost = cost, comp = comp)
}

# -----------------------------
# 3) PLOTS (USANDO COSTOS REALES)
# -----------------------------

plot_city_sex_facet_demo <- function(df, metric, city, sex) {
  
  d <- df %>% filter(ciudad == city, Sex == sex)
  
  ggplot(d, aes(x = fecha, y = cost_day_real)) +
    geom_line(linewidth = 0.45) +
    facet_wrap(~ Demo_Group, ncol = 3, scales = "free_y") +
    date_scale_monthly() +
    labs(
      title = paste0(metric, " — ", city, " — ", sex),
      subtitle = "Daily cost (REAL)",
      x = "Date", y = "Daily cost (COP reales)"
    ) +
    theme_ts(12)
}

plot_city_mean_1000kcal <- function(df, metric, city) {
  
  if (!("Cost_1000kcal_real" %in% names(df))) return(NULL)
  
  d <- df %>%
    dplyr::filter(ciudad == city) %>%
    dplyr::group_by(fecha) %>%
    dplyr::summarise(mean_1000kcal = mean(Cost_1000kcal_real, na.rm = TRUE), .groups = "drop")
  
  ggplot(d, aes(x = fecha, y = mean_1000kcal)) +
    geom_line(linewidth = 0.55) +
    date_scale_monthly() +
    labs(
      title = paste0(metric, " — ", city, " — Mean cost per 1,000 kcal (REAL)"),
      x = "Date", y = "COP reales por 1,000 kcal"
    ) +
    theme_ts(12)
}

plot_compare_cities_mean_cost_day <- function(df, metric) {
  
  d <- df %>%
    dplyr::group_by(ciudad, fecha) %>%
    dplyr::summarise(mean_cost_day = mean(cost_day_real, na.rm = TRUE), .groups = "drop")
  
  ggplot(d, aes(x = fecha, y = mean_cost_day, group = ciudad, linetype = ciudad)) +
    geom_line(linewidth = 0.55) +
    date_scale_monthly() +
    labs(
      title = paste0(metric, " — City comparison (REAL)"),
      subtitle = "Monthly mean of cost_day_real",
      x = "Date", y = "Daily cost (COP reales)", linetype = "City"
    ) +
    theme_ts(12)
}

# -----------------------------
# 4) RUN
# -----------------------------
metrics <- c("CoCA", "CoNA", "CoRD")

for (metric in metrics) {
  
  message("Running plots for: ", metric)
  
  obj <- load_metric(metric)
  df_cost <- obj$cost
  
  metric_dir <- metric_dirs(tolower(metric))
  
  cities <- sort(unique(df_cost$ciudad))
  sexes  <- sort(unique(as.character(df_cost$Sex)))
  
  for (cc in cities) {
    for (sx in sexes) {
      
      p <- plot_city_sex_facet_demo(df_cost, metric, cc, sx)
      
      save_plot(
        p,
        file.path(metric_dir, paste0(tolower(metric), "__", safe_name(cc), "__", safe_name(sx), "__facet_demo.png"))
      )
    }
    
    p1000 <- plot_city_mean_1000kcal(df_cost, metric, cc)
    
    if (!is.null(p1000)) {
      save_plot(
        p1000,
        file.path(metric_dir, paste0(tolower(metric), "__", safe_name(cc), "__mean_1000kcal.png"))
      )
    }
  }
  
  p_comp <- plot_compare_cities_mean_cost_day(df_cost, metric)
  
  save_plot(
    p_comp,
    file.path(plot_dir_agg, paste0(tolower(metric), "_compare_cities_real.png"))
  )
}

message("DONE. Plots saved in: ", plot_dir_base)