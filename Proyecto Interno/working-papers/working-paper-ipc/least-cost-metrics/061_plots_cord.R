########################################################
## 06_plots_cord.R
## Plots for CoRD (Herforth) — COST + COMP
## Inputs:
##  - out_dir/cordH_cost_fullsample.rds
##  - out_dir/cordH_comp_fullsample.rds
## Outputs:
##  - out_dir/plots/cord/cost/*.png
##  - out_dir/plots/cord/comp/*.png
##  - out_dir/plots/cord/aggregates/*.png
########################################################

source("working-papers/working-paper-ipc/least-cost-metrics/00_config.R")

suppressPackageStartupMessages({
  library(tidyverse)
  library(ggplot2)
  library(scales)
})

plot_dir_cord <- file.path(out_dir, "plots", "cord")
plot_dir_agg  <- file.path(out_dir, "plots", "aggregates")
dir.create(plot_dir_cord, recursive = TRUE, showWarnings = FALSE)
dir.create(plot_dir_agg,  recursive = TRUE, showWarnings = FALSE)

cord_cost_path <- file.path(out_dir, "cordH_cost_fullsample.rds")
if (!file.exists(cord_cost_path)) stop("Missing: ", cord_cost_path)
cord_cost_raw <- readRDS(cord_cost_path)

# ---- estandarizar ----
cord_cost_raw <- cord_cost_raw %>%
  dplyr::mutate(
    fecha      = as.Date(fecha),
    ciudad     = as.character(ciudad),
    Demo_Group = as.factor(Demo_Group),
    Sex        = as.factor(Sex),
    escenario  = as.character(escenario)
  )

cord_cost_raw <- cord_cost_raw %>% dplyr::filter(escenario == "precio_100g")

cord_cost <- cord_cost_raw %>%
  dplyr::group_by(ciudad, Sex, Demo_Group, fecha) %>%
  dplyr::summarise(
    cost_day      = mean(cost_day, na.rm = TRUE),
    Cost_1000kcal = mean(Cost_1000kcal, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::arrange(ciudad, Sex, Demo_Group, fecha)

max_n <- cord_cost %>%
  dplyr::count(ciudad, Sex, Demo_Group, fecha, name = "n") %>%
  dplyr::summarise(max_n = max(n), .groups = "drop") %>%
  dplyr::pull(max_n)

if (max_n > 1) {
  stop("SIGUEN duplicados por fecha (max_n=", max_n, "). Algo está evitando el colapso o hay otra llave oculta.")
}

# ---- utils ----
safe_name <- function(x) {
  x <- as.character(x)
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  x <- gsub("[^A-Za-z0-9]+", "_", x)
  x <- gsub("_+", "_", x)
  gsub("^_|_$", "", x)
}
save_plot <- function(p, filename, width = 15, height = 9, dpi = 300) {
  ggplot2::ggsave(filename = filename, plot = p, width = width, height = height, dpi = dpi)
}
date_scale_monthly <- function() ggplot2::scale_x_date(date_labels = "%Y-%m", date_breaks = "12 months")
theme_ts <- function(base_size = 12) {
  ggplot2::theme_bw(base_size = base_size) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = "bottom"
    )
}

# ---- plots ----
plot_city_sex_1000kcal <- function(df, city, sex) {
  d <- df %>% dplyr::filter(ciudad == city, Sex == sex) %>% dplyr::arrange(fecha)
  
  ggplot2::ggplot(d, ggplot2::aes(x = fecha, y = Cost_1000kcal, group = 1)) +
    ggplot2::geom_line(linewidth = 0.45) +
    ggplot2::facet_wrap(~ Demo_Group, ncol = 3, scales = "free_y") +
    date_scale_monthly() +
    ggplot2::labs(
      title = paste0("CoRD — Cost_1000kcal — ", city, " — ", sex),
      x = "Fecha", y = "Costo 1000 kcal (COP)"
    ) +
    theme_ts(12)
}

plot_city_sex_costday <- function(df, city, sex) {
  d <- df %>% dplyr::filter(ciudad == city, Sex == sex) %>% dplyr::arrange(fecha)
  
  ggplot2::ggplot(d, ggplot2::aes(x = fecha, y = cost_day, group = 1)) +
    ggplot2::geom_line(linewidth = 0.45) +
    ggplot2::facet_wrap(~ Demo_Group, ncol = 3, scales = "free_y") +
    date_scale_monthly() +
    ggplot2::labs(
      title = paste0("CoRD — cost_day — ", city, " — ", sex),
      x = "Fecha", y = "Costo diario (COP)"
    ) +
    theme_ts(12)
}

plot_compare_mean_1000kcal <- function(df) {
  d <- df %>%
    dplyr::group_by(ciudad, fecha) %>%
    dplyr::summarise(mean_1000kcal = mean(Cost_1000kcal, na.rm = TRUE), .groups = "drop") %>%
    dplyr::arrange(fecha)
  
  ggplot2::ggplot(d, ggplot2::aes(x = fecha, y = mean_1000kcal, group = ciudad, linetype = ciudad)) +
    ggplot2::geom_line(linewidth = 0.6) +
    date_scale_monthly() +
    ggplot2::labs(
      title = "CoRD — mean(Cost_1000kcal) por ciudad",
      x = "Fecha", y = "Costo 1000 kcal (COP)", linetype = "Ciudad"
    ) +
    theme_ts(12)
}

# ---- Save ----
cities <- sort(unique(cord_cost$ciudad))
sexes  <- sort(unique(as.character(cord_cost$Sex)))

for (cc in cities) {
  for (sx in sexes) {
    p_kcal <- plot_city_sex_1000kcal(cord_cost, cc, sx)
    save_plot(p_kcal, file.path(plot_dir_cord, paste0("cord__", safe_name(cc), "__", safe_name(sx), "__Cost_1000kcal.png")))
    
    p_day <- plot_city_sex_costday(cord_cost, cc, sx)
    save_plot(p_day, file.path(plot_dir_cord, paste0("cord__", safe_name(cc), "__", safe_name(sx), "__cost_day.png")))
  }
}

save_plot(plot_compare_mean_1000kcal(cord_cost),
          file.path(plot_dir_agg, "cord_compare_mean_1000kcal.png"),
          width = 15, height = 7)
