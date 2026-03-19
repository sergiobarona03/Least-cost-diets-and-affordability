# ============================================================
# AUDITORIA NOMINAL VS REAL
# Least-cost diets: CoCA, CoNA, CoRD
# Compara salidas ya optimizadas en nominal y en real
# ============================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(readr)
  library(readxl)
  library(lubridate)
  library(fs)
  library(scales)
  library(purrr)
})

# ------------------------------------------------------------
# RUTAS
# ------------------------------------------------------------
dir_nominal <- "C:/Users/danie/OneDrive/Escritorio/Least-cost-diets-and-affordability/Proyecto Interno/working-papers/working-paper-ipc/output/least_cost_metrics"
dir_real    <- "C:/Users/danie/OneDrive/Escritorio/Least-cost-diets-and-affordability/Proyecto Interno/working-papers/working-paper-ipc/output/least_cost_metrics/real"
dir_out     <- file.path(dir_nominal, "diagnostico_nominal_vs_real")

dir_create(dir_out)

# ------------------------------------------------------------
# ARCHIVOS A USAR
# ------------------------------------------------------------
files_to_compare <- tribble(
  ~diet,   ~file_nominal,               ~file_real,
  "CoCA",  "coca_fullsample.rds",       "coca_fullsample.rds",
  "CoNA",  "cona_cost_fullsample.rds",  "cona_cost_fullsample.rds",
  "CoRD",  "cord_cost_fullsample.rds",  "cord_cost_fullsample.rds"
)

# ------------------------------------------------------------
# HELPERS
# ------------------------------------------------------------
norm_names <- function(df){
  n <- names(df)
  n <- iconv(n, from = "", to = "ASCII//TRANSLIT")
  n <- tolower(n)
  n <- gsub("[^a-z0-9]+", "_", n)
  n <- gsub("^_|_$", "", n)
  names(df) <- n
  df
}

detect_col <- function(df, candidates, required = TRUE){
  nm <- names(df)
  hit <- candidates[candidates %in% nm][1]
  
  if (is.na(hit) || length(hit) == 0) {
    if (required) {
      stop(
        "No encontré ninguna de estas columnas: ",
        paste(candidates, collapse = ", "),
        "\nColumnas disponibles:\n",
        paste(nm, collapse = ", ")
      )
    } else {
      return(NA_character_)
    }
  }
  
  hit
}

parse_date_num <- function(x){
  if (inherits(x, "Date")) return(x)
  if (inherits(x, c("POSIXct", "POSIXt"))) return(as.Date(x))
  
  x_num <- suppressWarnings(as.numeric(x))
  as.Date(x_num, origin = "1970-01-01")
}

standardize_city <- function(x){
  x <- as.character(x)
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  x <- toupper(trimws(x))
  x <- gsub("\\.", "", x)
  x <- gsub("\\s+", " ", x)
  
  case_when(
    str_detect(x, "BOGOTA")   ~ "BOGOTA",
    str_detect(x, "MEDELLIN") ~ "MEDELLIN",
    str_detect(x, "CALI")     ~ "CALI",
    TRUE ~ x
  )
}

read_any <- function(path_file){
  ext <- tools::file_ext(path_file)
  
  if (ext == "rds") return(readRDS(path_file))
  if (ext == "csv") return(readr::read_csv(path_file, show_col_types = FALSE))
  if (ext %in% c("xls", "xlsx")) return(readxl::read_excel(path_file))
  
  stop("Formato no soportado: ", path_file)
}

prepare_cost_df <- function(path_file, diet_name, version_name){
  df <- read_any(path_file) %>%
    as_tibble() %>%
    norm_names()
  
  date_col <- detect_col(df, c("date", "fecha", "anio_mes", "month", "period"))
  city_col <- detect_col(df, c("city", "ciudad", "nombre_ciudad"))
  cost_col <- detect_col(df, c("cost_day", "daily_cost", "cost", "costo_dia", "cost_per_day"))
  
  sex_col  <- detect_col(df, c("sex"), required = FALSE)
  demo_col <- detect_col(df, c("demo_group", "demogroup", "grupo_demo"), required = FALSE)
  
  out <- df %>%
    dplyr::mutate(
      date_std = parse_date_num(.data[[date_col]]),
      city_std = standardize_city(.data[[city_col]]),
      cost_std = suppressWarnings(as.numeric(.data[[cost_col]])),
      diet     = diet_name,
      version  = version_name
    ) %>%
    dplyr::filter(!is.na(date_std), !is.na(city_std), !is.na(cost_std)) %>%
    dplyr::select(
      date_std, city_std, cost_std, diet, version,
      any_of(c(sex_col, demo_col))
    ) %>%
    dplyr::mutate(
      month = floor_date(date_std, unit = "month")
    )
  
  return(out)
}

# ------------------------------------------------------------
# LEER Y PREPARAR
# ------------------------------------------------------------
all_data <- pmap_dfr(
  files_to_compare,
  function(diet, file_nominal, file_real){
    path_nominal <- file.path(dir_nominal, file_nominal)
    path_real    <- file.path(dir_real, file_real)
    
    if (!file_exists(path_nominal)) stop("No existe: ", path_nominal)
    if (!file_exists(path_real)) stop("No existe: ", path_real)
    
    bind_rows(
      prepare_cost_df(path_nominal, diet, "nominal"),
      prepare_cost_df(path_real,    diet, "real")
    )
  }
)

# ------------------------------------------------------------
# RESUMEN GENERAL
# ------------------------------------------------------------
summary_general <- all_data %>%
  dplyr::group_by(diet, version) %>%
  dplyr::summarise(
    n = n(),
    min_date = min(month, na.rm = TRUE),
    max_date = max(month, na.rm = TRUE),
    min_cost = min(cost_std, na.rm = TRUE),
    p50_cost = median(cost_std, na.rm = TRUE),
    max_cost = max(cost_std, na.rm = TRUE),
    .groups = "drop"
  )

print(summary_general)

write_csv(summary_general, file.path(dir_out, "summary_general.csv"))

# ------------------------------------------------------------
# AGREGAR COMO EN TUS GRAFICAS:
# promedio mensual sobre Sex y Demo_Group
# ------------------------------------------------------------
series_monthly <- all_data %>%
  dplyr::group_by(diet, version, city_std, month) %>%
  dplyr::summarise(
    mean_cost_day = mean(cost_std, na.rm = TRUE),
    n_obs = n(),
    .groups = "drop"
  )

write_csv(series_monthly, file.path(dir_out, "series_monthly_nominal_real.csv"))

# ------------------------------------------------------------
# UNIR NOMINAL Y REAL
# ------------------------------------------------------------
compare_monthly <- series_monthly %>%
  dplyr::select(diet, version, city_std, month, mean_cost_day) %>%
  tidyr::pivot_wider(names_from = version, values_from = mean_cost_day) %>%
  dplyr::group_by(diet, city_std) %>%
  dplyr::arrange(month, .by_group = TRUE) %>%
  dplyr::mutate(
    implicit_deflator = nominal / real,
    real_growth_yoy   = (real / dplyr::lag(real, 12) - 1) * 100,
    nom_growth_yoy    = (nominal / dplyr::lag(nominal, 12) - 1) * 100
  ) %>%
  dplyr::ungroup()

write_csv(compare_monthly, file.path(dir_out, "compare_monthly_nominal_real.csv"))

# ------------------------------------------------------------
# ALERTAS
# ------------------------------------------------------------
alerts <- compare_monthly %>%
  dplyr::group_by(diet, city_std) %>%
  dplyr::arrange(month, .by_group = TRUE) %>%
  dplyr::mutate(
    jump_real = real / dplyr::lag(real),
    jump_nom  = nominal / dplyr::lag(nominal)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::filter(
    (!is.na(jump_real) & jump_real > 1.20) |
      (!is.na(jump_nom) & jump_nom > 1.20)
  ) %>%
  dplyr::arrange(desc(jump_real), desc(jump_nom))

write_csv(alerts, file.path(dir_out, "alerts_large_jumps.csv"))

# ------------------------------------------------------------
# GRAFICO 1: nominal vs real
# ------------------------------------------------------------
plot_nom_real <- function(diet_name){
  dfp <- compare_monthly %>%
    dplyr::filter(diet == diet_name) %>%
    tidyr::pivot_longer(
      cols = c(nominal, real),
      names_to = "series",
      values_to = "value"
    )
  
  g <- ggplot(dfp, aes(x = month, y = value, linetype = series)) +
    geom_line(linewidth = 0.8) +
    facet_wrap(~city_std, scales = "free_y") +
    labs(
      title = paste0(diet_name, " — nominal vs real"),
      subtitle = "Promedio mensual de cost_day por ciudad",
      x = "Fecha",
      y = "Costo diario (COP)",
      linetype = ""
    ) +
    theme_minimal(base_size = 13)
  
  ggsave(
    filename = file.path(dir_out, paste0("plot_", tolower(diet_name), "_nominal_vs_real.png")),
    plot = g, width = 12, height = 7, dpi = 300
  )
}

walk(unique(compare_monthly$diet), plot_nom_real)

# ------------------------------------------------------------
# GRAFICO 2: deflactor implícito
# ------------------------------------------------------------
plot_deflator <- function(diet_name){
  dfp <- compare_monthly %>%
    dplyr::filter(diet == diet_name)
  
  g <- ggplot(dfp, aes(x = month, y = implicit_deflator, linetype = city_std)) +
    geom_line(linewidth = 0.8) +
    labs(
      title = paste0(diet_name, " — deflactor implícito (nominal/real)"),
      subtitle = "Si aquí ves rarezas fuertes, el problema puede estar en la construcción del real",
      x = "Fecha",
      y = "Deflactor implícito",
      linetype = "Ciudad"
    ) +
    theme_minimal(base_size = 13)
  
  ggsave(
    filename = file.path(dir_out, paste0("plot_", tolower(diet_name), "_implicit_deflator.png")),
    plot = g, width = 12, height = 7, dpi = 300
  )
}

walk(unique(compare_monthly$diet), plot_deflator)

# ------------------------------------------------------------
# GRAFICO 3: YoY nominal vs real
# ------------------------------------------------------------
plot_yoy <- function(diet_name){
  dfp <- compare_monthly %>%
    dplyr::filter(diet == diet_name) %>%
    dplyr::select(diet, city_std, month, real_growth_yoy, nom_growth_yoy) %>%
    tidyr::pivot_longer(
      cols = c(real_growth_yoy, nom_growth_yoy),
      names_to = "series",
      values_to = "yoy"
    )
  
  g <- ggplot(dfp, aes(x = month, y = yoy, linetype = series)) +
    geom_line(linewidth = 0.8) +
    facet_wrap(~city_std, scales = "free_y") +
    labs(
      title = paste0(diet_name, " — crecimiento interanual"),
      subtitle = "Comparación nominal vs real",
      x = "Fecha",
      y = "% YoY",
      linetype = ""
    ) +
    theme_minimal(base_size = 13)
  
  ggsave(
    filename = file.path(dir_out, paste0("plot_", tolower(diet_name), "_yoy_nominal_real.png")),
    plot = g, width = 12, height = 7, dpi = 300
  )
}

walk(unique(compare_monthly$diet), plot_yoy)

# ------------------------------------------------------------
# TABLA DE PICOS
# ------------------------------------------------------------
peak_table <- compare_monthly %>%
  dplyr::group_by(diet, city_std) %>%
  dplyr::summarise(
    peak_nominal = max(nominal, na.rm = TRUE),
    month_peak_nominal = month[which.max(nominal)][1],
    peak_real = max(real, na.rm = TRUE),
    month_peak_real = month[which.max(real)][1],
    .groups = "drop"
  )

write_csv(peak_table, file.path(dir_out, "peak_table.csv"))

print(peak_table)

cat("\nListo. Revisa la carpeta:\n", dir_out, "\n")