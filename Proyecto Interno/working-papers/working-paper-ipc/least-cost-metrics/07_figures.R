#######################################################################
## FIGURA 2 y FIGURA 3 (REAL) - Monthly + Quarterly
## CoCA / CoNA / CoRD - hogar representativo
#######################################################################

#----------------------------------------------------------------------
# Packages
#----------------------------------------------------------------------
library(tidyverse)
library(lubridate)
library(stringr)
library(scales)
library(ggsci)
library(janitor)
library(readxl)

#----------------------------------------------------------------------
# Directories
#----------------------------------------------------------------------
base_dir <- "C:\\Users\\danie\\OneDrive\\Escritorio\\Least-cost-diets-and-affordability\\Proyecto Interno\\"
setwd(base_dir)

out_dir <- file.path(base_dir, "working-papers/working-paper-ipc/output")
afford_cost_dir <- file.path(out_dir, "affordability/real")
least_cost_dir <- file.path(out_dir, "least_cost_metrics/real")
fig_dir <- file.path(out_dir, "figures_paper/real")
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

#----------------------------------------------------------------------
# Input paths (REAL)
#----------------------------------------------------------------------
paths <- list(
  month = list(
    CoCA = file.path(afford_cost_dir, "CoCA_city_month.rds"),
    CoNA = file.path(afford_cost_dir, "CoNA_city_month.rds"),
    CoRD = file.path(afford_cost_dir, "CoRD_city_month.rds")
  ),
  quarter = list(
    CoCA = file.path(afford_cost_dir, "CoCA_city_cuartiles.rds"),
    CoNA = file.path(afford_cost_dir, "CoNA_city_cuartiles.rds"),
    CoRD = file.path(afford_cost_dir, "CoRD_city_cuartiles.rds")
  )
)

paths_kcal <- list(
  CoCA = file.path(least_cost_dir, "coca_fullsample.xlsx"),
  CoNA = file.path(least_cost_dir, "cona_fullsample.xlsx"),
  CoRD = file.path(least_cost_dir, "cord_fullsample.xlsx")
)

#----------------------------------------------------------------------
# City standardization + Paper style
#----------------------------------------------------------------------
city_levels <- c("BOGOTA", "CALI", "MEDELLIN")

std_city <- function(x) {
  x <- as.character(x)
  dplyr::case_when(
    x %in% c("BOGOTÁ D.C.", "BOGOTA D.C.", "BOGOTA") ~ "BOGOTA",
    x %in% c("MEDELLÍN", "MEDELLIN")                 ~ "MEDELLIN",
    x %in% c("CALI")                                ~ "CALI",
    TRUE ~ x
  )
}

theme_paper <- theme_classic(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(size = 11),
    axis.title = element_text(size = 11),
    axis.text  = element_text(size = 10, color = "black"),
    legend.title = element_text(size = 10),
    legend.text  = element_text(size = 10),
    legend.position = "top",
    legend.justification = "left",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(linewidth = 0.4),
    axis.ticks = element_line(linewidth = 0.4),
    plot.margin = margin(6, 6, 6, 6)
  )

color_scale_city <- scale_color_nejm(name = "City")
color_scale_demo <- scale_color_brewer(palette = "Dark2", name = "Demographic group")

#----------------------------------------------------------------------
# Helpers
#----------------------------------------------------------------------
sex_lab <- function(x) {
  dplyr::case_when(
    is.na(x) ~ NA_character_,
    x %in% c(0, "0", "H", "MASC", "Male", "male") ~ "H",
    x %in% c(1, "1", "M", "FEM", "Female", "female") ~ "M",
    TRUE ~ as.character(x)
  )
}

assign_person <- function(demo_group, sex) {
  sex_hm <- sex_lab(sex)
  dplyr::case_when(
    demo_group == "31 a 50 años" & sex_hm == "H" ~ 1L,
    demo_group == "31 a 50 años" & sex_hm == "M" ~ 2L,
    demo_group == "9 a 13 años"  & sex_hm == "M" ~ 3L,
    TRUE ~ NA_integer_
  )
}

#----------------------------------------------------------------------
# Date 
#----------------------------------------------------------------------
coerce_to_date <- function(x) {
  if (inherits(x, "Date")) return(x)
  
  if (inherits(x, c("POSIXct", "POSIXt"))) {
    return(as.Date(x))
  }
  
  # numérico: unix si es pequeño, excel si es grande
  if (is.numeric(x)) {
    out <- as.Date(rep(NA_character_, length(x)))
    unix_idx  <- !is.na(x) & x < 30000
    excel_idx <- !is.na(x) & x >= 30000
    
    out[unix_idx]  <- as.Date(x[unix_idx],  origin = "1970-01-01")
    out[excel_idx] <- as.Date(x[excel_idx], origin = "1899-12-30")
    return(out)
  }
  
  x_chr <- trimws(as.character(x))
  x_chr[x_chr %in% c("", "NA", "NaN", "NULL")] <- NA_character_
  
  out <- as.Date(rep(NA_character_, length(x_chr)))
  
  # YYYY-MM-DD o YYYY/MM/DD
  idx <- !is.na(x_chr) & stringr::str_detect(x_chr, "^\\d{4}[-/]\\d{2}[-/]\\d{2}$")
  if (any(idx)) {
    out[idx] <- suppressWarnings(as.Date(stringr::str_replace_all(x_chr[idx], "/", "-")))
  }
  
  # YYYY-MM o YYYY/MM
  idx <- is.na(out) & !is.na(x_chr) & stringr::str_detect(x_chr, "^\\d{4}[-/]\\d{2}$")
  if (any(idx)) {
    out[idx] <- suppressWarnings(as.Date(paste0(stringr::str_replace_all(x_chr[idx], "/", "-"), "-01")))
  }
  
  # YYYYMM
  idx <- is.na(out) & !is.na(x_chr) & stringr::str_detect(x_chr, "^\\d{6}$")
  if (any(idx)) {
    yy <- substr(x_chr[idx], 1, 4)
    mm <- substr(x_chr[idx], 5, 6)
    out[idx] <- suppressWarnings(as.Date(sprintf("%s-%s-01", yy, mm)))
  }
  
  # por si llega numérico como texto
  num <- suppressWarnings(as.numeric(x_chr))
  idx <- is.na(out) & !is.na(num)
  if (any(idx)) {
    unix_idx  <- num[idx] < 30000
    excel_idx <- num[idx] >= 30000
    
    tmp <- as.Date(rep(NA_character_, sum(idx)))
    tmp[unix_idx]  <- as.Date(num[idx][unix_idx],  origin = "1970-01-01")
    tmp[excel_idx] <- as.Date(num[idx][excel_idx], origin = "1899-12-30")
    
    out[idx] <- tmp
  }
  
  out
}

safe_date_month <- function(df) {
  if ("fecha" %in% names(df)) {
    df %>% dplyr::mutate(fecha = coerce_to_date(fecha))
  } else if (all(c("year","mes") %in% names(df))) {
    df %>%
      dplyr::mutate(
        year  = as.integer(year),
        mes   = as.integer(mes),
        fecha = as.Date(sprintf("%d-%02d-01", year, mes))
      )
  } else {
    stop("Missing date info: need 'fecha' or (year, mes) for monthly.")
  }
}

safe_date_quarter <- function(df) {
  if ("trimestre" %in% names(df)) {
    df <- df %>% dplyr::mutate(trimestre = as.character(trimestre))
    tri_chr <- trimws(df$trimestre)
    
    fecha_q <- as.Date(rep(NA_character_, length(tri_chr)))
    
    # 1999Q1 / 1999-Q1 / 1999 Q1
    idx_q <- !is.na(tri_chr) & stringr::str_detect(tri_chr, "^\\d{4}[- ]?Q[1-4]$")
    if (any(idx_q)) {
      tri_std <- stringr::str_replace_all(tri_chr[idx_q], "[- ]", "")
      yy <- as.integer(substr(tri_std, 1, 4))
      qq <- as.integer(substr(tri_std, 6, 6))
      mm <- c(1, 4, 7, 10)[qq]
      fecha_q[idx_q] <- as.Date(sprintf("%d-%02d-01", yy, mm))
    }
    
    # respaldo: trimestre ya como fecha normal
    idx_other <- is.na(fecha_q) & !is.na(tri_chr)
    if (any(idx_other)) {
      fecha_q[idx_other] <- coerce_to_date(tri_chr[idx_other])
    }
    
    df %>%
      dplyr::mutate(
        fecha = fecha_q,
        year  = lubridate::year(fecha),
        q     = lubridate::quarter(fecha),
        trimestre = paste0(year, "Q", q)
      )
    
  } else if ("fecha" %in% names(df)) {
    df %>%
      dplyr::mutate(
        fecha = coerce_to_date(fecha),
        year  = lubridate::year(fecha),
        q     = lubridate::quarter(fecha),
        trimestre = paste0(year, "Q", q)
      )
    
  } else if (all(c("year","q") %in% names(df))) {
    df %>%
      dplyr::mutate(
        year = as.integer(year),
        q    = as.integer(q),
        fecha = as.Date(sprintf("%d-%02d-01", year, c(1, 4, 7, 10)[q])),
        trimestre = paste0(year, "Q", q)
      )
  } else {
    stop("Missing quarter info: need 'trimestre', 'fecha' or (year, q).")
  }
}

harmonize_names <- function(df) {
  df <- janitor::clean_names(df)
  
  rename_if_exists <- function(data, new, old_candidates) {
    hit <- intersect(old_candidates, names(data))
    if (length(hit) > 0 && !(new %in% names(data))) {
      data <- data %>% dplyr::rename(!!new := all_of(hit[1]))
    }
    data
  }
  
  df <- rename_if_exists(df, "demo_group",    c("demo_group", "demogroup", "grupo_demo", "grupo_demografico"))
  df <- rename_if_exists(df, "sex",           c("sex", "sexo"))
  df <- rename_if_exists(df, "person",        c("person", "persona", "p"))
  df <- rename_if_exists(df, "cost_day",      c("cost_day", "daily_cost"))
  df <- rename_if_exists(df, "cost_1000kcal", c("cost_1000kcal", "cost_1000_kcal", "cost1000kcal"))
  df <- rename_if_exists(df, "per_capita",    c("per_capita", "percapita", "pc_cost"))
  df <- rename_if_exists(df, "ciudad",        c("ciudad", "city", "municipio", "ciudad_nombre"))
  df <- rename_if_exists(df, "fecha",         c("fecha", "date", "period"))
  df <- rename_if_exists(df, "trimestre",     c("trimestre", "quarter_label"))
  
  df
}

is_rep_household <- function(demo_group, sex) {
  sex_hm <- sex_lab(sex)
  (demo_group == "31 a 50 años" & sex_hm == "H") |
    (demo_group == "31 a 50 años" & sex_hm == "M") |
    (demo_group == "9 a 13 años"  & sex_hm == "M")
}

make_quarter_axis <- function(df) {
  df <- df %>% dplyr::mutate(trimestre = as.character(trimestre))
  
  tri_levels <- df %>%
    dplyr::distinct(trimestre) %>%
    dplyr::filter(!is.na(trimestre)) %>%
    dplyr::mutate(
      year = as.integer(stringr::str_extract(trimestre, "^\\d{4}")),
      q    = as.integer(stringr::str_extract(trimestre, "(?<=Q)\\d+"))
    ) %>%
    dplyr::arrange(year, q) %>%
    dplyr::pull(trimestre)
  
  df %>% dplyr::mutate(trimestre = factor(trimestre, levels = tri_levels))
}

quarter_breaks_q1_2y <- function(tri_levels) {
  tmp <- tibble::tibble(trimestre = tri_levels) %>%
    dplyr::filter(!is.na(trimestre)) %>%
    dplyr::mutate(
      year = as.integer(stringr::str_extract(trimestre, "^\\d{4}")),
      q    = as.integer(stringr::str_extract(trimestre, "(?<=Q)\\d+"))
    ) %>%
    dplyr::arrange(year, q) %>%
    dplyr::filter(q == 1)
  
  if (nrow(tmp) == 0) return(tri_levels)
  
  yrs_keep <- tmp$year[seq(1, nrow(tmp), by = 2)]
  
  tmp %>%
    dplyr::filter(year %in% yrs_keep) %>%
    dplyr::pull(trimestre)
}

#----------------------------------------------------------------------
# Loaders
#----------------------------------------------------------------------
load_model <- function(path, model, freq = c("month","quarter")) {
  freq <- match.arg(freq)
  if (!file.exists(path)) stop("File not found: ", path)
  
  df <- readRDS(path) %>% harmonize_names()
  
  if (!("ciudad" %in% names(df))) stop("Missing column 'ciudad'.")
  df <- if (freq == "month") safe_date_month(df) else safe_date_quarter(df)
  
  if (!("demo_group" %in% names(df))) df$demo_group <- NA_character_
  if (!("sex" %in% names(df))) df$sex <- NA
  
  if (!("person" %in% names(df))) {
    df$person <- assign_person(df$demo_group, df$sex)
  }
  
  if (!("per_capita" %in% names(df)) && "cost_day" %in% names(df)) {
    df$per_capita <- suppressWarnings(as.numeric(df$cost_day))
  }
  
  out <- df %>%
    dplyr::mutate(
      fecha = coerce_to_date(fecha),
      ciudad = factor(std_city(ciudad), levels = city_levels),
      model  = model,
      sex_hm = sex_lab(sex),
      keep_rep = is_rep_household(demo_group, sex),
      person = ifelse(is.na(person), assign_person(demo_group, sex), as.integer(person)),
      demo_label = dplyr::case_when(
        keep_rep & !is.na(demo_group) & !is.na(sex_hm) & !is.na(person) ~ paste0(demo_group, " ", sex_hm, " (P", person, ")"),
        keep_rep & !is.na(demo_group) & !is.na(sex_hm) ~ paste0(demo_group, " ", sex_hm),
        TRUE ~ NA_character_
      )
    ) %>%
    dplyr::filter(keep_rep, !is.na(fecha))
  
  message(model, " - ", freq, ": n = ", nrow(out))
  out
}

bind_all_models <- function(freq = c("month","quarter")) {
  freq <- match.arg(freq)
  
  out <- dplyr::bind_rows(
    load_model(paths[[freq]]$CoCA, "CoCA", freq),
    load_model(paths[[freq]]$CoNA, "CoNA", freq),
    load_model(paths[[freq]]$CoRD, "CoRD", freq)
  ) %>%
    dplyr::mutate(model = factor(model, levels = c("CoCA","CoNA","CoRD")))
  
  message("Total rows in bind_all_models(", freq, "): ", nrow(out))
  out
}

pick_yvar_fig2 <- function(df) {
  if ("cost_day" %in% names(df)) return("cost_day")
  stop("Figure 2 requires 'cost_day' in the input bases.")
}

#----------------------------------------------------------------------
# FIGURE 2
#----------------------------------------------------------------------
plot_fig2_month <- function(df, out_file) {
  yvar <- pick_yvar_fig2(df)
  
  plot_df <- df %>%
    dplyr::filter(!is.na(ciudad), !is.na(fecha), !is.na(model), !is.na(demo_label))
  
  if (nrow(plot_df) == 0) stop("plot_fig2_month: no rows left after filtering.")
  
  p <- ggplot(plot_df, aes(x = fecha, y = .data[[yvar]], color = demo_label, group = demo_label)) +
    geom_line(linewidth = 0.9) +
    facet_grid(rows = vars(model), cols = vars(ciudad), scales = "free_y") +
    scale_x_date(
      date_breaks = "2 years",
      date_labels = "%Y",
      expand = expansion(mult = c(0.01, 0.01))
    ) +
    labs(
      title = "CoCA, CoNA and CoRD evolution by demographic group (representative household)",
      x = NULL,
      y = "Daily cost per person (COP)"
    ) +
    color_scale_demo +
    theme_paper +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
  
  ggsave(
    filename = file.path(fig_dir, out_file),
    plot = p,
    width = 14, height = 7,
    dpi = 300, bg = "white"
  )
  p
}

plot_fig2_quarter <- function(df, out_file) {
  yvar <- pick_yvar_fig2(df)
  
  df2 <- df %>% make_quarter_axis()
  brks <- quarter_breaks_q1_2y(levels(df2$trimestre))
  
  plot_df <- df2 %>%
    dplyr::filter(!is.na(ciudad), !is.na(model), !is.na(demo_label), !is.na(trimestre))
  
  if (nrow(plot_df) == 0) stop("plot_fig2_quarter: no rows left after filtering.")
  
  p <- ggplot(plot_df, aes(x = trimestre, y = .data[[yvar]], color = demo_label, group = demo_label)) +
    geom_line(linewidth = 0.9) +
    facet_grid(rows = vars(model), cols = vars(ciudad), scales = "free_y") +
    scale_x_discrete(breaks = brks, labels = brks) +
    labs(
      title = "CoCA, CoNA and CoRD evolution by demographic group (representative household)",
      x = NULL,
      y = "Daily cost per person (COP)"
    ) +
    color_scale_demo +
    theme_paper +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
  
  ggsave(
    filename = file.path(fig_dir, out_file),
    plot = p,
    width = 14, height = 7,
    dpi = 300, bg = "white"
  )
  p
}

#----------------------------------------------------------------------
# FIGURE 3
#----------------------------------------------------------------------
plot_fig3_month <- function(df, out_file) {
  if (!("per_capita" %in% names(df))) {
    stop("Figure 3 requires 'per_capita' or fallback from cost_day.")
  }
  
  agg <- df %>%
    dplyr::transmute(
      ciudad     = ciudad,
      fecha      = coerce_to_date(fecha),
      model      = as.character(model),
      per_capita = suppressWarnings(as.numeric(per_capita))
    ) %>%
    dplyr::filter(!is.na(ciudad), !is.na(fecha), !is.na(model), !is.na(per_capita)) %>%
    dplyr::group_by(model, ciudad, fecha) %>%
    dplyr::summarise(per_capita = mean(per_capita, na.rm = TRUE), .groups = "drop") %>%
    dplyr::mutate(model = factor(model, levels = c("CoCA", "CoNA", "CoRD")))
  
  if (nrow(agg) == 0) stop("plot_fig3_month: no rows left after filtering.")
  
  p <- ggplot(agg, aes(x = fecha, y = per_capita, color = ciudad, group = ciudad)) +
    geom_line(linewidth = 1) +
    facet_wrap(vars(model), nrow = 1, scales = "free_y") +
    scale_x_date(
      date_breaks = "2 years",
      date_labels = "%Y",
      expand = expansion(mult = c(0.01, 0.01))
    ) +
    labs(
      title = "CoCA, CoNA and CoRD evolution (representative household)",
      subtitle = "Per-capita cost per day",
      x = NULL,
      y = "Per-capita daily cost (COP)"
    ) +
    color_scale_city +
    theme_paper +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
  
  ggsave(
    filename = file.path(fig_dir, out_file),
    plot = p,
    width = 14, height = 4.8,
    dpi = 300, bg = "white"
  )
  
  p
}

plot_fig3_quarter <- function(df, out_file) {
  if (!("per_capita" %in% names(df))) {
    stop("Figure 3 requires 'per_capita' or fallback from cost_day.")
  }
  
  df2 <- df %>% make_quarter_axis()
  brks <- quarter_breaks_q1_2y(levels(df2$trimestre))
  
  agg <- df2 %>%
    dplyr::transmute(
      ciudad     = ciudad,
      trimestre  = as.character(trimestre),
      model      = as.character(model),
      per_capita = suppressWarnings(as.numeric(per_capita))
    ) %>%
    dplyr::filter(!is.na(ciudad), !is.na(trimestre), !is.na(model), !is.na(per_capita)) %>%
    dplyr::group_by(model, ciudad, trimestre) %>%
    dplyr::summarise(per_capita = mean(per_capita, na.rm = TRUE), .groups = "drop") %>%
    dplyr::mutate(
      model = factor(model, levels = c("CoCA", "CoNA", "CoRD")),
      trimestre = factor(
        trimestre,
        levels = df2 %>%
          dplyr::mutate(trimestre = as.character(trimestre)) %>%
          dplyr::distinct(trimestre) %>%
          dplyr::filter(!is.na(trimestre)) %>%
          dplyr::mutate(
            year = as.integer(stringr::str_extract(trimestre, "^\\d{4}")),
            q    = as.integer(stringr::str_extract(trimestre, "(?<=Q)\\d+"))
          ) %>%
          dplyr::arrange(year, q) %>%
          dplyr::pull(trimestre)
      )
    )
  
  if (nrow(agg) == 0) stop("plot_fig3_quarter: no rows left after filtering.")
  
  p <- ggplot(agg, aes(x = trimestre, y = per_capita, color = ciudad, group = ciudad)) +
    geom_line(linewidth = 1) +
    facet_wrap(vars(model), nrow = 1, scales = "free_y") +
    scale_x_discrete(breaks = brks, labels = brks) +
    labs(
      title = "CoCA, CoNA and CoRD evolution (representative household)",
      subtitle = "Per-capita cost per day",
      x = NULL,
      y = "Per-capita daily cost (COP)"
    ) +
    color_scale_city +
    theme_paper +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
  
  ggsave(
    filename = file.path(fig_dir, out_file),
    plot = p,
    width = 14, height = 4.8,
    dpi = 300, bg = "white"
  )
  
  p
}

#======================================================================
# RUN: MONTHLY (REAL)
#======================================================================
df_m <- bind_all_models("month") %>%
  dplyr::mutate(fecha = floor_date(coerce_to_date(fecha), "month"))

message("Rows in df_m: ", nrow(df_m))
print(df_m %>% dplyr::count(model, ciudad))
print(range(df_m$fecha, na.rm = TRUE))

plot_fig2_month(df_m, "Fig2_demogroups_month_real.png")
plot_fig3_month(df_m, "Fig3_household_month_real_percap_day.png")

#======================================================================
# RUN: QUARTERLY (REAL)
#======================================================================
df_q <- bind_all_models("quarter")

message("Rows in df_q: ", nrow(df_q))
print(df_q %>% dplyr::count(model, ciudad))
print(range(df_q$fecha, na.rm = TRUE))

plot_fig2_quarter(df_q, "Fig2_demogroups_quarter_real.png")
plot_fig3_quarter(df_q, "Fig3_household_quarter_real_percap_day.png")

#######################################################################
## Cost_1000kcal (REAL) - Monthly + Quarterly
## CoCA / CoNA / CoRD - representative household
#######################################################################

std_cols_kcal <- function(df) {
  harmonize_names(df)
}

sex_hm_from01 <- function(x) {
  dplyr::case_when(
    is.na(x) ~ NA_character_,
    x %in% c(0, "0") ~ "H",
    x %in% c(1, "1") ~ "M",
    TRUE ~ as.character(x)
  )
}

is_rep_household_kcal <- function(demo_group, sex) {
  sex_hm <- sex_hm_from01(sex)
  (demo_group == "31 a 50 años" & sex_hm == "H") |
    (demo_group == "31 a 50 años" & sex_hm == "M") |
    (demo_group == "9 a 13 años"  & sex_hm == "M")
}

make_quarter_axis_from_date <- function(df) {
  df <- df %>%
    dplyr::mutate(
      fecha = coerce_to_date(fecha),
      year = lubridate::year(fecha),
      q    = lubridate::quarter(fecha),
      trimestre = paste0(year, "Q", q)
    )
  
  tri_levels <- df %>%
    dplyr::distinct(trimestre) %>%
    dplyr::filter(!is.na(trimestre)) %>%
    dplyr::mutate(
      year = as.integer(stringr::str_extract(trimestre, "^\\d{4}")),
      q    = as.integer(stringr::str_extract(trimestre, "(?<=Q)\\d+"))
    ) %>%
    dplyr::arrange(year, q) %>%
    dplyr::pull(trimestre)
  
  df %>% dplyr::mutate(trimestre = factor(trimestre, levels = tri_levels))
}

load_kcal_model <- function(path, model) {
  if (!file.exists(path)) stop("No encuentro: ", path)
  
  df <- readxl::read_excel(path, sheet = 1) %>%
    as.data.frame() %>%
    std_cols_kcal()
  
  need <- c("demo_group", "sex", "cost_1000kcal", "ciudad", "fecha")
  miss <- setdiff(need, names(df))
  if (length(miss) > 0) {
    stop("Faltan columnas en ", basename(path), ": ", paste(miss, collapse = ", "))
  }
  
  out <- df %>%
    dplyr::mutate(
      model  = model,
      fecha  = coerce_to_date(fecha),
      ciudad = factor(std_city(ciudad), levels = city_levels),
      sex    = as.integer(sex),
      demo_group = as.character(demo_group),
      cost_1000kcal = as.numeric(cost_1000kcal),
      sex_hm = sex_hm_from01(sex),
      person = assign_person(demo_group, sex),
      demo_label = dplyr::case_when(
        !is.na(person) ~ paste0(demo_group, " ", sex_hm, " (P", person, ")"),
        TRUE ~ paste0(demo_group, " ", sex_hm)
      ),
      keep_rep = is_rep_household_kcal(demo_group, sex)
    ) %>%
    dplyr::filter(
      keep_rep,
      !is.na(fecha),
      fecha >= as.Date("1999-01-01")
    ) %>%
    dplyr::select(model, ciudad, fecha, demo_group, sex, person, demo_label, cost_1000kcal)
  
  message("KCAL ", model, ": n = ", nrow(out))
  out
}

df_kcal_m <- dplyr::bind_rows(
  load_kcal_model(paths_kcal$CoCA, "CoCA"),
  load_kcal_model(paths_kcal$CoNA, "CoNA"),
  load_kcal_model(paths_kcal$CoRD, "CoRD")
) %>%
  dplyr::mutate(
    model = factor(model, levels = c("CoCA", "CoNA", "CoRD")),
    fecha = floor_date(fecha, "month")
  ) %>%
  dplyr::filter(fecha >= as.Date("1999-01-01"))

df_kcal_q <- df_kcal_m %>%
  dplyr::mutate(fecha = floor_date(fecha, "quarter")) %>%
  dplyr::filter(fecha >= as.Date("1999-01-01")) %>%
  dplyr::group_by(model, ciudad, fecha) %>%
  dplyr::summarise(cost_1000kcal = mean(cost_1000kcal, na.rm = TRUE), .groups = "drop") %>%
  dplyr::mutate(
    model = factor(model, levels = c("CoCA", "CoNA", "CoRD")),
    ciudad = factor(ciudad, levels = city_levels)
  ) %>%
  make_quarter_axis_from_date() %>%
  dplyr::filter(fecha >= as.Date("1999-01-01"))

x_scale_month_2y <- scale_x_date(
  date_breaks = "2 years",
  date_labels = "%Y",
  expand = expansion(mult = c(0.01, 0.01))
)

plot_kcal_month <- function(df, out_file) {
  agg <- df %>%
    dplyr::transmute(
      model = as.character(model),
      ciudad = ciudad,
      fecha = coerce_to_date(fecha),
      cost_1000kcal = as.numeric(cost_1000kcal)
    ) %>%
    dplyr::filter(
      !is.na(model),
      !is.na(ciudad),
      !is.na(fecha),
      !is.na(cost_1000kcal),
      fecha >= as.Date("1999-01-01")
    ) %>%
    dplyr::group_by(model, ciudad, fecha) %>%
    dplyr::summarise(cost_1000kcal = mean(cost_1000kcal, na.rm = TRUE), .groups = "drop") %>%
    dplyr::mutate(model = factor(model, levels = c("CoCA", "CoNA", "CoRD")))
  
  if (nrow(agg) == 0) stop("plot_kcal_month: no rows left after filtering.")
  
  p <- ggplot(agg, aes(x = fecha, y = cost_1000kcal, color = ciudad, group = ciudad)) +
    geom_line(linewidth = 1) +
    facet_wrap(vars(model), nrow = 1, scales = "free_y") +
    x_scale_month_2y +
    labs(
      title = "CoCA, CoNA and CoRD evolution by city",
      x = NULL,
      y = "Cost per 1,000 kcal (COP)"
    ) +
    color_scale_city +
    theme_paper +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  ggsave(file.path(fig_dir, out_file), p, width = 14, height = 4.8, dpi = 300, bg = "white")
  p
}

plot_kcal_quarter <- function(df, out_file) {
  brks <- quarter_breaks_q1_2y(levels(df$trimestre))
  
  agg <- df %>%
    dplyr::transmute(
      model = as.character(model),
      ciudad = ciudad,
      fecha = coerce_to_date(fecha),
      trimestre = as.character(trimestre),
      cost_1000kcal = as.numeric(cost_1000kcal)
    ) %>%
    dplyr::filter(
      !is.na(model),
      !is.na(ciudad),
      !is.na(fecha),
      !is.na(trimestre),
      !is.na(cost_1000kcal),
      fecha >= as.Date("1999-01-01")
    ) %>%
    dplyr::group_by(model, ciudad, trimestre) %>%
    dplyr::summarise(cost_1000kcal = mean(cost_1000kcal, na.rm = TRUE), .groups = "drop") %>%
    dplyr::mutate(
      model = factor(model, levels = c("CoCA", "CoNA", "CoRD")),
      trimestre = factor(
        trimestre,
        levels = df %>%
          dplyr::mutate(trimestre = as.character(trimestre)) %>%
          dplyr::distinct(trimestre) %>%
          dplyr::filter(!is.na(trimestre)) %>%
          dplyr::mutate(
            year = as.integer(stringr::str_extract(trimestre, "^\\d{4}")),
            q    = as.integer(stringr::str_extract(trimestre, "(?<=Q)\\d+"))
          ) %>%
          dplyr::arrange(year, q) %>%
          dplyr::pull(trimestre)
      )
    )
  
  if (nrow(agg) == 0) stop("plot_kcal_quarter: no rows left after filtering.")
  
  p <- ggplot(agg, aes(x = trimestre, y = cost_1000kcal, color = ciudad, group = ciudad)) +
    geom_line(linewidth = 1) +
    facet_wrap(vars(model), nrow = 1, scales = "free_y") +
    scale_x_discrete(breaks = brks) +
    labs(
      title = "CoCA, CoNA and CoRD evolution by city",
      x = NULL,
      y = "Cost per 1,000 kcal (COP)"
    ) +
    color_scale_city +
    theme_paper +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  ggsave(file.path(fig_dir, out_file), p, width = 14, height = 4.8, dpi = 300, bg = "white")
  p
}

message("Rows in df_kcal_m: ", nrow(df_kcal_m))
print(df_kcal_m %>% dplyr::count(model, ciudad))
print(range(df_kcal_m$fecha, na.rm = TRUE))

message("Rows in df_kcal_q: ", nrow(df_kcal_q))
print(df_kcal_q %>% dplyr::count(model, ciudad))
print(range(df_kcal_q$fecha, na.rm = TRUE))

plot_kcal_month(df_kcal_m, "Kcal1000_city_month_real.png")
plot_kcal_quarter(df_kcal_q, "Kcal1000_city_quarter_real.png")

#######################################################################
## DONE
#######################################################################