#######################################################################
## FIGURA 2 y FIGURA 3 (NOMINAL) - Monthly + Quarterly
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

#----------------------------------------------------------------------
# Directories
#----------------------------------------------------------------------
base_dir <- "C:\\Users\\danie\\OneDrive\\Escritorio\\Least-cost-diets-and-affordability\\Proyecto Interno\\"
setwd(base_dir)

out_dir <- file.path(base_dir, "working-papers/working-paper-ipc/output")
afford_cost_dir <- file.path(out_dir, "affordability")
fig_dir <- file.path(out_dir, "figures_paper")
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

#----------------------------------------------------------------------
# Input paths (NOMINALES)
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

#----------------------------------------------------------------------
# City standardization + Paper style
#----------------------------------------------------------------------
city_levels <- c("BOGOTA", "CALI", "MEDELLIN")

std_city <- function(x) {
  x <- as.character(x)
  dplyr::case_when(
    x %in% c("BOGOTÁ D.C.", "BOGOTA D.C.", "BOGOTA") ~ "BOGOTA",
    x %in% c("MEDELLÍN", "MEDELLIN")                 ~ "MEDELLIN",
    x %in% c("CALI")                                 ~ "CALI",
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

# City palette (Figura 3)
color_scale_city <- scale_color_nejm(name = "City")

# Demographic palette (Figura 2)
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

safe_date_month <- function(df) {
  if ("fecha" %in% names(df)) {
    df %>% mutate(fecha = as.Date(fecha))
  } else if (all(c("year","mes") %in% names(df))) {
    df %>% mutate(fecha = as.Date(sprintf("%d-%02d-01", as.integer(year), as.integer(mes))))
  } else {
    stop("Missing date info: need 'fecha' or (year, mes) for monthly.")
  }
}

safe_date_quarter <- function(df) {
  if ("trimestre" %in% names(df)) {
    df <- df %>% mutate(trimestre = as.character(trimestre))
    df %>%
      mutate(
        year = as.integer(str_extract(trimestre, "^\\d{4}")),
        q    = as.integer(str_extract(trimestre, "(?<=Q)\\d+")),
        fecha = as.Date(sprintf("%d-%02d-01", year, (q - 1) * 3 + 1))
      )
  } else if (all(c("year","q") %in% names(df))) {
    df %>%
      mutate(
        year  = as.integer(year),
        q     = as.integer(q),
        trimestre = paste0(year, "Q", q),
        fecha = as.Date(sprintf("%d-%02d-01", year, (q - 1) * 3 + 1))
      )
  } else {
    stop("Missing quarter info: need 'trimestre' or (year, q).")
  }
}

load_model <- function(path, model, freq = c("month","quarter")) {
  freq <- match.arg(freq)
  if (!file.exists(path)) stop("File not found: ", path)
  
  df <- readRDS(path) %>% janitor::clean_names()
  
  if (!("ciudad" %in% names(df))) stop("Missing column 'ciudad' (after clean_names).")
  
  df <- df %>%
    mutate(
      ciudad = factor(std_city(ciudad), levels = city_levels),
      model  = model
    )
  
  df <- if (freq == "month") safe_date_month(df) else safe_date_quarter(df)
  
  # Ensure demographic columns exist
  if (!("demo_group" %in% names(df))) df$demo_group <- NA_character_
  if (!("sex" %in% names(df))) df$sex <- NA
  if (!("person" %in% names(df))) df$person <- NA
  
  df %>%
    mutate(
      sex_hm = sex_lab(sex),
      demo_label = dplyr::case_when(
        !is.na(demo_group) & !is.na(sex_hm) & !is.na(person) ~ paste0(demo_group, " ", sex_hm, " (P", person, ")"),
        !is.na(demo_group) & !is.na(sex_hm) ~ paste0(demo_group, " ", sex_hm),
        !is.na(demo_group) ~ as.character(demo_group),
        TRUE ~ NA_character_
      )
    )
}

bind_all_models <- function(freq = c("month","quarter")) {
  freq <- match.arg(freq)
  bind_rows(
    load_model(paths[[freq]]$CoCA, "CoCA", freq),
    load_model(paths[[freq]]$CoNA, "CoNA", freq),
    load_model(paths[[freq]]$CoRD, "CoRD", freq)
  ) %>%
    mutate(model = factor(model, levels = c("CoCA","CoNA","CoRD")))
}

# For Figure 2, we MUST use cost_day (your request)
pick_yvar_fig2 <- function(df) {
  if ("cost_day" %in% names(df)) return("cost_day")
  stop("Figure 2 requires 'cost_day' in the input bases.")
}

# Quarter axis (ordered factor)
make_quarter_axis <- function(df) {
  df <- df %>% mutate(trimestre = as.character(trimestre))
  tri_levels <- df %>%
    distinct(trimestre) %>%
    mutate(
      year = as.integer(str_extract(trimestre, "^\\d{4}")),
      q    = as.integer(str_extract(trimestre, "(?<=Q)\\d+"))
    ) %>%
    arrange(year, q) %>%
    pull(trimestre)
  df %>% mutate(trimestre = factor(trimestre, levels = tri_levels))
}

# Keep only Q1 labels (one tick per year), label as "YYYYQ1"
quarter_breaks_q1_2y <- function(tri_levels) {
  
  tmp <- tibble(trimestre = tri_levels) %>%
    mutate(
      year = as.integer(stringr::str_extract(trimestre, "^\\d{4}")),
      q    = as.integer(stringr::str_extract(trimestre, "(?<=Q)\\d+"))
    ) %>%
    arrange(year, q)
  
  # Solo Q1
  tmp <- tmp %>% filter(q == 1)
  
  # Cada 2 años
  yrs_keep <- tmp$year[seq(1, nrow(tmp), by = 2)]
  
  tmp %>%
    filter(year %in% yrs_keep) %>%
    pull(trimestre)
}

#----------------------------------------------------------------------
# FIGURE 2: evolution by demographic group (representative household)
# Monthly / Quarterly (NOMINAL)
# Facet: rows=model, cols=city. Lines=demo_label.
# Uses cost_day (COP/day/person in your base)
#----------------------------------------------------------------------
plot_fig2_month <- function(df, out_file) {
  yvar <- pick_yvar_fig2(df)
  
  p <- df %>%
    filter(!is.na(ciudad), !is.na(fecha), !is.na(model), !is.na(demo_label)) %>%
    ggplot(aes(x = fecha, y = .data[[yvar]], color = demo_label, group = demo_label)) +
    geom_line(linewidth = 0.9) +
    facet_grid(model ~ ciudad, scales = "free_y") +
    scale_x_date(
      date_breaks = "2 years",       # <- reduce clutter
      date_labels = "%Y",
      expand = expansion(mult = c(0.01, 0.01))
    ) +
    labs(
      title = "CoCA, CoNA and CoRD evolution by demographic group (representative household)",
      subtitle = "Nominal values",
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
  
  p <- df2 %>%
    filter(!is.na(ciudad), !is.na(model), !is.na(demo_label)) %>%
    ggplot(aes(x = trimestre, y = .data[[yvar]], color = demo_label, group = demo_label)) +
    geom_line(linewidth = 0.9) +
    facet_grid(model ~ ciudad, scales = "free_y") +
    scale_x_discrete(
      breaks = brks,
      labels = brks                 # shows "YYYYQ1" explicitly
    ) +
    labs(
      title = "CoCA, CoNA and CoRD evolution by demographic group (representative household)",
      subtitle = "Nominal values",
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
# FIGURE 3: evolution of Co* per-capita per day (representative household)
# Lines=city, facet=model
# Uses per_capita (from your base)
#----------------------------------------------------------------------
plot_fig3_month <- function(df, out_file) {
  if (!("per_capita" %in% names(df))) stop("Figure 3 requires 'per_capita' in these bases.")
  
  agg <- df %>%
    filter(!is.na(ciudad), !is.na(fecha), !is.na(model)) %>%
    group_by(ciudad, fecha, model) %>%
    summarise(per_capita = mean(as.numeric(per_capita), na.rm = TRUE), .groups = "drop")
  
  p <- agg %>%
    ggplot(aes(x = fecha, y = per_capita, color = ciudad, group = ciudad)) +
    geom_line(linewidth = 1) +
    facet_wrap(~model, nrow = 1, scales = "free_y") +
    scale_x_date(
      date_breaks = "2 years",       # <- reduce clutter
      date_labels = "%Y",
      expand = expansion(mult = c(0.01, 0.01))
    ) +
    labs(
      title = "CoCA, CoNA and CoRD evolution (representative household)",
      subtitle = "Per-capita cost per day — Nominal values",
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
  if (!("per_capita" %in% names(df))) stop("Figure 3 requires 'per_capita' in these bases.")
  
  df2 <- df %>% make_quarter_axis()
  brks <- quarter_breaks_q1_2y(levels(df2$trimestre))
  
  agg <- df2 %>%
    filter(!is.na(ciudad), !is.na(trimestre), !is.na(model)) %>%
    group_by(ciudad, trimestre, model) %>%
    summarise(per_capita = mean(as.numeric(per_capita), na.rm = TRUE), .groups = "drop")
  
  p <- agg %>%
    ggplot(aes(x = trimestre, y = per_capita, color = ciudad, group = ciudad)) +
    geom_line(linewidth = 1) +
    facet_wrap(~model, nrow = 1, scales = "free_y") +
    scale_x_discrete(
      breaks = brks,
      labels = brks
    ) +
    labs(
      title = "CoCA, CoNA and CoRD evolution (representative household)",
      subtitle = "Per-capita cost per day — Nominal values",
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
# RUN: MONTHLY (NOMINAL)
#======================================================================
df_m <- bind_all_models("month") %>%
  mutate(fecha = floor_date(fecha, "month"))

plot_fig2_month(df_m, "Fig2_demogroups_month_nominal.png")
plot_fig3_month(df_m, "Fig3_household_month_nominal_percap_day.png")

#======================================================================
# RUN: QUARTERLY (NOMINAL)
#======================================================================
df_q <- bind_all_models("quarter")

plot_fig2_quarter(df_q, "Fig2_demogroups_quarter_nominal.png")
plot_fig3_quarter(df_q, "Fig3_household_quarter_nominal_percap_day.png")


#######################################################################
## Cost_1000kcal (NOMINAL) - Monthly + Quarterly (built from monthly)
## CoCA / CoNA / CoRD - representative household
#######################################################################

library(readxl)

#----------------------------------------------------------------------
# Directories / output
#----------------------------------------------------------------------
least_cost_dir <- file.path(out_dir, "least_cost_metrics")
fig_dir <- file.path(out_dir, "figures_paper")
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

#----------------------------------------------------------------------
# Input paths (NOMINAL - monthly only)
#   Only change diet name (coca -> cona, cord, etc.)
#----------------------------------------------------------------------
paths_kcal <- list(
  CoCA = file.path(least_cost_dir, "coca_fullsample.xlsx"),
  CoNA = file.path(least_cost_dir, "cona_fullsample.xlsx"),
  CoRD = file.path(least_cost_dir, "cord_fullsample.xlsx")
)

#----------------------------------------------------------------------
# Helpers: standardize columns safely
#----------------------------------------------------------------------
std_cols_kcal <- function(df) {
  df <- janitor::clean_names(df)
  
  # harmonize key names if needed
  if (!"demo_group" %in% names(df) && "demo_group" %in% names(df)) {
    # nothing
  }
  # cost_1000kcal could arrive as cost_1000kcal or cost_1000_kcal, etc.
  if (!"cost_1000kcal" %in% names(df)) {
    cand <- intersect(names(df), c("cost_1000_kcal", "cost1000kcal", "cost_1000kcal", "cost_1000kcal_"))
    if (length(cand) >= 1) df <- df %>% rename(cost_1000kcal = all_of(cand[1]))
  }
  
  # date column
  if (!"fecha" %in% names(df)) {
    cand <- intersect(names(df), c("date", "fecha_date", "time", "period"))
    if (length(cand) >= 1) df <- df %>% rename(fecha = all_of(cand[1]))
  }
  
  # city column
  if (!"ciudad" %in% names(df)) {
    cand <- intersect(names(df), c("city", "municipio", "ciudad_nombre"))
    if (length(cand) >= 1) df <- df %>% rename(ciudad = all_of(cand[1]))
  }
  
  # sex column
  if (!"sex" %in% names(df)) {
    cand <- intersect(names(df), c("sexo", "sex"))
    if (length(cand) >= 1) df <- df %>% rename(sex = all_of(cand[1]))
  }
  
  # demo column
  if (!"demo_group" %in% names(df)) {
    cand <- intersect(names(df), c("demo_group", "demogroup", "grupo_demo", "grupo_demografico"))
    if (length(cand) >= 1) df <- df %>% rename(demo_group = all_of(cand[1]))
  }
  
  df
}

# sex label -> H/M (but keep numeric for filtering)
sex_hm_from01 <- function(x) {
  dplyr::case_when(
    is.na(x) ~ NA_character_,
    x %in% c(0, "0") ~ "H",
    x %in% c(1, "1") ~ "M",
    TRUE ~ as.character(x)
  )
}

# Representative household filter (3 persons)
is_rep_household <- function(demo_group, sex) {
  # demo_group expected like "31 a 50 años" and "9 a 13 años"
  (demo_group == "31 a 50 años" & sex %in% c(0, "0")) |
    (demo_group == "31 a 50 años" & sex %in% c(1, "1")) |
    (demo_group == "9 a 13 años"  & sex %in% c(1, "1"))
}

# Quarterly axis breaks: Q1 every 2 years (same density as monthly 2-year breaks)
quarter_breaks_q1_2y <- function(tri_levels) {
  tmp <- tibble(trimestre = tri_levels) %>%
    mutate(
      year = as.integer(stringr::str_extract(trimestre, "^\\d{4}")),
      q    = as.integer(stringr::str_extract(trimestre, "(?<=Q)\\d+"))
    ) %>%
    arrange(year, q) %>%
    filter(q == 1)
  
  yrs_keep <- tmp$year[seq(1, nrow(tmp), by = 2)]
  tmp %>% filter(year %in% yrs_keep) %>% pull(trimestre)
}

make_quarter_axis <- function(df) {
  df <- df %>% mutate(
    year = year(fecha),
    q    = quarter(fecha),
    trimestre = paste0(year, "Q", q)
  )
  
  tri_levels <- df %>%
    distinct(trimestre) %>%
    mutate(
      year = as.integer(str_extract(trimestre, "^\\d{4}")),
      q    = as.integer(str_extract(trimestre, "(?<=Q)\\d+"))
    ) %>%
    arrange(year, q) %>%
    pull(trimestre)
  
  df %>% mutate(trimestre = factor(trimestre, levels = tri_levels))
}

#----------------------------------------------------------------------
# Load one model (monthly)
#----------------------------------------------------------------------
load_kcal_model <- function(path, model) {
  if (!file.exists(path)) stop("No encuentro: ", path)
  
  df <- readxl::read_excel(path) %>%
    as.data.frame() %>%
    std_cols_kcal()
  
  # minimal required columns
  need <- c("demo_group", "sex", "cost_1000kcal", "ciudad", "fecha")
  miss <- setdiff(need, names(df))
  if (length(miss) > 0) stop("Faltan columnas en ", basename(path), ": ", paste(miss, collapse = ", "))
  
  df %>%
    mutate(
      model  = model,
      fecha  = as.Date(fecha),
      ciudad = factor(std_city(ciudad), levels = city_levels),
      sex    = as.integer(sex),
      demo_group = as.character(demo_group),
      cost_1000kcal = as.numeric(cost_1000kcal),
      sex_hm = sex_hm_from01(sex),
      demo_label = paste0(demo_group, " ", sex_hm),
      keep_rep = is_rep_household(demo_group, sex)
    ) %>%
    filter(keep_rep) %>%
    select(model, ciudad, fecha, demo_group, sex, demo_label, cost_1000kcal)
}

df_kcal_m <- bind_rows(
  load_kcal_model(paths_kcal$CoCA, "CoCA"),
  load_kcal_model(paths_kcal$CoNA, "CoNA"),
  load_kcal_model(paths_kcal$CoRD, "CoRD")
) %>%
  mutate(
    model = factor(model, levels = c("CoCA", "CoNA", "CoRD")),
    fecha = floor_date(fecha, "month")
  )

# Quarterly built from monthly
df_kcal_q <- df_kcal_m %>%
  mutate(fecha_q = floor_date(fecha, "quarter")) %>%
  group_by(model, ciudad, demo_label, fecha = fecha_q) %>%
  summarise(cost_1000kcal = mean(cost_1000kcal, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    model = factor(model, levels = c("CoCA", "CoNA", "CoRD")),
    ciudad = factor(ciudad, levels = city_levels)
  ) %>%
  make_quarter_axis()

#----------------------------------------------------------------------
# Paper-style tweaks for x-axis (less clutter + diagonal labels)
#   Monthly: tick every 2 years
#   Quarterly: show Q1 every 2 years
#----------------------------------------------------------------------
x_scale_month_2y <- scale_x_date(
  date_breaks = "2 years",
  date_labels = "%Y",
  expand = expansion(mult = c(0.01, 0.01))
)

#----------------------------------------------------------------------
# FIGURE: Cost per 1000 kcal by demographic group (representative household)
#  - facet rows = model, cols = city
#  - lines = demographic group
#----------------------------------------------------------------------
plot_kcal_by_demo_month <- function(df, out_file) {
  p <- df %>%
    filter(!is.na(cost_1000kcal)) %>%
    ggplot(aes(x = fecha, y = cost_1000kcal, color = demo_label, group = demo_label)) +
    geom_line(linewidth = 0.95) +
    facet_grid(model ~ ciudad, scales = "free_y") +
    x_scale_month_2y +
    labs(
      title = "CoCA, CoNA and CoRD evolution by demographic group (representative household)",
      subtitle = "Cost per 1,000 kcal — Nominal values",
      x = NULL,
      y = "Cost per 1,000 kcal (COP)",
      color = "Demographic group"
    ) +
    color_scale_demo +
    theme_paper +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  ggsave(file.path(fig_dir, out_file), p, width = 14, height = 7, dpi = 300, bg = "white")
  p
}

plot_kcal_by_demo_quarter <- function(df, out_file) {
  brks <- quarter_breaks_q1_2y(levels(df$trimestre))
  
  p <- df %>%
    filter(!is.na(cost_1000kcal)) %>%
    ggplot(aes(x = trimestre, y = cost_1000kcal, color = demo_label, group = demo_label)) +
    geom_line(linewidth = 0.95) +
    facet_grid(model ~ ciudad, scales = "free_y") +
    scale_x_discrete(breaks = brks) +
    labs(
      title = "CoCA, CoNA and CoRD evolution by demographic group (representative household)",
      subtitle = "Cost per 1,000 kcal — Nominal values",
      x = NULL,
      y = "Cost per 1,000 kcal (COP)",
      color = "Demographic group"
    ) +
    color_scale_demo +
    theme_paper +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  ggsave(file.path(fig_dir, out_file), p, width = 14, height = 7, dpi = 300, bg = "white")
  p
}

#----------------------------------------------------------------------
# RUN
#----------------------------------------------------------------------
plot_kcal_by_demo_month(df_kcal_m, "Kcal1000_demogroups_month_nominal.png")
plot_kcal_by_demo_quarter(df_kcal_q, "Kcal1000_demogroups_quarter_nominal.png")


#######################################################################
## DONE
#######################################################################