########################################################
## affordability_plots.R
########################################################

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(readr)
  library(stringi)
  library(scales)
  library(ggsci)
})

# =========================
# 0. DIRECTORIO BASE
# =========================

base_dir <- "C:/Users/danie/OneDrive/Escritorio/Least-cost-diets-and-affordability/Proyecto Interno/working-papers/working-paper-ipc"

data_dir <- file.path(base_dir, "output", "affordability")
fig_dir  <- file.path(base_dir, "output", "affordability", "figures")

dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

# =========================
# 1. HELPERS
# =========================

normalizar_ciudad <- function(x) {
  x <- as.character(x)
  x <- stringi::stri_trans_general(x, "Latin-ASCII")
  x <- toupper(trimws(x))
  x <- gsub("\\.", "", x)
  x <- gsub(",", "", x)
  x <- gsub("\\s+", " ", x)
  
  dplyr::case_when(
    x %in% c("BOGOTA", "BOGOTA DC", "BOGOTA D C", "BOGOTA D.C", "BOGOTA D.C.") ~ "BOGOTA",
    x %in% c("MEDELLIN") ~ "MEDELLIN",
    x %in% c("CALI") ~ "CALI",
    TRUE ~ x
  )
}

parse_fecha_r <- function(x) {
  if (inherits(x, "Date")) return(x)
  as.Date(x, origin = "1970-01-01")
}

fmt_num <- function(x) {
  format(x, big.mark = ",", scientific = FALSE, trim = TRUE)
}

quarter_label <- function(x) {
  paste0(format(x, "%Y"), "-Q", ((as.integer(format(x, "%m")) - 1) %/% 3) + 1)
}

# =========================
# 2. ESCALAS Y TEMA
# =========================

scale_x_month_clean <- scale_x_date(
  limits = c(as.Date("2018-01-01"), as.Date("2025-12-31")),
  date_breaks = "2 years",
  date_labels = "%Y",
  expand = expansion(mult = c(0.01, 0.02))
)

scale_x_quarter_clean <- scale_x_date(
  limits = c(as.Date("2018-01-01"), as.Date("2025-12-31")),
  labels = quarter_label,
  date_breaks = "2 years",
  expand = expansion(mult = c(0.01, 0.02))
)

scale_y_clean <- scale_y_continuous(
  labels = function(x) format(x, big.mark = ",", scientific = FALSE, trim = TRUE),
  expand = expansion(mult = c(0.02, 0.04))
)

color_scale <- scale_color_nejm(name = "City")

theme_paper2 <- theme_classic(base_size = 12) +
  theme(
    plot.title      = element_text(face = "bold", size = 16, hjust = 0),
    axis.title      = element_text(size = 12),
    axis.text       = element_text(size = 10, color = "black"),
    axis.text.x     = element_text(angle = 45, hjust = 1),
    legend.title    = element_text(size = 11),
    legend.text     = element_text(size = 10),
    legend.position = "top",
    legend.justification = "left",
    axis.line       = element_line(linewidth = 0.5),
    axis.ticks      = element_line(linewidth = 0.4),
    plot.margin     = margin(8, 10, 8, 8)
  )

# =========================
# 3. CARGA DE BASES
# =========================

coca_month <- readRDS(file.path(data_dir, "CoCA_city_month.rds"))
coca_q     <- readRDS(file.path(data_dir, "CoCA_city_cuartiles.rds"))

cona_month <- readRDS(file.path(data_dir, "CoNA_city_month.rds"))
cona_q     <- readRDS(file.path(data_dir, "CoNA_city_cuartiles.rds"))

cord_month <- readRDS(file.path(data_dir, "CoRD_city_month.rds"))
cord_q     <- readRDS(file.path(data_dir, "CoRD_city_cuartiles.rds"))

# =========================
# 4. LIMPIEZA
# =========================

limpiar_afford <- function(df) {
  df %>%
    mutate(
      fecha  = parse_fecha_r(fecha),
      ciudad = normalizar_ciudad(ciudad)
    ) %>%
    filter(fecha >= as.Date("2018-01-01"))
}

coca_month <- limpiar_afford(coca_month)
coca_q     <- limpiar_afford(coca_q)

cona_month <- limpiar_afford(cona_month)
cona_q     <- limpiar_afford(cona_q)

cord_month <- limpiar_afford(cord_month)
cord_q     <- limpiar_afford(cord_q)

# =========================
# 5. RESÚMENES PARA GRÁFICAS
# =========================
# Se usa per_capita_month como variable principal.
# Si hay varias filas por ciudad-fecha, se promedia.

make_sum <- function(df, value_var = "per_capita_month") {
  df %>%
    dplyr::group_by(ciudad, fecha) %>%
    dplyr::summarise(
      value = mean(.data[[value_var]], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter(is.finite(value))
}

coca_sum   <- make_sum(coca_month, "per_capita_month")
cona_sum   <- make_sum(cona_month, "per_capita_month")
cord_sum   <- make_sum(cord_month, "per_capita_month")

coca_q_sum <- make_sum(coca_q, "per_capita_month")
cona_q_sum <- make_sum(cona_q, "per_capita_month")
cord_q_sum <- make_sum(cord_q, "per_capita_month")

# =========================
# 6. GRÁFICAS DE SERIES
# =========================

g_coca <- ggplot(coca_sum,
                 aes(x = fecha, y = value, color = ciudad, group = ciudad)) +
  geom_line(linewidth = 1) +
  scale_x_month_clean +
  scale_y_clean +
  color_scale +
  labs(
    title = "CoCA: Monthly Per Capita Cost",
    x = NULL,
    y = "Per capita monthly cost"
  ) +
  theme_paper2

ggsave(
  filename = file.path(fig_dir, "CoCA_per_capita_month_city_time.png"),
  plot     = g_coca,
  width    = 11, height = 5.8, dpi = 300, bg = "white"
)

g_cona <- ggplot(cona_sum,
                 aes(x = fecha, y = value, color = ciudad, group = ciudad)) +
  geom_line(linewidth = 1) +
  scale_x_month_clean +
  scale_y_clean +
  color_scale +
  labs(
    title = "CoNA: Monthly Per Capita Cost",
    x = NULL,
    y = "Per capita monthly cost"
  ) +
  theme_paper2

ggsave(
  filename = file.path(fig_dir, "CoNA_per_capita_month_city_time.png"),
  plot     = g_cona,
  width    = 11, height = 5.8, dpi = 300, bg = "white"
)

if (!is.null(cord_sum) && nrow(cord_sum) > 0) {
  g_cord <- ggplot(cord_sum,
                   aes(x = fecha, y = value, color = ciudad, group = ciudad)) +
    geom_line(linewidth = 1) +
    scale_x_month_clean +
    scale_y_clean +
    color_scale +
    labs(
      title = "CoRD: Monthly Per Capita Cost",
      x = NULL,
      y = "Per capita monthly cost"
    ) +
    theme_paper2
  
  ggsave(
    filename = file.path(fig_dir, "CoRD_per_capita_month_city_time.png"),
    plot     = g_cord,
    width    = 11, height = 5.8, dpi = 300, bg = "white"
  )
}

g_coca_q <- ggplot(coca_q_sum,
                   aes(x = fecha, y = value, color = ciudad, group = ciudad)) +
  geom_line(linewidth = 1) +
  scale_x_quarter_clean +
  scale_y_clean +
  color_scale +
  labs(
    title = "CoCA: Quarterly Per Capita Cost",
    x = "Quarter",
    y = "Per capita quarterly cost"
  ) +
  theme_paper2

ggsave(
  filename = file.path(fig_dir, "CoCA_per_capita_quarter_city_time.png"),
  plot     = g_coca_q,
  width    = 11, height = 5.8, dpi = 300, bg = "white"
)

g_cona_q <- ggplot(cona_q_sum,
                   aes(x = fecha, y = value, color = ciudad, group = ciudad)) +
  geom_line(linewidth = 1) +
  scale_x_quarter_clean +
  scale_y_clean +
  color_scale +
  labs(
    title = "CoNA: Quarterly Per Capita Cost",
    x = "Quarter",
    y = "Per capita quarterly cost"
  ) +
  theme_paper2

ggsave(
  filename = file.path(fig_dir, "CoNA_per_capita_quarter_city_time.png"),
  plot     = g_cona_q,
  width    = 11, height = 5.8, dpi = 300, bg = "white"
)

if (!is.null(cord_q_sum) && nrow(cord_q_sum) > 0) {
  g_cord_q <- ggplot(cord_q_sum,
                     aes(x = fecha, y = value, color = ciudad, group = ciudad)) +
    geom_line(linewidth = 1) +
    scale_x_quarter_clean +
    scale_y_clean +
    color_scale +
    labs(
      title = "CoRD: Quarterly Per Capita Cost",
      x = "Quarter",
      y = "Per capita quarterly cost"
    ) +
    theme_paper2
  
  ggsave(
    filename = file.path(fig_dir, "CoRD_per_capita_quarter_city_time.png"),
    plot     = g_cord_q,
    width    = 11, height = 5.8, dpi = 300, bg = "white"
  )
}

# =========================
# 7. SCATTERS
# =========================

coca_sc <- coca_sum %>% dplyr::rename(CoCA = value)
cona_sc <- cona_sum %>% dplyr::rename(CoNA = value)

scat_coca_cona <- coca_sc %>%
  inner_join(cona_sc, by = c("ciudad", "fecha"))

g_scatter_coca_cona <- ggplot(scat_coca_cona,
                              aes(x = CoCA, y = CoNA, color = ciudad)) +
  geom_point(alpha = 0.75, size = 2) +
  color_scale +
  scale_x_continuous(labels = function(x) format(x, big.mark = ",", scientific = FALSE, trim = TRUE)) +
  scale_y_continuous(labels = function(x) format(x, big.mark = ",", scientific = FALSE, trim = TRUE)) +
  labs(
    title = "CoCA vs CoNA",
    x = "CoCA monthly per capita cost",
    y = "CoNA monthly per capita cost"
  ) +
  theme_paper2

ggsave(
  filename = file.path(fig_dir, "CoCA_vs_CoNA_scatter.png"),
  plot     = g_scatter_coca_cona,
  width    = 7.5, height = 5.5, dpi = 300, bg = "white"
)

if (!is.null(cord_sum) && nrow(cord_sum) > 0) {
  cord_sc <- cord_sum %>% dplyr::rename(CoRD = value)
  
  scat_coca_cord <- coca_sc %>%
    inner_join(cord_sc, by = c("ciudad", "fecha"))
  
  g_scatter_coca_cord <- ggplot(scat_coca_cord,
                                aes(x = CoCA, y = CoRD, color = ciudad)) +
    geom_point(alpha = 0.75, size = 2) +
    color_scale +
    scale_x_continuous(labels = function(x) format(x, big.mark = ",", scientific = FALSE, trim = TRUE)) +
    scale_y_continuous(labels = function(x) format(x, big.mark = ",", scientific = FALSE, trim = TRUE)) +
    labs(
      title = "CoCA vs CoRD",
      x = "CoCA monthly per capita cost",
      y = "CoRD monthly per capita cost"
    ) +
    theme_paper2
  
  ggsave(
    filename = file.path(fig_dir, "CoCA_vs_CoRD_scatter.png"),
    plot     = g_scatter_coca_cord,
    width    = 7.5, height = 5.5, dpi = 300, bg = "white"
  )
  
  scat_cona_cord <- cona_sc %>%
    inner_join(cord_sc, by = c("ciudad", "fecha"))
  
  g_scatter_cona_cord <- ggplot(scat_cona_cord,
                                aes(x = CoNA, y = CoRD, color = ciudad)) +
    geom_point(alpha = 0.75, size = 2) +
    color_scale +
    scale_x_continuous(labels = function(x) format(x, big.mark = ",", scientific = FALSE, trim = TRUE)) +
    scale_y_continuous(labels = function(x) format(x, big.mark = ",", scientific = FALSE, trim = TRUE)) +
    labs(
      title = "CoNA vs CoRD",
      x = "CoNA monthly per capita cost",
      y = "CoRD monthly per capita cost"
    ) +
    theme_paper2
  
  ggsave(
    filename = file.path(fig_dir, "CoNA_vs_CoRD_scatter.png"),
    plot     = g_scatter_cona_cord,
    width    = 7.5, height = 5.5, dpi = 300, bg = "white"
  )
}

cat("Gráficas guardadas en:\n", fig_dir, "\n")