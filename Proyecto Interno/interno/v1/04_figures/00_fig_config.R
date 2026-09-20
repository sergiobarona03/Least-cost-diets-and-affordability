########################################################
## v1/04_figures/00_fig_config.R
## Version v1: mismas rutas y helpers que 04_figures/00_fig_config.R,
## apuntando a v1/output y v1/03_models en vez de los del pipeline
## en produccion. Colores, temas y funciones de recodificacion son
## identicos (no dependen del metodo de precio).
########################################################

# -----------------------------------------------------------------------
# 0. Rutas base
# -----------------------------------------------------------------------
base_dir <- "C:/Users/danie/OneDrive/Escritorio/Least-cost-diets-and-affordability/Proyecto Interno/interno/"

output_dir <- file.path(base_dir, "v1/output")
coca_dir   <- file.path(base_dir, "v1/03_models/coca")
cona_dir   <- file.path(base_dir, "v1/03_models/cona")
fig_dir    <- file.path(output_dir, "figuras")

dir.create(file.path(fig_dir, "01_costos"),     recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(fig_dir, "02_composicion"), recursive = TRUE, showWarnings = FALSE)

# -----------------------------------------------------------------------
# 1. Ciudades: colores y etiquetas
##   Orden aproximado por tamano de poblacion (area metropolitana)
# -----------------------------------------------------------------------
CITY_ORDER <- c("BOGOTA", "MEDELLIN", "CALI", "BARRANQUILLA", "CARTAGENA",
                "CUCUTA", "BUCARAMANGA", "IBAGUE", "PEREIRA",
                "VILLAVICENCIO", "MONTERIA", "MANIZALES", "PASTO")

CITY_LABS <- c(
  "BOGOTA"        = "Bogot\u00e1",
  "MEDELLIN"      = "Medell\u00edn",
  "CALI"          = "Cali",
  "BARRANQUILLA"  = "Barranquilla",
  "CARTAGENA"     = "Cartagena",
  "CUCUTA"        = "C\u00facuta",
  "BUCARAMANGA"   = "Bucaramanga",
  "IBAGUE"        = "Ibagu\u00e9",
  "PEREIRA"       = "Pereira",
  "VILLAVICENCIO" = "Villavicencio",
  "MONTERIA"      = "Monter\u00eda",
  "MANIZALES"     = "Manizales",
  "PASTO"         = "Pasto"
)

CITY_COLS <- c(
  "BOGOTA"        = "#1B4F72",
  "MEDELLIN"      = "#C0392B",
  "CALI"          = "#1A7A4A",
  "BARRANQUILLA"  = "#D68910",
  "CARTAGENA"     = "#7D3C98",
  "CUCUTA"        = "#148F77",
  "BUCARAMANGA"   = "#B9770E",
  "IBAGUE"        = "#2E86C1",
  "PEREIRA"       = "#CB4335",
  "VILLAVICENCIO" = "#229954",
  "MONTERIA"      = "#A569BD",
  "MANIZALES"     = "#839192",
  "PASTO"         = "#D4AC0D"
)

city_scale_fill <- function(...) {
  scale_fill_manual(values = setNames(unname(CITY_COLS[CITY_ORDER]),
                                      CITY_LABS[CITY_ORDER]), ...)
}

city_scale_color <- function(...) {
  scale_color_manual(values = setNames(unname(CITY_COLS[CITY_ORDER]),
                                       CITY_LABS[CITY_ORDER]), ...)
}

# -----------------------------------------------------------------------
# 2. Metricas: colores y etiquetas
# -----------------------------------------------------------------------
MODEL_COLS <- c(
  "CoCA" = "#95A5A6",
  "CoNA" = "#2C3E6B"
)

# -----------------------------------------------------------------------
# 3. Miembros del hogar representativo
##   Hombre adulto 31-50, mujer adulta 31-50, nina 9-13
##   OJO: la clave "[10, 14)" (con espacio) viene tal cual del output
##   del paquete Foodprice; se conserva sin modificar para el cruce.
# -----------------------------------------------------------------------
MEMBER_LABS <- c(
  "0_[31,51)"  = "Hombre adulto (31\u201350 a\u00f1os)",
  "1_[31,51)"  = "Mujer adulta (31\u201350 a\u00f1os)",
  "1_[10, 14)" = "Ni\u00f1a (9\u201313 a\u00f1os)"
)

MEMBER_ORDER <- c("Hombre adulto (31\u201350 a\u00f1os)",
                  "Mujer adulta (31\u201350 a\u00f1os)",
                  "Ni\u00f1a (9\u201313 a\u00f1os)")

MEMBER_COLS <- c(
  "Hombre adulto (31\u201350 a\u00f1os)" = "#2C3E6B",
  "Mujer adulta (31\u201350 a\u00f1os)"  = "#C0392B",
  "Ni\u00f1a (9\u201313 a\u00f1os)"      = "#1A7A4A"
)

recode_member <- function(df, sex_col = "Sex", age_col = "Demo_Group",
                          out_col = "member") {
  df %>%
    dplyr::mutate(
      !!out_col := dplyr::recode(paste0(.data[[sex_col]], "_", .data[[age_col]]),
                                 !!!MEMBER_LABS),
      !!out_col := factor(.data[[out_col]], levels = MEMBER_ORDER))
}

recode_city <- function(df, col = "ciudad", out_col = "ciudad_lbl") {
  df %>%
    dplyr::mutate(
      !!out_col := factor(CITY_LABS[.data[[col]]],
                          levels = CITY_LABS[CITY_ORDER]))
}

# -----------------------------------------------------------------------
# 4. Meses del trimestre (jul-ago-sep 2025)
# -----------------------------------------------------------------------
MES_LABS <- c("7" = "Julio 2025", "8" = "Agosto 2025", "9" = "Septiembre 2025")

recode_mes <- function(df, col = "mes", out_col = "mes_lbl") {
  df %>%
    dplyr::mutate(
      !!out_col := factor(MES_LABS[as.character(.data[[col]])],
                          levels = MES_LABS))
}

# -----------------------------------------------------------------------
# 5. Tema grafico compartido
# -----------------------------------------------------------------------
paper_theme <- function(base_size = 11) {
  theme_bw(base_size = base_size) +
    theme(
      text             = element_text(family = "serif"),
      plot.title       = element_text(face = "bold", size = base_size + 1,
                                      margin = margin(b = 4)),
      plot.subtitle    = element_text(size = base_size - 1,
                                      color = "grey40",
                                      margin = margin(b = 6)),
      plot.caption     = element_text(size = base_size - 3,
                                      color = "grey50", hjust = 0,
                                      margin = margin(t = 6)),
      axis.title       = element_text(size = base_size - 1),
      axis.text        = element_text(size = base_size - 2),
      axis.text.x      = element_text(angle = 45, hjust = 1),
      legend.position  = "bottom",
      legend.title     = element_blank(),
      legend.text      = element_text(size = base_size - 2),
      legend.key.width = unit(1.2, "cm"),
      panel.grid.major = element_line(color = "grey92", linewidth = 0.3),
      panel.grid.minor = element_blank(),
      strip.background = element_rect(fill = "grey96", color = "grey70"),
      strip.text       = element_text(face = "bold", size = base_size - 1),
      plot.margin      = margin(6, 8, 6, 6)
    )
}

cop_format <- function(prefix = "$", suffix = "") {
  scales::comma_format(prefix = prefix, suffix = suffix, big.mark = ".",
                       decimal.mark = ",")
}

message("v1/04_figures/00_fig_config.R cargado. fig_dir: ", fig_dir)
