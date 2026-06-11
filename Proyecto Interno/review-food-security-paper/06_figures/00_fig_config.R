########################################################
## 05_figures/00_fig_config.R
## Shared aesthetics, labels, and helpers for all figures
## Source this at the top of every figure script:
##   source(file.path(REVIEW_DIR, "05_figures", "00_fig_config.R"))
########################################################

# -----------------------------------------------------------------------
# City colours and labels
# -----------------------------------------------------------------------
CITY_COLS <- c(
  "BOGOTA"   = "#2C3E6B",
  "MEDELLIN" = "#C0392B",
  "CALI"     = "#1A7A4A"
)

CITY_LABS <- c(
  "BOGOTA"   = "Bogotá",
  "MEDELLIN" = "Medellín",
  "CALI"     = "Cali"
)

# -----------------------------------------------------------------------
# Model colours and labels
# -----------------------------------------------------------------------
MODEL_COLS <- c(
  "CoCA"   = "#95A5A6",
  "CoNA"   = "#2C3E6B",
  "CoRD"   = "#C0392B",
  "CC-CoNA"= "#1A7A4A"
)

MODEL_LABS <- c(
  "CoCA"    = "CoCA",
  "CoNA"    = "CoNA",
  "CoRD"    = "CoRD",
  "CC-CoNA" = "CC-CoNA"
)

MODEL_LTYPE <- c(
  "CoCA"    = "dotted",
  "CoNA"    = "solid",
  "CoRD"    = "dashed",
  "CC-CoNA" = "longdash"
)

# -----------------------------------------------------------------------
# Member labels
# -----------------------------------------------------------------------
MEMBER_LABS <- c(
  "0_[31,51)"  = "Adult male",
  "1_[31,51)"  = "Adult female",
  "1_[10, 14)" = "Female child"
)

MEMBER_ORDER <- c("Adult male", "Adult female", "Female child")

MEMBER_COLS <- c(
  "Adult male"   = "#2C3E6B",
  "Adult female" = "#C0392B",
  "Female child" = "#1A7A4A"
)

# -----------------------------------------------------------------------
# Alpha palette for CC-CoNA
# -----------------------------------------------------------------------
ALPHA_COLS <- c(
  "0"    = "#AAAAAA",
  "0.25" = "#A9DFBF",
  "0.5"  = "#52BE80",
  "0.75" = "#1E8449",
  "1"    = "#0B5A2A"
)

ALPHA_LABS <- c(
  "0"    = "\u03b1 = 0 (CoNA)",
  "0.25" = "\u03b1 = 0.25",
  "0.5"  = "\u03b1 = 0.50",
  "0.75" = "\u03b1 = 0.75",
  "1"    = "\u03b1 = 1.00"
)

ALPHA_LTYPE <- c(
  "0"    = "solid",
  "0.25" = "dashed",
  "0.5"  = "dashed",
  "0.75" = "dashed",
  "1"    = "dashed"
)

# -----------------------------------------------------------------------
# Paper theme (Q1 submission standard)
# -----------------------------------------------------------------------
paper_theme <- function(base_size = 11) {
  theme_bw(base_size = base_size) +
    theme(
      text              = element_text(family = "serif"),
      plot.title        = element_text(face = "bold", size = base_size + 1,
                                       margin = margin(b = 4)),
      plot.subtitle     = element_text(size = base_size - 1,
                                       color = "grey40",
                                       margin = margin(b = 6)),
      plot.caption      = element_text(size = base_size - 3,
                                       color = "grey50", hjust = 0,
                                       margin = margin(t = 6)),
      axis.title        = element_text(size = base_size - 1),
      axis.text         = element_text(size = base_size - 2),
      axis.text.x       = element_text(angle = 45, hjust = 1),
      legend.position   = "bottom",
      legend.title      = element_blank(),
      legend.text       = element_text(size = base_size - 2),
      legend.key.width  = unit(1.2, "cm"),
      panel.grid.major  = element_line(color = "grey92", linewidth = 0.3),
      panel.grid.minor  = element_blank(),
      strip.background  = element_rect(fill = "grey96", color = "grey70"),
      strip.text        = element_text(face = "bold", size = base_size - 1),
      plot.margin       = margin(6, 8, 6, 6)
    )
}

# -----------------------------------------------------------------------
# Helpers
# -----------------------------------------------------------------------

# Format COP with thousands separator
cop_format <- function(prefix = "$", suffix = " COP") {
  scales::comma_format(prefix = prefix, suffix = suffix, big.mark = ",")
}

# Standard x axis for monthly series
date_axis <- function(breaks = "1 year", labels = "%Y") {
  scale_x_date(date_breaks = breaks, date_labels = labels,
               expand = c(0.01, 0))
}

# Recode city column
recode_city <- function(df, col = "ciudad") {
  df %>% dplyr::mutate(!!col := dplyr::recode(.data[[col]], !!!CITY_LABS))
}

# Recode member column from Sex_DemoGroup to label
recode_member <- function(df, sex_col = "Sex", age_col = "Demo_Group",
                          out_col = "member") {
  df %>%
    dplyr::mutate(!!out_col :=
                    dplyr::recode(paste0(.data[[sex_col]], "_", .data[[age_col]]),
                                  !!!MEMBER_LABS),
                  !!out_col := factor(.data[[out_col]], levels = MEMBER_ORDER))
}

message("Figure config loaded.")