########################################################
## 05_figures/bloque1_costos/tab02_premium_median.R
##
## Table 2: Nutritional quality premiums — median (Q1, Q3)
##
## Structure:
##   Rows: city × period
##     - Full period (2019–2024)
##     - Annual medians (2019, 2020, ..., 2024)
##
##   Cells: median (Q1, Q3)
##
## Writes: TAB_DIR/final/tab02_premium_median.xlsx
##         TAB_DIR/final/tab02_premium_median.tex
########################################################

#source("00_config.R")
source(file.path(REVIEW_DIR, "06_figures", "00_fig_config.R"))
library(tidyverse)
library(scales)
library(lubridate)
library(openxlsx)

# -----------------------------------------------------------------------
# 1. Load and prepare
# -----------------------------------------------------------------------
hcost <- readRDS(file.path(HCOST_DIR, "hcost_full.rds")) %>%
  mutate(fecha = as.Date(fecha))

deflator <- readRDS(file.path(PREP_DIR, "deflator_monthly.rds")) %>%
  filter(ciudad != "NACIONAL") %>%
  select(ciudad, fecha, deflator)

cost_monthly <- hcost %>%
  group_by(model, ciudad, fecha) %>%
  dplyr::summarise(cost_pc = mean(per_capita, na.rm = TRUE), .groups = "drop") %>%
  left_join(deflator, by = c("ciudad", "fecha")) %>%
  mutate(
    cost_real  = cost_pc * deflator,
    year       = year(fecha),
    ciudad_lbl = CITY_LABELS[ciudad]) %>%
  filter(fecha >= PAPER_START, fecha <= PAPER_END, !is.na(cost_real))

# Sanity check — uncomment to verify city label coverage
# print(unique(cost_monthly$ciudad_lbl))
# print(table(cost_monthly$ciudad, useNA = "always"))

# Wide format — real + premiums (adimensional)
cost_wide <- cost_monthly %>%
  select(ciudad_lbl, fecha, year, model, cost_real) %>%
  pivot_wider(names_from = model, values_from = cost_real) %>%
  mutate(
    `CoNA/CoCA` = CoNA / CoCA,
    `CoRD/CoCA` = CoRD / CoCA,
    `CoRD/CoNA` = CoRD / CoNA)

# -----------------------------------------------------------------------
# 2. Summary function: median (Q1, Q3) formatted
# -----------------------------------------------------------------------
fmt_median_iqr <- function(x, digits = 3) {
  q <- quantile(x, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
  sprintf("%.*f (%.*f, %.*f)",
          digits, q[2],
          digits, q[1],
          digits, q[3])
}

# -----------------------------------------------------------------------
# 3. Build summary table
# -----------------------------------------------------------------------
CITIES   <- c("Bogotá", "Medellín", "Cali")
PERIODS  <- c("2019–2024", as.character(2019:2024))
PREMS    <- c("CoNA/CoCA", "CoRD/CoCA", "CoRD/CoNA")

make_panel_median <- function(data_wide, vars, digits = 3) {
  rows <- list()
  for (city in CITIES) {
    for (period in PERIODS) {
      df <- if (period == "2019–2024") {
        data_wide %>% filter(ciudad_lbl == city)
      } else {
        data_wide %>% filter(ciudad_lbl == city,
                             year == as.integer(period))
      }
      row <- tibble(City = city, Period = period)
      for (v in vars) {
        row[[v]] <- fmt_median_iqr(df[[v]], digits)
      }
      rows[[length(rows) + 1]] <- row
    }
  }
  bind_rows(rows)
}

panel_prems_median <- make_panel_median(cost_wide, PREMS, digits = 3)

message("Panel premiums (median): ", nrow(panel_prems_median), " rows")

# -----------------------------------------------------------------------
# 4. LaTeX output
# -----------------------------------------------------------------------
write_latex_panel <- function(panel, vars, panel_title, label_suffix) {
  n_vars <- length(vars)
  col_spec <- paste0("llr", strrep("r", n_vars - 1))
  
  header_cols <- paste(vars, collapse = " & ")
  
  lines <- c(
    "\\begin{table}[htbp]",
    "\\centering\\small",
    sprintf("\\caption{%s}", panel_title),
    sprintf("\\label{tab:cost_%s}", label_suffix),
    sprintf("\\begin{tabular}{%s}", col_spec),
    "\\toprule",
    sprintf("City & Period & %s \\\\", header_cols),
    "\\addlinespace[2pt]",
    "\\midrule")
  
  for (city in CITIES) {
    sub <- panel %>% filter(City == city)
    lines <- c(lines,
               sprintf("\\multicolumn{%d}{l}{\\textit{%s}} \\\\",
                       n_vars + 2, city))
    for (i in seq_len(nrow(sub))) {
      r    <- sub[i, ]
      vals <- paste(sapply(vars, function(v) r[[v]]), collapse = " & ")
      lines <- c(lines,
                 sprintf(" & %s & %s \\\\", r$Period, vals))
    }
    if (city != CITIES[length(CITIES)])
      lines <- c(lines, "\\midrule")
  }
  
  lines <- c(lines,
             "\\bottomrule",
             "\\end{tabular}",
             "\\begin{minipage}{\\linewidth}",
             "\\vspace{2pt}\\footnotesize",
             paste0("\\textit{Note:} Cells report median (Q1, Q3) across monthly ",
                    "observations within each period. Premiums computed from real ",
                    "COP, deflated by city-level food CPI ",
                    "(DANE Divisi\\'{o}n 01100000, base: December 2018)."),
             "\\end{minipage}",
             "\\end{table}")
  
  lines
}

tex_prems_median <- write_latex_panel(
  panel_prems_median, PREMS,
  paste0("Nutritional quality premiums by city, median (Q1, Q3), ",
         "2019\\textendash{}2024"),
  "premiums_median")

writeLines(tex_prems_median,
           file.path(TAB_DIR, "final", "tab02_premium_median.tex"))

# -----------------------------------------------------------------------
# 5. Excel output
# -----------------------------------------------------------------------
write_panel_sheet <- function(wb, sheet_name, panel, vars, title) {
  
  addWorksheet(wb, sheet_name)
  
  writeData(wb, sheet_name, title, startRow = 1, startCol = 1)
  mergeCells(wb, sheet_name, rows = 1, cols = 1:(length(vars) + 2))
  addStyle(wb, sheet_name,
           createStyle(fontSize = 11, textDecoration = "bold",
                       wrapText = TRUE),
           rows = 1, cols = 1, gridExpand = TRUE)
  setRowHeights(wb, sheet_name, rows = 1, heights = 28)
  
  writeData(wb, sheet_name, panel, startRow = 2, colNames = TRUE)
  
  addStyle(wb, sheet_name,
           createStyle(textDecoration = "bold",
                       fgFill = "#E8EAF0",
                       border = "TopBottomLeftRight",
                       halign = "center"),
           rows = 2, cols = 1:(length(vars) + 2),
           gridExpand = TRUE)
  
  row_idx <- 3
  for (k in seq_along(CITIES)) {
    n_rows <- nrow(panel %>% filter(City == CITIES[k]))
    bg     <- if (k %% 2 == 1) "#FFFFFF" else "#F7F8FC"
    addStyle(wb, sheet_name,
             createStyle(fgFill = bg,
                         border = "TopBottomLeftRight",
                         borderColour = "#DDDDDD"),
             rows = row_idx:(row_idx + n_rows - 1),
             cols = 1:(length(vars) + 2),
             gridExpand = TRUE)
    addStyle(wb, sheet_name,
             createStyle(fgFill = bg,
                         textDecoration = "bold",
                         border = "TopBottomLeftRight",
                         borderColour = "#DDDDDD"),
             rows = row_idx,
             cols = 1:(length(vars) + 2),
             gridExpand = TRUE)
    row_idx <- row_idx + n_rows
  }
  
  note_row <- nrow(panel) + 3
  mergeCells(wb, sheet_name, rows = note_row,
             cols = 1:(length(vars) + 2))
  writeData(wb, sheet_name,
            paste0("Note: Median (Q1, Q3) across monthly observations. ",
                   "Premiums computed from real COP, deflated by city-level food CPI ",
                   "(DANE Divisi\u00f3n 01100000, base: December 2018)."),
            startRow = note_row, startCol = 1)
  addStyle(wb, sheet_name,
           createStyle(fontSize = 9,
                       textDecoration = "italic",
                       wrapText = TRUE),
           rows = note_row, cols = 1, gridExpand = TRUE)
  setRowHeights(wb, sheet_name, rows = note_row, heights = 35)
  
  setColWidths(wb, sheet_name,
               cols = 1:(length(vars) + 2),
               widths = c(10, 10, rep(18, length(vars))))
}

wb <- createWorkbook()

write_panel_sheet(
  wb, "Premiums (median)",
  panel_prems_median, PREMS,
  "Table 2. Nutritional quality premiums, median (Q1, Q3)")

saveWorkbook(wb,
             file.path(TAB_DIR, "final", "tab02_premium_median.xlsx"),
             overwrite = TRUE)

message("Table 2 saved — xlsx + tex.")