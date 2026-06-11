########################################################
## 05_figures/bloque1_costos/tab01_cost_annual.R
##
## Table 1: Real per capita diet costs and premiums
##
## Structure:
##   Panel A — Costs (CoCA, CoNA, CoRD)
##   Panel B — Premiums (CoNA/CoCA, CoRD/CoCA, CoRD/CoNA)
##
##   Rows: city × period
##     - Full period (2019–2024)
##     - Annual means (2019, 2020, ..., 2024)
##
##   Cells: mean (SD)
##
## Writes: TAB_DIR/final/tab01_cost_annual.xlsx
##         TAB_DIR/final/tab01_cost_annual.tex
########################################################

source("00_config.R")
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
    ciudad_lbl = CITY_LABS[ciudad]) %>%
  filter(fecha >= PAPER_START, fecha <= PAPER_END, !is.na(cost_real))

# Wide format for premiums
cost_wide <- cost_monthly %>%
  select(ciudad_lbl, fecha, year, model, cost_real) %>%
  pivot_wider(names_from = model, values_from = cost_real) %>%
  mutate(
    `CoNA/CoCA` = CoNA / CoCA,
    `CoRD/CoCA` = CoRD / CoCA,
    `CoRD/CoNA` = CoRD / CoNA)

# -----------------------------------------------------------------------
# 2. Summary function: mean (SD) formatted
# -----------------------------------------------------------------------
fmt_mean_sd <- function(x, digits_mean = 0, digits_sd = 0) {
  m  <- mean(x, na.rm = TRUE)
  s  <- sd(x,   na.rm = TRUE)
  if (digits_mean == 0) {
    sprintf("%s (%s)",
            scales::comma(round(m), big.mark = ","),
            scales::comma(round(s), big.mark = ","))
  } else {
    sprintf("%.*f (%.*f)", digits_mean, m, digits_sd, s)
  }
}

# -----------------------------------------------------------------------
# 3. Build summary table
# -----------------------------------------------------------------------
CITIES   <- c("Bogotá", "Medellín", "Cali")
PERIODS  <- c("2019–2024", as.character(2019:2024))
COSTS    <- c("CoCA", "CoNA", "CoRD")
PREMS    <- c("CoNA/CoCA", "CoRD/CoCA", "CoRD/CoNA")

make_panel <- function(vars, digits_mean, digits_sd) {
  rows <- list()
  for (city in CITIES) {
    for (period in PERIODS) {
      # Filter data
      df <- if (period == "2019–2024") {
        cost_wide %>% filter(ciudad_lbl == city)
      } else {
        cost_wide %>% filter(ciudad_lbl == city,
                             year == as.integer(period))
      }
      row <- tibble(City = city, Period = period)
      for (v in vars) {
        row[[v]] <- fmt_mean_sd(df[[v]], digits_mean, digits_sd)
      }
      rows[[length(rows) + 1]] <- row
    }
  }
  bind_rows(rows)
}

panel_costs <- make_panel(COSTS, digits_mean = 0, digits_sd = 0)
panel_prems <- make_panel(PREMS, digits_mean = 3, digits_sd = 3)

message("Panel costs: ", nrow(panel_costs), " rows")
message("Panel prems: ", nrow(panel_prems), " rows")

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
             paste0("\\textit{Note:} Cells report mean (standard deviation) across monthly ",
                    "observations within each period. Real COP deflated by city-level food CPI ",
                    "(DANE Divisi\\'{o}n 01100000, base: December 2018). ",
                    "Per capita cost = mean daily cost across household members."),
             "\\end{minipage}",
             "\\end{table}")
  
  lines
}

tex_costs <- write_latex_panel(
  panel_costs, COSTS,
  paste0("Mean annual real per capita daily diet costs by city, ",
         "2019\\textendash{}2024 (real COP/day)"),
  "levels")

tex_prems <- write_latex_panel(
  panel_prems, PREMS,
  paste0("Nutritional quality premiums by city, ",
         "2019\\textendash{}2024"),
  "premiums")

writeLines(tex_costs,
           file.path(TAB_DIR, "final", "tab01a_cost_levels.tex"))
writeLines(tex_prems,
           file.path(TAB_DIR, "final", "tab01b_cost_premiums.tex"))

# -----------------------------------------------------------------------
# 5. Excel output — two sheets
# -----------------------------------------------------------------------
write_panel_sheet <- function(wb, sheet_name, panel, vars, title) {
  
  addWorksheet(wb, sheet_name)
  
  # Title
  writeData(wb, sheet_name, title, startRow = 1, startCol = 1)
  mergeCells(wb, sheet_name, rows = 1, cols = 1:(length(vars) + 2))
  addStyle(wb, sheet_name,
           createStyle(fontSize = 11, textDecoration = "bold",
                       wrapText = TRUE),
           rows = 1, cols = 1, gridExpand = TRUE)
  setRowHeights(wb, sheet_name, rows = 1, heights = 28)
  
  # Data
  writeData(wb, sheet_name, panel, startRow = 2, colNames = TRUE)
  
  # Header style
  addStyle(wb, sheet_name,
           createStyle(textDecoration = "bold",
                       fgFill = "#E8EAF0",
                       border = "TopBottomLeftRight",
                       halign = "center"),
           rows = 2, cols = 1:(length(vars) + 2),
           gridExpand = TRUE)
  
  # Alternate shading by city
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
    # Bold for full-period row
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
  
  # Note
  note_row <- nrow(panel) + 3
  mergeCells(wb, sheet_name, rows = note_row,
             cols = 1:(length(vars) + 2))
  writeData(wb, sheet_name,
            paste0("Note: Mean (standard deviation) across monthly observations. ",
                   "Real COP deflated by city-level food CPI ",
                   "(DANE Divisi\u00f3n 01100000, base: December 2018)."),
            startRow = note_row, startCol = 1)
  addStyle(wb, sheet_name,
           createStyle(fontSize = 9,
                       textDecoration = "italic",
                       wrapText = TRUE),
           rows = note_row, cols = 1, gridExpand = TRUE)
  setRowHeights(wb, sheet_name, rows = note_row, heights = 35)
  
  # Column widths
  setColWidths(wb, sheet_name,
               cols = 1:(length(vars) + 2),
               widths = c(10, 10, rep(16, length(vars))))
}

wb <- createWorkbook()

write_panel_sheet(
  wb, "A. Costs",
  panel_costs, COSTS,
  "Table 1A. Mean real per capita daily diet costs (real COP/day, base: December 2018)")

write_panel_sheet(
  wb, "B. Premiums",
  panel_prems, PREMS,
  "Table 1B. Nutritional quality premiums (cost ratios)")

saveWorkbook(wb,
             file.path(TAB_DIR, "final", "tab01_cost_annual.xlsx"),
             overwrite = TRUE)

message("Table 1 saved — xlsx (2 sheets) + tex (2 files).")