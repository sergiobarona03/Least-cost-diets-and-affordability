########################################################
## 05_figures/bloque3_ccona/tab03_ccona_premium.R
##
## Table 3: CC-CoNA cost and premium over CoNA
##
## Two panels:
##   Panel A — real per capita cost (COP/day)
##             rows: alpha × period | cols: city
##   Panel B — premium over CoNA (%)
##             rows: alpha × period | cols: city
##   Both: mean (SD), full period bold + annual rows
##
## Writes: TAB_DIR/final/tab03_ccona_premium.xlsx
##         TAB_DIR/final/tab03_ccona_premium.tex
########################################################

source("00_config.R")
source(file.path(REVIEW_DIR, "06_figures", "00_fig_config.R"))
library(tidyverse)
library(lubridate)
library(openxlsx)

# -----------------------------------------------------------------------
# 1. Load and prepare
# -----------------------------------------------------------------------
ccona <- readRDS(file.path(CCONA_DIR, "ccona_results.rds"))$cost %>%
  mutate(fecha = as.Date(fecha))

deflator <- readRDS(file.path(PREP_DIR, "deflator_monthly.rds")) %>%
  filter(ciudad != "NACIONAL") %>%
  select(ciudad, fecha, deflator)

cost_pc <- ccona %>%
  group_by(alpha_val, ciudad, fecha) %>%
  dplyr::summarise(cost_pc = mean(cost_day, na.rm = TRUE),
                   .groups = "drop") %>%
  left_join(deflator, by = c("ciudad", "fecha")) %>%
  mutate(
    cost_real  = cost_pc * deflator,
    year       = year(fecha),
    ciudad_lbl = CITY_LABS[ciudad]) %>%
  filter(fecha >= PAPER_START, fecha <= PAPER_END, !is.na(cost_real))

# Premium over CoNA (alpha = 0 baseline)
cona_base <- cost_pc %>%
  filter(alpha_val == 0) %>%
  select(ciudad, fecha, cost_base = cost_real)

cost_pc <- cost_pc %>%
  left_join(cona_base, by = c("ciudad", "fecha")) %>%
  mutate(premium_pct = (cost_real / cost_base - 1) * 100)

# -----------------------------------------------------------------------
# 2. Summary helper
# -----------------------------------------------------------------------
fmt_cost <- function(x) sprintf("%s (%s)",
                                scales::comma(round(mean(x, na.rm=TRUE), 0), big.mark=","),
                                scales::comma(round(sd(x,   na.rm=TRUE), 0), big.mark=","))

fmt_prem <- function(x) sprintf("%.2f (%.2f)",
                                mean(x, na.rm=TRUE), sd(x, na.rm=TRUE))

ALPHAS  <- c(0, 0.25, 0.5, 0.75, 1)
CITIES  <- c("Bogotá", "Medellín", "Cali")
PERIODS <- c("2019\u20132024", as.character(2019:2024))

# -----------------------------------------------------------------------
# 3. Build panels
# -----------------------------------------------------------------------
make_panel <- function(fmt_fn, value_col) {
  rows <- list()
  for (alp in ALPHAS) {
    for (per in PERIODS) {
      df <- if (per == "2019\u20132024") {
        cost_pc %>% filter(alpha_val == alp)
      } else {
        cost_pc %>% filter(alpha_val == alp, year == as.integer(per))
      }
      row <- tibble(
        Alpha  = paste0("\u03b1 = ", alp),
        Period = per)
      for (city in CITIES) {
        vals <- df[[value_col]][df$ciudad_lbl == city]
        row[[city]] <- if (length(vals) > 0) fmt_fn(vals) else "---"
      }
      rows[[length(rows) + 1]] <- row
    }
  }
  bind_rows(rows)
}

panel_cost <- make_panel(fmt_cost, "cost_real")
panel_prem <- make_panel(fmt_prem, "premium_pct")

# -----------------------------------------------------------------------
# 4. LaTeX
# -----------------------------------------------------------------------
write_latex <- function(panel, caption_txt, label) {
  lines <- c(
    "\\begin{table}[htbp]",
    "\\centering\\small",
    sprintf("\\caption{%s}", caption_txt),
    sprintf("\\label{%s}", label),
    "\\begin{tabular}{llrrr}",
    "\\toprule",
    "\\multicolumn{1}{c}{$\\alpha$} & Period & Bogot\\'{a} & Medell\\'{i}n & Cali \\\\",
    "\\midrule")
  
  cur_alp <- ""
  for (i in seq_len(nrow(panel))) {
    r   <- panel[i, ]
    alp <- r$Alpha
    per <- r$Period
    if (alp != cur_alp) {
      if (cur_alp != "") lines <- c(lines, "\\midrule")
      cur_alp <- alp
      lines <- c(lines,
                 sprintf("\\multicolumn{5}{l}{\\textit{%s}} \\\\",
                         gsub("\u03b1", "$\\\\alpha$", alp)))
    }
    is_full <- grepl("\u2013", per)
    per_fmt <- if (is_full) sprintf("\\textbf{%s}", per) else per
    lines <- c(lines,
               sprintf(" & %s & %s & %s & %s \\\\",
                       per_fmt,
                       r$Bogotá, r$Medellín, r$Cali))
  }
  
  lines <- c(lines,
             "\\bottomrule",
             "\\end{tabular}",
             "\\begin{minipage}{\\linewidth}",
             "\\vspace{2pt}\\footnotesize",
             paste0("\\textit{Note:} Cells report mean (standard deviation) across ",
                    "monthly observations. Bold rows = full-period mean ",
                    "(2019\\textendash{}2024). $\\alpha = 0$ corresponds to standard CoNA."),
             "\\end{minipage}",
             "\\end{table}")
  lines
}

writeLines(
  write_latex(
    panel_cost,
    paste0("CC-CoNA daily per capita real cost by $\\alpha$ and city, ",
           "2019\\textendash{}2024 (real COP/day)"),
    "tab:ccona_cost"),
  file.path(TAB_DIR, "final", "tab03a_ccona_cost.tex"))

writeLines(
  write_latex(
    panel_prem,
    paste0("CC-CoNA premium over CoNA by $\\alpha$ and city, ",
           "2019\\textendash{}2024 (\\%)"),
    "tab:ccona_premium"),
  file.path(TAB_DIR, "final", "tab03b_ccona_premium.tex"))

# -----------------------------------------------------------------------
# 5. Excel — two sheets
# -----------------------------------------------------------------------
write_sheet <- function(wb, sheet, panel, title) {
  
  addWorksheet(wb, sheet)
  NCOLS <- ncol(panel)
  
  # Title
  writeData(wb, sheet, title, startRow = 1, startCol = 1)
  mergeCells(wb, sheet, rows = 1, cols = 1:NCOLS)
  addStyle(wb, sheet,
           createStyle(fontSize = 11, textDecoration = "bold",
                       wrapText = TRUE),
           rows = 1, cols = 1)
  setRowHeights(wb, sheet, rows = 1, heights = 28)
  
  # Data
  writeData(wb, sheet, panel, startRow = 2, colNames = TRUE)
  addStyle(wb, sheet,
           createStyle(textDecoration = "bold", fgFill = "#E8EAF0",
                       border = "TopBottomLeftRight",
                       halign = "center"),
           rows = 2, cols = 1:NCOLS, gridExpand = TRUE)
  
  # Row styles
  alphas_vec    <- panel$Alpha
  alpha_changes <- c(TRUE, alphas_vec[-1] != alphas_vec[-length(alphas_vec)])
  alpha_group   <- cumsum(alpha_changes)
  
  for (i in seq_len(nrow(panel))) {
    r       <- i + 2
    per     <- panel$Period[i]
    grp     <- alpha_group[i]
    is_full <- grepl("\u2013", per)
    bg      <- if (is_full) "#D6EAF8" else
      if (grp %% 2 == 1) "#FFFFFF" else "#F7F8FC"
    
    if (is_full) {
      addStyle(wb, sheet,
               createStyle(fgFill = bg, textDecoration = "bold",
                           border = "TopBottomLeftRight",
                           borderColour = "#CCCCCC"),
               rows = r, cols = 1:NCOLS, gridExpand = TRUE)
    } else {
      addStyle(wb, sheet,
               createStyle(fgFill = bg,
                           border = "TopBottomLeftRight",
                           borderColour = "#CCCCCC"),
               rows = r, cols = 1:NCOLS, gridExpand = TRUE)
    }
  }
  
  # Note
  note_row <- nrow(panel) + 3
  mergeCells(wb, sheet, rows = note_row, cols = 1:NCOLS)
  writeData(wb, sheet,
            paste0("Note: Mean (SD) across monthly observations. ",
                   "Bold = full-period mean. \u03b1 = 0 is standard CoNA."),
            startRow = note_row, startCol = 1)
  addStyle(wb, sheet,
           createStyle(fontSize = 9, textDecoration = "italic",
                       wrapText = TRUE),
           rows = note_row, cols = 1)
  setRowHeights(wb, sheet, rows = note_row, heights = 30)
  setColWidths(wb, sheet, cols = 1:NCOLS,
               widths = c(12, 10, 16, 16, 16))
}

wb <- createWorkbook()
write_sheet(wb, "A. Cost",
            panel_cost,
            paste0("Table 3A. CC-CoNA daily per capita real cost ",
                   "(real COP/day, base: December 2018)"))
write_sheet(wb, "B. Premium",
            panel_prem,
            paste0("Table 3B. CC-CoNA premium over CoNA (%). ",
                   "\u03b1 = 0 is CoNA baseline."))

saveWorkbook(wb,
             file.path(TAB_DIR, "final", "tab03_ccona_premium.xlsx"),
             overwrite = TRUE)

message("Table 3 saved (xlsx + tex).")