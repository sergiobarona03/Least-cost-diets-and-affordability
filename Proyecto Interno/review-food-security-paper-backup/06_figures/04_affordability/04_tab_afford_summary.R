########################################################
## 05_figures/bloque4_affordability/tab04_afford_summary.R
##
## Table 4: Affordability indicators by decile × city × model
##
## Two panels:
##   Panel A — unaffordability rate (%)
##   Panel B — affordability gap (COP/day)
##   Rows:  decile × period (full + annual)
##   Cols:  city × model
##   Cells: mean (SD)
##
## Writes: TAB_DIR/final/tab04_afford_summary.xlsx
##         TAB_DIR/final/tab04_afford_summary.tex
########################################################

source("00_config.R")
source(file.path(REVIEW_DIR, "06_figures", "00_fig_config.R"))
library(tidyverse)
library(readxl)
library(lubridate)
library(scales)
library(openxlsx)

# -----------------------------------------------------------------------
# 1. Load and clean
# -----------------------------------------------------------------------
afford <- read_excel(file.path(AFFORD_DIR, "afford_results.xlsx")) %>%
  mutate(
    fecha      = as.Date(fecha),
    year       = year(fecha),
    ciudad_lbl = case_when(
      grepl("BOGOT", ciudad, ignore.case = TRUE) ~ "Bogotá",
      grepl("MEDEL", ciudad, ignore.case = TRUE) ~ "Medellín",
      grepl("CALI",  ciudad, ignore.case = TRUE) ~ "Cali",
      TRUE ~ ciudad),
    model   = factor(model,   levels = c("CoCA", "CoNA", "CoRD")),
    deciles = factor(deciles, levels = paste0("Decil ", 1:10))) %>%
  filter(fecha >= PAPER_START, fecha <= PAPER_END)

# -----------------------------------------------------------------------
# 2. Summary helpers
# -----------------------------------------------------------------------
fmt_rate <- function(x) sprintf("%.1f (%.1f)",
                                mean(x, na.rm=TRUE), sd(x, na.rm=TRUE))

fmt_gap <- function(x) sprintf("%.3f (%.3f)",
                               mean(x, na.rm=TRUE), sd(x, na.rm=TRUE))

fmt_sev <- function(x) sprintf("%.3f (%.3f)",
                               mean(x, na.rm=TRUE), sd(x, na.rm=TRUE))

DECILES <- paste0("Decil ", 1:10)
CITIES  <- c("Bogotá", "Medellín", "Cali")
MODELS  <- c("CoCA", "CoNA", "CoRD")
PERIODS <- c("2019\u20132024", as.character(2019:2024))

col_order <- as.vector(outer(CITIES, MODELS, paste, sep = " \u2014 "))

# -----------------------------------------------------------------------
# 3. Build panels
# -----------------------------------------------------------------------
make_panel <- function(fmt_fn, value_col) {
  rows <- list()
  for (dec in DECILES) {
    for (per in PERIODS) {
      df <- if (per == "2019\u20132024") {
        afford %>% filter(deciles == dec)
      } else {
        afford %>% filter(deciles == dec, year == as.integer(per))
      }
      row <- tibble(Decile = dec, Period = per)
      for (city in CITIES) {
        for (mod in MODELS) {
          vals <- df[[value_col]][df$ciudad_lbl == city &
                                    as.character(df$model) == mod]
          col_name <- paste(city, mod, sep = " \u2014 ")
          row[[col_name]] <- if (length(vals) > 0 && any(!is.na(vals)))
            fmt_fn(vals) else "---"
        }
      }
      rows[[length(rows) + 1]] <- row
    }
  }
  bind_rows(rows) %>% select(Decile, Period, any_of(col_order))
}

panel_rate <- make_panel(fmt_rate, "rate")
panel_gap  <- make_panel(fmt_gap,  "gap")
panel_sev  <- make_panel(fmt_sev,  "severity")

message(sprintf("  Table: %d rows × %d columns",
                nrow(panel_rate), ncol(panel_rate)))

# -----------------------------------------------------------------------
# 4. LaTeX
# -----------------------------------------------------------------------
write_latex <- function(panel, caption_txt, label) {
  NCOLS    <- ncol(panel)
  col_spec <- paste0("ll", strrep("r", NCOLS - 2))
  
  city_header <- paste(
    sapply(CITIES, function(c)
      sprintf("\\multicolumn{%d}{c}{%s}", length(MODELS),
              gsub("á","\\'{a}", gsub("é","\\'{e}", gsub("í","\\'{i}", c))))),
    collapse = " & ")
  
  model_header <- paste(rep(MODELS, length(CITIES)), collapse = " & ")
  
  lines <- c(
    "\\begin{table}[htbp]",
    "\\centering\\small\\setlength{\\tabcolsep}{3pt}",
    sprintf("\\caption{%s}", caption_txt),
    sprintf("\\label{%s}", label),
    sprintf("\\begin{tabular}{%s}", col_spec),
    "\\toprule",
    sprintf("Decile & Period & %s \\\\", city_header),
    sprintf(" & & %s \\\\", model_header),
    "\\midrule")
  
  cur_dec <- ""
  for (i in seq_len(nrow(panel))) {
    r   <- panel[i, ]
    dec <- r$Decile
    per <- r$Period
    if (dec != cur_dec) {
      if (cur_dec != "") lines <- c(lines, "\\midrule")
      cur_dec <- dec
      lines <- c(lines,
                 sprintf("\\multicolumn{%d}{l}{\\textit{%s}} \\\\", NCOLS, dec))
    }
    is_full <- grepl("\u2013", per)
    per_fmt <- if (is_full) sprintf("\\textbf{%s}", per) else per
    vals <- paste(sapply(col_order, function(c) {
      v <- r[[c]]; if (is.null(v) || is.na(v)) "---" else v
    }), collapse = " & ")
    lines <- c(lines, sprintf(" & %s & %s \\\\", per_fmt, vals))
  }
  
  lines <- c(lines,
             "\\bottomrule", "\\end{tabular}",
             "\\begin{minipage}{\\linewidth}",
             "\\vspace{2pt}\\footnotesize",
             paste0("\\textit{Note:} Cells report mean (standard deviation) across ",
                    "monthly observations. Bold rows = full-period mean ",
                    "(2019\\textendash{}2024)."),
             "\\end{minipage}", "\\end{table}")
  lines
}

writeLines(
  write_latex(panel_rate,
              paste0("Household unaffordability rate by income decile, city and diet model, ",
                     "2019\\textendash{}2024 (\\%)"),
              "tab:afford_rate"),
  file.path(TAB_DIR, "final", "tab04a_afford_rate.tex"))

writeLines(
  write_latex(panel_gap,
              paste0("Affordability gap by income decile, city and diet model, ",
                     "2019\\textendash{}2024. Mean proportional deficit of food budget (FGT1, 0\\textendash{}1)."),
              "tab:afford_gap"),
  file.path(TAB_DIR, "final", "tab04b_afford_gap.tex"))

writeLines(
  write_latex(panel_sev,
              paste0("Affordability severity (FGT2) by income decile, city and diet model, ",
                     "2019\textendash{}2024 (proportion 0\textendash{}1)"),
              "tab:afford_severity"),
  file.path(TAB_DIR, "final", "tab04c_afford_severity.tex"))

# -----------------------------------------------------------------------
# 5. Excel
# -----------------------------------------------------------------------
write_sheet <- function(wb, sheet, panel, title) {
  
  addWorksheet(wb, sheet)
  NCOLS <- ncol(panel)
  
  writeData(wb, sheet, title, startRow = 1, startCol = 1)
  mergeCells(wb, sheet, rows = 1, cols = 1:NCOLS)
  addStyle(wb, sheet,
           createStyle(fontSize = 11, textDecoration = "bold",
                       wrapText = TRUE),
           rows = 1, cols = 1)
  setRowHeights(wb, sheet, rows = 1, heights = 28)
  
  # City header
  city_row <- c("Decile", "Period",
                rep(CITIES, each = length(MODELS)))
  writeData(wb, sheet, as.data.frame(t(city_row)),
            startRow = 2, colNames = FALSE)
  col_start <- 3
  for (city in CITIES) {
    mergeCells(wb, sheet, rows = 2,
               cols = col_start:(col_start + length(MODELS) - 1))
    col_start <- col_start + length(MODELS)
  }
  addStyle(wb, sheet,
           createStyle(textDecoration = "bold", fgFill = "#E8EAF0",
                       halign = "center", border = "TopBottomLeftRight"),
           rows = 2, cols = 1:NCOLS, gridExpand = TRUE)
  
  # Model header
  model_row <- c("", "", rep(MODELS, length(CITIES)))
  writeData(wb, sheet, as.data.frame(t(model_row)),
            startRow = 3, colNames = FALSE)
  addStyle(wb, sheet,
           createStyle(textDecoration = "bold", fgFill = "#E8EAF0",
                       halign = "center", border = "TopBottomLeftRight"),
           rows = 3, cols = 1:NCOLS, gridExpand = TRUE)
  setRowHeights(wb, sheet, rows = c(2, 3), heights = 18)
  
  # Data
  writeData(wb, sheet, panel, startRow = 4, colNames = FALSE)
  
  decile_vec    <- panel$Decile
  dec_changes   <- c(TRUE, decile_vec[-1] != decile_vec[-length(decile_vec)])
  dec_group     <- cumsum(dec_changes)
  
  for (i in seq_len(nrow(panel))) {
    r       <- i + 3
    per     <- panel$Period[i]
    grp     <- dec_group[i]
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
  
  note_row <- nrow(panel) + 4
  mergeCells(wb, sheet, rows = note_row, cols = 1:NCOLS)
  writeData(wb, sheet,
            paste0("Note: Mean (SD) across monthly observations. ",
                   "Bold = full-period mean (2019\u20132024)."),
            startRow = note_row, startCol = 1)
  addStyle(wb, sheet,
           createStyle(fontSize = 9, textDecoration = "italic",
                       wrapText = TRUE),
           rows = note_row, cols = 1)
  setRowHeights(wb, sheet, rows = note_row, heights = 28)
  setColWidths(wb, sheet, cols = 1, widths = 10)
  setColWidths(wb, sheet, cols = 2, widths = 10)
  setColWidths(wb, sheet, cols = 3:NCOLS, widths = 14)
}

wb <- createWorkbook()
write_sheet(wb, "A. Rate (%)",
            panel_rate,
            paste0("Table 4A. Unaffordability rate (%) by income decile, ",
                   "city and diet model, 2019\u20132024"))
write_sheet(wb, "B. Gap (FGT1, 0-1)",
            panel_gap,
            paste0("Table 4B. Affordability gap (FGT1, 0–1) by income decile, ",
                   "city and diet model, 2019\u20132024"))

write_sheet(wb, "C. Severity (FGT2, 0-1)",
            panel_sev,
            paste0("Table 4C. Affordability severity (FGT2, 0–1) by income decile, ",
                   "city and diet model, 2019–2024. ",
                   "Squared gap, giving greater weight to households furthest from threshold."))

saveWorkbook(wb,
             file.path(TAB_DIR, "final", "tab04_afford_summary.xlsx"),
             overwrite = TRUE)

message("Table 4 saved (xlsx + tex).")