########################################################
## 05_figures/bloque2_shadow_prices/tab02_shadow_prices.R
##
## Table 2: Shadow price elasticity (SPE) by nutrient,
##          household member, and city
##
## Structure:
##   Rows:    binding nutrients (ordered by binding freq)
##   Columns: city × member
##   Cells:   mean (SD) of SPE — full period + annual
##
## Two panels:
##   Panel A — full period mean (SD)
##   Panel B — annual mean by year
##
## Writes: TAB_DIR/final/tab02_shadow_prices.xlsx
##         TAB_DIR/final/tab02_shadow_prices.tex
########################################################

source("00_config.R")
source(file.path(REVIEW_DIR, "06_figures", "00_fig_config.R"))
library(tidyverse)
library(lubridate)
library(scales)
library(openxlsx)

# -----------------------------------------------------------------------
# 1. Load data
# -----------------------------------------------------------------------
cona <- readRDS(file.path(CONA_DIR, "cona_results.rds"))

spe <- cona$spe %>%
  mutate(
    fecha      = as.Date(fecha),
    year       = year(fecha),
    member     = recode(paste0(Sex, "_", Age), !!!MEMBER_LABS),
    member     = factor(member, levels = MEMBER_ORDER),
    ciudad_lbl = CITY_LABS[ciudad],
    ciudad_lbl = factor(ciudad_lbl,
                        levels = c("Bogotá", "Medellín", "Cali"))) %>%
  filter(constraint == "Min",
         fecha >= PAPER_START, fecha <= PAPER_END)

limit <- cona$limit %>%
  mutate(fecha = as.Date(fecha)) %>%
  filter(fecha >= PAPER_START, fecha <= PAPER_END)

# -----------------------------------------------------------------------
# 2. All nutrients ordered by overall binding frequency
#    Include all nutrients from $spe — order by freq from $limit
# -----------------------------------------------------------------------
all_nutrients <- unique(spe$Nutrients)

freq_order <- limit %>%
  group_by(Nutrients) %>%
  dplyr::summarise(freq = mean(Limiting == 1, na.rm = TRUE),
                   .groups = "drop") %>%
  arrange(desc(freq))

# Nutrients in $limit ordered by freq, then any remaining from $spe
nutrient_order <- c(
  freq_order$Nutrients,
  setdiff(all_nutrients, freq_order$Nutrients))

message(sprintf("  %d total nutrients", length(nutrient_order)))

# -----------------------------------------------------------------------
# 3. Format helper: mean (SD)
# -----------------------------------------------------------------------
fmt <- function(x) {
  m <- mean(x, na.rm = TRUE)
  s <- sd(x,   na.rm = TRUE)
  sprintf("%.2f (%.2f)", m, s)
}

CITIES  <- c("Bogotá", "Medellín", "Cali")
MEMBERS <- MEMBER_ORDER
YEARS   <- 2019:2024

# Column order: Bogotá × [male, female, child], Medellín × ..., Cali × ...
col_order <- as.vector(outer(CITIES, MEMBERS, paste, sep = " — "))

# -----------------------------------------------------------------------
# 4. Panel A: full period mean (SD)
# -----------------------------------------------------------------------
panel_full <- spe %>%
  filter(Nutrients %in% nutrient_order) %>%
  group_by(Nutrients, ciudad_lbl, member) %>%
  dplyr::summarise(cell = fmt(SPE), .groups = "drop") %>%
  mutate(col = paste(ciudad_lbl, member, sep = " — ")) %>%
  select(Nutrients, col, cell) %>%
  pivot_wider(names_from = col, values_from = cell) %>%
  mutate(Nutrients = factor(Nutrients, levels = nutrient_order)) %>%
  arrange(Nutrients) %>%
  mutate(Period = "2019\u20132024", .after = Nutrients) %>%
  select(Nutrients, Period, any_of(col_order))

# -----------------------------------------------------------------------
# 5. Panel B: annual mean (SD)
# -----------------------------------------------------------------------
panel_annual <- map_dfr(YEARS, function(yr) {
  spe %>%
    filter(Nutrients %in% nutrient_order, year == yr) %>%
    group_by(Nutrients, ciudad_lbl, member) %>%
    dplyr::summarise(cell = fmt(SPE), .groups = "drop") %>%
    mutate(col    = paste(ciudad_lbl, member, sep = " — "),
           Period = as.character(yr)) %>%
    select(Nutrients, Period, col, cell) %>%
    pivot_wider(names_from = col, values_from = cell)
}) %>%
  mutate(Nutrients = factor(Nutrients, levels = nutrient_order)) %>%
  arrange(Nutrients, Period) %>%
  select(Nutrients, Period, any_of(col_order))

# -----------------------------------------------------------------------
# 6. Combined table: full period first, then years
# -----------------------------------------------------------------------
tab_full <- bind_rows(panel_full, panel_annual) %>%
  arrange(Nutrients, Period)

message(sprintf("  Table: %d rows × %d columns",
                nrow(tab_full), ncol(tab_full)))

# -----------------------------------------------------------------------
# 7. LaTeX output
# -----------------------------------------------------------------------
NCOLS    <- ncol(tab_full)
col_spec <- paste0("ll", strrep("r", NCOLS - 2))

# Column headers (city + member, two-row header)
city_header <- paste(
  sapply(CITIES, function(c)
    sprintf("\\multicolumn{%d}{c}{%s}", length(MEMBERS), c)),
  collapse = " & ")

member_header <- paste(
  rep(c("Male", "Female", "Child"), length(CITIES)),
  collapse = " & ")

latex_lines <- c(
  "\\begin{table}[htbp]",
  "\\centering\\small\\setlength{\\tabcolsep}{3pt}",
  paste0("\\caption{Shadow price elasticity (SPE) of binding nutritional ",
         "constraints by household member and city, 2019\\textendash{}2024}"),
  "\\label{tab:shadow_prices}",
  sprintf("\\begin{tabular}{%s}", col_spec),
  "\\toprule",
  sprintf("Nutrient & Period & %s \\\\", city_header),
  sprintf(" &  & %s \\\\", member_header),
  "\\midrule")

current_nut <- ""
for (i in seq_len(nrow(tab_full))) {
  r   <- tab_full[i, ]
  nut <- as.character(r$Nutrients)
  per <- r$Period
  vals <- paste(sapply(col_order, function(c) {
    v <- r[[c]]
    if (is.na(v)) "---" else v
  }), collapse = " & ")
  
  if (nut != current_nut) {
    if (current_nut != "") latex_lines <- c(latex_lines, "\\midrule")
    current_nut <- nut
    latex_lines <- c(latex_lines,
                     sprintf("\\multicolumn{%d}{l}{\\textit{%s}} \\\\", NCOLS, nut))
  }
  # Bold for full period row
  per_fmt <- if (per == "2019\u20132024") sprintf("\\textbf{%s}", per) else per
  latex_lines <- c(latex_lines,
                   sprintf(" & %s & %s \\\\", per_fmt, vals))
}

latex_lines <- c(latex_lines,
                 "\\bottomrule",
                 "\\end{tabular}",
                 "\\begin{minipage}{\\linewidth}",
                 "\\vspace{2pt}\\footnotesize",
                 paste0("\\textit{Note:} Cells report mean (standard deviation) of the shadow ",
                        "price elasticity (SPE) across monthly observations. ",
                        "SPE = shadow price as share of total daily diet cost. ",
                        "Bold rows report the full-period mean (2019\\textendash{}2024). ",
                        "Only nutrients with Limiting = 1 in at least one observation shown. ",
                        "Nutrients ordered by descending overall binding frequency."),
                 "\\end{minipage}",
                 "\\end{table}")

writeLines(latex_lines,
           file.path(TAB_DIR, "final", "tab02_shadow_prices.tex"))

# -----------------------------------------------------------------------
# 8. Excel output
# -----------------------------------------------------------------------
wb <- createWorkbook()
addWorksheet(wb, "SPE")

HEADER_COLOR  <- "#1A3A5C"
SUBHEAD_COLOR <- "#E8EAF0"
BOLD_COLOR    <- "#D6EAF8"   # full-period rows
ALT1          <- "#FFFFFF"
ALT2          <- "#F7F8FC"

# Title
title_txt <- paste0("Table 2. Shadow price elasticity (SPE) of binding ",
                    "nutritional constraints by household member and city, ",
                    "2019\u20132024. Cells: mean (SD).")
writeData(wb, "SPE", title_txt, startRow = 1, startCol = 1)
mergeCells(wb, "SPE", rows = 1, cols = 1:NCOLS)
addStyle(wb, "SPE",
         createStyle(fontSize = 11, textDecoration = "bold",
                     fontColour = "white", fgFill = paste0("#", "1A3A5C"),
                     wrapText = TRUE, valign = "center"),
         rows = 1, cols = 1, gridExpand = TRUE)
setRowHeights(wb, "SPE", rows = 1, heights = 32)

# City header row
city_row <- c("Nutrient", "Period",
              rep(CITIES, each = length(MEMBERS)))
writeData(wb, "SPE",
          as.data.frame(t(city_row)),
          startRow = 2, colNames = FALSE)

# Merge city header cells
col_start <- 3
for (city in CITIES) {
  mergeCells(wb, "SPE", rows = 2,
             cols = col_start:(col_start + length(MEMBERS) - 1))
  col_start <- col_start + length(MEMBERS)
}

addStyle(wb, "SPE",
         createStyle(textDecoration = "bold", fgFill = SUBHEAD_COLOR,
                     halign = "center", border = "TopBottomLeftRight"),
         rows = 2, cols = 1:NCOLS, gridExpand = TRUE)

# Member header row
member_row <- c("", "", rep(c("Male", "Female", "Child"), length(CITIES)))
writeData(wb, "SPE",
          as.data.frame(t(member_row)),
          startRow = 3, colNames = FALSE)
addStyle(wb, "SPE",
         createStyle(textDecoration = "bold", fgFill = SUBHEAD_COLOR,
                     halign = "center", border = "TopBottomLeftRight"),
         rows = 3, cols = 1:NCOLS, gridExpand = TRUE)
setRowHeights(wb, "SPE", rows = c(2, 3), heights = 20)

# Data
writeData(wb, "SPE", tab_full, startRow = 4, colNames = FALSE)

# Style rows
nutrients_vec <- as.character(tab_full$Nutrients)
periods_vec   <- tab_full$Period
nut_changes   <- c(TRUE, nutrients_vec[-1] != nutrients_vec[-length(nutrients_vec)])
nut_group     <- cumsum(nut_changes)

for (i in seq_len(nrow(tab_full))) {
  r   <- i + 3
  per <- periods_vec[i]
  grp <- nut_group[i]
  
  is_full   <- per == "2019\u20132024"
  bg        <- if (is_full) BOLD_COLOR else
    if (grp %% 2 == 1) ALT1 else ALT2
  bold_dec  <- if (is_full) "bold" else "normal"
  
  if (is_full) {
    addStyle(wb, "SPE",
             createStyle(fgFill = bg, textDecoration = "bold",
                         border = "TopBottomLeftRight",
                         borderColour = "#CCCCCC", halign = "right"),
             rows = r, cols = 1:NCOLS, gridExpand = TRUE)
    addStyle(wb, "SPE",
             createStyle(fgFill = bg, textDecoration = "bold",
                         border = "TopBottomLeftRight",
                         borderColour = "#CCCCCC", halign = "left"),
             rows = r, cols = 1:2, gridExpand = TRUE)
  } else {
    addStyle(wb, "SPE",
             createStyle(fgFill = bg,
                         border = "TopBottomLeftRight",
                         borderColour = "#CCCCCC", halign = "right"),
             rows = r, cols = 1:NCOLS, gridExpand = TRUE)
    addStyle(wb, "SPE",
             createStyle(fgFill = bg,
                         border = "TopBottomLeftRight",
                         borderColour = "#CCCCCC", halign = "left"),
             rows = r, cols = 1:2, gridExpand = TRUE)
  }
}

# Note row
note_row <- nrow(tab_full) + 4
mergeCells(wb, "SPE", rows = note_row, cols = 1:NCOLS)
writeData(wb, "SPE",
          paste0("Note: Mean (SD) of SPE across monthly observations. ",
                 "SPE = shadow price as share of total daily diet cost. ",
                 "Bold rows = full-period mean (2019\u20132024). ",
                 "Nutrients ordered by descending binding frequency."),
          startRow = note_row, startCol = 1)
addStyle(wb, "SPE",
         createStyle(fontSize = 9, textDecoration = "italic",
                     wrapText = TRUE),
         rows = note_row, cols = 1, gridExpand = TRUE)
setRowHeights(wb, "SPE", rows = note_row, heights = 35)

# Column widths
setColWidths(wb, "SPE", cols = 1,   widths = 16)
setColWidths(wb, "SPE", cols = 2,   widths = 10)
setColWidths(wb, "SPE", cols = 3:NCOLS, widths = 14)

saveWorkbook(wb,
             file.path(TAB_DIR, "final", "tab02_shadow_prices.xlsx"),
             overwrite = TRUE)

message("Table 2 saved (xlsx + tex).")