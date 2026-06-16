########################################################
## appendix/tabA3_cord_adequacy.R
##
## Table A3 (Appendix): Nutritional adequacy of CoRD
##   Rows: nutrients (excluding Sodium)
##   Cols: city
##   Cells: mean adequacy ratio (supply/requirement × 100)
##          + status (below / within / above)
##
## Reads:  CORD_DIR/cord_results.rds
##         PREP_DIR/panel_food_paper.rds
##         PREP_DIR/deflator_monthly.rds (via cord_adequacy_full)
##         PREP_DIR/household_ul.rds
##
## Note: run after fig02c_cord_composition.R which builds
##       cord_adequacy_full in the environment
##
## Writes: TAB_DIR/final/tabA3_cord_adequacy.xlsx
##         TAB_DIR/final/tabA3_cord_adequacy.tex
########################################################

source("00_config.R")
source(file.path(REVIEW_DIR, "06_figures", "00_fig_config.R"))
library(tidyverse)
library(lubridate)
library(openxlsx)

# cord_adequacy_full must exist in environment
# (built in fig02c_cord_composition.R)
stopifnot(exists("cord_adequacy_full"))

CITIES <- c("Bogotá","Medellín","Cali")

# -----------------------------------------------------------------------
# 1. Summary by nutrient × city — mean ratio + dominant status
# -----------------------------------------------------------------------
adequacy_tab <- cord_adequacy_full %>%
  group_by(ciudad_lbl, Nutrients) %>%
  dplyr::summarise(
    mean_ratio  = round(mean(supply / requirement * 100, na.rm=TRUE), 1),
    pct_below   = round(mean(status == "Below minimum")  * 100, 1),
    pct_within  = round(mean(status == "Within range")   * 100, 1),
    pct_above   = round(mean(status == "Above maximum")  * 100, 1),
    .groups     = "drop") %>%
  mutate(
    status_dom = case_when(
      pct_within == 100 ~ "Always within range",
      pct_above  > 50   ~ paste0("Exceeds UL (", pct_above, "% of obs)"),
      pct_below  > 0    ~ paste0("Below min (", pct_below, "% of obs)"),
      TRUE              ~ "Within range"),
    cell = paste0(mean_ratio, "% [", status_dom, "]"))

# Wide format: nutrients as rows, cities as columns
tab <- adequacy_tab %>%
  select(ciudad_lbl, Nutrients, cell) %>%
  pivot_wider(names_from = ciudad_lbl, values_from = cell) %>%
  mutate(Nutrients = factor(Nutrients,
                            levels = sort(unique(Nutrients)))) %>%
  arrange(Nutrients) %>%
  select(Nutrients, all_of(CITIES))

# -----------------------------------------------------------------------
# 2. LaTeX
# -----------------------------------------------------------------------
nc <- ncol(tab)
tex_lines <- c(
  "\\begin{table}[htbp]",
  "\\centering\\small\\setlength{\\tabcolsep}{4pt}",
  paste0("\\caption{Nutritional adequacy of the Cost of Recommended Diet ",
         "(CoRD) relative to CoNA minimum requirements, 2019\\textendash{}2024 ",
         "(Appendix)}"),
  "\\label{tab:cord_adequacy}",
  sprintf("\\begin{tabular}{l%s}", strrep("p{4.2cm}", nc-1)),
  "\\toprule",
  sprintf("%s \\\\", paste(names(tab), collapse=" & ")),
  "\\midrule")

for (i in seq_len(nrow(tab))) {
  r <- tab[i,]
  vals <- paste(sapply(2:nc, function(j) {
    v <- r[[j]]
    if (is.na(v)) "---" else v
  }), collapse=" & ")
  tex_lines <- c(tex_lines,
                 sprintf("%s & %s \\\\", r$Nutrients, vals))
}

tex_lines <- c(tex_lines,
               "\\bottomrule","\\end{tabular}",
               "\\begin{minipage}{\\linewidth}",
               "\\vspace{2pt}\\footnotesize",
               paste0("\\textit{Note:} Cells report mean adequacy ratio ",
                      "(CoRD supply / CoNA minimum requirement $\\times$ 100) ",
                      "and dominant nutritional status across all household member $\\times$ ",
                      "month observations. Sodium excluded: in the CoNA model it operates as ",
                      "a maximum constraint, not a minimum. ",
                      "UL = tolerable upper intake level from \\texttt{household\\_ul.rds}. ",
                      "CoRD = Cost of Recommended Diet; CoNA = Cost of Nutritional Adequacy."),
               "\\end{minipage}","\\end{table}")

writeLines(tex_lines,
           file.path(TAB_DIR, "final", "tabA3_cord_adequacy.tex"))

# -----------------------------------------------------------------------
# 3. Excel
# -----------------------------------------------------------------------
wb <- createWorkbook()
addWorksheet(wb, "CoRD adequacy")

nc <- ncol(tab)
title_txt <- paste0("Table A3. Nutritional adequacy of the CoRD diet ",
                    "relative to CoNA minimum requirements, 2019\u20132024. ",
                    "Cells: mean ratio (supply/requirement \u00d7 100) [status].")
writeData(wb, "CoRD adequacy", title_txt, startRow=1, startCol=1)
mergeCells(wb, "CoRD adequacy", rows=1, cols=1:nc)
addStyle(wb, "CoRD adequacy",
         createStyle(fontSize=11, textDecoration="bold", wrapText=TRUE),
         rows=1, cols=1)
setRowHeights(wb, "CoRD adequacy", rows=1, heights=40)

writeData(wb, "CoRD adequacy", tab, startRow=2)
addStyle(wb, "CoRD adequacy",
         createStyle(textDecoration="bold", fgFill="#E8EAF0",
                     border="TopBottomLeftRight", halign="center"),
         rows=2, cols=1:nc, gridExpand=TRUE)

for (i in seq_len(nrow(tab))) {
  bg <- if(i%%2==1) "#FFFFFF" else "#F7F8FC"
  addStyle(wb, "CoRD adequacy",
           createStyle(fgFill=bg, border="TopBottomLeftRight",
                       borderColour="#DDDDDD", wrapText=TRUE),
           rows=i+2, cols=1:nc, gridExpand=TRUE)
}
setRowHeights(wb, "CoRD adequacy", rows=3:(nrow(tab)+2), heights=35)

note_row <- nrow(tab)+3
mergeCells(wb, "CoRD adequacy", rows=note_row, cols=1:nc)
writeData(wb, "CoRD adequacy",
          paste0("Note: Sodium excluded (maximum constraint in CoNA model). ",
                 "UL = tolerable upper intake level. ",
                 "Status based on all household member \u00d7 month observations."),
          startRow=note_row, startCol=1)
addStyle(wb, "CoRD adequacy",
         createStyle(fontSize=9, textDecoration="italic", wrapText=TRUE),
         rows=note_row, cols=1)
setRowHeights(wb, "CoRD adequacy", rows=note_row, heights=35)
setColWidths(wb, "CoRD adequacy", cols=1, widths=16)
setColWidths(wb, "CoRD adequacy", cols=2:nc, widths=30)

saveWorkbook(wb,
             file.path(TAB_DIR, "final", "tabA3_cord_adequacy.xlsx"),
             overwrite=TRUE)

message("Table A3 saved (xlsx + tex).")