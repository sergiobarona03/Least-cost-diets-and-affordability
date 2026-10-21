########################################################
## appendix/tabA2_cord_composition.R
##
## Table A2 (Appendix): CoRD composition and cost by GABA group
##   Panel A — Mean daily servings (SD) by group × city
##   Panel B — Mean real cost contribution (COP/day) and % of total
##             by group × city
##
## Reads:  CORD_DIR/cord_results.rds
##         PREP_DIR/panel_food_paper.rds
##         PREP_DIR/deflator_monthly.rds
##
## Writes: TAB_DIR/final/tabA2_cord_composition.xlsx
##         TAB_DIR/final/tabA2_cord_composition.tex
########################################################

source("00_config.R")
source(file.path(REVIEW_DIR, "06_figures", "00_fig_config.R"))
library(tidyverse)
library(lubridate)
library(openxlsx)

CITIES     <- c("BOGOTA","MEDELLIN","CALI")
CITY_LABS_TAB <- c("BOGOTA"="Bogotá","MEDELLIN"="Medellín","CALI"="Cali")

GROUP_LABS <- c(
  "Grasas"                                                       = "Fats",
  "Leche y productos lácteos"                                    = "Dairy",
  "Carnes, huevos, leguminosas, frutos secos y semillas"         = "Meat, eggs & legumes",
  "Azúcares"                                                     = "Sugars",
  "Cereales, raíces, tubérculos y plátanos"                      = "Cereals & starches",
  "Frutas"                                                       = "Fruits",
  "Verduras"                                                     = "Vegetables")

GROUP_ORDER <- c("Fats","Dairy","Meat, eggs & legumes","Sugars",
                 "Cereals & starches","Fruits","Vegetables")

# -----------------------------------------------------------------------
# 1. Load
# -----------------------------------------------------------------------
cord       <- readRDS(file.path(CORD_DIR, "cord_results.rds"))
data_paper <- readRDS(file.path(PREP_DIR, "panel_food_paper.rds"))
deflator   <- readRDS(file.path(PREP_DIR, "deflator_monthly.rds")) %>%
  filter(ciudad != "NACIONAL") %>%
  select(ciudad, fecha, deflator)

# -----------------------------------------------------------------------
# 2. Panel A — servings
# -----------------------------------------------------------------------
servings <- cord$comp %>%
  mutate(fecha   = as.Date(fecha)) %>%
  filter(fecha >= PAPER_START, fecha <= PAPER_END) %>%
  mutate(Group_en = recode(Group, !!!GROUP_LABS)) %>%
  group_by(ciudad, Group_en) %>%
  dplyr::summarise(
    mean_s = round(mean(Number_Serving, na.rm=TRUE), 2),
    sd_s   = round(sd(Number_Serving,   na.rm=TRUE), 3),
    .groups = "drop") %>%
  mutate(
    cell       = sprintf("%.2f (%.3f)", mean_s, sd_s),
    ciudad_lbl = CITY_LABS_TAB[ciudad],
    Group_en   = factor(Group_en, levels = GROUP_ORDER)) %>%
  select(Group_en, ciudad_lbl, cell) %>%
  pivot_wider(names_from = ciudad_lbl, values_from = cell) %>%
  arrange(Group_en) %>%
  select(Group_en, all_of(CITY_LABS_TAB[CITIES]))

names(servings)[1] <- "Food group"

# -----------------------------------------------------------------------
# 3. Panel B — cost contribution
# -----------------------------------------------------------------------
cord_cost <- cord$comp %>%
  mutate(fecha = as.Date(fecha)) %>%
  filter(fecha >= PAPER_START, fecha <= PAPER_END) %>%
  left_join(
    data_paper %>%
      mutate(fecha = as.Date(fecha)) %>%
      select(ciudad, fecha, articulo, precio_100g,
             gramos_g_1_intercambio_1_intercambio) %>%
      rename(Food = articulo,
             Serving_g = gramos_g_1_intercambio_1_intercambio),
    by = c("ciudad","fecha","Food"),
    relationship = "many-to-many") %>%
  left_join(deflator, by = c("ciudad","fecha")) %>%
  mutate(
    cost_contrib = (precio_100g * deflator) * Serving_g / 100 * Number_Serving,
    Group_en     = recode(Group, !!!GROUP_LABS)) %>%
  filter(!is.na(cost_contrib))

cost_tab <- cord_cost %>%
  group_by(ciudad, Group_en) %>%
  dplyr::summarise(
    mean_cost = mean(cost_contrib, na.rm=TRUE),
    .groups   = "drop") %>%
  group_by(ciudad) %>%
  mutate(pct = round(mean_cost / sum(mean_cost) * 100, 1),
         mean_cost = round(mean_cost, 1)) %>%
  ungroup() %>%
  mutate(
    cell       = sprintf("%.0f (%.1f%%)", mean_cost, pct),
    ciudad_lbl = CITY_LABS_TAB[ciudad],
    Group_en   = factor(Group_en, levels = GROUP_ORDER)) %>%
  select(Group_en, ciudad_lbl, cell) %>%
  pivot_wider(names_from = ciudad_lbl, values_from = cell) %>%
  arrange(Group_en) %>%
  select(Group_en, all_of(CITY_LABS_TAB[CITIES]))

names(cost_tab)[1] <- "Food group"

# -----------------------------------------------------------------------
# 4. LaTeX — two panels
# -----------------------------------------------------------------------
write_panel_tex <- function(panel, caption, label, note) {
  nc <- ncol(panel)
  lines <- c(
    "\\begin{table}[htbp]",
    "\\centering\\small",
    sprintf("\\caption{%s}", caption),
    sprintf("\\label{%s}", label),
    sprintf("\\begin{tabular}{l%s}", strrep("r", nc-1)),
    "\\toprule",
    sprintf("%s \\\\",
            paste(names(panel), collapse = " & ")),
    "\\midrule")
  for (i in seq_len(nrow(panel))) {
    r <- panel[i,]
    lines <- c(lines,
               sprintf("%s \\\\",
                       paste(sapply(r, function(x) ifelse(is.na(x),"---",x)),
                             collapse=" & ")))
  }
  lines <- c(lines,
             "\\bottomrule","\\end{tabular}",
             "\\begin{minipage}{\\linewidth}",
             "\\vspace{2pt}\\footnotesize",
             sprintf("\\textit{Note:} %s", note),
             "\\end{minipage}","\\end{table}")
  lines
}

tex_A <- write_panel_tex(
  servings,
  paste0("CoRD diet composition: mean daily servings by GABAS food group ",
         "and city, 2019\\textendash{}2024 (Appendix)"),
  "tab:cord_servings",
  paste0("Cells report mean (standard deviation) of daily servings per ",
         "household member across all monthly observations. ",
         "CoRD = Cost of Recommended Diet."))

tex_B <- write_panel_tex(
  cost_tab,
  paste0("CoRD diet cost structure: mean real cost contribution by GABAS ",
         "food group and city, 2019\\textendash{}2024 (Appendix)"),
  "tab:cord_cost",
  paste0("Cells report mean daily real cost contribution (real COP/day) and ",
         "percentage of total CoRD cost in parentheses. ",
         "Real COP deflated by city-level food CPI ",
         "(DANE Divisi\\'{o}n 01100000, base: December 2018). ",
         "CoRD = Cost of Recommended Diet."))

writeLines(tex_A,
           file.path(TAB_DIR, "final", "tabA2a_cord_servings.tex"))
writeLines(tex_B,
           file.path(TAB_DIR, "final", "tabA2b_cord_cost.tex"))

# -----------------------------------------------------------------------
# 5. Excel — two sheets
# -----------------------------------------------------------------------
wb <- createWorkbook()

write_sheet <- function(wb, sheet, panel, title, note) {
  addWorksheet(wb, sheet)
  nc <- ncol(panel)
  writeData(wb, sheet, title, startRow=1, startCol=1)
  mergeCells(wb, sheet, rows=1, cols=1:nc)
  addStyle(wb, sheet,
           createStyle(fontSize=11, textDecoration="bold", wrapText=TRUE),
           rows=1, cols=1)
  setRowHeights(wb, sheet, rows=1, heights=28)
  writeData(wb, sheet, panel, startRow=2)
  addStyle(wb, sheet,
           createStyle(textDecoration="bold", fgFill="#E8EAF0",
                       border="TopBottomLeftRight", halign="center"),
           rows=2, cols=1:nc, gridExpand=TRUE)
  for (i in seq_len(nrow(panel))) {
    bg <- if(i%%2==1) "#FFFFFF" else "#F7F8FC"
    addStyle(wb, sheet,
             createStyle(fgFill=bg, border="TopBottomLeftRight",
                         borderColour="#DDDDDD"),
             rows=i+2, cols=1:nc, gridExpand=TRUE)
  }
  nr <- nrow(panel)+3
  mergeCells(wb, sheet, rows=nr, cols=1:nc)
  writeData(wb, sheet, note, startRow=nr, startCol=1)
  addStyle(wb, sheet,
           createStyle(fontSize=9, textDecoration="italic", wrapText=TRUE),
           rows=nr, cols=1)
  setRowHeights(wb, sheet, rows=nr, heights=30)
  setColWidths(wb, sheet, cols=1, widths=22)
  setColWidths(wb, sheet, cols=2:nc, widths=18)
}

write_sheet(wb, "A. Servings",
            servings,
            "Table A2A. CoRD diet: mean daily servings by GABAS group and city (mean (SD))",
            "Mean (SD) of daily servings per household member across all monthly observations.")

write_sheet(wb, "B. Cost contribution",
            cost_tab,
            "Table A2B. CoRD diet: mean real cost contribution by GABAS group and city",
            "Mean daily real cost (COP/day) and % of total CoRD cost. Real COP base: December 2018.")

saveWorkbook(wb,
             file.path(TAB_DIR, "final", "tabA2_cord_composition.xlsx"),
             overwrite=TRUE)

message("Table A2 saved (xlsx 2 sheets + 2 tex files).")