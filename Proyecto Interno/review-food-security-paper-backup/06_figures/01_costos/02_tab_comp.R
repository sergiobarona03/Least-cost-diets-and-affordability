########################################################
## appendix/tabA1_cona_composition.R
##
## Table A1 (Appendix): CoNA core food composition
##   Rows: food items (present >= 75% months)
##   Cols: city × year (2019, 2022, 2024)
##   Cells: mean quantity g/day (% months present)
##   + real price per 100g in parentheses
##
## Reads:  CONA_DIR/cona_results.rds
##         PREP_DIR/panel_food_paper.rds
##         PREP_DIR/deflator_monthly.rds
##
## Writes: TAB_DIR/final/tabA1_cona_composition.xlsx
##         TAB_DIR/final/tabA1_cona_composition.tex
########################################################

source("00_config.R")
source(file.path(REVIEW_DIR, "06_figures", "00_fig_config.R"))
library(tidyverse)
library(lubridate)
library(openxlsx)

CORE_FOODS <- c("LECHE PASTEURIZADA", "ARVEJA SECA",
                "HARINA DE TRIGO",   "LENTEJAS",
                "MANTECA VEGETAL",   "GUAYABAS")

YEARS_SHOW <- c(2019, 2022, 2024)
CITIES     <- c("BOGOTA","MEDELLIN","CALI")
CITY_LABS_TAB <- c("BOGOTA"="Bogotá","MEDELLIN"="Medellín","CALI"="Cali")

# -----------------------------------------------------------------------
# 1. Load
# -----------------------------------------------------------------------
cona       <- readRDS(file.path(CONA_DIR, "cona_results.rds"))
data_paper <- readRDS(file.path(PREP_DIR, "panel_food_paper.rds"))
deflator   <- readRDS(file.path(PREP_DIR, "deflator_monthly.rds")) %>%
  filter(ciudad != "NACIONAL") %>%
  select(ciudad, fecha, deflator)

# -----------------------------------------------------------------------
# 2. Quantities
# -----------------------------------------------------------------------
qty <- cona$comp %>%
  mutate(fecha = as.Date(fecha), year = year(fecha)) %>%
  filter(Food %in% CORE_FOODS,
         fecha >= PAPER_START, fecha <= PAPER_END,
         year %in% YEARS_SHOW) %>%
  group_by(ciudad, Food, year) %>%
  dplyr::summarise(
    mean_qty   = round(mean(quantity, na.rm=TRUE), 1),
    pct_months = round(mean(quantity > 0)*100, 1),
    .groups    = "drop")

# -----------------------------------------------------------------------
# 3. Real prices
# -----------------------------------------------------------------------
prices <- data_paper %>%
  mutate(fecha = as.Date(fecha), year = year(fecha)) %>%
  filter(articulo %in% CORE_FOODS,
         fecha >= PAPER_START, fecha <= PAPER_END,
         year %in% YEARS_SHOW) %>%
  left_join(deflator, by = c("ciudad","fecha")) %>%
  mutate(precio_real = precio_100g * deflator) %>%
  group_by(ciudad, articulo, year) %>%
  dplyr::summarise(
    mean_price = round(mean(precio_real, na.rm=TRUE), 0),
    .groups    = "drop") %>%
  rename(Food = articulo)

# -----------------------------------------------------------------------
# 4. Combined cell: qty (pct%) [price]
# -----------------------------------------------------------------------
combined <- qty %>%
  left_join(prices, by = c("ciudad","Food","year")) %>%
  mutate(
    cell = sprintf("%.0f g (%.0f%%)\n[COP %s]",
                   mean_qty, pct_months,
                   scales::comma(mean_price, big.mark=",")),
    col  = paste0(CITY_LABS_TAB[ciudad], " ", year)) %>%
  select(Food, col, cell)

# Order columns: city × year
col_order <- as.vector(outer(CITY_LABS_TAB[CITIES],
                             YEARS_SHOW, paste))

# Order foods by overall quantity
food_order <- qty %>%
  group_by(Food) %>%
  dplyr::summarise(overall = mean(mean_qty), .groups="drop") %>%
  arrange(desc(overall)) %>%
  pull(Food)

tab <- combined %>%
  pivot_wider(names_from=col, values_from=cell,
              values_fill = "---") %>%
  mutate(Food = factor(Food, levels=food_order)) %>%
  arrange(Food) %>%
  select(Food, any_of(col_order))

# -----------------------------------------------------------------------
# 5. LaTeX
# -----------------------------------------------------------------------
NCOLS <- ncol(tab)
city_header <- paste(
  sapply(CITY_LABS_TAB[CITIES], function(c)
    sprintf("\\multicolumn{%d}{c}{%s}",
            length(YEARS_SHOW),
            gsub("á","\\'{a}",gsub("í","\\'{i}",c)))),
  collapse = " & ")

year_header <- paste(rep(YEARS_SHOW, length(CITIES)),
                     collapse = " & ")

latex_lines <- c(
  "\\begin{table}[htbp]",
  "\\centering\\small\\setlength{\\tabcolsep}{3pt}",
  paste0("\\caption{Core food composition of the CoNA diet: ",
         "mean daily quantity, selection frequency and real price ",
         "by city and year (Appendix)}"),
  "\\label{tab:cona_composition}",
  sprintf("\\begin{tabular}{l%s}", strrep("r", NCOLS-1)),
  "\\toprule",
  sprintf("Food & %s \\\\", city_header),
  sprintf(" & %s \\\\", year_header),
  "\\addlinespace[2pt]",
  "\\midrule")

for (i in seq_len(nrow(tab))) {
  r    <- tab[i,]
  vals <- paste(sapply(2:NCOLS, function(j) {
    v <- r[[j]]; if(is.na(v)||v=="---") "---" else v
  }), collapse=" & ")
  latex_lines <- c(latex_lines,
                   sprintf("%s & %s \\\\", r$Food, vals))
}

latex_lines <- c(latex_lines,
                 "\\bottomrule","\\end{tabular}",
                 "\\begin{minipage}{\\linewidth}",
                 "\\vspace{2pt}\\footnotesize",
                 paste0("\\textit{Note:} Cells report mean daily quantity (g/day), ",
                        "percentage of monthly observations in which the food is selected ",
                        "(in parentheses), and mean real price per 100g ",
                        "[in brackets, real COP base December 2018]. ",
                        "Only foods present in $\\geq$75\\% of months in at least one city shown."),
                 "\\end{minipage}","\\end{table}")

writeLines(latex_lines,
           file.path(TAB_DIR, "final", "tabA1_cona_composition.tex"))

# -----------------------------------------------------------------------
# 6. Excel
# -----------------------------------------------------------------------
wb <- createWorkbook()
addWorksheet(wb, "CoNA composition")

title_txt <- paste0("Table A1. Core food composition of the CoNA diet: ",
                    "mean quantity (g/day), selection frequency (%) and ",
                    "real price [COP/100g] by city and year.")
writeData(wb, "CoNA composition", title_txt, startRow=1, startCol=1)
mergeCells(wb, "CoNA composition", rows=1, cols=1:NCOLS)
addStyle(wb, "CoNA composition",
         createStyle(fontSize=11, textDecoration="bold", wrapText=TRUE),
         rows=1, cols=1)
setRowHeights(wb, "CoNA composition", rows=1, heights=28)

writeData(wb, "CoNA composition", tab, startRow=2)
addStyle(wb, "CoNA composition",
         createStyle(textDecoration="bold", fgFill="#E8EAF0",
                     border="TopBottomLeftRight", halign="center",
                     wrapText=TRUE),
         rows=2, cols=1:NCOLS, gridExpand=TRUE)
setRowHeights(wb, "CoNA composition", rows=2, heights=28)

for (i in seq_len(nrow(tab))) {
  bg <- if(i%%2==1) "#FFFFFF" else "#F7F8FC"
  addStyle(wb, "CoNA composition",
           createStyle(fgFill=bg,
                       border="TopBottomLeftRight",
                       borderColour="#DDDDDD",
                       wrapText=TRUE),
           rows=i+2, cols=1:NCOLS, gridExpand=TRUE)
}
setRowHeights(wb, "CoNA composition",
              rows=3:(nrow(tab)+2), heights=40)

note_row <- nrow(tab)+3
mergeCells(wb, "CoNA composition", rows=note_row, cols=1:NCOLS)
writeData(wb, "CoNA composition",
          paste0("Note: Mean g/day (% months selected) [real COP/100g]. ",
                 "Years shown: 2019 (pre-inflation), 2022 (peak), 2024 (end)."),
          startRow=note_row, startCol=1)
addStyle(wb, "CoNA composition",
         createStyle(fontSize=9, textDecoration="italic", wrapText=TRUE),
         rows=note_row, cols=1)
setRowHeights(wb, "CoNA composition", rows=note_row, heights=30)
setColWidths(wb, "CoNA composition", cols=1, widths=22)
setColWidths(wb, "CoNA composition", cols=2:NCOLS, widths=16)

# -----------------------------------------------------------------------
# 7. Table 2 — Annual decomposition: price vs substitution effect
#    This goes in the main body of the paper (not appendix)
# -----------------------------------------------------------------------

# Reload monthly qty and prices for all core foods
qty_dec <- cona$comp %>%
  mutate(fecha = as.Date(fecha), year = year(fecha)) %>%
  filter(Food %in% CORE_FOODS,
         fecha >= PAPER_START, fecha <= PAPER_END) %>%
  group_by(ciudad, Food, year) %>%
  dplyr::summarise(mean_qty = mean(quantity, na.rm=TRUE), .groups="drop")

deflator_dec <- readRDS(file.path(PREP_DIR, "deflator_monthly.rds")) %>%
  filter(ciudad != "NACIONAL") %>%
  select(ciudad, fecha, deflator)

data_paper_dec <- readRDS(file.path(PREP_DIR, "panel_food_paper.rds"))

prices_dec <- data_paper_dec %>%
  mutate(fecha = as.Date(fecha), year = year(fecha)) %>%
  filter(articulo %in% CORE_FOODS,
         fecha >= PAPER_START, fecha <= PAPER_END) %>%
  left_join(deflator_dec, by = c("ciudad","fecha")) %>%
  mutate(precio_real = precio_100g * deflator) %>%
  group_by(ciudad, articulo, year) %>%
  dplyr::summarise(mean_precio_real = mean(precio_real, na.rm=TRUE),
                   .groups = "drop") %>%
  rename(Food = articulo)

base_qty_dec   <- qty_dec    %>% filter(year==2019) %>%
  rename(qty_base=mean_qty)       %>% select(-year)
base_price_dec <- prices_dec %>% filter(year==2019) %>%
  rename(price_base=mean_precio_real) %>% select(-year)

decomp_annual <- qty_dec %>%
  left_join(prices_dec,      by=c("ciudad","Food","year")) %>%
  left_join(base_qty_dec,    by=c("ciudad","Food")) %>%
  left_join(base_price_dec,  by=c("ciudad","Food")) %>%
  group_by(ciudad, year) %>%
  dplyr::summarise(
    cost_base  = round(sum(price_base       * qty_base / 100, na.rm=TRUE), 1),
    cost_price = round(sum(mean_precio_real * qty_base / 100, na.rm=TRUE), 1),
    cost_obs   = round(sum(mean_precio_real * mean_qty / 100, na.rm=TRUE), 1),
    .groups    = "drop") %>%
  mutate(
    price_effect = round(cost_price - cost_base, 1),
    subs_effect  = round(cost_obs   - cost_price, 1),
    total_effect = round(cost_obs   - cost_base, 1),
    ciudad_lbl   = CITY_LABS_TAB[ciudad]) %>%
  filter(year > 2019) %>%
  select(City      = ciudad_lbl,
         Year      = year,
         `Base (2019)`         = cost_base,
         `Observed cost`       = cost_obs,
         `Price effect`        = price_effect,
         `Substitution effect` = subs_effect,
         `Total change`        = total_effect)

# Wide format: cities as row groups, years as columns
decomp_wide <- decomp_annual %>%
  pivot_longer(cols = c(`Price effect`, `Substitution effect`, `Total change`),
               names_to = "Component", values_to = "value") %>%
  pivot_wider(names_from = Year, values_from = value) %>%
  arrange(City, factor(Component,
                       levels = c("Price effect",
                                  "Substitution effect",
                                  "Total change"))) %>%
  select(City, Component, everything(),
         -`Base (2019)`, -`Observed cost`)

NCOLS_D <- ncol(decomp_wide)
YEARS_D <- as.character(2020:2024)

# --- LaTeX ---
latex_decomp <- c(
  "\begin{table}[htbp]",
  "\centering\small",
  paste0("\caption{Annual decomposition of CoNA real cost change relative ",
         "to 2019: price effect vs substitution effect (real COP/day)}"),
  "\label{tab:decomp_price_subs}",
  sprintf("\begin{tabular}{ll%s}", strrep("r", length(YEARS_D))),
  "\toprule",
  sprintf("City & Component & %s \\", paste(YEARS_D, collapse=" & ")),
  "\midrule")

for (city in unique(decomp_wide$City)) {
  sub <- decomp_wide %>% filter(City == city)
  latex_decomp <- c(latex_decomp,
                    sprintf("\multicolumn{%d}{l}{\textit{%s}} \\",
                            NCOLS_D,
                            gsub("á","\'{a}", gsub("í","\'{i}", gsub("é","\'{e}", city)))))
  for (i in seq_len(nrow(sub))) {
    r    <- sub[i,]
    vals <- paste(sapply(YEARS_D, function(y) {
      v <- r[[y]]
      if (is.na(v)) "---"
      else if (r$Component == "Total change")
        sprintf("\textbf{%.1f}", v)
      else sprintf("%.1f", v)
    }), collapse=" & ")
    latex_decomp <- c(latex_decomp,
                      sprintf(" & %s & %s \\", r$Component, vals))
  }
  if (city != tail(unique(decomp_wide$City), 1))
    latex_decomp <- c(latex_decomp, "\midrule")
}

latex_decomp <- c(latex_decomp,
                  "\bottomrule", "\end{tabular}",
                  "\begin{minipage}{\linewidth}",
                  "\vspace{2pt}\footnotesize",
                  paste0("\textit{Note:} Price effect = cost change holding January 2019 ",
                         "quantities fixed at observed prices. ",
                         "Substitution effect = additional change from LP quantity adjustments. ",
                         "Total change = price effect + substitution effect. ",
                         "Core foods only (pasteurised milk, dried peas, wheat flour, lentils, ",
                         "vegetable fat, guavas). Real COP (base: December 2018)."),
                  "\end{minipage}", "\end{table}")

writeLines(latex_decomp,
           file.path(TAB_DIR, "final", "tab02_decomp_price_subs.tex"))

# --- Excel ---
addWorksheet(wb, "Table 2. Decomposition")

title_d <- paste0("Table 2. Annual decomposition of CoNA real cost change ",
                  "relative to 2019: price vs substitution effect (real COP/day)")
writeData(wb, "Table 2. Decomposition", title_d, startRow=1, startCol=1)
mergeCells(wb, "Table 2. Decomposition", rows=1, cols=1:NCOLS_D)
addStyle(wb, "Table 2. Decomposition",
         createStyle(fontSize=11, textDecoration="bold", wrapText=TRUE),
         rows=1, cols=1)
setRowHeights(wb, "Table 2. Decomposition", rows=1, heights=35)

writeData(wb, "Table 2. Decomposition", decomp_wide, startRow=2)
addStyle(wb, "Table 2. Decomposition",
         createStyle(textDecoration="bold", fgFill="#E8EAF0",
                     border="TopBottomLeftRight", halign="center"),
         rows=2, cols=1:NCOLS_D, gridExpand=TRUE)

# Shade by city and bold total rows
row_idx <- 3
for (k in seq_along(unique(decomp_wide$City))) {
  city_k <- unique(decomp_wide$City)[k]
  n_rows <- nrow(decomp_wide %>% filter(City == city_k))
  bg     <- if(k%%2==1) "#FFFFFF" else "#F7F8FC"
  addStyle(wb, "Table 2. Decomposition",
           createStyle(fgFill=bg, border="TopBottomLeftRight",
                       borderColour="#DDDDDD"),
           rows=row_idx:(row_idx+n_rows-1), cols=1:NCOLS_D,
           gridExpand=TRUE)
  # Bold total row
  addStyle(wb, "Table 2. Decomposition",
           createStyle(fgFill=bg, textDecoration="bold",
                       border="TopBottomLeftRight",
                       borderColour="#DDDDDD"),
           rows=row_idx+n_rows-1, cols=1:NCOLS_D,
           gridExpand=TRUE)
  row_idx <- row_idx + n_rows
}

note_row2 <- nrow(decomp_wide)+3
mergeCells(wb, "Table 2. Decomposition", rows=note_row2, cols=1:NCOLS_D)
writeData(wb, "Table 2. Decomposition",
          paste0("Note: Price effect = holding 2019 quantities fixed. ",
                 "Substitution effect = LP quantity adjustments. ",
                 "Core foods only. Real COP (base: December 2018)."),
          startRow=note_row2, startCol=1)
addStyle(wb, "Table 2. Decomposition",
         createStyle(fontSize=9, textDecoration="italic", wrapText=TRUE),
         rows=note_row2, cols=1)
setRowHeights(wb, "Table 2. Decomposition", rows=note_row2, heights=30)
setColWidths(wb, "Table 2. Decomposition",
             cols=1:NCOLS_D,
             widths=c(12, 20, rep(10, length(YEARS_D))))

saveWorkbook(wb,
             file.path(TAB_DIR, "final", "tabA1_cona_composition.xlsx"),
             overwrite=TRUE)

message("Table A1 saved (xlsx with 2 sheets + tex).")