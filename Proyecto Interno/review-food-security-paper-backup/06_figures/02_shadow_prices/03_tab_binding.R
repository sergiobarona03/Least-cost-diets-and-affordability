########################################################
## 05_figures/bloque2_shadow_prices/tab03_binding_frequency.R
##
## Table 3: Binding frequency of limiting nutrients
##   % of months that each nutrient is binding (Limiting == 1)
##
## Structure:
##   Rows: city × member × period
##     - Full period (2019–2024)
##     - Annual values (2019, 2020, ..., 2024)
##   Cols: nutrients (only those with > 0% in at least one year/period)
##
## Reads:  CONA_DIR/cona_results.rds
##
## Writes: TAB_DIR/final/tab03_binding_frequency.xlsx
##         TAB_DIR/final/tab03_binding_frequency.tex
########################################################

#source("00_config.R")
source(file.path(REVIEW_DIR, "06_figures", "00_fig_config.R"))
library(tidyverse)
library(lubridate)
library(openxlsx)

# -----------------------------------------------------------------------
# 0. Local city map
# -----------------------------------------------------------------------
city_lbl_map <- c(
  "BOGOTA"   = "Bogotá",
  "MEDELLIN" = "Medellín",
  "CALI"     = "Cali")

# -----------------------------------------------------------------------
# 1. Load data
# -----------------------------------------------------------------------
limit <- readRDS(file.path(CONA_DIR, "cona_results.rds"))$limit %>%
  mutate(
    fecha      = as.Date(fecha),
    year       = year(fecha),
    member     = recode(paste0(Sex, "_", Age), !!!MEMBER_LABS),
    member     = factor(member, levels = MEMBER_ORDER),
    ciudad_lbl = city_lbl_map[ciudad],
    ciudad_lbl = factor(ciudad_lbl,
                        levels = c("Bogotá", "Medellín", "Cali"))) %>%
  filter(fecha >= PAPER_START, fecha <= PAPER_END)

# -----------------------------------------------------------------------
# 2. Binding frequency — full period and by year
# -----------------------------------------------------------------------
freq_full <- limit %>%
  group_by(ciudad_lbl, member, Nutrients) %>%
  dplyr::summarise(pct_binding = mean(Limiting, na.rm = TRUE) * 100,
                   .groups = "drop") %>%
  mutate(Period = "2019–2024")

freq_annual <- limit %>%
  group_by(ciudad_lbl, member, Nutrients, year) %>%
  dplyr::summarise(pct_binding = mean(Limiting, na.rm = TRUE) * 100,
                   .groups = "drop") %>%
  mutate(Period = as.character(year)) %>%
  select(-year)

freq_all <- bind_rows(freq_full, freq_annual)

# -----------------------------------------------------------------------
# 3. Drop nutrients with 0% binding in every period/year
# -----------------------------------------------------------------------
nutrients_keep <- freq_all %>%
  group_by(Nutrients) %>%
  dplyr::summarise(max_pct = max(pct_binding, na.rm = TRUE), .groups = "drop") %>%
  filter(max_pct > 0) %>%
  pull(Nutrients)

freq_all <- freq_all %>%
  filter(Nutrients %in% nutrients_keep)

# Order nutrients by descending overall binding frequency (full period)
nutrient_order <- freq_full %>%
  filter(Nutrients %in% nutrients_keep) %>%
  group_by(Nutrients) %>%
  dplyr::summarise(overall = mean(pct_binding, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(overall)) %>%
  pull(Nutrients)

message(sprintf("Kept %d of %d nutrients (dropped %d with 0%% in all periods)",
                length(nutrients_keep),
                n_distinct(limit$Nutrients),
                n_distinct(limit$Nutrients) - length(nutrients_keep)))

# -----------------------------------------------------------------------
# 4. Wide format: rows = city × member × period, cols = nutrients
# -----------------------------------------------------------------------
CITIES   <- c("Bogotá", "Medellín", "Cali")
MEMBERS  <- MEMBER_ORDER
PERIODS  <- c("2019–2024", as.character(2019:2024))

freq_wide <- freq_all %>%
  mutate(Nutrients = factor(Nutrients, levels = nutrient_order),
         pct_fmt   = sprintf("%.1f", pct_binding)) %>%
  select(ciudad_lbl, member, Period, Nutrients, pct_fmt) %>%
  pivot_wider(names_from = Nutrients, values_from = pct_fmt) %>%
  mutate(Period = factor(Period, levels = PERIODS)) %>%
  arrange(ciudad_lbl, member, Period)

# -----------------------------------------------------------------------
# 5. LaTeX output
# -----------------------------------------------------------------------
write_latex_binding <- function(data_wide, nutrients, panel_title, label_suffix) {
  n_nut    <- length(nutrients)
  col_spec <- paste0("lllr", strrep("r", n_nut - 1))
  header   <- paste(nutrients, collapse = " & ")
  
  lines <- c(
    "\\begin{table}[htbp]",
    "\\centering",
    "\\begingroup",
    "\\fontsize{7}{8.5}\\selectfont",
    "\\setlength{\\tabcolsep}{3.5pt}",
    "\\renewcommand{\\arraystretch}{0.95}",
    sprintf("\\caption{%s}", panel_title),
    sprintf("\\label{tab:%s}", label_suffix),
    sprintf("\\begin{tabular}{%s}", col_spec),
    "\\toprule",
    sprintf("City & Member & Period & %s \\\\", header),
    "\\midrule")
  
  for (city in CITIES) {
    for (mem in MEMBERS) {
      sub <- data_wide %>% filter(ciudad_lbl == city, member == mem)
      if (nrow(sub) == 0) next
      lines <- c(lines,
                 sprintf("\\multicolumn{%d}{l}{\\textit{%s --- %s}} \\\\",
                         n_nut + 3, city, mem))
      for (i in seq_len(nrow(sub))) {
        r    <- sub[i, ]
        vals <- paste(sapply(nutrients, function(n) {
          v <- r[[n]]
          if (is.na(v)) "--" else v
        }), collapse = " & ")
        lines <- c(lines,
                   sprintf(" & & %s & %s \\\\", as.character(r$Period), vals))
      }
      lines <- c(lines, "\\addlinespace[2pt]")
    }
  }
  
  lines <- c(lines,
             "\\bottomrule",
             "\\end{tabular}",
             "\\vspace{0.5em}",
             "\\noindent{\\scriptsize \\textit{Note:} Cells report the share of months ",
             "in each period where the nutrient constraint was binding ",
             "(Limiting = 1) in the CoNA linear program. Nutrients with 0\\% binding ",
             "frequency in every period and city are omitted.}",
             "\\endgroup",
             "\\end{table}")
  
  lines
}

tex_binding <- write_latex_binding(
  freq_wide, nutrient_order,
  "Binding frequency of limiting nutrients by city and household member, 2019\\textendash{}2024 (\\% of months)",
  "binding_frequency")

writeLines(tex_binding,
           file.path(TAB_DIR, "final", "tab03_binding_frequency.tex"))

# -----------------------------------------------------------------------
# 6. Excel output
# -----------------------------------------------------------------------
wb <- createWorkbook()
addWorksheet(wb, "Binding frequency")

title <- "Table 3. Binding frequency of limiting nutrients (% of months)"
n_cols <- length(nutrient_order) + 3

writeData(wb, "Binding frequency", title, startRow = 1, startCol = 1)
mergeCells(wb, "Binding frequency", rows = 1, cols = 1:n_cols)
addStyle(wb, "Binding frequency",
         createStyle(fontSize = 11, textDecoration = "bold", wrapText = TRUE),
         rows = 1, cols = 1, gridExpand = TRUE)
setRowHeights(wb, "Binding frequency", rows = 1, heights = 28)

writeData(wb, "Binding frequency", freq_wide, startRow = 2, colNames = TRUE)

addStyle(wb, "Binding frequency",
         createStyle(textDecoration = "bold", fgFill = "#E8EAF0",
                     border = "TopBottomLeftRight", halign = "center"),
         rows = 2, cols = 1:n_cols, gridExpand = TRUE)

# Shading by city-member block
row_idx <- 3
for (city in CITIES) {
  for (mem in MEMBERS) {
    n_rows <- nrow(freq_wide %>% filter(ciudad_lbl == city, member == mem))
    if (n_rows == 0) next
    bg <- if ((match(city, CITIES) + match(mem, MEMBERS)) %% 2 == 0) "#FFFFFF" else "#F7F8FC"
    addStyle(wb, "Binding frequency",
             createStyle(fgFill = bg, border = "TopBottomLeftRight",
                         borderColour = "#DDDDDD"),
             rows = row_idx:(row_idx + n_rows - 1),
             cols = 1:n_cols, gridExpand = TRUE)
    row_idx <- row_idx + n_rows
  }
}

note_row <- nrow(freq_wide) + 3
mergeCells(wb, "Binding frequency", rows = note_row, cols = 1:n_cols)
writeData(wb, "Binding frequency",
          paste0("Note: Cells report the share of months in each period where the ",
                 "nutrient constraint was binding (Limiting = 1) in the CoNA linear ",
                 "program. Nutrients with 0% binding frequency in every period and ",
                 "city are omitted."),
          startRow = note_row, startCol = 1)
addStyle(wb, "Binding frequency",
         createStyle(fontSize = 9, textDecoration = "italic", wrapText = TRUE),
         rows = note_row, cols = 1, gridExpand = TRUE)
setRowHeights(wb, "Binding frequency", rows = note_row, heights = 35)

setColWidths(wb, "Binding frequency",
             cols = 1:n_cols,
             widths = c(10, 14, 10, rep(11, length(nutrient_order))))

saveWorkbook(wb,
             file.path(TAB_DIR, "final", "tab03_binding_frequency.xlsx"),
             overwrite = TRUE)

message("Table 3 saved — xlsx + tex.")