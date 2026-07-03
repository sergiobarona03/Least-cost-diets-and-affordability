########################################################
## 05_figures/bloque4_affordability/fig08_tab04_affordability.R
##
## Figure 8: Unaffordability rate heatmap by decile × MONTH
##   Facet: model (cols) × city (rows)
##   X: month (2019:1 - 2024:12) | Y: decile | Fill: % unaffordable
##
## Table 4: Aggregate unaffordability rate by city, model, and year
##   Cells: mean (SD) across monthly observations within each year/decile
##   Aggregate = simple mean across deciles (equal population weight)
##
## Reads:  AFFORD_DIR/afford_results.xlsx
##
## Writes: FIG_DIR/final/fig08_afford_deciles.png / .pdf
##         TAB_DIR/final/tab04_afford_annual.xlsx / .tex
########################################################

source("00_config.R")
source(file.path(REVIEW_DIR, "06_figures", "00_fig_config.R"))
library(tidyverse)
library(readxl)
library(scales)
library(lubridate)
library(openxlsx)

# -----------------------------------------------------------------------
# 1. Load and clean
# -----------------------------------------------------------------------
afford <- read_excel(file.path(AFFORD_DIR, "afford_results.xlsx")) %>%
  mutate(
    fecha      = as.Date(fecha),
    ciudad_lbl = case_when(
      grepl("BOGOT", ciudad, ignore.case = TRUE) ~ "Bogotá",
      grepl("MEDEL", ciudad, ignore.case = TRUE) ~ "Medellín",
      grepl("CALI",  ciudad, ignore.case = TRUE) ~ "Cali",
      TRUE ~ ciudad),
    ciudad_lbl = factor(ciudad_lbl,
                        levels = c("Bogotá", "Medellín", "Cali")),
    model      = factor(model, levels = c("CoCA", "CoNA", "CoRD")),
    deciles    = factor(deciles,
                        levels = paste0("Decil ", 1:10))) %>%
  filter(fecha >= PAPER_START, fecha <= PAPER_END)

CITIES  <- c("Bogotá", "Medellín", "Cali")
MODELS  <- c("CoCA", "CoNA", "CoRD")
PERIODS <- c("2019–2024", as.character(2019:2024))

# Year boundary dates for vertical lines on the monthly heatmap
year_lines <- as.Date(paste0(2020:2024, "-01-01"))

########################################################
## PART A — FIGURE 8: heatmap by decile × MONTH
########################################################

# -----------------------------------------------------------------------
# A1. Monthly mean by decile × city × model (already monthly — no collapse)
# -----------------------------------------------------------------------
afford_monthly <- afford %>%
  group_by(deciles, ciudad_lbl, model, fecha) %>%
  dplyr::summarise(
    rate = mean(rate, na.rm = TRUE),
    .groups = "drop")

message(sprintf("  %d rows | months: %d | deciles: %s",
                nrow(afford_monthly),
                n_distinct(afford_monthly$fecha),
                paste(levels(afford_monthly$deciles), collapse = ", ")))

# -----------------------------------------------------------------------
# A2. Figure — heatmap, monthly frequency
# -----------------------------------------------------------------------
fig8 <- ggplot(afford_monthly,
               aes(x = fecha, y = deciles, fill = rate)) +
  geom_tile(color = NA) +
  geom_vline(xintercept = as.numeric(year_lines),
             color = "grey30", linewidth = 0.3,
             linetype = "dashed", alpha = 0.6) +
  facet_grid(ciudad_lbl ~ model) +
  scale_fill_gradient(
    low      = "#FFF7EC",
    high     = "#C0392B",
    limits   = c(0, 100),
    name     = "% households\nunaffordable") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y",
               expand = c(0.01, 0)) +
  scale_y_discrete(labels = 1:10) +
  labs(
    title    = " ",
    subtitle = " ",
    caption  = paste0(
      "Note: Unaffordability rate = share of households in each decile ",
      "whose monthly food expenditure per capita is below the monthly diet cost (FGT0).\n",
      "CoCA = Cost of Caloric Adequacy; CoNA = Cost of Nutritional Adequacy; ",
      "CoRD = Cost of Recommended Diet. Dashed vertical lines mark year boundaries."),
    x = NULL,
    y = "Income decile") +
  paper_theme() +
  theme(
    axis.text.x        = element_text(angle = 45, hjust = 1, size = 8),
    axis.text.y        = element_text(size = 8),
    strip.text         = element_text(face = "bold", size = 9),
    legend.position     = "bottom",
    legend.direction    = "horizontal",
    legend.key.width    = unit(1.4, "cm"),
    legend.key.height   = unit(0.4, "cm"),
    legend.text         = element_text(family = "serif", size = 9),
    legend.title        = element_text(family = "serif", size = 9),
    legend.background   = element_rect(color = "black", fill = "white",
                                       linewidth = 0.5),
    legend.margin       = margin(5, 10, 5, 10),
    panel.grid          = element_blank(),
    panel.spacing       = unit(0.25, "cm"))

ggsave(file.path(FIG_DIR, "final", "fig08_afford_deciles.png"),
       fig8, width = 14, height = 9, dpi = 300, bg = "white")
ggsave(file.path(FIG_DIR, "final", "fig08_afford_deciles.pdf"),
       fig8, width = 14, height = 9)

message("Figure 8 (monthly heatmap) saved.")

########################################################
## PART B — TABLE 4: aggregate annual rate, mean (SD)
########################################################

# -----------------------------------------------------------------------
# B1. Aggregate rate per month = simple mean across deciles (equal weight)
# -----------------------------------------------------------------------
agg_monthly <- afford %>%
  mutate(year = year(fecha)) %>%
  group_by(ciudad_lbl, model, fecha, year) %>%
  dplyr::summarise(agg_rate = mean(rate, na.rm = TRUE), .groups = "drop")

# -----------------------------------------------------------------------
# B2. Summary function: mean (SD) formatted
# -----------------------------------------------------------------------
fmt_mean_sd <- function(x, digits = 1) {
  m <- mean(x, na.rm = TRUE)
  s <- sd(x,   na.rm = TRUE)
  sprintf("%.*f (%.*f)", digits, m, digits, s)
}

# -----------------------------------------------------------------------
# B3. Build summary table — full period + annual
# -----------------------------------------------------------------------
make_panel <- function(data, digits = 1) {
  rows <- list()
  for (city in CITIES) {
    for (period in PERIODS) {
      df <- if (period == "2019–2024") {
        data %>% filter(ciudad_lbl == city)
      } else {
        data %>% filter(ciudad_lbl == city, year == as.integer(period))
      }
      row <- tibble(City = city, Period = period)
      for (m in MODELS) {
        vals <- df %>% filter(model == m) %>% pull(agg_rate)
        row[[m]] <- fmt_mean_sd(vals, digits)
      }
      rows[[length(rows) + 1]] <- row
    }
  }
  bind_rows(rows)
}

afford_panel <- make_panel(agg_monthly, digits = 1)

message(sprintf("Panel built: %d rows", nrow(afford_panel)))

# -----------------------------------------------------------------------
# B4. LaTeX output
# -----------------------------------------------------------------------
write_latex_afford <- function(panel, vars, panel_title, label_suffix) {
  n_vars   <- length(vars)
  col_spec <- paste0("lr", strrep("r", n_vars - 1))
  header   <- paste(vars, collapse = " & ")
  
  lines <- c(
    "\\begin{table}[htbp]",
    "\\centering",
    "\\begingroup",
    "\\fontsize{8}{9.5}\\selectfont",
    "\\setlength{\\tabcolsep}{4pt}",
    "\\renewcommand{\\arraystretch}{0.95}",
    sprintf("\\caption{%s}", panel_title),
    sprintf("\\label{tab:%s}", label_suffix),
    sprintf("\\begin{tabular}{l%s}", col_spec),
    "\\toprule",
    sprintf("Period & %s \\\\", header),
    "\\midrule")
  
  for (city in CITIES) {
    sub <- panel %>% filter(City == city)
    lines <- c(lines,
               sprintf("\\multicolumn{%d}{l}{\\textit{%s}} \\\\",
                       n_vars + 1, city))
    for (i in seq_len(nrow(sub))) {
      r    <- sub[i, ]
      vals <- paste(sapply(vars, function(v) r[[v]]), collapse = " & ")
      lines <- c(lines, sprintf("%s & %s \\\\", r$Period, vals))
    }
    if (city != CITIES[length(CITIES)])
      lines <- c(lines, "\\midrule")
  }
  
  lines <- c(lines,
             "\\bottomrule",
             "\\end{tabular}",
             "\\vspace{0.5em}",
             "\\noindent{\\scriptsize \\textit{Note:} Cells report mean (standard ",
             "deviation) of the aggregate unaffordability rate across monthly ",
             "observations within each period. The aggregate rate is the simple ",
             "mean across the ten income deciles (equal population weight). ",
             "CoCA = Cost of Caloric Adequacy; CoNA = Cost of Nutritional ",
             "Adequacy; CoRD = Cost of Recommended Diet.}",
             "\\endgroup",
             "\\end{table}")
  
  lines
}

tex_afford <- write_latex_afford(
  afford_panel, MODELS,
  paste0("Aggregate household unaffordability rate by city and diet model, ",
         "2019\\textendash{}2024 (\\% of households, mean (SD))"),
  "afford_annual")

writeLines(tex_afford,
           file.path(TAB_DIR, "final", "tab04_afford_annual.tex"))

# -----------------------------------------------------------------------
# B5. Excel output
# -----------------------------------------------------------------------
wb <- createWorkbook()
addWorksheet(wb, "Unaffordability rate")

title  <- "Table 4. Aggregate unaffordability rate by city and diet model, mean (SD)"
n_cols <- length(MODELS) + 1

writeData(wb, "Unaffordability rate", title, startRow = 1, startCol = 1)
mergeCells(wb, "Unaffordability rate", rows = 1, cols = 1:n_cols)
addStyle(wb, "Unaffordability rate",
         createStyle(fontSize = 11, textDecoration = "bold", wrapText = TRUE),
         rows = 1, cols = 1, gridExpand = TRUE)
setRowHeights(wb, "Unaffordability rate", rows = 1, heights = 28)

writeData(wb, "Unaffordability rate", afford_panel, startRow = 2, colNames = TRUE)

addStyle(wb, "Unaffordability rate",
         createStyle(textDecoration = "bold", fgFill = "#E8EAF0",
                     border = "TopBottomLeftRight", halign = "center"),
         rows = 2, cols = 1:n_cols, gridExpand = TRUE)

row_idx <- 3
for (k in seq_along(CITIES)) {
  n_rows <- nrow(afford_panel %>% filter(City == CITIES[k]))
  bg     <- if (k %% 2 == 1) "#FFFFFF" else "#F7F8FC"
  addStyle(wb, "Unaffordability rate",
           createStyle(fgFill = bg, border = "TopBottomLeftRight",
                       borderColour = "#DDDDDD"),
           rows = row_idx:(row_idx + n_rows - 1),
           cols = 1:n_cols, gridExpand = TRUE)
  addStyle(wb, "Unaffordability rate",
           createStyle(fgFill = bg, textDecoration = "bold",
                       border = "TopBottomLeftRight", borderColour = "#DDDDDD"),
           rows = row_idx, cols = 1:n_cols, gridExpand = TRUE)
  row_idx <- row_idx + n_rows
}

note_row <- nrow(afford_panel) + 3
mergeCells(wb, "Unaffordability rate", rows = note_row, cols = 1:n_cols)
writeData(wb, "Unaffordability rate",
          paste0("Note: Mean (SD) of the aggregate unaffordability rate across ",
                 "monthly observations within each period. The aggregate rate ",
                 "is the simple mean across the ten income deciles (equal ",
                 "population weight)."),
          startRow = note_row, startCol = 1)
addStyle(wb, "Unaffordability rate",
         createStyle(fontSize = 9, textDecoration = "italic", wrapText = TRUE),
         rows = note_row, cols = 1, gridExpand = TRUE)
setRowHeights(wb, "Unaffordability rate", rows = note_row, heights = 35)

setColWidths(wb, "Unaffordability rate",
             cols = 1:n_cols, widths = c(12, rep(16, length(MODELS))))

saveWorkbook(wb,
             file.path(TAB_DIR, "final", "tab04_afford_annual.xlsx"),
             overwrite = TRUE)

message("Table 4 saved — xlsx + tex.")