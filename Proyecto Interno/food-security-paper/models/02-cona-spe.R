########################################################
## Cost of Nutritional Adequacy (CoNA)
## Figure 8: Binding constraints heatmap over time
## Table: Binding frequency — LaTeX and Excel
########################################################

library(tidyverse)
library(readxl)
library(scales)
library(writexl)
library(openxlsx)

##----------------------------------------------------------
## Directories and data
##----------------------------------------------------------

dirs <- c(
  "C:/Users/Portatil/Desktop/Least-cost-diets-and-affordability/Proyecto Interno",
  "C:/Users/danie/OneDrive/Escritorio/Least-cost-diets-and-affordability/Proyecto Interno"
)

base_dir <- dirs[dir.exists(dirs)][1]

if (is.na(base_dir)) {
  stop("Ninguno de los directorios existe")
}

out_cona <- file.path(base_dir, "food-security-paper", "output", "cona")
out_fig  <- file.path(base_dir, "food-security-paper", "output", "cona")
out_tabs <- file.path(base_dir, "food-security-paper", "output", "cona")

path_xlsx <- file.path(out_cona, "230326_cona_full.xlsx")

df.limit <- read_excel(path_xlsx, sheet = "limit") %>%
  mutate(fecha = as.Date(fecha))

##----------------------------------------------------------
## Shared labels
##----------------------------------------------------------

city_labels <- c(
  "BOGOTA"   = "Bogotá",
  "MEDELLIN" = "Medellín",
  "CALI"     = "Cali"
)

member_labels <- c(
  "0_[31,51)"  = "Adult male",
  "1_[10, 14)" = "Female child",
  "1_[31,51)"  = "Adult female"
)

member_order <- c("Adult male", "Adult female", "Female child")

##----------------------------------------------------------
## Prepare data
##----------------------------------------------------------

df.limit <- df.limit %>%
  mutate(
    member       = recode(paste0(Sex, "_", Age), !!!member_labels),
    member       = factor(member, levels = member_order),
    ciudad_label = recode(ciudad, !!!city_labels)
  )

# Reorder nutrients by descending binding frequency
# (consistent ordering with SPE heatmap)
nutrient_order <- df.limit %>%
  dplyr::group_by(Nutrients) %>%
  dplyr::summarize(freq = mean(Limiting == 1, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(freq)) %>%
  pull(Nutrients)

df.limit <- df.limit %>%
  mutate(Nutrients = factor(Nutrients, levels = nutrient_order))

##----------------------------------------------------------
## Figure 8: Binary binding heatmap — time × nutrient
##           facet: city (rows) × member (columns)
##----------------------------------------------------------

fig8 <- ggplot(df.limit,
               aes(x = fecha, y = Nutrients, fill = factor(Limiting))) +
  geom_tile(color = NA) +
  facet_grid(ciudad_label ~ member) +
  scale_fill_manual(
    values = c("0" = "grey92", "1" = "#C0392B"),
    labels = c("0" = "Not binding", "1" = "Binding"),
    name   = NULL
  ) +
  scale_x_date(
    date_breaks = "1 year",
    date_labels = "%Y",
    expand      = c(0, 0)
  ) +
  labs(
    title    = "Figure 8. Monthly binding nutritional constraints by household member and city, 2019–2024",
    subtitle = "Red cells indicate months in which the constraint is binding for the cost-minimising diet",
    caption  = paste0(
      "Note: A constraint is binding when the cost-minimising diet exactly meets — but does not exceed —\n",
      "the minimum nutritional requirement. Nutrients are ordered by descending overall binding frequency.\n",
      "Grey cells indicate non-binding constraints."
    ),
    x = NULL,
    y = NULL
  ) +
  theme_minimal(base_size = 10) +
  theme(
    text              = element_text(family = "serif"),
    plot.title        = element_text(face = "bold", size = 12),
    plot.subtitle     = element_text(size = 10, color = "grey40"),
    plot.caption      = element_text(size = 8, color = "grey50", hjust = 0),
    axis.text.x       = element_text(size = 8, angle = 45, hjust = 1),
    axis.text.y       = element_text(size = 8),
    strip.text.x      = element_text(face = "bold", size = 10),
    strip.text.y      = element_text(face = "bold", size = 10),
    legend.position   = "bottom",
    legend.key.size   = unit(0.5, "cm"),
    panel.grid        = element_blank(),
    panel.spacing     = unit(0.3, "cm")
  )

ggsave(file.path(out_fig, "fig8_binding_heatmap_time.png"),
       fig8, width = 14, height = 8, dpi = 300)

message("Figure 8 saved to: ", out_fig)

##----------------------------------------------------------
## Summary table: binding frequency by nutrient × member × city
##----------------------------------------------------------

tab_binding <- df.limit %>%
  dplyr::group_by(Nutrients, member, ciudad_label) %>%
  dplyr::summarize(
    freq     = mean(Limiting == 1, na.rm = TRUE),
    n_months = sum(Limiting == 1, na.rm = TRUE),
    .groups  = "drop"
  ) %>%
  mutate(freq_pct = round(freq * 100, 1)) %>%
  select(Nutrients, member, ciudad_label, freq_pct, n_months)

# Wide format: columns = city × member
tab_wide <- tab_binding %>%
  unite("col", member, ciudad_label, sep = " — ") %>%
  pivot_wider(names_from = col, values_from = freq_pct) %>%
  arrange(match(Nutrients, levels(df.limit$Nutrients)))

##----------------------------------------------------------
## Excel table
##----------------------------------------------------------

HEADER   <- "1A1A2E"
SUBHEAD  <- "E8EAF0"
WHITE    <- "FFFFFF"
LIGHT    <- "F7F8FC"
RED_HIGH <- "FADBD8"   # light red for high binding frequency

wb <- createWorkbook()
addWorksheet(wb, "Binding frequency")

# Title
mergeCells(wb, "Binding frequency", rows = 1, cols = 1:(ncol(tab_wide)))
writeData(wb, "Binding frequency",
          "Table A1. Frequency of binding nutritional constraints by household member and city, 2019–2024 (%)",
          startRow = 1, startCol = 1)
addStyle(wb, "Binding frequency",
         style = createStyle(fontSize = 11, fontColour = WHITE,
                             fgFill = paste0("#", HEADER),
                             textDecoration = "bold",
                             halign = "center", valign = "center",
                             wrapText = TRUE),
         rows = 1, cols = 1:ncol(tab_wide), gridExpand = TRUE)
setRowHeights(wb, "Binding frequency", rows = 1, heights = 30)

# Column headers
writeData(wb, "Binding frequency", tab_wide, startRow = 2,
          colNames = TRUE, rowNames = FALSE)

# Header style
addStyle(wb, "Binding frequency",
         style = createStyle(fontSize = 10, fontColour = paste0("#", HEADER),
                             fgFill = paste0("#", SUBHEAD),
                             textDecoration = "bold",
                             halign = "center", valign = "center",
                             wrapText = TRUE, border = "TopBottomLeftRight",
                             borderColour = "#CCCCCC"),
         rows = 2, cols = 1:ncol(tab_wide), gridExpand = TRUE)

# Data rows
for (i in seq_len(nrow(tab_wide))) {
  r  <- i + 2
  bg <- if (i %% 2 == 0) WHITE else LIGHT
  
  # Nutrient name column
  addStyle(wb, "Binding frequency",
           style = createStyle(fontSize = 10, fgFill = paste0("#", bg),
                               halign = "left", border = "TopBottomLeftRight",
                               borderColour = "#CCCCCC"),
           rows = r, cols = 1)
  
  # Numeric columns — highlight high frequency (>= 75%)
  for (j in 2:ncol(tab_wide)) {
    val    <- tab_wide[[i, j]]
    cell_bg <- if (!is.na(val) && val >= 75) RED_HIGH else paste0("#", bg)
    addStyle(wb, "Binding frequency",
             style = createStyle(fontSize = 10, fgFill = cell_bg,
                                 halign = "right", numFmt = "0.0",
                                 border = "TopBottomLeftRight",
                                 borderColour = "#CCCCCC"),
             rows = r, cols = j)
  }
}

# Note row
note_row <- nrow(tab_wide) + 3
mergeCells(wb, "Binding frequency", rows = note_row, cols = 1:ncol(tab_wide))
writeData(wb, "Binding frequency",
          paste0("Note: Values represent the percentage of city–month observations (out of 72 months, ",
                 "January 2019–December 2024) in which each nutritional lower bound is binding for the ",
                 "cost-minimising diet. Pink cells indicate binding frequency ≥75%. ",
                 "CoNA = Cost of Nutritional Adequacy."),
          startRow = note_row, startCol = 1)
addStyle(wb, "Binding frequency",
         style = createStyle(fontSize = 9, fontColour = "#555555",
                             italic = TRUE, wrapText = TRUE, halign = "left"),
         rows = note_row, cols = 1:ncol(tab_wide), gridExpand = TRUE)
setRowHeights(wb, "Binding frequency", rows = note_row, heights = 48)

# Column widths
setColWidths(wb, "Binding frequency", cols = 1, widths = 16)
setColWidths(wb, "Binding frequency", cols = 2:ncol(tab_wide), widths = 18)

saveWorkbook(wb, file.path(out_tabs, "tableA1_binding_frequency.xlsx"),
             overwrite = TRUE)

