
###############################################################
## Export wide (EViews-ready) datasets:
## 1) WHOLESALE (SIPSA): columns = alimento_sipsa, plus date
## 2) RETAIL (IPC):      columns = articulo_ipc, plus date
## One file per city (recommended for EViews), saved as .xlsx
###############################################################

library(tidyverse)
library(readxl)
library(writexl)
library(lubridate)

setwd("C:\\Users\\sergio.barona\\Desktop\\Least-cost-diets-and-affordability\\Proyecto Interno\\")

date_tag <- "261225"

# -----------------------------
# Input
# -----------------------------
path.in <- paste0("working-papers\\working-paper-aecm\\input\\",
                  date_tag, "_selected_foods_dataset.xlsx")

df0 <- read_excel(path.in)

# -----------------------------
# Output folder
# -----------------------------
out_dir <- "working-papers\\working-paper-aecm\\output\\ts-output\\"

# -----------------------------
# Prepare + collapse duplicates to monthly series
# -----------------------------
df <- df0 %>%
  mutate(
    cod_mun = sprintf("%05d", as.integer(cod_mun)),
    Year = as.integer(Year),
    Month = as.integer(Month),
    date = as.Date(sprintf("%d-%02d-01", Year, Month)),
    articulo_ipc = str_squish(as.character(articulo_ipc)),
    alimento_sipsa = str_squish(as.character(alimento_sipsa)),
    precio_ipc = as.numeric(precio_ipc),
    precio_sipsa = as.numeric(precio_sipsa)
  ) %>%
  filter(!is.na(date))

# Collapse duplicates: one value per city-month-item (mean)
wholesale_long <- df %>%
  group_by(cod_mun, date, alimento_sipsa) %>%
  summarise(precio_sipsa = mean(precio_sipsa, na.rm = TRUE), .groups = "drop") %>%
  filter(is.finite(precio_sipsa))

retail_long <- df %>%
  group_by(cod_mun, date, articulo_ipc) %>%
  summarise(precio_ipc = mean(precio_ipc, na.rm = TRUE), .groups = "drop") %>%
  filter(is.finite(precio_ipc))

# Cities and labels
cities_required <- c("76001", "11001", "05001")
city_labs <- c("76001"="Cali", "11001"="Bogotá", "05001"="Medellín")

wholesale_long <- wholesale_long %>% filter(cod_mun %in% cities_required)
retail_long    <- retail_long    %>% filter(cod_mun %in% cities_required)

# -----------------------------
# Helper: clean column names (EViews-friendly)
# - uppercase
# - remove accents and special chars
# - replace spaces with underscore
# - keep letters/numbers/underscore only
# - avoid starting with a number
# -----------------------------
clean_name <- function(x) {
  x %>%
    toupper() %>%
    iconv(from = "UTF-8", to = "ASCII//TRANSLIT") %>%
    str_replace_all("[^A-Z0-9]+", "_") %>%
    str_replace_all("_+", "_") %>%
    str_replace_all("^_|_$", "") %>%
    (\(z) ifelse(str_detect(z, "^[0-9]"), paste0("X_", z), z))()
}

# -----------------------------
# Build and save 2 files per city (recommended for EViews)
#  - WHOLESALE_city.xlsx: date + columns per alimento_sipsa
#  - RETAIL_city.xlsx:    date + columns per articulo_ipc
# -----------------------------
for (c in cities_required) {
  
  city_name <- city_labs[[c]]
  
  # WHOLESALE wide
  w_wide <- wholesale_long %>%
    filter(cod_mun == c) %>%
    mutate(food_col = clean_name(alimento_sipsa)) %>%
    select(date, food_col, precio_sipsa) %>%
    pivot_wider(names_from = food_col, values_from = precio_sipsa) %>%
    arrange(date)
  
  # RETAIL wide
  r_wide <- retail_long %>%
    filter(cod_mun == c) %>%
    mutate(food_col = clean_name(articulo_ipc)) %>%
    select(date, food_col, precio_ipc) %>%
    pivot_wider(names_from = food_col, values_from = precio_ipc) %>%
    arrange(date)
  
  # Save
  out_wh <- file.path(out_dir, paste0(date_tag, "_WHOLESALE_", city_name, ".xlsx"))
  out_rt <- file.path(out_dir, paste0(date_tag, "_RETAIL_", city_name, ".xlsx"))
  
  write_xlsx(w_wide, out_wh)
  write_xlsx(r_wide, out_rt)
  
  cat("\nSaved:\n", out_wh, "\n", out_rt, "\n")
}

# -----------------------------
# OPTIONAL: also save "all cities in one file" (multiple sheets)
# (Sometimes convenient, but EViews users often prefer one file per city)
# -----------------------------
wholesale_sheets <- list()
retail_sheets <- list()

for (c in cities_required) {
  city_name <- city_labs[[c]]
  
  wholesale_sheets[[city_name]] <- wholesale_long %>%
    filter(cod_mun == c) %>%
    mutate(food_col = clean_name(alimento_sipsa)) %>%
    select(date, food_col, precio_sipsa) %>%
    pivot_wider(names_from = food_col, values_from = precio_sipsa) %>%
    arrange(date)
  
  retail_sheets[[city_name]] <- retail_long %>%
    filter(cod_mun == c) %>%
    mutate(food_col = clean_name(articulo_ipc)) %>%
    select(date, food_col, precio_ipc) %>%
    pivot_wider(names_from = food_col, values_from = precio_ipc) %>%
    arrange(date)
}

write_xlsx(wholesale_sheets, file.path(out_dir, paste0(date_tag,
                                                       "_WHOLESALE_allcities.xlsx")))
write_xlsx(retail_sheets,   file.path(out_dir, paste0(date_tag,
                                                      "_RETAIL_allcities.xlsx")))

