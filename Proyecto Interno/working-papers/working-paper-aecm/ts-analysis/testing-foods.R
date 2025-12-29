#############################################################
## Prueba: Trayectoria temporal (retail)                    ##
## CEBOLLA CABEZONA, TOMATE, ZANAHORIA                      ##
## Objetivo: inspección gráfica (posible no-unit-root)      ##
## Guarda gráficos en: working-papers/.../output/ts-output   ##
#############################################################

# Librerías
library(tidyverse)
library(readxl)
library(lubridate)

# Directorio de trabajo
setwd("C:\\Users\\sergio.barona\\Desktop\\Least-cost-diets-and-affordability\\Proyecto Interno\\")

date_tag <- "261225"

# ---------------------------
# Input and output
# ---------------------------
path.in <- paste0("working-papers\\working-paper-aecm\\input\\",
                  date_tag, "_selected_foods_dataset.xlsx")

out_dir <- "working-papers\\working-paper-aecm\\output\\ts-output\\"
# ---------------------------
# Load dataset
# ---------------------------
df0 <- read_excel(path.in) %>%
  mutate(
    cod_mun = cod_mun,
    Year = as.integer(Year),
    Month = as.integer(Month),
    date = as.Date(sprintf("%d-%02d-01", Year, Month)),
    articulo_ipc = str_squish(as.character(articulo_ipc)),
    precio_ipc = as.numeric(precio_ipc)
  ) %>%
  filter(!is.na(precio_ipc), !is.na(articulo_ipc), !is.na(date))

# ---------------------------
# Target foods (retail)
# Use exact matches if they exist; otherwise use pattern matching.
# ---------------------------
targets_exact <- c("CEBOLLA CABEZONA", "TOMATE", "ZANAHORIA")

# If your labels differ (e.g., "CEBOLLA CABEZONA BLANCA"), pattern helps:
targets_pattern <- c("CEBOLLA", "TOMATE", "ZANAHORIA")

# City labels
city_labs <- c("76001" = "Cali", "11001" = "Bogotá", "05001" = "Medellín")

# ---------------------------
# Filter retail series for targets
# ---------------------------
df_tar <- df0 %>%
  mutate(articulo_ipc_up = toupper(articulo_ipc)) %>%
  filter(
    articulo_ipc_up %in% targets_exact |
      str_detect(articulo_ipc_up, paste(targets_pattern, collapse = "|"))
  ) %>%
  mutate(
    city = recode(cod_mun, !!!city_labs)
  )

# ---------------------------
# Collapse duplicates:
# one price per (city, month, articulo_ipc)
# ---------------------------
retail_m <- df_tar %>% select(cod_mun, city,
                              date, 
                              Year, Month, 
                              articulo_ipc, 
                              precio_ipc) %>% distinct() 

# ---------------------------
# Quick checks
# ---------------------------


# Coverage table (how many months per city-food)
coverage_check <- retail_m %>%
  group_by(city, articulo_ipc) %>%
  summarise(
    n_months = n(),
    start = min(date),
    end   = max(date),
    .groups = "drop"
  ) %>%
  arrange(articulo_ipc, city)

print(coverage_check)

# ---------------------------
# 1) Plot in levels (most important)
# Facet by food; lines by city
# ---------------------------
p_levels <- ggplot(retail_m, 
                   aes(x = date, y = precio_ipc, group = city, linetype = city)) +
  geom_line(linewidth = 0.7) +
  facet_wrap(~ articulo_ipc, scales = "free_y", ncol = 1) +
  labs(
    title = "Retail price trajectories (levels)",
    subtitle = "CEBOLLA CABEZONA / TOMATE / ZANAHORIA (monthly, collapsed mean within city-month)",
    x = NULL, y = "precio_ipc (nivel)",
    linetype = "City"
  ) +
  theme_minimal(base_size = 12)

ggsave(
  filename = file.path(out_dir, paste0(date_tag, "_retail_levels_cebolla_tomate_zanahoria.png")),
  plot = p_levels, width = 11, height = 8, dpi = 300
)

# ---------------------------
# 2) Plot in logs (helps see multiplicative growth + variance)
# (only if positive prices)
# ---------------------------
retail_m2 <- retail_m %>% filter(precio_ipc > 0) %>% mutate(log_precio = log(precio_ipc))

p_logs <- ggplot(retail_m2, aes(x = date, y = log_precio, group = city, linetype = city)) +
  geom_line(linewidth = 0.7) +
  facet_wrap(~ articulo_ipc, scales = "free_y", ncol = 1) +
  labs(
    title = "Retail price trajectories (log)",
    subtitle = "Useful to check trend stability and changes in volatility",
    x = NULL, y = "log(precio_ipc)",
    linetype = "City"
  ) +
  theme_minimal(base_size = 12)

ggsave(
  filename = file.path(out_dir, paste0(date_tag, "_retail_log_cebolla_tomate_zanahoria.png")),
  plot = p_logs, width = 11, height = 8, dpi = 300
)

# ---------------------------
# 3) First differences (Δp) and log-differences (Δlog p)
# This is just to visually see whether levels look stationary vs trending.
# ---------------------------
retail_diff <- retail_m %>%
  group_by(cod_mun, city, articulo_ipc) %>%
  arrange(date) %>%
  mutate(
    d_precio = precio_ipc - lag(precio_ipc),
    d_log_precio = if_else(precio_ipc > 0 & lag(precio_ipc) > 0,
                           log(precio_ipc) - log(lag(precio_ipc)),
                           NA_real_)
  ) %>%
  ungroup()

p_dlevels <- ggplot(retail_diff, aes(x = date, y = d_precio, group = city, linetype = city)) +
  geom_hline(yintercept = 0, linewidth = 0.4) +
  geom_line(linewidth = 0.7) +
  facet_wrap(~ articulo_ipc, scales = "free_y", ncol = 1) +
  labs(
    title = "Retail price trajectories (first differences)",
    subtitle = "Δ precio_ipc (monthly changes) — helps see mean reversion vs drift",
    x = NULL, y = "Δ precio_ipc",
    linetype = "City"
  ) +
  theme_minimal(base_size = 12)

ggsave(
  filename = file.path(out_dir, paste0(date_tag, "_retail_dlevels_cebolla_tomate_zanahoria.png")),
  plot = p_dlevels, width = 11, height = 8, dpi = 300
)

p_dlogs <- ggplot(retail_diff, aes(x = date, y = d_log_precio, group = city, linetype = city)) +
  geom_hline(yintercept = 0, linewidth = 0.4) +
  geom_line(linewidth = 0.7) +
  facet_wrap(~ articulo_ipc, scales = "free_y", ncol = 1) +
  labs(
    title = "Retail price trajectories (log differences)",
    subtitle = "Δ log(precio_ipc) — approximate monthly inflation rate for each item",
    x = NULL, y = "Δ log(precio_ipc)",
    linetype = "City"
  ) +
  theme_minimal(base_size = 12)

ggsave(
  filename = file.path(out_dir, paste0(date_tag, "_retail_dlog_cebolla_tomate_zanahoria.png")),
  plot = p_dlogs, width = 11, height = 8, dpi = 300
)

