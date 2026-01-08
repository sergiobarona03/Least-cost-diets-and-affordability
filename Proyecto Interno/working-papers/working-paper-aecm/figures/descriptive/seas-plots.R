###################################################
## Seasonal component plots (X-13 / seasonal)    ##
## Retail (IPC) vs Wholesale (SIPSA)              ##
## Facet 7 x 3 (foods x cities)                   ##
## NO interpolation (assumes balanced monthly TS) ##
###################################################

library(lubridate)
library(tidyverse)
library(readxl)
library(seasonal)

# -------------------------------------------------------------------
# Paths
# -------------------------------------------------------------------
wd_path <- "C:\\Users\\sergio.barona\\Desktop\\Least-cost-diets-and-affordability\\Proyecto Interno\\"
setwd(wd_path)

date_tag <- "261225"

path_in <- file.path(
  "working-papers", "working-paper-aecm", "input",
  paste0(date_tag, "_selected_foods_dataset.xlsx")
)

out_dir <- file.path(
  "working-papers", "working-paper-aecm", "output",
  "ts-output", "seasonal-plots"
)
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# -------------------------------------------------------------------
# Load data
# -------------------------------------------------------------------
dataset <- read_excel(path_in) %>% na.omit()

# -------------------------------------------------------------------
# Map cod_mun to city names + monthly date
# -------------------------------------------------------------------
dataset <- dataset %>%
  mutate(
    cod_mun_chr = as.character(cod_mun),
    cod_mun_chr = if_else(nchar(cod_mun_chr) < 5,
                          str_pad(cod_mun_chr, 5, pad = "0"),
                          cod_mun_chr),
    city = case_when(
      cod_mun_chr == "76001" ~ "Cali",
      cod_mun_chr == "05001" ~ "Medellín",
      cod_mun_chr == "11001" ~ "Bogotá",
      TRUE ~ cod_mun_chr
    ),
    date = as.Date(sprintf("%d-%02d-01", Year, Month))
  )

# -------------------------------------------------------------------
# Long dataset (two series)
# -------------------------------------------------------------------
plot_df <- dataset %>%
  select(city, date, articulo_ipc, precio_ipc, precio_sipsa) %>%
  pivot_longer(
    cols = c(precio_ipc, precio_sipsa),
    names_to = "series",
    values_to = "price"
  ) %>%
  mutate(
    series = recode(series,
                    precio_ipc   = "Retail (IPC)",
                    precio_sipsa = "Wholesale (SIPSA)"
    ),
    series = factor(series, levels = c("Retail (IPC)", "Wholesale (SIPSA)")),
    price = as.numeric(price),
    log_price = log(price)
  ) %>%
  filter(is.finite(log_price))

# -------------------------------------------------------------------
# Enforce facet order: 7 foods x 3 cities
# -------------------------------------------------------------------
foods_order <- c(
  "ARROZ PARA SECO",
  "CEBOLLA CABEZONA",
  "PAPA",
  "PLÁTANO",
  "TOMATE",
  "YUCA",
  "ZANAHORIA"
)

cities_order <- c("Cali", "Medellín", "Bogotá")

plot_df <- plot_df %>%
  mutate(
    articulo_ipc = factor(articulo_ipc, levels = foods_order),
    city = factor(city, levels = cities_order)
  )

# -------------------------------------------------------------------
# Function: extract X-13 seasonal component (no interpolation)
# Assumes:
#   - No missing dates within each group
#   - Monthly frequency = 12
# -------------------------------------------------------------------
get_seasonal_x13 <- function(df) {
  df <- df %>% arrange(date)
  
  start_ym <- c(year(min(df$date)), month(min(df$date)))
  ts_x <- ts(df$log_price, start = start_ym, frequency = 12)
  
  seas_fit <- try(
    seas(
      ts_x,
      x11 = "",
      transform.function = "none",
      regression.aictest = NULL,
      outlier = NULL
    ),
    silent = TRUE
  )
  
  if (inherits(seas_fit, "try-error")) {
    df$seasonal <- NA_real_
    return(df)
  }
  
  df$seasonal <- as.numeric(seas_fit$data[, "seasonal"])
  df
}

# -------------------------------------------------------------------
# Compute seasonal component for each city × food × series
# -------------------------------------------------------------------
seasonal_df <- plot_df %>%
  group_by(city, articulo_ipc, series) %>%
  group_modify(~ get_seasonal_x13(.x)) %>%
  ungroup()

# -------------------------------------------------------------------
# Plot: seasonal components (Retail vs Wholesale)
# -------------------------------------------------------------------
series_cols <- c(
  "Retail (IPC)"      = "#1F78B4",
  "Wholesale (SIPSA)" = "#E31A1C"
)

p_seas <- ggplot(seasonal_df, aes(x = date, y = seasonal, color = series)) +
  geom_hline(yintercept = 0, linewidth = 0.25, alpha = 0.6) +
  geom_line(linewidth = 0.55, alpha = 0.95) +
  facet_grid(articulo_ipc ~ city, scales = "free_y") +
  scale_color_manual(values = series_cols) +
  labs(
    x = NULL,
    y = "Seasonal component (log points)",
    color = NULL
  ) +
  theme_bw(base_size = 10) +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 9),
    strip.text = element_text(size = 9, face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(linewidth = 0.25),
    axis.text.x = element_text(size = 7),
    axis.text.y = element_text(size = 7),
    plot.margin = margin(8, 8, 8, 8)
  )

print(p_seas)

# -------------------------------------------------------------------
# Save outputs
# -------------------------------------------------------------------
out_png <- file.path(out_dir, paste0(date_tag, "_seasonal_retail_vs_wholesale_7x3_x13.png"))
out_pdf <- file.path(out_dir, paste0(date_tag, "_seasonal_retail_vs_wholesale_7x3_x13.pdf"))

ggsave(filename = out_png, plot = p_seas, width = 11, height = 13, dpi = 300)
ggsave(filename = out_pdf, plot = p_seas, width = 11, height = 13)
