###################################################
## Log plots: Retail (IPC) vs Wholesale (SIPSA)   ##
## Facet 7 x 3 (foods x cities)                   ##
## "Fancier" version: colors + cleaner theme      ##
###################################################

library(lubridate)
library(tidyverse)
library(readxl)

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
  "ts-output", "log-levels-plots"
)
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# -------------------------------------------------------------------
# Load data
# -------------------------------------------------------------------
dataset <- read_excel(path_in) %>% na.omit()

# -------------------------------------------------------------------
# Helpers: map cod_mun to city names (robust to numeric/character codes)
# -------------------------------------------------------------------
dataset <- dataset %>%
  mutate(
    cod_mun_chr = case_when(
      is.na(cod_mun) ~ NA_character_,
      TRUE ~ as.character(cod_mun)
    ),
    cod_mun_chr = if_else(nchar(cod_mun_chr) < 5, str_pad(cod_mun_chr, 5, pad = "0"), cod_mun_chr),
    city = case_when(
      cod_mun_chr == "76001" ~ "Cali",
      cod_mun_chr == "05001" ~ "Medellín",
      cod_mun_chr == "11001" ~ "Bogotá",
      TRUE ~ cod_mun_chr
    ),
    date = as.Date(sprintf("%d-%02d-01", Year, Month))
  )

# -------------------------------------------------------------------
# Prepare long dataset: two series per panel
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
# Plot: colors + better legend + subtle grid + nicer strips
# -------------------------------------------------------------------
# Colors chosen to print well (including grayscale-friendly contrast)
series_cols <- c(
  "Retail (IPC)"       = "#1F78B4",  # blue
  "Wholesale (SIPSA)"  = "#E31A1C"   # red
)

p <- ggplot(plot_df, aes(x = date, y = log_price, color = series)) +
  geom_line(linewidth = 0.55, alpha = 0.95) +
  facet_grid(articulo_ipc ~ city, scales = "free_y") +
  scale_color_manual(values = series_cols) +
  labs(
    x = NULL,
    y = "Log price",
    color = NULL
  ) +
  theme_bw(base_size = 10) +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box = "horizontal",
    legend.text = element_text(size = 9),
    strip.text = element_text(size = 9, face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(linewidth = 0.25),
    axis.text.x = element_text(size = 7),
    axis.text.y = element_text(size = 7),
    axis.title.y = element_text(size = 9),
    plot.margin = margin(8, 8, 8, 8)
  ) +
  guides(
    color = guide_legend(override.aes = list(linewidth = 1.1, alpha = 1))
  )

print(p)

# -------------------------------------------------------------------
# Save outputs
# -------------------------------------------------------------------
out_png <- file.path(out_dir, paste0(date_tag, "_log_retail_vs_wholesale_7x3.png"))
out_pdf <- file.path(out_dir, paste0(date_tag, "_log_retail_vs_wholesale_7x3.pdf"))

ggsave(filename = out_png, plot = p, width = 11, height = 13, dpi = 300)
ggsave(filename = out_pdf, plot = p, width = 11, height = 13)
