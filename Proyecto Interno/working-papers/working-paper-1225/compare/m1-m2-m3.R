############################################################
## Compare M1 vs M2 vs M3 metrics + tables + figures       ##
############################################################

# -----------------------
# Libraries
# -----------------------
library(tidyverse)
library(lubridate)
library(readxl)
library(readr)
library(ggplot2)
library(ggforce)

# -----------------------
# Settings
# -----------------------
setwd("C:/Users/Portatil/Desktop/Least-cost-diets-and-affordability/Proyecto Interno")

date_tag  <- "121225"
city_code <- "76001"

out_dir_compare <- "working-papers\\working-paper-1225\\compare\\output\\"

# -----------------------
# Helper: locate files
# -----------------------
locate_file <- function(paths) {
  for (p in paths) if (file.exists(p)) return(p)
  stop("File not found. Tried:\n", paste(paths, collapse = "\n"))
}

# -----------------------
# 0) File paths (robust)
# -----------------------

# M1 metrics (created earlier)
m1_by_food_path <- locate_file(c(
  paste0("output/", date_tag, "_m1_metrics_by_food.csv"),
  paste0("working-papers\\working-paper-1225\\m1\\output\\", date_tag, "_m1_metrics_by_food.csv")
))

m1_agg_path <- locate_file(c(
  paste0("output/", date_tag, "_m1_metrics_aggregate.csv"),
  paste0("working-papers\\working-paper-1225\\m1\\output\\", date_tag, "_m1_metrics_aggregate.csv")
))

# M2
m2_by_food_path <- locate_file(c(
  paste0("working-papers\\working-paper-1225\\m2\\output\\", date_tag, "_m2_metrics_by_food.csv")
))

m2_agg_path <- locate_file(c(
  paste0("working-papers\\working-paper-1225\\m2\\output\\", date_tag, "_m2_metrics_aggregate.csv")
))

# M3
m3_by_food_path <- locate_file(c(
  paste0("working-papers\\working-paper-1225\\m3\\output\\", date_tag, "_m3_metrics_by_food_city_", city_code, ".csv")
))

m3_agg_path <- locate_file(c(
  paste0("working-papers\\working-paper-1225\\m3\\output\\", date_tag, "_m3_metrics_aggregate_city_", city_code, ".csv")
))

# -----------------------
# 1) Load metrics
# -----------------------
m1_by_food_raw <- read_csv(m1_by_food_path, show_col_types = FALSE)
m1_agg_raw     <- read_csv(m1_agg_path, show_col_types = FALSE)

m2_by_food_raw <- read_csv(m2_by_food_path, show_col_types = FALSE)
m2_agg_raw     <- read_csv(m2_agg_path, show_col_types = FALSE)

m3_by_food_raw <- read_csv(m3_by_food_path, show_col_types = FALSE)
m3_agg_raw     <- read_csv(m3_agg_path, show_col_types = FALSE)

# -----------------------
# 2) Harmonize per-food metrics (use Q2 for M1 point forecast)
# -----------------------
m1_by_food <- m1_by_food_raw %>%
  transmute(
    alimento_sipsa,
    n_test,
    RMSE = RMSE_Q2,
    MAPE = MAPE_Q2
  ) %>%
  mutate(method = "M1 (Margins Q2)")

m2_by_food <- m2_by_food_raw %>%
  transmute(
    alimento_sipsa,
    n_test,
    RMSE,
    MAPE
  ) %>%
  mutate(method = "M2 (SARIMAX)")

m3_by_food <- m3_by_food_raw %>%
  transmute(
    alimento_sipsa,
    n_test,
    RMSE,
    MAPE
  ) %>%
  mutate(method = "M3 (ECM)")

# Keep only foods common to all methods (fair comparison)
common_foods <- Reduce(intersect, list(
  unique(m1_by_food$alimento_sipsa),
  unique(m2_by_food$alimento_sipsa),
  unique(m3_by_food$alimento_sipsa)
))

m1_by_food <- m1_by_food %>% filter(alimento_sipsa %in% common_foods)
m2_by_food <- m2_by_food %>% filter(alimento_sipsa %in% common_foods)
m3_by_food <- m3_by_food %>% filter(alimento_sipsa %in% common_foods)

metrics_food_long <- bind_rows(m1_by_food, m2_by_food, m3_by_food)

# Save per-food metrics (common foods only)
write_csv(metrics_food_long, paste0(out_dir_compare, date_tag, "_compare_metrics_by_food_long_common.csv"))

# -----------------------
# 3) Simple summary table (across foods)
# -----------------------
summary_methods <- metrics_food_long %>%
  group_by(method) %>%
  summarise(
    n_foods = n_distinct(alimento_sipsa),
    RMSE_mean = mean(RMSE, na.rm = TRUE),
    RMSE_median = median(RMSE, na.rm = TRUE),
    MAPE_mean = mean(MAPE, na.rm = TRUE),
    MAPE_median = median(MAPE, na.rm = TRUE),
    .groups = "drop"
  )

write_csv(summary_methods,
          paste0(out_dir_compare, date_tag, "_compare_summary_methods_common.csv"))

# -----------------------
# 4) “Winner” method counts by food (RMSE & MAPE) - tie-safe
# -----------------------

# RMSE winners (ties split equally)
wins_rmse <- metrics_food_long %>%
  group_by(alimento_sipsa) %>%
  filter(RMSE == min(RMSE, na.rm = TRUE)) %>%
  mutate(weight = 1 / n()) %>%
  ungroup() %>%
  group_by(method) %>%
  summarise(
    wins_rmse = sum(weight),
    share_rmse = wins_rmse / length(common_foods) * 100,
    .groups = "drop"
  )

# MAPE winners (ties split equally)
wins_mape <- metrics_food_long %>%
  group_by(alimento_sipsa) %>%
  filter(MAPE == min(MAPE, na.rm = TRUE)) %>%
  mutate(weight = 1 / n()) %>%
  ungroup() %>%
  group_by(method) %>%
  summarise(
    wins_mape = sum(weight),
    share_mape = wins_mape / length(common_foods) * 100,
    .groups = "drop"
  )

wins_summary <- full_join(wins_rmse, wins_mape, by = "method") %>%
  replace_na(list(wins_rmse = 0, share_rmse = 0, wins_mape = 0, share_mape = 0)) %>%
  arrange(desc(wins_rmse), desc(wins_mape))

write_csv(wins_summary, paste0(out_dir_compare, date_tag, "_compare_wins_summary_common.csv"))

# -----------------------
# 5) Figures
# -----------------------

# 5.1 Winner shares plot
wins_plot <- wins_summary %>%
  select(method, share_rmse, share_mape) %>%
  pivot_longer(cols = c(share_rmse, share_mape),
               names_to = "metric", values_to = "share") %>%
  mutate(metric = recode(metric,
                         share_rmse = "Winner share (RMSE)",
                         share_mape = "Winner share (MAPE)"))

p_wins <- ggplot(wins_plot, aes(x = method, y = share)) +
  geom_col() +
  facet_wrap(~ metric) +
  coord_flip() +
  labs(
    title = "Share of foods where each methodology is best (common foods)",
    x = NULL,
    y = "% of foods"
  ) +
  theme_classic()

ggsave(
  paste0(out_dir_compare, date_tag, "_fig_winner_shares_common.png"),
  p_wins, width = 10, height = 5
)

# 5.2 Boxplot RMSE
p_box_rmse <- ggplot(metrics_food_long, aes(x = method, y = RMSE)) +
  geom_boxplot() +
  coord_flip() +
  labs(
    title = "RMSE distribution across foods (common foods)",
    x = NULL, y = "RMSE"
  ) +
  theme_classic()

ggsave(
  paste0(out_dir_compare, date_tag, "_fig_box_rmse_common.png"),
  p_box_rmse, width = 10, height = 5
)

# 5.3 Boxplot MAPE
p_box_mape <- ggplot(metrics_food_long, aes(x = method, y = MAPE)) +
  geom_boxplot() +
  coord_flip() +
  labs(
    title = "MAPE distribution across foods (common foods)",
    x = NULL, y = "MAPE (%)"
  ) +
  theme_classic()

ggsave(
  paste0(out_dir_compare, date_tag, "_fig_box_mape_common.png"),
  p_box_mape, width = 10, height = 5
)

# -----------------------
# Console summary
# -----------------------
cat("\n=== COMMON FOODS COUNT ===\n")
cat(length(common_foods), "\n")

cat("\nSaved outputs in:\n", out_dir_compare, "\n")
