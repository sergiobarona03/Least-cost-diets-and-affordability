############################################################
## Compare models (m0, m1, m4, m5, m6, m7): tables + plots ##
## - Harmonizes food names across sources                   ##
## - Builds wide + long comparison tables                   ##
## - Produces summary plots + per-food “best model” views   ##
############################################################

# -----------------------
# 0) Packages
# -----------------------
library(tidyverse)
library(readxl)
library(janitor)
library(writexl)
library(stringi)

# -----------------------
# 1) Working directory + paths
# -----------------------
base_dir <- "C:/Users/Portatil/Desktop/Least-cost-diets-and-affordability/Proyecto Interno/working-papers/working-paper-1225/"
setwd(base_dir)

out_dir <- file.path(base_dir, "model_comparison/output")

# Inputs (edit if needed)
m0_path <- "m0/output_ipc_fill/resultados_metricas_ipc.xlsx"
m1_path <- "m1/output/121225_m1_metrics.xlsx"              # if your file is different, edit
m4_path <- "m4/output/summary_metrics.csv"
m5_path <- "m5/output_ecm/m5_ecm_metrics_test.csv"
m6_path <- "m6/output_dummies/m6_outputs.xlsx"
m7_path <- "m7/output_dummies/summary_metrics_m7_dummies.csv"

# -----------------------
# 2) Helpers
# -----------------------
normalize_food <- function(x) {
  # robust key: remove accents, uppercase, trim, collapse spaces, remove punctuation
  x %>%
    as.character() %>%
    stri_trans_general("Latin-ASCII") %>%
    toupper() %>%
    stringr::str_replace_all("[[:punct:]]+", " ") %>%
    stringr::str_replace_all("\\s+", " ") %>%
    stringr::str_trim()
}

pick_first_existing <- function(df, candidates) {
  nm <- names(df)
  hit <- candidates[candidates %in% nm]
  if (length(hit) == 0) stop("No column found among: ", paste(candidates, collapse = ", "))
  hit[1]
}

# -----------------------
# 3) Load each model metrics + standardize columns
# -----------------------

## ---- m0 (IPC forward-fill) ----
m0_raw <- read_excel(m0_path) %>% clean_names()

# Expect: ciudad, articulo, rmse, mape, n_valid
m0 <- m0_raw %>%
  mutate(
    food = articulo,
    food_key = normalize_food(food)
  ) %>%
  transmute(
    model = "m0_ipc_fill",
    food,
    food_key,
    rmse = as.numeric(rmse),
    mape = as.numeric(mape),
    n_test = as.numeric(n_valid),
    extra = ifelse("ciudad" %in% names(m0_raw),
                   as.character(ciudad), NA_character_)
  )

## ---- m1 (margin scenarios Q1/Q2/Q3) ----
m1_raw <- read_excel(m1_path) %>% clean_names()

# Expect: alimento_sipsa, n_test, RMSE_Q1/Q2/Q3, MAPE_Q1/Q2/Q3
m1 <- m1_raw %>%
  mutate(
    food = alimento_sipsa,
    food_key = normalize_food(food),
    # best scenario by MAPE (you can change to RMSE if you prefer)
    best_scn = c("Q1","Q2","Q3")[max.col(
      cbind(-mape_q1, -mape_q2, -mape_q3), ties.method = "first"
    )],
    best_rmse = dplyr::case_when(
      best_scn == "Q1" ~ rmse_q1,
      best_scn == "Q2" ~ rmse_q2,
      TRUE             ~ rmse_q3
    ),
    best_mape = dplyr::case_when(
      best_scn == "Q1" ~ mape_q1,
      best_scn == "Q2" ~ mape_q2,
      TRUE             ~ mape_q3
    )
  )

# Keep BOTH: (i) Q2 as canonical, (ii) best scenario
m1_q2 <- m1 %>%
  transmute(
    model = "m1_margin_Q2",
    food, food_key,
    rmse = as.numeric(rmse_q2),
    mape = as.numeric(mape_q2),
    n_test = as.numeric(n_test),
    extra = "Q2"
  )

m1_best <- m1 %>%
  transmute(
    model = "m1_margin_best",
    food, food_key,
    rmse = as.numeric(best_rmse),
    mape = as.numeric(best_mape),
    n_test = as.numeric(n_test),
    extra = paste0("best=", best_scn)
  )

## ---- m4 (asymmetric ECM MTAR) ----
m4_raw <- read_csv(m4_path, show_col_types = FALSE) %>% clean_names()

# Expect: alimento_sipsa, RMSE_test, MAPE_test, n_total (train metrics may be NA)
m4 <- m4_raw %>%
  mutate(
    food = alimento_sipsa,
    food_key = normalize_food(food)
  ) %>%
  transmute(
    model = "m4_asy_ecm_mtar",
    food, food_key,
    rmse = as.numeric(rmse_test),
    mape = as.numeric(mape_test),
    n_test = NA_real_,
    extra = paste0("n_total=", n_total)
  )

## ---- m5 (ECM + cointegration test) ----
m5_raw <- read_csv(m5_path, show_col_types = FALSE) %>% clean_names()

# Expect: alimento_sipsa, n_test, RMSE_log, MAPE_level, coint_p_type*
m5 <- m5_raw %>%
  mutate(
    food = alimento_sipsa,
    food_key = normalize_food(food)
  ) %>%
  transmute(
    model = "m5_ecm",
    food, food_key,
    rmse = as.numeric(rmse_log),      # note: rmse on log-scale
    mape = as.numeric(mape_level),
    n_test = as.numeric(n_test),
    extra = paste0("p2=", coint_p_type2)
  )

## ---- m6 (LM with month dummies) ----
m6_raw <- read_excel(m6_path) %>% clean_names()

# Your file appears to have: alimento_sipsa, RMSE_log, MAPE_level, n_total, n_test
m6 <- m6_raw %>%
  mutate(
    food = alimento_sipsa,
    food_key = normalize_food(food)
  ) %>%
  transmute(
    model = "m6_lm_dummies",
    food, food_key,
    rmse = as.numeric(rmse_log),      # rmse on log-scale
    mape = as.numeric(mape_level),
    n_test = as.numeric(n_test),
    extra = paste0("n_total=", n_total)
  )

## ---- m7 (first-difference with month dummies) ----
m7_raw <- read_csv(m7_path, show_col_types = FALSE) %>% clean_names()

# Expect: alimento_sipsa, RMSE_level, MAPE_level (and RMSE_dy)
m7 <- m7_raw %>%
  mutate(
    food = alimento_sipsa,
    food_key = normalize_food(food)
  ) %>%
  transmute(
    model = "m7_fd_dummies",
    food, food_key,
    rmse = as.numeric(rmse_level),    # rmse in levels
    mape = as.numeric(mape_level),
    n_test = NA_real_,
    extra = paste0("n_obs=", n_obs)
  )

# -----------------------
# 4) Combine all models
# -----------------------
all_long2 <- bind_rows(
  m0,
  m1_q2,
  m1_best,
  m4,
  m5,
  m6,
  m7
) %>%
  mutate(
    model = factor(model, levels = c(
      "m0_ipc_fill",
      "m1_margin_Q2",
      "m1_margin_best",
      "m4_asy_ecm_mtar",
      "m5_ecm",
      "m6_lm_dummies",
      "m7_fd_dummies"
    ))
  )

# -----------------------
# 5) Wide comparison table (one row per food, columns per model)
# -----------------------
wide_mape <- all_long2 %>%
  select(food_key, food, model, mape) %>%
  group_by(food_key, model) %>% summarise(mape = first(mape), .groups = "drop") %>%
  pivot_wider(names_from = model, values_from = mape)

wide_rmse <- all_long2 %>%
  select(food_key, food, model, rmse) %>%
  group_by(food_key, model) %>% summarise(rmse = first(rmse), .groups = "drop") %>%
  pivot_wider(names_from = model, values_from = rmse)

# Best model per food by MAPE (lower is better)
best_by_food <- all_long2 %>%
  group_by(food_key) %>%
  filter(!is.na(mape)) %>%
  slice_min(order_by = mape, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  transmute(
    food_key, food,
    best_model = as.character(model),
    best_mape = mape,
    best_rmse = rmse,
    info = extra
  )

# Summary table by model
summary_by_model <- all_long2 %>%
  group_by(model) %>%
  summarise(
    n_foods = n_distinct(food_key),
    mape_median = median(mape, na.rm = TRUE),
    mape_mean   = mean(mape, na.rm = TRUE),
    rmse_median = median(rmse, na.rm = TRUE),
    rmse_mean   = mean(rmse, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(mape_median)

# Save tables
write_csv(all_long2, file.path(out_dir, "compare_models_long.csv"))
write_csv(wide_mape, file.path(out_dir, "compare_models_wide_mape.csv"))
write_csv(wide_rmse, file.path(out_dir, "compare_models_wide_rmse.csv"))
write_csv(best_by_food, file.path(out_dir, "best_model_by_food.csv"))
write_csv(summary_by_model, file.path(out_dir, "summary_by_model.csv"))

write_xlsx(
  list(
    long = all_long2,
    wide_mape = wide_mape,
    wide_rmse = wide_rmse,
    best_by_food = best_by_food,
    summary_by_model = summary_by_model
  ),
  file.path(out_dir, "compare_models_tables.xlsx")
)

# -----------------------
# 6) Plots
# -----------------------

# (A) Boxplot of MAPE by model (distribution across foods)
p_box_mape <- ggplot(all_long2, aes(x = model, y = mape*100)) +
  geom_boxplot(outlier.alpha = 0.25) +
  facet_wrap(~ model, scales = "free", nrow = 1) +
  labs(
    title = "MAPE distribution by model (across foods)",
    x = NULL,
    y = "MAPE (%)"
  ) +
  theme_classic(base_size = 12)

p_box_mape

ggsave(file.path(out_dir, "plot_box_mape_by_model.png"), p_box_mape, width = 10, height = 6, dpi = 300)

# (B) Median MAPE per model (bar)
p_bar_mape <- summary_by_model %>%
  ggplot(aes(x = reorder(as.character(model), mape_median), y = mape_median)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Median MAPE by model",
    x = NULL,
    y = "Median MAPE (%)"
  ) +
  theme_classic(base_size = 12)

ggsave(file.path(out_dir, "plot_median_mape_by_model.png"), p_bar_mape, width = 10, height = 6, dpi = 300)

# (C) Scatter RMSE vs MAPE (per food per model)
p_scatter <- ggplot(all_long2, aes(x = rmse, y = mape*100)) +
  geom_point(alpha = 0.6) +
  facet_wrap(~ model, scales = "free") +
  labs(
    title = "RMSE vs MAPE by model (each dot = food)",
    x = "RMSE (as reported per model)",
    y = "MAPE (%)"
  ) +
  theme_classic(base_size = 11)

ggsave(file.path(out_dir, "plot_scatter_rmse_vs_mape_facets.png"), p_scatter, width = 13, height = 8, dpi = 300)

# (D) Heatmap of MAPE (foods x models) for a compact comparison
heat_df <- all_long2 %>%
  select(food, food_key, model, mape) %>%
  group_by(food_key, model) %>%
  summarise(mape = first(mape), food = first(food), .groups = "drop")

# Keep top N foods by frequency (optional). Comment out to keep all.
# topN <- 60
# top_foods <- heat_df %>% count(food_key, sort = TRUE) %>% slice_head(n = topN) %>% pull(food_key)
# heat_df <- heat_df %>% filter(food_key %in% top_foods)

p_heat_ipc <- ggplot(heat_df %>%
                       filter(
                         model == "m0_ipc_fill"
                       ), aes(x = model, y = reorder(food, mape, FUN = median, na.rm = TRUE), fill = mape)) +
  geom_tile() +
  labs(
    title = "Heatmap: MAPE (%) by food and model",
    x = NULL,
    y = NULL,
    fill = "MAPE"
  ) +
  theme_classic(base_size = 10) +
  theme(
    axis.text.y = element_text(size = 7)
  )

ggsave(file.path(out_dir, "ipc_plot_heatmap_mape_food_model.png"),
       p_heat_ipc, width = 12, height = 14, dpi = 300)

p_heat_sipsa <- ggplot(heat_df %>%
                         filter(
                           model != "m0_ipc_fill"
                         ), aes(x = model, y = reorder(food, mape, FUN = median, na.rm = TRUE), fill = mape)) +
  geom_tile() +
  labs(
    title = "Heatmap: MAPE (%) by food and model",
    x = NULL,
    y = NULL,
    fill = "MAPE"
  ) +
  theme_classic(base_size = 10) +
  theme(
    axis.text.y = element_text(size = 7)
  )
ggsave(file.path(out_dir, "sipsa_plot_heatmap_mape_food_model.png"), 
       p_heat_sipsa, width = 12, height = 14, dpi = 300)


# (E) “Best model count” plot
best_counts <- best_by_food %>%
  count(best_model, sort = TRUE)

p_best <- ggplot(best_counts, aes(x = reorder(best_model, n), y = n)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "How often each model is best (lowest MAPE) across foods",
    x = NULL,
    y = "# Foods where model is best"
  ) +
  theme_classic(base_size = 12)

ggsave(file.path(out_dir, "plot_best_model_counts.png"), p_best, width = 10, height = 6, dpi = 300)

