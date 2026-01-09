############################################################
## Compare models (m0, m1, m4, m5, m6, m7) using MAPE      ##
## + Fix m4 columns: uses rmse_test_dy / mape_test_dy      ##
## + (Optional) auto-rescale MAPE if some are fractions    ##
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

m0_path <- "m0/output_ipc_fill/resultados_metricas_ipc.xlsx"
m1_path <- "m1/output/121225_m1_metrics_by_food.csv"

m4_path <- "m4/output/summary_metrics_dlog.csv"
m5_path <- "m5/output_ecm/m5_ecm_metrics_test_bestlags.csv"
m6_path <- "m6/output_dummies/summary_metrics_m6_dummies.csv"
m7_path <- "m7/output_dummies/summary_metrics_m7_dummies.csv"

# -----------------------
# 2) Helpers
# -----------------------
normalize_food <- function(x) {
  x %>%
    as.character() %>%
    stri_trans_general("Latin-ASCII") %>%
    toupper() %>%
    stringr::str_replace_all("[[:punct:]]+", " ") %>%
    stringr::str_replace_all("\\s+", " ") %>%
    stringr::str_trim()
}


# -----------------------
# 3) Load each model metrics + standardize columns
# -----------------------

## ---- m0 (IPC forward-fill) ----
m0_raw <- read_excel(m0_path) %>% clean_names()

m0 <- m0_raw %>%
  mutate(
    food = articulo,
    food_key = normalize_food(food)
  ) %>%
  transmute(
    model = "m0_ipc_fill",
    food,
    food_key,
    rmse = as_num(rmse),
    mape = as_num(mape),
    n_test = as_num(n_valid),
    extra = ifelse("ciudad" %in% names(m0_raw),
                   as.character(ciudad), NA_character_)
  )

## ---- m1 (margin scenarios Q1/Q2/Q3) ----
m1_raw <- read.csv(m1_path) %>% clean_names()

m1 <- m1_raw %>% mutate(food = alimento_sipsa, 
                   food_key = normalize_food(food),
                   # best scenario by MAPE (you can change to RMSE if you prefer) 
                   best_scn = c("Q1","Q2","Q3")[max.col( cbind(-mape_q1, -mape_q2, -mape_q3), 
                                                         ties.method = "first" )],
                   best_rmse = dplyr::case_when( best_scn == "Q1" ~ rmse_q1, 
                                                 best_scn == "Q2" ~ rmse_q2, 
                                                 TRUE ~ rmse_q3 ),
                   best_mape = dplyr::case_when( best_scn == "Q1" ~ mape_q1,
                                                 best_scn == "Q2" ~ mape_q2,
                                                 TRUE ~ mape_q3 ) )

m1_q2 <- m1 %>% transmute( model = "m1_margin_Q2",
                           food, food_key, 
                           rmse = as.numeric(rmse_q2), 
                           mape = as.numeric(mape_q2),
                           n_test = as.numeric(n_test)) 

m1_best <- m1 %>% transmute( model = "m1_margin_Q_best",
                             food, food_key, 
                             rmse = as.numeric(best_rmse),
                             mape = as.numeric(best_mape),
                             n_test = as.numeric(n_test))


## ---- m4 (MTAR / asymmetric ECM) ----
m4_raw <- read_csv(m4_path, show_col_types = FALSE) %>% clean_names()

m4 <- m4_raw %>%
  mutate(
    food = alimento_sipsa,
    food_key = normalize_food(food)
  ) %>%
  transmute(
    model = "m4_asy_ecm_mtar",
    food, food_key,
    rmse = rmse_test_dy,
    mape = mape_test_dy,
    n_test = n_total,
  )

## ---- m5 (ECM + cointegration test) ----
m5_raw <- read_csv(m5_path, show_col_types = FALSE) %>% clean_names()

m5 <- m5_raw %>%
  mutate(
    food = alimento_sipsa,
    food_key = normalize_food(food)
  ) %>%
  transmute(
    model = "m5_ecm",
    food, food_key,
    rmse = as_num(rmse_dlog),
    mape = as_num(mape_dlog))

## ---- m6 (LM with month dummies) ----
m6_raw <- read.csv(m6_path) %>% clean_names()

m6 <- m6_raw %>%
  mutate(
    food = alimento_sipsa,
    food_key = normalize_food(food)
  ) %>%
  transmute(
    model = "m6_lm_dummies",
    food, food_key,
    rmse = as_num(rmse_level),
    mape = as_num(mape_level),
    n_test = as_num(n_test))

## ---- m7 (first-difference with month dummies) ----
m7_raw <- read_csv(m7_path, show_col_types = FALSE) %>% clean_names()

m7 <- m7_raw %>%
  mutate(
    food = alimento_sipsa,
    food_key = normalize_food(food)
  ) %>%
  transmute(
    model = "m7_fd_dummies",
    food, food_key,
    rmse = as_num(rmse_dy),
    mape = as_num(mape_dy),
    n_test = n_obs
  )

# -----------------------
# 4) Combine all models
# -----------------------
all_long2 <- bind_rows(
  m0,m1_q2, m1_best, m4,
  m5,m6,m7) %>%
  mutate(
    model = factor(model, levels = c(
      "m0_ipc_fill",
      "m1_margin_Q2",
      "m1_margin_Q_best",
      "m4_asy_ecm_mtar",
      "m5_ecm",
      "m6_lm_dummies",
      "m7_fd_dummies"
    ))
  ) %>% select(model, food_key, food,
               rmse, mape, n_test)


# -----------------------
# 5) Wide comparison tables
# -----------------------
wide_mape_ipc <- all_long2 %>%
  select(food_key, food, model, mape) %>%
  filter(model == "m0_ipc_fill") %>%
  group_by(food_key, model) %>%
  summarise(mape = first(mape), .groups = "drop") %>%
  pivot_wider(names_from = model, values_from = mape)

wide_mape_ipc_sipsa <- all_long2 %>%
  select(food_key, food, model, mape) %>%
  filter(model != "m0_ipc_fill") %>%
  group_by(food_key, model) %>%
  summarise(mape = first(mape), .groups = "drop") %>%
  pivot_wider(names_from = model, values_from = mape)

wide_rmse_ipc <- all_long2 %>%
  select(food_key, food, model, rmse) %>%
  filter(model == "m0_ipc_fill") %>%
  group_by(food_key, model) %>%
  summarise(rmse = first(rmse), .groups = "drop") %>%
  pivot_wider(names_from = model, values_from = rmse)

wide_rmse_ipc_sipsa <- all_long2 %>%
  select(food_key, food, model, rmse) %>%
  filter(model != "m0_ipc_fill") %>%
  group_by(food_key, model) %>%
  summarise(rmse = first(rmse), .groups = "drop") %>%
  pivot_wider(names_from = model, values_from = rmse)


# Save tables
write_csv(all_long2, file.path(out_dir, "compare_models_long.csv"))
write_csv(wide_mape_ipc, file.path(out_dir, "wide_mape_ipc.csv"))
write_csv(wide_mape_ipc_sipsa, file.path(out_dir, "wide_mape_ipc_sipsa.csv"))
write_csv(wide_rmse_ipc, file.path(out_dir, "wide_rmse_ipc.csv"))
write_csv(wide_rmse_ipc_sipsa, file.path(out_dir, "wide_rmse_ipc_sipsa.csv"))

write_xlsx(
  list(
    long = all_long2,
    wide_mape_ipc = wide_mape_ipc,
    wide_mape_ipc_sipsa = wide_mape_ipc_sipsa,
    wide_rmse_ipc = wide_rmse_ipc,
    wide_rmse_ipc_sipsa = wide_rmse_ipc_sipsa
  ),
  file.path(out_dir, "compare_models_tables.xlsx")
)

# -----------------------
# 6) Plots (MAPE is treated as percent here)
# -----------------------
p_box_mape <- ggplot(all_long2, aes(x = model, y = mape)) +
  geom_boxplot(outlier.alpha = 0.25) +
  facet_wrap(~ model, scales = "free", nrow = 1) +
  labs(
    title = "MAPE distribution by model (across foods)",
    x = NULL,
    y = "MAPE (%)"
  ) +
  theme_classic(base_size = 12)

ggsave(file.path(out_dir, "plot_box_mape_by_model.png"),
       p_box_mape, width = 10, height = 6, dpi = 300)


heat_df <- all_long2 %>% 
  select(food, food_key, model, mape) %>%
  group_by(food_key, model) %>%
  summarise(
    mape = first(mape),
    food = first(food),
    .groups = "drop"
  ) %>%
  mutate(
    mape_band = cut(
      mape,
      breaks = c(-Inf, 5, 10, 20, Inf),
      labels = c(
        "Excellent (<5%)",
        "Good (5–10%)",
        "Acceptable (10–20%)",
        "Poor (>20%)"
      ),
      right = TRUE
    )
  )

p_heat_ipc <- ggplot(
  heat_df %>% filter(model == "m0_ipc_fill"),
  aes(
    x = model,
    y = reorder(food, mape, FUN = median, na.rm = TRUE),
    fill = mape_band
  )
) +
  geom_tile(color = "white", linewidth = 0.2) +
  scale_fill_manual(
    values = c(
      "Excellent (<5%)"     = "#1a9850",
      "Good (5–10%)"        = "#91cf60",
      "Acceptable (10–20%)" = "#fee08b",
      "Poor (>20%)"         = "#d73027"
    ),
    drop = FALSE
  ) +
  labs(
    title = "Heatmap: Forecast accuracy by food (IPC model)",
    x = NULL,
    y = NULL,
    fill = "MAPE category"
  ) +
  theme_classic(base_size = 10) +
  theme(
    axis.text.y = element_text(size = 7),
    legend.position = "right"
  )

ggsave(
  file.path(out_dir, "ipc_plot_heatmap_mape_food_model.png"),
  p_heat_ipc, width = 12, height = 14, dpi = 300
)


p_heat_sipsa <- ggplot(
  heat_df %>% filter(model != "m0_ipc_fill"),
  aes(
    x = model,
    y = reorder(food, mape, FUN = median, na.rm = TRUE),
    fill = mape_band
  )
) +
  geom_tile(color = "white", linewidth = 0.2) +
  scale_fill_manual(
    values = c(
      "Excellent (<5%)"     = "#1a9850",
      "Good (5–10%)"        = "#91cf60",
      "Acceptable (10–20%)" = "#fee08b",
      "Poor (>20%)"         = "#d73027"
    ),
    drop = FALSE
  ) +
  labs(
    title = "Heatmap: Forecast accuracy by food and model (SIPSA-based models)",
    x = NULL,
    y = NULL,
    fill = "MAPE category"
  ) +
  theme_classic(base_size = 10) +
  theme(
    axis.text.y = element_text(size = 7),
    legend.position = "right"
  )

ggsave(
  file.path(out_dir, "sipsa_plot_heatmap_mape_food_model.png"),
  p_heat_sipsa, width = 12, height = 14, dpi = 300
)

