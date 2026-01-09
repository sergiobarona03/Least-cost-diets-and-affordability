############################################################
# Compare CoNA COSTS across models (baseline included)
# Filter: Demo_Group == "31 a 50 años" & Sex == 1
# - 3-scenario models as bands (geom_ribbon):
#     m1: q1-q3 band, q2 line
#     m5/m6: lwr-upr band, hat line
# - Single-scenario as lines:
#     baseline, m0
# Saves ALL outputs to: output/compare
############################################################

library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(readr)

# -----------------------
# 0) Paths
# -----------------------
base_dir <- "C:/Users/Portatil/Desktop/Least-cost-diets-and-affordability/Proyecto Interno"
setwd(base_dir)

out_dir <- file.path(base_dir, "working-papers", "working-paper-0125", "output")
compare_dir <- file.path(out_dir, "compare")
dir.create(compare_dir, recursive = TRUE, showWarnings = FALSE)

# -----------------------
# 1) Filter parameters
# -----------------------
target_demo <- "31 a 50 años"
target_sex  <- 1

# -----------------------
# 2) Helpers
# -----------------------
pick_first_existing <- function(df, candidates) {
  nm <- names(df)
  hit <- candidates[candidates %in% nm]
  if (length(hit) == 0) {
    stop("No column found among: ", paste(candidates, collapse = ", "))
  }
  hit[1]
}

extract_cost_day <- function(cost_df, demo_group, sex_value) {
  if (is.null(cost_df) || all(is.na(cost_df))) return(NA_real_)
  if (!is.data.frame(cost_df)) return(NA_real_)
  
  # nombres flexibles
  dg_col <- intersect(names(cost_df), c("Demo_Group", "demo_group", "grupo_demo", "grupo_edad"))
  sx_col <- intersect(names(cost_df), c("Sex", "sex", "sexo"))
  cd_col <- intersect(names(cost_df), c("cost_day", "costo_dia", "cost", "costo"))
  
  if (length(dg_col) == 0 || length(sx_col) == 0 || length(cd_col) == 0) return(NA_real_)
  
  dg_col <- dg_col[1]; sx_col <- sx_col[1]; cd_col <- cd_col[1]
  
  out <- cost_df %>%
    filter(.data[[dg_col]] == demo_group, .data[[sx_col]] == sex_value) %>%
    slice(1) %>%
    pull(.data[[cd_col]])
  
  if (length(out) == 0) NA_real_ else as.numeric(out)
}

# wide object: fecha + scenario columns (list-cols of dfs)
wide_to_midlowup <- function(wide_obj, model, mid_col, low_col = NULL, up_col = NULL,
                             demo_group, sex_value) {
  
  stopifnot("fecha" %in% names(wide_obj))
  
  tibble(
    fecha = as.Date(wide_obj$fecha),
    model = model,
    mid   = map_dbl(wide_obj[[mid_col]], extract_cost_day, demo_group = demo_group, sex_value = sex_value),
    low   = if (!is.null(low_col)) map_dbl(wide_obj[[low_col]], extract_cost_day, demo_group = demo_group, sex_value = sex_value) else NA_real_,
    up    = if (!is.null(up_col))  map_dbl(wide_obj[[up_col]],  extract_cost_day, demo_group = demo_group, sex_value = sex_value) else NA_real_
  )
}

# Robust converter for baseline/m0:
# Case B: long format with columns (fecha, Demo_Group, Sex, cost_day)
# Case A: fecha + list-column (each element is a df 22x4)
# Case C: fecha + cost_col is itself a data.frame (then treat as long if possible)
long_to_midonly <- function(obj, model, cost_col = "cona_cost", demo_group, sex_value) {
  
  # ---- Case B: already long
  if ("fecha" %in% names(obj)) {
    dg_hit <- intersect(names(obj), c("Demo_Group", "demo_group", "grupo_demo", "grupo_edad"))
    sx_hit <- intersect(names(obj), c("Sex", "sex", "sexo"))
    cd_hit <- intersect(names(obj), c("cost_day", "costo_dia", "cost", "costo"))
    
    if (length(dg_hit) > 0 && length(sx_hit) > 0 && length(cd_hit) > 0) {
      dg_col <- dg_hit[1]; sx_col <- sx_hit[1]; cd_col <- cd_hit[1]
      
      return(
        obj %>%
          mutate(fecha = as.Date(fecha)) %>%
          filter(.data[[dg_col]] == demo_group, .data[[sx_col]] == sex_value) %>%
          group_by(fecha) %>%
          summarise(mid = first(.data[[cd_col]]), .groups = "drop") %>%
          mutate(model = model, low = NA_real_, up = NA_real_) %>%
          select(fecha, model, mid, low, up)
      )
    }
  }
  
  # ---- If cost_col missing, fail early
  if (!(cost_col %in% names(obj))) {
    stop(model, ": falta columna ", cost_col, " y no es formato largo con cost_day.")
  }
  
  # ---- Case A: list-column (must be list AND NOT data.frame)
  if (is.list(obj[[cost_col]]) && !is.data.frame(obj[[cost_col]]) && "fecha" %in% names(obj)) {
    return(
      tibble(
        fecha = as.Date(obj$fecha),
        model = model,
        mid   = map_dbl(obj[[cost_col]], extract_cost_day, demo_group = demo_group, sex_value = sex_value),
        low = NA_real_,
        up  = NA_real_
      )
    )
  }
  
  # ---- Case C: cost_col is a data.frame; try to treat as long, using fecha inside obj
  if (is.data.frame(obj[[cost_col]]) && "fecha" %in% names(obj)) {
    df_long <- obj[[cost_col]]
    df_long$fecha <- as.Date(obj$fecha)  # align by row if same nrow
    
    # try long extraction
    dg_col <- intersect(names(df_long), c("Demo_Group", "demo_group", "grupo_demo", "grupo_edad"))
    sx_col <- intersect(names(df_long), c("Sex", "sex", "sexo"))
    cd_col <- intersect(names(df_long), c("cost_day", "costo_dia", "cost", "costo"))
    
    if (length(dg_col) > 0 && length(sx_col) > 0 && length(cd_col) > 0) {
      dg_col <- dg_col[1]; sx_col <- sx_col[1]; cd_col <- cd_col[1]
      
      return(
        df_long %>%
          filter(.data[[dg_col]] == demo_group, .data[[sx_col]] == sex_value) %>%
          group_by(fecha) %>%
          summarise(mid = first(.data[[cd_col]]), .groups = "drop") %>%
          mutate(model = model, low = NA_real_, up = NA_real_) %>%
          select(fecha, model, mid, low, up)
      )
    }
    
    stop(model, ": cost_col es data.frame pero no tiene columnas esperadas (Demo_Group/Sex/cost_day).")
  }
  
  stop(model, ": No reconozco la estructura del objeto. Revisa columnas/estructura del RDS.")
}

# -----------------------
# 3) Load model outputs
# -----------------------
baseline_path <- file.path(out_dir, "cona", "baseline_cona_cost.RDS")
m0_path       <- file.path(out_dir, "cona", "m0_cona_cost.RDS")

m1_wide_path  <- file.path(out_dir, "cona", "m1_cona_cost_wide_q1_q2_q3.RDS")
m6_wide_path  <- file.path(out_dir, "cona", "m6_cona_cost_wide_hat_lwr_upr.RDS")

baseline_df <- if (file.exists(baseline_path)) {
  baseline_obj <- readRDS(baseline_path)
  long_to_midonly(baseline_obj, "baseline", cost_col = "cona_cost", demo_group = target_demo, sex_value = target_sex)
} else {
  warning("No encuentro baseline_cona_cost.RDS en: ", baseline_path)
  tibble(fecha = as.Date(character()), model = character(), mid = double(), low = double(), up = double())
}

m0_df <- if (file.exists(m0_path)) {
  m0_obj <- readRDS(m0_path)
  long_to_midonly(m0_obj, "m0", cost_col = "cona_cost", demo_group = target_demo, sex_value = target_sex)
} else {
  warning("No encuentro m0_cona_cost.RDS en: ", m0_path)
  tibble(fecha = as.Date(character()), model = character(), mid = double(), low = double(), up = double())
}

m1_df <- if (file.exists(m1_wide_path)) {
  m1_wide <- readRDS(m1_wide_path)
  wide_to_midlowup(m1_wide, "m1", mid_col = "q2", low_col = "q1", up_col = "q3",
                   demo_group = target_demo, sex_value = target_sex)
} else {
  warning("No encuentro m1_cona_cost_wide_q1_q2_q3.RDS en: ", m1_wide_path)
  tibble(fecha = as.Date(character()), model = character(), mid = double(), low = double(), up = double())
}


m6_df <- if (file.exists(m6_wide_path)) {
  m6_wide <- readRDS(m6_wide_path)
  wide_to_midlowup(m6_wide, "m6", mid_col = "hat", low_col = "lwr", up_col = "upr",
                   demo_group = target_demo, sex_value = target_sex)
} else {
  warning("No encuentro m6_cona_cost_wide_hat_lwr_upr.RDS en: ", m6_wide_path)
  tibble(fecha = as.Date(character()), model = character(), mid = double(), low = double(), up = double())
}

# -----------------------
# 4) Combine + save panel
# -----------------------
panel <- bind_rows(baseline_df, m0_df, m1_df, m6_df) %>%
  mutate(
    model = as.character(model),
    has_band = !is.na(low) & !is.na(up)
  ) %>%
  arrange(fecha, model)

saveRDS(panel, file.path(compare_dir, "panel_cona_cost_mid_low_up_31_50_sex1.RDS"))
write_csv(panel, file.path(compare_dir, "panel_cona_cost_mid_low_up_31_50_sex1.csv"))

# -----------------------
# 5) Tables
# -----------------------
wide_mid <- panel %>%
  select(fecha, model, mid) %>%
  pivot_wider(names_from = model, values_from = mid) %>%
  arrange(fecha)

saveRDS(wide_mid, file.path(compare_dir, "table_mid_cost_wide_31_50_sex1.RDS"))
write_csv(wide_mid, file.path(compare_dir, "table_mid_cost_wide_31_50_sex1.csv"))

wide_band <- panel %>%
  filter(has_band) %>%
  select(fecha, model, low, up) %>%
  pivot_wider(names_from = model, values_from = c(low, up)) %>%
  arrange(fecha)

saveRDS(wide_band, file.path(compare_dir, "table_band_low_up_wide_31_50_sex1.RDS"))
write_csv(wide_band, file.path(compare_dir, "table_band_low_up_wide_31_50_sex1.csv"))

summary_mid <- panel %>%
  group_by(model) %>%
  summarise(
    n_dates = sum(!is.na(mid)),
    mean_mid = mean(mid, na.rm = TRUE),
    median_mid = median(mid, na.rm = TRUE),
    sd_mid = sd(mid, na.rm = TRUE),
    min_mid = min(mid, na.rm = TRUE),
    max_mid = max(mid, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(median_mid)

saveRDS(summary_mid, file.path(compare_dir, "summary_mid_cost_by_model_31_50_sex1.RDS"))
write_csv(summary_mid, file.path(compare_dir, "summary_mid_cost_by_model_31_50_sex1.csv"))

# -----------------------
# 6) Plots (bands + lines)
# -----------------------
p_ts_band <- ggplot() +
  geom_ribbon(
    data = panel %>% filter(has_band),
    aes(x = fecha, ymin = low, ymax = up, fill = model),
    alpha = 0.20,
    inherit.aes = FALSE
  ) +
  geom_line(
    data = panel,
    aes(x = fecha, y = mid, color = model),
    linewidth = 0.9,
    na.rm = TRUE
  ) +
  labs(
    title = paste0("CoNA cost_day comparison (bands for 3-scenario models)\n",
                   target_demo, " | Sex = ", target_sex),
    x = NULL,
    y = "Cost per day",
    color = "Model",
    fill  = "Model"
  ) +
  theme_classic(base_size = 12)

ggsave(
  file.path(compare_dir, "plot_cona_cost_day_timeseries_band_31_50_sex1.png"),
  p_ts_band, width = 12, height = 6, dpi = 300
)

p_facet <- ggplot(panel, aes(x = fecha)) +
  geom_ribbon(
    data = panel %>% filter(has_band),
    aes(ymin = low, ymax = up),
    alpha = 0.20
  ) +
  geom_line(aes(y = mid), linewidth = 0.9, na.rm = TRUE) +
  facet_wrap(~ model, scales = "free_y", ncol = 1) +
  labs(
    title = paste0("CoNA cost_day by model (", target_demo, ", Sex=", target_sex, ")"),
    x = NULL,
    y = "Cost per day"
  ) +
  theme_classic(base_size = 12)

ggsave(
  file.path(compare_dir, "plot_cona_cost_day_facets_band_31_50_sex1.png"),
  p_facet, width = 10, height = 10, dpi = 300
)

print(summary_mid, n = Inf)
