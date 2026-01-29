########################################################
## 05_compute_HCost_affordability_q1_q2_q3.R
## HCost (Model_CoCA + Model_CoNA) for 3 cities, all months
## Three price scenarios: q1, q2, q3 (from new panel)
## Plots: Q2 line + Q1–Q3 band, faceted by city
########################################################

source("working-papers/working-paper-q2/scripts/00_config_q2.R")

suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(readr)
  library(writexl)
  library(FoodpriceR)
  library(tidyr)
})

# -----------------------------
# 1) Load panel (NEW INPUT)
# -----------------------------
in_panel <- file.path(q2_fullsample_dir, "panel_city_month_food_2013_2025.rds")
if (!file.exists(in_panel)) {
  stop("No se encontró el panel: ", in_panel)
}
panel <- readRDS(in_panel)

# Chequeos mínimos
req_cols <- c(
  "ciudad","fecha","alimento_sipsa",
  "precio_100g_q1","precio_100g_q2","precio_100g_q3",
  "energia_kcal","proteina_g","lipidos_g","carbohidratos_totales_g",
  "vitamina_c_mg","folatos_mcg","vitamina_a_er","tiamina_mg","riboflavina_mg",
  "niacina_mg","vitamina_b12_mcg","magnesio_mg","fosforo_mg","sodio_mg",
  "calcio_mg","hierro_mg","zinc_mg"
)
missing_cols <- setdiff(req_cols, names(panel))
if (length(missing_cols) > 0) {
  stop("Al panel le faltan estas columnas requeridas: ", paste(missing_cols, collapse = ", "))
}

panel <- panel %>%
  mutate(
    ciudad = as.character(ciudad),
    fecha  = as.Date(fecha),
    alimento_sipsa = as.character(alimento_sipsa)
  )

message("Panel rows: ", nrow(panel))
message("Cities: ", paste(sort(unique(panel$ciudad)), collapse = ", "))
message("Date range: ", min(panel$fecha, na.rm = TRUE), " to ", max(panel$fecha, na.rm = TRUE))

# -----------------------------
# 2) Scenario mapping (q1/q2/q3)
# -----------------------------
scenario_map <- tibble(
  escenario = c("q1", "q2", "q3"),
  price_col = c("precio_100g_q1", "precio_100g_q2", "precio_100g_q3")
)

# -----------------------------
# 3) Nutrient rename for FoodpriceR::HCost
# -----------------------------
panel2 <- panel %>%
  rename(
    Energy        = energia_kcal,
    Protein       = proteina_g,
    Lipids        = lipidos_g,
    Carbohydrates = carbohidratos_totales_g,
    VitaminC      = vitamina_c_mg,
    Folate        = folatos_mcg,
    VitaminA      = vitamina_a_er,
    Thiamine      = tiamina_mg,
    Riboflavin    = riboflavina_mg,
    Niacin        = niacina_mg,
    VitaminB12    = vitamina_b12_mcg,
    Magnesium     = magnesio_mg,
    Phosphorus    = fosforo_mg,
    Sodium        = sodio_mg,
    Calcium       = calcio_mg,
    Iron          = hierro_mg,
    Zinc          = zinc_mg
  )

nutr_cols <- c(
  "Energy","Protein","Lipids","Carbohydrates","VitaminC","Folate","VitaminA",
  "Thiamine","Riboflavin","Niacin","VitaminB12","Magnesium","Phosphorus",
  "Sodium","Calcium","Iron","Zinc"
)

# -----------------------------
# 4) Helper: compute HCost city x date x scenario
# -----------------------------
compute_hcost_city_date_scenario <- function(city.x, date.x, escenario, price_col, panel_city) {
  
  df.aux <- panel_city %>%
    filter(fecha == date.x) %>%
    transmute(
      Food       = alimento_sipsa,
      Price_100g = .data[[price_col]],
      Serving    = 100,
      across(all_of(nutr_cols), ~ .x)
    ) %>%
    filter(
      !is.na(Price_100g),
      !is.na(Energy),
      Price_100g > 0,
      Energy > 0
    )
  
  if (nrow(df.aux) == 0) return(NULL)
  
  # Collapse duplicates by Food (recommended)
  df.aux <- df.aux %>%
    group_by(Food) %>%
    summarise(
      Price_100g = mean(Price_100g, na.rm = TRUE),
      Serving = 100,
      across(all_of(nutr_cols), ~ mean(.x, na.rm = TRUE)),
      .groups = "drop"
    )
  
  hcost.aux <- FoodpriceR::HCost(
    Data      = df.aux,
    ERR       = EER,
    EER_LL    = EER_LL,
    UL        = UL,
    Household = FoodpriceR::Household
  )
  
  coca <- hcost.aux$Model_CoCA %>%
    mutate(ciudad = city.x, fecha = date.x, escenario = escenario)
  
  cona <- hcost.aux$Model_CoNA %>%
    mutate(ciudad = city.x, fecha = date.x, escenario = escenario)
  
  list(CoCA = coca, CoNA = cona)
}

# -----------------------------
# 5) Loop city x month x scenario
# -----------------------------
city_vector <- sort(unique(panel2$ciudad))

res_coca <- list()
res_cona <- list()
fail_hcost <- list()

k_coca <- 1
k_cona <- 1

for (cc in city_vector) {
  
  message("== HCost city: ", cc)
  
  panel_c <- panel2 %>%
    filter(ciudad == cc) %>%
    arrange(fecha)
  
  fechas <- sort(unique(panel_c$fecha))
  
  for (k in 1:length(fechas)) {
    
    f <- fechas[k]
    
    for (s in seq_len(nrow(scenario_map))) {
      
      esc  <- scenario_map$escenario[s]
      pcol <- scenario_map$price_col[s]
      
      out <- tryCatch({
        compute_hcost_city_date_scenario(
          city.x = cc,
          date.x = f,
          escenario = esc,
          price_col = pcol,
          panel_city = panel_c
        )
      }, error = function(e) {
        fail_hcost[[length(fail_hcost) + 1]] <- tibble(
          ciudad = cc, fecha = f, escenario = esc, motivo = e$message
        )
        NULL
      })
      
      if (is.null(out)) {
        fail_hcost[[length(fail_hcost) + 1]] <- tibble(
          ciudad = cc, fecha = f, escenario = esc,
          motivo = "No result (NULL): no valid foods or upstream issue"
        )
        next
      }
      
      res_coca[[k_coca]] <- out$CoCA; k_coca <- k_coca + 1
      res_cona[[k_cona]] <- out$CoNA; k_cona <- k_cona + 1
    }
  }
}

coca_df <- if (length(res_coca) == 0) tibble() else bind_rows(res_coca)
cona_df <- if (length(res_cona) == 0) tibble() else bind_rows(res_cona)
fail_df <- if (length(fail_hcost) == 0) tibble() else bind_rows(fail_hcost)

# -----------------------------
# 6) Save outputs (new folder)
# -----------------------------
afford_dir <- file.path(q2_out_dir, "affordability_qtiles")
dir.create(afford_dir, recursive = TRUE, showWarnings = FALSE)

saveRDS(coca_df, file.path(afford_dir, "HCost_Model_CoCA_city_month_q1_q2_q3.rds"))
saveRDS(cona_df, file.path(afford_dir, "HCost_Model_CoNA_city_month_q1_q2_q3.rds"))
saveRDS(fail_df, file.path(afford_dir, "HCost_failures_q1_q2_q3.rds"))

write_csv(coca_df, file.path(afford_dir, "HCost_Model_CoCA_city_month_q1_q2_q3.csv"))
write_csv(cona_df, file.path(afford_dir, "HCost_Model_CoNA_city_month_q1_q2_q3.csv"))
write_csv(fail_df, file.path(afford_dir, "HCost_failures_q1_q2_q3.csv"))

write_xlsx(
  list(CoCA = coca_df, CoNA = cona_df, failures = fail_df),
  file.path(afford_dir, "HCost_city_month_q1_q2_q3.xlsx")
)

message("Saved HCost outputs in: ", afford_dir)
message("Rows CoCA: ", nrow(coca_df), " | Rows CoNA: ", nrow(cona_df), " | Failures: ", nrow(fail_df))

# -----------------------------
# 7) Plots: Q2 line + Q1–Q3 band, faceted by city
#    (robust to scenario naming via tolower())
# -----------------------------
coca_df <- coca_df %>% mutate(fecha = as.Date(fecha), escenario = tolower(escenario))
cona_df <- cona_df %>% mutate(fecha = as.Date(fecha), escenario = tolower(escenario))

# --- CoCA summary -> wide: q1, q2, q3 ---
coca_sum <- coca_df %>%
  group_by(ciudad, fecha, escenario) %>%
  dplyr::summarise(per_capita_month = sum(per_capita_month, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    names_from  = escenario,
    values_from = per_capita_month,
    values_fill = NA_real_
  ) %>%
  filter(!is.na(q2))

g_coca <- ggplot(coca_sum, aes(x = fecha)) +
  geom_ribbon(aes(ymin = q1, ymax = q3), alpha = 0.25) +
  geom_line(aes(y = q2), linewidth = 0.8) +
  facet_wrap(~ ciudad, scales = "free_y") +
  labs(
    title = "CoCA (HCost): Costo mensual per cápita",
    subtitle = "Línea: Q2 (mediana) | Banda: Q1–Q3",
    x = "Fecha",
    y = "Costo per cápita mensual"
  ) +
  theme_classic()

ggsave(
  filename = file.path(afford_dir, "CoCA_per_capita_month_Q1_Q2_Q3_band_by_city.png"),
  plot     = g_coca,
  width    = 12, height = 5
)

# --- CoNA summary -> wide: q1, q2, q3 ---
cona_sum <- cona_df %>%
  group_by(ciudad, fecha, escenario) %>%
  dplyr::summarise(per_capita_month = sum(per_capita_month, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    names_from  = escenario,
    values_from = per_capita_month,
    values_fill = NA_real_
  ) %>%
  filter(!is.na(q2))

g_cona <- ggplot(cona_sum, aes(x = fecha)) +
  geom_ribbon(aes(ymin = q1, ymax = q3), alpha = 0.25) +
  geom_line(aes(y = q2), linewidth = 0.8) +
  facet_wrap(~ ciudad, scales = "free_y") +
  labs(
    title = "CoNA (HCost): Costo mensual per cápita",
    subtitle = "Línea: Q2 (mediana) | Banda: Q1–Q3",
    x = "Fecha",
    y = "Costo per cápita mensual"
  ) +
  theme_classic()

ggsave(
  filename = file.path(afford_dir, "CoNA_per_capita_month_Q1_Q2_Q3_band_by_city.png"),
  plot     = g_cona,
  width    = 12, height = 5
)

message("Saved band plots (Q1–Q3 + Q2 line) in: ", afford_dir)
