# ============================================================
# Least-cost metrics (CoCA / CoNA) FoodpriceR::HCost - Monthly
# ============================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(FoodpriceR)
  library(scales)
})

# ------------------------------------------------------------
# Directorios
# ------------------------------------------------------------
base_dir <- "C:/Users/danie/OneDrive/Escritorio/Least-cost-diets-and-affordability/Proyecto Interno/"

out_dir <- file.path(base_dir,
                     "working-papers/working-paper-ipc/output/least_cost_metrics")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

tmp_dir <- file.path(out_dir, "tmp")

fig_dir <- file.path(out_dir, "figures")
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

log_dir <- file.path(out_dir, "logs")
dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------
# Config
# ------------------------------------------------------------
source(file.path(base_dir,
                 "working-papers/working-paper-ipc/least-cost-metrics/00_config.R"))

# Para replicar el script que te funciona:
Household_obj <- FoodpriceR::Household

# ------------------------------------------------------------
# Panel
# ------------------------------------------------------------
panel <- readRDS(file.path(tmp_dir, "panel_city_month_food_1999_2025.rds"))
panel$fecha  <- as.Date(panel$fecha)
panel$ciudad <- as.character(panel$ciudad)

city_vector <- sort(unique(panel$ciudad))
date_vector <- sort(unique(panel$fecha))

nutr_cols <- c(
  "Energy","Protein","Lipids","Carbohydrates","VitaminC","Folate","VitaminA",
  "Thiamine","Riboflavin","Niacin","VitaminB12","Magnesium","Phosphorus",
  "Sodium","Calcium","Iron","Zinc"
)

# ------------------------------------------------------------
# Paper theme helper
# ------------------------------------------------------------
theme_paper <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title.position = "plot",
      plot.title = element_text(face = "bold"),
      legend.position = "top",
      legend.title = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      strip.background = element_rect(fill = "white", color = "black", linewidth = 0.4),
      strip.text = element_text(face = "bold"),
      axis.title.x = element_blank(),
      axis.text.x = element_text(angle = 0)
    )
}

# ------------------------------------------------------------
# Normalizar ciudades 
# ------------------------------------------------------------
std_city <- function(x) {
  x <- as.character(x)
  dplyr::case_when(
    x %in% c("BOGOTÁ D.C.", "BOGOTA D.C.", "BOGOTA") ~ "BOGOTA",
    x %in% c("MEDELLÍN", "MEDELLIN")                 ~ "MEDELLIN",
    x %in% c("CALI")                                 ~ "CALI",
    TRUE ~ x
  )
}
city_levels <- c("BOGOTA", "CALI", "MEDELLIN")

# ------------------------------------------------------------
# FUNCIÓN HCost 
# ------------------------------------------------------------
compute_hcost_city_date <- function(city.x, date.x, panel) {
  
  panel.aux <- panel %>%
    dplyr::filter(ciudad == city.x, fecha == date.x) %>%
    dplyr::rename(
      Energy          = energia_kcal,
      Protein         = proteina_g,
      Lipids          = lipidos_g,
      Carbohydrates   = carbohidratos_totales_g,
      VitaminC        = vitamina_c_mg,
      Folate          = folatos_mcg,
      VitaminA        = vitamina_a_er,
      Thiamine        = tiamina_mg,
      Riboflavin      = riboflavina_mg,
      Niacin          = niacina_mg,
      VitaminB12      = vitamina_b12_mcg,
      Magnesium       = magnesio_mg,
      Phosphorus      = fosforo_mg,
      Sodium          = sodio_mg,
      Calcium         = calcio_mg,
      Iron            = hierro_mg,
      Zinc            = zinc_mg
    ) %>%
    dplyr::transmute(
      Food       = articulo,
      Price_100g = precio_100g,
      Serving    = 100,
      dplyr::across(dplyr::all_of(nutr_cols), ~ .x)
    ) %>%
    dplyr::filter(
      !is.na(Price_100g),
      !is.na(Energy),
      Price_100g > 0,
      Energy > 0
    )
  
  if (nrow(panel.aux) == 0) return(NULL)
  
  hcost.aux <- FoodpriceR::HCost(
    Data      = panel.aux,
    ERR       = EER,
    EER_LL    = EER_LL,
    UL        = UL,
    Household = Household_obj
  )
  
  coca <- hcost.aux$Model_CoCA %>%
    mutate(ciudad = city.x, fecha = date.x, dieta = "CoCA")
  
  cona <- hcost.aux$Model_CoNA %>%
    mutate(ciudad = city.x, fecha = date.x, dieta = "CoNA")
  
  list(CoCA = coca, CoNA = cona)
}

# ------------------------------------------------------------
# LOOP
# ------------------------------------------------------------
res_coca <- list()
res_cona <- list()
err_log  <- list()
k_coca <- 1; k_cona <- 1; k_err <- 1

for (city.x in city_vector) {
  for (date.x in date_vector) {
    
    message("Procesando ciudad = ", city.x, " | fecha = ", date.x, " ...")
    
    out <- tryCatch(
      compute_hcost_city_date(city.x, date.x, panel),
      error = function(e) {
        err_log[[k_err]] <<- tibble(ciudad = city.x, fecha = date.x, error = conditionMessage(e))
        message("   -> ERROR HCost: ", conditionMessage(e))
        k_err <<- k_err + 1
        return(NULL)
      }
    )
    
    if (is.null(out)) {
      message("   -> NULL (sin datos suficientes) en: ", city.x, " | ", as.character(date.x))
      next
    }
    
    res_coca[[k_coca]] <- out$CoCA
    res_cona[[k_cona]] <- out$CoNA
    k_coca <- k_coca + 1
    k_cona <- k_cona + 1
  }
}

coca_df <- bind_rows(res_coca)
cona_df <- bind_rows(res_cona)
errors_df <- bind_rows(err_log)

# Normalizar ciudad + niveles 
coca_df <- coca_df %>% mutate(ciudad = factor(std_city(ciudad), levels = city_levels))
cona_df <- cona_df %>% mutate(ciudad = factor(std_city(ciudad), levels = city_levels))

# Guardar outputs 
saveRDS(coca_df, file.path(out_dir, "CoCA_city_month.rds"))
saveRDS(cona_df, file.path(out_dir, "CoNA_city_month.rds"))

message("FIN HCost. CoCA rows=", nrow(coca_df), " | CoNA rows=", nrow(cona_df), " | errores=", nrow(errors_df))

# ============================================================
# 3 COSTOS 
# ============================================================

pick_col <- function(df, candidates) {
  hit <- intersect(names(df), candidates)
  if (length(hit) >= 1) hit[1] else NA_character_
}

ensure_3costs <- function(df) {
  df <- as_tibble(df)
  
  # 1) month 
  c_month <- pick_col(df, c("per_capita_month", "percapita_month"))
  
  # 2) day
  c_day <- pick_col(df, c("per_capita_day","percapita_day","cost_day","Cost_day","cost_daily"))
  
  # 3) 1000 kcal
  c_kcal <- pick_col(df, c("per_1000kcal_day","per1000kcal_day","Cost_1000kcal","cost_1000kcal","COST_1000KCAL"))
  
  # construir columnas estándar
  df <- df %>%
    mutate(
      per_capita_month = if (!is.na(c_month)) .data[[c_month]] else NA_real_,
      
      per_capita_day = case_when(
        !is.na(c_day) ~ as.numeric(.data[[c_day]]),
        # fallback 1: si existe per_capita_month, convertir a día (aprox 30.4375)
        !is.na(c_month) ~ as.numeric(.data[[c_month]]) / 30.4375,
        TRUE ~ NA_real_
      ),
      
      per_1000kcal_day = if (!is.na(c_kcal)) as.numeric(.data[[c_kcal]]) else NA_real_
    )
  
  df
}

coca_df2 <- ensure_3costs(coca_df)
cona_df2 <- ensure_3costs(cona_df)

stack_costs <- function(df, dieta_label) {
  df %>%
    mutate(dieta = dieta_label) %>%
    pivot_longer(
      cols = c(per_capita_month, per_capita_day, per_1000kcal_day),
      names_to = "costo",
      values_to = "valor"
    )
}

metrics_long <- bind_rows(
  stack_costs(coca_df2, "CoCA"),
  stack_costs(cona_df2, "CoNA")
) %>%
  mutate(
    costo = recode(
      costo,
      per_capita_month = "Per-capita (month)",
      per_capita_day   = "Per-capita (day)",
      per_1000kcal_day = "Per 1000 kcal (day)"
    ),
    fecha = as.Date(fecha),
    ciudad = factor(std_city(as.character(ciudad)), levels = city_levels)
  )

# ------------------------------------------------------------
# Detectar columna grupo 
# ------------------------------------------------------------
group_col <- {
  nms <- names(metrics_long)
  cand <- nms[grepl("Demo_Group|demo_group|group|member|demo|hh|sex|age", nms, ignore.case = TRUE)]
  if (length(cand) >= 1) cand[1] else NA_character_
}
if (is.na(group_col)) {
  cand2 <- intersect(names(metrics_long), c("Group","group","name","Name","HHmember","member"))
  group_col <- if (length(cand2) >= 1) cand2[1] else NA_character_
}
metrics_long <- if (is.na(group_col)) {
  metrics_long %>% mutate(grupo = "ALL")
} else {
  metrics_long %>% mutate(grupo = as.character(.data[[group_col]]))
}

# ============================================================
# FIGURAS
# ============================================================

metrics_long <- metrics_long %>%
  mutate(
    fecha  = as.Date(fecha),
    ciudad = factor(std_city(as.character(ciudad)), levels = city_levels),
    dieta  = factor(as.character(dieta), levels = c("CoCA","CoNA")),
    Person = as.factor(Person)  # Person = 1,2,3
  )

stopifnot("Person" %in% names(metrics_long))

# ============================================================
# FIG 1) 1 figura por costo
# ============================================================

make_fig1_by_cost <- function(cost_label) {
  
  d <- metrics_long %>%
    filter(costo == cost_label, !is.na(valor)) %>%
    distinct(ciudad, fecha, dieta, costo, grupo, Person, valor, .keep_all = TRUE)
  
  if (nrow(d) == 0) return(NULL)
  
  ggplot(
    d,
    aes(
      x = fecha, y = valor,
      group = interaction(Person, grupo),
      color = grupo
    )
  ) +
    geom_line(linewidth = 0.55, alpha = 0.95) +
    facet_grid(rows = vars(dieta), cols = vars(ciudad), scales = "free_y") +
    scale_color_grey(start = 0.25, end = 0.7) +
    scale_x_date(date_labels = "%Y", date_breaks = "2 years") +
    labs(
      title = paste0("Least-cost diet metrics by household member (", cost_label, ")"),
      y = "Cost", x = NULL
    ) +
    theme_paper(12)
}

for (ct in unique(metrics_long$costo)) {
  p <- make_fig1_by_cost(ct)
  if (is.null(p)) next
  
  ggsave(
    filename = file.path(fig_dir, paste0("FIG1_members_", gsub("[^A-Za-z0-9]+","_", ct), ".png")),
    plot = p,
    width = 14, height = 6, dpi = 300
  )
}

# ============================================================
# FIG 2) CoCA vs CoNA por 3 costos 
# ============================================================

fig2_df <- metrics_long %>%
  filter(!is.na(valor)) %>%
  group_by(ciudad, fecha, dieta, costo) %>%
  summarise(valor = mean(valor, na.rm = TRUE), .groups = "drop") %>%
  filter(is.finite(valor))

fig2 <- ggplot(fig2_df, aes(x = fecha, y = valor, linetype = dieta, group = dieta)) +
  geom_line(linewidth = 0.75, color = "black") +
  facet_grid(rows = vars(ciudad), cols = vars(costo), scales = "free_y") +
  scale_linetype_manual(values = c("CoCA" = "solid", "CoNA" = "dashed")) +
  scale_x_date(date_labels = "%Y", date_breaks = "2 years") +
  labs(
    title = "CoCA vs CoNA (3 cost definitions)",
    y = "Cost", x = NULL, linetype = NULL
  ) +
  theme_paper(12)

ggsave(
  filename = file.path(fig_dir, "FIG2_coca_vs_cona_3costs.png"),
  plot = fig2,
  width = 16, height = 7, dpi = 300
)

# ============================================================
# FIG 1B) Todo en una sola figura (miembros)
# ============================================================

d_all <- metrics_long %>%
  filter(!is.na(valor)) %>%
  distinct(ciudad, fecha, dieta, costo, grupo, Person, valor, .keep_all = TRUE)

fig1_all <- ggplot(
  d_all,
  aes(
    x = fecha, y = valor,
    group = interaction(Person, grupo),
    color = grupo
  )
) +
  geom_line(linewidth = 0.45, alpha = 0.95) +
  facet_grid(rows = vars(dieta, costo), cols = vars(ciudad), scales = "free_y") +
  scale_color_grey(start = 0.25, end = 0.7) +
  scale_x_date(date_labels = "%Y", date_breaks = "2 years") +
  labs(
    title = "Least-cost diet metrics by household member (all costs)",
    y = "Cost", x = NULL
  ) +
  theme_paper(10)

ggsave(
  filename = file.path(fig_dir, "FIG1B_all_members_all_costs.png"),
  plot = fig1_all,
  width = 16, height = 10, dpi = 300
)

message("Listo. Figuras paper en: ", fig_dir)