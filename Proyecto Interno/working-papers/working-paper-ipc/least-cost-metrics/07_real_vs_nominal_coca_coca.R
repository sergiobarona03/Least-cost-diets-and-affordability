#######################################################################
## FIGURE 2: CoCA and CoNA by demographic group (Nominal vs Real) Monthly
## 3 PLOTS:
##   (1) Nominal only
##   (2) Real only
##   (3) Nominal vs Real comparison
#######################################################################

#----------------------------------------------------------------------
# Packages
#----------------------------------------------------------------------
library(tidyverse)
library(FoodpriceR)
library(readxl)
library(lubridate)

#----------------------------------------------------------------------
# Directories
#----------------------------------------------------------------------
base_dir <- "C:\\Users\\danie\\OneDrive\\Escritorio\\Least-cost-diets-and-affordability\\Proyecto Interno\\"
setwd(base_dir)

xlsx_path <- "Precios DANE/OUTPUT_DANE/precios_DANE_deflactados_base2018_12.xlsx"

out_fig2_dir <- file.path(
  base_dir,
  "working-papers/working-paper-ipc/output/least_cost_metrics/figura 2"
)
dir.create(out_fig2_dir, recursive = TRUE, showWarnings = FALSE)

#----------------------------------------------------------------------
# Load data
#----------------------------------------------------------------------
df <- readxl::read_excel(xlsx_path)

df <- df %>%
  mutate(
    fecha = as.Date(paste0(anio_mes, "-01")),
    ciudad = as.character(nombre_ciudad),
    codigo_articulo = as.character(codigo_articulo),
    articulo = as.character(articulo),
    precio_100g_nom  = as.numeric(precio),
    precio_100g_real = as.numeric(precio_real_base2018_12)
  )

#----------------------------------------------------------------------
# Nutritional columns (if already present in df)
#----------------------------------------------------------------------
nutr_cols <- c(
  "Energy","Protein","Lipids","Carbohydrates","VitaminC","Folate","VitaminA",
  "Thiamine","Riboflavin","Niacin","VitaminB12","Magnesium","Phosphorus",
  "Sodium","Calcium","Iron","Zinc"
)

nutr_rename <- c(
  Energy        = "energia_kcal",
  Protein       = "proteina_g",
  Lipids        = "lipidos_g",
  Carbohydrates = "carbohidratos_totales_g",
  VitaminC      = "vitamina_c_mg",
  Folate        = "folatos_mcg",
  VitaminA      = "vitamina_a_er",
  Thiamine      = "tiamina_mg",
  Riboflavin    = "riboflavina_mg",
  Niacin        = "niacina_mg",
  VitaminB12    = "vitamina_b12_mcg",
  Magnesium     = "magnesio_mg",
  Phosphorus    = "fosforo_mg",
  Sodium        = "sodio_mg",
  Calcium       = "calcio_mg",
  Iron          = "hierro_mg",
  Zinc          = "zinc_mg"
)

#----------------------------------------------------------------------
# Function: HCost by city-date using nominal or real prices
#----------------------------------------------------------------------
compute_hcost_city_date <- function(city.x, date.x, df,
                                    price_col = c("precio_100g_nom","precio_100g_real")) {
  
  price_col <- match.arg(price_col)
  
  panel.aux <- df %>%
    filter(ciudad == city.x, fecha == date.x) %>%
    rename(!!!setNames(names(nutr_rename), nutr_rename)) %>%
    transmute(
      Food       = articulo,
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
  
  if (nrow(panel.aux) == 0) return(NULL)
  
  hcost.aux <- FoodpriceR::HCost(
    Data      = panel.aux,
    ERR       = EER,
    EER_LL    = EER_LL,
    UL        = UL,
    Household = FoodpriceR::Household
  )
  
  coca <- hcost.aux$Model_CoCA %>% mutate(ciudad = city.x, fecha = date.x)
  cona <- hcost.aux$Model_CoNA %>% mutate(ciudad = city.x, fecha = date.x)
  
  list(CoCA = coca, CoNA = cona)
}

#----------------------------------------------------------------------
# Vectors
#----------------------------------------------------------------------
city_vector <- sort(unique(df$ciudad))
date_vector <- sort(unique(df$fecha))

#----------------------------------------------------------------------
# LOOP: NOMINAL
#----------------------------------------------------------------------
res_coca_nom <- list(); res_cona_nom <- list()
k1 <- 1; k2 <- 1

for (city.x in city_vector) {
  for (date.x in date_vector) {
    message("NOMINAL | city=", city.x, " | date=", date.x)
    
    out <- try(compute_hcost_city_date(city.x, date.x, df, "precio_100g_nom"), silent = TRUE)
    if (inherits(out, "try-error") || is.null(out)) next
    
    res_coca_nom[[k1]] <- out$CoCA
    res_cona_nom[[k2]] <- out$CoNA
    k1 <- k1 + 1; k2 <- k2 + 1
  }
}

coca_nom_df <- bind_rows(res_coca_nom) %>% mutate(price_type = "Nominal")
cona_nom_df <- bind_rows(res_cona_nom) %>% mutate(price_type = "Nominal")

#----------------------------------------------------------------------
# LOOP: REAL
#----------------------------------------------------------------------
res_coca_real <- list(); res_cona_real <- list()
k3 <- 1; k4 <- 1

for (city.x in city_vector) {
  for (date.x in date_vector) {
    message("REAL | city=", city.x, " | date=", date.x)
    
    out <- try(compute_hcost_city_date(city.x, date.x, df, "precio_100g_real"), silent = TRUE)
    if (inherits(out, "try-error") || is.null(out)) next
    
    res_coca_real[[k3]] <- out$CoCA
    res_cona_real[[k4]] <- out$CoNA
    k3 <- k3 + 1; k4 <- k4 + 1
  }
}

coca_real_df <- bind_rows(res_coca_real) %>% mutate(price_type = "Real (2018-12 base)")
cona_real_df <- bind_rows(res_cona_real) %>% mutate(price_type = "Real (2018-12 base)")

#----------------------------------------------------------------------
# Helper: robust date handling for plots
#   - Uses `fecha` if present
#   - Otherwise tries common alternatives
#----------------------------------------------------------------------
ensure_fecha <- function(d) {
  if ("fecha" %in% names(d)) return(d)
  

  candidates <- intersect(
    names(d),
    c("Fecha", "FECHA","date", "Date", "DATE", "month", "Month", "MONTH", "time", "Time")
  )
  
  if (length(candidates) == 0) {
    stop("plot_demo(): No date column found. Available columns are: ",
         paste(names(d), collapse = ", "))
  }
  
  d %>% mutate(fecha = as.Date(.data[[candidates[1]]]))
}

#----------------------------------------------------------------------
# Plot helper (same structure for all three figures)
#----------------------------------------------------------------------
plot_demo <- function(df_in, title_txt, yvar = "per_capita_month") {
  
  d <- df_in %>%
    ensure_fecha() %>%
    mutate(
      fecha = as.Date(fecha),
      Demo_Group = as.factor(Demo_Group),
      ciudad = as.character(ciudad)
    ) %>%
    group_by(ciudad, fecha, Demo_Group, price_type) %>%
    summarise(val = sum(.data[[yvar]], na.rm = TRUE), .groups = "drop")
  
  ggplot(d, aes(x = fecha, y = val, color = price_type)) +
    geom_line(linewidth = 0.6) +
    facet_grid(Demo_Group ~ ciudad, scales = "free_y") +
    labs(
      title = title_txt,
      x = "Date",
      y = "Monthly per capita cost",
      color = NULL
    ) +
    theme_classic()
}

#----------------------------------------------------------------------
# 3 PLOTS: NOMINAL, REAL, COMPARISON (CoCA and CoNA)
#----------------------------------------------------------------------
# ---- CoCA
g_coca_nom <- plot_demo(
  coca_nom_df,
  "Figure 2 — CoCA (Nominal) — Monthly evolution by demographic group"
)
ggsave(file.path(out_fig2_dir, "Figure2_CoCA_Nominal.png"),
       g_coca_nom, width = 16, height = 10)

g_coca_real <- plot_demo(
  coca_real_df,
  "Figure 2 — CoCA (Real) — Monthly evolution by demographic group"
)
ggsave(file.path(out_fig2_dir, "Figure2_CoCA_Real.png"),
       g_coca_real, width = 16, height = 10)

g_coca_comp <- plot_demo(
  bind_rows(coca_nom_df, coca_real_df),
  "Figure 2 — CoCA — Nominal vs Real comparison by demographic group"
)
ggsave(file.path(out_fig2_dir, "Figure2_CoCA_Nominal_vs_Real.png"),
       g_coca_comp, width = 16, height = 10)

# ---- CoNA
g_cona_nom <- plot_demo(
  cona_nom_df,
  "Figure 2 — CoNA (Nominal) — Monthly evolution by demographic group"
)
ggsave(file.path(out_fig2_dir, "Figure2_CoNA_Nominal.png"),
       g_cona_nom, width = 16, height = 10)

g_cona_real <- plot_demo(
  cona_real_df,
  "Figure 2 — CoNA (Real) — Monthly evolution by demographic group"
)
ggsave(file.path(out_fig2_dir, "Figure2_CoNA_Real.png"),
       g_cona_real, width = 16, height = 10)

g_cona_comp <- plot_demo(
  bind_rows(cona_nom_df, cona_real_df),
  "Figure 2 — CoNA — Nominal vs Real comparison by demographic group"
)
ggsave(file.path(out_fig2_dir, "Figure2_CoNA_Nominal_vs_Real.png"),
       g_cona_comp, width = 16, height = 10)

#----------------------------------------------------------------------
# Save tables
#----------------------------------------------------------------------
saveRDS(coca_nom_df,  file.path(out_fig2_dir, "CoCA_city_month_nominal.rds"))
saveRDS(coca_real_df, file.path(out_fig2_dir, "CoCA_city_month_real.rds"))
saveRDS(cona_nom_df,  file.path(out_fig2_dir, "CoNA_city_month_nominal.rds"))
saveRDS(cona_real_df, file.path(out_fig2_dir, "CoNA_city_month_real.rds"))

write.csv(coca_nom_df,  file.path(out_fig2_dir, "CoCA_city_month_nominal.csv"), row.names = FALSE)
write.csv(coca_real_df, file.path(out_fig2_dir, "CoCA_city_month_real.csv"), row.names = FALSE)
write.csv(cona_nom_df,  file.path(out_fig2_dir, "CoNA_city_month_nominal.csv"), row.names = FALSE)
write.csv(cona_real_df, file.path(out_fig2_dir, "CoNA_city_month_real.csv"), row.names = FALSE)

