#----------------------------------------------------------------------
# Paquetes
#----------------------------------------------------------------------

library(devtools)
library(tidyverse)
library(FoodpriceR)

#----------------------------------------------------------------------
# Directorios
#----------------------------------------------------------------------
base_dir <- "C:/Users/Portatil/Desktop/Least-cost-diets-and-affordability/Proyecto Interno"

out_dir <- file.path(base_dir, "working-papers/working-paper-ipc/output/least_cost_metrics")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

tmp_dir <- file.path(out_dir, "tmp")
dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)

# panel en tmp_dir (ajusta a rds o csv según lo que uses)
# panel <- readRDS(file.path(tmp_dir, "panel_city_month_food_1999_2025.rds"))
panel <- read.csv(file.path(tmp_dir, "panel_city_month_food_1999_2025.csv"))

#----------------------------------------------------------------------
# Vectores de ciudad y fecha
#----------------------------------------------------------------------
city_vector <- sort(unique(panel$ciudad))
date_vector <- sort(unique(panel$fecha))

#----------------------------------------------------------------------
# Columnas nutricionales
#----------------------------------------------------------------------
nutr_cols <- c(
  "Energy","Protein","Lipids","Carbohydrates","VitaminC","Folate","VitaminA",
  "Thiamine","Riboflavin","Niacin","VitaminB12","Magnesium","Phosphorus",
  "Sodium","Calcium","Iron","Zinc"
)

#----------------------------------------------------------------------
# Función auxiliar para construir el panel.aux y calcular HCost
#----------------------------------------------------------------------
compute_hcost_city_date <- function(city.x, date.x, panel) {
  
  panel.aux <- panel %>% 
    filter(ciudad == city.x,
           fecha  == date.x) %>%
    dplyr::rename(
      Energy       = energia_kcal,
      Protein      = proteina_g,
      Lipids       = lipidos_g,
      Carbohydrates = carbohidratos_totales_g,
      VitaminC     = vitamina_c_mg,
      Folate       = folatos_mcg,
      VitaminA     = vitamina_a_er,
      Thiamine     = tiamina_mg,
      Riboflavin   = riboflavina_mg,
      Niacin       = niacina_mg,
      VitaminB12   = vitamina_b12_mcg,
      Magnesium    = magnesio_mg,
      Phosphorus   = fosforo_mg,
      Sodium       = sodio_mg,
      Calcium      = calcio_mg,
      Iron         = hierro_mg,
      Zinc         = zinc_mg
    ) %>%
    transmute(
      Food       = articulo,
      Price_100g = precio_100g,
      Serving    = 100,
      across(all_of(nutr_cols), ~ .x)
    ) %>%
    filter(
      !is.na(Price_100g),
      !is.na(Energy),
      Price_100g > 0,
      Energy > 0
    )
  
  if (nrow(panel.aux) == 0) {
    return(NULL)
  }
  
  # IMPORTANTE: asegúrate de que EER, EER_LL y UL estén definidos
  # (por ejemplo: EER <- FoodpriceR::EER; etc., si el paquete los trae)
  hcost.aux <- FoodpriceR::HCost(
    Data      = panel.aux,
    ERR       = EER,
    EER_LL    = EER_LL,
    UL        = UL,
    Household = FoodpriceR::Household
  )
  
  # Añadimos ciudad y fecha a las dos tablas
  coca <- hcost.aux$Model_CoCA %>%
    mutate(ciudad = city.x,
           fecha  = date.x)
  
  cona <- hcost.aux$Model_CoNA %>%
    mutate(ciudad = city.x,
           fecha  = date.x)
  
  list(CoCA = coca, CoNA = cona)
}

#----------------------------------------------------------------------
# Bucle sobre todas las ciudades y fechas
#----------------------------------------------------------------------
res_coca <- list()
res_cona <- list()
k_coca   <- 1
k_cona   <- 1

for (city.x in city_vector) {
  for (date.x in date_vector) {

    message("Procesando ciudad = ", city.x, 
            " | fecha = ", date.x, " ...")
    
    out <- try(
      compute_hcost_city_date(city.x, date.x, panel),
      silent = TRUE
    )
    
    if (inherits(out, "try-error") || is.null(out)) {
      message("   -> sin resultado (error o sin datos suficientes).")
      next
    }
    
    res_coca[[k_coca]] <- out$CoCA
    res_cona[[k_cona]] <- out$CoNA
    k_coca <- k_coca + 1
    k_cona <- k_cona + 1
  }
}

# Consolidar en data.frames
coca_df <- bind_rows(res_coca)
cona_df <- bind_rows(res_cona)

#----------------------------------------------------------------------
# Guardar resultados en directorio de AFFORDABILITY
#----------------------------------------------------------------------
afford_dir <- file.path(
  base_dir,
  "working-papers/working-paper-ipc/output/affordability"
)
dir.create(afford_dir, recursive = TRUE, showWarnings = FALSE)

saveRDS(coca_df, file = file.path(afford_dir, "CoCA_city_month.rds"))
saveRDS(cona_df, file = file.path(afford_dir, "CoNA_city_month.rds"))

write.csv(coca_df, file = file.path(afford_dir, "CoCA_city_month.csv"),
          row.names = FALSE)
write.csv(cona_df, file = file.path(afford_dir, "CoNA_city_month.csv"),
          row.names = FALSE)

#----------------------------------------------------------------------
# Gráficos de visualización
#   1) CoCA: costo per_capita_month por ciudad y tiempo
#   2) CoNA: idem
#   3) Comparación CoCA vs CoNA (scatter)
#----------------------------------------------------------------------
# Aseguramos que fecha sea fecha real (si viene como carácter)
if (!inherits(coca_df$fecha, "Date")) {
  suppressWarnings({
    coca_df$fecha <- as.Date(coca_df$fecha)
    cona_df$fecha <- as.Date(cona_df$fecha)
  })
}

# 1) CoCA: promedio per_capita_month por ciudad-fecha
coca_sum <- coca_df %>%
  group_by(ciudad, fecha) %>%
  dplyr::summarise(per_capita_month = sum(per_capita_month, na.rm = TRUE),
            .groups = "drop")

g_coca <- ggplot(coca_sum,
                 aes(x = fecha, y = per_capita_month, color = ciudad)) +
  geom_line() +
  labs(
    title = "CoCA: Costo mensual per cápita de la dieta asequible",
    x = "Fecha",
    y = "Costo per cápita mensual",
    color = "Ciudad"
  ) +
  theme_classic()

ggsave(
  filename = file.path(afford_dir, "CoCA_per_capita_month_city_time.png"),
  plot     = g_coca,
  width    = 9, height = 5
)

# 2) CoNA: promedio per_capita_month por ciudad-fecha
cona_sum <- cona_df %>%
  group_by(ciudad, fecha) %>%
  dplyr::summarise(per_capita_month = sum(per_capita_month, na.rm = TRUE),
            .groups = "drop")

g_cona <- ggplot(cona_sum,
                 aes(x = fecha, y = per_capita_month, color = ciudad)) +
  geom_line() +
  labs(
    title = "CoNA: Costo mensual per cápita de la dieta nutritiva",
    x = "Fecha",
    y = "Costo per cápita mensual",
    color = "Ciudad"
  ) +
  theme_classic()

ggsave(
  filename = file.path(afford_dir, "CoNA_per_capita_month_city_time.png"),
  plot     = g_cona,
  width    = 9, height = 5
)

