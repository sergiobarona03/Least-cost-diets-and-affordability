#----------------------------------------------------------------------
# Paquetes
#----------------------------------------------------------------------
library(devtools)
library(tidyverse)
library(FoodpriceR)
library(lubridate)

#----------------------------------------------------------------------
# Directorios
#----------------------------------------------------------------------
base_dir <- "C:\\Users\\danie\\OneDrive\\Escritorio\\Least-cost-diets-and-affordability\\Proyecto Interno\\"

out_dir <- file.path(base_dir, "working-papers/working-paper-ipc/output/least_cost_metrics")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

tmp_dir <- file.path(out_dir, "tmp")
dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)

# ✅ Directorio donde vas a guardar (te faltaba en tu script final)
afford_dir <- file.path(
  base_dir,
  "working-papers/working-paper-ipc/output/affordability"
)
dir.create(afford_dir, recursive = TRUE, showWarnings = FALSE)

# panel <- readRDS(file.path(tmp_dir, "panel_city_month_food_1999_2025.rds"))
panel <- read.csv(file.path(tmp_dir, "panel_city_month_food_1999_2025.csv"))

panel$fecha  <- as.Date(panel$fecha)
panel$ciudad <- as.character(panel$ciudad)

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
  
  # Asegúrate de que EER, EER_LL y UL existan en tu entorno
  hcost.aux <- FoodpriceR::HCost(
    Data      = panel.aux,
    ERR       = EER,
    EER_LL    = EER_LL,
    UL        = UL,
    Household = FoodpriceR::Household
  )
  
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
    
    if (inherits(out, "try-error")) {
      message("   -> ERROR HCost en: ", city.x, " | ", as.character(date.x))
      next
    }
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

# Consolidar en data.frames
coca_df <- bind_rows(res_coca)
cona_df <- bind_rows(res_cona)

#======================================================================
# AGREGACIÓN TRIMESTRAL (MEDIANA)
#======================================================================

# asegurar Date
coca_df$fecha <- as.Date(coca_df$fecha)
cona_df$fecha <- as.Date(cona_df$fecha)

# crear trimestre (YYYYQ#)
coca_q <- coca_df %>%
  mutate(
    year = lubridate::year(fecha),
    q = lubridate::quarter(fecha),
    trimestre = paste0(year, "Q", q)
  )

cona_q <- cona_df %>%
  mutate(
    year = lubridate::year(fecha),
    q = lubridate::quarter(fecha),
    trimestre = paste0(year, "Q", q)
  )

# Agregar métrica 
coca_quarter <- coca_q %>%
  group_by(ciudad, year, q, trimestre) %>%
  dplyr::summarise(
    per_capita_month = median(per_capita_month, na.rm = TRUE),
    .groups = "drop"
  )

cona_quarter <- cona_q %>%
  group_by(ciudad, year, q, trimestre) %>%
  dplyr::summarise(
    per_capita_month = median(per_capita_month, na.rm = TRUE),
    .groups = "drop"
  )

# Guardar trimestral 
saveRDS(coca_quarter, file = file.path(afford_dir, "CoCA_city_cuartiles.rds"))
saveRDS(cona_quarter, file = file.path(afford_dir, "CoNA_city_cuartiles.rds"))

write.csv(coca_quarter, file = file.path(afford_dir, "CoCA_city_cuartiles.csv"),
          row.names = FALSE)
write.csv(cona_quarter, file = file.path(afford_dir, "CoNA_city_cuartiles.csv"),
          row.names = FALSE)

#----------------------------------------------------------------------
# Gráficos TRIMESTRALES
#   per_capita_month (mediana trimestral) por ciudad y trimestre
#----------------------------------------------------------------------

# CoCA
g_coca_q <- ggplot(coca_quarter,
                   aes(x = trimestre, y = per_capita_month, color = ciudad, group = ciudad)) +
  geom_line() +
  labs(
    title = "CoCA: Costo per cápita (mediana trimestral)",
    x = "Trimestre",
    y = "Costo per cápita",
    color = "Ciudad"
  ) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(
  filename = file.path(afford_dir, "CoCA_per_capita_cuartiles_city_time.png"),
  plot     = g_coca_q,
  width    = 9, height = 5
)

# CoNA
g_cona_q <- ggplot(cona_quarter,
                   aes(x = trimestre, y = per_capita_month, color = ciudad, group = ciudad)) +
  geom_line() +
  labs(
    title = "CoNA: Costo per cápita (mediana trimestral)",
    x = "Trimestre",
    y = "Costo per cápita",
    color = "Ciudad"
  ) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(
  filename = file.path(afford_dir, "CoNA_per_capita_cuartiles_city_time.png"),
  plot     = g_cona_q,
  width    = 9, height = 5
)
