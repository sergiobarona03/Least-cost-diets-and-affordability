########################################################
## SCRIPT 02_models/03_cord.R
## Cost of Recommended Diet (CoRD)
## Loop over cities × dates for paper period
##
## Reads:  PREP_DIR/panel_food_paper.rds
##         PREP_DIR/gaba_exchanges_adj.rds
##         IN_AUX_DIR/CoRD_Herforth.R
##
## Writes: CORD_DIR/cord_results.rds
##         CORD_DIR/cord_results.xlsx
########################################################

source("00_config.R", encoding = "UTF-8")
library(tidyverse)
library(writexl)

source(file.path(IN_AUX_DIR, "CoRD_Herforth.R"))

# -----------------------------------------------------------------------
# Canonical food group vocabulary (must match GABA and food table)
# -----------------------------------------------------------------------
GROUP_CANON <- c(
  "Cereales, raíces, tubérculos y plátanos",
  "Frutas",
  "Verduras",
  "Leche y productos lácteos",
  "Carnes, huevos, leguminosas, frutos secos y semillas",
  "Grasas",
  "Azúcares"
)

# -----------------------------------------------------------------------
# Recode maps
# -----------------------------------------------------------------------
map_group_paper <- c(
  "AZUCARES"                                                     = "Azúcares",
  "CARNES, HUEVOS, LEGUMINOSAS SECAS, FRUTOS SECOS Y SEMILLAS"   = "Carnes, huevos, leguminosas, frutos secos y semillas",
  "CEREALES, RAÍCES, TUBÉRCULOS Y PLÁTANOS"                      = "Cereales, raíces, tubérculos y plátanos",
  "FRUTAS"                                                        = "Frutas",
  "GRASAS"                                                        = "Grasas",
  "LECHE Y PRODUCTOS LACTEOS"                                     = "Leche y productos lácteos",
  "VERDURAS"                                                      = "Verduras"
)

map_group_serv <- c(
  "AZÚCARES"                                                      = "Azúcares",
  "CARNES, HUEVOS, LEGUMINOSAS, FRUTOS SECOS Y SEMILLAS"          = "Carnes, huevos, leguminosas, frutos secos y semillas",
  "CEREALES, RAÍCES, TUBÉRCULOS Y PLÁTANOS"                       = "Cereales, raíces, tubérculos y plátanos",
  "FRUTAS"                                                         = "Frutas",
  "GRASAS"                                                         = "Grasas",
  "LECHE Y PRODUCTOS LÁCTEOS"                                      = "Leche y productos lácteos",
  "VERDURAS"                                                        = "Verduras"
)

# -----------------------------------------------------------------------
# Diversity requirements (Herforth et al.)
# -----------------------------------------------------------------------
div_cord <- tribble(
  ~Group,                                                          ~Number,
  "Cereales, raíces, tubérculos y plátanos",                        3,
  "Frutas",                                                         2,
  "Verduras",                                                       2,
  "Leche y productos lácteos",                                      1,
  "Carnes, huevos, leguminosas, frutos secos y semillas",           2,
  "Grasas",                                                         1,
  "Azúcares",                                                       1
)

# -----------------------------------------------------------------------
# Load inputs
# -----------------------------------------------------------------------
message("Loading inputs...")

data_paper <- readRDS(file.path(PREP_DIR, "panel_food_paper.rds")) %>%
  mutate(
    Group = if_else(subgrupos_gabas == "FRUTAS",   "FRUTAS",   grupos_gabas),
    Group = if_else(subgrupos_gabas == "VERDURAS", "VERDURAS", Group),
    Group = recode(Group, !!!map_group_paper)
  ) %>%
  filter(Group %in% GROUP_CANON) %>%
  mutate(
    Price_serving = precio_100g * gramos_g_1_intercambio_1_intercambio / 100,
    Serving_g     = gramos_g_1_intercambio_1_intercambio
  )

serv_adj <- readRDS(file.path(PREP_DIR, "gaba_exchanges_adj.rds")) %>%
  filter(
    (sex == "Masculino" & rango == "[31,51)") |
      (sex == "Femenino"  & rango == "[31,51)") |
      (sex == "Femenino"  & rango == "[10, 14)")
  ) %>%
  rename(Age = rango, Sex = sex, Group = grupo_principal,
         Serving = n_exchanges_adj) %>%
  mutate(
    Group = recode(Group, !!!map_group_serv),
    Sex   = if_else(Sex == "Masculino", 0L, 1L)
  ) %>%
  filter(Group %in% GROUP_CANON)

message(sprintf("  data_paper: %d rows | serv_adj: %d rows",
                nrow(data_paper), nrow(serv_adj)))

# -----------------------------------------------------------------------
# Loop: CoRD for each city × date
# -----------------------------------------------------------------------
dominios <- sort(unique(serv_adj$ciudad))
fechas   <- sort(unique(data_paper$fecha))

message(sprintf("Estimating CoRD: %d cities × %d dates...",
                length(dominios), length(fechas)))

out_cost <- list()
out_comp <- list()
n_ok <- 0L; n_fail <- 0L

for (i in dominios) {
  
  serv.aux <- serv_adj %>%
    filter(ciudad == i) %>% select(-ciudad, -cod_mun)
  
  for (t in fechas) {
    
    data.aux <- data_paper %>%
      filter(ciudad == i, fecha == t, !is.na(precio_100g)) %>%
      rename(Food = articulo) %>%
      as.data.frame()
    
    if (nrow(data.aux) == 0) next
    
    result <- tryCatch(
      CoRD_Herforth(data    = data.aux,
                    serv    = serv.aux,
                    diverse = div_cord),
      error = function(e) {
        warning("Error — ", i, " | ", t, " | ", conditionMessage(e))
        NULL
      })
    
    if (!is.null(result)) {
      out_cost[[length(out_cost) + 1]] <- result$cost %>%
        mutate(ciudad = i, fecha = t)
      out_comp[[length(out_comp) + 1]] <- result$comp %>%
        mutate(ciudad = i, fecha = t)
      n_ok <- n_ok + 1L
    } else {
      n_fail <- n_fail + 1L
    }
  }
}

df.cost <- bind_rows(out_cost) %>%
  mutate(fecha = as.Date(fecha),
         year  = year(fecha),
         mes   = month(fecha))

df.comp <- bind_rows(out_comp) %>%
  mutate(fecha = as.Date(fecha))

message(sprintf("  Done. %d OK | %d failed | %d cost rows",
                n_ok, n_fail, nrow(df.cost)))

# -----------------------------------------------------------------------
# Save
# -----------------------------------------------------------------------
saveRDS(list(cost = df.cost, comp = df.comp),
        file.path(CORD_DIR, "cord_results.rds"))
write_xlsx(list(cost = df.cost, comp = df.comp),
           file.path(CORD_DIR, "cord_results.xlsx"))

message("Done. Run 04_ccona.R next.")