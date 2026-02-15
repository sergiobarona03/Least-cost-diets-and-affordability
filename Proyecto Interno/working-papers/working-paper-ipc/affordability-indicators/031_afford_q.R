#######################################################################
## Afford TRIMESTRAL: Income trimestral + CoCA/CoNA trimestral
#######################################################################

#----------------------------------------------------------------------
# Paquetes
#----------------------------------------------------------------------
library(tidyverse)
library(lubridate)
library(janitor)

#----------------------------------------------------------------------
# Directorios
#----------------------------------------------------------------------
base_dir <- "C:\\Users\\danie\\OneDrive\\Escritorio\\Least-cost-diets-and-affordability\\Proyecto Interno\\"
setwd(base_dir)

source("working-papers/working-paper-ipc/affordability-indicators/aux_functions/Afford_Expansion.R")

out_dir <- file.path(base_dir, "working-papers/working-paper-ipc/output")

income_dir         <- file.path(out_dir, "incomecol")
afford_cost_dir    <- file.path(out_dir, "affordability")
afford_metrics_dir <- file.path(out_dir, "affordability_metrics")
dir.create(afford_metrics_dir, recursive = TRUE, showWarnings = FALSE)

#----------------------------------------------------------------------
# 1) Income TRIMESTRAL 
#----------------------------------------------------------------------
income_files <- list.files(
  income_dir,
  pattern = "^IncomeCol_quartiles_.*\\.rds$",
  full.names = TRUE
)

income_df <- income_files %>%
  lapply(readRDS) %>%
  bind_rows()

# Normalizar ciudad + trimestre
income_df <- income_df %>%
  mutate(
    ciudad = dominio,
    trimestre = if ("trimestre" %in% names(.)) as.character(trimestre) else paste0(year, "Q", q),
    year = if ("year" %in% names(.)) as.integer(year) else as.integer(str_extract(trimestre, "^\\d{4}")),
    q    = if ("q" %in% names(.)) as.integer(q) else as.integer(str_extract(trimestre, "(?<=Q)\\d+"))
  )

income_df$ciudad[income_df$ciudad == "MEDELLIN"] <- "MEDELLÍN"
income_df$ciudad[income_df$ciudad == "BOGOTA"]   <- "BOGOTÁ D.C."

# Asegurar deciles como "Decil X"
if ("deciles" %in% names(income_df)) {
  if (!is.character(income_df$deciles) || !all(grepl("^Decil\\s", income_df$deciles))) {
    income_df$deciles <- paste0("Decil ", income_df$deciles)
  }
}

#----------------------------------------------------------------------
# 1.1) Compatibilidad con tu Afford_expansion()
#----------------------------------------------------------------------
income_df <- income_df %>%
  mutate(
    ung = if ("ung" %in% names(.)) ung else if ("nug" %in% names(.)) nug else ung,
    fex_c18 = if ("fex_c18" %in% names(.)) fex_c18 else if ("fex_c" %in% names(.)) as.numeric(fex_c) else 1,
    food_income = if ("food_income" %in% names(.)) food_income else if ("food_exp" %in% names(.)) food_exp else NA_real_,
    food_exp_per_capita_year = if ("food_exp_per_capita_year" %in% names(.)) food_exp_per_capita_year else (food_exp_per_capita * 12)
  )

needed_hexp <- c(
  "deciles","income","ung","per_capita_income","food_exp_per_capita",
  "food_exp_per_capita_year","share","fex_c18","food_income"
)
missing_hexp <- setdiff(needed_hexp, names(income_df))
if (length(missing_hexp) > 0) {
  stop("A Income trimestral le faltan columnas requeridas por Afford_expansion(): ",
       paste(missing_hexp, collapse = ", "))
}

#----------------------------------------------------------------------
# 2) CoCA/CoNA TRIMESTRAL 
#----------------------------------------------------------------------
coca_q_path <- file.path(afford_cost_dir, "CoCA_city_cuartiles.rds")
cona_q_path <- file.path(afford_cost_dir, "CoNA_city_cuartiles.rds")

if (!file.exists(coca_q_path)) stop("No encuentro: ", coca_q_path)
if (!file.exists(cona_q_path)) stop("No encuentro: ", cona_q_path)

coca_df <- readRDS(coca_q_path)
cona_df <- readRDS(cona_q_path)

coca_df$ciudad <- as.character(coca_df$ciudad)
cona_df$ciudad <- as.character(cona_df$ciudad)

# asegurar trimestre si no está
if (!("trimestre" %in% names(coca_df))) coca_df <- coca_df %>% mutate(trimestre = paste0(year, "Q", q))
if (!("trimestre" %in% names(cona_df))) cona_df <- cona_df %>% mutate(trimestre = paste0(year, "Q", q))

coca_df$trimestre <- as.character(coca_df$trimestre)
cona_df$trimestre <- as.character(cona_df$trimestre)

#----------------------------------------------------------------------
# 2.1) Compatibilidad con Afford_expansion(): necesita columna 'per_capita'
#   - si solo tienes per_capita_month, reconstruimos per_capita (diario aprox.)
#----------------------------------------------------------------------
if (!("per_capita" %in% names(coca_df))) {
  if ("per_capita_month" %in% names(coca_df)) {
    coca_df <- coca_df %>% mutate(per_capita = per_capita_month / 30.4167)
  } else {
    stop("CoCA trimestral no tiene 'per_capita' ni 'per_capita_month'. Revisa columnas.")
  }
}

if (!("per_capita" %in% names(cona_df))) {
  if ("per_capita_month" %in% names(cona_df)) {
    cona_df <- cona_df %>% mutate(per_capita = per_capita_month / 30.4167)
  } else {
    stop("CoNA trimestral no tiene 'per_capita' ni 'per_capita_month'. Revisa columnas.")
  }
}

#----------------------------------------------------------------------
# 3) Vectores ciudad–trimestre
#----------------------------------------------------------------------
city_vector <- sort(unique(income_df$ciudad))
tri_vector  <- sort(unique(income_df$trimestre))

#######################################################################
## BLOQUE 1: Afford ciudad–trimestre
#######################################################################
res_afford <- list()
k <- 1

for (city.x in city_vector) {
  for (tri.x in tri_vector) {
    
    message("Procesando Afford TRIMESTRAL: ", city.x, " | ", tri.x)
    
    inc_aux <- income_df %>%
      filter(ciudad == city.x, trimestre == tri.x)
    if (nrow(inc_aux) == 0) next
    
    coca_aux <- coca_df %>%
      filter(ciudad == city.x, trimestre == tri.x)
    cona_aux <- cona_df %>%
      filter(ciudad == city.x, trimestre == tri.x)
    
    if (nrow(coca_aux) == 0 | nrow(cona_aux) == 0) next
    
    out <- try(
      Afford_expansion(
        Hexpense   = inc_aux,
        Model_CoCA = coca_aux,
        Model_CoNA = cona_aux
      ),
      silent = TRUE
    )
    
    if (inherits(out, "try-error") | is.null(out)) {
      message("  -> error/NULL en: ", city.x, " | ", tri.x)
      next
    }
    
    po <- out$Poverty_outcome %>%
      mutate(ciudad = city.x, trimestre = tri.x)
    
    res_afford[[k]] <- po
    k <- k + 1
  }
}

afford_df <- bind_rows(res_afford)

#----------------------------------------------------------------------
# Guardar resultados detallados trimestrales
#----------------------------------------------------------------------
saveRDS(afford_df, file = file.path(afford_metrics_dir, "Afford_city_cuartiles.rds"))
write.csv(afford_df, file = file.path(afford_metrics_dir, "Afford_city_cuartiles.csv"),
          row.names = FALSE)

#######################################################################
## BLOQUE 2: Incidencia promedio por ciudad–trimestre
#######################################################################
aff_inc <- afford_df %>%
  group_by(ciudad, trimestre, model) %>%
  dplyr::summarise(
    n_deciles = n_distinct(deciles),
    incidence = mean(rate, na.rm = TRUE),
    .groups   = "drop"
  )

saveRDS(aff_inc, file = file.path(afford_metrics_dir, "Afford_incidence_city_cuartiles.rds"))
write.csv(aff_inc, file = file.path(afford_metrics_dir, "Afford_incidence_city_cuartiles.csv"),
          row.names = FALSE)

#######################################################################
## BLOQUE 3: Gráficas trimestrales
#######################################################################
g_coca <- aff_inc %>%
  filter(model == "CoCA") %>%
  ggplot(aes(x = trimestre, y = incidence, color = ciudad, group = ciudad)) +
  geom_line(linewidth = 1) +
  labs(title = "Incidencia de pobreza de asequibilidad (CoCA) - trimestral",
       x = "Trimestre", y = "Incidencia (%)", color = "Ciudad") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(file.path(afford_metrics_dir, "Afford_Incidence_CoCA_cuartiles_city_time.png"),
       g_coca, width = 10, height = 5)

g_cona <- aff_inc %>%
  filter(model == "CoNA") %>%
  ggplot(aes(x = trimestre, y = incidence, color = ciudad, group = ciudad)) +
  geom_line(linewidth = 1) +
  labs(title = "Incidencia de pobreza de asequibilidad (CoNA) - trimestral",
       x = "Trimestre", y = "Incidencia (%)", color = "Ciudad") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(file.path(afford_metrics_dir, "Afford_Incidence_CoNA_cuartiles_city_time.png"),
       g_cona, width = 10, height = 5)
