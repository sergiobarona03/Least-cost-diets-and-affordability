########################################################
## SCRIPT 08 — Model validation figure (Appendix A)
##
## Script 07 solves CoCA, CoNA and CoRD twice over the validation window,
## once on observed DANE prices and once on extrapolated ones, and reports
## the deviation between the two. This script redraws that comparison as a
## scatter of observed against extrapolated cost, with the 45-degree line,
## to replace the table currently in Appendix A.
##
## Both runs are restricted to the same food x city x month support, so the
## deviation comes only from price error and not from one panel having a
## larger basket. The uncontrolled version, in which each run optimises
## over its own panel, was examined and gave practically the same answer,
## so only the controlled specification is reported.
##
## Beyond the figure, the script prints the figures needed to write the
## interpretation in the appendix: MAD and MD by model and city, the
## deviation by horizon, the share of months in which the extrapolated
## panel understates the cost, and which foods are selected under one
## panel but not the other.
##
## Reads:  FORECAST_DIR/prices_validation.csv
##         PREP_DIR/tcac_master.rds
##         PREP_DIR/household_eer.rds, household_eer_ll.rds, household_ul.rds
##         PREP_DIR/gaba_exchanges_adj.rds
##         IN_AUX_DIR/CoCA_paper.R, CoNA_paper.R, CoRD_Herforth.R
##
## Writes: VALID_DIR/model_validation_cost.csv
##         VALID_DIR/model_validation_summary.csv
##         VALID_DIR/model_validation_horizon.csv
##         FIG_DIR/final/validation/figA_validation_scatter.png / .pdf
##
## CHECK BEFORE USING: the panel construction, nutrient renaming and CoRD
## serving setup are copied from script 07, which in turn reproduces
## 02_food_table.R, 02_cona.R and 03_cord.R. If any of those changed, this
## script drifts with them.
########################################################

source("00_config.R", encoding = "UTF-8")
source(file.path(REVIEW_DIR, "06_figures", "00_fig_config.R"))
library(tidyverse)
library(scales)

source(file.path(IN_AUX_DIR, "CoCA_paper.R"))
source(file.path(IN_AUX_DIR, "CoNA_paper.R"))
source(file.path(IN_AUX_DIR, "CoRD_Herforth.R"))

# The model functions pacman::p_load Hmisc and others that mask dplyr
summarise <- dplyr::summarise
filter    <- dplyr::filter
select    <- dplyr::select
rename    <- dplyr::rename
count     <- dplyr::count
mutate    <- dplyr::mutate
arrange   <- dplyr::arrange
n         <- dplyr::n

VALID_FIG_DIR <- file.path(FIG_DIR, "final", "validation")
dir.create(VALID_FIG_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(VALID_DIR, recursive = TRUE, showWarnings = FALSE)

# -----------------------------------------------------------------------
# 1. Vocabulary, copied from script 07
# -----------------------------------------------------------------------
FOODS_EXCLUDE <- c(
  "AREPAS RELLENAS CON ALGO", "BOCADILLOS",
  "CEREAL ALIMENTO PARA BEBÉ", "CEREAL PARA DESAYUNO",
  "CHOCOLATE INSTANTANEO", "CHORIZO",
  "GALLETAS DE SAL", "GALLETAS DULCES", "GALLETAS INTEGRALES",
  "GASEOSAS", "GELATINA O FLAN", "HARINA PARA TORTAS",
  "HELADOS DE CREMA", "JAMÓN",
  "JUGOS INSTANTANEOS O EN POLVO", "JUGOS PROCESADOS",
  "MALTAS", "MAYONESA", "MERMELADA", "MORTADELA",
  "PIZZA", "SALCHICHAS", "SALCHICHÓN", "SALSA DE TOMATE",
  "SOPAS", "YOGOURT", "CREMA DE LECHE", "PAPAS FRITAS",
  "CILANTRO", "COLOR", "COMINOS", "LAUREL", "MOSTAZA",
  "PIMIENTA", "TOMILLO", "REVUELTO VERDE",
  "ALMUERZO CORRIENTE O EJECUTIVO",
  "ALMUERZO ESPECIAL O A LA CARTA",
  "CHOCOLATE EN PASTA", "CAFÉ INSTANTANEO", "CAFÉ MOLIDO",
  "COMBOS", "CREMAS", "TINTO",
  "HAMBURGUESA", "KUMIS", "JUGOS NATURALES", "SUERO",
  "ARROZ PARA SOPA")

GRUPO_COCA <- "CEREALES, RAÍCES, TUBÉRCULOS Y PLÁTANOS"

NUTRIENT_MAP <- c(
  energia_kcal             = "Energy",
  proteina_g               = "Protein",
  lipidos_g                = "Lipids",
  carbohidratos_totales_g  = "Carbohydrates",
  vitamina_c_mg            = "VitaminC",
  folatos_mcg              = "Folate",
  vitamina_a_er            = "VitaminA",
  tiamina_mg               = "Thiamine",
  riboflavina_mg           = "Riboflavin",
  niacina_mg               = "Niacin",
  vitamina_b12_mcg         = "VitaminB12",
  magnesio_mg              = "Magnesium",
  fosforo_mg               = "Phosphorus",
  sodio_mg                 = "Sodium",
  calcio_mg                = "Calcium",
  hierro_mg                = "Iron",
  zinc_mg                  = "Zinc")

GROUP_CANON <- c(
  "Cereales, raíces, tubérculos y plátanos",
  "Frutas",
  "Verduras",
  "Leche y productos lácteos",
  "Carnes, huevos, leguminosas, frutos secos y semillas",
  "Grasas",
  "Azúcares")

map_group_paper <- c(
  "AZUCARES"                                                   = "Azúcares",
  "CARNES, HUEVOS, LEGUMINOSAS SECAS, FRUTOS SECOS Y SEMILLAS" = "Carnes, huevos, leguminosas, frutos secos y semillas",
  "CEREALES, RAÍCES, TUBÉRCULOS Y PLÁTANOS"                    = "Cereales, raíces, tubérculos y plátanos",
  "FRUTAS"                                                     = "Frutas",
  "GRASAS"                                                     = "Grasas",
  "LECHE Y PRODUCTOS LACTEOS"                                  = "Leche y productos lácteos",
  "VERDURAS"                                                   = "Verduras")

map_group_serv <- c(
  "AZÚCARES"                                             = "Azúcares",
  "CARNES, HUEVOS, LEGUMINOSAS, FRUTOS SECOS Y SEMILLAS" = "Carnes, huevos, leguminosas, frutos secos y semillas",
  "CEREALES, RAÍCES, TUBÉRCULOS Y PLÁTANOS"              = "Cereales, raíces, tubérculos y plátanos",
  "FRUTAS"                                               = "Frutas",
  "GRASAS"                                               = "Grasas",
  "LECHE Y PRODUCTOS LÁCTEOS"                            = "Leche y productos lácteos",
  "VERDURAS"                                             = "Verduras")

div_cord <- tribble(
  ~Group,                                                  ~Number,
  "Cereales, raíces, tubérculos y plátanos",                3,
  "Frutas",                                                 2,
  "Verduras",                                               2,
  "Leche y productos lácteos",                              1,
  "Carnes, huevos, leguminosas, frutos secos y semillas",    2,
  "Grasas",                                                 1,
  "Azúcares",                                               1)

# -----------------------------------------------------------------------
# 2. Inputs and panels
# -----------------------------------------------------------------------
message("Loading inputs...")

prices_val <- read_csv(file.path(FORECAST_DIR, "prices_validation.csv"),
                       show_col_types = FALSE) %>%
  mutate(fecha = as.Date(fecha))

tcac_master      <- readRDS(file.path(PREP_DIR, "tcac_master.rds"))
household_eer    <- readRDS(file.path(PREP_DIR, "household_eer.rds"))
household_eer_ll <- readRDS(file.path(PREP_DIR, "household_eer_ll.rds"))
household_ul     <- readRDS(file.path(PREP_DIR, "household_ul.rds"))
gaba_adj         <- readRDS(file.path(PREP_DIR, "gaba_exchanges_adj.rds"))

normalise_city <- function(x) {
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  x <- toupper(trimws(x))
  case_when(x %in% c("BOGOTA D.C.", "BOGOTÁ D.C.") ~ "BOGOTA",
            x %in% c("MEDELLIN", "MEDELLÍN")       ~ "MEDELLIN",
            x == "CALI"                            ~ "CALI",
            TRUE                                   ~ x)
}

build_panel <- function(price_col) {
  prices_val %>%
    filter(!is.na(.data[[price_col]]), .data[[price_col]] > 0) %>%
    transmute(ciudad = normalise_city(ciudad), articulo, fecha,
              precio_500g = .data[[price_col]]) %>%
    left_join(tcac_master %>% select(-codigo_articulo, -any_of("subclase_ipc")),
              by = "articulo") %>%
    mutate(
      pc = case_when(
        is.na(parte_comestible_percent) ~ NA_real_,
        parte_comestible_percent > 1    ~ parte_comestible_percent / 100,
        TRUE                            ~ parte_comestible_percent),
      precio_100g = (precio_500g / 5) / pc,
      gramos_g_1_intercambio_1_intercambio =
        suppressWarnings(as.numeric(gramos_g_1_intercambio_1_intercambio))) %>%
    filter(!articulo %in% FOODS_EXCLUDE,
           !is.na(precio_100g), precio_100g > 0,
           !is.na(energia_kcal)) %>%
    mutate(
      Group = if_else(subgrupos_gabas == "FRUTAS",   "FRUTAS",   grupos_gabas),
      Group = if_else(subgrupos_gabas == "VERDURAS", "VERDURAS", Group),
      Group = recode(Group, !!!map_group_paper),
      Serving_g     = gramos_g_1_intercambio_1_intercambio,
      Price_serving = precio_100g * Serving_g / 100)
}

panel_obs <- build_panel("price_obs")
panel_ext <- build_panel("price_S5")

# Common support: only food x city x month present in BOTH panels
support <- panel_obs %>% distinct(ciudad, articulo, fecha) %>%
  inner_join(panel_ext %>% distinct(ciudad, articulo, fecha),
             by = c("ciudad", "articulo", "fecha"))

panel_obs <- panel_obs %>% semi_join(support, by = c("ciudad", "articulo", "fecha"))
panel_ext <- panel_ext %>% semi_join(support, by = c("ciudad", "articulo", "fecha"))

message(sprintf("  common support: %d rows | %d foods | %d months | %d cities",
                nrow(support), n_distinct(support$articulo),
                n_distinct(support$fecha), n_distinct(support$ciudad)))

# -----------------------------------------------------------------------
# 3. Requirement objects
# -----------------------------------------------------------------------
META_COLS <- c("ciudad", "cod_mun", "rango", "sex", "eer", "municipio",
               "departamento", "poblacion")

eer_for <- function(city) {
  household_eer %>% filter(ciudad == city) %>%
    select(-any_of(c("ciudad", "cod_mun"))) %>%
    rename(Age = rango, Sex = sex, Energy = eer) %>%
    mutate(Sex = if_else(Sex == "Masculino", 0L, 1L)) %>%
    as.data.frame()
}
ll_for <- function(city) {
  household_eer_ll %>% filter(ciudad == city) %>%
    select(-any_of(META_COLS)) %>% as.data.frame()
}
ul_for <- function(city) {
  household_ul %>% filter(ciudad == city) %>%
    select(-any_of(META_COLS)) %>% as.data.frame()
}

serv_adj <- gaba_adj %>%
  filter((sex == "Masculino" & rango == "[31,51)") |
           (sex == "Femenino" & rango == "[31,51)") |
           (sex == "Femenino" & rango == "[10, 14)")) %>%
  rename(Age = rango, Sex = sex, Group = grupo_principal,
         Serving = n_exchanges_adj) %>%
  mutate(Group = recode(Group, !!!map_group_serv),
         Sex   = if_else(Sex == "Masculino", 0L, 1L)) %>%
  filter(Group %in% GROUP_CANON)

serv_for <- function(city) {
  serv_adj %>% filter(ciudad == city) %>%
    select(-any_of(c("ciudad", "cod_mun"))) %>% as.data.frame()
}

# -----------------------------------------------------------------------
# 4. Solvers
# -----------------------------------------------------------------------
run_coca <- function(d, eer) {
  dd <- d %>% filter(grupos_gabas == GRUPO_COCA) %>%
    rename(Price_100g = precio_100g, Food = articulo, Energy = energia_kcal) %>%
    as.data.frame()
  if (nrow(dd) == 0) return(NULL)
  tryCatch(CoCA_paper(data = dd, EER = eer), error = function(e) NULL)
}

run_cona <- function(d, ll, ul) {
  dd <- d %>%
    rename(Price_100g = precio_100g, Food = articulo) %>%
    rename(any_of(setNames(names(NUTRIENT_MAP), unname(NUTRIENT_MAP)))) %>%
    as.data.frame()
  if (nrow(dd) == 0) return(NULL)
  tryCatch(CoNA_paper(data = dd, EER_LL = ll, UL = ul), error = function(e) NULL)
}

run_cord <- function(d, serv) {
  dd <- d %>%
    filter(Group %in% GROUP_CANON,
           !is.na(Serving_g), Serving_g > 0, !is.na(Price_serving)) %>%
    rename(Food = articulo, Price_100g = precio_100g, Energy = energia_kcal) %>%
    as.data.frame()
  if (nrow(dd) == 0) return(NULL)
  tryCatch(CoRD_Herforth(data = dd, serv = serv, diverse = div_cord),
           error = function(e) NULL)
}

harmonise <- function(x) {
  x %>% mutate(Demo_Group = as.character(Demo_Group),
               Sex        = suppressWarnings(as.numeric(Sex)))
}

keep_cost <- function(x, model_name, ci, ft, src) {
  x %>%
    select(any_of(c("Demo_Group", "Sex", "cost_day", "cona_cost",
                    "Cost_1000kcal"))) %>%
    { if ("cona_cost" %in% names(.)) rename(., cost_day = cona_cost) else . } %>%
    harmonise() %>%
    distinct(Demo_Group, Sex, .keep_all = TRUE) %>%
    mutate(model = model_name, ciudad = ci, fecha = as.Date(ft), source = src)
}

keep_comp <- function(x, model_name, ci, ft, src) {
  x %>% harmonise() %>%
    mutate(model = model_name, ciudad = ci, fecha = as.Date(ft), source = src)
}

# -----------------------------------------------------------------------
# 5. Solve
# -----------------------------------------------------------------------
message("Solving models...")

ciudades <- sort(intersect(unique(panel_obs$ciudad), unique(household_eer$ciudad)))
fechas   <- sort(unique(panel_obs$fecha))

cost_rows <- list(); comp_rows <- list()

for (ci in ciudades) {
  eer  <- eer_for(ci); ll <- ll_for(ci); ul <- ul_for(ci)
  serv <- serv_for(ci)
  
  for (ft in fechas) {
    d_obs <- panel_obs %>% filter(ciudad == ci, fecha == ft)
    d_ext <- panel_ext %>% filter(ciudad == ci, fecha == ft)
    if (nrow(d_obs) == 0 || nrow(d_ext) == 0) next
    
    for (src in c("observed", "extrapolated")) {
      d <- if (src == "observed") d_obs else d_ext
      
      r1 <- run_coca(d, eer)
      if (!is.null(r1) && !is.null(r1$cost))
        cost_rows[[length(cost_rows) + 1]] <- keep_cost(r1$cost, "CoCA", ci, ft, src)
      
      r2 <- run_cona(d, ll, ul)
      if (!is.null(r2) && !is.null(r2$cost)) {
        cost_rows[[length(cost_rows) + 1]] <- keep_cost(r2$cost, "CoNA", ci, ft, src)
        comp_rows[[length(comp_rows) + 1]] <- keep_comp(r2$comp, "CoNA", ci, ft, src)
      }
      
      r3 <- run_cord(d, serv)
      if (!is.null(r3) && !is.null(r3$cost)) {
        cost_rows[[length(cost_rows) + 1]] <- keep_cost(r3$cost, "CoRD", ci, ft, src)
        comp_rows[[length(comp_rows) + 1]] <- keep_comp(r3$comp, "CoRD", ci, ft, src)
      }
    }
  }
  message("  ", ci, " done.")
}

costs <- bind_rows(cost_rows)
comps <- bind_rows(comp_rows)

if (nrow(costs) == 0) stop("No model solved. Check the panels and requirements.")

# -----------------------------------------------------------------------
# 6. Per capita deviation
# -----------------------------------------------------------------------
per_capita <- costs %>%
  filter(!is.na(cost_day)) %>%
  group_by(model, ciudad, fecha, source) %>%
  summarise(cost_pc = mean(cost_day), n_members = n(), .groups = "drop") %>%
  filter(n_members == 3) %>%
  select(-n_members) %>%
  pivot_wider(names_from = source, values_from = cost_pc) %>%
  filter(!is.na(observed), !is.na(extrapolated)) %>%
  mutate(dev_pct = 100 * (extrapolated - observed) / observed,
         h_mes   = as.integer(round(as.numeric(fecha - T0) / 30.44)),
         model   = factor(model, levels = c("CoCA", "CoNA", "CoRD")),
         ciudad_lbl = factor(CITY_LABS[ciudad],
                             levels = c("Bogotá", "Medellín", "Cali")))

# -----------------------------------------------------------------------
# 7. Everything needed to write the appendix interpretation
# -----------------------------------------------------------------------
rule <- function(txt) {
  cat("\n", strrep("=", 70), "\n  ", txt, "\n", strrep("=", 70), "\n\n", sep = "")
}

rule("MAD and MD by model and city")

summary_tbl <- per_capita %>%
  group_by(model, ciudad_lbl) %>%
  summarise(n_months = n(),
            MAD_pct  = mean(abs(dev_pct)),
            MD_pct   = mean(dev_pct),
            p90_abs  = quantile(abs(dev_pct), 0.9),
            max_abs  = max(abs(dev_pct)),
            .groups  = "drop") %>%
  bind_rows(
    per_capita %>% group_by(model) %>%
      summarise(ciudad_lbl = "All", n_months = n(),
                MAD_pct = mean(abs(dev_pct)), MD_pct = mean(dev_pct),
                p90_abs = quantile(abs(dev_pct), 0.9),
                max_abs = max(abs(dev_pct)), .groups = "drop")) %>%
  arrange(model, ciudad_lbl)

print(as.data.frame(summary_tbl), digits = 4)

rule("Share of months in which the extrapolated panel understates the cost")

cat("  (the optimiser-selection bias: if well above 50%, selection is\n")
cat("   correlated with the sign of the price error)\n\n")
print(as.data.frame(
  per_capita %>% group_by(model) %>%
    summarise(pct_understated = 100 * mean(dev_pct < 0),
              n_months = n(), .groups = "drop")), digits = 4)

rule("Deviation by horizon — where the bias turns and how it grows")

horizon_tbl <- per_capita %>%
  group_by(model, h_mes) %>%
  summarise(MAD_pct = mean(abs(dev_pct)), MD_pct = mean(dev_pct),
            .groups = "drop")
print(as.data.frame(horizon_tbl), digits = 4)

cat("\n  First horizon at which each model's MD turns negative:\n\n")
print(as.data.frame(
  horizon_tbl %>% filter(MD_pct < 0) %>%
    group_by(model) %>% slice_min(h_mes, n = 1)), digits = 4)

rule("R-squared and slope of extrapolated on observed, by model")

cat("  (a slope below one means the panel compresses the range of costs)\n\n")
print(as.data.frame(
  per_capita %>% group_by(model) %>%
    summarise(r2    = summary(lm(extrapolated ~ observed))$r.squared,
              slope = coef(lm(extrapolated ~ observed))[2],
              .groups = "drop")), digits = 4)

rule("Foods selected under one panel only")

comp_tbl <- comps %>%
  distinct(model, ciudad, fecha, source, Food) %>%
  count(model, ciudad, source, Food, name = "n_months") %>%
  pivot_wider(names_from = source, values_from = n_months, values_fill = 0) %>%
  mutate(only_in = case_when(observed > 0 & extrapolated == 0 ~ "observed only",
                             observed == 0 & extrapolated > 0 ~ "extrapolated only",
                             TRUE ~ "both"))

print(as.data.frame(comp_tbl %>% filter(only_in != "both")), digits = 4)

cat("\n  Foods whose selection frequency differs most between panels:\n\n")
print(as.data.frame(
  comp_tbl %>% mutate(diff = extrapolated - observed) %>%
    filter(abs(diff) > 0) %>%
    arrange(desc(abs(diff))) %>% head(15)), digits = 4)

write_csv(per_capita,  file.path(VALID_DIR, "model_validation_cost.csv"))
write_csv(summary_tbl, file.path(VALID_DIR, "model_validation_summary.csv"))
write_csv(horizon_tbl, file.path(VALID_DIR, "model_validation_horizon.csv"))

# -----------------------------------------------------------------------
# 8. Figure — observed vs extrapolated, with the 45-degree line
# -----------------------------------------------------------------------
figA <- ggplot(per_capita,
               aes(x = observed, y = extrapolated, colour = model)) +
  geom_abline(slope = 1, intercept = 0, colour = "grey50",
              linetype = "dashed", linewidth = 0.4) +
  geom_point(size = 1.1, alpha = 0.75) +
  facet_wrap(~ ciudad_lbl, nrow = 1) +
  scale_colour_manual(
    values = c("CoCA" = "#1F4E79", "CoNA" = "#C07A17", "CoRD" = "#7B2D8E"),
    name = NULL) +
  scale_x_continuous(labels = comma_format(big.mark = ",")) +
  scale_y_continuous(labels = comma_format(big.mark = ",")) +
  labs(x = "Cost on observed prices (COP/day)",
       y = "Cost on extrapolated prices (COP/day)") +
  paper_theme() +
  theme(axis.text = element_text(size = 7),
        axis.title = element_text(size = 9),
        strip.text = element_text(face = "bold", size = 9),
        legend.position = "top", legend.direction = "horizontal",
        legend.text = element_text(family = "serif", size = 9),
        legend.key.size = unit(0.35, "cm"),
        legend.background = element_rect(colour = "black", fill = "white",
                                         linewidth = 0.4),
        panel.spacing = unit(0.45, "cm"))

ggsave(file.path(VALID_FIG_DIR, "figA_validation_scatter.png"),
       figA, width = 17.4, height = 8, units = "cm", dpi = 400, bg = "white")
ggsave(file.path(VALID_FIG_DIR, "figA_validation_scatter.pdf"),
       figA, width = 17.4, height = 8, units = "cm", bg = "white")

message("\nFigure saved to: ", VALID_FIG_DIR)