########################################################
## SCRIPT 04_validation/01_model_validation.R
## Model validation — pseudo out-of-sample
## Jan 2000 – Mar 2018
##
## Three price scenarios:
##   S1 — BASE : anchor × (IPC_t/IPC_T0)^1
##   S2 — S5   : anchor × (IPC_t/IPC_T0)^λ*
##   S3 — Obs  : DANE raw prices (ground truth)
##
## Scatter plots: observed vs estimated costs per model
##
## Reads:  CACHE_DIR/dane.rds
##         CACHE_DIR/ipc.rds
##         CACHE_DIR/lambdas.rds         ← JS-shrunk lambdas
##         PREP_DIR/tcac_master.rds
##         PREP_DIR/gaba_exchanges_adj.rds
##         PREP_DIR/household_eer.rds
##         PREP_DIR/household_eer_ll.rds
##         PREP_DIR/household_ul.rds
##         BASE_DIR/.../cpi-weights-2024.xlsx
##
## Writes: VALID_DIR/validation_cost_all.csv
##         VALID_DIR/validation_results.xlsx
##         VALID_DIR/fig_made_by_model.png
##         VALID_DIR/fig_made_timeseries.png
##         VALID_DIR/fig_scatter_*.png
########################################################

source("00_config.R")
library(tidyverse)
library(readxl)
library(writexl)
library(lubridate)
library(scales)
library(FoodpriceR)

source(file.path(IN_AUX_DIR, "CoCA_paper.R"))
source(file.path(IN_AUX_DIR, "CoNA_paper.R"))
source(file.path(IN_AUX_DIR, "CoRD_Herforth.R"))
source(file.path(IN_AUX_DIR, "CoNA_IPC_paper.R"))

# Extended validation window
VAL_START_EXT <- as.Date("2000-01-01")
VAL_END_EXT   <- as.Date("2018-03-01")

# -----------------------------------------------------------------------
# 1. Load DANE raw prices (ground truth)
# -----------------------------------------------------------------------
message("Loading DANE raw prices...")

dane <- readRDS(file.path(CACHE_DIR, "dane.rds")) %>%
  filter(fecha >= VAL_START_EXT,
         fecha <= VAL_END_EXT,
         !is.na(precio_500g)) %>%
  mutate(ciudad = case_when(
    nombre_ciudad == "BOGOTÁ D.C." ~ "BOGOTA",
    nombre_ciudad == "MEDELLÍN"    ~ "MEDELLIN",
    nombre_ciudad == "CALI"        ~ "CALI",
    TRUE ~ nombre_ciudad))

message(sprintf("  DANE: %d rows | %s – %s",
                nrow(dane),
                min(dane$fecha), max(dane$fecha)))

# -----------------------------------------------------------------------
# 2. Load IPC and lambdas — build extrapolated prices
# -----------------------------------------------------------------------
message("Building extrapolated price panels...")

ipc <- readRDS(file.path(CACHE_DIR, "ipc.rds")) %>%
  mutate(ciudad = case_when(
    ciudad == "BOGOTÁ D.C." ~ "BOGOTA",
    ciudad == "MEDELLÍN"    ~ "MEDELLIN",
    TRUE                    ~ ciudad)) %>%
  filter(ciudad %in% c("BOGOTA", "MEDELLIN", "CALI"))

lambdas <- readRDS(file.path(CACHE_DIR, "lambdas.rds"))
# lambdas has: ciudad, cod_subclase, lambda_js (JS-shrunk)
# lambda_base = 1 for all (proportional)

# Anchor: mean observed price in T0 window (Jan–Dec 1999)
T0_START <- as.Date("1999-01-01")
T0_END   <- as.Date("1999-12-01")

anchor <- dane %>%
  filter(fecha >= T0_START, fecha <= T0_END) %>%
  group_by(ciudad, articulo, cod_subclase) %>%
  dplyr::summarise(
    price_anchor = mean(precio_500g, na.rm = TRUE),
    .groups = "drop")

# IPC index at T0 (mean over anchor window)
ipc_T0 <- ipc %>%
  filter(fecha >= T0_START, fecha <= T0_END) %>%
  group_by(ciudad, cod_subclase) %>%
  dplyr::summarise(ipc_T0 = mean(ipc, na.rm = TRUE), .groups = "drop")

# All dates in validation window
dates_val <- dane %>%
  distinct(fecha) %>%
  arrange(fecha) %>%
  pull(fecha)

# Build extrapolated prices for each article × city × date
prices_ext <- anchor %>%
  crossing(fecha = dates_val) %>%
  left_join(ipc %>% select(ciudad, cod_subclase, fecha, ipc_t = ipc),
            by = c("ciudad", "cod_subclase", "fecha")) %>%
  left_join(ipc_T0, by = c("ciudad", "cod_subclase")) %>%
  left_join(lambdas %>% select(ciudad, cod_subclase,
                               lambda_js),
            by = c("ciudad", "cod_subclase")) %>%
  mutate(
    lambda_js  = replace_na(lambda_js, 1),
    ipc_ratio  = ipc_t / ipc_T0,
    price_BASE = price_anchor * ipc_ratio,
    price_S5   = price_anchor * ipc_ratio ^ lambda_js) %>%
  filter(!is.na(price_BASE))

message(sprintf("  Extrapolated prices: %d rows", nrow(prices_ext)))

# -----------------------------------------------------------------------
# 3. Load nutritional composition
# -----------------------------------------------------------------------
message("Loading nutritional composition...")

tcac_master <- readRDS(file.path(PREP_DIR, "tcac_master.rds"))

nutrients <- tcac_master %>%
  select(articulo,
         gramos_g_1_intercambio_1_intercambio,
         grupos_gabas, subgrupos_gabas,
         energia_kcal, proteina_g, lipidos_g,
         carbohidratos_totales_g, vitamina_c_mg,
         folatos_mcg, vitamina_a_er, tiamina_mg,
         riboflavina_mg, niacina_mg, vitamina_b12_mcg,
         magnesio_mg, fosforo_mg, sodio_mg,
         calcio_mg, hierro_mg, zinc_mg) %>%
  distinct(articulo, .keep_all = TRUE)

pc_table <- tcac_master %>%
  select(articulo, parte_comestible_percent) %>%
  distinct(articulo, .keep_all = TRUE) %>%
  mutate(pc = case_when(
    is.na(parte_comestible_percent) ~ NA_real_,
    parte_comestible_percent > 1    ~ parte_comestible_percent / 100,
    TRUE                            ~ parte_comestible_percent))

# -----------------------------------------------------------------------
# 4. Load EER inputs
# -----------------------------------------------------------------------
household_eer    <- readRDS(file.path(PREP_DIR, "household_eer.rds"))
household_eer_ll <- readRDS(file.path(PREP_DIR, "household_eer_ll.rds"))
household_ul     <- readRDS(file.path(PREP_DIR, "household_ul.rds"))

# -----------------------------------------------------------------------
# 5. Load GABA exchanges (CoRD)
# -----------------------------------------------------------------------
serv_adj <- readRDS(file.path(PREP_DIR, "gaba_exchanges_adj.rds")) %>%
  filter(
    (sex == "Masculino" & rango == "[31,51)") |
      (sex == "Femenino"  & rango == "[31,51)") |
      (sex == "Femenino"  & rango == "[10, 14)")) %>%
  mutate(
    Group = recode(grupo_principal,
                   "AZÚCARES" = "Azúcares",
                   "CARNES, HUEVOS, LEGUMINOSAS, FRUTOS SECOS Y SEMILLAS" =
                     "Carnes, huevos, leguminosas, frutos secos y semillas",
                   "CEREALES, RAÍCES, TUBÉRCULOS Y PLÁTANOS" =
                     "Cereales, raíces, tubérculos y plátanos",
                   "FRUTAS"   = "Frutas",
                   "GRASAS"   = "Grasas",
                   "LECHE Y PRODUCTOS LÁCTEOS" = "Leche y productos lácteos",
                   "VERDURAS" = "Verduras"),
    Age     = rango,
    Sex     = if_else(sex == "Masculino", 0L, 1L),
    Serving = n_exchanges_adj) %>%
  select(cod_mun, ciudad, Age, Sex, Group, Serving)

GROUP_CANON <- c(
  "Cereales, raíces, tubérculos y plátanos", "Frutas", "Verduras",
  "Leche y productos lácteos",
  "Carnes, huevos, leguminosas, frutos secos y semillas",
  "Grasas", "Azúcares")

div_cord <- tribble(
  ~Group, ~Number,
  "Cereales, raíces, tubérculos y plátanos", 3,
  "Frutas", 2, "Verduras", 2,
  "Leche y productos lácteos", 1,
  "Carnes, huevos, leguminosas, frutos secos y semillas", 2,
  "Grasas", 1, "Azúcares", 1)

# -----------------------------------------------------------------------
# 6. Load IPC weights (CC-CoNA)
# -----------------------------------------------------------------------
in_weights <- file.path(BASE_DIR,
                        "food-security-paper", "input", "cpi-weights", "cpi-weights-2024.xlsx")

weights_24 <- read_excel(in_weights) %>%
  janitor::clean_names() %>%
  select(nivel, codigo, nombre, total_ingresos)

alphas_ccona <- c(0, 0.25, 0.50, 0.75, 1.00)

# -----------------------------------------------------------------------
# 7. Helper functions
# -----------------------------------------------------------------------
normalise_eer <- function(df) {
  if ("rango" %in% names(df)) names(df)[names(df) == "rango"] <- "Age"
  if ("sex"   %in% names(df)) names(df)[names(df) == "sex"]   <- "Sex"
  if ("eer"   %in% names(df)) names(df)[names(df) == "eer"]   <- "Energy"
  if ("Sex"   %in% names(df) && is.character(df$Sex))
    df$Sex <- if_else(df$Sex == "Masculino", 0L, 1L)
  df
}

make_panel <- function(price_df, price_col) {
  names(price_df)[names(price_df) == price_col] <- "precio_500g_use"
  price_df %>%
    left_join(pc_table,  by = "articulo") %>%
    left_join(nutrients, by = "articulo") %>%
    mutate(
      precio_100g   = (precio_500g_use / 5) / pc,
      clase_ipc     = paste0(substr(cod_subclase, 1, 4), "0000"),
      Group_ipc     = clase_ipc,
      Group_cord    = if_else(subgrupos_gabas == "FRUTAS",
                              "FRUTAS", grupos_gabas),
      Group_cord    = if_else(subgrupos_gabas == "VERDURAS",
                              "VERDURAS", Group_cord),
      Group_cord    = recode(Group_cord,
                             "AZUCARES" = "Azúcares",
                             "CARNES, HUEVOS, LEGUMINOSAS SECAS, FRUTOS SECOS Y SEMILLAS" =
                               "Carnes, huevos, leguminosas, frutos secos y semillas",
                             "CEREALES, RAÍCES, TUBÉRCULOS Y PLÁTANOS" =
                               "Cereales, raíces, tubérculos y plátanos",
                             "FRUTAS"   = "Frutas",
                             "GRASAS"   = "Grasas",
                             "LECHE Y PRODUCTOS LACTEOS" = "Leche y productos lácteos",
                             "VERDURAS" = "Verduras"),
      Serving_g = gramos_g_1_intercambio_1_intercambio) %>%
    filter(!is.na(precio_100g), precio_100g > 0,
           !is.na(energia_kcal))
}

# Build panels
message("Building food panels for three scenarios...")

base_prices <- prices_ext %>%
  select(ciudad, fecha, articulo, cod_subclase, price_BASE)

s5_prices <- prices_ext %>%
  select(ciudad, fecha, articulo, cod_subclase, price_S5)

obs_prices <- dane %>%
  select(ciudad, fecha, articulo,
         cod_subclase, precio_500g)

panel_S1 <- make_panel(base_prices, "price_BASE")
panel_S2 <- make_panel(s5_prices,   "price_S5")
panel_S3 <- make_panel(obs_prices,  "precio_500g")

message(sprintf("  S1:%d | S2:%d | S3:%d rows",
                nrow(panel_S1), nrow(panel_S2), nrow(panel_S3)))

scenarios <- list(S1_BASE = panel_S1,
                  S2_S5   = panel_S2,
                  S3_Obs  = panel_S3)

cities <- sort(unique(panel_S3$ciudad))
fechas <- sort(unique(panel_S3$fecha))

# -----------------------------------------------------------------------
# 8. Run models
# -----------------------------------------------------------------------
run_all_models <- function(panel, scenario_label) {
  
  results     <- list()
  spe_results <- list()
  n_ok <- 0L; n_fail <- 0L
  
  for (i in cities) {
    
    eer.aux    <- household_eer    %>% filter(ciudad==i) %>%
      select(-ciudad, -any_of("cod_mun")) %>% as.data.frame() %>%
      normalise_eer()
    eer_ll.aux <- household_eer_ll %>% filter(ciudad==i) %>%
      select(-ciudad, -any_of("cod_mun")) %>% as.data.frame() %>%
      normalise_eer()
    ul.aux     <- household_ul     %>% filter(ciudad==i) %>%
      select(-ciudad, -any_of("cod_mun")) %>% as.data.frame() %>%
      normalise_eer()
    serv.aux   <- serv_adj %>% filter(ciudad==i) %>%
      select(Age, Sex, Group, Serving)
    
    clases_v   <- unique(panel$clase_ipc)
    ipc_sh_aux <- weights_24 %>%
      filter(nivel == "Clase", codigo %in% clases_v) %>%
      mutate(share = total_ingresos / sum(total_ingresos),
             Group = codigo) %>%
      select(Group, nombre, share) %>%
      as.data.frame()
    
    for (t in fechas) {
      
      data_t <- panel %>%
        filter(ciudad==i, fecha==t, !is.na(precio_100g)) %>%
        filter(articulo != "ARROZ PARA SOPA")
      
      if (nrow(data_t) == 0) next
      
      # CoCA
      data_coca <- data_t %>%
        filter(grupos_gabas ==
                 "CEREALES, RAÍCES, TUBÉRCULOS Y PLÁTANOS") %>%
        as.data.frame()
      names(data_coca)[names(data_coca)=="articulo"]    <- "Food"
      names(data_coca)[names(data_coca)=="precio_100g"] <- "Price_100g"
      names(data_coca)[names(data_coca)=="energia_kcal"]<- "Energy"
      
      res_coca <- tryCatch(
        CoCA_paper(data=data_coca, EER=eer.aux),
        error=function(e) NULL)
      if (!is.null(res_coca$cost)) {
        results[[length(results)+1]] <- res_coca$cost %>%
          mutate(model="CoCA", ciudad=i, fecha=t,
                 scenario=scenario_label, alpha_val=NA_real_)
        n_ok <- n_ok + 1L
      } else { n_fail <- n_fail + 1L }
      
      # CoNA
      data_cona <- data_t %>% as.data.frame()
      names(data_cona)[names(data_cona)=="articulo"]               <- "Food"
      names(data_cona)[names(data_cona)=="precio_100g"]            <- "Price_100g"
      names(data_cona)[names(data_cona)=="energia_kcal"]           <- "Energy"
      names(data_cona)[names(data_cona)=="proteina_g"]             <- "Protein"
      names(data_cona)[names(data_cona)=="lipidos_g"]              <- "Lipids"
      names(data_cona)[names(data_cona)=="carbohidratos_totales_g"]<- "Carbohydrates"
      names(data_cona)[names(data_cona)=="vitamina_c_mg"]          <- "VitaminC"
      names(data_cona)[names(data_cona)=="folatos_mcg"]            <- "Folate"
      names(data_cona)[names(data_cona)=="vitamina_a_er"]          <- "VitaminA"
      names(data_cona)[names(data_cona)=="tiamina_mg"]             <- "Thiamine"
      names(data_cona)[names(data_cona)=="riboflavina_mg"]         <- "Riboflavin"
      names(data_cona)[names(data_cona)=="niacina_mg"]             <- "Niacin"
      names(data_cona)[names(data_cona)=="vitamina_b12_mcg"]       <- "VitaminB12"
      names(data_cona)[names(data_cona)=="magnesio_mg"]            <- "Magnesium"
      names(data_cona)[names(data_cona)=="fosforo_mg"]             <- "Phosphorus"
      names(data_cona)[names(data_cona)=="sodio_mg"]               <- "Sodium"
      names(data_cona)[names(data_cona)=="calcio_mg"]              <- "Calcium"
      names(data_cona)[names(data_cona)=="hierro_mg"]              <- "Iron"
      names(data_cona)[names(data_cona)=="zinc_mg"]                <- "Zinc"
      
      res_cona <- tryCatch(
        CoNA_paper(data=data_cona, EER_LL=eer_ll.aux, UL=ul.aux),
        error=function(e) NULL)
      if (!is.null(res_cona$cost)) {
        results[[length(results)+1]] <- res_cona$cost %>%
          mutate(model="CoNA", ciudad=i, fecha=t,
                 scenario=scenario_label, alpha_val=NA_real_)
        if (!is.null(res_cona$spe))
          spe_results[[length(spe_results)+1]] <- res_cona$spe %>%
            mutate(model="CoNA", ciudad=i, fecha=t,
                   scenario=scenario_label)
        n_ok <- n_ok + 1L
      } else { n_fail <- n_fail + 1L }
      
      # CoRD
      data_cord <- data_t %>%
        filter(Group_cord %in% GROUP_CANON) %>%
        as.data.frame()
      names(data_cord)[names(data_cord)=="articulo"]   <- "Food"
      names(data_cord)[names(data_cord)=="Group_cord"] <- "Group"
      
      res_cord <- tryCatch(
        CoRD_Herforth(data=data_cord, serv=serv.aux,
                      diverse=div_cord),
        error=function(e) NULL)
      if (!is.null(res_cord$cost)) {
        results[[length(results)+1]] <- res_cord$cost %>%
          mutate(model="CoRD", ciudad=i, fecha=t,
                 scenario=scenario_label, alpha_val=NA_real_)
        n_ok <- n_ok + 1L
      } else { n_fail <- n_fail + 1L }
      
      # CC-CoNA
      data_ccona <- data_cona %>%
        mutate(Group = data_t$Group_ipc)
      
      for (alp in alphas_ccona) {
        res_cc <- tryCatch(
          CoNA_IPC_paper(data=data_ccona, EER_LL=eer_ll.aux,
                         UL=ul.aux, IPC_shares=ipc_sh_aux,
                         alpha=alp),
          error=function(e) NULL)
        if (!is.null(res_cc$cost)) {
          results[[length(results)+1]] <- res_cc$cost %>%
            mutate(model="CC-CoNA", ciudad=i, fecha=t,
                   scenario=scenario_label, alpha_val=alp)
          n_ok <- n_ok + 1L
        }
      }
    }
  }
  
  message(sprintf("  %s: %d OK | %d failed",
                  scenario_label, n_ok, n_fail))
  list(cost = bind_rows(results),
       spe  = bind_rows(spe_results))
}

message("\nRunning S1 (BASE)...")
res_S1 <- run_all_models(panel_S1, "S1_BASE")
message("\nRunning S2 (S5)...")
res_S2 <- run_all_models(panel_S2, "S2_S5")
message("\nRunning S3 (Observed)...")
res_S3 <- run_all_models(panel_S3, "S3_Obs")

# -----------------------------------------------------------------------
# 9. Compute MADE
# -----------------------------------------------------------------------
message("\nComputing MADE...")

standardise_cost <- function(df) {
  cost_col <- intersect(names(df),
                        c("cost_day","Cost","cost","CoNA_cost",
                          "total_cost","diet_cost"))
  if (length(cost_col)==1)
    names(df)[names(df)==cost_col] <- "cost"
  df
}

res_all <- bind_rows(
  standardise_cost(res_S1$cost),
  standardise_cost(res_S2$cost),
  standardise_cost(res_S3$cost)) %>%
  mutate(fecha = as.Date(fecha))

join_keys <- c("ciudad","fecha","model","alpha_val","Demo_Group","Sex")

cost_wide <- res_all %>%
  filter(scenario=="S3_Obs") %>%
  select(all_of(join_keys), cost_S3=cost) %>%
  inner_join(
    res_all %>% filter(scenario=="S1_BASE") %>%
      select(all_of(join_keys), cost_S1=cost),
    by=join_keys) %>%
  inner_join(
    res_all %>% filter(scenario=="S2_S5") %>%
      select(all_of(join_keys), cost_S2=cost),
    by=join_keys) %>%
  mutate(
    delta_S1     = (cost_S1 - cost_S3) / cost_S3 * 100,
    delta_S2     = (cost_S2 - cost_S3) / cost_S3 * 100,
    abs_delta_S1 = abs(delta_S1),
    abs_delta_S2 = abs(delta_S2),
    closer       = if_else(abs_delta_S2 <= abs_delta_S1,
                           "S2_S5","S1_BASE"),
    imp_pp       = abs_delta_S1 - abs_delta_S2)

message(sprintf("  Matched observations: %d", nrow(cost_wide)))

# -----------------------------------------------------------------------
# 10. Summaries
# -----------------------------------------------------------------------
global_summary <- cost_wide %>%
  dplyr::summarise(
    n_obs           = n(),
    MADE_S1         = mean(abs_delta_S1, na.rm=TRUE),
    MADE_S2         = mean(abs_delta_S2, na.rm=TRUE),
    pct_within_1_S1 = mean(abs_delta_S1<1, na.rm=TRUE)*100,
    pct_within_1_S2 = mean(abs_delta_S2<1, na.rm=TRUE)*100,
    pct_within_3_S1 = mean(abs_delta_S1<3, na.rm=TRUE)*100,
    pct_within_3_S2 = mean(abs_delta_S2<3, na.rm=TRUE)*100,
    pct_S2_closer   = mean(closer=="S2_S5", na.rm=TRUE)*100,
    imp_mean_pp     = mean(imp_pp, na.rm=TRUE))

model_summary <- cost_wide %>%
  group_by(model, alpha_val) %>%
  dplyr::summarise(
    n_obs           = n(),
    MADE_S1         = mean(abs_delta_S1, na.rm=TRUE),
    MADE_S2         = mean(abs_delta_S2, na.rm=TRUE),
    pct_within_3_S1 = mean(abs_delta_S1<3, na.rm=TRUE)*100,
    pct_within_3_S2 = mean(abs_delta_S2<3, na.rm=TRUE)*100,
    pct_S2_closer   = mean(closer=="S2_S5", na.rm=TRUE)*100,
    imp_pp          = MADE_S1 - MADE_S2,
    .groups="drop") %>%
  arrange(model, alpha_val)

city_summary <- cost_wide %>%
  group_by(model, ciudad, alpha_val) %>%
  dplyr::summarise(
    n_obs   = n(),
    MADE_S1 = mean(abs_delta_S1, na.rm=TRUE),
    MADE_S2 = mean(abs_delta_S2, na.rm=TRUE),
    imp_pp  = MADE_S1 - MADE_S2,
    .groups="drop") %>%
  arrange(model, ciudad, alpha_val)

cat("\n", strrep("=",65), "\n")
cat(sprintf("  VALIDATION: Jan 2000 – Mar 2018\n"))
cat(sprintf("  MADE S1 (BASE): %.4f%% | MADE S2 (S5): %.4f%%\n",
            global_summary$MADE_S1, global_summary$MADE_S2))
cat(sprintf("  Improvement:    %+.4f pp\n\n",
            global_summary$imp_mean_pp))

# -----------------------------------------------------------------------
# 11. Figures — MADE
# -----------------------------------------------------------------------
COL_BASE <- "#2C3E6B"
COL_S5   <- "#C0392B"

model_fig <- model_summary %>%
  mutate(
    model_lbl = if_else(model=="CC-CoNA",
                        paste0("CC-CoNA\nα=",alpha_val), model),
    model_lbl = fct_reorder(model_lbl, MADE_S1)) %>%
  pivot_longer(c(MADE_S1, MADE_S2),
               names_to="scenario", values_to="MADE") %>%
  mutate(scenario_lbl = if_else(scenario=="MADE_S1","BASE","S5"))

p_made <- ggplot(model_fig,
                 aes(x=MADE, y=model_lbl,
                     color=scenario_lbl, shape=scenario_lbl)) +
  geom_point(size=3.5, alpha=0.9) +
  geom_line(aes(group=model_lbl),
            color="grey70", linewidth=0.7) +
  scale_color_manual(values=c("BASE"=COL_BASE,"S5"=COL_S5)) +
  scale_shape_manual(values=c("BASE"=16,"S5"=15)) +
  geom_vline(xintercept=c(1,3,5),
             linetype="dotted", color="grey60", linewidth=0.4) +
  labs(title="Model validation — MADE by model and scenario",
       subtitle=sprintf(
         "Validation window: Jan 2000–Mar 2018\nMADE — BASE: %.4f%% | S5: %.4f%%",
         global_summary$MADE_S1, global_summary$MADE_S2),
       x="MADE (%)", y=NULL,
       color="Scenario", shape="Scenario") +
  theme_bw(base_size=11) +
  theme(legend.position="bottom",
        panel.grid.major.y=element_blank())

ggsave(file.path(VALID_DIR,"fig_made_by_model.png"),
       p_made, width=9, height=7, dpi=200)

# Time-series MADE
ts_made <- cost_wide %>%
  filter(is.na(alpha_val) | alpha_val==0.50) %>%
  mutate(model_lbl=if_else(model=="CC-CoNA",
                           "CC-CoNA (α=0.50)",model)) %>%
  group_by(model_lbl, fecha) %>%
  dplyr::summarise(
    MADE_S1=mean(abs_delta_S1,na.rm=TRUE),
    MADE_S2=mean(abs_delta_S2,na.rm=TRUE),
    .groups="drop") %>%
  pivot_longer(c(MADE_S1,MADE_S2),
               names_to="scenario", values_to="MADE") %>%
  mutate(scenario_lbl=if_else(scenario=="MADE_S1","BASE","S5"))

p_ts <- ggplot(ts_made,
               aes(x=fecha, y=MADE,
                   color=scenario_lbl, linetype=scenario_lbl)) +
  geom_line(linewidth=0.8) +
  facet_wrap(~model_lbl, ncol=2, scales="free_y") +
  scale_color_manual(values=c("BASE"=COL_BASE,"S5"=COL_S5)) +
  scale_linetype_manual(values=c("BASE"="dashed","S5"="solid")) +
  scale_x_date(date_breaks="2 years", date_labels="%Y") +
  labs(title="MADE over time by model",
       subtitle="Jan 2000–Mar 2018 | CC-CoNA at α=0.50",
       x=NULL, y="MADE (%)", color=NULL, linetype=NULL) +
  theme_bw(base_size=11) +
  theme(legend.position="bottom",
        panel.grid.minor=element_blank(),
        strip.text=element_text(face="bold"),
        axis.text.x=element_text(angle=45,hjust=1))

ggsave(file.path(VALID_DIR,"fig_made_timeseries.png"),
       p_ts, width=11, height=8, dpi=200)

# -----------------------------------------------------------------------
# 12. Scatter plots — observed vs estimated
# -----------------------------------------------------------------------
message("Building scatter plots...")

scatter_data <- cost_wide %>%
  filter(is.na(alpha_val) | alpha_val %in% c(0, 0.5, 1)) %>%
  mutate(
    model_lbl = case_when(
      model == "CC-CoNA" & alpha_val == 0   ~ "CC-CoNA (\u03b1=0)",
      model == "CC-CoNA" & alpha_val == 0.5 ~ "CC-CoNA (\u03b1=0.50)",
      model == "CC-CoNA" & alpha_val == 1   ~ "CC-CoNA (\u03b1=1.00)",
      TRUE ~ model),
    ciudad_lbl = case_when(
      ciudad == "BOGOTA"   ~ "Bogot\u00e1",
      ciudad == "MEDELLIN" ~ "Medell\u00edn",
      TRUE                 ~ ciudad))

make_scatter <- function(df, est_col, est_lab, scenario_col) {
  ggplot(df, aes(x = cost_S3, y = .data[[est_col]],
                 color = ciudad_lbl)) +
    geom_point(size = 0.8, alpha = 0.4) +
    geom_abline(intercept = 0, slope = 1,
                color = "grey30", linewidth = 0.6,
                linetype = "dashed") +
    geom_smooth(method = "lm", se = FALSE,
                linewidth = 0.6, alpha = 0.8) +
    facet_wrap(~ model_lbl, scales = "free", ncol = 2) +
    scale_color_manual(
      values = c(
        "Bogot\u00e1"   = "#2C3E6B",
        "Medell\u00edn" = "#C0392B",
        "Cali"          = "#1A7A4A"),
      name = NULL) +
    scale_x_continuous(labels = comma_format(big.mark=",")) +
    scale_y_continuous(labels = comma_format(big.mark=",")) +
    labs(
      title    = paste0("Observed vs estimated diet cost — ",
                        est_lab),
      subtitle = "Dashed line = 45\u00b0 (perfect fit). Points = city\u2013month\u2013member.",
      caption  = paste0(
        "Note: Each point represents a city\u2013month\u2013member observation. ",
        "Jan 2000\u2013Mar 2018."),
      x = "Observed cost (COP/day)",
      y = paste0("Estimated cost — ", est_lab, " (COP/day)")) +
    theme_bw(base_size = 11) +
    theme(
      text             = element_text(family = "serif"),
      legend.position  = "bottom",
      legend.text      = element_text(size = 10),
      strip.text       = element_text(face = "bold", size = 10),
      panel.grid.minor = element_blank(),
      axis.text        = element_text(size = 8))
}

p_scatter_S1 <- make_scatter(scatter_data, "cost_S1", "BASE")
p_scatter_S2 <- make_scatter(scatter_data, "cost_S2", "S5")

ggsave(file.path(VALID_DIR, "fig_scatter_BASE.png"),
       p_scatter_S1, width=12, height=10, dpi=200, bg="white")
ggsave(file.path(VALID_DIR, "fig_scatter_S5.png"),
       p_scatter_S2, width=12, height=10, dpi=200, bg="white")

message("Scatter plots saved.")

# -----------------------------------------------------------------------
# 13. Save outputs
# -----------------------------------------------------------------------
message("Saving outputs...")

write_csv(cost_wide,
          file.path(VALID_DIR,"validation_cost_all.csv"))

write_xlsx(
  list(
    cost_wide    = cost_wide,
    global       = global_summary %>%
      pivot_longer(everything(),
                   names_to="metric", values_to="value"),
    by_model     = model_summary,
    by_city      = city_summary,
    shadow_prices= bind_rows(
      res_S1$spe %>% mutate(scenario="S1_BASE"),
      res_S2$spe %>% mutate(scenario="S2_S5"),
      res_S3$spe %>% mutate(scenario="S3_Obs")) %>%
      mutate(fecha=as.Date(fecha))),
  file.path(VALID_DIR,"validation_results.xlsx"))

message(sprintf("Done. Outputs saved to: %s", VALID_DIR))