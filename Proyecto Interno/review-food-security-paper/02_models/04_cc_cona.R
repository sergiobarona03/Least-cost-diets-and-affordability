########################################################
## SCRIPT 02_models/04_ccona.R
## IPC-Constrained Cost of Nutritional Adequacy (CC-CoNA)
## Alpha grid: 0, 0.25, 0.50, 0.75, 1.00
##   alpha = 0    : standard CoNA (no IPC constraint)
##   alpha = 1.00 : full IPC quantity share enforced
##
## Reads:  PREP_DIR/panel_food_paper.rds
##         PREP_DIR/household_eer_ll.rds
##         PREP_DIR/household_ul.rds
##         BASE_DIR/food-security-paper/input/cpi-weights/
##           cpi-weights-2024.xlsx
##         IN_AUX_DIR/CoNA_IPC_paper.R
##
## Writes: CCONA_DIR/ccona_results.rds
##         CCONA_DIR/ccona_results.xlsx
##         CCONA_DIR/ccona_alpha{N}.rds  ← one per alpha value
########################################################

source("00_config.R")
library(tidyverse)
library(readxl)
library(writexl)
library(FoodpriceR)

source(file.path(IN_AUX_DIR, "CoNA_IPC_paper.R"))

# -----------------------------------------------------------------------
# Load inputs
# -----------------------------------------------------------------------
message("Loading inputs...")

data_paper <- readRDS(file.path(PREP_DIR, "panel_food_paper.rds"))

# panel_food_paper.rds does not contain subclase_ipc
# Recover from panel_city_cod_subclase.csv via ciudad + articulo
subclase_lookup <- read.csv(
  file.path(REVIEW_DIR, "input", "prices", "panel_city_cod_subclase.csv"),
  sep = ";") %>%
  select(ciudad, articulo, subclase_ipc) %>%
  distinct()

data_paper <- data_paper %>%
  left_join(subclase_lookup, by = c("ciudad", "articulo")) %>%
  mutate(Group = subclase_ipc)

message(sprintf(
  "  subclase_ipc coverage: %d/%d rows have Group",
  sum(!is.na(data_paper$Group)), nrow(data_paper)))

message(sprintf(
  "  subclase_ipc coverage: %d/%d rows have Group",
  sum(!is.na(data_paper$Group)), nrow(data_paper)))

household_eer_ll <- readRDS(file.path(PREP_DIR, "household_eer_ll.rds"))
household_ul     <- readRDS(file.path(PREP_DIR, "household_ul.rds"))

# IPC expenditure share weights (2024 base)
in_weights <- file.path(BASE_DIR,
                        "food-security-paper", "input", "cpi-weights", "cpi-weights-2024.xlsx")

weights_24 <- read_excel(in_weights) %>%
  janitor::clean_names() %>%
  select(nivel, codigo, nombre, total_ingresos)

# subclase_ipc is numeric short code (e.g. 11501)
# weights_24$codigo is DANE format (e.g. "01110101" = "0" + "11501" + "00" → "01150100"? )
# Correct mapping: pad to 8 digits as "0" + 5-digit code + "00"
subclases_v <- unique(data_paper$subclase_ipc)
subclases_v_fmt <- sprintf("0%05d00", as.integer(subclases_v))

message(sprintf("  subclase codes (formatted): %s",
                paste(head(subclases_v_fmt, 5), collapse = ", ")))
message(sprintf("  weights_24 sample codes:    %s",
                paste(head(weights_24$codigo, 5), collapse = ", ")))
message(sprintf("  Intersection: %d codes match",
                length(intersect(subclases_v_fmt, weights_24$codigo))))

ipc_shares <- weights_24 %>%
  filter(nivel == "Subclase", codigo %in% subclases_v_fmt) %>%
  mutate(
    share = total_ingresos / sum(total_ingresos),
    Group = codigo
  ) %>%
  select(Group, nombre, share) %>%
  as.data.frame()

# Map data_paper$Group to formatted codes
data_paper <- data_paper %>%
  mutate(Group = sprintf("0%05d00", as.integer(subclase_ipc)))

stopifnot(abs(sum(ipc_shares$share) - 1) < 1e-6)

missing_groups <- setdiff(ipc_shares$Group, unique(data_paper$Group))
if (length(missing_groups) > 0)
  warning("Groups in ipc_shares not in data_paper$Group: ",
          paste(missing_groups, collapse = ", "))

message(sprintf(
  "  data_paper: %d rows | %d cities | %d dates | %d IPC groups",
  nrow(data_paper),
  n_distinct(data_paper$ciudad),
  n_distinct(data_paper$fecha),
  nrow(ipc_shares)))

# -----------------------------------------------------------------------
# Alpha grid
# -----------------------------------------------------------------------
alphas   <- c(0, 0.25, 0.50, 0.75, 1.00)
dominios <- sort(unique(household_eer_ll$ciudad))
fechas   <- sort(unique(data_paper$fecha))

message(sprintf(
  "Estimating CC-CoNA: %d alphas × %d cities × %d dates...",
  length(alphas), length(dominios), length(fechas)))

# -----------------------------------------------------------------------
# Loop: alpha × city × date
# -----------------------------------------------------------------------
out_cost  <- list()
out_comp  <- list()
out_limit <- list()
out_spe   <- list()

for (alp in alphas) {
  
  message(sprintf("\n  alpha = %.2f", alp))
  n_ok <- 0L; n_fail <- 0L
  
  for (i in dominios) {
    
    eer_ll.aux <- household_eer_ll %>%
      filter(ciudad == i) %>%
      select(-ciudad, -any_of("cod_mun")) %>%
      as.data.frame()
    ul.aux <- household_ul %>%
      filter(ciudad == i) %>%
      select(-ciudad, -any_of("cod_mun")) %>%
      as.data.frame()
    
    for (t in fechas) {
      
      data.aux <- data_paper %>%
        filter(ciudad == i, fecha == t, !is.na(precio_100g)) %>%
        filter(articulo != "ARROZ PARA SOPA") %>%
        dplyr::rename(
          Food          = articulo,
          Price_100g    = precio_100g,
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
        as.data.frame()
      
      if (nrow(data.aux) == 0) next
      
      result <- tryCatch(
        CoNA_IPC_paper(
          data       = data.aux,
          EER_LL     = eer_ll.aux,
          UL         = ul.aux,
          IPC_shares = ipc_shares,
          alpha      = alp),
        error = function(e) {
          warning("Error — ", i, " | ", t,
                  " | alpha=", alp, " | ", conditionMessage(e))
          NULL
        })
      
      if (!is.null(result)) {
        out_cost[[length(out_cost) + 1]] <- result$cost %>%
          mutate(ciudad = i, fecha = t, alpha_val = alp)
        out_comp[[length(out_comp) + 1]] <- result$comp %>%
          mutate(ciudad = i, fecha = t, alpha_val = alp)
        out_limit[[length(out_limit) + 1]] <- result$limit %>%
          mutate(ciudad = i, fecha = t, alpha_val = alp)
        out_spe[[length(out_spe) + 1]] <- result$spe %>%
          mutate(ciudad = i, fecha = t, alpha_val = alp)
        n_ok <- n_ok + 1L
      } else {
        n_fail <- n_fail + 1L
      }
    }
  }
  message(sprintf("    %d OK | %d failed", n_ok, n_fail))
}

# -----------------------------------------------------------------------
# Consolidate
# -----------------------------------------------------------------------
df.cost  <- bind_rows(out_cost)  %>% mutate(fecha = as.Date(fecha))
df.comp  <- bind_rows(out_comp)  %>% mutate(fecha = as.Date(fecha))
df.limit <- bind_rows(out_limit) %>% mutate(fecha = as.Date(fecha))
df.spe   <- bind_rows(out_spe)   %>% mutate(fecha = as.Date(fecha))

message(sprintf("\n  Total rows in df.cost: %d", nrow(df.cost)))

# -----------------------------------------------------------------------
# Save — combined + one file per alpha
# -----------------------------------------------------------------------
saveRDS(list(cost  = df.cost,
             comp  = df.comp,
             limit = df.limit,
             spe   = df.spe),
        file.path(CCONA_DIR, "ccona_results.rds"))

write_xlsx(list(cost  = df.cost,
                comp  = df.comp,
                limit = df.limit,
                spe   = df.spe),
           file.path(CCONA_DIR, "ccona_results.xlsx"))

# One RDS per alpha for convenience
for (alp in alphas) {
  alp_tag <- gsub("\\.", "", as.character(alp))
  saveRDS(
    list(cost  = df.cost  %>% filter(alpha_val == alp),
         comp  = df.comp  %>% filter(alpha_val == alp),
         limit = df.limit %>% filter(alpha_val == alp),
         spe   = df.spe   %>% filter(alpha_val == alp)),
    file.path(CCONA_DIR, paste0("ccona_alpha", alp_tag, ".rds")))
}

message("Done. Run 05_hcost.R next.")