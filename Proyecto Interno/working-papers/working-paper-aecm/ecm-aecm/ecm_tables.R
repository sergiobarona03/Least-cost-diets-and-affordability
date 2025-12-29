############################################################
## ecm_tables.R
## Builds your 5 Excel tables from date_tag_ecm_raw.rds
## Each table is exported as a separate .xlsx file
############################################################

library(tidyverse)
library(writexl)

setwd("C:\\Users\\sergio.barona\\Desktop\\Least-cost-diets-and-affordability\\Proyecto Interno\\")
date_tag <- "261225"

out_dir <- "working-papers\\working-paper-aecm\\output\\ts-ecm\\"
rds_path <- file.path(out_dir, paste0(date_tag, "_ecm_raw.rds"))

obj <- readRDS(rds_path)

coint_long <- obj$coint_long
lr_long <- obj$lr_long
ecm_long <- obj$ecm_long
lags_long <- obj$lags_long

pair_lab <- function(art, ali) paste0(art, " / ", ali)

# -----------------------------
# Table 1: cointegration p-values
# -----------------------------
tab1 <- coint_long %>%
  mutate(pair = pair_lab(articulo_ipc, alimento_sipsa)) %>%
  pivot_longer(starts_with("p_type"),
               names_to = "type", values_to = "p") %>%
  mutate(type = recode(type, p_type1 = "Type 1", p_type2 = "Type 2", p_type3 = "Type 3")) %>%
  unite(col = "col", city, type, sep = " - ") %>%
  select(pair, col, p) %>%
  pivot_wider(names_from = col, values_from = p) %>%
  arrange(pair) %>%
  select(pair,
         any_of(c("Cali - Type 1","Cali - Type 2","Cali - Type 3",
                  "Bogotá - Type 1","Bogotá - Type 2","Bogotá - Type 3",
                  "Medellín - Type 1","Medellín - Type 2","Medellín - Type 3")))

# -----------------------------
# Table 2: long-run beta (b,se,p) by city
# -----------------------------
tab2 <- lr_long %>%
  mutate(pair = pair_lab(articulo_ipc, alimento_sipsa)) %>%
  pivot_longer(c(b, se, p_value), names_to = "stat", values_to = "val") %>%
  mutate(stat = recode(stat, b = "b", se = "se", p_value = "p-value")) %>%
  unite(col = "col", city, stat, sep = " - ") %>%
  select(pair, col, val) %>%
  pivot_wider(names_from = col, values_from = val) %>%
  arrange(pair) %>%
  select(pair,
         any_of(c("Cali - b","Cali - se","Cali - p-value",
                  "Bogotá - b","Bogotá - se","Bogotá - p-value",
                  "Medellín - b","Medellín - se","Medellín - p-value")))

# -----------------------------
# Nice terms for ECM tables
# -----------------------------
nice_term <- function(term) {
  if (term == "(Intercept)") return("Intercept")
  if (term == "ect_l1") return("e_{t-1}")
  if (str_detect(term, "^dlog_ipc_l[0-9]+$")) {
    k <- str_extract(term, "[0-9]+")
    return(paste0("Δy_{t-", k, "}"))
  }
  if (str_detect(term, "^dlog_sipsa_l[0-9]+$")) {
    k <- str_extract(term, "[0-9]+")
    if (k == "0") return("Δx_{t}")
    return(paste0("Δx_{t-", k, "}"))
  }
  if (str_detect(term, "^mes")) return(NA_character_)
  NA_character_
}

build_city_ecm_table <- function(city_name, ecm_long) {
  
  tmp <- ecm_long %>%
    filter(city == city_name) %>%
    mutate(pair = pair_lab(articulo_ipc, alimento_sipsa),
           term2 = map_chr(term, nice_term)) %>%
    filter(!is.na(term2)) %>%
    select(pair, term2, b, se, p_value)
  
  pairs <- sort(unique(tmp$pair))
  out <- list()
  
  for (pp in pairs) {
    header <- tibble(pair = pp, term = "", b = NA_real_, se = NA_real_, `p-value` = NA_real_)
    block <- tmp %>%
      filter(pair == pp) %>%
      transmute(pair = "", term = term2, b = b, se = se, `p-value` = p_value)
    out[[length(out) + 1]] <- bind_rows(header, block)
  }
  
  bind_rows(out)
}

tab3 <- build_city_ecm_table("Cali", ecm_long)
tab4 <- build_city_ecm_table("Bogotá", ecm_long)
tab5 <- build_city_ecm_table("Medellín", ecm_long)

# -----------------------------
# (Optional) Save lag-selection top-5 table too (often useful)
# -----------------------------
tab_lags <- lags_long %>%
  mutate(pair = pair_lab(articulo_ipc, alimento_sipsa)) %>%
  select(city, pair, k, crit, criterion) %>%
  arrange(city, pair, crit)


# -----------------------------
# Export: EACH TABLE in a separate .xlsx
# -----------------------------
write_xlsx(list("Table1" = tab1), file.path(out_dir, paste0(date_tag, "_table1_coint.xlsx")))
write_xlsx(list("Table2" = tab2), file.path(out_dir, paste0(date_tag, "_table2_longrun.xlsx")))
write_xlsx(list("Table3" = tab3), file.path(out_dir, paste0(date_tag, "_table3_ecm_cali.xlsx")))
write_xlsx(list("Table4" = tab4), file.path(out_dir, paste0(date_tag, "_table4_ecm_bogota.xlsx")))
write_xlsx(list("Table5" = tab5), file.path(out_dir, paste0(date_tag, "_table5_ecm_medellin.xlsx")))

# optional lags file
write_xlsx(list("Top5_lags" = tab_lags), file.path(out_dir, paste0(date_tag, "_table_lags_top5.xlsx")))

cat("\nSaved separate .xlsx tables in:\n", out_dir, "\n")
