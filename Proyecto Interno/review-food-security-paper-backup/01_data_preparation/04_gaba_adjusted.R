########################################################
## SCRIPT 01_data_preparation/04_gaba_adjusted.R
## Ajusta los intercambios GABA por EER de cada ciudad
## El factor de ajuste es constante en el tiempo
## (EER no varía — se usa el mismo para todo el paper)
##
## Reads:  PREP_DIR/gaba_exchanges_base.rds
##         PREP_DIR/gaba_energy_base.rds
##         IN_EER (220326_agg_eer.xlsx)
##
## Writes: PREP_DIR/gaba_exchanges_adj.rds
##         PREP_DIR/gaba_exchanges_adj.xlsx
##         PREP_DIR/fig_gaba_eer_comparison.png
########################################################

source("00_config.R")
library(tidyverse)
library(readxl)
library(writexl)

# -----------------------------------------------------------------------
# Load inputs
# -----------------------------------------------------------------------
message("Loading inputs...")

df.exchanges <- readRDS(file.path(PREP_DIR, "gaba_exchanges_base.rds"))
df.energia   <- readRDS(file.path(PREP_DIR, "gaba_energy_base.rds"))
agg_eer      <- read_excel(IN_EER)

# -----------------------------------------------------------------------
# Harmonise age ranges and sex labels
# -----------------------------------------------------------------------
map_edad <- tribble(
  ~rango,     ~edad,
  "[2, 6)",   "2 to 5",
  "[6, 10)",  "6 to 9",
  "[10, 14)", "10 to 13",
  "[14, 18)", "14 to 17",
  "[18,31)",  "19 to 59",
  "[31,51)",  "19 to 59",
  "[51,70)",  "> 60"
)

map_sexo <- tribble(
  ~sexo,    ~sex,
  "male",   "Masculino",
  "female", "Femenino"
)

df.exchanges_exp <- map_edad %>%
  left_join(df.exchanges, by = "edad", relationship = "many-to-many") %>%
  left_join(map_sexo,     by = "sexo") %>%
  select(-sexo)

df.energia_exp <- map_edad %>%
  left_join(df.energia, by = "edad", relationship = "many-to-many") %>%
  left_join(map_sexo,   by = "sexo") %>%
  select(-sexo)

# -----------------------------------------------------------------------
# Compute adjustment factors: EER_city / EER_GABA
# -----------------------------------------------------------------------
message("Computing adjustment factors...")

eer_gaba <- df.energia_exp %>%
  filter(tipo_fila == "recomendacion") %>%
  select(sex, rango, eer_gabas = energia_kcal)

gaba_eer <- merge(agg_eer, eer_gaba,
                  by = c("sex", "rango"), all.x = TRUE) %>%
  mutate(factor_ajuste = eer / eer_gabas)

# -----------------------------------------------------------------------
# Apply adjustment to exchanges
# -----------------------------------------------------------------------
exchanges_eer <- merge(gaba_eer,
                       df.exchanges_exp,
                       by = c("sex", "rango"),
                       all.x = TRUE) %>%
  mutate(
    n_exchanges_adj = n_exchanges * factor_ajuste,
    e_kcal_adj      = e_kcal      * factor_ajuste)

# Normalise city codes for downstream model compatibility
exchanges_eer <- exchanges_eer %>%
  mutate(ciudad = case_when(
    cod_mun == "05001" ~ "MEDELLIN",
    cod_mun == "11001" ~ "BOGOTA",
    cod_mun == "76001" ~ "CALI",
    TRUE               ~ cod_mun))

message(sprintf("  exchanges_eer: %d rows | %d cities",
                nrow(exchanges_eer),
                n_distinct(exchanges_eer$ciudad)))

# -----------------------------------------------------------------------
# Diagnostic: compare adjusted vs original energy supply
# -----------------------------------------------------------------------
aporte_adj <- exchanges_eer %>%
  group_by(cod_mun, sex, rango) %>%
  summarise(aporte_adj = sum(e_kcal_adj, na.rm=TRUE), .groups="drop")

df.aportes <- df.energia_exp %>%
  filter(tipo_fila == "aporte_total") %>%
  mutate(aporte_original = energia_kcal)

compare_aportes <- merge(aporte_adj, df.aportes,
                         by = c("sex", "rango")) %>%
  select(cod_mun, sex, rango, aporte_adj, aporte_original) %>%
  merge(exchanges_eer %>%
          select(cod_mun, sex, rango, eer, eer_gabas) %>%
          distinct(),
        all.x = TRUE)

# -----------------------------------------------------------------------
# Figure: energy comparison
# -----------------------------------------------------------------------
compare_long <- compare_aportes %>%
  distinct(cod_mun, sex, rango,
           aporte_adj, aporte_original, eer, eer_gabas) %>%
  pivot_longer(
    cols      = c(aporte_original, eer_gabas, aporte_adj, eer),
    names_to  = "fuente",
    values_to = "kcal") %>%
  mutate(fuente = factor(fuente,
                         levels = c("aporte_original","eer_gabas","aporte_adj","eer"),
                         labels = c("GABA supply","GABA recommendation",
                                    "Adjusted supply","City EER")))

colores <- c(
  "GABA supply"        = "#B0C4DE",
  "GABA recommendation"= "#4682B4",
  "Adjusted supply"    = "#F4A460",
  "City EER"           = "#D2691E")

p_compare <- ggplot(compare_long,
                    aes(x = rango, y = kcal, fill = fuente)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  facet_grid(cod_mun ~ sex) +
  scale_fill_manual(values = colores, name = NULL) +
  scale_y_continuous(labels = scales::comma_format(suffix = " kcal")) +
  labs(
    title    = "Energy supply comparison: GABA original vs adjusted by city EER",
    x        = "Age range",
    y        = "Energy (kcal)") +
  theme_bw(base_size = 11) +
  theme(axis.text.x     = element_text(angle = 45, hjust = 1),
        legend.position  = "bottom",
        strip.text       = element_text(face = "bold"))

ggsave(file.path(PREP_DIR, "fig_gaba_eer_comparison.png"),
       p_compare, width = 12, height = 8, dpi = 300)

# -----------------------------------------------------------------------
# Save
# -----------------------------------------------------------------------
exchanges_out <- exchanges_eer %>%
  select(cod_mun, ciudad, sex, rango,
         grupo_principal,
         n_exchanges, e_kcal,
         factor_ajuste,
         n_exchanges_adj, e_kcal_adj)

saveRDS(exchanges_out,  file.path(PREP_DIR, "gaba_exchanges_adj.rds"))
write_xlsx(exchanges_out, file.path(PREP_DIR, "gaba_exchanges_adj.xlsx"))

message("Done. Run 05_eer.R next.")
