########################################################
## Ajuste de recomendaciones GABAS según aporte
## energético por grupo demográfico y ciudad
########################################################

# Librerías
library(tidyverse)
library(readxl)
library(writexl)

##----------------------------------------------------------
## Directorios
##----------------------------------------------------------

base_dir <- "C:\\Users\\Portatil\\Desktop\\Least-cost-diets-and-affordability\\Proyecto Interno"

# Inputs
path_gaba <- file.path(base_dir, "food-security-paper", "input",  "gaba-recommend")

# Outputs
out_gaba  <- file.path(base_dir, "food-security-paper", "output", "gaba")
out_eer   <- file.path(base_dir, "food-security-paper", "output", "eer")

##----------------------------------------------------------
## Cargar archivos
##----------------------------------------------------------

# GABAS: intercambios y energía base
df.exchanges <- readxl::read_excel(file.path(out_gaba, "230326_gaba_exchanges_base.xlsx"))
df.energia   <- readxl::read_excel(file.path(out_gaba, "230326_gaba_energy_base.xlsx"))

# EER: requerimientos energéticos por grupo demográfico
agg_eer <- readxl::read_excel(file.path(out_eer, "220326_agg_eer.xlsx"))


##----------------------------------------------------------
## Armonizar rangos de edad y sexo entre df.exchanges y agg_eer
##----------------------------------------------------------

# Tabla de correspondencia: rango agg_eer → edad df.exchanges
map_edad <- tribble(
  ~rango,      ~edad,
  "[2, 6)",    "2 to 5",
  "[6, 10)",   "6 to 9",
  "[10, 14)",  "10 to 13",
  "[14, 18)",  "14 to 17",
  "[18,31)",   "19 to 59",
  "[31,51)",   "19 to 59",
  "[51,70)",   "> 60"      # > 60 toma los requerimientos de [51,70)
)

# Tabla de correspondencia: sexo
map_sexo <- tribble(
  ~sexo,    ~sex,
  "male",   "Masculino",
  "female", "Femenino"
)

# Expandir df.exchanges a los rangos de agg_eer y armonizar sexo
df.exchanges_exp <- map_edad %>%
  left_join(df.exchanges, by = "edad", relationship = "many-to-many") %>%
  left_join(map_sexo, by = "sexo") %>%
  select(-sexo)


# Extender la energía
df.energia_exp <- map_edad %>%
  left_join(df.energia, by = "edad", relationship = "many-to-many") %>%
  left_join(map_sexo, by = "sexo") %>%
  select(-sexo) 

##----------------------------------------------------------
## Crear factores de ajuste
##----------------------------------------------------------

df.energia_exp2  = df.energia_exp %>% filter(
  tipo_fila == "recomendacion"
)
gaba_eer = merge(agg_eer, df.energia_exp2[c("sex", "rango",
                                           "energia_kcal")],
                 by = c("sex", "rango"), all.x = TRUE) %>%
  rename(eer_gabas = energia_kcal)


gaba_eer = gaba_eer %>% mutate(
  factor_ajuste = eer/eer_gabas
)

# Recuperar las recomendaciones
exchanges_eer = merge(gaba_eer, 
                      df.exchanges_exp, by = c("sex", "rango"),
                      all.x = TRUE)

# Ajustar
exchanges_eer = exchanges_eer %>%
  mutate(n_exchanges_adj = n_exchanges*factor_ajuste,
         e_kcal_adj = e_kcal*factor_ajuste)

# Calcular aportes
aporte_adj = exchanges_eer %>% 
  select(cod_mun, sex, rango, grupo_principal, e_kcal_adj) %>% 
  distinct()

aporte_adj = aporte_adj %>% group_by(cod_mun, sex, rango) %>%
  summarize(aporte_adj = sum(e_kcal_adj, na.rm = TRUE))

# Comparar con los aportes originales
df.aportes  = df.energia_exp %>% filter(
  tipo_fila == "aporte_total"
) %>% mutate(aporte_original = energia_kcal)

# Comparar con aportes y requerimientos
compare.aportes = merge(aporte_adj, df.aportes, 
                        by = c("sex", "rango")) %>%
  select(cod_mun, sex, rango, aporte_adj, aporte_original)

req_eer = exchanges_eer %>% select(cod_mun, sex, rango, eer,
                                   eer_gabas)

compare.aportes = merge(compare.aportes, req_eer,
                        all.x = T)

##----------------------------------------------------------
## Gráfica de barras: comparación de aportes energéticos
##----------------------------------------------------------

library(tidyverse)

# Transformar a formato largo para graficar
compare_long <- compare.aportes %>%
  distinct(cod_mun, sex, rango, aporte_adj, aporte_original, eer, eer_gabas) %>%
  pivot_longer(
    cols      = c(aporte_original, eer_gabas, aporte_adj, eer),
    names_to  = "fuente",
    values_to = "kcal"
  ) %>%
  mutate(
    fuente = factor(fuente, 
                    levels = c("aporte_original", "eer_gabas", "aporte_adj", "eer"),
                    labels = c("Aporte GABAS", "Recomendación GABAS", 
                               "Aporte ajustado", "EER ciudad"))
  )

# Paleta
colores <- c(
  "Aporte GABAS"        = "#B0C4DE",
  "Recomendación GABAS" = "#4682B4",
  "Aporte ajustado"     = "#F4A460",
  "EER ciudad"          = "#D2691E"
)

# Gráfica
plot_compare = ggplot(compare_long, aes(x = rango, y = kcal, fill = fuente)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  facet_grid(cod_mun ~ sex) +
  scale_fill_manual(values = colores, name = NULL) +
  scale_y_continuous(labels = scales::comma_format(suffix = " kcal")) +
  labs(
    title    = "Comparación de aportes energéticos por ciudad, sexo y grupo de edad",
    subtitle = "GABAS original vs. ajustado por EER de cada ciudad",
    x        = "Rango de edad",
    y        = "Energía (kcal)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x      = element_text(angle = 45, hjust = 1),
    legend.position  = "bottom",
    strip.text       = element_text(face = "bold"),
    panel.grid.major.x = element_blank()
  )


##----------------------------------------------------------
## Guardar outputs de GABAS
##----------------------------------------------------------
out_gaba = "food-security-paper\\output\\gaba\\"

# Gráfica de comparación
ggsave(plot = plot_compare, file.path(base_dir,
                                          out_gaba,
                                            "230326_compare_eer_gaba.jpeg"),
       width = 12, height = 8, dpi = 300)

# Guardar intercambios ajustados por EER
exchanges_eer_out = exchanges_eer %>%
  select(cod_mun, sex, rango, grupo_principal, 
         n_exchanges, e_kcal, factor_ajuste,
         n_exchanges_adj, e_kcal_adj)

writexl::write_xlsx(exchanges_eer_out, file.path(base_dir,
                               out_gaba,
                               "230326_ajd_gabas_exchanges.xlsx"))

