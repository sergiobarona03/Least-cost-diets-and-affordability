#######################################################################
# Ridgeline Plot - Ingreso per cápita TRIMESTRAL por ciudad
#######################################################################

library(tidyverse)
library(lubridate)
library(ggridges)

#----------------------------------------------------------------------
# 1. Cargar datos
#----------------------------------------------------------------------

df_2018 <- readRDS("working-papers/working-paper-ipc/output/incomecol/IncomeCol_2018.rds")
df_2019 <- readRDS("working-papers/working-paper-ipc/output/incomecol/IncomeCol_2019.rds")
df_2020 <- readRDS("working-papers/working-paper-ipc/output/incomecol/IncomeCol_2020.rds")

df <- bind_rows(df_2018, df_2019, df_2020)

#----------------------------------------------------------------------
# 2. Preparar variables
#----------------------------------------------------------------------

df <- df %>%
  mutate(
    fecha = ymd(paste(year, mes, "01", sep = "-")),
    trimestre = paste0(year(fecha), " Q", quarter(fecha)),
    ciudad = dominio,
    ingreso_pc = as.numeric(per_capita_income)
  ) %>%
  filter(ciudad %in% c("BOGOTA","MEDELLIN","CALI"),
         !is.na(ingreso_pc),
         ingreso_pc > 0)

# Recorte de outliers (recomendado)
df <- df %>%
  filter(ingreso_pc < quantile(ingreso_pc, 0.99, na.rm = TRUE))

# Orden correcto de trimestres
df <- df %>%
  arrange(fecha) %>%
  mutate(trimestre = factor(trimestre,
                            levels = unique(trimestre)))

#----------------------------------------------------------------------
# 3. Ridgeline plot trimestral
#----------------------------------------------------------------------

plot_income <- ggplot(df,
                      aes(x = log(ingreso_pc),   # log mejora la forma
                          y = trimestre,
                          group = trimestre,
                          fill = ciudad)) +
  stat_density_ridges(quantile_lines = FALSE,
                      quantiles = c(0.25, 0.5,
                                    0.75),
                      alpha = 0.4,
                      scale = 2,
                      rel_min_height = 0.01) +
  facet_wrap(~ciudad, ncol = 3, scales = "free_x") +
  theme_ridges() +
  theme(legend.position = "top",
        legend.title = element_blank()) +
  labs(y = "Quarter", 
       x = "Ln(Income PC)")
plot_income 

#----------------------------------------------------------------------
# 4. Guardar
#----------------------------------------------------------------------

ggsave("working-papers/working-paper-ipc/output/incomecol/income_ridgeline_plot_quarterly.png",
       plot = plot_income,
       width = 14,
       height = 8,
       dpi = 300, bg = "white")

#######################################################################
#                               FIN
#######################################################################
