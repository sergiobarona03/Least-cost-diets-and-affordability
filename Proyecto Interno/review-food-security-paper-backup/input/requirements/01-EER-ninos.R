########################################################
## Procesamiento de los requerimientos energéticos
## en niños y adolescentes
## Nota: el procedimiento consiste en calcular un promedio
## ponderado considerando la estructura poblacional del CNPV-2018
########################################################

# Librerías
library(tidyverse)
library(readxl)

# Directorio base
base_dir = "C:\\Users\\Portatil\\Desktop\\Least-cost-diets-and-affordability\\Proyecto Interno"

# ------------------------------------------------------------
# 1. Cargar RIEN para niños y niñas
# ------------------------------------------------------------
male_rien = readxl::read_excel(file.path(base_dir,
                                           "food-security-paper\\input\\requirements\\eer-ninos-adultos.xlsx"),
                                 sheet = 1) %>% mutate(sex = "male")
female_rien = readxl::read_excel(file.path(base_dir,
                                           "food-security-paper\\input\\requirements\\eer-ninos-adultos.xlsx"),
                                 sheet = 2) %>% mutate(sex = "female")

rien_energy = rbind(male_rien, female_rien) %>%
  mutate(rango = paste0(Lim_inf, " - ",
                        Lim_sup))

# ------------------------------------------------------------
# 2. Cargar la estructura poblacional
# ------------------------------------------------------------

# Ruta censo
cnpv_dir  <- "\\food-security-paper\\input\\requirements\\cnpv-2018-municipios.xlsx"
cnpv_mun <- readxl::read_excel(file.path(base_dir,cnpv_dir)) %>%
  filter(edad != "Total grupo edad")

# Completar la base de datos
cnpv_mun <- cnpv_mun %>%
  fill(dpto, municipio, grupo, .direction = "down")
cnpv_mun$cod_mun = substr(cnpv_mun$municipio, 1, 5)

# Definir los municipios de interés
cnpv_mun = cnpv_mun %>% filter(
  cod_mun %in% c("05001", "11001", "76001")
)

# Definir variables de interés
cnpv_mun = cnpv_mun %>%
  mutate(m = cabe_male,
         f = cabe_female,
         Lim_inf = edad) %>%
  select(cod_mun, Lim_inf, m, f)

cnpv_mun <- cnpv_mun %>%
  pivot_longer(
    cols      = c(m, f),
    names_to  = "sex",
    values_to = "n"
  ) %>%
  mutate(sex = case_when(
    sex == "m" ~ "male",
    sex == "f" ~ "female"
  ))

# ------------------------------------------------------------
# 3. Merge entre requerimientos y población
# ------------------------------------------------------------

# Base de datos agregada
cnpv_mun_eer = merge(cnpv_mun %>%
                       filter(as.numeric(Lim_inf) <= 17,
                              as.numeric(Lim_inf) > 1), rien_energy %>%
                       filter(Moderada  == 1), 
                     by = c("Lim_inf", "sex"),
                     all.x = TRUE)

cnpv_mun_eer = cnpv_mun_eer %>% 
  select(cod_mun, Lim_inf, sex, KCAL_DIA, n) %>% clean_names()

# Construir rango
cnpv_mun_eer <- cnpv_mun_eer %>%
  mutate(
    rango = cut(
      as.numeric(lim_inf),
      breaks         = c(2, 6, 10, 14, 18),
      right          = FALSE,
      labels         = c("[2, 6)", "[6, 10)", "[10, 14)", "[14, 18)")
    )
  )

# Estimar EER por promedio ponderado (diferenciado por ciudad)
child_eer = cnpv_mun_eer %>%
  group_by(cod_mun, sex, rango) %>%
  summarize(eer = weighted.mean(x = as.numeric(kcal_dia),
                                w = n))

# Estimar EER nacional
col_child_eer = cnpv_mun_eer  %>% select(rango, sex, kcal_dia) %>%
  distinct() %>% mutate(cod_mun = "Nacional",
                        eer = as.numeric(kcal_dia)) %>% select(-kcal_dia) 

col_child_eer = col_child_eer %>% group_by(cod_mun, sex, rango) %>%
  summarise(eer = mean(eer, na.rm = TRUE)) 

# Unir ambos
weight_eer = bind_rows(col_child_eer, child_eer)

# ------------------------------------------------------------
# 4. Preparación Appendix B
# ------------------------------------------------------------
out_eer = file.path(base_dir,
                    "food-security-paper\\output\\eer\\")
# Guardar la base de datos
writexl::write_xlsx(weight_eer, 
                    paste0(out_eer, "\\220326_child_eer.xlsx"))

# Guardar gráfica
city_labels <- c(
  "Nacional" = "National",
  "05001"    = "Medellín",
  "11001"    = "Bogotá",
  "76001"    = "Cali"
)

sex_labels <- c(
  "female" = "Female",
  "male"   = "Male"
)

plot_eer = weight_eer %>%
  mutate(
    ciudad = city_labels[cod_mun],
    sex_en = sex_labels[sex],
    tipo   = ifelse(cod_mun == "Nacional", "Reference (RIEN)", "Weighted EER")
  ) %>%
  ggplot(aes(x = rango, y = eer,
             fill  = ciudad,
             alpha = tipo)) +
  geom_col(position = "dodge") +
  geom_text(
    aes(label = round(eer, 0)),
    position = position_dodge(width = 0.9),
    vjust    = -0.4,
    size     = 2.5
  ) +
  facet_wrap(~sex_en) +
  scale_fill_manual(
    name   = "City",
    values = c("National" = "#000000",
               "Medellín" = "#E63946",
               "Bogotá"   = "#457B9D",
               "Cali"     = "#2A9D8F")
  ) +
  scale_alpha_manual(
    name   = NULL,
    values = c("Reference (RIEN)" = 0.4,
               "Weighted EER"     = 1.0)
  ) +
  labs(
    x = "Age group (years)",
    y = "Energy requirement (kcal/day)"
  ) +
  theme_bw(base_size = 11) +
  theme(
    legend.position       = "bottom",
    strip.text            = element_text(face = "bold"),
    panel.grid.minor      = element_blank(),
    legend.background     = element_rect(fill = "white", color = "black",
                                         linewidth = 0.5),
    legend.box.background = element_rect(color = "black", linewidth = 0.5),
    legend.margin         = margin(5, 10, 5, 10)
  )

ggsave(filename = paste0(out_eer,"220326_plot_eer.jpg"),
       plot = plot_eer, height = 8, width = 12, dpi = 300
       )
