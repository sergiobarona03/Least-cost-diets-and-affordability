########################################################
## Procesamiento de los requerimientos energéticos
## en niños y adolescentes
## Nota: el procedimiento consiste en calcular un promedio
## ponderado considerando la estructura poblacional del CNPV-2018
########################################################

# Librerías
library(tidyverse)
library(readxl)
library(writexl)
library(janitor)

# Directorio base
dirs <- c(
  "C:/Users/Portatil/Desktop/Least-cost-diets-and-affordability/Proyecto Interno",
  "C:/Users/danie/OneDrive/Escritorio/Least-cost-diets-and-affordability/Proyecto Interno"
)

base_dir <- dirs[dir.exists(dirs)][1]

if (is.na(base_dir)) {
  stop("Ninguno de los directorios existe")
}

# ------------------------------------------------------------
# 1. Cargar RIEN para niños y niñas
# ------------------------------------------------------------

male_rien = readxl::read_excel(file.path(base_dir,
                                         "interno\\input\\requirements\\eer-ninos-adultos.xlsx"),
                               sheet = 1) %>%
  mutate(sex = "male")

female_rien = readxl::read_excel(file.path(base_dir,
                                           "interno\\input\\requirements\\eer-ninos-adultos.xlsx"),
                                 sheet = 2) %>%
  mutate(sex = "female")

rien_energy = rbind(male_rien, female_rien) %>%
  mutate(rango = paste0(Lim_inf, " - ", Lim_sup))

# ------------------------------------------------------------
# 2. Cargar la estructura poblacional
# ------------------------------------------------------------

cnpv_dir  <- "\\interno\\input\\requirements\\cnpv-2018-municipios.xlsx"

cnpv_mun <- readxl::read_excel(file.path(base_dir, cnpv_dir)) %>%
  filter(edad != "Total grupo edad")

cnpv_mun <- cnpv_mun %>%
  fill(dpto, municipio, grupo, .direction = "down")

cnpv_mun$cod_mun = substr(cnpv_mun$municipio, 1, 5)

# Definir los municipios de interés: 13 ciudades
cnpv_mun = cnpv_mun %>% filter(
  cod_mun %in% c(
    "08001", # Barranquilla
    "11001", # Bogotá
    "68001", # Bucaramanga
    "76001", # Cali
    "13001", # Cartagena
    "54001", # Cúcuta
    "73001", # Ibagué
    "17001", # Manizales
    "05001", # Medellín
    "23001", # Montería
    "52001", # Pasto
    "66001", # Pereira
    "50001"  # Villavicencio
  )
)

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

cnpv_mun_eer = merge(
  cnpv_mun %>%
    filter(as.numeric(Lim_inf) <= 17,
           as.numeric(Lim_inf) > 1),
  rien_energy %>%
    filter(Moderada == 1),
  by = c("Lim_inf", "sex"),
  all.x = TRUE
)

cnpv_mun_eer = cnpv_mun_eer %>%
  select(cod_mun, Lim_inf, sex, KCAL_DIA, n) %>%
  clean_names()

cnpv_mun_eer <- cnpv_mun_eer %>%
  mutate(
    rango = cut(
      as.numeric(lim_inf),
      breaks = c(2, 6, 10, 14, 18),
      right = FALSE,
      labels = c("[2, 6)", "[6, 10)", "[10, 14)", "[14, 18)")
    )
  )

# Estimar EER por promedio ponderado diferenciado por ciudad
child_eer = cnpv_mun_eer %>%
  group_by(cod_mun, sex, rango) %>%
  summarize(
    eer = weighted.mean(x = as.numeric(kcal_dia), w = n),
    .groups = "drop"
  )

# Estimar EER nacional
col_child_eer = cnpv_mun_eer %>%
  select(rango, sex, kcal_dia) %>%
  distinct() %>%
  mutate(
    cod_mun = "Nacional",
    eer = as.numeric(kcal_dia)
  ) %>%
  select(-kcal_dia)

col_child_eer = col_child_eer %>%
  group_by(cod_mun, sex, rango) %>%
  summarise(
    eer = mean(eer, na.rm = TRUE),
    .groups = "drop"
  )

# Unir ambos
weight_eer = bind_rows(col_child_eer, child_eer)

# ------------------------------------------------------------
# 4. Preparación Appendix B
# ------------------------------------------------------------

out_eer = file.path(base_dir, "interno\\output\\eer\\")

writexl::write_xlsx(
  weight_eer,
  paste0(out_eer, "\\250526_child_eer.xlsx")
)

# ------------------------------------------------------------
# 5. Etiquetas de ciudad, sexo y región
# ------------------------------------------------------------
city_labels <- c(
  "Nacional" = "National",
  "08001"    = "Barranquilla",
  "11001"    = "Bogotá",
  "68001"    = "Bucaramanga",
  "76001"    = "Cali",
  "13001"    = "Cartagena",
  "54001"    = "Cúcuta",
  "73001"    = "Ibagué",
  "17001"    = "Manizales",
  "05001"    = "Medellín",
  "23001"    = "Montería",
  "52001"    = "Pasto",
  "66001"    = "Pereira",
  "50001"    = "Villavicencio"
)

sex_labels <- c(
  "female" = "Female",
  "male"   = "Male"
)

city_colors <- c(
  "National"      = "#000000",
  "Barranquilla"  = "#1B9E77",
  "Bogotá"        = "#457B9D",
  "Bucaramanga"   = "#7570B3",
  "Cali"          = "#2A9D8F",
  "Cartagena"     = "#66A61E",
  "Cúcuta"        = "#E6AB02",
  "Ibagué"        = "#A6761D",
  "Manizales"     = "#666666",
  "Medellín"      = "#E63946",
  "Montería"      = "#D95F02",
  "Pasto"         = "#E7298A",
  "Pereira"       = "#F4A261",
  "Villavicencio" = "#264653"
)

city_groups <- list(
  "1" = c("Barranquilla", "Bogotá",   "Bucaramanga",                "National"),
  "2" = c("Cali",         "Cartagena","Cúcuta",                     "National"),
  "3" = c("Ibagué",       "Manizales","Medellín",                   "National"),
  "4" = c("Montería",     "Pasto",    "Pereira",    "Villavicencio","National")
)

# ------------------------------------------------------------
# 6. Guardar gráficas
# ------------------------------------------------------------
data_base <- weight_eer %>%
  mutate(
    ciudad = city_labels[cod_mun],
    sex_en = sex_labels[sex],
    tipo   = ifelse(cod_mun == "Nacional", "Reference (RIEN)", "Weighted EER")
  )

for (i in seq_along(city_groups)) {
  
  ciudades_sel <- city_groups[[i]]
  
  p <- data_base %>%
    filter(ciudad %in% ciudades_sel) %>%
    mutate(ciudad = factor(ciudad, levels = ciudades_sel)) %>%
    ggplot(aes(x = rango, y = eer, fill = ciudad, alpha = tipo)) +
    geom_col(position = position_dodge(width = 0.9), width = 0.8) +
    geom_text(
      aes(label = round(eer, 0)),
      position = position_dodge(width = 0.9),
      vjust    = -0.4,
      size     = 2.5
    ) +
    facet_wrap(~sex_en) +
    scale_fill_manual(
      name   = "City",
      values = city_colors[ciudades_sel]
    ) +
    scale_alpha_manual(
      name   = NULL,
      values = c("Reference (RIEN)" = 0.4, "Weighted EER" = 1.0)
    ) +
    scale_y_continuous(
      expand = expansion(mult = c(0, 0.12))
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
  
  ggsave(
    filename = paste0(out_eer, "250526_plot_eer_g", i, ".jpg"),
    plot     = p,
    height   = 8,
    width    = 12,
    dpi      = 300
  )
  
  message("Guardada: grupo ", i, " — ", paste(ciudades_sel, collapse = ", "))
}