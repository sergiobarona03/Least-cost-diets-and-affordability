

#########################################
## Estimación de modelo CoNA           ##
## A partir de diferentes estimaciones ##
#########################################

# Definir directorio de trabajo
setwd("C:\\Users\\Portatil\\Desktop\\Least-cost-diets-and-affordability\\Proyecto Interno\\")

##-------------------------------------------##
## Cargar datos de precios minoristas (2018) ##
##-------------------------------------------##

# Cargar datos (13 ciudades principales)
m0_dataset = read.csv(file.path("working-papers/working-paper-0125/input",
                                            "m0_forecast_dataset.csv"))

# Reemplazar el precio estimado
for (k in 1:nrow(m0_dataset)) {
  if(m0_dataset$fecha[k] < "2015-01-01" &
     is.na(m0_dataset$precio_hat[k])){
    m0_dataset$precio_hat[k] = m0_dataset$precio_500g[k]
  }
}

m0_dataset = m0_dataset %>% select(fecha, nombre_ciudad, articulo, precio_500g,
                                   precio_hat) %>% distinct() %>% na.omit()

##-------------------------------------------------##
## Cargar base de datos de composición nutricional ##
##-------------------------------------------------##


# Cargar datos de composición nutricional
dane_tcac = readxl::read_excel("composicion-nut\\Copia_DANE_4_DIC_2025act.xlsx") %>%
  janitor::clean_names() %>% rename(articulo = articulo_dane)

# Unir ambas bases de datos
dane_tcac = dane_tcac %>% select(-c("codigo_articulo")) %>% distinct()
m0_dataset = m0_dataset %>% left_join(dane_tcac %>% dplyr::select(-any_of("nombre_ciudad")),
                                              by = "articulo")

# Conversión a 100 gramos en parte comestible
m0_dataset = m0_dataset %>%
  mutate(
    pc = parte_comestible_percent,
    precio_100g = precio_500g*(100/(5*pc)),
    precio_100g_hat = precio_hat*(100/(5*pc))) %>%
  distinct()

saveRDS(m0_dataset,
        "working-papers\\working-paper-0125\\food_tables\\m0_input.RDS")
