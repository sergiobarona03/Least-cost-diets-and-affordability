
# Cargar librerías
library(tidyverse)
library(janitor)
library(dplyr)
library(Hmisc)

# Definir wd
setwd("C:/Users/sergio.barona/Desktop/Least-cost-diets-and-affordability/Proyecto Interno")

############################
## Cargar bases de datos
############################

# Cargar datos de viviendas
viv.enph = read.csv("ENPH\\micro-data\\Viviendas y hogares\\Viviendas y hogares.csv",
                    sep = ";") %>% clean_names()

# Cargar datos de hogares
car.enph = read.csv("ENPH\\micro-data\\Caracteristicas generales personas\\Caracteristicas generales personas.csv",
                    sep = ";") %>% clean_names()

############################
## Recodificación
############################

# Recodificar numéricas
f_dot = function(x){
  return(as.numeric(gsub(",", ".", x)))
}

# id hogar y viviendas
viv.enph$id_hogar = paste0(viv.enph$directorio,"-",
                           viv.enph$secuencia_p)

viv.enph = viv.enph %>% mutate(
  ciudad = dominio,
  clase = p3,
  n.hogar = p6008,
  fex_hogar = fex_c %>% f_dot(),
  periodo = periodo,
  i_total = it %>% f_dot(),
  icgu = icgu %>% f_dot(),
  icmug = icmug  %>% f_dot(),
  icmdug = icmdug  %>% f_dot(),
  gtug = gtug  %>% f_dot(),
  gcug = gcug %>% f_dot(),
  gcmug = gcmug  %>% f_dot()) %>%
  select(periodo, id_hogar, ciudad, clase, n.hogar,
         fex_hogar, i_total, icgu, icmug, icmdug,
         gtug, gcug, gcmug)

# id individuo
car.enph$id_hogar = paste0(car.enph$directorio,"-",
                           car.enph$secuencia_p)
car.enph$id <- paste0(car.enph$directorio,"-",
                      car.enph$secuencia_p,"-",
                      car.enph$orden)

car.enph = car.enph %>% mutate(
  sexo = p6020,
  edad = p6040,
  jefe = p6050, 
  fex_per = fex_c  %>% f_dot()) %>%
  select(id, id_hogar, sexo, edad, jefe, fex_per)

# Merge
enph.16 = merge(car.enph, 
                viv.enph, by = "id_hogar")

# Filtro para tres ciudades
tres.ciudades <- viv.enph %>%
  filter(ciudad %in% c("CALI", "BOGOTÁ", "MEDELLÍN Y A.M."),
         clase == 1) %>%
  select(ciudad, icmdug, gcmug, gcug,
         fex_hogar) %>%
  na.omit()

#----------------------------------------------------------------------
# Descriptivas: ingresos corrientes
#----------------------------------------------------------------------
cities = levels(as.factor(tres.ciudades$ciudad))
list_cities = vector(mode = "list", length = 3)

for (k in 1:length(cities)) {
 
df.aux = tres.ciudades %>% filter(ciudad == cities[k])

# Cortes ponderados
cuts <- wtd.quantile(
  df.aux$icmdug,
  weights = df.aux$fex_hogar,
  probs = seq(0, 1, 0.1),
  na.rm = TRUE
)

df.aux <- df.aux %>%
  mutate(
    decil_num = cut(icmdug,
                    breaks = cuts,
                    include.lowest = TRUE,
                    labels = FALSE),
    deciles = paste0("Decil ", decil_num)
  )

# Total ciudad
total_weight <- sum(df.aux$fex_hogar)

total.df.aux <- df.aux %>%
  summarise(
    total = total_weight / 1000,  # miles de hogares
    icmdug_total = sum(icmdug * fex_hogar) / 1000000,
    icmdug_mean = weighted.mean(icmdug, w = fex_hogar) / 1000
  ) %>%
  mutate(deciles = cities[k],
         ciudad = cities[k])

# Resultados por decil
deciles.df.aux <- df.aux %>%
  group_by(deciles) %>%
  summarise(
    total = sum(fex_hogar) / 1000,
    icmdug_total = sum(icmdug * fex_hogar) / 1000000,
    icmdug_mean = weighted.mean(icmdug, w = fex_hogar) / 1000,
    .groups = "drop"
  ) %>% mutate(ciudad = cities[k])

# Tabla final
summ.df.aux <- bind_rows(total.df.aux,
                           deciles.df.aux) %>%
  select(ciudad, deciles, total, icmdug_total, icmdug_mean)

list_cities[[k]] = summ.df.aux
}

summ.cities = do.call(rbind, list_cities)

#----------------------------------------------------------------------
# Descriptivas: gasto corriente monetario mensual de la UG
#----------------------------------------------------------------------
gasto_cities = vector(mode = "list", length = 3)

for (k in 1:length(cities)) {
  df.aux = tres.ciudades %>% filter(ciudad == cities[k])
  
  # Cortes ponderados
  cuts <- wtd.quantile(
    df.aux$icmdug,
    weights = df.aux$fex_hogar,
    probs = seq(0, 1, 0.1),
    na.rm = TRUE
  )
  
  df.aux <- df.aux %>%
    mutate(
      decil_num = cut(icmdug,
                      breaks = cuts,
                      include.lowest = TRUE,
                      labels = FALSE),
      deciles = paste0("Decil ", decil_num)
    )
  
  # Total ciudad
  total_weight <- sum(df.aux$fex_hogar)
  
  total.df.aux <- df.aux %>%
    summarise(
      total = total_weight / 1000,      
      gcmug_total = sum(gcmug * fex_hogar) / 1000000,  
      gcug_total = sum(gcug*fex_hogar)/ 1000000,
      gcmug_mean = weighted.mean(gcmug, w = fex_hogar) / 1000 
    ) %>%
    mutate(deciles = cities[k],
           ciudad = cities[k])
  
  # Resultados por decil
  deciles.df.aux <- df.aux %>%
    group_by(deciles) %>%
    summarise(
      total = sum(fex_hogar) / 1000,
      gcmug_total = sum(gcmug * fex_hogar) / 1000000,
      gcug_total = sum(gcug*fex_hogar)/ 1000000,
      gcmug_mean = weighted.mean(gcmug, w = fex_hogar) / 1000,
      .groups = "drop"
    ) %>% mutate(ciudad = cities[k])
  
  # Tabla final
  summ.df.aux <- bind_rows(total.df.aux,
                           deciles.df.aux) %>%
    select(ciudad, deciles, total,
           gcmug_total, gcug_total, gcmug_mean)
  
  gasto_cities[[k]] = summ.df.aux
}

gasto.cities = do.call(rbind, gasto_cities)


