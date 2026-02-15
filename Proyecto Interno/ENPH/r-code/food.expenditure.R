
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
  select(ciudad, icmdug, gcmug,
         fex_hogar) %>%
  na.omit()

#----------------------------------------------------------------------
# Gasto en alimentación: Capítulo C
#----------------------------------------------------------------------
capitulo_c = haven::read_dta("ENPH\\micro-data\\Gastos diarios Urbano - Capitulo C\\Gastos diarios Urbano - Capitulo C.dta") %>%
  janitor::clean_names()

capitulo_c$id_hogar = paste0(capitulo_c$directorio,"-",
                             capitulo_c$secuencia_p)
capitulo_c$id <- paste0(capitulo_c$directorio,"-",
                        capitulo_c$secuencia_p,"-",
                        capitulo_c$orden)

# Mensualizar las compras 
library(dplyr)

capitulo_c <- capitulo_c %>%
  mutate(
    month_food = case_when(
      is.na(nc2_cc_p2)        ~ NA_real_,
      nc2_cc_p2 %in% c(1,2,3) ~ nc2_cc_p3_s1 * 2.14,
      nc2_cc_p2 == 4          ~ nc2_cc_p3_s1 * 2,
      nc2_cc_p2 == 5          ~ nc2_cc_p3_s1,
      nc2_cc_p2 == 6          ~ nc2_cc_p3_s1 / 2,
      nc2_cc_p2 == 7          ~ nc2_cc_p3_s1 / 3,
      nc2_cc_p2 == 9          ~ nc2_cc_p3_s1,
      nc2_cc_p2 == 11         ~ 0,
      TRUE                    ~ NA_real_
    )
  )

hogar.cap.c = capitulo_c %>%
  group_by(id_hogar) %>%
  dplyr::summarise(
    exp_food_month = sum(month_food, na.rm = T)
  )

ciudades.cap.c = merge(viv.enph, hogar.cap.c, by = "id_hogar")

summ.cities.c = ciudades.cap.c %>%
  group_by(ciudad) %>%
  dplyr::summarise(
    exp_food_month = sum(exp_food_month*fex_hogar, na.rm = T)/1000000
  )

#----------------------------------------------------------------------
# Gasto en alimentación: Cuadernillo 2
#----------------------------------------------------------------------
mercado = haven::read_dta("ENPH\\micro-data\\Gastos diarios Urbanos - Mercados\\Gastos diarios Urbanos - Mercados.dta") %>%
  janitor::clean_names()

mercado$id_hogar = paste0(mercado$directorio,"-",
                          mercado$secuencia_p)

mercado$valor_mercado = mercado$nc2_cc_p4s1*4.28

mercado$valor_mercado[mercado$valor_mercado %in%
                        c(98, 99 )] = NA

mercado.hogares = mercado %>% group_by(id_hogar) %>%
  dplyr::summarize(total.mercado = sum(valor_mercado, na.rm = T))

ciudades.mercado.hogares= merge(viv.enph, mercado.hogares, by = "id_hogar")

summ.cities.mercado = ciudades.mercado.hogares %>%
  group_by(ciudad) %>%
  dplyr::summarise(
    total.mercado = sum(total.mercado*fex_hogar, na.rm = T)/1000000
  )

View(summ.cities.mercado)


