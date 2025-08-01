
##################################################################
## Prueba: análisis sobre margen de comercialización (subclase) ##
## Fecha: 31 de julio de 2025                                   ##
##################################################################

# Cargar librerías
library(lubridate)
library(tidyverse)

# Definir directorio de trabajo
setwd("C:\\Users\\Portatil\\Desktop\\Least-cost-diets-and-affordability\\Proyecto Interno\\")

##-------------------------------------------##
## Cargar datos de precios minoristas (2018) ##
##-------------------------------------------##

# Cargar datos (13 ciudades principales)
retail_99_18 = readxl::read_excel("Precios DANE\\OUTPUT_DANE\\precios_IPC_1999_2018.xlsx")

# Eliminar alimentos que son ultraprocesados o preparaciones
alimentos_excluir <- c(
  # Ultraprocesados
  "AREPAS  PRECOCIDAS", "AREPAS RELLENAS CON ALGO", "BOCADILLOS",
  "CEREAL ALIMENTO PARA BEBÉ", "CEREAL PARA DESAYUNO", "CHOCOLATE INSTANTANEO",
  "CHORIZO", "GALLETAS DE SAL", "GALLETAS DULCES", "GALLETAS INTEGRALES",
  "GASEOSAS", "GELATINA O FLAN", "HARINA PARA TORTAS", "HELADOS DE CREMA",
  "JAMÓN", "JUGOS INSTANTANEOS O EN POLVO", "JUGOS PROCESADOS",
  "MALTAS", "MARGARINA", "MAYONESA", "MERMELADA", "MORTADELA",
  "PIZZA", "SALCHICHAS","SALCHICHÓN", "SALSA DE TOMATE", 
  "SOPAS", "YOGOURT", "CREMA DE LECHE", "PAPAS FRITAS",
  
  # Condimentos y hierbas
  "CILANTRO", "COLOR", "COMINOS", "LAUREL", "MOSTAZA",
  "PIMIENTA", "TOMILLO", "REVUELTO VERDE",
  
  # Preparaciones y alimentos compuestos
  "ALMUERZO CORRIENTE O EJECUTIVO", "ALMUERZO ESPECIAL O A LA CARTA",
  "CHOCOLATE EN PASTA", "CAFÉ INSTANTANEO", "COMBOS", 
  "CREMAS", "ENSALADA  DE FRUTAS", "QUESO CREMA", "TINTO", 
  "HAMBURGUESA", "KUMIS", "JUGOS NATURALES", "SUERO"
)

# Excluir alimentos
retail_99_18 <- retail_99_18 %>% filter(!articulo %in% alimentos_excluir)

# Armonizar los nombres de las ciudades (código DIVIPOLA)
retail_99_18 <- retail_99_18 %>%
  mutate(cod_mun = case_when(
    nombre_ciudad == "BARRANQUILLA"   ~ "08001",
    nombre_ciudad == "BOGOTÁ D.C."    ~ "11001",
    nombre_ciudad == "BUCARAMANGA"    ~ "68001",
    nombre_ciudad == "CALI"           ~ "76001",
    nombre_ciudad == "CARTAGENA"      ~ "13001",
    nombre_ciudad == "CÚCUTA"         ~ "54001",
    nombre_ciudad == "MANIZALES"      ~ "17001",
    nombre_ciudad == "MEDELLÍN"       ~ "05001",
    nombre_ciudad == "MONTERÍA"       ~ "23001",
    nombre_ciudad == "NEIVA"          ~ "41001",
    nombre_ciudad == "PASTO"          ~ "52001",
    nombre_ciudad == "PEREIRA"        ~ "66001",
    nombre_ciudad == "VILLAVICENCIO"  ~ "50001",
    TRUE ~ NA_character_
  ))

# Recodificar año
retail_99_18 <- retail_99_18 %>%
  mutate(ano = as.integer(ano))

# Recodificar mes
retail_99_18 <- retail_99_18 %>%
  mutate(
    mes_num = recode(mes,
                     "enero" = 1,
                     "febrero" = 2,
                     "marzo" = 3,
                     "abril" = 4,
                     "mayo" = 5,
                     "junio" = 6,
                     "julio" = 7,
                     "agosto" = 8,
                     "septiembre" = 9,
                     "octubre" = 10,
                     "noviembre" = 11,
                     "diciembre" = 12
    )
  )

# Conversión de las unidades (precio_500g)
retail_99_18 <- retail_99_18 %>%
  mutate(
    unidad = str_replace_all(tolower(unidad), "\\s", ""),
    precio = as.numeric(precio),
    precio_500g = case_when(
      unidad == "125grs." ~ precio * 4,
      unidad == "250grs." ~ precio * 2,
      unidad %in% c("400grs.", "400grs") ~ precio * (500 / 400),
      unidad %in% c("500grs.", "500grs") ~ precio,
      unidad %in% c("600grs.", "600grs") ~ precio * (500 / 600),
      unidad %in% c("1und.", "1und") & articulo == "HUEVOS" ~ precio * 8.33,
      unidad %in% c("1000c.c.", "1000cc") & str_detect(articulo, regex("aceite", ignore_case = TRUE)) ~ precio * (500 / 920),
      unidad %in% c("1000c.c.", "1000cc") & str_detect(articulo, regex("leche", ignore_case = TRUE)) ~ precio * 0.5,
      unidad %in% c("250c.c.", "250cc") & str_detect(articulo, regex("mantequilla", ignore_case = TRUE)) ~ precio * 2,
      TRUE ~ NA_real_
    )
  )


##------------------------------------##
## Cargar datos de precios mayoristas ##
##------------------------------------##

# Lista output
whole_list = vector(mode = "list", length = length(2013:2018))

# Cargar series de sipsa
for (k in 2013:2018) {
  whole_list[[k]] = readRDS(paste0("Precios al por mayor\\Bases historicas\\", k,".rds"))
}

# whole_18 significa whole hasta 2018
whole_18 <- do.call(rbind, whole_list)

# Identificar los mercados de las principales ciudades
whole_18 <- whole_18 %>%
  mutate(nombre_ciudad = case_when(
    str_detect(Mercado, regex("Barranquilla", ignore_case = TRUE)) ~ "BARRANQUILLA",
    str_detect(Mercado, regex("Bogotá", ignore_case = TRUE)) ~ "BOGOTÁ D.C.",
    str_detect(Mercado, regex("Bucaramanga", ignore_case = TRUE)) ~ "BUCARAMANGA",
    str_detect(Mercado, regex("Cali", ignore_case = TRUE)) ~ "CALI",
    str_detect(Mercado, regex("Cartagena", ignore_case = TRUE)) ~ "CARTAGENA",
    str_detect(Mercado, regex("Cúcuta", ignore_case = TRUE)) ~ "CÚCUTA",
    str_detect(Mercado, regex("Manizales", ignore_case = TRUE)) ~ "MANIZALES",
    str_detect(Mercado, regex("Medellín", ignore_case = TRUE)) ~ "MEDELLÍN",
    str_detect(Mercado, regex("Montería", ignore_case = TRUE)) ~ "MONTERÍA",
    str_detect(Mercado, regex("Neiva", ignore_case = TRUE)) ~ "NEIVA",
    str_detect(Mercado, regex("Pasto", ignore_case = TRUE)) ~ "PASTO",
    str_detect(Mercado, regex("Pereira", ignore_case = TRUE)) ~ "PEREIRA",
    str_detect(Mercado, regex("Villavicencio", ignore_case = TRUE)) ~ "VILLAVICENCIO",
    TRUE ~ NA_character_
  ))

# Filtrar para las 13 ciudades principales
whole_18 <- whole_18 %>% filter(!is.na(nombre_ciudad))

# Armonizar los nombres de las ciudades (código DIVIPOLA)
whole_18 <- whole_18 %>%
  mutate(cod_mun = case_when(
    nombre_ciudad == "BARRANQUILLA"   ~ "08001",
    nombre_ciudad == "BOGOTÁ D.C."    ~ "11001",
    nombre_ciudad == "BUCARAMANGA"    ~ "68001",
    nombre_ciudad == "CALI"           ~ "76001",
    nombre_ciudad == "CARTAGENA"      ~ "13001",
    nombre_ciudad == "CÚCUTA"         ~ "54001",
    nombre_ciudad == "MANIZALES"      ~ "17001",
    nombre_ciudad == "MEDELLÍN"       ~ "05001",
    nombre_ciudad == "MONTERÍA"       ~ "23001",
    nombre_ciudad == "NEIVA"          ~ "41001",
    nombre_ciudad == "PASTO"          ~ "52001",
    nombre_ciudad == "PEREIRA"        ~ "66001",
    nombre_ciudad == "VILLAVICENCIO"  ~ "50001",
    TRUE ~ NA_character_
  ))

# Antes de calcular el precio medio, se armonizan las unidad (P500g)
# El precio 
whole_18 <- whole_18 %>%
  mutate(
    precio_500g = case_when(
      str_detect(Alimento, regex("aceite", ignore_case = TRUE)) ~ Precio_kg * (500 / 920),
      
      Alimento %in% c("Huevo blanco A", "Huevo rojo A") ~ Precio_kg * (500 / 50),
      Alimento %in% c("Huevo blanco AA", "Huevo rojo AA") ~ Precio_kg * (500 / 60),
      Alimento %in% c("Huevo blanco extra", "Huevo rojo extra") ~ Precio_kg * (500 / 67),
      
      TRUE ~ Precio_kg / 2  # Por defecto, mitad del precio por kg
    )
  ) %>%
  filter(!Alimento %in% c("Jugo de frutas", "Bocadillo veleño", "Vinagre",
                          "Huevo blanco B", "Huevo rojo B"))


# Crear el precio promedio para cada alimento según: año, mes, ciudad, alimento
whole_18_mean <- whole_18 %>% group_by(Year, Month, cod_mun, Alimento) %>%
  summarise(precio_medio = mean(precio_500g, na.rm = TRUE))

##------------------------------------##
## Cargar mapeo: DANE (IPC) - SIPSA   ##
##------------------------------------##

# Cargar el mapeo de ambas bases:
ipc_sipsa = readxl::read_excel("Time-series\\mapeo_retail_sipsa.xlsx")

# Añadir al retail las denominaciones de sipsa
retail_99_18 = retail_99_18 %>% left_join(ipc_sipsa, by = c("articulo" = "retail"))

# Añadir los precios mayoristas
retail_whole_18 <- retail_99_18 %>%
  left_join(
    whole_18_mean[c("Year", "Month", "cod_mun", "Alimento", "precio_medio")],
    by = c(
      "ano" = "Year",
      "mes_num" = "Month",
      "cod_mun" = "cod_mun",
      "sipsa" = "Alimento"
    )
  ) %>% filter(ano >= 2013)

# Filtro para Cali, Medellín y Bogotá
whole_tres = retail_whole_18 %>% filter(nombre_ciudad %in% c("MEDELLÍN",
                                                             "CALI",
                                                             "BOGOTÁ D.C."))

######################################
###--------------------------------###
### Datos sobre precios minoristas ###
###--------------------------------###
######################################

# Filtrar para la ciudad principal
ciudad.input = c("CALI")
ipc_sipsa = whole_tres %>% filter(nombre_ciudad %in% ciudad.input)

# Recodificar fecha
ipc_sipsa$fecha = as.Date(paste(ipc_sipsa$ano, 
                                ipc_sipsa$mes_num, 
                                 "01", sep = "-"))

# Crear la variable "código subclase"
ipc_sipsa$cod_subclase = paste0("0",substr(ipc_sipsa$codigo_articulo, 1, 6), "0")

# Calcular los cuartiles por subclase
margen_subclase <- ipc_sipsa %>% 
  group_by(articulo) %>%
  mutate(
    factor = precio_500g / precio_medio,
    margen = (factor - 1) * 100
  )

# Calcular los cuartiles (Q1, Q2, Q3) por subclase
cuartiles_subclase <- margen_subclase %>%
  group_by(cod_subclase) %>%
  summarize(
    m_q1 = quantile(margen, 0.25, na.rm = TRUE),
    m_q2 = quantile(margen, 0.50, na.rm = TRUE),
    m_q3 = quantile(margen, 0.75, na.rm = TRUE),
    .groups = "drop"
  ) 

# Asociar los margenes a cada producto según subclase
ipc_sipsa = merge(ipc_sipsa,cuartiles_subclase, by = "cod_subclase")

# Prueba con una ciudad y una alimento
city_i = levels(as.factor(ipc_sipsa$nombre_ciudad))
food_j = levels(as.factor(ipc_sipsa$sipsa))

# Base de datos fallidos
list.fail = vector(mode = "list")

# Métricas 
resultados_metricas <- data.frame(
  ciudad = character(),
  articulo = character(),
  rmse_q1 = numeric(),
  rmse_q2 = numeric(),
  rmse_q3 = numeric(),
  mape_q1 = numeric(),
  mape_q2 = numeric(),
  mape_q3 = numeric(),
  stringsAsFactors = FALSE
)
# Bucle
for (i in 1:length(city_i)) {
  for (j in 1:length(food_j)) {
 
    print(paste0(city_i[i], " - ", food_j[j]))
    
    # Base de datos aux.
    df.aux = ipc_sipsa %>% filter(nombre_ciudad == city_i[i] &
                                    sipsa == food_j[j])
    
    # Filtrar fechas_75
    train.df = df.aux %>% dplyr::filter(fecha <= "2015-01-01")
    test.df = df.aux %>% filter(fecha >= "2015-01-01")
    
    if (all(is.na(df.aux$precio_medio))) {
      df.aux2 = ipc_sipsa %>% filter(sipsa == food_j[j])
      list.fail[[length(list.fail) + 1]] <- data.frame(
        ciudad = city_i[i],
        articulo = unique(df.aux2$sipsa),
        cod_subclase = unique(df.aux2$cod_subclase),
        articulo = food_j[j]
      )
    } else {
      
        # Formato long
        library(tidyverse)
        meses_esp <- c("Ene", "Feb", "Mar", "Abr", "May", "Jun", 
                       "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")
        
        # Merge
        ipc_df = test.df %>%
          select(fecha, ano, mes_num, ciudad, nombre_ciudad, 
                 cod_subclase,
                 codigo_articulo, articulo, precio_500g, precio_medio,
                 m_q1, m_q2, m_q3) %>%
          arrange(fecha) %>%
          mutate(precio_hat_q1 = NA,
                 precio_hat_q2 = NA,
                 precio_hat_q3 = NA)
        
        # Llenar precios hacia adelante usando la variación del IPC
        ipc_df$precio_hat_q1 = ipc_df$precio_medio*(1+(ipc_df$m_q1/100))
        ipc_df$precio_hat_q2 = ipc_df$precio_medio*(1+(ipc_df$m_q2/100))
        ipc_df$precio_hat_q3 = ipc_df$precio_medio*(1+(ipc_df$m_q3/100))
        
        
        # Unir con la base de datos
        test.df2 = test.df %>% left_join(ipc_df[c("fecha",
                                                  "precio_hat_q1",
                                                  "precio_hat_q2",
                                                  "precio_hat_q3")],
                                         by = "fecha") %>%
          select(fecha, nombre_ciudad, articulo, precio_500g,
                 precio_hat_q1, precio_hat_q2, precio_hat_q3, sipsa, precio_medio)
        
        # Unir con la base de datos de entrenamiento
        test.df2 = bind_rows(train.df, test.df2)
        
        # Filtrar datos de validación (predicciones no faltantes)
        valid_df <- test.df2 %>% filter(!is.na(precio_hat_q1),
                                        !is.na(precio_hat_q2),
                                        !is.na(precio_hat_q3))
        
        # Cálculo de métricas para cada estimación
        rmse_q1 <- sqrt(mean((valid_df$precio_500g - valid_df$precio_hat_q1)^2, na.rm = TRUE))
        rmse_q2 <- sqrt(mean((valid_df$precio_500g - valid_df$precio_hat_q2)^2, na.rm = TRUE))
        rmse_q3 <- sqrt(mean((valid_df$precio_500g - valid_df$precio_hat_q3)^2, na.rm = TRUE))
        
        mape_q1 <- mean(abs(valid_df$precio_500g - valid_df$precio_hat_q1) / valid_df$precio_500g, na.rm = TRUE) * 100
        mape_q2 <- mean(abs(valid_df$precio_500g - valid_df$precio_hat_q2) / valid_df$precio_500g, na.rm = TRUE) * 100
        mape_q3 <- mean(abs(valid_df$precio_500g - valid_df$precio_hat_q3) / valid_df$precio_500g, na.rm = TRUE) * 100
        
        # Guardar resultados
        resultados_metricas <- resultados_metricas %>%
          add_row(
            ciudad = unique(valid_df$nombre_ciudad),
            articulo = unique(valid_df$articulo),
            rmse_q1 = rmse_q1,
            rmse_q2 = rmse_q2,
            rmse_q3 = rmse_q3,
            mape_q1 = mape_q1,
            mape_q2 = mape_q2,
            mape_q3 = mape_q3
          )
        
        
        # Validar la estimación:
        plot_aux = test.df2 %>%
          ggplot(aes(x = fecha)) +
          geom_line(aes(y = precio_500g, color = "Precio minorista"), size = 1) +
          geom_line(aes(y = precio_medio, color = "Precio mayorista"), size = 1) +
          geom_line(aes(y = precio_hat_q2, color = "Precio estimado"), 
                    linetype = 1, size = 1) +
          geom_ribbon(aes(ymin = precio_hat_q1, ymax = precio_hat_q3),
                      fill = "red", alpha = 0.3, linetype = "dashed",
                      col = "red") +
          labs(
            title = "Comparación entre precio real y estimado",
            subtitle = paste0(
              unique(valid_df$nombre_ciudad), " - ", unique(valid_df$sipsa),
              "\nRMSE = ", round(rmse_q2, 2),
              " | MAPE Q1 = ", round(mape_q1, 2), "%",
              " | MAPE Q2 = ", round(mape_q2, 2), "%",
              " | MAPE Q3 = ", round(mape_q3, 2), "%",
              "\n(Rango: RMSE Q1=", round(rmse_q1, 2),
              " - Q3=", round(rmse_q3, 2), ")"
            ),
            x = "Fecha",
            y = "Precio (500g)",
            color = " "
          ) +
          scale_color_manual(values = c("Precio minorista" = "black", "Precio estimado" = "red",
                                        "Precio mayorista" = "darkgreen")) +
          theme_bw() +
          theme(
            legend.position = "bottom",
            plot.title = element_text(face = "bold")
          )
        
        
        print(plot_aux)
        
        ggsave(plot = plot_aux, 
               filename = paste0("margen-dist/output-ciudades/CALI/price_hat/",
                                 unique(test.df2$sipsa),".png"),
               dpi = 300, height = 12, width = 12)
        
      }
      
    }
    
  }


readr::write_csv(resultados_metricas, "300725_resultados_metricas_ipc.csv")










