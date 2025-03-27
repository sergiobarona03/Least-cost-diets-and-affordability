# Librerías 

library(plotly)
library(dplyr)
library(tidyverse)
library(lubridate)

# Cargar bases
year_2018 <- readRDS("C:\\Users\\danie\\OneDrive\\Escritorio\\Least-cost-diets-and-affordability\\Proyecto Interno\\Flujos de carga (Abastecimiento)\\Bases historicas\\2018.rds")

year_2019 <- readRDS("C:\\Users\\danie\\OneDrive\\Escritorio\\Least-cost-diets-and-affordability\\Proyecto Interno\\Flujos de carga (Abastecimiento)\\Bases historicas\\2019.rds")

year_2020 <- readRDS("C:\\Users\\danie\\OneDrive\\Escritorio\\Least-cost-diets-and-affordability\\Proyecto Interno\\Flujos de carga (Abastecimiento)\\Bases historicas\\2020.rds")

year_2021 <- readRDS("C:\\Users\\danie\\OneDrive\\Escritorio\\Least-cost-diets-and-affordability\\Proyecto Interno\\Flujos de carga (Abastecimiento)\\Bases historicas\\2021.rds")

year_2022 <- readRDS("C:\\Users\\danie\\OneDrive\\Escritorio\\Least-cost-diets-and-affordability\\Proyecto Interno\\Flujos de carga (Abastecimiento)\\Bases historicas\\2022.rds")

year_2023 <- readRDS("C:\\Users\\danie\\OneDrive\\Escritorio\\Least-cost-diets-and-affordability\\Proyecto Interno\\Flujos de carga (Abastecimiento)\\Bases historicas\\2023.rds")

year_2024 <- readRDS("C:\\Users\\danie\\OneDrive\\Escritorio\\Least-cost-diets-and-affordability\\Proyecto Interno\\Flujos de carga (Abastecimiento)\\Bases historicas\\2024.rds")


# Crear una lista con todas las bases
lista_bases <- list(year_2018, year_2019, year_2020, year_2021, year_2022, year_2023, year_2024)

# Normalizar columnas
lista_bases <- lapply(lista_bases, function(df) {
  if (!"Cod_CPC" %in% colnames(df)) {
    df$Cod_CPC <- NA  
  }
  return(df)
})

# Unir todas las bases
dataset <- bind_rows(lista_bases)

# Convertir columnas a formatos correctos
dataset <- dataset %>%
  mutate(
    Fecha = as.Date(Fecha),  
    Cantidad_KG = as.numeric(Cantidad_KG) 
  )

# Convertir la fecha a formato "Año-Mes" y seleccionar las 13 ciudades principales
dataset <- dataset %>%
  mutate(Year_Month = floor_date(Fecha, "month")) %>%
  filter(Ciudad %in% c("Bogotá", "Medellín", "Cali", "Barranquilla", 
                       "Bucaramanga", "Manizales", "Pasto", "Pereira", 
                       "Ibagué", "Cúcuta", "Villavicencio", "Montería", "Cartagena"))

# Agrupar datos por mes, ciudad y grupo de alimento
dataset_1 <- dataset %>%
  group_by(Year_Month, Ciudad, Grupo) %>%
  summarise(Cantidad_Total = sum(Cantidad_KG, na.rm = TRUE) / 1000, .groups = "drop")  

# Crear gráficos por ciudad y por grupo de alimento
lista_graficos1 <- lapply(unique(dataset_1$Ciudad), function(ciudad) {
  
  datos_ciudad <- dataset_1 %>% filter(Ciudad == ciudad)
  
  plot_ly(
    data = datos_ciudad,
    x = ~Year_Month,
    y = ~Cantidad_Total,
    color = ~Grupo,  
    type = "scatter",
    mode = "lines+markers", 
    hoverinfo = "text",
    text = ~paste(
      "<b>Fecha:</b>", format(Year_Month, "%Y-%m"), "<br>",
      "<b>Cantidad (Ton):</b>", round(Cantidad_Total, 2), "<br>",
      "<b>Grupo:</b>", Grupo
    )
  ) %>%
    layout(
      title = paste("Evolución de Alimentos en", ciudad),
      xaxis = list(title = "Fecha", type = "date"),
      yaxis = list(title = "Cantidad (Ton)"),
      legend = list(
        title = list(text = "Grupo de Alimento"),
        orientation = "h",
        x = 0,  
        y = -0.3  
      ),
      hovermode = "x unified"  
    )
})


# Agrupar por mes, ciudad, grupo y alimento
dataset_2 <- dataset %>%
  group_by(Year_Month, Ciudad, Grupo, Alimento) %>%
  summarise(Cantidad_Total = sum(Cantidad_KG, na.rm = TRUE) / 1000, .groups = "drop") 

num_colores <- length(unique(dataset_2$Alimento)) 
paleta_colores <- colorRampPalette(brewer.pal(12, "Set3"))(num_colores) 

# Crear gráficos por ciudad y grupo de alimento
lista_graficos2 <- lapply(unique(dataset_2$Ciudad), function(ciudad) {
  lapply(unique(dataset_2$Grupo), function(grupo) {
    
    datos_filtro <- dataset_2 %>%
      filter(Ciudad == ciudad, Grupo == grupo)
    
    plot_ly(
      data = datos_filtro,
      x = ~Year_Month,
      y = ~Cantidad_Total,
      color = ~Alimento,
      colors = paleta_colores,  
      type = "scatter",
      mode = "lines+markers", 
      hoverinfo = "text",
      text = ~paste(
        "<b>Fecha:</b>", format(Year_Month, "%Y-%m"), "<br>",
        "<b>Cantidad (Ton):</b>", round(Cantidad_Total, 2), "<br>",
        "<b>Alimento:</b>", Alimento
      )
    ) %>%
      layout(
        title = paste("Evolución de", grupo, "en", ciudad),
        xaxis = list(title = "Fecha", type = "date"),
        yaxis = list(title = "Cantidad (Ton)"),
        legend = list(
          title = list(text = "Alimento"),
          orientation = "h",  
          x = 0,  
          y = -0.3  
        ),
        hovermode = "x unified"  
      )
  })
})
