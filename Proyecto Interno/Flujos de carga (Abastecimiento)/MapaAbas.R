# Cargar librerías
library(plotly)
library(dplyr)
library(tidyverse)
library(lubridate)
library(shiny)

# Cargar base de datos
year_2018 <- readRDS("C:\\Users\\danie\\OneDrive\\Escritorio\\Least-cost-diets-and-affordability\\Proyecto Interno\\Avances (6 meses)\\Avance 2\\Bases historicas\\2018.rds")

year_2019 <- readRDS("C:\\Users\\danie\\OneDrive\\Escritorio\\Least-cost-diets-and-affordability\\Proyecto Interno\\Avances (6 meses)\\Avance 2\\Bases historicas\\2019.rds")

year_2020 <- readRDS("C:\\Users\\danie\\OneDrive\\Escritorio\\Least-cost-diets-and-affordability\\Proyecto Interno\\Avances (6 meses)\\Avance 2\\Bases historicas\\2020.rds")

year_2021 <- readRDS("C:\\Users\\danie\\OneDrive\\Escritorio\\Least-cost-diets-and-affordability\\Proyecto Interno\\Avances (6 meses)\\Avance 2\\Bases historicas\\2021.rds")

year_2022 <- readRDS("C:\\Users\\danie\\OneDrive\\Escritorio\\Least-cost-diets-and-affordability\\Proyecto Interno\\Avances (6 meses)\\Avance 2\\Bases historicas\\2022.rds")

year_2023 <- readRDS("C:\\Users\\danie\\OneDrive\\Escritorio\\Least-cost-diets-and-affordability\\Proyecto Interno\\Avances (6 meses)\\Avance 2\\Bases historicas\\2023.rds")

year_2024 <- readRDS("C:\\Users\\danie\\OneDrive\\Escritorio\\Least-cost-diets-and-affordability\\Proyecto Interno\\Avances (6 meses)\\Avance 2\\Bases historicas\\2024.rds")


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
datos<- bind_rows(lista_bases)

# Convertir columnas a formatos correctos
datos <- datos %>%
  mutate(
    Fecha = as.Date(Fecha),  
    Cantidad_KG = as.numeric(Cantidad_KG) 
  )
# Filtrar los datos para las 4 ciudades y procesarlos
datos_1 <- datos %>%
  mutate(Year_Month = floor_date(Fecha, "month")) %>%
  filter(Ciudad %in% c("Bogotá", "Medellín", "Cali", "Barranquilla")) %>%
  group_by(Ciudad, Grupo, Alimento, Year_Month) %>%  
  summarise(Cantidad_Total = sum(Cantidad_KG, na.rm = TRUE), .groups = "drop") %>%
  group_by(Alimento, Ciudad) %>%  
  mutate(Quintil = ntile(Cantidad_Total, 5)) %>%
  ungroup()

# Interfaz de usuario 1
ui1 <- fluidPage(
  titlePanel("Aplicación 1 - Evolución del Abastecimiento por Alimentos (Quintil sobre el comportamiento del alimento)"),
  sidebarLayout(
    sidebarPanel(
      selectInput("ciudad", "Selecciona una ciudad:", 
                  choices = unique(datos_1$Ciudad))
    ),
    mainPanel(
      plotlyOutput("grafico_consumo", height = "900px", width = "1100px")
    )
  )
)

# Servidor 1
server1 <- function(input, output) {
  output$grafico_consumo <- renderPlotly({
    datos_ciudad1 <- datos_1 %>% filter(Ciudad == input$ciudad)
    
    p <- ggplot(datos_ciudad1, aes(x = Year_Month, y = Alimento, fill = Quintil)) +
      geom_tile() +
      scale_fill_gradient(low = "#D0E1F9", high = "#08306B") +
      labs(title = paste("Evolución del Consumo de Alimentos en", input$ciudad),
           x = "Mes", y = "Alimento", fill = "Quintil") +
      facet_grid(Grupo ~ ., scales = "free_y", space = "free_y", switch = "y") +  
      theme_minimal(base_size = 12) +
      theme(
        strip.text.y = element_text(size = 8, face = "bold"),  
        axis.text.x = element_text(angle = 45, hjust = 1, size = 10),  
        axis.text.y = element_text(size = 9),  
        strip.placement = "outside",  
        legend.position = "bottom", 
        legend.key.size = unit(0.6, "cm"),  
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        panel.spacing = unit(1.5, "lines")  
      )
    
    ggplotly(p, height = 900, width = 1100)
  })
}

shinyApp(ui1, server1)


# Filtrar los datos para las 4 ciudades y procesarlos
datos_2 <- datos %>%
  mutate(Year_Month = floor_date(Fecha, "month")) %>%
  filter(Ciudad %in% c("Bogotá", "Medellín", "Cali", "Barranquilla")) %>%
  group_by(Ciudad, Grupo, Alimento, Year_Month) %>%  
  summarise(Cantidad_Total = sum(Cantidad_KG, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    Quintil = ntile(Cantidad_Total, 5)  
  ) %>%
  ungroup()

# Interfaz de usuario 2
ui2 <- fluidPage(
  titlePanel(" Aplicación 2 - Distribución General de Alimentos (Quintil generalizado)"),
  sidebarLayout(
    sidebarPanel(
      selectInput("ciudad", "Selecciona una ciudad:", 
                  choices = unique(datos_2$Ciudad))
    ),
    mainPanel(
      plotlyOutput("grafico_consumo", height = "900px", width = "1100px")
    )
  )
)

# Servidor 2
server2 <- function(input, output) {
  output$grafico_consumo <- renderPlotly({
    datos_ciudad2 <- datos_2 %>% filter(Ciudad == input$ciudad)
    
    p <- ggplot(datos_ciudad2, aes(x = Year_Month, y = Alimento, fill = Quintil)) +
      geom_tile() +
      scale_fill_gradient(low = "#D0E1F9", high = "#08306B") +
      labs(title = paste("Evolución del Consumo de Alimentos en", input$ciudad),
           x = "Mes", y = "Alimento", fill = "Quintil") +
      facet_grid(Grupo ~ ., scales = "free_y", space = "free_y", switch = "y") +  
      theme_minimal(base_size = 12) +
      theme(
        strip.text.y = element_text(size = 8, face = "bold"),  
        axis.text.x = element_text(angle = 45, hjust = 1, size = 10),  
        axis.text.y = element_text(size = 9),  
        strip.placement = "outside",  
        legend.position = "bottom", 
        legend.key.size = unit(0.6, "cm"),  
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        panel.spacing = unit(1.5, "lines")  
      )
    
    ggplotly(p, height = 900, width = 1100)
  })
}

shinyApp(ui2, server2)
