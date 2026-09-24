########################################################
## SCRIPT 02_dataprep/03_shiny_boxplots/app.R
##
## Boxplots de precio por gramo bajo el metodo en produccion
## (menor precio por gramo bajo un umbral, ver 01_webscrap_prep/
## 01_construccion_panel.R).
##
## Reads: data_boxplots.rds
## Correr con: shiny::runApp("shiny_boxplots")
########################################################

library(shiny)
library(tidyverse)
library(scales)

# ============================================================
# Datos (ya preparados por data_prep.R)
## Primero intenta la ruta relativa (funciona al desplegar en
## shinyapps.io, donde data_boxplots.rds viaja junto a app.R).
## Si no la encuentra, usa la ruta absoluta del proyecto.
# ============================================================

ruta_datos <- "data_boxplots.rds"
if (!file.exists(ruta_datos)) {
  ruta_datos <- file.path(
    "C:/Users/Portatil/Desktop/Least-cost-diets-and-affordability/Proyecto Interno/interno/",
    "shiny_boxplots/data_boxplots.rds"
  )
}
if (!file.exists(ruta_datos)) {
  stop("No se encontro data_boxplots.rds. Corre primero data_prep.R.")
}

datos         <- readRDS(ruta_datos)
data_all      <- datos$all
grupos_gabas  <- sort(unique(na.omit(data_all$grupos_gabas)))
excluded_food <- datos$excluded_food

cop_format <- function() scales::comma_format(big.mark = ".", decimal.mark = ",", prefix = "$")

tabla_ui <- function(id) {
  if (requireNamespace("DT", quietly = TRUE)) DT::DTOutput(id) else tableOutput(id)
}

# ============================================================
# UI
# ============================================================

ui <- fluidPage(
  titlePanel("Boxplots precio por gramo (v2: menor precio bajo umbral)"),

  tabsetPanel(

    tabPanel(
      "Todos los alimentos",
      sidebarLayout(
        sidebarPanel(
          width = 3,
          selectInput("grupo_todos", "Grupo GABA",
                      choices = c("Todos", grupos_gabas), selected = "Todos")
        ),
        mainPanel(
          width = 9,
          plotOutput("box_todos", height = "auto")
        )
      )
    ),

    tabPanel(
      "Alimentos seleccionados (CoNA)",
      sidebarLayout(
        sidebarPanel(
          width = 3,
          selectInput("grupo_sel", "Grupo GABA",
                      choices = c("Todos", grupos_gabas), selected = "Todos")
        ),
        mainPanel(
          width = 9,
          plotOutput("box_sel", height = "auto")
        )
      )
    ),

    tabPanel(
      "Resumen min/mediana/max",
      sidebarLayout(
        sidebarPanel(
          width = 3,
          checkboxInput("solo_sel", "Solo alimentos seleccionados", value = FALSE)
        ),
        mainPanel(
          width = 9,
          p(
            "Precio por gramo (COP/g) de cada alimento en las 13 ciudades y los 3 meses del trimestre: ",
            strong("mínimo"), ", ", strong("mediana"), " y ", strong("máximo"), " observados. ",
            sprintf("\"En CoNA\" marca los alimentos que entran en la dieta óptima (excluyendo %s).", excluded_food)
          ),
          tabla_ui("tabla_resumen")
        )
      )
    )
  )
)

# ============================================================
# Server
# ============================================================

server <- function(input, output, session) {

  box_theme <- theme_bw(base_size = 12) +
    theme(panel.grid.minor = element_blank(), legend.position = "none")

  hacer_boxplot <- function(df) {
    orden <- df %>%
      group_by(articulo) %>%
      summarise(mediana = median(precio_gramo, na.rm = TRUE), .groups = "drop") %>%
      arrange(mediana) %>%
      pull(articulo)

    df <- df %>% mutate(articulo = factor(articulo, levels = orden))

    ggplot(df, aes(x = articulo, y = precio_gramo, fill = articulo)) +
      geom_boxplot(outlier.size = 0.8, linewidth = 0.3) +
      scale_y_continuous(labels = cop_format()) +
      scale_fill_viridis_d(option = "D") +
      labs(x = NULL, y = "Precio por gramo (COP/g)") +
      box_theme
  }

  altura_px <- function(n_alimentos) max(400, 28 * n_alimentos)

  data_todos <- reactive({
    df <- data_all
    if (input$grupo_todos != "Todos") df <- df %>% filter(grupos_gabas == input$grupo_todos)
    df
  })

  output$box_todos <- renderPlot({
    req(nrow(data_todos()) > 0)
    hacer_boxplot(data_todos()) + coord_flip()
  }, height = function() altura_px(n_distinct(data_todos()$articulo)))

  data_sel <- reactive({
    df <- data_all %>% filter(seleccionado)
    if (input$grupo_sel != "Todos") df <- df %>% filter(grupos_gabas == input$grupo_sel)
    df
  })

  output$box_sel <- renderPlot({
    req(nrow(data_sel()) > 0)
    hacer_boxplot(data_sel()) + coord_flip()
  }, height = function() altura_px(n_distinct(data_sel()$articulo)))

  tabla_resumen_reactive <- reactive({
    df <- datos$resumen
    if (isTRUE(input$solo_sel)) df <- df %>% filter(seleccionado)
    df %>%
      transmute(
        Alimento          = articulo,
        `Grupo GABA`      = grupos_gabas,
        `En CoNA`         = if_else(seleccionado, "Sí", "No"),
        `N (ciudad-mes)`  = n,
        `Mín (COP/g)`     = round(min_pg, 1),
        `Mediana (COP/g)` = round(mediana_pg, 1),
        `Máx (COP/g)`     = round(max_pg, 1)
      )
  })

  output$tabla_resumen <- if (requireNamespace("DT", quietly = TRUE)) {
    DT::renderDT({ tabla_resumen_reactive() }, options = list(pageLength = 20), rownames = FALSE)
  } else {
    renderTable({ tabla_resumen_reactive() })
  }
}

shinyApp(ui, server)
