# =========================================================
# Shiny — Documento de trabajo 08/25: Primeras estimaciones
# =========================================================

library(shiny)
library(bslib)
library(DT)
library(readxl)
library(dplyr)
library(tidyr)
library(janitor)
library(ggplot2)
library(stringr)

# ------------ helpers ------------
pick_col <- function(df, patterns){
  nms <- names(df)
  for (p in patterns){
    hit <- grep(p, nms, ignore.case = TRUE, perl = TRUE, value = TRUE)
    if (length(hit)) return(hit[1])
  }
  NA_character_
}


# env que hereda (para que el script vea base::library, etc.)
source_inheriting <- function(path, parent = globalenv()){
  env <- new.env(parent = parent)
  sys.source(path, envir = env, chdir = FALSE)
  env
}

ui <- navbarPage(
  title = "Documento de trabajo 08/25: Primeras estimaciones",
  theme = bs_theme(bootswatch = "cerulean"),
  collapsible = TRUE,
  
  tabPanel(
    "Presentación",
    fluidPage(
      br(),
      h4("Objetivo"),
      p("Presentar primeras estimaciones de márgenes de comercialización y métricas CoCA/CoNA para Cali usando datos DANE."),
      hr(),
      h5("Configuración"),
      textInput(
        "base_dir",
        "Carpeta base del proyecto (OneDrive):",
        value = "C:/Users/danie/OneDrive/Escritorio/Least-cost-diets-and-affordability/Proyecto Interno"
      ),
      actionButton("load", "Cargar / recargar datos", class = "btn btn-primary"),
      br(), br(),
      htmlOutput("status_box")
    )
  ),
  
  tabPanel(
    "Datos",
    fluidPage(
      br(),
      h4("Datos de precios minoristas - DANE (fragmento)"),
      DTOutput("tbl_dane"),
      br(), hr(),
      h4("IPC por subclase (COICOP) (fragmento)"),
      DTOutput("tbl_ipc"),
      br(), hr(),
      h4("Tabla correlativa (fragmento)"),
      DTOutput("tbl_corr"),
      br(), hr(),
      h4("Clasificación IPC 2008 (fragmento)"),
      DTOutput("tbl_xyz")
    )
  ),
  tabPanel(
    "Metodología",
    fluidPage(
      withMathJax(),
      br(),
      
      h3("Conformación de la base de datos"),
      p("Como se indicó en la sección anterior, las dos fuentes de información —los precios minoristas y el IPC— emplean sistemas de clasificación distintos. 
       Mientras que los datos de precios minoristas utilizan la clasificación correspondiente a la canasta del IPC de 2008 (organizada por Grupo, Subgrupo, 
       Clase, Gasto Básico y Artículo), los datos del IPC se encuentran estructurados según la nomenclatura COICOP (División, Grupo, Clase, Subclase, Artículo)."),
      p("Para integrar ambos sistemas en una única base de datos, se recurre a la tabla correlativa (COICOP ↔ IPC 2008) cargada en la pestaña de Datos."),
      
      hr(),
      h3("Análisis sobre el margen de comercialización"),
      p("El margen de comercialización se estima comparando el precio minorista y el precio mayorista de cada producto, 
       para cuantificar la diferencia relativa entre ambos."),
      p("Cualquiera que sea el producto $k$, el margen se define como:"),
      div("$$\\text{Margen}_{k} = \\frac{P^{\\text{min}}_k - P^{\\text{may}}_k}{P^{\\text{may}}_k}$$"),
      p("donde $P^{\\text{min}}_k$ corresponde al precio minorista y $P^{\\text{may}}_k$ corresponde al precio mayorista del producto $k$. 
       Para el análisis de los márgenes de comercialización, se examina su distribución a lo largo del tiempo. 
       En particular, se calcula para cada producto el margen mediano (Q2) y los márgenes correspondientes al primer cuartil (Q1) y el tercer cuartil (Q3)."),
      
      hr(),
      h3("Estimación de métricas basadas en dietas de costo mínimo"),
      
      h4("CoCA — Costo mínimo de una dieta suficiente en energía"),
      p("El CoCA se estima seleccionando el alimento —o conjunto de alimentos— que proporciona la cantidad de calorías necesaria para satisfacer el 
       Requerimiento Energético Estimado (EER). Siguiendo la literatura previa, para un grupo demográfico $i$, el CoCA se calcula mediante:"),
      div("
$$
\\begin{aligned}
\\text{Minimizar:}\\quad & \\sum_{j=1}^n p_j x_j \\\\
\\text{sujeto a:}\\quad & \\sum_{j=1}^n e_j x_j = \\text{EER}_i \\\\
& x_j \\ge 0 \\quad \\forall j
\\end{aligned}
$$
    "),
      p("donde:"),
      tags$ul(
        tags$li("$x_j$ es la cantidad (en gramos) del alimento $j$ seleccionada para un individuo del grupo $i$."),
        tags$li("$p_j$ es el precio minorista del alimento $j$."),
        tags$li("$e_j$ es el contenido energético (kcal/gramo) del alimento $j$."),
        tags$li("$\\text{EER}_i$ es el Requerimiento Energético Estimado para el grupo $i$.")
      ),
      p("La solución óptima corresponde al alimento (o combinación) con menor precio por kilocaloría."),
      
      h4("CoNA — Costo mínimo de una dieta adecuada en nutrientes"),
      p("El CoNA se determina a partir del conjunto de alimentos disponibles que, para una ubicación y período determinados, permiten satisfacer el EER al menor costo 
       posible, cumpliendo además con los límites inferior y superior de ingesta de macro- y micronutrientes:"),
      div("$$\\text{CoNA}(i) = \\min_{x_1,\\dots,x_n} \\sum_{j=1}^n p_j x_j$$"),
      p("sujeto a:"),
      div("$$\\sum_{j=1}^n e_j x_j = \\text{EER}_i$$"),
      div("$$\\sum_{j=1}^n a_{kj} x_j \\le U_{ik} \\quad \\forall k$$"),
      div("$$\\sum_{j=1}^n a_{kj} x_j \\ge L_{ik} \\quad \\forall k$$"),
      div("$$x_j \\ge 0$$"),
      p("donde:"),
      tags$ul(
        tags$li("$x_j$ es la cantidad —en gramos— del alimento $j$ en la dieta óptima."),
        tags$li("$p_j$ corresponde al precio minorista del alimento $j$."),
        tags$li("$e_j$ es el contenido energético (kcal) por gramo del alimento $j$."),
        tags$li("$a_{kj}$ indica el contenido del nutriente $k$ en el alimento $j$."),
        tags$li("$L_{ik}$ y $U_{ik}$ son los límites inferior y superior de ingesta del nutriente $k$ para el grupo $i$.")
      )
    )
  ),
  
  tabPanel(
    "Resultados: Márgenes",
    fluidPage(
      br(),
      fluidRow(
        column(
          4,
          h5("Filtros"),
          selectInput("ciudad", "Ciudad", c("CALI","MEDELLÍN","BOGOTÁ D.C."), "CALI"),
          uiOutput("subclase_ui"),
          br(),
          htmlOutput("margenes_msg")
        ),
        column(
          8,
          plotOutput("plot_subclase", height = "460px")
        )
      )
    )
  ),
  tabPanel(
    "Precios a partir de los márgenes",
    fluidPage(
      br(),
      fluidRow(
        column(
          4,
          h5("Filtros"),
          uiOutput("precio_alimento_ui"),
          helpText("El estimado (Q1–Q3/Q2) se calcula a partir de los márgenes del alimento seleccionado.")
        ),
        column(
          8,
          plotOutput("plot_precios_margen", height = "520px")
        )
      )
    )
  ),
  
  tabPanel(
    "Métricas: CoCA / CoNA",
    fluidPage(
      br(),
      h3("CoCA"),
      fluidRow(
        column(4, uiOutput("coca_filtros")),
        column(8, plotOutput("plot_coca", height = "520px"))
      ),
      br(), hr(),
      h3("CoNA"),
      fluidRow(
        column(4, uiOutput("cona_filtros")),
        column(8, plotOutput("plot_cona", height = "520px"))
      )
    )
  )
)

server <- function(input, output, session){
  
  rv <- reactiveValues(
    xyz = NULL, dane = NULL, ipc = NULL, corr = NULL,
    retail = NULL, coca = NULL, cona = NULL,
    logs = list()
  )
  
  observeEvent(input$load, {
    req(input$base_dir)
    rv$logs <- list()
    
    dir_base <- normalizePath(trimws(input$base_dir), winslash = "/", mustWork = FALSE)
    if (!dir.exists(dir_base)) {
      showNotification("La carpeta base no existe.", type = "error")
      return(invisible(NULL))
    }
    
    # --- rutas  ---
    rutas <- list(
      xyz     = file.path(dir_base, "var-ipc", "XYZ_Correlativa-ENPH-IPC-2008.xlsx"),
      dane    = file.path(dir_base, "Precios DANE", "OUTPUT_DANE", "precios_unadj_DANE_1999_2018.xlsx"),
      ipc     = file.path(dir_base, "var-ipc", "IPC.xls"),
      corr    = file.path(dir_base, "var-ipc", "correlativa_ipc.xlsx"),
      s_retail= file.path(dir_base, "margen-dist", "v1-join-ipc-sipsa.R"),
      s_coca  = file.path(dir_base, "estimadores-banrep", "CALI", "coca", "v1_coca_q1_q3.R"),
      s_cona  = file.path(dir_base, "estimadores-banrep", "CALI", "cona", "v1_cona_q1_q3.R")
    )
    
    # helper lectura con logs
    leer_archivo <- function(path_abs, fun) {
      p <- normalizePath(path_abs, winslash = "/", mustWork = FALSE)
      if (!file.exists(p)) { rv$logs <- append(rv$logs, list(paste("[WARN] No existe:", p))); return(NULL) }
      tryCatch(fun(p),
               error = function(e){ rv$logs <- append(rv$logs, list(paste("[ERROR]", e$message))); NULL })
    }
    
    withProgress(message = "Cargando datos…", value = 0, {
      incProgress(0.1)
      
      rv$xyz <- leer_archivo(rutas$xyz, function(p)
        readxl::read_excel(p) |>
          janitor::clean_names() |>
          dplyr::mutate(clase = clase_9, articulo = articulo_11) |>
          dplyr::select(clase, gasto_basico, articulo, descripcion_ipc)
      )
      
      rv$dane <- leer_archivo(rutas$dane, readxl::read_excel)
      
      rv$ipc <- leer_archivo(rutas$ipc, function(p)
        readxl::read_excel(p) |>
          janitor::clean_names() |>
          dplyr::mutate(
            ciudad = dplyr::case_when(
              ciudad == "CARTAGENA DE INDIAS" ~ "CARTAGENA",
              ciudad == "BOGOTÁ, D.C." ~ "BOGOTÁ D.C.",
              TRUE ~ ciudad
            ),
            cod_subclase = substr(subclase, 1, 8)
          )
      )
      
      rv$corr <- leer_archivo(rutas$corr, function(p)
        readxl::read_excel(p) |>
          tidyr::fill(subclase, ipc, .direction = "down")
      )
      
      # --- Integración (retail) ---
      rv$retail <- tryCatch({
        if (!file.exists(rutas$s_retail)) stop("No se encontró script retail: ", rutas$s_retail)
        e <- new.env(parent = globalenv())
        sys.source(rutas$s_retail, envir = e, chdir = FALSE)
        if (exists("retail_whole_18", envir = e)) get("retail_whole_18", envir = e) else NULL
      }, error = function(e){ rv$logs <- append(rv$logs, list(paste("[ERROR retail]", e$message))); NULL })
      
      # --- CoCA ---
      rv$env_coca <- new.env(parent = globalenv())
      tryCatch(sys.source(rutas$s_coca, envir = rv$env_coca, chdir = FALSE),
               error = function(e) rv$logs <- append(rv$logs, list(paste("[ERROR CoCA]", e$message))))
      rv$coca <- if (exists("coca_resultados", envir = rv$env_coca)) get("coca_resultados", rv$env_coca)
      else if (exists("resultados_coca", envir = rv$env_coca)) get("resultados_coca", rv$env_coca) else NULL
      
      # --- CoNA ---
      rv$env_cona <- new.env(parent = globalenv())
      tryCatch(sys.source(rutas$s_cona, envir = rv$env_cona, chdir = FALSE),
               error = function(e) rv$logs <- append(rv$logs, list(paste("[ERROR CoNA]", e$message))))
      rv$cona <- if (exists("resultados_cona", envir = rv$env_cona)) get("resultados_cona", rv$env_cona)
      else if (exists("cona_resultados", envir = rv$env_cona)) get("cona_resultados", rv$env_cona) else NULL
      
      incProgress(0.9)
    })
    
    showNotification("Carga finalizada.", type = "message")
  })
  
  # --- Estado (✓/✗). Píntalo en 'resultado_carga' y también en 'status_box' ---
  render_estado <- function(){
    ok <- c(
      "XYZ" = !is.null(rv$xyz),
      "DANE" = !is.null(rv$dane),
      "IPC" = !is.null(rv$ipc),
      "Correlativa" = !is.null(rv$corr),
      "Pipeline (retail_whole_18)" = !is.null(rv$retail),
      "CoCA" = !is.null(rv$coca),
      "CoNA" = !is.null(rv$cona)
    )
    cls <- if (all(ok)) "alert-success" else if (any(ok)) "alert-warning" else "alert-danger"
    div(class = paste("alert", cls),
        tags$b("Resultado de la carga:"),
        tags$ul(lapply(names(ok), function(nm)
          tags$li(sprintf("%s: %s", nm, if (ok[[nm]]) "✓" else "✗"))
        ))
    )
  }
  output$resultado_carga <- renderUI(render_estado())
  output$status_box      <- renderUI(render_estado())   # opcional para compatibilidad
  
  output$logs <- renderText(paste(unlist(rv$logs), collapse = "\n"))
  
  # ---- tablas ----
  output$tbl_xyz <- renderDT({
    if (is.null(rv$xyz)) return(datatable(data.frame(Mensaje="No se encontró var-ipc/XYZ_Correlativa-ENPH-IPC-2008.xlsx")))
    datatable(head(rv$xyz[11:nrow(rv$xyz),], 30), options=list(scrollX=TRUE))
  })
  output$tbl_dane <- renderDT({
    if (is.null(rv$dane)) return(datatable(data.frame(Mensaje="No se encontró Precios DANE/OUTPUT_DANE/precios_unadj_DANE_1999_2018.xlsx")))
    datatable(head(rv$dane[,c("ano","mes_num","nombre_ciudad","codigo_articulo","articulo","precio_500g")], 30),
              options=list(scrollX=TRUE))
  })
  output$tbl_ipc <- renderDT({
    if (is.null(rv$ipc)) return(datatable(data.frame(Mensaje="No se encontró var-ipc/IPC.xls")))
    datatable(head(rv$ipc[,c("ano","mes","ciudad","division","grupo","clase","subclase","cod_subclase","numero_indice")], 30),
              options=list(scrollX=TRUE))
  })
  output$tbl_corr <- renderDT({
    if (is.null(rv$corr)) return(datatable(data.frame(Mensaje="No se encontró var-ipc/correlativa_ipc.xlsx")))
    datatable(head(rv$corr, 30), options=list(scrollX=TRUE))
  })
  
  # ---- márgenes ----
  margenes_df <- reactive({
    req(rv$retail)
    whole_tres <- rv$retail %>%
      filter(nombre_ciudad %in% c("MEDELLÍN","CALI","BOGOTÁ D.C.")) %>%
      rename(precio_500g_ipc = precio_500g,
             precio_500g_sipsa = precio_medio)
    data_min_may <- whole_tres %>%
      select(cod_mun, ciudad, nombre_ciudad, Year, mes, Month, Alimento, retail,
             codigo_articulo, precio_500g_sipsa, precio_500g_ipc) %>%
      rename(mes_num = Month, ano = Year, sipsa = Alimento, articulo = retail) %>%
      mutate(fecha = as.Date(paste(ano, mes_num, "01", sep = "-"))) %>%
      filter(nombre_ciudad == input$ciudad)
    if (!nrow(data_min_may)) return(NULL)
    data_min_may$cod_subclase <- paste0("0", substr(data_min_may$codigo_articulo, 1, 6), "0")
    data_min_may %>%
      group_by(articulo, cod_subclase) %>%
      mutate(factor = precio_500g_ipc / precio_500g_sipsa,
             margen = (factor - 1) * 100) %>%
      ungroup() %>%
      filter(is.finite(margen), margen > 0)
  })
  
  output$margenes_msg <- renderUI({
    if (is.null(rv$retail))
      return(div(class="alert alert-warning","No se pudo cargar 'retail_whole_18' desde margen-dist/v1-join-ipc-sipsa.R"))
    if (is.null(margenes_df()))
      return(div(class="alert alert-warning","No hay datos de márgenes para la ciudad seleccionada."))
    NULL
  })
  
  output$subclase_ui <- renderUI({
    md <- margenes_df(); if (is.null(md)) return(NULL)
    subs <- sort(unique(md$cod_subclase))
    selectInput("subclase","Subclase", choices = subs, selected = subs[1])
  })
  
  output$plot_subclase <- renderPlot({
    md <- margenes_df(); req(md, input$subclase)
    df_sub <- subset(md, cod_subclase == input$subclase)
    validate(need(nrow(df_sub) > 0, "Sin datos para esa subclase."))
    ggplot(df_sub, aes(x = articulo, y = margen)) +
      geom_boxplot(outlier.shape = 16, outlier.size = 2, alpha = .85, width = .7) +
      labs(title = paste("Distribución de márgenes —", input$ciudad, "— Subclase", input$subclase),
           x = "Artículo", y = "Margen (%)") +
      theme_bw(base_size = 14) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # ---- Precios a partir de márgenes (filtro por alimento) ----
  output$precio_alimento_ui <- renderUI({
    md <- margenes_df(); req(md)
    arts <- sort(unique(as.character(md$articulo)))
    selectInput("alimento", "Alimento (artículo retail)", choices = arts, selected = arts[1])
  })
  
  # serie preparada para el plot (minorista, mayorista y estimado por Q1/Q2/Q3)
  serie_precios_margen <- reactive({
    req(rv$retail, input$ciudad, input$alimento)
    d <- rv$retail %>%
      dplyr::filter(nombre_ciudad == input$ciudad, retail == input$alimento) %>%
      dplyr::mutate(fecha = as.Date(paste(Year, Month, "01", sep="-"))) %>%
      dplyr::rename(mayorista = precio_medio, minorista = precio_500g)
    
    # puede haber varias filas por fecha (varios SIPSA). Agregamos con mediana.
    d <- d %>%
      dplyr::group_by(fecha) %>%
      dplyr::summarise(
        mayorista = median(mayorista, na.rm = TRUE),
        minorista = median(minorista, na.rm = TRUE),
        .groups = "drop"
      )
    
    # cuartiles del factor (como en tu Rmd: precio_min / precio_may)
    md <- margenes_df(); req(md)
    fac <- md %>% dplyr::filter(articulo == input$alimento) %>%
      dplyr::transmute(factor = precio_500g_ipc / precio_500g_sipsa) %>% dplyr::pull(factor)
    q <- stats::quantile(fac[is.finite(fac)], probs = c(.25,.5,.75), na.rm = TRUE)
    
    d %>%
      dplyr::mutate(
        est_q1 = mayorista * q[1],
        est_q2 = mayorista * q[2],
        est_q3 = mayorista * q[3]
      )
  })
  
  output$plot_precios_margen <- renderPlot({
    df <- serie_precios_margen(); req(df)
    ggplot2::ggplot(df, ggplot2::aes(x = fecha)) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = est_q1, ymax = est_q3), fill = "red", alpha = 0.18) +
      ggplot2::geom_line(ggplot2::aes(y = est_q1), color = "red", linetype = 2, size = 0.6) +
      ggplot2::geom_line(ggplot2::aes(y = est_q3), color = "red", linetype = 2, size = 0.6) +
      ggplot2::geom_line(ggplot2::aes(y = est_q2), color = "red", size = 0.9) +
      ggplot2::geom_line(ggplot2::aes(y = minorista), color = "black", size = 1.0) +
      ggplot2::geom_line(ggplot2::aes(y = mayorista), color = "darkgreen", linetype = 2, size = 0.8) +
      ggplot2::labs(
        title = paste(input$ciudad, "—", input$alimento),
        x = "Fecha", y = "Precio (500g)",
        caption = "Negro: minorista | Verde (discontinua): mayorista | Rojo: estimado (Q1–Q3 / Q2)"
      ) +
      ggplot2::theme_minimal(base_size = 13) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  })
  
  # Filtros CoCA
  output$coca_filtros <- renderUI({
    if (is.null(rv$coca))
      return(div(class="alert alert-warning","No se pudo cargar CoCA (falta 'coca_resultados')."))
    df <- rv$coca
    sex_col <- pick_col(df, c("^sexo$","genero","sex"))
    age_col <- pick_col(df, c("grupo.*etario","grupo.*edad","demo.*group","age_group","edad"))
    tagList(
      selectInput("coca_sexo","Sexo", sort(unique(df[[sex_col]]))),
      selectInput("coca_edad","Grupo etario", sort(unique(df[[age_col]])))
    )
  })
  
  # Plot CoCA
  output$plot_coca <- renderPlot({
    df <- rv$coca; req(df, input$coca_sexo, input$coca_edad)
    date_col <- pick_col(df, c("^fecha$","date","period","mes"))
    cost_col <- pick_col(df, c("^costo","^cost","cost_day"))
    sex_col  <- pick_col(df, c("^sexo$","genero","sex"))
    age_col  <- pick_col(df, c("grupo.*etario","grupo.*edad","demo.*group","age_group","edad"))
    validate(need(!is.na(date_col) && !is.na(cost_col), "Faltan fecha/costo en CoCA."))
    
    d <- df %>% dplyr::filter(.data[[sex_col]] == input$coca_sexo,
                              .data[[age_col]] == input$coca_edad)
    
    if ("escenario" %in% names(d)) {
      d <- d %>%
        dplyr::select(all_of(c(date_col,"escenario",cost_col))) %>%
        dplyr::group_by(.data[[date_col]], escenario) %>%
        dplyr::summarise(val = mean(.data[[cost_col]], na.rm = TRUE), .groups="drop") %>%
        tidyr::pivot_wider(names_from = escenario, values_from = val)
    }
    
    q1_col <- pick_col(d, c("precio.*q1","q1","p25","low"))
    q2_col <- pick_col(d, c("precio.*q2","q2","median"))
    q3_col <- pick_col(d, c("precio.*q3","q3","p75","high"))
    
    ggplot2::ggplot(d, ggplot2::aes(x = .data[[date_col]])) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = .data[[q1_col]], ymax = .data[[q3_col]]),
                           fill = "red", alpha = 0.18) +
      ggplot2::geom_line(ggplot2::aes(y = .data[[q1_col]]), color = "red", linetype = 2, size = 0.6) +
      ggplot2::geom_line(ggplot2::aes(y = .data[[q3_col]]), color = "red", linetype = 2, size = 0.6) +
      ggplot2::geom_line(ggplot2::aes(y = .data[[q2_col]]), color = "black", size = 0.9) +
      ggplot2::labs(x = "Fecha", y = "Costo diario (COP)",
                    title = paste("CoCA —", input$coca_sexo, "—", input$coca_edad),
                    caption = "Rojo: banda Q1–Q3 (punteadas) | Negro: Q2") +
      ggplot2::theme_bw(base_size = 13) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  })
  
  # Filtros CoNA
  output$cona_filtros <- renderUI({
    if (is.null(rv$cona))
      return(div(class="alert alert-warning","No se pudo cargar CoNA (falta 'resultados_cona'/'cona_resultados')."))
    df <- rv$cona
    sex_col <- pick_col(df, c("^sexo$","genero","sex","^Sex$"))
    age_col <- pick_col(df, c("^Demo.*Group$","demo.*group","grupo.*etario","grupo.*edad","age_group","edad"))
    tagList(
      selectInput("cona_sexo","Sexo", sort(unique(df[[sex_col]]))),
      selectInput("cona_edad","Grupo etario", sort(unique(df[[age_col]])))
    )
  })
  
  # Plot CoNA 
  output$plot_cona <- renderPlot({
    df <- rv$cona; req(df, input$cona_sexo, input$cona_edad)
    date_col <- pick_col(df, c("^fecha$","date","period","mes"))
    sex_col  <- pick_col(df, c("^sexo$","genero","sex","^Sex$"))
    age_col  <- pick_col(df, c("^Demo.*Group$","demo.*group","grupo.*etario","grupo.*edad","age_group","edad"))
    cost_col <- pick_col(df, c("cost_day","^costo","^cost"))
    validate(need(!is.na(date_col) && !is.na(cost_col), "Faltan fecha/costo en CoNA."))
    
    d <- df %>%
      dplyr::filter(.data[[sex_col]] == input$cona_sexo,
                    .data[[age_col]] == input$cona_edad) %>%
      dplyr::select(all_of(c(date_col,"escenario",cost_col))) %>%
      dplyr::group_by(.data[[date_col]], escenario) %>%
      dplyr::summarise(val = mean(.data[[cost_col]], na.rm = TRUE), .groups="drop") %>%
      tidyr::pivot_wider(names_from = escenario, values_from = val)
    
    q1_col <- pick_col(d, c("precio.*q1","q1","p25","low"))
    q2_col <- pick_col(d, c("precio.*q2","q2","median"))
    q3_col <- pick_col(d, c("precio.*q3","q3","p75","high"))
    
    ggplot2::ggplot(d, ggplot2::aes(x = .data[[date_col]])) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = .data[[q1_col]], ymax = .data[[q3_col]]),
                           fill = "red", alpha = 0.18) +
      ggplot2::geom_line(ggplot2::aes(y = .data[[q1_col]]), color = "red", linetype = 2, size = 0.6) +
      ggplot2::geom_line(ggplot2::aes(y = .data[[q3_col]]), color = "red", linetype = 2, size = 0.6) +
      ggplot2::geom_line(ggplot2::aes(y = .data[[q2_col]]), color = "black", size = 0.9) +
      ggplot2::labs(x = "Fecha", y = "Costo diario (COP)",
                    title = paste("CoNA —", input$cona_sexo, "—", input$cona_edad),
                    caption = "Rojo: banda Q1–Q3 (punteadas) | Negro: Q2") +
      ggplot2::theme_bw(base_size = 13) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  })
}

shinyApp(ui, server)