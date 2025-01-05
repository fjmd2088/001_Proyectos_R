library(shiny)
library(highcharter)
library(dplyr)
library(tidyr)
library(bslib)
library(shinycssloaders)
library(DT)
library(purrr)

options(shiny.maxRequestSize = 30*1024^2)

ui <- page_sidebar(
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  
  title = div(
    style = "display: flex; align-items: center; gap: 10px; margin-bottom: 10px;",
    # img(src = "LOGO_PP.png", height = "50px"),  # Replace with your image path
    span("Control de inventarios", 
         style = "font-size: 28px; font-weight: bold;")
  ),
  
  sidebar = sidebar(
    fileInput("file", "Cargar archivo CSV",
              accept = c("text/csv",
                         "text/comma-separated-values,text/plain",
                         ".csv")),
    uiOutput("filtros_dinamicos"),
    hr(),
    actionButton("reset_filters", "Limpiar Filtros", 
                 class = "btn-warning", 
                 style = "width: 100%;")
  ),

    
  # Contenedor con scroll
  div(
    style = "height: calc(100vh - 80px); overflow-y: auto; padding: 16px;",
    
    # grafico costo mensuales
    card(
      full_screen = TRUE,
      card_header("Histórico Mensual"),
      div(
        style = "height: 570px;",
        withSpinner(highchartOutput("grafico_lineas", height = "100%"))
      )
    ),
    
    #  grafico productos
    card(
      full_screen = TRUE,
      card_header("Top 20 Productos de mayor impacto"),
      div(
        style = "height: 570px;",
        withSpinner(highchartOutput("grafico_productos", height = "100%"))
      )
    ),
    
    #  grafico consumos
    card(
      full_screen = TRUE,
      card_header("Histórico consumo"),
      div(
        style = "height: 570px;",
        withSpinner(highchartOutput("grafico_consumo", height = "100%"))
      )
    ),
    
    card(
      DTOutput("tabla_datos")
    )
  )
)

server <- function(input, output, session) {
  
  datos <- reactive({
    req(input$file)
    tryCatch({
      df <- read.csv(input$file$datapath, 
                     stringsAsFactors = FALSE,
                     check.names = FALSE,
                     encoding = "latin1") %>%
        mutate(
          mes = as.Date(mes, format = "%d/%m/%Y"),
          costo_menos_2porc = as.numeric(costo_menos_2porc),
          consumo_real = as.numeric(consumo_real),
          consumo_teorico = as.numeric(consumo_teorico),
          dif_teo_final = as.numeric(dif_teo_final)
        )
      
      # Filtrar filas que contengan "****" en cualquier columna
      df <- df[!apply(df, 1, function(x) any(grepl("\\*{4}", x))), ]
      
      columnas_requeridas <- c("sucursal", "semana", "descripcion", "mes", "consumo_teorico", "consumo_real", "dif_teo_final", "costo_menos_2porc")
      columnas_faltantes <- setdiff(columnas_requeridas, names(df))
      
      if(length(columnas_faltantes) > 0) {
        showNotification(paste("Faltan las columnas:", paste(columnas_faltantes, collapse = ", ")),
                         type = "error")
        return(NULL)
      }
      
      return(df)
    }, error = function(e) {
      showNotification(paste("Error al leer el archivo:", e$message),
                       type = "error")
      return(NULL)
    })
  })
  
  output$filtros_dinamicos <- renderUI({
    req(datos())
    
    tagList(
      selectInput("sucursal", "Sucursal:", 
                  choices = c("Todas" = "", sort(unique(datos()$sucursal))), 
                  multiple = TRUE,
                  selected = NULL),
      selectInput("semana", "Semana:",
                  choices = c("Todas" = "", sort(unique(datos()$semana))),
                  multiple = TRUE,
                  selected = NULL),
      selectInput("descripcion", "Descripción:",
                  choices = c("Todas" = "", sort(unique(datos()$descripcion))),
                  multiple = TRUE,
                  selected = NULL)
    )
  })
  
  #-----------------------------------------------------------------------------
  # grafico costo mensuales
  #-----------------------------------------------------------------------------
  datos_filtrados <- reactive({
    req(datos())
    df <- datos()
    
    if (!is.null(input$sucursal) && length(input$sucursal) > 0) {
      df <- df %>% filter(sucursal %in% input$sucursal)
    }
    if (!is.null(input$descripcion) && length(input$descripcion) > 0) {
      df <- df %>% filter(descripcion %in% input$descripcion)
    }
    
    # Variables a graficar
    df %>%
      group_by(mes) %>%
      summarise(consumo_mensual = sum(costo_menos_2porc, na.rm = TRUE)) %>%
      arrange(mes)
  })
  
  output$grafico_lineas <- renderHighchart({
    req(datos_filtrados())
    
    hchart(datos_filtrados(), "line",
           hcaes(x = mes, y = consumo_mensual)) %>%
      hc_title(text = paste("Costo Mensual","(",input$sucursal,")",sep = " ")) %>%
      hc_xAxis(title = list(text = "Mes")) %>%
      hc_yAxis(
        title = list(text = ""),
        labels = list(enabled = FALSE),  # Remove y-axis labels
        gridLines = list(enabled = FALSE)  # Remove y-axis grid lines
      ) %>%
      hc_plotOptions(
        line = list(
          lineWidth = 4,
          dataLabels = list(
            enabled = TRUE,
            format = "{point.y:,.2f}"
          )
        )
      ) %>%
      hc_tooltip(pointFormat = "Consumo: {point.y:,.2f}") %>%
      hc_credits(enabled = FALSE) %>%
      hc_size(height = 570)
  })
  
  #-----------------------------------------------------------------------------
  # grafico productos
  #-----------------------------------------------------------------------------
  
  datos_filtrados_prod <- reactive({
    req(datos())
    df <- datos()
    
    if (!is.null(input$sucursal) && length(input$sucursal) > 0) {
      df <- df %>% filter(sucursal %in% input$sucursal)
    }
    if (!is.null(input$semana) && length(input$semana) > 0) {
      df <- df %>% filter(semana %in% input$semana)
    }
    
    # Variables a graficar
    df %>%
      group_by(descripcion) %>%
      summarise(consumo_producto = sum(costo_menos_2porc, na.rm = TRUE)) %>%
      filter(consumo_producto > 0) %>%
      arrange(desc(consumo_producto)) %>%
      slice_head(n = 20)
  })
  
  output$grafico_productos <- renderHighchart({
    req(datos_filtrados_prod())
    
    hchart(datos_filtrados_prod(), "column",
           hcaes(x = descripcion, y = consumo_producto)) %>%
      hc_title(text = paste0("Costo por producto"," (Semana ",input$semana," - Sucursal ",input$sucursal,")")) %>%
      hc_xAxis(title = list(text = "Producto")) %>%
      hc_yAxis(
        title = list(text = ""),
        labels = list(enabled = FALSE),  # Remove y-axis labels
        gridLines = list(enabled = FALSE)  # Remove y-axis grid lines
      ) %>%
      hc_plotOptions(
        column = list(
          columnWidth = 6,
          dataLabels = list(
            enabled = TRUE,
            format = "{point.y:,.2f}"
          )
        )
      )%>%
      hc_tooltip(pointFormat = "Consumo: {point.y:,.2f}") %>%
      hc_credits(enabled = FALSE) %>%
      hc_size(height = 570)  # Ajuste de altura para el gráfico
  })
  
  #-----------------------------------------------------------------------------
  # grafico consumo
  #-----------------------------------------------------------------------------
  datos_filtrados_cons <- reactive({
    req(datos())
    df <- datos()
    
    # Filtros
    if (!is.null(input$sucursal) && length(input$sucursal) > 0) {
      df <- df %>% filter(sucursal %in% input$sucursal)
    }
    if (is.null(input$descripcion) || length(input$descripcion) == 0) {
      primera_descripcion <- "ACEITE"
      primera_sucursal <- "CAFETALES"
      df <- df %>% filter(descripcion == primera_descripcion & sucursal == primera_sucursal)
    } else {
      df <- df %>% filter(descripcion %in% input$descripcion)
    }
    
    df %>% arrange(semana)
  })
  
  
  output$grafico_consumo <- renderHighchart({
    req(datos_filtrados_cons())
    datos_graf <- datos_filtrados_cons()
    
    # Prepare data for dumbbell chart
    dumbbell_data <- lapply(seq_along(datos_graf$consumo_teorico), function(i) {
      list(
        low = datos_graf$consumo_teorico[i],
        high = datos_graf$consumo_real[i],
        week = datos_graf$semana[i]
      )
    })
    
    highchart() %>%
      hc_title(text = paste0("Consumos (Sucursal: ",input$sucursal," - Producto: ",input$descripcion,")")) %>%
      hc_xAxis(
        title = list(text = "Semana"),
        categories = datos_graf$semana,
        type = "category"
      ) %>%
      hc_yAxis(title = list(text = "Consumo")) %>%
      hc_add_series(
        data = dumbbell_data,
        type = "dumbbell",
        name = "Rango Consumo",
        lowColor = "#0073C2",
        highColor = "#EFC000",
        marker = list(
          enabled = TRUE,
          radius = 4
        ),
        tooltip = list(
          pointFormat = "Consumo Teórico: <b>{point.low:.1f}</b><br/>
                         Consumo Real: <b>{point.high:.1f}</b><br/>"
                         # Diferencia: <b>{point.datos_graf$dif_teo_final:.1f}</b>"
        )
      ) %>%
      hc_add_series(
        name = "Diferencia Teórico Final",
        data = datos_graf$dif_teo_final,
        type = "scatter",
        color = "#FF5733",
        lineWidth = 2,
        marker = list(
          enabled = TRUE,
          radius = 3
        ),
        tooltip = list(
          pointFormat = "<br> Diferencia: <b>{point.y:.1f}</b>"
        )
      ) %>%
      hc_tooltip(shared = TRUE) %>%
      hc_credits(enabled = FALSE) %>%
      hc_plotOptions(
        series = list(
          animation = FALSE
        ),
        dumbbell = list(
          grouping = FALSE
        )
      ) %>%
      hc_size(height = 570)
  })
  
  # resetear filtros
  observeEvent(input$reset_filters, {
    # Remove current UI
    output$filtros_dinamicos <- renderUI({
      tagList(
        selectInput("sucursal", "Sucursal:", 
                    choices = c("Todas" = "", sort(unique(datos()$sucursal))), 
                    multiple = TRUE,
                    selected = character(0)),  # explicitly set to empty
        selectInput("semana", "Semana:",
                    choices = c("Todas" = "", sort(unique(datos()$semana))),
                    multiple = TRUE,
                    selected = character(0)),  # explicitly set to empty
        selectInput("descripcion", "Descripción:",
                    choices = c("Todas" = "", sort(unique(datos()$descripcion))),
                    multiple = TRUE,
                    selected = character(0))  # explicitly set to empty
      )
    })
  })
  
  output$tabla_datos <- renderDT({
    datatable(datos_filtrados_cons(),
              options = list(pageLength = 5))
  })
}

shinyApp(ui = ui, server = server)