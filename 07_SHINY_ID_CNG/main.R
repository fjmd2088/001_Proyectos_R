library(shiny)
library(DT)
library(bslib)
library(dplyr)
library(jsonlite)
library(openxlsx)
library(shinyjs)

RUTA_DATABASE <- "database/"

# Función para cargar datos de manera más eficiente
load_data <- function() {
  RUTA_DATABASE <- "database/"
  
  datos_inconsistencias <- openxlsx::read.xlsx(paste0(RUTA_DATABASE,
                                                      "inconsistencias_2025-03-06",
                                                      ".xlsx")) %>%
    # filter(cve_mun_INC != -2) %>%
    select(-cve_mun) %>%
    rename(
      CNG = "censo",
      tipo_infra = "nom_infra",
      ID_2024 = "ID_INEGI_2024",
      Ent = "NOM_ENT",
      Mun = "nom_mun",
      cve_mun = "cve_mun_INC",
      nom_mun = "nom_mun_INC",
      nom_infra = "nom_infraestructura_INC",
      latitud = "latitud_INC",
      longitud = "longitud_INC",
      estatus = "estatus_INC"
    ) %>%
    select(CNG, tipo_infra, ID_2024, Ent, Mun, cve_mun, nom_mun, nom_infra, latitud, 
           longitud, estatus, BASE)
  
  # Convertir a tipo adecuado para mejorar rendimiento
  datos_inconsistencias$CNG <- as.character(datos_inconsistencias$CNG)
  datos_inconsistencias$ID_2024 <- as.character(datos_inconsistencias$ID_2024)
  
  # Separar datos
  datos_oficina <- datos_inconsistencias %>%
    filter(BASE == "Dashboard ID") %>%
    select(-BASE)
  
  datos_censo <- datos_inconsistencias %>%
    filter(BASE == "CENSO") %>%
    select(-BASE)
  
  return(list(
    oficina = datos_oficina,
    censo = datos_censo
  ))
}

# Base de datos para guardar modificaciones
datos_corregidos <- data.frame()

# UI con mejoras para carga de datos
ui <- page_fluid(
  useShinyjs(),  # Necesario para habilitar shinyjs
  
  theme = bs_theme(bootswatch = "flatly"),
  
  navset_card_pill(
    id = "navset",
    nav_panel(
      title = "Inconsistencias",
      layout_sidebar(
        sidebar = sidebar(
          title = "Filtros",
          width = 300,
          # Usar selectizeInput normal sin server = TRUE
          selectizeInput("CNG", "CNG:", choices = NULL, multiple = TRUE),
          selectizeInput("tipo_infra", "Tipo Infraestructura:", choices = NULL, multiple = TRUE),
          selectizeInput("ID_2024", "ID INEGI 2024:", choices = NULL, multiple = TRUE),
          selectizeInput("Ent", "Entidad:", choices = NULL, multiple = TRUE),
          selectizeInput("Mun", "Municipio:", choices = NULL, multiple = TRUE),
          actionButton("reset", "Limpiar filtros", class = "btn-secondary"),
          hr(),
          # Agregar limitador de resultados para mejorar rendimiento
          numericInput("max_rows", "Mostrar máximo filas:", 100, min = 10, max = 1000)
        ),
        card(
          card_header("Información Oficina Central"),
          # Cambio a DT para mejorar rendimiento con paginación
          DTOutput("tabla_oficina")
        )
      )
    ),
    nav_panel(
      title = "Corregir Registro",
      uiOutput("edicion_panel")
    )
  )
)


server <- function(input, output, session) {
  # Cargar datos de manera reactiva al inicio
  datos <- reactiveVal()
  
  valores <- reactiveValues(
    # save_status = "init",
    # cng_seleccionado = NULL,
    registro_corregido = NULL,  # Aquí almacenaremos la información después de guardar
    mostrar_card_corregida = FALSE
  )
  
  # Cargar los datos inmediatamente al iniciar la aplicación
  datos_cargados <- tryCatch({
    load_data()
  }, error = function(e) {
    showNotification(paste("Error al cargar datos:", e$message), 
                     type = "error", duration = NULL)
    return(list(oficina = data.frame(), censo = data.frame()))
  })
  
  # Establecer datos cargados
  datos(datos_cargados)
  
  # Inicializar opciones de selectizeInput sin el parámetro server = TRUE
  # Función auxiliar para filtrar datos según los filtros actuales
  filtered_data_for_choices <- reactive({
    result <- datos()$oficina
    
    if (!is.null(input$CNG) && length(input$CNG) > 0)
      result <- result %>% filter(CNG %in% input$CNG)
    
    if (!is.null(input$tipo_infra) && length(input$tipo_infra) > 0)
      result <- result %>% filter(tipo_infra %in% input$tipo_infra)
    
    if (!is.null(input$ID_2024) && length(input$ID_2024) > 0)
      result <- result %>% filter(ID_2024 %in% input$ID_2024)
    
    if (!is.null(input$Ent) && length(input$Ent) > 0)
      result <- result %>% filter(Ent %in% input$Ent)
    
    if (!is.null(input$Mun) && length(input$Mun) > 0)
      result <- result %>% filter(Mun %in% input$Mun)
    
    return(result)
  })
  
  # Inicializar todos los selectores al cargar la aplicación
  observe({
    req(datos())
    updateSelectizeInput(session, "CNG", choices = unique(datos()$oficina$CNG), selected = character(0))
    updateSelectizeInput(session, "tipo_infra", choices = unique(datos()$oficina$tipo_infra), selected = character(0))
    updateSelectizeInput(session, "ID_2024", choices = unique(datos()$oficina$ID_2024), selected = character(0))
    updateSelectizeInput(session, "Ent", choices = unique(datos()$oficina$Ent), selected = character(0))
    updateSelectizeInput(session, "Mun", choices = unique(datos()$oficina$Mun), selected = character(0))
  })
  
  # Actualizar filtros CNG cuando cambian otros filtros
  observeEvent(list(input$tipo_infra, input$ID_2024, input$Ent, input$Mun), {
    filtered <- filtered_data_for_choices()
    current_selected <- isolate(input$CNG)
    new_choices <- unique(filtered$CNG)
    
    # Mantener selecciones válidas
    valid_selected <- intersect(current_selected, new_choices)
    
    updateSelectizeInput(session, "CNG", 
                         choices = new_choices,
                         selected = valid_selected)
  }, ignoreNULL = FALSE, ignoreInit = TRUE)
  
  # Actualizar filtros tipo_infra cuando cambian otros filtros
  observeEvent(list(input$CNG, input$ID_2024, input$Ent, input$Mun), {
    filtered <- filtered_data_for_choices()
    current_selected <- isolate(input$tipo_infra)
    new_choices <- unique(filtered$tipo_infra)
    
    # Mantener selecciones válidas
    valid_selected <- intersect(current_selected, new_choices)
    
    updateSelectizeInput(session, "tipo_infra", 
                         choices = new_choices,
                         selected = valid_selected)
  }, ignoreNULL = FALSE, ignoreInit = TRUE)
  
  # Actualizar filtros ID_2024 cuando cambian otros filtros
  observeEvent(list(input$CNG, input$tipo_infra, input$Ent, input$Mun), {
    filtered <- filtered_data_for_choices()
    current_selected <- isolate(input$ID_2024)
    new_choices <- unique(filtered$ID_2024)
    
    # Mantener selecciones válidas
    valid_selected <- intersect(current_selected, new_choices)
    
    updateSelectizeInput(session, "ID_2024", 
                         choices = new_choices,
                         selected = valid_selected)
  }, ignoreNULL = FALSE, ignoreInit = TRUE)
  
  # Actualizar filtros Ent cuando cambian otros filtros
  observeEvent(list(input$CNG, input$tipo_infra, input$ID_2024, input$Mun), {
    filtered <- filtered_data_for_choices()
    current_selected <- isolate(input$Ent)
    new_choices <- unique(filtered$Ent)
    
    # Mantener selecciones válidas
    valid_selected <- intersect(current_selected, new_choices)
    
    updateSelectizeInput(session, "Ent", 
                         choices = new_choices,
                         selected = valid_selected)
  }, ignoreNULL = FALSE, ignoreInit = TRUE)
  
  # Actualizar filtros Mun cuando cambian otros filtros
  observeEvent(list(input$CNG, input$tipo_infra, input$ID_2024, input$Ent), {
    filtered <- filtered_data_for_choices()
    current_selected <- isolate(input$Mun)
    new_choices <- unique(filtered$Mun)
    
    # Mantener selecciones válidas
    valid_selected <- intersect(current_selected, new_choices)
    
    updateSelectizeInput(session, "Mun", 
                         choices = new_choices,
                         selected = valid_selected)
  }, ignoreNULL = FALSE, ignoreInit = TRUE)
  
  # Datos filtrados
  datos_filtrados <- reactive({
    req(datos())
    datos_oficina <- datos()$oficina
    
    if (!is.null(input$CNG) && length(input$CNG) > 0) {
      datos_oficina <- datos_oficina %>% filter(CNG %in% input$CNG)
    }
    if (!is.null(input$tipo_infra) && length(input$tipo_infra) > 0) {
      datos_oficina <- datos_oficina %>% filter(tipo_infra %in% input$tipo_infra)
    }
    if (!is.null(input$ID_2024) && length(input$ID_2024) > 0) {
      datos_oficina <- datos_oficina %>% filter(ID_2024 %in% input$ID_2024)
    }
    if (!is.null(input$Ent) && length(input$Ent) > 0) {
      datos_oficina <- datos_oficina %>% filter(Ent %in% input$Ent)
    }
    if (!is.null(input$Mun) && length(input$Mun) > 0) {
      datos_oficina <- datos_oficina %>% filter(Mun %in% input$Mun)
    }
    
    # Limitar número de resultados para mejorar rendimiento
    if (nrow(datos_oficina) > input$max_rows) {
      datos_oficina <- head(datos_oficina, input$max_rows)
    }
    
    return(datos_oficina)
  })
  
  datos_censo_filtrados <- reactive({
    req(datos())
    datos_filtrados_ids <- datos_filtrados()$CNG
    
    censo_filtrado <- datos()$censo %>% 
      filter(CNG %in% datos_filtrados_ids)
    
    return(censo_filtrado)
  })
  

  # Botón de resetear filtros-----------------------------------------------------------------------
  observeEvent(input$reset, {
    updateSelectizeInput(session, "CNG", selected = character(0))
    updateSelectizeInput(session, "tipo_infra", selected = character(0))
    updateSelectizeInput(session, "ID_2024", selected = character(0))
    updateSelectizeInput(session, "Ent", selected = character(0))
    updateSelectizeInput(session, "Mun", selected = character(0))
    
    # Actualizar todos los filtros con los valores completos originales
    updateSelectizeInput(session, "CNG", choices = unique(datos()$oficina$CNG))
    updateSelectizeInput(session, "tipo_infra", choices = unique(datos()$oficina$tipo_infra))
    updateSelectizeInput(session, "ID_2024", choices = unique(datos()$oficina$ID_2024))
    updateSelectizeInput(session, "Ent", choices = unique(datos()$oficina$Ent))
    updateSelectizeInput(session, "Mun", choices = unique(datos()$oficina$Mun))
  })
  
  # Preparar datos para DataTable con colores
  prepare_dt_data <- function(data_oficina, data_censo) {
    # Seleccionar solo columnas necesarias
    data_comp <- data_oficina %>%
      select(cve_mun, nom_mun, nom_infra, latitud, longitud, estatus)
    
    # Crear identificadores de diferencias para colorear celdas
    diff_status <- matrix(0, nrow = nrow(data_comp), ncol = ncol(data_comp))
    colnames(diff_status) <- colnames(data_comp)
    
    # Comparar con datos del censo
    if (nrow(data_censo) > 0) {
      data_censo_comp <- data_censo %>%
        select(cve_mun, nom_mun, nom_infra, latitud, longitud, estatus)
      
      # Para cada fila y columna, determinar si hay diferencia
      for (i in 1:min(nrow(data_comp), nrow(data_censo_comp))) {
        for (j in 1:ncol(data_comp)) {
          if (is.na(data_comp[i,j]) && is.na(data_censo_comp[i,j])) {
            # Ambos NA - coincidencia
            diff_status[i,j] <- -1
          } else if (is.na(data_comp[i,j]) || is.na(data_censo_comp[i,j])) {
            # Uno es NA - hay diferencia
            diff_status[i,j] <- 1
          } else if (!isTRUE(all.equal(data_comp[i,j], data_censo_comp[i,j]))) {
            # Valores diferentes
            diff_status[i,j] <- 1
          } else {
            # Valores iguales
            diff_status[i,j] <- -1
          }
        }
      }
    }
    
    return(list(
      data = data_comp,
      diff_status = diff_status
    ))
  }
  
  # Tabla Oficina mejorada con DT y coloreado de diferencias----------------------------------------

  output$tabla_oficina <- renderDT({
    req(datos_filtrados())
    datos_of <- datos_filtrados()
    datos_cen <- datos_censo_filtrados()
    
    if (nrow(datos_of) == 0) {
      return(datatable(data.frame(Mensaje = "No hay datos disponibles")))
    }
    
    # Columnas que queremos comparar para identificar diferencias
    columnas_comparar <- c("cve_mun", "nom_mun", "nom_infra", "latitud", "longitud", "estatus")
    
    # Crear la tabla base
    dt <- datatable(
      datos_of %>% select(CNG, tipo_infra, ID_2024, Ent, Mun, cve_mun, nom_mun, nom_infra, latitud, longitud, estatus),
      options = list(
        pageLength = 10,
        lengthMenu = c(5, 10, 15, 20),
        searching = TRUE,
        scrollX = TRUE,
        dom = 'Blfrtip',
        buttons = c('copy', 'csv', 'excel'),
        deferRender = TRUE,
        scroller = TRUE
      ),
      rownames = FALSE,
      selection = "single",
      callback = JS("
      table.on('dblclick', 'tr', function() {
        var rowData = table.row(this).data();
        var rowIndex = table.row(this).index();
        Shiny.setInputValue('tabla_oficina_row_dblclick', rowIndex + 1, {priority: 'event'});
        // También enviamos los datos de la fila para depuración
        Shiny.setInputValue('tabla_oficina_row_data', rowData, {priority: 'event'});
      });
    "),
    caption = "Haga doble clic en una fila para editar"
    )
    
    # Colorear celdas con diferencias
    for (i in 1:nrow(datos_of)) {
      # Buscar datos correspondientes en el censo
      fila_censo <- datos_cen %>% filter(CNG == datos_of$CNG[i])
      
      if (nrow(fila_censo) > 0) {
        for (col in columnas_comparar) {
          # Comparar valores
          valor_oficina <- datos_of[[col]][i]
          valor_censo <- fila_censo[[col]][1]
          
          # Si son diferentes, colorear la celda
          if (!identical(valor_oficina, valor_censo)) {
            dt <- dt %>% formatStyle(
              col,
              target = "cell",
              backgroundColor = styleRow(i, "#FFCCCC") # Color rojo claro para diferencias
            )
          }
        }
      } else {
        # Si no hay datos en el censo, colorear toda la fila
        for (col in columnas_comparar) {
          dt <- dt %>% formatStyle(
            col,
            target = "cell",
            backgroundColor = styleRow(i, "#FFFFCC") # Color amarillo claro para filas sin datos en censo
          )
        }
      }
    }
    
    dt
  })
  
  # Registro seleccionado para edición
  selected_row <- reactiveVal(NULL)
  
  save_status <- reactiveVal("not_started") # Valores posibles: "not_started", "in_progress", "completed"
  save_results <- reactiveVal(NULL) # Para almacenar los resultados de la corrección
  
  # Detectar doble clic en la tabla de oficina
  observeEvent(input$tabla_oficina_row_dblclick, {
    # Guardar el índice de la fila seleccionada
    fila_seleccionada <- input$tabla_oficina_row_dblclick
    
    # Imprimir para depuración
    cat("Fila seleccionada por doble clic:", fila_seleccionada, "\n")
    
    # Actualizar el reactiveVal
    selected_row(fila_seleccionada)
    
    # Cambiar a la pestaña de edición
    updateTabsetPanel(session, "navset", selected = "Corregir Registro")
  }, ignoreNULL = TRUE, priority = 10)
  
  # Panel de edición--------------------------------------------------------------------------------

  output$edicion_panel <- renderUI({
    # Verificar el estado de guardado
    if(save_status() == "in_progress") {
      return(card(
        card_header("Guardando corrección"),
        div(
          style = "text-align: center; padding: 20px;",
          div(class = "progress",
              div(class = "progress-bar progress-bar-striped progress-bar-animated", 
                  role = "progressbar", 
                  style = "width: 100%",
                  "Guardando información...")
          ),
          p("Por favor espere mientras se guardan los cambios...")
        )
      ))
    }
    
    # Mostrar resultados después de guardar
    if(save_status() == "completed" && !is.null(save_results())) {
      resultados <- save_results()
      
      return(card(
        card_header("Corrección guardada exitosamente"),
        div(
          style = "padding: 15px;",
          p(strong("ID:"), resultados$id),
          p(strong("Archivo actualizado:"), resultados$nombre_archivo),
          p(strong("Fecha de actualización:"), resultados$fecha),
          hr(),
          h4("Campos actualizados:"),
          
          # Tabla de cambios
          tags$table(class = "table table-bordered",
                     tags$thead(
                       tags$tr(
                         tags$th("Campo"),
                         tags$th("Valor original"),
                         tags$th("Valor nuevo")
                       )
                     ),
                     tags$tbody(
                       tags$tr(
                         tags$td("Clave Municipio"),
                         tags$td(resultados$campos_actualizados$cve_mun$original),
                         tags$td(resultados$campos_actualizados$cve_mun$nuevo)
                       ),
                       tags$tr(
                         tags$td("Nombre Municipio"),
                         tags$td(resultados$campos_actualizados$nom_mun$original),
                         tags$td(resultados$campos_actualizados$nom_mun$nuevo)
                       ),
                       tags$tr(
                         tags$td("Nombre Infraestructura"),
                         tags$td(resultados$campos_actualizados$nom_infra$original),
                         tags$td(resultados$campos_actualizados$nom_infra$nuevo)
                       ),
                       tags$tr(
                         tags$td("Latitud"),
                         tags$td(resultados$campos_actualizados$latitud$original),
                         tags$td(resultados$campos_actualizados$latitud$nuevo)
                       ),
                       tags$tr(
                         tags$td("Longitud"),
                         tags$td(resultados$campos_actualizados$longitud$original),
                         tags$td(resultados$campos_actualizados$longitud$nuevo)
                       ),
                       tags$tr(
                         tags$td("Estatus"),
                         tags$td(resultados$campos_actualizados$estatus$original),
                         tags$td(resultados$campos_actualizados$estatus$nuevo)
                       )
                     )
          ),
          
          div(style = "text-align: center; margin-top: 20px;",
              actionButton("regresar_principal", "Regresar a la página principal", class = "btn-primary btn-lg")
          )
        )
      ))
    }
    
    # Verificar que tenemos un índice de fila válido
    if(is.null(selected_row())) {
      return(card(
        card_header("Edición"),
        "Seleccione un registro para editar haciendo doble clic en la tabla de inconsistencias."
      ))
    }
    
    # Obtener datos de la fila seleccionada
    datos_of <- datos_filtrados()
    
    # Verificar que la fila existe
    row_idx <- selected_row()
    cat("Usando fila índice:", row_idx, "de", nrow(datos_of), "filas totales\n")
    
    if(row_idx > nrow(datos_of) || row_idx < 1) {
      return(card(
        card_header("Error"),
        "El índice de fila seleccionado no es válido. Por favor, vuelva a la tabla y seleccione una fila."
      ))
    }
    
    # Extraer datos de la fila seleccionada
    fila_of <- datos_of[row_idx, ]
    
    # Buscar datos correspondientes en censo usando el CNG específico
    cng_seleccionado <- fila_of$ID_2024
    
    # Imprimir para depuración
    print(paste("CNG seleccionado:", cng_seleccionado))
    
    # Buscar directamente en los datos originales del censo
    fila_cen <- datos()$censo %>% 
      filter(ID_2024 == cng_seleccionado)
    
    # Si se encuentran múltiples filas, tomar solo la primera
    if (nrow(fila_cen) > 0) {
      fila_cen <- fila_cen[1, ]
    } else {
      # Si no se encuentra ninguna fila, crear una con valores NA
      fila_cen <- data.frame(
        cve_mun = NA, nom_mun = NA, nom_infra = NA,
        latitud = NA, longitud = NA, estatus = NA
      )
    }
    
    # Función para comparar valores y aplicar color
    compare_value <- function(val1, val2, is_na_handling = TRUE) {
      # Comparar teniendo en cuenta valores NA
      if (is_na_handling && (is.na(val1) || is.na(val2))) {
        return(tags$span(style = "color: red; font-weight: bold;", 
                         if(is.na(val1)) "No disponible" else val1))
      } else if (identical(val1, val2)) {
        return(tags$span(style = "color: green; font-weight: bold;", val1))
      } else {
        return(tags$span(style = "color: red; font-weight: bold;", val1))
      }
    }
    
    # UI para edición
    layout_column_wrap(
      width = "250px",
      card(
        card_header("Información del registro"),
        p(strong("CNG:"), fila_of$CNG),
        p(strong("Tipo infraestructura:"), fila_of$tipo_infra),
        p(strong("ID 2024:"), fila_of$ID_2024),
        p(strong("Entidad:"), fila_of$Ent),
        p(strong("Municipio:"), fila_of$Mun)
      ),
      card(
        card_header("Datos Oficina Central"),
        p(strong("Clave Municipio:"), compare_value(fila_of$cve_mun, fila_cen$cve_mun)),
        p(strong("Nombre Municipio:"), compare_value(fila_of$nom_mun, fila_cen$nom_mun)),
        p(strong("Nombre Infraestructura:"), compare_value(fila_of$nom_infra, fila_cen$nom_infra)),
        p(strong("Latitud:"), compare_value(fila_of$latitud, fila_cen$latitud)),
        p(strong("Longitud:"), compare_value(fila_of$longitud, fila_cen$longitud)),
        p(strong("Estatus:"), compare_value(fila_of$estatus, fila_cen$estatus))
      ),
      card(
        card_header(paste0("Datos CENSO (CNG: ", cng_seleccionado, ")")),
        p(strong("Clave Municipio:"), compare_value(fila_cen$cve_mun, fila_of$cve_mun)),
        p(strong("Nombre Municipio:"), compare_value(fila_cen$nom_mun, fila_of$nom_mun)),
        p(strong("Nombre Infraestructura:"), compare_value(fila_cen$nom_infra, fila_of$nom_infra)),
        p(strong("Latitud:"), compare_value(fila_cen$latitud, fila_of$latitud)),
        p(strong("Longitud:"), compare_value(fila_cen$longitud, fila_of$longitud)),
        p(strong("Estatus:"), compare_value(fila_cen$estatus, fila_of$estatus))
      ),
      # En el renderUI, usar esta variable para mostrar la card
      if (valores$mostrar_card_corregida) {
        card(
          # Tu card de información corregida...
          card(
            card_header("Información Corregida"),
            p(strong("Clave Municipio:"), valores$registro_corregido$cve_mun),
            p(strong("Nombre Municipio:"), valores$registro_corregido$nom_mun),
            p(strong("Nombre Infraestructura:"), valores$registro_corregido$nom_infra),
            p(strong("Latitud:"), valores$registro_corregido$latitud),
            p(strong("Longitud:"), valores$registro_corregido$longitud),
            p(strong("Estatus:"), valores$registro_corregido$estatus),
            hr(),
            div(style = "font-style: italic; color: green; font-weight: bold;",
                "✓ Información guardada correctamente")
          )
        )
      } else {
        NULL
      },
      card(
        card_header("Corregir inconsistencia"),
        # Campos de entrada para la corrección
        numericInput("correccion_cve_mun", "Clave Municipio:", value = fila_of$cve_mun),
        textInput("correccion_nom_mun", "Nombre Municipio:", value = fila_of$nom_mun),
        textInput("correccion_nom_infra", "Nombre Infraestructura:", value = fila_of$nom_infra),
        numericInput("correccion_latitud", "Latitud:", value = fila_of$latitud, max = 50),
        numericInput("correccion_longitud", "Longitud:", value = fila_of$longitud),
        selectInput("correccion_estatus", "Estatus:", 
                    choices = c("ACTIVO", "INACTIVO"),
                    selected = fila_of$estatus),
        
        # Botones de acción
        layout_column_wrap(
          width = 1/2,
          actionButton("guardar_correccion", "Guardar", class = "btn-primary"),
          actionButton("cancelar_correccion", "Cancelar", class = "btn-secondary"),
          actionButton("regresar_principal", "Regresar", class = "btn-secondary")
        )
      )
    )
  })
  
  # Guardar corrección------------------------------------------------------------------------------
  observeEvent(input$guardar_correccion, {
    
    # Cambiar estado a "en progreso" ANTES de cualquier operación
    save_status("in_progress")
    
    # Guardar los datos corregidos en valores reactivos
    valores$registro_corregido <- list(
      cve_mun = input$correccion_cve_mun,
      nom_mun = input$correccion_nom_mun,
      nom_infra = input$correccion_nom_infra,
      latitud = input$correccion_latitud,
      longitud = input$correccion_longitud,
      estatus = input$correccion_estatus
    )
    
    valores$mostrar_card_corregida <- TRUE
    
    req(selected_row())
    
    # Obtener datos originales
    row_idx <- selected_row()
    datos_of <- datos_filtrados()
    fila_of <- datos_of[row_idx, ]
    
    # Crear registro de corrección
    nueva_correccion <- data.frame(
      CNG = fila_of$CNG,
      tipo_infra = fila_of$tipo_infra,
      ID_2024 = fila_of$ID_2024,
      Ent = fila_of$Ent,
      Mun = fila_of$Mun,
      
      cve_mun_original = fila_of$cve_mun,
      nom_mun_original = fila_of$nom_mun,
      nom_infra_original = fila_of$nom_infra,
      latitud_original = fila_of$latitud,
      longitud_original = fila_of$longitud,
      estatus_original = fila_of$estatus,
      
      cve_mun_corregido = input$correccion_cve_mun,
      nom_mun_corregido = input$correccion_nom_mun,
      nom_infra_corregido = input$correccion_nom_infra,
      latitud_corregido = input$correccion_latitud,
      longitud_corregido = input$correccion_longitud,
      estatus_corregido = input$correccion_estatus,
      
      fecha_correccion = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      usuario = "usuario_app"
    )
    
    # Ejecutar el proceso de guardado usando withProgress
    withProgress(message = 'Guardando corrección', value = 0, {
      
      # Agregar a la base de datos de correcciones
      datos_corregidos <<- rbind(datos_corregidos, nueva_correccion)
      
      # Guardar en archivo
      tryCatch({
        
        # Crear directorio si no existe
        if (!dir.exists("correcciones")) {
          dir.create("correcciones")
        }
        
        # Nombre del archivo con fecha
        nombre_archivo <- paste0("correcciones/correcciones_", 
                                 format(Sys.Date(), "%Y-%m-%d"), ".json")
        
        incProgress(0.2, detail = "Guardando en JSON...")
        
        # Guardar en formato JSON
        write_json(datos_corregidos, nombre_archivo, pretty = TRUE)
        
        incProgress(0.2, detail = "Actualizando base de datos...")
        
        tryCatch({
          # Crear directorio results si no existe
          if (!dir.exists("results")) {
            dir.create("results")
          }
          
          # Cargar DB_infraestructuras.xlsx
          db_path <- paste0(RUTA_DATABASE, "DB_infraestructuras.xlsx")
          db_infra <- openxlsx::read.xlsx(db_path)
          
          incProgress(0.2, detail = "Actualizando registros...")
          
          # Buscar la fila con el ID_2024 correspondiente
          idx <- which(db_infra$ID_INEGI_2024 == fila_of$ID_2024)
          
          if (length(idx) > 0) {
            # Actualizar los valores en la fila correspondiente
            db_infra$ID_INEGI_2025[idx] <- fila_of$ID_2024
            db_infra$cve_mun[idx] <- input$correccion_cve_mun
            db_infra$nom_mun[idx] <- input$correccion_nom_mun
            db_infra$nom_infraestructura[idx] <- input$correccion_nom_infra
            db_infra$latitud[idx] <- input$correccion_latitud
            db_infra$longitud[idx] <- input$correccion_longitud
            db_infra$estatus[idx] <- input$correccion_estatus
            
            # Guardar el archivo actualizado con la fecha
            fecha_archivo <- format(Sys.Date(), "%Y-%m-%d")
            output_path <- paste0("results/DB_infraestructuras_", fecha_archivo, ".xlsx")
            
            openxlsx::write.xlsx(db_infra, output_path)
            
            incProgress(0.2, detail = "Completando...")
            
            showNotification(
              paste("Base de datos actualizada correctamente y guardada en:", output_path), 
              type = "message"
            )
          } else {
            showNotification(
              paste("No se encontró el ID_2024:", fila_of$ID_2024, "en DB_infraestructuras.xlsx"), 
              type = "warning"
            )
          }
        }, error = function(e) {
          showNotification(
            paste("Error al actualizar DB_infraestructuras:", e$message), 
            type = "error", 
            duration = NULL
          )
        })
        
        # Cambiar estado a "completado"
        save_status("completed")
        
        showNotification("Corrección guardada exitosamente", type = "message")
        
        # AÑADIR ESTE CÓDIGO PARA LIMPIAR LOS CAMPOS DE ENTRADA:
        updateNumericInput(session, "correccion_cve_mun", value = NA)
        updateTextInput(session, "correccion_nom_mun", value = "")
        updateTextInput(session, "correccion_nom_infra", value = "")
        updateNumericInput(session, "correccion_latitud", value = NA)
        updateNumericInput(session, "correccion_longitud", value = NA)
        updateSelectInput(session, "correccion_estatus", selected = character(0))
        
        
      }, error = function(e) {
        showNotification(paste("Error al guardar corrección:", e$message), 
                         type = "error", duration = NULL)
        
        # Restablecer estado en caso de error
        save_status("not_started")
        
      })
    })
    
    
    
    
    
  })
  
  # Cancelar corrección ----------------------------------------------------------------------------
  observeEvent(input$cancelar_correccion, {
    valores$mostrar_card_corregida <- FALSE
    updateTabsetPanel(session, "navset", selected = "Inconsistencias")
    selected_row(NULL)
  })
  
  # Regresar principal ----------------------------------------------------------------------------
  observeEvent(input$regresar_principal, {
    updateTabsetPanel(session, "navset", selected = "Inconsistencias")
    selected_row(NULL)
  })
}

shinyApp(ui, server)