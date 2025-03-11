library(shiny)
library(DT)
library(bslib)
library(dplyr)
library(jsonlite)
library(openxlsx)

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

names(datos_inconsistencias)
unique(datos_inconsistencias$ID_2024)

datos_oficina <- datos_inconsistencias %>%
  filter(BASE == "Dashboard ID") %>%
  select(-BASE)

datos_censo <- datos_inconsistencias %>%
  filter(BASE == "CENSO") %>%
  select(-BASE)

# Base de datos para guardar modificaciones
datos_corregidos <- data.frame()

ui <- page_fluid(
  theme = bs_theme(bootswatch = "flatly"),
  
  navset_card_pill(
    id = "navset",
    nav_panel(
      title = "Inconsistencias",
      layout_sidebar(
        sidebar = sidebar(
          title = "Filtros",
          width = 300,
          selectizeInput("CNG", "CNG:", choices = unique(datos_oficina$CNG), multiple = TRUE),
          selectizeInput("tipo_infra", "Tipo Infraestructura:", choices = unique(datos_oficina$tipo_infra), multiple = TRUE),
          selectizeInput("ID_2024", "ID INEGI 2024:", choices = unique(datos_oficina$ID_2024), multiple = TRUE),
          selectizeInput("Ent", "Entidad:", choices = unique(datos_oficina$Ent), multiple = TRUE),
          selectizeInput("Mun", "Municipio:", choices = unique(datos_oficina$Mun), multiple = TRUE),
          actionButton("reset", "Limpiar filtros", class = "btn-secondary")
        ),
        card(
          card_header("Información Oficina Central"),
          tableOutput("tabla_oficina"),
          tags$script(HTML("
            $(document).on('dblclick', '#tabla_oficina tr', function() {
              var rowIndex = $(this).index();
              Shiny.setInputValue('tabla_oficina_row_dblclick', rowIndex + 1);
            });
          "))
        ),
        card(
          card_header("Información CENSO"),
          tableOutput("tabla_censo"),
          tags$script(HTML("
            $(document).on('dblclick', '#tabla_censo tr', function() {
              var rowIndex = $(this).index();
              Shiny.setInputValue('tabla_censo_row_dblclick', rowIndex + 1);
            });
          "))
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
  
  # Datos filtrados
  datos_filtrados <- reactive({
    datos <- datos_oficina
    
    if (!is.null(input$CNG) && length(input$CNG) > 0) {
      datos <- datos %>% filter(CNG %in% input$CNG)
    }
    if (!is.null(input$tipo_infra) && length(input$tipo_infra) > 0) {
      datos <- datos %>% filter(tipo_infra %in% input$tipo_infra)
    }
    if (!is.null(input$ID_2024) && length(input$ID_2024) > 0) {
      datos <- datos %>% filter(ID_2024 %in% input$ID_2024)
    }
    if (!is.null(input$Ent) && length(input$Ent) > 0) {
      datos <- datos %>% filter(Ent %in% input$Ent)
    }
    if (!is.null(input$Mun) && length(input$Mun) > 0) {
      datos <- datos %>% filter(Mun %in% input$Mun)
    }
    
    return(datos)
  })
  
  datos_censo_filtrados <- reactive({
    ids <- datos_filtrados()$CNG
    return(datos_censo %>% filter(CNG %in% ids))
  })
  
  # Actualizar los filtros dinámicamente
  observe({
    filtered_data <- datos_filtrados()
    
    updateSelectizeInput(session, "CNG", choices = unique(datos_oficina$CNG), 
                         selected = input$CNG)
    updateSelectizeInput(session, "tipo_infra", choices = unique(datos_oficina$tipo_infra), 
                         selected = input$tipo_infra)
    updateSelectizeInput(session, "ID_2024", choices = unique(datos_oficina$ID_2024), 
                         selected = input$ID_2024)
    updateSelectizeInput(session, "Ent", choices = unique(datos_oficina$Ent), 
                         selected = input$Ent)
    updateSelectizeInput(session, "Mun", choices = unique(datos_oficina$Mun), 
                         selected = input$Mun)
  })
  
  # Botón de resetear filtros
  observeEvent(input$reset, {
    updateSelectizeInput(session, "CNG", selected = character(0))
    updateSelectizeInput(session, "tipo_infra", selected = character(0))
    updateSelectizeInput(session, "ID_2024", selected = character(0))
    updateSelectizeInput(session, "Ent", selected = character(0))
    updateSelectizeInput(session, "Mun", selected = character(0))
  })
  
  # Función para colorear la tabla
  highlight_diferencias <- function(data_oficina, data_censo) {
    mapped_data <- data_oficina
    
    for (i in 1:nrow(data_oficina)) {
      for (j in 1:ncol(data_oficina)) {
        if (i <= nrow(data_censo) && j <= ncol(data_censo)) {
          # Usar identical() o all.equal() para manejar NAs correctamente
          if (is.na(data_oficina[i, j]) && is.na(data_censo[i, j])) {
            # Ambos son NA, considerarlos iguales
            mapped_data[i, j] <- sprintf('<span style="background-color: #ccffcc;">%s</span>', "NA")
          } else if (is.na(data_oficina[i, j]) || is.na(data_censo[i, j])) {
            # Uno es NA pero el otro no, considerarlos diferentes
            val_display <- if(is.na(data_oficina[i, j])) "NA" else data_oficina[i, j]
            mapped_data[i, j] <- sprintf('<span style="background-color: #ffcccc;">%s</span>', val_display)
          } else if (isTRUE(all.equal(data_oficina[i, j], data_censo[i, j]))) {
            # Ambos tienen valores y son iguales
            mapped_data[i, j] <- sprintf('<span style="background-color: #ccffcc;">%s</span>', data_oficina[i, j])
          } else {
            # Ambos tienen valores pero son diferentes
            mapped_data[i, j] <- sprintf('<span style="background-color: #ffcccc;">%s</span>', data_oficina[i, j])
          }
        }
      }
    }
    
    return(mapped_data)
  }
  
  # Tabla Oficina
  output$tabla_oficina <- renderTable({
    data <- datos_filtrados() %>%
      select(cve_mun, nom_mun, nom_infra, latitud, longitud, estatus)
    
    if (nrow(data) == 0) {
      return(data.frame(Mensaje = "No hay datos disponibles"))
    }
    
    # Obtener datos censo para comparación
    data_censo <- datos_censo_filtrados() %>%
      select(cve_mun, nom_mun, nom_infra, latitud, longitud, estatus)
    
    highlight_diferencias(data, data_censo)
  }, sanitize.text.function = function(x) x, bordered = TRUE, hover = TRUE, width = "100%")
  
  # Tabla Censo
  output$tabla_censo <- renderTable({
    data <- datos_censo_filtrados() %>%
      select(cve_mun, nom_mun, nom_infra, latitud, longitud, estatus)
    
    if (nrow(data) == 0) {
      return(data.frame(Mensaje = "No hay datos disponibles"))
    }
    
    # Obtener datos oficina para comparación
    data_oficina <- datos_filtrados() %>%
      select(cve_mun, nom_mun, nom_infra, latitud, longitud, estatus)
    
    highlight_diferencias(data, data_oficina)
  }, sanitize.text.function = function(x) x, bordered = TRUE, hover = TRUE, width = "100%")
  
  # Detectar doble clic en la tabla
  registro_seleccionado <- reactiveVal(NULL)
  
  # Manejar doble clic en tabla oficina
  observeEvent(input$tabla_oficina_row_dblclick, {
    row <- input$tabla_oficina_row_dblclick
    if (!is.null(row) && row > 0 && row <= nrow(datos_filtrados())) {
      oficina_row <- datos_filtrados()[row, ]
      censo_row <- datos_censo_filtrados()[row, ]
      
      # Guardar en reactivo para usar en el panel
      registro_seleccionado(list(
        oficina = oficina_row,
        censo = censo_row,
        indice = row,
        fuente = "oficina"
      ))
      
      # Cambiar a la pestaña de edición
      updateTabsetPanel(session, inputId = "navset", selected = "Corregir Registro")
    }
  })
  
  # Manejar doble clic en tabla censo
  observeEvent(input$tabla_censo_row_dblclick, {
    row <- input$tabla_censo_row_dblclick
    if (!is.null(row) && row > 0 && row <= nrow(datos_censo_filtrados())) {
      oficina_row <- datos_filtrados()[row, ]
      censo_row <- datos_censo_filtrados()[row, ]
      
      # Guardar en reactivo para usar en el panel
      registro_seleccionado(list(
        oficina = oficina_row,
        censo = censo_row,
        indice = row,
        fuente = "censo"
      ))
      
      # Cambiar a la pestaña de edición
      updateTabsetPanel(session, inputId = "navset", selected = "Corregir Registro")
    }
  })
  
  # Panel de edición
  output$edicion_panel <- renderUI({
    reg <- registro_seleccionado()
    
    if (is.null(reg)) {
      return(card(
        card_header("Editar Registro"),
        p("Seleccione un registro con doble clic en la tabla para editarlo")
      ))
    }
    
    # Detectar diferencias
    diferencias <- c()
    campos_comparar <- c("cve_mun", "nom_mun", "nom_infra", "latitud", "longitud", "estatus")
    
    for (campo in campos_comparar) {
      if (reg$oficina[[campo]] != reg$censo[[campo]]) {
        diferencias <- c(diferencias, campo)
      }
    }
    
    # Crear los inputs para la edición
    inputs_list <- list()
    
    for (campo in campos_comparar) {
      es_diferente <- campo %in% diferencias
      
      # Para los campos numéricos
      if (campo %in% c("latitud", "longitud")) {
        inputs_list[[campo]] <- list(
          card(
            card_header(campo),
            layout_column_wrap(
              width = "100%",
              value_box(
                title = "Oficina Central",
                value = reg$oficina[[campo]],
                showcase = bsicons::bs_icon("building"),
                theme = if (es_diferente) "danger" else "success"
              ),
              value_box(
                title = "CENSO",
                value = reg$censo[[campo]],
                showcase = bsicons::bs_icon("clipboard2-data"),
                theme = if (es_diferente) "danger" else "success"
              )
            ),
            numericInput(
              paste0("edit_", campo),
              "Valor corregido:",
              value = reg$oficina[[campo]],
              width = "100%"
            ) %>% tagAppendAttributes(disabled = !es_diferente)
          )
        )
      } else if (campo == "estatus") {
        # Para el campo estatus (dropdown)
        inputs_list[[campo]] <- list(
          card(
            card_header(campo),
            layout_column_wrap(
              width = "100%",
              value_box(
                title = "Oficina Central",
                value = reg$oficina[[campo]],
                showcase = bsicons::bs_icon("building"),
                theme = if (es_diferente) "danger" else "success"
              ),
              value_box(
                title = "CENSO",
                value = reg$censo[[campo]],
                showcase = bsicons::bs_icon("clipboard2-data"),
                theme = if (es_diferente) "danger" else "success"
              )
            ),
            selectInput(
              paste0("edit_", campo),
              "Valor corregido:",
              choices = c("Activo", "Inactivo"),
              selected = reg$oficina[[campo]],
              width = "100%"
            ) %>% tagAppendAttributes(disabled = !es_diferente)
          )
        )
      } else {
        # Para los campos de texto
        inputs_list[[campo]] <- list(
          card(
            card_header(campo),
            layout_column_wrap(
              width = "100%",
              value_box(
                title = "Oficina Central",
                value = reg$oficina[[campo]],
                showcase = bsicons::bs_icon("building"),
                theme = if (es_diferente) "danger" else "success"
              ),
              value_box(
                title = "CENSO",
                value = reg$censo[[campo]],
                showcase = bsicons::bs_icon("clipboard2-data"),
                theme = if (es_diferente) "danger" else "success"
              )
            ),
            textInput(
              paste0("edit_", campo),
              "Valor corregido:",
              value = reg$oficina[[campo]],
              width = "100%"
            ) %>% tagAppendAttributes(disabled = !es_diferente)
          )
        )
      }
    }
    
    # Construir la interfaz de edición
    registro_info <- card(
      card_header("Información del registro"),
      layout_column_wrap(
        width = 1/3,
        value_box(
          title = "CNG",
          value = reg$oficina$CNG,
          showcase = bsicons::bs_icon("upc")
        ),
        value_box(
          title = "Tipo Infraestructura",
          value = reg$oficina$tipo_infra,
          showcase = bsicons::bs_icon("buildings")
        ),
        value_box(
          title = "ID 2024",
          value = reg$oficina$ID_2024,
          showcase = bsicons::bs_icon("fingerprint")
        )
      )
    )
    
    campos_edit <- layout_column_wrap(
      width = 1/2,
      !!!inputs_list
    )
    
    botones <- layout_column_wrap(
      width = 1/2,
      actionButton("guardar", "Guardar cambios", class = "btn-success", width = "100%"),
      actionButton("cancelar", "Cancelar", class = "btn-secondary", width = "100%")
    )
    
    return(
      card(
        card_header("Editar Registro"),
        registro_info,
        campos_edit,
        botones
      )
    )
  })
  
  # Manejar acción de guardar
  observeEvent(input$guardar, {
    reg <- registro_seleccionado()
    
    if (!is.null(reg)) {
      # Crear registro corregido
      registro_corregido <- reg$oficina
      
      # Actualizar valores
      campos_comparar <- c("cve_mun", "nom_mun", "nom_infra", "latitud", "longitud", "estatus")
      for (campo in campos_comparar) {
        input_id <- paste0("edit_", campo)
        if (!is.null(input[[input_id]])) {
          registro_corregido[[campo]] <- input[[input_id]]
        }
      }
      
      # En una aplicación real, aquí guardarías en la base de datos
      # Por ahora, solo añadimos a la tabla de registros corregidos
      datos_corregidos <<- rbind(datos_corregidos, registro_corregido)
      
      # Mensaje de éxito
      showModal(modalDialog(
        title = "Corrección Guardada",
        "Los cambios han sido guardados correctamente.",
        easyClose = TRUE,
        footer = actionButton("ok_modal", "Aceptar", class = "btn-primary")
      ))
      
      # Volver a la página principal al cerrar el modal
      observeEvent(input$ok_modal, {
        removeModal()
        updateTabsetPanel(session, "navset", selected = "Inconsistencias")
        registro_seleccionado(NULL)
      }, once = TRUE)
    }
  })
  
  # Manejar acción de cancelar
  observeEvent(input$cancelar, {
    updateTabsetPanel(session, "navset", selected = "Inconsistencias")
    registro_seleccionado(NULL)
  })
}

shinyApp(ui, server)