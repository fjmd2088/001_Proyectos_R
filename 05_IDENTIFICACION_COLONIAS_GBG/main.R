
# BUENAS PRACTICAS
# 1. Para data frame y variables usamos: snake_case
# 2. Manten un maximo de 80 a 100 caracteres por linea (para evitar desplazamiento horizontal)
# 3. Cargamos las librerias solamente necesarias
# 4. Constantes: En mayúsculas
# 5. Dataframe: Agregar prefijos cuando sea necesario
#    a) raw: Datos en bruto sin procesar
#    b) clean: Datos ya limpios
#    c) agg: Datos agregados o resumidos
#    d) temp: Data frame temporal

#----------------------------------------PRELIMINARES-----------------------------------------------
remove(list = ls())

# LIBRERIAS
library(readxl)
library(stringr)
library(stringi)
library(dplyr)
library(tidyr)
library(ggplot2)
library(officer)
library(flextable)
library(forcats)  # Para manipulación de factores
library(magick)

# DIRECTORIOS
DIRECTORIO_CATALOGOS <- "catalogos/"
DIRECTORIO_BD <-"bd/" 
# DIRECTORIO_IMG <- "img/"
# DIRECTORIO_PPT <- "ppt/"

DIRECTORIO_IMG <- "img/"
DIRECTORIO_PPT <- "ppt/"
logo_path <- paste0(DIRECTORIO_IMG, "logo.png")
grafico_path <- paste0(DIRECTORIO_IMG, "grafico1.png")
archivo_ppt <- paste0(DIRECTORIO_PPT, "Reporte_mtcars.pptx")

# VARIABLES
NOMBRE_CATALOGO_CP <- "CodigosPostales_CDMX_modificado"
NOMBRE_REPORTE <- "OpenOrderReport30012025"
# NOMBRE_COLONIAS <- "Colonias"

# CARGA CATALOGOS 
codigo_postal_cdmx <- read_excel(paste0(DIRECTORIO_CATALOGOS,NOMBRE_CATALOGO_CP,".xlsx"))
# colonias <- read.csv(paste0(DIRECTORIO_CATALOGOS,NOMBRE_COLONIAS,".csv"))

# CARGA BASE DE DATOS
raw_reporte_entrega_domicilio <- read.csv(paste0(DIRECTORIO_BD,NOMBRE_REPORTE,".csv"))

# Leer la imagen y obtener dimensiones originales
img <- image_read(logo_path)
img_info <- image_info(img)
original_width <- img_info$width
original_height <- img_info$height

# Escalar la imagen al 50%
scale_factor <- 0.5
scaled_width <- original_width * scale_factor / 96  # Convertir a pulgadas
scaled_height <- original_height * scale_factor / 96  # Convertir a pulgadas
#---------------------------------------ANALISIS----------------------------------------------------

# mod_colonias <- colonias %>%
#   rename(colonia = Colonia) %>%
#   mutate(colonia = gsub("NEWLINE","",stri_trans_general(str_to_upper(colonia),
#                                                         "Latin-ASCII")),
#          colonia = str_trim(colonia),
#          indicador = 1)

# se obtiene la base de cp que se necesita para el analisis
mod_cp_cdmx <- codigo_postal_cdmx %>%
  rename(cp = "Código Postal",
         colonia = "Asentamiento") %>%
  select(cp,Municipio,colonia) %>%
  mutate(colonia = stri_trans_general(str_to_upper(colonia),"Latin-ASCII")
         )

# se obtiene la base modificada 
mod_reporte_entrega_domicilio <- raw_reporte_entrega_domicilio %>%
  select(Address) %>% # se selecciona la columna de interes
  mutate(Adddress_original = Address,
         Address = str_to_upper(Address)) %>%
  separate(Address, 
           into = c("calle","colonia","cp"), 
           sep = "@", 
           extra = "merge", 
           fill = "right") %>%
  mutate(colonia = gsub("NEWLINE","",stri_trans_general(str_to_upper(colonia),
                                                        "Latin-ASCII")), # se quita patron en colonia
         colonia = gsub("COL","",colonia),
         colonia = str_trim(colonia),
         cp = gsub("\\D","",cp), # se extrae solo los numeros del cp
         cp = as.numeric(cp))

# Se extrae informacion donde el campo colonia es "" o NA
informacion_sin_colonia <- mod_reporte_entrega_domicilio %>% filter(is.na(colonia) | colonia == "")

# Se extrae la informacion que si tiene contenido
informacion_con_colonia <- mod_reporte_entrega_domicilio %>% filter(!is.na(colonia) & colonia != "")


# se sustituyen patrones de colonias mal escritas que han sido identificadas

# Definir las variantes incorrectas y su corrección
# reemplazos <- c(
#   "PKRTALES SUR" = "PORTALES SUR",
#   "PORTALES SU"  = "PORTALES SUR",
#   "PORTAES SUR"  = "PORTALES SUR",
#   "PIRTALES SUR" = "PORTALES SUR",
#   "INDENDENCIA" = "INDEPENDENCIA",
#   "INDEPENCIA" = "INDEPENDENCIA",
#   "INDEPENDECIA" = "INDEPENDENCIA",
#   "INDEPENDENCA" = "INDEPENDENCIA",
#   "INDEPENDNCIA" = "INDEPENDENCIA",
#   "INDPEND4NCIA" = "INDEPENDENCIA",
#   "SANTA CRUZ ATEYOC" = "SANTA CRUZ ATOYAC",
#   "STA CRUS ATOYAC" = "SANTA CRUZ ATOYAC",
#   "STA CRUZ  ATOYAC" = "SANTA CRUZ ATOYAC",
#   "STA CRUZ ATOYAC" = "SANTA CRUZ ATOYAC",
#   "STA. CRUZ ATOYAC" = "SANTA CRUZ ATOYAC",
#   "STA. CRUZ ATOYAC" = "SANTA CRUZ ATOYAC",
#   "PORTALE NORTE" = "PORTALES NORTE",
#   "PORTALEE NOT" = "PORTALES NORTE",
#   "PORTALES NTE" = "PORTALES NORTE",
#   "PORTALLES NORTE" = "PORTALES NORTE",
#   "PORTARLES NORTE" = "PORTALES NORTE",
#   "POTALES NORTE" = "PORTALES NORTE",
#   "PORTALES N" = "PORTALES NORTE",
#   "PORTALES OTE" = "PORTALES ORIENTE",
#   "PORTALES O" = "PORTALES ORIENTE",
#   "LEAN VALLE" = "LETRAN VALLE",
#   "LELTRAN VALLE" = "LETRAN VALLE",
#   "LETAN VALLE" = "LETRAN VALLE",
#   "LETRAN" = "LETRAN VALLE",
#   "LETRAN DEL VALLE" = "LETRAN VALLE",
#   "LETRAN VALLLE" = "LETRAN VALLE",
#   "LETRANVALLE" = "LETRAN VALLE",
#   "LEYRA VALLE" = "LETRAN VALLE",
#   "LLETRN VALLE" = "LETRAN VALLE",
#   "BELTRAN VALLE" = "LETRAN VALLE",
#   "AMERICA HUNDIDAS" = "AMERICAS UNIDAS",
#   "AMERICA UNIDA" = "AMERICAS UNIDAS",
#   "AMERICAS UNIDOS" = "AMERICAS UNIDAS",
#   "SAN SIMN" = "SAN SIMON TICUMAC",
#   "SAN SIMON" = "SAN SIMON TICUMAC",
#   "SAN SIMON  TICUMAC" = "SAN SIMON TICUMAC",
#   "SAN SIMON TICOMAC" = "SAN SIMON TICUMAC",
#   "SAN SIMON TICUMAN" = "SAN SIMON TICUMAC",
#   "NARVARTE OTE" = "NARVARTE ORIENTE",
#   "NARVARTE VERTIZ" = "VERTIZ NARVARTE",
#   "NARVTE PONIENTE" = "NARVARTE PONIENTE",
#   "NAVARTE PONIENTE" = "NARVARTE PONIENTE",
#   "NARVARTE PNIEBT" = "NARVARTE PONIENTE",
#   "GENERAL ANAYA" = "GENERAL PEDRO MARIA ANAYA",
#   "GENRAL ANAYA" = "GENERAL PEDRO MARIA ANAYA",
#   "GNRL PRDRO MARIA" = "GENERAL PEDRO MARIA ANAYA",
#   "GRAL PEDRO MARIA ANAYA" = "GENERAL PEDRO MARIA ANAYA",
#   "GRAN ANAYA" = "GENERAL PEDRO MARIA ANAYA",
#   "ZACAHUISCO" = "ZACAHUITZCO",
#   "ZACAHUISTCO" = "ZACAHUITZCO" 
# )

# Aplicar la sustitución en la columna "colonia"
# mod_reporte_entrega_domicilio$colonia <- str_replace_all(
#   mod_reporte_entrega_domicilio$colonia,
#   reemplazos
# )


# cruce de informacion entre la informacion del servicio postal mexicano y la base modificada
  cruce_informacion <- left_join(informacion_con_colonia,mod_cp_cdmx,by = c("colonia","cp"))

  # se filtra la informacion que coincide en colonia y codigo postal
  datos_colonia_cp_correcto <- cruce_informacion %>%
    filter(!is.na(Municipio))
  
  # se crea catalogo con las colonias correctas 
  catalogo_colonia_cp_correcta <- datos_colonia_cp_correcto %>%
    select(colonia,cp,Municipio) %>%
    distinct(colonia,.keep_all = TRUE)
  
  # se filtra informacion que no coincide la colonia y cp
  datos_inconsistencias <- cruce_informacion %>%
    filter(is.na(Municipio))
  
  # se detecta los registros donde se ubica la colonia con base al catalogo de colonias correctas
  # COLONIAS_CORRECTAS <- sort(unique(catalogo_colonia_cp_correcta$colonia))
  
  for (i in 1:length(catalogo_colonia_cp_correcta$colonia)){
    # i <- 13
    IND_I <- which(str_detect(datos_inconsistencias$colonia,catalogo_colonia_cp_correcta$colonia[i])==TRUE)

    datos_inconsistencias$colonia[IND_I] <- catalogo_colonia_cp_correcta$colonia[i]
    datos_inconsistencias$Municipio[IND_I] <- catalogo_colonia_cp_correcta$Municipio[i]
    datos_inconsistencias$cp[IND_I] <- catalogo_colonia_cp_correcta$cp[i]
  }
  
  # se filtra la informacion donde la colonia es correcta y se le asigna los valores respectivos
  datos_colonia_correcto <- datos_inconsistencias %>%
    filter(!is.na(Municipio))
  
  
  # se filtra nuevamente las registros donde la colonia es correcta con base al catalogo
  datos_inconsistencias <- datos_inconsistencias %>%
    filter(is.na(Municipio))
  
  
  # concentrado final con la información correcta
  datos_correctos <- rbind(datos_colonia_cp_correcto,datos_colonia_correcto)
  
  # Agrupar y ordenar los datos
  datos_graf1 <- datos_correctos %>%
    group_by(Municipio,colonia) %>%
    summarise(total = n()) %>%
    arrange(desc(total)) %>%
    mutate(colonia = fct_reorder(colonia, -total))  # Ordenar colonias por total
  
  resumen <- datos_graf1
  
  grafico <- ggplot(datos_graf1[1:15,], aes(x = colonia, y = total)) +
    geom_bar(stat = "identity", fill = "blue") +
    geom_text(aes(label = total), vjust = -0.3, color = "black", size = 4) +  # Etiquetas sobre las barras
    labs(title = "Cantidad de entregas por colonia",
         x = "Colonias") +  # Quitamos el título del eje Y
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1),  # Rotar nombres para mejor legibilidad
          axis.text.y = element_blank(),  # Quitar números del eje Y
          axis.ticks.y = element_blank(),  # Quitar las marcas del eje Y
          axis.title.y = element_blank())  # Quitar el título del eje Y
  
  # Guardar el gráfico como imagen
  grafico_path <- paste0(DIRECTORIO_IMG,"grafico1.png")
  ggsave(grafico_path, plot = grafico, width = 6, height = 4, dpi = 300)
  
  # Mostrar gráfico
  print(grafico)
  
  # Crear una presentación con tamaño personalizado (Ejemplo: 16:9 -> 13.33 x 7.5 pulgadas)
  ppt <- read_pptx(paste0(DIRECTORIO_PPT,"plantilla.pptx"))
  
  layout_summary(ppt)
  
  # Verificar los nombres de los placeholders disponibles en la diapositiva "Title Slide"
  layout_info <- layout_properties(ppt, layout = "Diapositiva de título", master = "Tema de Office")
  print(layout_info)  # Imprime los tipos de placeholders disponibles
  
  layout_info <- layout_properties(ppt, layout = "Título y texto vertical", master = "Tema de Office")
  print(layout_info)  # Imprime los tipos de placeholders disponibles
  # Crear la primera diapositiva con título y logo
  ppt <- ppt %>% 
    add_slide( layout = "Diapositiva de título", master = "Tema de Office") %>%
    ph_with(value = "Análisis de mtcars en R", location = ph_location_type(type = "ctrTitle")) %>% 
    ph_with(value = "Reporte generado en RStudio con officer y flextable", 
            location = ph_location_type(type = "subTitle")) %>%
    ph_with(external_img(logo_path),  
            location = ph_location(left = 11, top = 6, width = 2, height = 1))  # Ajusta el tamaño aquí
  
  # Función para agregar diapositivas con logo y texto
  add_slide_with_logo <- function(ppt, title, text) {
    ppt <- ppt %>% add_slide( layout = "Título y texto vertical", master = "Tema de Office") %>%
      ph_with(value = fpar(ftext(title, prop = fp_text(bold = TRUE, font.size = 20))),
              location = ph_location_type(type = "title")) %>%
      ph_with(value = fpar(ftext(text, prop = fp_text(font.size = 14))),
              location = ph_location_type(type = "body")) %>%
      ph_with(external_img(logo_path),  
              location = ph_location(left = 11, top = 6, width = 2, height = 1))
    return(ppt)
  }
  
  # Resumen estadístico
  ppt <-  add_slide_with_logo(ppt, "Resumen Estadístico", "Aquí se muestran estadísticas básicas de las variables más relevantes.") %>%
    ph_with(value = flextable(resumen[1:10,]) %>% autofit(),
            location = ph_location_type(type = "body"))
  
  # Gráfico de datos
  ppt <- add_slide_with_logo(ppt, "Gráfico: Cantidad de entregas", "Este gráfico muestra la cantidad de entregas por colonia.") %>%
    ph_with(value = external_img(grafico_path, height = 4, width = 6), location = ph_location_type(type = "body"))
  
  # Guardar el PowerPoint
  print(ppt, target = archivo_ppt)
  
  # Crear PowerPoint
  # ppt <- read_pptx()
  # 
  # # Verificar los layouts disponibles en la plantilla de PowerPoint
  # layout_info <- layout_properties(ppt)
  # print(layout_info)  # Verifica qué nombres de placeholder están disponibles
  # 
  # # Crear la primera diapositiva con título y logo
  # ppt <- add_slide(ppt, layout = "Title Slide", master = "Office Theme") %>%
  #   ph_with(value = "Análisis de mtcars en R", location = ph_location_type(type = "title")) %>%
  #   ph_with(value = "Reporte generado en RStudio con officer y flextable", location = ph_location_type(type = "subTitle")) %>%
  #   ph_with(external_img(logo_path, height = 1, width = 2), location = ph_location(left = 0.2, top = 0.2))
  # 
  
  # 
 # 
  # 
  # # Guardar el PowerPoint
  # print(ppt, target = archivo_ppt)
  
  cat("El reporte ha sido generado y guardado como '", archivo_ppt, "'")
  
  
  # unique(datos_inconsistencias$Municipio)
 
  
  # *???????????????????????????????????????????????????????????????????
  # se filtra informacion con las siguientes caracteristicas:
  # vacio, 0, NA
  
  
   # %>%
  #   filter(!is.na(colonia) & colonia != "")
  # 
  # sort(unique(informacion_inconsistencias$colonia))
  # sort(unique(informacion_inconsistencias$cp))
  

  
  # informacion_inconsistencias_2 <- informacion_inconsistencias %>%
  #   mutate(indicador = 2)
  # 
  # cruce_2 <- left_join(informacion_inconsistencias_2,catalogo_inf_real,b = "colonia")
  # 
  # informacion_real_2 <- cruce_2 %>%
  #   filter(!is.na(cp.y))
  #          
  # informacion_inconsistencias_3 <- cruce_2 %>%
  #   filter(is.na(cp.y)) %>%
  #   select(-cp.y) 
  # 
  
  # 
  # informacion_real_3 <- informacion_inconsistencias_3 %>%
  #   filter(indicador == 3)
  # 
  # inconsistencias_4 <- informacion_inconsistencias_3 %>%
  #   filter(indicador == 2)
  # 
  # 
  # # se detecta todo lo que trae INT y se quita de la información
  # 
  # # Encontrar todas las filas que contienen patrones no deseados
  # indices_a_eliminar <- which(str_detect(inconsistencias_4$colonia, "INT|_|#|DEPTO|DPTO|CASA"))
  # 
  # # Eliminar las filas en un solo paso
  # inconsistencias_4 <- inconsistencias_4[-indices_a_eliminar, ]
  # 
  # sort(unique(inconsistencias_4$colonia))
  # 
  # View(inconsistencias_4)
  
  
# se seleccionan los valores unicos por cp y colonia
# unique_rep_ent_dom <- mod_reporte_entrega_domicilio %>%
  # distinct(colonia,cp,.keep_all = TRUE)

# merge_red_vs_cp <- full_join(unique_rep_ent_dom,mod_cp_cdmx,by = "cp")


# str(mod_reporte_entrega_domicilio)
# 
# sort(unique(mod_reporte_entrega_domicilio$cp))
# sort(unique(mod_cp_cdmx$cp))
# 
# names(raw_reporte_entrega_domicilio)
# names(codigo_postal_cdmx)

#---------------------------------------------------------------------------------------------------
