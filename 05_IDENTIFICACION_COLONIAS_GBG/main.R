
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
DIRECTORIO_IMG <- "img/"
DIRECTORIO_PPT <- "ppt/"

logo_path <- paste0(DIRECTORIO_IMG, "logo.png")
grafico_path <- paste0(DIRECTORIO_IMG, "grafico1.png")

ti <- Sys.time() # TIEMPO INICIAL

# VARIABLES
SUCURSALES <- c("Cafetales","Coyoacan","Division","Miramontes","Renato","SantaAna","Xochimilco")
NOMBRE_CATALOGO_CP <- "CodigosPostales_CDMX_modificado"
ARCHIVOS_SUCURSALES <- list.files(DIRECTORIO_BD,pattern = ".csv") # Se detecta los archivos en la carpeta

# CARGA CATALOGOS 
codigo_postal_cdmx <- read_excel(paste0(DIRECTORIO_CATALOGOS,NOMBRE_CATALOGO_CP,".xlsx"))
# se obtiene la base de cp que se necesita para el analisis
mod_cp_cdmx <- codigo_postal_cdmx %>%
  rename(cp = "Código Postal",
         colonia = "Asentamiento") %>%
  select(cp,Municipio,colonia) %>%
  mutate(colonia = stri_trans_general(str_to_upper(colonia),"Latin-ASCII")
  )

# Bucle sobre las sucursales
for(i in 1:length(SUCURSALES)){
  # i <- 1
  cat("Procesando sucursal: ", SUCURSALES[i], "\n\n")
  
  idx <- which(str_detect(ARCHIVOS_SUCURSALES,pattern = SUCURSALES[i]) == TRUE)
  
  NOMBRE_REPORTE <- ARCHIVOS_SUCURSALES[idx]
  archivo_ppt <- paste0(DIRECTORIO_PPT, "Reporte_analisis_colonias_,",SUCURSALES[i],".pptx")
  
  # CARGA BASE DE DATOS
  raw_reporte_entrega_domicilio <- read.csv(paste0(DIRECTORIO_BD,NOMBRE_REPORTE))
  
  # PPT---------------------------------------------------------------------------------------------
  # Leer la imagen y obtener dimensiones originales
  img <- image_read(logo_path)
  img_info <- image_info(img)
  original_width <- img_info$width
  original_height <- img_info$height
  
  # Escalar la imagen al 50%
  scale_factor <- 0.5
  scaled_width <- original_width * scale_factor / 96  # Convertir a pulgadas
  scaled_height <- original_height * scale_factor / 96  # Convertir a pulgadas
  
  #---------------------------------------ANALISIS--------------------------------------------------
  
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
  
  CANT_TOTAL_REGISTROS <- nrow(mod_reporte_entrega_domicilio)
  
  # Se extrae informacion donde el campo colonia es "" o NA
  informacion_sin_colonia <- mod_reporte_entrega_domicilio %>% filter(is.na(colonia) | colonia == "")
  CANT_REG_SIN_INF <- nrow(informacion_sin_colonia) 
  
  # Se extrae la informacion que si tiene contenido
  informacion_con_colonia <- mod_reporte_entrega_domicilio %>% filter(!is.na(colonia) & colonia != "")
  CANT_REG_CON_INF <- nrow(informacion_con_colonia) 
  
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
  for (j in 1:length(catalogo_colonia_cp_correcta$colonia)){
    # j <- 13
    IND_I <- which(str_detect(datos_inconsistencias$colonia,catalogo_colonia_cp_correcta$colonia[j])==TRUE)
    
    datos_inconsistencias$colonia[IND_I] <- catalogo_colonia_cp_correcta$colonia[j]
    datos_inconsistencias$Municipio[IND_I] <- catalogo_colonia_cp_correcta$Municipio[j]
    datos_inconsistencias$cp[IND_I] <- catalogo_colonia_cp_correcta$cp[j]
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
  
  grafico <- ggplot(datos_graf1[1:15,], aes(x = reorder(colonia, total, function(x) -x), y = total)) +
    geom_bar(stat = "identity", fill = "blue") +
    geom_text(aes(label = total), vjust = -0.3, color = "black", size = 4) +
    labs(title = paste("Cantidad de entregas por colonia", " Sucursal: ", SUCURSALES[i]),
         x = "Colonias",
         y = "Total Entregas") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  
  # Guardar el gráfico como imagen
  grafico_path <- paste0(DIRECTORIO_IMG,SUCURSALES[i],".png")
  ggsave(grafico_path, plot = grafico, width = 12, height = 10, dpi = 300)
  
  # Crear una presentación con tamaño personalizado (Ejemplo: 16:9 -> 13.33 x 7.5 pulgadas)---------
  ppt <- read_pptx(paste0(DIRECTORIO_PPT,"plantilla.pptx"))
  layout_summary(ppt)
  
  # Verificar los nombres de los placeholders disponibles en la diapositiva "Title Slide"
  layout_info <- layout_properties(ppt, layout = "Diapositiva de título", master = "Tema de Office")
  # print(layout_info)  # Imprime los tipos de placeholders disponibles
  layout_info <- layout_properties(ppt, layout = "Título y texto vertical", master = "Tema de Office")
  # print(layout_info)  # Imprime los tipos de placeholders disponibles
  
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

  # # Gráfico de datos
  # ppt <- add_slide_with_logo(ppt, "Gráfico: Cantidad de entregas", "Este gráfico muestra la cantidad de entregas por colonia.") %>%
    # ph_with(value = external_img(grafico_path, height = 4, width = 6), location = ph_location_type(type = "body"))
  # Obtener dimensiones de la diapositiva
  ppt_size <- slide_size(ppt)
  
  # Definir proporción de la imagen (porcentaje de la diapositiva)
  img_width <- ppt_size$width * 0.8  # 80% del ancho de la diapositiva
  img_height <- ppt_size$height * 0.6  # 60% del alto de la diapositiva
  
  ppt <- add_slide_with_logo(ppt, "Gráfico: Cantidad de entregas", "Este gráfico muestra la cantidad de entregas por colonia.") %>%
    ph_with(value = external_img(grafico_path, height = img_height, width = img_width), 
            location = ph_location(left = (ppt_size$width - img_width) / 2, 
                                   top = (ppt_size$height - img_height) / 2, 
                                   width = img_width, height = img_height))

  # Guardar el PowerPoint
  print(ppt, target = archivo_ppt)
}

cat("Archivos procesados: ", length(SUCURSALES), "\n")
Sys.time() - ti # TIEMPO FINAL