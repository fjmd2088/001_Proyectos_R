remove(list = ls())

#---------------------------------------------------------------------------------------------------
# SE CARGAN LIBRERIAS
#---------------------------------------------------------------------------------------------------
library(openxlsx)
library(readxl)
library(dplyr)
library(stringr)
# library(data.table)
library(svDialogs)
library(tidyr)
# library(plotly)
# library(plyr)
library(parallel)


options(warn = -1) # Desactivar todas las advertencias, 0 para activar

ti <- Sys.time()

#---------------------------------------------------------------------------------------------------
# FUNCIONES
#---------------------------------------------------------------------------------------------------
# source("E_Funciones.R")

#---------------------------------------------------------------------------------------------------
# DIRECTORIOS
#---------------------------------------------------------------------------------------------------


RUTA_ED <- file.path(getwd(),"01_Estructura_Datos_Extraccion")
DIR_RE <- paste0(getwd(),"/02_Database_Reporte_Estadistico/")

# dir_cat <- paste0(getwd(),"/03_Catalogo/")
# dir_res <- paste0(getwd(),"/04_Resultados/")
# dir_lec <- paste0(getwd(),"/05_Verificacion_R/")
# dir_res_ana <- paste0(dir_res,"Analisis_mensual/")
# dir_res_men <- paste0(dir_res,"Original_mensual/")
# dir_res_ed <- paste0(dir_res,"Estructura_Datos/") 

#---------------------------------------------------------------------------------------------------
# CARGA DE ARCHIVOS
#---------------------------------------------------------------------------------------------------
# catalogo <- openxlsx::read.xlsx(paste0(dir_cat,"Catalogo_Organo_Jurisdiccional_2023",".xlsx"),
                                # sheet = "Catalogos",colNames = TRUE) # ***** MODIFICAR *****
# Se extraen los tribunales
# tribunales <- str_trim(catalogo$Nombre.del.órgano.jurisdiccional)

#---------------------------------------------------------------------------------------------------
# VARIABLES
#---------------------------------------------------------------------------------------------------
# Mes y año a analizar
# MESES <- c("01_ENERO","02_FEBRERO","03_MARZO","04_ABRIL","05_MAYO","06_JUNIO","07_JULIO","08_AGOSTO","09_SEPTIEMBRE","10_OCTUBRE","11_NOVIEMBRE","12_DICIEMBRE") # ***** MODIFICAR *****
MESES <- c("01_ENERO") # ***** MODIFICAR SOLO ANALIZAR UN MES*****
ANIO <- 2024 # ***** MODIFICAR *****



#---------------------------------------------------------------------------------------------------
#                                             ANALISIS
#---------------------------------------------------------------------------------------------------

# se carga la informacion de la estructura de datos

NOMBRE_ED <- "04_Estructura_Datos_Justicia_Agraria_CNIJF2025_VF(20feb2025)_Extraccion_Variables.xlsx"
HOJAS_ED <-c("Ingresos","Trámite") 

data_hoja_ed <- openxlsx::read.xlsx(file.path(RUTA_ED,NOMBRE_ED),
                                     sheet = HOJAS_ED[1],
                                     colNames = FALSE,
                                    skipEmptyRows = FALSE,
                                    skipEmptyCols = FALSE)

# bucle sobre las columnas de la hoja en la estructura de datos

for(j in 1:ncol(data_ingresos)){
  j <- 6
  
  booleano <- str_detect(string = data_hoja_ed[6,j],pattern = "HOJA")
  
  # la columna contiene una variable de la estructura de datos
  if(booleano == TRUE){
    cadena <- data_hoja_ed[6:8,j]
    
    # Extraer el número de Hoja
    hoja <- as.numeric(str_extract(cadena, "(?<=HOJA\\s)\\d+"))
    
    # Extraer la letra de la columna y convertirla a número de Excel
    columna_letra <- str_extract(cadena, "(?<=COLUMNA\\s)[A-Z]")
    columna_numero <- match(columna_letra, LETTERS)
    
    # Extraer el número de la fila
    fila <- as.numeric(str_extract(cadena, "(?<=FILA\\s)\\d+"))
    
    # Mostrar resultados
    resultado <- tibble(Hoja = hoja, Columna = columna_numero, Fila = fila)
    print(resultado)
  }
  
}


coordenadas_variables <- function(data_estructura_datos, columna){
  
  coor_var <- data_hoja_ed %>%
    mutate(
      Hoja = str_extract(columna, "HOJA\\s\\d+"),
      Columna = str_extract(columna, "COLUMNA\\s[A-Z]"),
      Fila = str_extract(columna, "FILA\\s\\d+")
    ) %>%
    select(Hoja, Columna, Fila) %>%
    filter(!is.na(Hoja) | !is.na(Columna) | !is.na(Fila)) # Filtrar solo filas con valores
  
 
}

#---------------------------------------------------------------------------------------------------
#                                             PARALLELO
#---------------------------------------------------------------------------------------------------

# Cargar los paquetes necesarios en la sesión principal
library(parallel)
library(readxl)
library(dplyr)

# Definir el número de núcleos a utilizar
NUM_CORES <- detectCores() - 1

# Crear el clúster
cl <- makeCluster(NUM_CORES)

# Cargar los paquetes necesarios en cada nodo del clúster
clusterEvalQ(cl, {
  library(readxl)
  library(dplyr)
})

# Definir la función para procesar cada archivo Excel
procesar_excel <- function(ruta_archivo) {
  hojas <- excel_sheets(ruta_archivo)
  datos <- lapply(hojas[5:6], function(hoja) read_excel(ruta_archivo, sheet = hoja))
  bind_rows(datos)  # Combinar datos de todas las hojas
}

# Definir la función para procesar cada mes
procesar_mes <- function(ruta_mes) {
  archivos <- list.files(ruta_mes, pattern = "\\.xls$", full.names = TRUE)
  datos_mes <- lapply(archivos, procesar_excel)
  bind_rows(datos_mes)
}

# Exportar las funciones necesarias al clúster
clusterExport(cl, c("procesar_excel", "procesar_mes"))

# Definir el directorio principal con las carpetas de los meses
directorio_principal <- file.path(getwd(), "02_Database_Reporte_Estadistico")
meses <- list.dirs(directorio_principal, recursive = FALSE)

# Ejecutar en paralelo el procesamiento para cada mes
datos_final <- parLapply(cl, meses, procesar_mes)
TF <- bind_rows(datos_final)

# Cerrar el clúster
stopCluster(cl)


#---------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------

# Se extrae informacion
# extraer <- svDialogs::dlgInput(message=paste0("¿Qué desea realizar?  ",
                                              # "1: Extraer información RE,  ",
                                              # "2: Generar información ED"))$res
# 
#---------------------------------------------------------------------------------------------------
#                                             ANALISIS
#---------------------------------------------------------------------------------------------------

# if(extraer == 1){
  #---------------------------------------------------------------------------------------------------
  # BASES AUXILIARES
  #---------------------------------------------------------------------------------------------------
  # Ubicaciones <- openxlsx::read.xlsx(paste0(dir_cat,"Ubicacion_variables_ED_2025_V1.0.xlsx"),
                                     # sheet = "Sheet 1",colNames = TRUE) # ***** MODIFICAR *****
  
  # SE EXTRAE LA INFORMACION
  # source("C_Extraccion_informacion_V1.0.R")
  
# }else{
  # 
  files.archivo <- "01_Estructura_Datos_Justicia_Agraria_2024_Extraccion_Variables_25mar2024" # ***** MODIFICAR *****
  
  # SE REALIZA CONCENTRADO DE INFORMACICION
  
  source("D_Vaciado_informacion_V1.0.R")
  
  db_analizar <- svDialogs::dlg_open(
    default = dir_res,
    title =  "Seleccionar que CONCETRADO se va a procesar:",
    multiple = FALSE,
    filters = dlg_filters["All", ],
    gui = .GUI
  )
  
  db_analizar <- db_analizar$res
  
  if(str_detect(db_analizar,"ORIGINAL") == TRUE){
    name_file <- "ORI"
  }
  
  if(str_detect(db_analizar,"MOD") == TRUE){
    name_file <- "MOD"
  }
  
  data <- fread(db_analizar)
  
  # SE GENERAN LOS ARCHIVOS PARA LA ESTRUCTURA DE DATOS---------------------------------------------
  source("01_ED_INGRESOS_V1.0.R")
  source("02_ED_TRAMITE_V1.0.R")
  source("03_ED_CONCLUSIONES_V1.0.R")
  source("04_ED_ACTOS_PROCESALES_V1.0.R")
  source("05_ED_EJECUTORIAS_V1.0.R")
  source("06_ED_EXHORTOS_V1.0.R")
  source("07_ED_HIDROCARBUROS_V1.0.R")
  source("08_ED_CONTROL_V1.0.R")
# }

Sys.time() - ti
