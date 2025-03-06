remove(list = ls())

#---------------------------------------------------------------------------------------------------
# SE CARGAN LIBRERIAS
#---------------------------------------------------------------------------------------------------
library(openxlsx)
library(tidyverse)
library(readxl)
library(dplyr)
library(data.table)
library(svDialogs)
library(tidyr)
library(plotly)
library(openxlsx)
library(plyr)

# options(warn = -1) # Desactivar todas las advertencias, 0 para activar

ti <- Sys.time()

#---------------------------------------------------------------------------------------------------
# FUNCIONES
#---------------------------------------------------------------------------------------------------
source("E_Funciones.R")

#---------------------------------------------------------------------------------------------------
# DIRECTORIOS
#---------------------------------------------------------------------------------------------------

dir_db <- paste0(getwd(),"/01_Database/")
dir_ed <- paste0(getwd(),"/02_Estructura_datos/")
dir_cat <- paste0(getwd(),"/03_Catalogo/")
dir_res <- paste0(getwd(),"/04_Resultados/")
dir_lec <- paste0(getwd(),"/05_Verificacion_R/")
dir_res_ana <- paste0(dir_res,"Analisis_mensual/")
dir_res_men <- paste0(dir_res,"Original_mensual/")
dir_res_ed <- paste0(dir_res,"Estructura_Datos/") 

#---------------------------------------------------------------------------------------------------
# CARGA DE ARCHIVOS
#---------------------------------------------------------------------------------------------------
# ***** MODIFICAR NOMBRE CATALOGO *****
catalogo <- openxlsx::read.xlsx(paste0(dir_cat,"Catalogo_Organo_Jurisdiccional_2024",".xlsx"),
                                sheet = "Catalogos",colNames = TRUE) 
# Se extraen los tribunales
tribunales <- str_trim(catalogo$Nombre.del.órgano.jurisdiccional)

#---------------------------------------------------------------------------------------------------
# PARAMETROS
#---------------------------------------------------------------------------------------------------
# Mes y año a analizar

# ***** MODIFICAR: AJUSTAR A LOS MESES QUE REQUIERA ANALIZARSE *****
# MESES <- c("01_ENERO","02_FEBRERO","03_MARZO","04_ABRIL","05_MAYO","06_JUNIO","07_JULIO","08_AGOSTO","09_SEPTIEMBRE","10_OCTUBRE","11_NOVIEMBRE","12_DICIEMBRE") # ***** MODIFICAR *****
 MESES <- c("01_ENERO","12_DICIEMBRE") # 

# ***** MODIFICAR: AJUSTAR AL EJERCICIO (AÑO) QUE SE ESTA ANALIZANDO *****
ANIO <- 2024 

# Se extrae informacion
extraer <- svDialogs::dlgInput(message=paste0("¿Qué desea realizar?  ",
                                   "1: Extraer información RE,  ",
                                   "2: Generar información ED"))$res

#---------------------------------------------------------------------------------------------------
# ANALISIS
#---------------------------------------------------------------------------------------------------

if(extraer == 1){
  #---------------------------------------------------------------------------------------------------
  # BASES AUXILIARES
  #---------------------------------------------------------------------------------------------------
  # ***** MODIFICAR: AJUSTAR EL NOMBRE DEL ARCHIVO QUE CONTIENE LAS VARIABLES DEL REPORTE ESTADISTICO *****
  # ***** ESTE ARCHIVO SALE AL EJECUTAR EL SCRIPT "H_Genera_Catalogo_Variables_RE.R"  *****
  Ubicaciones <- openxlsx::read.xlsx(paste0(dir_cat,"Ubicacion_variables_ED_2024_V1.0.xlsx"),
                                     sheet = "Sheet 1",colNames = TRUE) 
  
  # SE EXTRAE LA INFORMACION
  source("C_Extraccion_informacion_V1.0.R")

  }else{
  # SE CARGA LA ED PARA LA EXTRACCION
    # ***** MODIFICAR: AJUSTAR EL NOMBRE DE LA ESTRUCTURA DE DATOS CON EL ETIQUETADO CORRECTO *****
    files.archivo <- "04_Estructura_Datos_Justicia_Agraria_CNIJF2025_VF(20feb2025)_Extraccion_Variables" 
    
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
    source("08_ED_CONTROL_V1.1.R")
}

Sys.time() - ti
