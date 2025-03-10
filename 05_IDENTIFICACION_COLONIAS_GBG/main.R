
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

# DIRECTORIOS
DIRECTORIO_CATALOGOS <- "catalogos/"
DIRECTORIO_BD <-"bd/" 

# VARIABLES
NOMBRE_CATALOGO_CP <- "CodigosPostales_CDMX_modificado"
NOMBRE_REPORTE <- "OpenOrderReport30012025"
NOMBRE_COLONIAS <- "Colonias"

# CARGA CATALOGOS 
codigo_postal_cdmx <- read_excel(paste0(DIRECTORIO_CATALOGOS,NOMBRE_CATALOGO_CP,".xlsx"))
colonias <- read.csv(paste0(DIRECTORIO_CATALOGOS,NOMBRE_COLONIAS,".csv"))

# CARGA BASE DE DATOS
raw_reporte_entrega_domicilio <- read.csv(paste0(DIRECTORIO_BD,NOMBRE_REPORTE,".csv"))

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

# se sustituyen patrones de colonias mal escritas que han sido identificadas

# Definir las variantes incorrectas y su corrección
reemplazos <- c(
  "PKRTALES SUR" = "PORTALES SUR",
  "PORTALES SU"  = "PORTALES SUR",
  "PORTAES SUR"  = "PORTALES SUR",
  "PIRTALES SUR" = "PORTALES SUR",
  "INDENDENCIA" = "INDEPENDENCIA",
  "INDEPENCIA" = "INDEPENDENCIA",
  "INDEPENDECIA" = "INDEPENDENCIA",
  "INDEPENDENCA" = "INDEPENDENCIA",
  "INDEPENDNCIA" = "INDEPENDENCIA",
  "INDPEND4NCIA" = "INDEPENDENCIA",
  "SANTA CRUZ ATEYOC" = "SANTA CRUZ ATOYAC",
  "STA CRUS ATOYAC" = "SANTA CRUZ ATOYAC",
  "STA CRUZ  ATOYAC" = "SANTA CRUZ ATOYAC",
  "STA CRUZ ATOYAC" = "SANTA CRUZ ATOYAC",
  "STA. CRUZ ATOYAC" = "SANTA CRUZ ATOYAC",
  "STA. CRUZ ATOYAC" = "SANTA CRUZ ATOYAC",
  "PORTALE NORTE" = "PORTALES NORTE",
  "PORTALEE NOT" = "PORTALES NORTE",
  "PORTALES NTE" = "PORTALES NORTE",
  "PORTALLES NORTE" = "PORTALES NORTE",
  "PORTARLES NORTE" = "PORTALES NORTE",
  "POTALES NORTE" = "PORTALES NORTE",
  "PORTALES N" = "PORTALES NORTE",
  "PORTALES OTE" = "PORTALES ORIENTE",
  "PORTALES O" = "PORTALES ORIENTE",
  "LEAN VALLE" = "LETRAN VALLE",
  "LELTRAN VALLE" = "LETRAN VALLE",
  "LETAN VALLE" = "LETRAN VALLE",
  "LETRAN" = "LETRAN VALLE",
  "LETRAN DEL VALLE" = "LETRAN VALLE",
  "LETRAN VALLLE" = "LETRAN VALLE",
  "LETRANVALLE" = "LETRAN VALLE",
  "LEYRA VALLE" = "LETRAN VALLE",
  "LLETRN VALLE" = "LETRAN VALLE",
  "BELTRAN VALLE" = "LETRAN VALLE",
  "AMERICA HUNDIDAS" = "AMERICAS UNIDAS",
  "AMERICA UNIDA" = "AMERICAS UNIDAS",
  "AMERICAS UNIDOS" = "AMERICAS UNIDAS",
  "SAN SIMN" = "SAN SIMON TICUMAC",
  "SAN SIMON" = "SAN SIMON TICUMAC",
  "SAN SIMON  TICUMAC" = "SAN SIMON TICUMAC",
  "SAN SIMON TICOMAC" = "SAN SIMON TICUMAC",
  "SAN SIMON TICUMAN" = "SAN SIMON TICUMAC",
  "NARVARTE OTE" = "NARVARTE ORIENTE",
  "NARVARTE VERTIZ" = "VERTIZ NARVARTE",
  "NARVTE PONIENTE" = "NARVARTE PONIENTE",
  "NAVARTE PONIENTE" = "NARVARTE PONIENTE",
  "NARVARTE PNIEBT" = "NARVARTE PONIENTE",
  "GENERAL ANAYA" = "GENERAL PEDRO MARIA ANAYA",
  "GENRAL ANAYA" = "GENERAL PEDRO MARIA ANAYA",
  "GNRL PRDRO MARIA" = "GENERAL PEDRO MARIA ANAYA",
  "GRAL PEDRO MARIA ANAYA" = "GENERAL PEDRO MARIA ANAYA",
  "GRAN ANAYA" = "GENERAL PEDRO MARIA ANAYA",
  "ZACAHUISCO" = "ZACAHUITZCO",
  "ZACAHUISTCO" = "ZACAHUITZCO" 
)

# Aplicar la sustitución en la columna "colonia"
mod_reporte_entrega_domicilio$colonia <- str_replace_all(
  mod_reporte_entrega_domicilio$colonia,
  reemplazos
)


# cruce de informacion entre la informacion del servicio postal mexicano y la base modificada
  cruce_informacion <- left_join(mod_reporte_entrega_domicilio,mod_cp_cdmx,by = c("colonia","cp"))

  # se filtra la informacion que coincide en colonia con codigo postal
  informacion_correcta_1 <- cruce_informacion %>%
    filter(!is.na(Municipio))
  
  # se crea catalogo con las colonias correctas 
  catalogo_inf_correcta <- informacion_correcta_1 %>%
    select(colonia,cp,Municipio) %>%
    distinct(colonia,.keep_all = TRUE)
  
  # Se extrae informacion donde el campo colonia es "" o NA
  informacion_sin_dato <- cruce_informacion %>%
    filter(is.na(colonia) | colonia == "")
  
  # *???????????????????????????????????????????????????????????????????
  # se filtra informacion con las siguientes caracteristicas:
  # vacio, 0, NA
  
  
  informacion_inconsistencias <- cruce_informacion %>%
    filter(is.na(Municipio)) %>%
    filter(!is.na(colonia) & colonia != "")
  
  sort(unique(informacion_inconsistencias$colonia))
  sort(unique(informacion_inconsistencias$cp))
  
  COLONIAS_CORRECTAS <- sort(unique(informacion_real$colonia)) 
  
  informacion_inconsistencias_2 <- informacion_inconsistencias %>%
    mutate(indicador = 2)

  cruce_2 <- left_join(informacion_inconsistencias_2,catalogo_inf_real,b = "colonia")
  
  informacion_real_2 <- cruce_2 %>%
    filter(!is.na(cp.y))
           
  informacion_inconsistencias_3 <- cruce_2 %>%
    filter(is.na(cp.y)) %>%
    select(-cp.y) 
  
  for (i in 1:length(COLONIAS_CORRECTAS)){
    # i <- 2
    IND_I <- which(str_detect(informacion_inconsistencias_3$colonia,COLONIAS_CORRECTAS[i])==TRUE)
    
    informacion_inconsistencias_3$colonia[IND_I] <- COLONIAS_CORRECTAS[i]
    informacion_inconsistencias_3$indicador[IND_I] <- 3
  }
  
  informacion_real_3 <- informacion_inconsistencias_3 %>%
    filter(indicador == 3)
  
  inconsistencias_4 <- informacion_inconsistencias_3 %>%
    filter(indicador == 2)
  
  
  # se detecta todo lo que trae INT y se quita de la información
  
  # Encontrar todas las filas que contienen patrones no deseados
  indices_a_eliminar <- which(str_detect(inconsistencias_4$colonia, "INT|_|#|DEPTO|DPTO|CASA"))
  
  # Eliminar las filas en un solo paso
  inconsistencias_4 <- inconsistencias_4[-indices_a_eliminar, ]
  
  sort(unique(inconsistencias_4$colonia))
  
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
