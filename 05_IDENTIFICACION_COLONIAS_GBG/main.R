
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

# CARGA CATALOGOS 
codigo_postal_cdmx <- read_excel(paste0(DIRECTORIO_CATALOGOS,NOMBRE_CATALOGO_CP,".xlsx"))

# CARGA BASE DE DATOS
raw_reporte_entrega_domicilio <- read.csv(paste0(DIRECTORIO_BD,NOMBRE_REPORTE,".csv"))

#---------------------------------------ANALISIS----------------------------------------------------

# se ontiene la base de cp que se necesita para el analisis
mod_cp_cdmx <- codigo_postal_cdmx %>%
  rename(cp = "Código Postal",
         colonia = "Asentamiento") %>%
  select(cp,colonia) %>%
  mutate(colonia = stri_trans_general(str_to_upper(colonia),"Latin-ASCII"))

# se obtiene la base modificada que cumpla con el cp de la cdmx
mod_reporte_entrega_domicilio <- raw_reporte_entrega_domicilio %>%
  select(Address) %>% # se selecciona la columna de interes
  mutate(Adddress_original = Address,
         Address = str_to_upper(Address)) %>%
  separate(Address, 
           into = c("calle","colonia","cp"), 
           sep = "@", 
           extra = "merge", 
           fill = "right") %>%
  mutate(colonia = gsub("NEWLINE","",colonia), # se quita patron en colonia
         cp = gsub("\\D","",cp), # se extrae solo los numeros del cp
         cp = as.numeric(cp)) %>% # se convierte a numerico cp
  filter(cp >= min(mod_cp_cdmx$cp) & cp <= max(mod_cp_cdmx$cp))  # se filtra por cp cdmx
  
# se seleccionan los valores unicos por cp y colonia
unique_rep_ent_dom <- mod_reporte_entrega_domicilio %>%
  distinct(colonia,cp,.keep_all = TRUE)

merge_red_vs_cp <- full_join(unique_rep_ent_dom,mod_cp_cdmx,by = "cp")


# str(mod_reporte_entrega_domicilio)
# 
# sort(unique(mod_reporte_entrega_domicilio$cp))
# sort(unique(mod_cp_cdmx$cp))
# 
# names(raw_reporte_entrega_domicilio)
# names(codigo_postal_cdmx)

#---------------------------------------------------------------------------------------------------
