
# remove(list = ls())

# Se cargan librerias
library(readxl)
library(dplyr)
library(purrr)
library(stringr)
library(tidyr)
library(data.table)
library(openxlsx)


# Directorios
DIR_ED_ETIQUETADO <- "06_ED_Etiquetado/"
DIR_CATALOGO <- "03_Catalogo/"

# Variables
NOMBRE_ED_ETIQUETADO <- "EST-34_Ejercicio2025" # Modificar dependiendo el ejercicio
HOJAS_INVOLUCRADAS <- c("Hoja1","Hoja2","Hoja3","Hoja4","Hoja5","Hoja6","Hoja7","Hoja8",
                        "Hoja9","Hoja11")

# Carga informacion

CATALOGO_VARIABLES <- list()
for(i in 1:length(HOJAS_INVOLUCRADAS)){
  # i <- 1
  
  PATRON_I <- case_when(
    HOJAS_INVOLUCRADAS[i] == "Hoja1" ~ "H01",
    HOJAS_INVOLUCRADAS[i] == "Hoja2" ~ "H02",
    HOJAS_INVOLUCRADAS[i] == "Hoja3" ~ "H03",
    HOJAS_INVOLUCRADAS[i] == "Hoja4" ~ "H04",
    HOJAS_INVOLUCRADAS[i] == "Hoja5" ~ "H05",
    HOJAS_INVOLUCRADAS[i] == "Hoja6" ~ "H06",
    HOJAS_INVOLUCRADAS[i] == "Hoja7" ~ "H07",
    HOJAS_INVOLUCRADAS[i] == "Hoja8" ~ "H08",
    HOJAS_INVOLUCRADAS[i] == "Hoja9" ~ "H09",
    TRUE ~ "H11"  # Si no coincide con los anteriores, asigna "H11"
  )
  
  raw_ed_etiquetado <- read_excel(paste0(DIR_ED_ETIQUETADO,NOMBRE_ED_ETIQUETADO,".xls"),
                                  sheet = HOJAS_INVOLUCRADAS[i], 
                                  col_names = FALSE)
  
  # Convertir el dataframe a formato largo
  coord_variables <- raw_ed_etiquetado %>%
    mutate(Fila = row_number()) %>%
    mutate(across(everything(), as.character)) %>%  # Convertir todas las columnas a carácter
    pivot_longer(cols = -Fila, names_to = "Columna", values_to = "ID") %>%
    filter(str_detect(ID, paste0("^",PATRON_I))) %>%  # Filtrar solo las coincidencias
    mutate(Columna = as.numeric(gsub("^\\.\\.\\.","",Columna)),
           Nombre.Hoja = HOJAS_INVOLUCRADAS[i],
           Variable = "")
  
  CATALOGO_VARIABLES[[i]] <- coord_variables
  
  print(coord_variables)
  
}

CATALOGO_VARIABLES <- rbindlist(CATALOGO_VARIABLES)

# Se eliminan variables

CATALOGO_VARIABLES <- CATALOGO_VARIABLES %>%
  filter(!(Fila == 59 & Columna == 4 & Nombre.Hoja == "Hoja1") &
           !(Fila == 60 & Columna == 17 & Nombre.Hoja == "Hoja7"))


write.xlsx(CATALOGO_VARIABLES,
           paste0(DIR_CATALOGO,"Ubicacion_variables_ED_2025_V1.0",".xlsx"),
           overwrite = TRUE)


