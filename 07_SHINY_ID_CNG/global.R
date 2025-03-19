library(shiny)
library(DT)
library(bslib)
library(dplyr)
library(jsonlite)
library(openxlsx)
library(shinyjs)

# Base de datos de usuarios (en producción esto debería estar en un archivo externo o base de datos)
# Por ahora lo definimos aquí para simplicidad
usuarios_db <- data.frame(
  email = c("admin@example.com", "javier@gmail.com"),
  password = c("admin123", "123"),
  nombre = c("Administrador", "Javier"),
  stringsAsFactors = FALSE
)

RUTA_DATABASE <- "database/"

# Base de datos para guardar modificaciones
datos_corregidos <- data.frame()