# Programa que hace la codificacion SHA

library(digest) # codigo SHA
library(tidyverse)

# Definir las variables
ent_fed <- "CDMX"
cve_ent <- "9"
programa <- "P1"

nombre <- "FRANCISCO Javier"
apellido <- "Martínez"
fecha_nacimiento <- as.Date("2025-01-23")
sexo <- "Masculino"


# Combinar las variables en un único string (puedes usar paste o paste0)
concatenacion <- str_to_upper(gsub("[^A-Za-z0-9]", "",
                                   gsub(" ","",
                                        paste0(nombre,apellido,fecha_nacimiento))))

print(concatenacion)
# Generar el código SHA-256
hash <- digest(concatenacion, algo = "sha256")

HM <- substr(str_to_upper(hash),1,10)

ID <- paste0(ent_fed,"_",programa,"_",HM)

print(HM)

nchar(hash)
