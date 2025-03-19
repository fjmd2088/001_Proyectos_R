library(RJDBC)
library(DBI)

# Definir el driver JDBC
# Instalar instantclient_19_21 antes
driver <- JDBC(driverClass = "oracle.jdbc.OracleDriver",
               classPath = "C:/oracle/instantclient_19_21/ojdbc8.jar", 
               identifier.quote = "`")

# Datos de conexión
host <- "cegopro_bd.inegi.gob.mx"
puerto <- "1521"
servicio <- "cegopro.inegi.gob.mx"  # Service Name o SID

# Construir cadena de conexión (elige la correcta)
# Para Service Name:
cadena <- paste0("jdbc:oracle:thin:@//", host, ":", puerto, "/", servicio)

# Para SID (si es necesario):
# cadena <- paste0("jdbc:oracle:thin:@", host, ":", puerto, ":", servicio)

usuario <- "cng_ids_adm"
password <- "cng_ids_adm"

# Conectar a la base de datos
conexion <- dbConnect(driver, cadena, usuario, password)

# Verificar conexión
if (!dbIsValid(conexion)) {
  stop("Error: No se pudo conectar a la base de datos.")
} else {
  print("Conexión exitosa a Oracle")
}

