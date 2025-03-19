

library(RJDBC)
library(odbc)

dirMaquina<-getwd()

fileBase23 <- dlg_open(default = dirMaquina,
                                    title = "Elige el archivo de la base 2023:",
                                    rstudio = getOption("svDialogs.rstudio", TRUE),
                                    gui = .GUI
)$res

base_2023 <- read_excel(path=fileBase23, guess_max = 10000000)

#Limpiamos acentos y dobles espacios
base<- base%>%
  mutate(across(c(nom_mun, tipo_infraestructura, nom_infraestructura), ~ {
    . %>%
      trimws() %>%
      stri_replace_all_regex("\\s+", " ") %>%
      stri_trans_general("Latin-ASCII")
  }))

base_2023<- base_2023%>%mutate(across(c(nom_mun, tipo_infraestructura, nom_infraestructura), ~ {
  . %>%
    trimws() %>%
    stri_replace_all_regex("\\s+", " ") %>%
    stri_trans_general("Latin-ASCII")
}))

columnas <- colnames(base_2023)
columnas <- paste0(columnas,".2023")
colnames(base_2023)<- columnas


base_2023$key<- paste(base_2023$censo.2023, base_2023$ID_INEGI_2024.2023, sep= ".")
base$key<- paste(base$censo, base$ID_INEGI_2024, sep= ".")


base_2023$key1<- paste(base_2023$censo.2023, base_2023$ID_INEGI_2024.2023, sep= ".")
base_2023$key2<- paste(base_2023$censo.2023, base_2023$ID_INEGI_2025.2023, sep= ".")
base_2023<- base_2023%>%mutate(key=ifelse(is.na(ID_INEGI_2024.2023), key2, key1))
base$key1<- paste(base$censo, base$ID_INEGI_2024, sep= ".")
base$key2<- paste(base$censo, base$ID_INEGI_2025, sep= ".")
base<- base%>%mutate(key=ifelse(is.na(ID_INEGI_2024), key2, key1))


base_unida<- base_2023%>%right_join(base, by= c("key"))
#base_unida$id_inegi_2023.2023[base_unida$id_inegi_2023.2023%in%"Sin ID previo"]<- NA
# base_unida<- base_unida%>%mutate(long.2023=as.numeric(long.2023),
#                                  lat.2023= as.numeric(lat.2023))
# base_unida[is.na(base_unida$id_registro.2023), 
#            c("lat.2023","long.2023","nom_mun.2023","cve_mun.2023","nom_infraestructura.2023")]<-base_unida[is.na(base_unida$id_registro.2023), 
#                                                                                                            c("latitud","longitud","nom_mun","cve_mun","nom_infraestructura")]
base_unida$id_inegi_2023[base_unida$id_inegi_2023%in%"Sin ID previo"]<- NA

dups<-base_unida$key[duplicated(base_unida$key)]

base_unida2 <- base_unida%>%
  mutate(lat.2023= ifelse(lat.2023%in%c(-1,-2,-3,-4), NA, as.numeric(lat.2023)),
         long.2023= ifelse(long.2023%in%c(-1,-2,-3,-4), NA, as.numeric(long.2023)),
         latitud=ifelse(is.na(latitud), as.numeric(lat.2023), as.numeric(latitud)),
         longitud= ifelse(is.na(longitud), as.numeric(long.2023), as.numeric(longitud))
  )%>%
  mutate(censo=ifelse(is.na(censo.2023), censo, censo.2023),
         id_inegi.2023=ifelse(is.na(id_inegi_2023.2023), id_inegi_2023, id_inegi_2023.2023),
         id_inegi.2024=ifelse(is.na(ID_INEGI_2024.2023), ID_INEGI_2024, ID_INEGI_2024.2023),
         id_inegi.2025=ifelse(is.na(ID_INEGI_2025.2023), ID_INEGI_2025, ID_INEGI_2025.2023),
         NOM_ENT=ifelse(is.na(NOM_ENT.2023), NOM_ENT, NOM_ENT.2023),
         cve_ent=ifelse(is.na(cve_ent.2023), cve_ent, cve_ent.2023),
         nom_infra=ifelse(is.na(nom_infra.2023), nom_infra, nom_infra.2023),
         tipo_infraestructura=ifelse(is.na(tipo_infraestructura.2023), tipo_infraestructura, tipo_infraestructura.2023),
         ambito=ifelse(is.na(ambito.2023), ambito, ambito.2023),
         nom_infraestructura.2024=nom_infraestructura,
         nom_mun.2024=nom_mun,
         cve_mun.2024=cve_mun,
         latitud.2024=latitud,
         longitud.2024=longitud,
         
         
         nom_infraestructura.2025=nom_infraestructura,
         nom_mun.2025=nom_mun,
         cve_mun.2025=cve_mun,
         latitud.2025=latitud,
         longitud.2025=longitud,
         
         latitud.2023=lat.2023,
         longitud.2023= long.2023,
         geocodigo= "S/C"
  )%>%
  select(id_registro,
         id_temporalidad,
         censo,
         id_inegi.2023,
         id_inegi.2024,
         id_inegi.2025,
         NOM_ENT,
         cve_ent,
         nom_infra,
         tipo_infraestructura,
         ambito,
         nom_infraestructura.2023, nom_infraestructura.2024, nom_infraestructura.2025,
         nom_mun.2023, nom_mun.2024, nom_mun.2025,
         cve_mun.2023, cve_mun.2024, cve_mun.2025,
         latitud.2023, latitud.2024, latitud.2025,
         longitud.2023, longitud.2024, longitud.2025,
         estatus,tipo_ubicacion,Observaciones)%>%
  #pivot_longer(starts_with("id_inegi"), names_to = "Anyo",names_prefix = "id_inegi_", values_to="id_inegi", values_drop_na = TRUE)%>%
  pivot_longer(ends_with(c("2023","2024", "2025")),names_pattern = "(.*)(.2023|.2024|.2025)$",
               names_to = c(".value", "Anyo"), values_drop_na = TRUE)%>%
  mutate(Anyo=gsub(x=Anyo, pattern="\\.", replacement=""),
         across(where(is.character),trimws),
         id_registro= 1:length(id_registro),
         estatus=ifelse(Anyo%in%2023, "ACTIVO", estatus))%>%
  mutate(across(where(is.character),~gsub("\\s+", " ", .)))%>%
  filter(!is.na(id_inegi))

colnames(base_unida2)<- tolower(colnames(base_unida2))


# Conectar a la base de datos Oracle
driver <- JDBC(driverClass = "oracle.jdbc.OracleDriver","C:/oracle/instantclient_19_21/ojdbc8.jar")
                 servicio <- "cegopro.inegi.gob.mx"  # Nombre del servicio Oracle
                 usuario <- "cng_ids_adm" 
                 password <- "cng_ids_adm"
                 host <- "cegopro_bd.inegi.gob.mx"
                 puerto <- "1521"
                 cadena <- paste0("jdbc:oracle:thin:@//",host,":",puerto,"/",servicio)
                 
conexion <- dbConnect(driver,cadena,usuario,password)         

# Nombre de la tabla en Oracle
tabla_oracle <- "TR_INSTITUCIONES"


options(RJDBC.identifier.quote = NULL)

# Borrar los datos existentes en la tabla
dbSendUpdate(conexion, paste("TRUNCATE TABLE", tabla_oracle))

# Subir el dataframe a la tabla en Oracle
dbWriteTable(conexion, name = tabla_oracle, value =base_unida2 , append = TRUE, row.names = FALSE)

# Cerrar la conexión
dbDisconnect(conexion)
