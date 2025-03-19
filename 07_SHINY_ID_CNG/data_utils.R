# Funciones relacionadas con la gestión de datos

# Función para cargar datos de manera más eficiente
load_data <- function() {
  RUTA_DATABASE <- "database/"
  
  datos_inconsistencias <- openxlsx::read.xlsx(paste0(RUTA_DATABASE,
                                                      "inconsistencias_2025-03-06",
                                                      ".xlsx")) %>%
    # filter(cve_mun_INC != -2) %>%
    select(-cve_mun) %>%
    rename(
      CNG = "censo",
      tipo_infra = "nom_infra",
      ID_2024 = "ID_INEGI_2024",
      Ent = "NOM_ENT",
      Mun = "nom_mun",
      cve_mun = "cve_mun_INC",
      nom_mun = "nom_mun_INC",
      nom_infra = "nom_infraestructura_INC",
      latitud = "latitud_INC",
      longitud = "longitud_INC",
      estatus = "estatus_INC"
    ) %>%
    select(CNG, tipo_infra, ID_2024, Ent, Mun, cve_mun, nom_mun, nom_infra, latitud, 
           longitud, estatus, BASE)
  
  # Convertir a tipo adecuado para mejorar rendimiento
  datos_inconsistencias$CNG <- as.character(datos_inconsistencias$CNG)
  datos_inconsistencias$ID_2024 <- as.character(datos_inconsistencias$ID_2024)
  
  # Separar datos
  datos_oficina <- datos_inconsistencias %>%
    filter(BASE == "Dashboard ID") %>%
    select(-BASE)
  
  datos_censo <- datos_inconsistencias %>%
    filter(BASE == "CENSO") %>%
    select(-BASE)
  
  return(list(
    oficina = datos_oficina,
    censo = datos_censo
  ))
}