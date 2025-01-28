

# Ajustes de las observaciones proporcionadas por TUA-------------------------------------------------------------------------------------------------

# 1.- Durante 2023 existieron los Tribunales Unitarios distritos 14 Pachuca (CVE= 1413048), Hidalgo y 43 Tampico (CVE = 4328038),
#     Tamaulipas  en el periodo enero a octubre 2023.
# 2. A partir de noviembre los mencionados Tribunales  cambian de sede y se convierten en distrito 14 Huejutla de Reyes, hidalgo (CVE = 1413028) 
#    y 43 Cuidad Valles Sal Luis Potosí (CVE = 4324013).


ind1 <- which(CRUCE2$Distrito == "14" & !CRUCE2$Mes %in% c("Noviembre","Diciembre"))
ind2 <- which(CRUCE2$Distrito == "43" & !CRUCE2$Mes %in% c("Noviembre","Diciembre"))

CRUCE2$Clave.del.órgano.jurisdiccional[ind1] <- "1413048" # cve huejutla
CRUCE2$Clave.del.órgano.jurisdiccional[ind2] <- "4328038" # cve ciudad valles
