
# --------------------------------------------------------------------------------------------------
# ANALISIS REPORTES ESTADISTICOS
# --------------------------------------------------------------------------------------------------

files_mes <- list.files(path = paste0(dir_res_men), pattern = ".csv", full.names = TRUE) 

ORIGINAL <- list()
for(m in 1:length(files_mes)){
  # m <- 1
  A <- fread(files_mes[m])
  ORIGINAL[[m]] <- A
}

ORIGINAL <- rbindlist(ORIGINAL,fill = TRUE)


# CRUCE CONTRA CATALOGO DE ORGANO JURISDICCIONAL
CRUCE <- left_join(ORIGINAL,catalogo,by = c("Distrito"))

CRUCE$Periodo <- paste0(CRUCE$Mes,"/",ANIO)

fwrite(CRUCE,paste0(dir_res,"01_CONCENTRADO_ORIGINAL_RE_",ANIO,".csv")) # SE GUARDA LA INFORMACION INTACTA

#--------------------------------------------------------------------------------------------------------
# Modificaciones sobre los tribunales
#--------------------------------------------------------------------------------------------------------
source("F_Modificaciones_TUA_1.R")

# CRUCE CONTRA CATALOGO DE ORGANO JURISDICCIONAL----------------------------------------------------
CRUCE2 <- left_join(ORIGINAL,catalogo,by = c("Distrito"))

CRUCE2$Periodo <- paste0(CRUCE2$Mes,"/",ANIO)

source("G_Modificaciones_TUA_2.R")

fwrite(CRUCE2,paste0(dir_res,"02_CONCENTRADO_MODIFICADO_RE_",ANIO,".csv"))

# --------------------------------------------------------------------------------------------------
# ANALISIS ADICIONAL
# --------------------------------------------------------------------------------------------------

# Hay duplicados en la información

distritos <- unique(CRUCE2$Distrito)

for(i in 1:length(distritos)){
  # i <- 2
  
  A <- CRUCE2 %>% plotly::filter(Distrito == distritos[i])
  
  res <- unique(duplicated(A$Mes))
  
  
  if(length(res) > 1 ){
    cat("Información duplicada en el mes para el distrito ", distritos[i], "\n\n")
    B <- A[,1:3]
    print(B)
    cat("\n\n")
  }
  
}


