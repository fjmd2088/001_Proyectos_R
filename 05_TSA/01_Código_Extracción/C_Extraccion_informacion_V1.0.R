
# SCRIPT PARA LA EXTRACCION DE INFROMACION DE LOS REPORTES ESTADISTICOS

# BUCLE SOBRE LOS MESES---------------------------------------------------------
DATA_ANIO <- list()
for(h in 1:length(MESES)){
  # h <- 1
  num_mes <- h
  
  cat("PROCESANDO EL MES DE: ", MESES[h], "\n")
  
  # EXTRACCIÓN DE INFORMACIÓN
  files_mes <- list.files(path = paste0(dir_db,"/",MESES[num_mes],"/"), pattern = ".xls", full.names = TRUE) # Se detecta todos los archivos de la carpeta
  
  # BUCLE SOBRE LOS ARCHIVOS DEL MES--------------------------------------------
  DATA_MES <- list(); ANALISIS_MES <- list()
  for(k in 1: length(files_mes)){
    # k <- 1
    # cat(files_mes[k],"\n")
    
    # Se extrae las hojas involucradas en ED
    hojas_k <- c("Hoja1","Hoja2","Hoja3","Hoja4","Hoja5","Hoja6","Hoja7","Hoja8","Hoja9","Hoja11")
    
    if(length(hojas_k)==10){
      cat("Hojas completas \n")
    }else{
      cat("Hacen falta Hojas \n")
    }
    
    # Analisis sobre el archivo de excel
    aux_file <- unlist(str_split(files_mes[k],pattern = "/"))
    name_file_k <- aux_file[length(aux_file)]
    
    AUX <- as.data.frame(cbind(Archivo = name_file_k, Hojas = hojas_k, ESTATUS = "SI"))
    ANALISIS_MES[[k]] <- AUX
    
    # BUCLE SOBRE LAS HOJAS DEL ARCHIVO-----------------------------------------
    DATOS_EXCEL <- list()
    for(i in 1:length(hojas_k)){
      # i <- 1
      data <- readxl::read_excel(files_mes[k],sheet = hojas_k[i] ,col_names = FALSE)
      cat(paste0("Procesando ",hojas_k[i],"\n"))
      
      #----------------------------------------------------------------------------
      # Se guarda el archivo como lo lee R, para comparación
      #----------------------------------------------------------------------------
      Verificacion(data,Ubicaciones)
      
      # Se extrae la informacion de la hoja asociada
      ind <- which(Ubicaciones$Nombre.Hoja == hojas_k[i])
     
      row_i <- Ubicaciones$Fila[ind]
      col_i <- Ubicaciones$Columna[ind]
      var_i <- Ubicaciones$Variable[ind]
      id_i <- Ubicaciones$ID[ind]
      
      # BUCLE SOBRE LOS DATOS A EXTRAER---------------------------------------
      datos_j <- list()
      for(j in 1:length(row_i)){ 
        # j <- 1
        row_j <- row_i[j]; col_j <- col_i[j]; var_j <- var_i[j]; id_j <- id_i[j]
        # cat(paste0(j),", ")
        
        AUX <- as.data.frame(cbind(id_j,var_j,data[row_j,col_j]))
        colnames(AUX) <- c("ID","Variable","Valor")
        
        datos_j[[j]] <- AUX
      }
      AUX <- rbindlist(datos_j,fill = TRUE)
      
      # Se pone el nombre del archivo asociado
      aux_file <- str_split(files_mes[k],pattern = "/")
      name_file_k <-aux_file[[1]][length(aux_file[[1]])]
      
      AUX$Archivo <- paste0("Archivo_",name_file_k)
      
      DATOS_EXCEL[[i]] <- AUX
      
      
    }
    
    DATOS_EXCEL <- rbindlist(DATOS_EXCEL,fill = TRUE)
    
    DATA_MES[[k]] <- DATOS_EXCEL
    
  }
  
  # SE CONCENTRA TODA LA INFORMACION DEL MES
  DATA_MES <- rbindlist(DATA_MES,fill = TRUE) 
  
  DATA_MES <- DATA_MES %>%
    plotly::select(-Variable)
  
  
  DATA_MES <- spread(DATA_MES,ID,Valor)
  
  DATA_ANALISIS <- rbindlist(ANALISIS_MES, fill = TRUE)
  
  DATA_ANALISIS <- spread(DATA_ANALISIS,key = Archivo,ESTATUS)
  DATA_ANALISIS$Mes <- MESES[h]
  
  C1 <- DATA_MES %>% plotly::select("Archivo","H01_01","H01_03")
  C2 <- DATA_MES %>% plotly::select(-"Archivo",-"H01_01",-"H01_03")
  
  # Se convierten en variables numericas
  DATA_MES <- cbind(C1,setNames(data.frame(lapply(C2, as.numeric)), 
                                 colnames(C2)))
  
  DATA_MES <- DATA_MES %>%
    plotly::rename("Distrito" = "H01_01",
                   "Mes" = "H01_03")
  
  #-------------------------------------------------------------------------------------------------
  
  # SE GUARDA LA INFORMACION
  fwrite(DATA_ANALISIS,paste0(dir_res_ana,"ANALISIS_RE_",MESES[h],"_",ANIO,".csv"))
  fwrite(DATA_MES,paste0(dir_res_men,"ORIGINAL_RE_",MESES[h],"_",ANIO,".csv"))
  
  remove(AUX,C1,C2,data,DATOS_EXCEL,datos_j)
  
}
