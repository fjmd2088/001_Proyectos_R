
# Se genera la información de la hoja de INGRESOS

cat("Procesando HIDROCARBUROS \n")


# Se lee la estructura de datos
ED <- openxlsx::read.xlsx(paste0(dir_ed,files.archivo,".xlsx"),
                                sheet = "Asuntos_hidrocarburos",colNames = FALSE)

ED <- ED %>% plotly::filter(X1 %in% c("NOMBRE_VARIABLE","VARIABLE"))
colnames(ED) <- ED[1,]
ED <- ED[-1,]

VACIADO_ED <- ED

AUX <- as.data.frame(matrix(data = NA, nrow = nrow(data), ncol = ncol(VACIADO_ED)))

for(i in 1:ncol(AUX)){
  # i <- 4
  
  if(i == 1){
    
    colnames(AUX)[i] <- "Archivo"
    AUX$Archivo <- data$Archivo
    
  }
  
  if(i == 2){
    
    colnames(AUX)[i] <- colnames(VACIADO_ED)[i]
    AUX$`Nombre del órgano jurisdiccional` <- data$Nombre.del.órgano.jurisdiccional
    
  }
  
  if(i == 3){
    
    colnames(AUX)[i] <- colnames(VACIADO_ED)[i]
    AUX$`Clave del órgano jurisdiccional` <- data$Clave.del.órgano.jurisdiccional
    
  }
  
  if(i == 4){
    
    colnames(AUX)[i] <- colnames(VACIADO_ED)[i]
    AUX$`Periodo de reporte de la información (mes/año)` <- data$Periodo
    
  }
  
  if(i >= 5){
    
    # SE EXTRAE LA VARIABLE Y SE SEPARA
    var <- str_split(VACIADO_ED[,i],",")
    
    # LONGITUD DE LA VARIABLE
    lon_var <- length(var[[1]])
    
    # SE SELECCIONA LAS COLUMNAS ASOCIADAS PARA LA SUMA
    A <- data %>% plotly::select(all_of(var[[1]]))
    
    # SE GUARDA LA INFORMACION EN LA MATRIZ
    colnames(AUX)[i] <- colnames(VACIADO_ED)[i]
    AUX[,i] <- rowSums(A,na.rm = TRUE)
    
  }
  
  
}


nc <- colnames(AUX)
colnames(AUX) <- paste0("X",1:ncol(AUX))

AUX$ordenar <- NA

for(i in 1:length(tribunales)){
  # i <- 1
  ind <- which(AUX$X2 == tribunales[i])
  AUX$ordenar[ind] <- i
}

AUX <- AUX %>% plotly::arrange(ordenar) %>% plotly::select(-ordenar)

colnames(AUX) <- nc
HOJA_HIDROCARBUROS <- AUX

write.xlsx(HOJA_HIDROCARBUROS,paste0(dir_res_ed,"HOJA_HIDROCARBUROS_",name_file,"_",ANIO,".xlsx"))