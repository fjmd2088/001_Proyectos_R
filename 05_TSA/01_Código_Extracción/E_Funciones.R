

Verificacion <- function(data,Ubicaciones){
  ## Create a new workbook
  wb <- createWorkbook(hojas_k[i])
  ## Add a worksheets
  addWorksheet(wb, "Expenditure", gridLines = FALSE)
  
  ## write data to worksheet 1
  writeData(wb, sheet = 1, data, rowNames = FALSE,colNames = FALSE)
  
  ## create and add a style to the column headers
  headerStyle <- createStyle(
    fontSize = 14, fontColour = "black", halign = "center",
    fgFill = "yellow", border = "TopBottom", borderColour = "yellow"
  )
  
  ind <- which(Ubicaciones$Nombre.Hoja == hojas_k[i]) 
  
  # unique(Ubicaciones$Nombre.Hoja)
  
  rowi <- Ubicaciones %>% plotly::filter(Nombre.Hoja == hojas_k[i]) 
  
  for(l in 1 : nrow(rowi)){
    addStyle(wb, sheet = 1, headerStyle, rows = rowi$Fila[l], cols = rowi$Columna[l], gridExpand = TRUE)
  }
  
  saveWorkbook(wb, paste0(dir_lec,hojas_k[i],".xlsx"), overwrite = TRUE) 
  
}

