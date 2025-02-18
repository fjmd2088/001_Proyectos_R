
cat("Procesando CONTROL \n")

# SE INFORMACION PARA LA PESTAÑA DE CONTROL ----------------------------------------------------
AUX1 <- HOJA_INGRESOS %>% plotly::group_by(`Clave del órgano jurisdiccional`) %>% plotly::summarise("Total" = sum(`Total de ingresos`,na.rm=TRUE),
                                                                                                    "Expedientes recibidos" = sum(`Subtotal de expedientes  recibidos `,na.rm=TRUE),
                                                                                                    "Demandas ingresadas" = sum(`Subtotal de demandas promovidas`,na.rm=TRUE))


AUX2 <- HOJA_TRAMITE %>% plotly::select(3,4,79) %>%
  plotly::filter(`Periodo de reporte de la información (mes/año)` == paste0("Diciembre/",ANIO)) %>% plotly::select(-`Periodo de reporte de la información (mes/año)`)

AUX3 <- HOJA_CONCLUSIONES %>% plotly::select(3,4,5) %>%
  plotly::group_by(`Clave del órgano jurisdiccional`) %>% plotly::summarise("Total de resoluciones" = sum(`Total de resoluciones dictadas en los asuntos`,na.rm=TRUE))


CONTROL <- plyr::join_all(list(AUX1,AUX2,AUX3), by = c("Clave del órgano jurisdiccional"),type = "left") 

CAT <- catalogo %>% plotly::select(1,3,4,5,7,6,9,8) %>% plotly::rename("Clave del órgano jurisdiccional" = "Clave.del.órgano.jurisdiccional") 

CONTROL <- left_join(CONTROL,CAT,by = "Clave del órgano jurisdiccional")
CONTROL$Periodo <- ANIO

CONTROL <- CONTROL[,c("Nombre.del.órgano.jurisdiccional","Clave del órgano jurisdiccional","Distrito","Sede","Clave.del.área.geoestadística.estatal.(AGEE)","Entidad.federativa",
                      "Clave.del.área.geoestadística.municipal.(AGEM)","Municipio/.demarcación.territorial","Periodo","Total","Expedientes recibidos","Demandas ingresadas",
                      "Total de asuntos en trámite ","Total de resoluciones")]


CONTROL$ordenar <- NA

for(i in 1:length(tribunales)){
  # i <- 1
  ind <- which(trimws(CONTROL$Nombre.del.órgano.jurisdiccional) == tribunales[i])
  CONTROL$ordenar[ind] <- i
}

CONTROL <- CONTROL %>% plotly::arrange(ordenar) %>% plotly::select(-ordenar)

write.xlsx(CONTROL,paste0(dir_res_ed,"CONTROL","_",ANIO,".xlsx"))