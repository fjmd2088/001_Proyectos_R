
remove(list = ls())

library(tidyverse)

dirData <- "data/"


nombre_archivo <- "20241118-20241125-payments_order-VIRGINIA_Zuiga_Vidal"

data <- read.csv(paste0(dirData,nombre_archivo,".csv"),header = TRUE)

unique(data$Nombre.del.socio.de.la.App)

dataVictor <- data |> filter(Nombre.del.socio.de.la.App == "Victor Octavio")

suma_columnas_numericas <- sapply(dataVictor[, sapply(dataVictor, is.numeric)], sum)

dataVicky <- data |> filter(Nombre.del.socio.de.la.App == "VIRGINIA")

suma_columnas_numericas_Vicky <- sapply(dataVicky[, sapply(dataVicky, is.numeric)], sum)


# str(dataVictor)

# apply(dataVictor,2,sum)


# summary(dataVictor)
