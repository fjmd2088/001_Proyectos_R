# Ajustes de formato

ORIGINAL$Mes <- str_to_title(ORIGINAL$Mes) # Se homologa los meses

# Se encuentran los valores que no concuerdan y hay que modificar
a <- unique(ORIGINAL$Distrito)
b <- unique(catalogo$Distrito)
val_distintos <- a[!a %in% b]

# Distrito 1-A
ind <- which(ORIGINAL$Distrito %in% c("1A","01-A"))
ORIGINAL$Distrito[ind] <- "1-A"

# Distrito 9
ind <- which(ORIGINAL$Distrito %in% c("NUEVE"))
ORIGINAL$Distrito[ind] <- "9"

# Distrito 8
ind <- which(ORIGINAL$Distrito %in% c("OCHO"))
ORIGINAL$Distrito[ind] <- "8"

# Distrito 34-A
ind <- which(ORIGINAL$Distrito %in% c("34A"))
ORIGINAL$Distrito[ind] <- "34-A"

# Distrito 39-A
ind <- which(ORIGINAL$Distrito %in% c("39A"))
ORIGINAL$Distrito[ind] <- "39-A"

