df <- read.csv("C:/Users/jmleo/Downloads/Inventarios_2024.csv", 
               stringsAsFactors = FALSE,
               check.names = FALSE,
               encoding = "latin1") %>%
  mutate(mes = as.Date(mes, format = "%d/%m/%Y"),
         costo_menos_2porc = as.numeric(costo_menos_2porc))

H <- df %>%
  group_by(semana, descripcion) %>%
  summarise(consumo_teorico = sum(consumo_teorico, na.rm = TRUE), .groups = 'drop') %>%
  filter(descripcion == "ACEITE")
  arrange(semana)

str(df)
