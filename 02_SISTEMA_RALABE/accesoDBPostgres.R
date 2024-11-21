
remove(list = ls())

library(RPostgres)
library(pool)
library(tidyverse)

# Configuración de la conexión a PostgreSQL
pool <- dbPool(
  drv = Postgres(),
  host = "localhost",
  dbname = "Ralab",
  user = "postgres",
  password = "inegi",
  port = 5432,
  idleTimeout = 3600,
  minSize = 1,
  maxSize = 5
)

# Consultar los datos de la tabla
tr_expediente <- dbGetQuery(pool,"SELECT * FROM tr_expediente")
sort(names(tr_expediente))

tr_expediente |> select(fecha_apertura_exped,id_tipo_expediente)

tc_procedimiento <- dbGetQuery(pool,"SELECT * FROM tc_procedimiento")

query <- "
  SELECT 
    TO_CHAR(TO_DATE(fecha_apertura_exped, 'YYYY-MM-DD'), 'FMMonth') AS mes, 
    COUNT(*) FILTER (WHERE id_estatus_exped = 1) AS ingreso, 
    COUNT(*) FILTER (WHERE id_estatus_exped = 2) AS tramite, 
    COUNT(*) FILTER (WHERE preg_incompetencia = 'NO' AND id_estatus_demanda = 1 AND id_estatus_exped = 1) AS concluidos 
  FROM tr_expediente 
  WHERE fecha_apertura_exped IS NOT NULL 
  GROUP BY mes 
  ORDER BY EXTRACT(MONTH FROM TO_DATE(fecha_apertura_exped, 'YYYY-MM-DD'))
"
dbGetQuery(pool,query)
