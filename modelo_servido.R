args <- commandArgs(trailingOnly = TRUE)
#print(args)

#' Predice con el modelo de metadatos de actren
#'
#' Asume que el modelo fue creado previamente y esta almacenado en un archivo
#' "modelo_metadata.rds"
#'
#' @param datos nombre del archivo .csv con los datos a predecir
#' @param resultados nombre del archivo .csv con los resultados de las predicciones
#'
#' @return Escribe archivo con los resultados de las predicciones
#'
predecir_metadatos <- function(datos_file,resultados_file) {
library(readr)
library(janitor)
library(tidymodels)
library(textrecipes)
library(readxl)
library(dplyr)
library(lubridate)

### Procesar y asignarle el tipo a los datos crudos ----------------------------
a_excluir <- c("100","CAC")

muestra <- read_csv(datos_file)  %>%
  clean_names() %>%
  select(code,
         modo,
         name,
         datecreated,
         datemodified,
         author,
         org_interesada_remitente,
         org_interesada_destinatario,
         nombre_fichero,
         mimetype,
         size,
         path,
         area,
         nombre_tipo_documental,
         nombre_padre,
         nombre_abuelo,
         en_expte,
         nombre_serie_documental) %>%
  unite(org_interesada,org_interesada_remitente:org_interesada_destinatario,
        remove = TRUE,
        na.rm = TRUE) %>%
  mutate(
    date_created = ymd_hms(datecreated,tz = "UTC") - hours(5),
    date_modified = ymd_hms(datemodified,tz = "UTC") - hours(5)
  ) %>%
  mutate(
    mes_creacion = month(datecreated,label = FALSE,abbr = FALSE),
    dia_semana_creacion = wday(datecreated,label = FALSE,
                               week_start = 1,abbr = FALSE),
    dia_creacion = day(datecreated),
    hora_creacion = hour(datecreated),
    mes_modificacion = month(datemodified,label = FALSE,abbr = FALSE),
    dia_semana_modificacion = wday(datemodified,label = FALSE,
                                   week_start = 1,abbr = FALSE),
    dia_modificacion = day(datemodified),
    hora_modificacion = hour(datemodified)) %>%
  rename(mime_type = mimetype) %>%
  mutate(area = as.character(area)) %>%
  # Ya que al predecir nuevos datos area es NA, no se puede incluir na.omit()
  # aqui:
  #na.omit() %>%
  mutate(
    tipo = factor(if_else(
      area  %in% a_excluir,"otros",area))
  )


### Obtener modelo -------------------------------------------------------------
modelo <- readRDS("modelo_metadata.rds")

### Obtener predicciones -------------------------------------------------------
clase <- predict(modelo,new_data = muestra, type = "class")
probabilidad <- predict(modelo,new_data = muestra, type = "prob")


### Calcular y escribir resultados ---------------------------------------------
resultados <- cbind.data.frame(clase,probabilidad)
write_csv(resultados,file = resultados_file)

}


### LLamar a la funcion con el nombre de los parametros dados ------------------
entrada <- args[1]
salida <- args[2]

predecir_metadatos(entrada,salida)
