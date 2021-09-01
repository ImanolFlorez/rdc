#' Produce el binario del modelo de metadatos para RDC
#'
#' Asume una carpeta "datos/RDC_2019_V1.xlsx" con los datos que se usan para
#'    ajustar el modelo
#' Devuelve un archivo `modelo_medatados.rds` con el binario del modelo
#'


### Librerías ------------------------------------------------------------------
library(dplyr)
library(janitor)
library(readxl)
library(skimr)
library(gt)
library(stringr)
library(text2vec)
library(LDAvis)
library(tidyr)
library(ggplot2)
library(lubridate)
library(stringr)
library(tidymodels)
library(tidytext)
library(stopwords)
library(purrr)
library(textrecipes)
library(discrim)
library(vip)
library(themis)
library(tictoc)
library(ranger)
library(readr)

library(doParallel)
all_cores <- parallel::detectCores(logical = FALSE)
registerDoParallel(cores = all_cores)

set.seed(4321)

### Leer y limpiar -------------------------------------------------------------
crudos <- read_excel("datos/RDC_2019_V1.xlsx") %>%
  clean_names()

datos <- crudos  %>%
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
  na.omit()



### Definir tipo ---------------------------------------------------------------

a_excluir <- c("100","CAC")
datos <- datos %>%
  mutate(
    tipo = factor(if_else(
      area  %in% a_excluir,"otros",area))
  )


### Division entrenamiento, prueba ---------------------------------------------
division <- initial_split(datos, strata = tipo)
entrenamiento <- training(division)
prueba <- testing(division)
pliegos <- vfold_cv(entrenamiento,strata = tipo)


### Especificación del modelo --------------------------------------------------
# Esta es la de random forest, la activa por el momento

rf_spec <- rand_forest() %>%
  set_args(trees = 500) %>%
  set_engine("ranger", importance = "impurity") %>%
  set_mode("classification")

### Receta ---------------------------------------------------------------------
receta <- recipe(tipo ~
                   org_interesada +
                   mime_type +
                   size +
                   name +
                   nombre_fichero +
                   mes_creacion +
                   dia_semana_creacion +
                   dia_creacion +
                   hora_creacion +
                   mes_modificacion +
                   dia_semana_modificacion +
                   dia_modificacion +
                   hora_modificacion,
                 data = entrenamiento)


receta_no <- receta %>%
  step_upsample(tipo, over_ratio = 1) %>%
  step_normalize(size) %>%
  step_other(org_interesada,
             mime_type,
             threshold = 0.01,
             other = "otro") %>%
  step_tokenize(c(name,nombre_fichero)) %>%
  step_stopwords(c(name, nombre_fichero),language = "es") %>%
  step_tokenfilter(c(name,nombre_fichero)) %>%
  step_tfidf(c(name,nombre_fichero))

### Definir flujo --------------------------------------------------------------
flujo <- workflow() %>%
  add_model(rf_spec) %>%
  add_recipe(receta_no)

### Obtener performance del modelo ---------------------------------------------

final_rf <- flujo %>%
  last_fit(split = division)

final_res_metrics <- collect_metrics(final_rf)
final_res_predictions <- collect_predictions(final_rf)
correlacion_matthews <- mcc(final_res_predictions,truth = tipo, estimate = .pred_class)
final_res_metrics <- bind_rows(final_res_metrics, correlacion_matthews) %>%
  select(-.config)

write_csv(final_res_metrics,"ficha_metadatos.csv")

### Guardar modelo -------------------------------------------------------------
# mejor <- flujo %>%
#   fit(data = entrenamiento)

# Esta forma es mejor:
modelo <- final_rf$.workflow[[1]]

saveRDS(modelo, file = "modelo_metadata.rds")
