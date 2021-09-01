# Corre xgboost en modo batch, para dejarlo y despreocuparse por eso
#
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
library(xgboost)

library(doParallel)
all_cores <- parallel::detectCores(logical = FALSE)
registerDoParallel(cores = all_cores - 2)

set.seed(4321)

# Lectura de datos ------------------------------------------------------------
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


# Limpiar -----------------------------------------------------------------

a_excluir <- c("100","CAC")
datos <- datos %>%
  mutate(
    tipo = factor(if_else(
      area  %in% a_excluir,"otros",area))
  )

n_clases <- datos %>%
  distinct(tipo) %>%
  tally() %>%
  pull()


# División ----------------------------------------------------------------

division <- initial_split(datos, strata = tipo)
entrenamiento <- training(division)
prueba <- testing(division)
pliegos <- vfold_cv(entrenamiento,v = 5,strata = tipo)


# Especificación del modelo -----------------------------------------------

xgb_spec <- boost_tree(
  trees = 500,
  tree_depth = tune(), min_n = tune(),
  loss_reduction = tune(),                     ## first three: model complexity
  sample_size = tune(), mtry = tune(),         ## randomness
  learn_rate = tune(),                         ## step size
) %>%
  set_engine("xgboost",objective = "multi:softprob",
             num_class = n_clases, verbose = 2) %>%
  set_mode("classification")



# Especificación de la grid -----------------------------------------------

xgb_grid <- grid_latin_hypercube(
  tree_depth(),
  min_n(),
  loss_reduction(),
  sample_size = sample_prop(),
  finalize(mtry(), entrenamiento),
  learn_rate(),
  size = 15
)


# Receta ------------------------------------------------------------------

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

receta_dummy <- receta %>%
  step_upsample(tipo, over_ratio = 1) %>%
  step_other(org_interesada,
             mime_type,
             threshold = 0.01,
             other = "otro") %>%
  step_dummy(org_interesada) %>%
  step_dummy(mime_type) %>%
  step_normalize(size) %>%
  step_tokenize(c(name,nombre_fichero)) %>%
  step_stopwords(c(name, nombre_fichero),language = "es") %>%
  step_tokenfilter(c(name,nombre_fichero),max_tokens = 10) %>%
  step_tfidf(c(name,nombre_fichero))


asi_queda <- prep(receta_dummy) %>% bake(entrenamiento)


# Definir el flujo --------------------------------------------------------

flujo <- workflow() %>%
  add_model(xgb_spec) %>%
  add_recipe(receta_dummy)


# Ajustar el modelo -------------------------------------------------------

tic()
xgb_res <- tune_grid(
  flujo,
  resamples = pliegos,
  grid = xgb_grid,
  control = control_grid(save_pred = TRUE, verbose = TRUE)
)
toc()


# Recoger métricas --------------------------------------------------------

collect_metrics(xgb_res)


# Seleccionar el mejor modelo ---------------------------------------------

best_auc <- select_best(xgb_res, "roc_auc")


# Finalizar ---------------------------------------------------------------


final_xgb <- finalize_workflow(
  xgb_wf,
  best_auc
)


# Con el conjunto de prueba -----------------------------------------------


final_res <- last_fit(final_xgb, division)

final_res_metrics <- collect_metrics(final_res)
final_res_predictions <- collect_predictions(final_res)
correlacion_matthews <- mcc(final_res_predictions,truth = tipo, estimate = .pred_class)
final_res_metrics <- bind_rows(final_res_metrics, correlacion_matthews) %>%
  select(-.config)

write_csv(final_res_metrics,"ficha_metadatos_xgboost.csv")

modelo <- final_xgb %>%
  fit(data = entrenamiento)

saveRDS(mejor, file = "modelo_metadata_xgboost.rds")
