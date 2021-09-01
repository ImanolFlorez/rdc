
# Clasificación automática de Documentos

## Ruta del Cacao

### Ejecutar el modelo final

Requisitos:

  - Tener instalado R con por lo menos los siguientes paquetes o
    librerías:
    
      - readr
      - janitor
      - tidymodels
      - textrecipes
      - readxl
      - dplyr
      - lubridate

  - `modelo.Rdata`: el binario del modelo

  - El archivo Excel de los metadatos correspondientes
    (`RDC_2019_V1.xlsx`) debe estar almacenados en una sub-carpeta
    `\datos` en el directorio de trabajo

  - El script `generar_datos.R` produce una muestra aleatoria de n
    números del archivo Excel y los guarda en un archivo de nombre
    `muestra.csv` para obtener fácilmente datos de prueba para ejecutar
    el modelo. Por ejemplo, si se quiere obtener 10 documentos para
    probar el algoritmo se escribiría en la terminal:

<!-- end list -->

``` bash
Rscript generar_datos.R 10
```

  - Para poder ejecutar este ejemplo se ejecuta `modelo_servido.R`: un
    script en R que recibe un .csv con datos de entrada de metadatos de
    RDC (en este ejemplo, `muestra.csv`) y produce como salida un .csv
    con los datos clasificados (en este ejemplo, `resultado.csv`). Para
    ejecutarlo se escribe en la terminal (o se hace una llamada al
    sistema en el lenguaje de programación en donde se este ejecutando)

<!-- end list -->

``` bash
Rscript modelo_servido.R muestra.csv resultado.csv
```
