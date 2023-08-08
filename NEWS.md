# covidmx 0.7.8

* Se corrigió que la lectura de datos abiertos no funcionaba con el nuevo archivo `.zip`

# covidmx 0.7.7

* Se cambió `donttest` issue #15 donde generaba error en el (check para release)[https://github.com/RodrigoZepeda/covidmx/issues/15]. 

# covidmx 0.7.6

* Se arregló el `Suggests` de `tidyverse` a solicitud de correo masivo de RStudio

# covidmx 0.7.5

* Se arregló #13 permitiendo ahora que los marcados con edad de cero años aparezcan. Los cortes de fecha ahora incluyen el límite inferior. 

# covidmx 0.7.4

* Se corrige el issue #14 [que devolvía agrupados los dataframes al correr casos y positividad](https://github.com/RodrigoZepeda/covidmx/issues/14).

* Se corrigió que arrojara problemas de lectura en la fecha al no poder parsear fechas del estilo 0001-11-03 al leer como `tibble`. Ahora dichas fechas son eliminadas automáticamente. 

# covidmx 0.7.3

* Se corrige el issue #12 [cannot open file 'NA/2022.csv': No such file or directory](https://github.com/RodrigoZepeda/covidmx/issues/12).

* Se actualizó la versión de roxygen2: `RoxygenNote: 7.2.3`. 

# covidmx 0.7.2

* Segundo intento de enviar a CRAN. 

# covidmx 0.7.1.2000

* Se agregaron los cambios de CRAN:
  + Quitar los cambios en `options`
  + Se agregó el `value` tag en `update_covidmx`
  + Se agregaron links en `DESCRIPTION`. 
  + Se arregló un error de encoding al leer como `tibble`.
  + Se arreglaron los `dontrun` excepto para `update_covidmx` donde sí es un `dontrun`.

# covidmx 0.7.1.1000

* Se cambió la descripción. 
* Se envió a CRAN. 

# covidmx 0.7.1.0000

* Se cambió la base de ejemplo por una más pequeña y con mejor compresión

# covidmx 0.7.0.2000

* Se arreglaron las notas sobre el manual del pdf en LaTeX
* Se quitaron el README.md y el NEWS.md del build
* Se arreglo el archivo que no estaba guardado como non-ascii

# covidmx 0.7.0.1000

* Se arreglaron los headings del `README.md`
* Se arregló que la función `update_covidmx` no se exportaba. 

# covidmx 0.7.0.0000

* Se arregló un `bug` que ocasionaba que `cfr` y `chr` regresaran `NaN` en lugar de `NA` cuando
usabas la opción `fill_NA`. 

* Se arregló que los casos eran un `tibble` vacío si no se reportaban casos con esas 
condiciones a pesar del `fill_zeros = TRUE`. Por ejemplo esto antes devolvía:

```{r}
datos_covid <- datosabiertos
datos_covid <- datos_covid |> casos(tipo_sector = "DIF", fill_zeros = TRUE)
datos_covid$casos

#ANTES (ERROR)
# A tibble: 0 × 5
# … with 5 variables: FECHA_SINTOMAS <dttm>, ENTIDAD_UM <chr>, n <int>, ENTIDAD_FEDERATIVA <chr>,
#   ABREVIATURA <chr>
# ℹ Use `colnames()` to see all variable names

#AHORA
# A tibble: 126 × 5
#   FECHA_SINTOMAS      ENTIDAD_UM     n ENTIDAD_FEDERATIVA  ABREVIATURA
#   <dttm>              <chr>      <int> <chr>               <chr>      
# 1 2021-07-01 00:00:00 02             0 BAJA CALIFORNIA     BC         
# 2 2021-07-01 00:00:00 03             0 BAJA CALIFORNIA SUR BS         
# 3 2021-07-02 00:00:00 02             0 BAJA CALIFORNIA     BC         
# 4 2021-07-02 00:00:00 03             0 BAJA CALIFORNIA SUR BS         
# 5 2021-07-03 00:00:00 02             0 BAJA CALIFORNIA     BC    
```

* Se mejoró la selección de variables automática en `plot_covid` para evitar que una `df_covariate`
sea tambien una `df_variable`.

* Se arregló un `bug` que impedía la agrupación por otras covariables de `cfr` y `chr`. 

# covidmx 0.6.2.1000

* Se mejoraron los ejemplos y la ayuda de `casos`.

# covidmx 0.6.2.0000

* Se corrigieron los menús de ayuda para ser más informativos.
* Se agrego la funcion `update_covidmx` para actualizar desde github. 
* Se cambio el mensaje de inicio. 

# covidmx 0.6.1.2000

* Se corrigieron los menús de ayuda para ser más informativos.

# covidmx 0.6.1.1000

* Se agregan preguntas al FAQ. 


# covidmx 0.6.1.0000

* Se agregó IMSS a la licencia.
* Se arregló un test que se rompía en windows relacionado con [este issue de duckdb](https://github.com/duckdb/duckdb/issues/77).

# covidmx 0.6.0.0000

## Breaking changes

* Se eliminó `MariaDB` y ahora se utiliza `duckdb`. 
* Se eliminó el requerimiento de instalar herramientas para abrir el `zip`. 
* Se eliminó la dependencia de `glue` y se agregó una de `cli`. 

# covidmx 0.5.1.0000
* Se eliminó el chequeo de `MariaDBhasDefault` para los sistemas donde hay conexión
a pesar de no tener el default. 

# covidmx 0.5.0.0000
* Se agregó la nueva variable `driver` a las conexiones a `MariaDB` para permitir otro tipo de `SQL`.
* Se agregó la variable `sqlimport` para que eventualmente se pueda cambiar `mysqlimport` a `mariadb-import` por el cambio en `MariaDB` [acá](https://mariadb.com/kb/en/mysqlimport/).
* Se arregló un error donde no se asignaba bien el `cache` del diccionario. 

# covidmx 0.4.2.0000
* Se arregló el `bug` que no eliminaba la descarga en `csv` si ésta se parseaba en `tibble`. 

# covidmx 0.4.1.0000
* Se arregló el `bug` que impedía seleccionar solo `Antígeno` como pruebas en `numero_pruebas`
* Se arregló el `bug` que impedía seleccionar solo `Antígeno` como pruebas en `positividad`
* Se arregló el `bug` que impedía seleccionar  `Defuncón` como `fecha_tipo` en `numero_pruebas`
* Se agregó la opción de `quiet` a `positividad`

# covidmx 0.4.0.0000

* Se arregló `bug` que al filtrar por `IMSS` devolvía `IMSS-BIENESTAR` en `tipo_sector`
* Se arregló `bug` que al filtrar por `NO` devolvía `NO ESPECIFICADO` en `tipo_paciente`
* Se agregaron `tests` y se conectò a `codecov`. 

# covidmx 0.3.0.0000

* Se arregló un `bug` que al filtrar por `BAJA CALIFORNIA` también devolvía `BAJA CALIFORNIA SUR` en `casos`. 
* Se agregó un dataset `datosabiertos` para poder probar funciones sobre ese data. 
* Se cambió el tutorial a una tabla llamada `tblname` para que sea más rápido que la descarga y creación del repositorio de Github.  
* Se agregó el parámetro `max_date` a `estima_rt` para mejorar la estimación de la ventana de tiempo del `RT`. 
* Se eliminó que `casos` por default descargue los datos si no tiene un input o si su input es vacío pues generaba un `bug` cuando el elemento de la lista no estaba. 
* Se agregó un mensaje `onAttach` y se eliminaron mensajes al descargar archivos.

# covidmx 0.2.0.0000

* Agregué `NEWS.md`
* La descarga y lectura ahora es más robusta con funciones para trabajar si descargaste pero se interrumpió el `unzip` o tienes el `csv` pero no lo leíste. Checa `descarga_datos_abiertos` y
`read_datos_abiertos`. 
* La descarga de archivos tiene una nueva estructura inspirada en `pins` que lee de memoria (cache) si no ha pasado más de un día de la descarga, si ya pasó más de un día pero detecta que el archivo en línea es idéntico al que tienes en memoria o si no tienes Internet. 
* Se cambió `rt` por `estima_rt` para no ocasionar problemas con la distribución de Student en `stats::rt`.
