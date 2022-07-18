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
