# covidmx 0.2.0.0000

* Agregué `NEWS.md`
* La descarga y lectura ahora es más robusta con funciones para trabajar si descargaste pero se interrumpió el `unzip` o tienes el `csv` pero no lo leíste. Checa `descarga_datos_abiertos` y
`read_datos_abiertos`. 
* La descarga de archivos tiene una nueva estructura inspirada en `pins` que lee de memoria (cache) si no ha pasado más de un día de la descarga, si ya pasó más de un día pero detecta que el archivo en línea es idéntico al que tienes en memoria o si no tienes Internet. 
* Se cambió `rt` por `estima_rt` para no ocasionar problemas con la distribución de Student en `stats::rt`.
