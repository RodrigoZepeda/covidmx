# covidmx

Descarga, etiqueta y datos abiertos de COVID-19 en México. El propósito de este paquete es hacer la descarga, análisis y graficación de manera rápida para que tú no tengas que preocuparte por bajar el archivo a tiempo, agrupar funciones o realizar visualizaciones sino en lo importante: analizar la información. 

> :warning: Necesitas tener una instalación de `MariaDB` o bien muchísima `RAM`. 

# Instalación

```{r}
devtools::install_github("RodrigoZepeda/covidmx")
```

Descarga la base de datos abiertos de la Dirección General de Epidemiología de la Secretaría de Salud.

```{r}
library(covidmx)

#~10 mins
datos_covid <- descarga_datos_abiertos(language = "Español")
```

Por default, no se agregan etiquetas a los datos pues es muy lento hacerlo:

```{r}
datos_covid$dats %>% dplyr::glimpse()

    Rows: ---------
    Columns: 43
    $ FECHA_ACTUALIZACION   <date> 2021-12-20, 2021-12-20, 2021-12-20, 2021-12-20, 20…
    $ ID_REGISTRO           <chr> "z38de4", "z579ac", "z2669f", "z54912", "z35a05", "…
    $ FECHA_INGRESO         <date> 2020-05-23, 2020-10-14, 2020-06-18, 2020-06-12, 20…
    $ FECHA_SINTOMAS        <date> 2020-05-20, 2020-10-10, 2020-06-16, 2020-06-10, 20…
    $ FECHA_DEF             <date> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
    $ EDAD                  <dbl> 7, 33, 43, 56, 40, 67, 58, 32, 37, 64, 62, 71, 67, …
    $ PAIS_NACIONALIDAD     <chr> "México", "México", "México", "México", "México", "…
    $ PAIS_ORIGEN           <chr> "97", "97", "97", "97", "97", "97", "97", "97", "97…
    $ Fuente                <chr> "http://datosabiertos.salud.gob.mx/gobmx/salud/dato…
    $ Fecha_descarga        <chr> "Mon Dec 20 22:39:30 2021", "Mon Dec 20 22:39:30 20…
    $ ORIGEN                <chr> "USMER", "USMER", "FUERA DE USMER", "USMER", "USMER…
    $ SECTOR                <chr> "SSA", "SSA", "SSA", "SSA", "SSA", "SSA", "SSA", "S…
    $ SEXO                  <chr> "MUJER", "HOMBRE", "HOMBRE", "MUJER", "MUJER", "MUJ…
    $ TIPO_PACIENTE         <chr> "AMBULATORIO", "AMBULATORIO", "AMBULATORIO", "AMBUL…
    $ INTUBADO              <chr> "NO APLICA", "NO APLICA", "NO APLICA", "NO APLICA",…
    $ NEUMONIA              <chr> "NO", "NO", "NO", "NO", "NO", "NO", "NO", "NO", "NO…
    $ EMBARAZO              <chr> "NO", "NO APLICA", "NO APLICA", "NO", "NO", "NO", "…
    $ HABLA_LENGUA_INDIG    <chr> "NO", "NO", "NO", "NO", "SI", "NO", "NO", "NO", "NO…
    $ INDIGENA              <chr> "NO", "NO", "NO", "NO", "SI", "NO", "NO", "NO", "NO…
    $ DIABETES              <chr> "NO", "NO", "NO", "SI", "NO", "NO", "SI", "NO", "NO…
    $ EPOC                  <chr> "NO", "NO", "NO", "NO", "NO", "NO", "NO", "NO", "NO…
    $ ASMA                  <chr> "NO", "NO", "SI", "NO", "NO", "NO", "NO", "NO", "NO…
    $ INMUSUPR              <chr> "NO", "NO", "NO", "NO", "NO", "NO", "NO", "NO", "NO…
    $ HIPERTENSION          <chr> "NO", "NO", "NO", "NO", "NO", "NO", "NO", "NO", "NO…
    $ OTRA_COM              <chr> "NO", "NO", "NO", "NO", "NO", "NO", "NO", "NO", "NO…
    $ OBESIDAD              <chr> "NO", "NO", "NO", "NO", "SI", "NO", "NO", "NO", "NO…
    $ RENAL_CRONICA         <chr> "NO", "NO", "NO", "NO", "NO", "NO", "NO", "NO", "NO…
    $ TABAQUISMO            <chr> "NO", "NO", "SI", "NO", "NO", "NO", "NO", "NO", "NO…
    $ MIGRANTE              <chr> "NO ESPECIFICADO", "NO ESPECIFICADO", "NO ESPECIFIC…
    $ UCI                   <chr> "NO APLICA", "NO APLICA", "NO APLICA", "NO APLICA",…
    $ CARDIOVASCULAR        <chr> "NO", "NO", "NO", "NO", "NO", "NO", "NO", "NO", "NO…
    $ OTRO_CASO             <chr> "NO", "NO", "SI", "NO", "NO", "NO", "NO", "NO", "NO…
    $ TOMA_MUESTRA_LAB      <chr> "NO", "SI", "SI", "SI", "SI", "NO", "SI", "SI", "SI…
    $ TOMA_MUESTRA_ANTIGENO <chr> "NO", "NO", "NO", "NO", "NO", "NO", "NO", "NO", "NO…
    $ MUNICIPIO_RES         <chr> "TUXTLA GUTIÉRREZ", "NICOLÁS ROMERO", "GUADALAJARA"…
    $ ENTIDAD_UM            <chr> "CHIAPAS", "MÉXICO", "JALISCO", "YUCATÁN", "YUCATÁN…
    $ ENTIDAD_NAC           <chr> "CHIAPAS", "MÉXICO", "JALISCO", "YUCATÁN", "YUCATÁN…
    $ ENTIDAD_RES           <chr> "CHIAPAS", "MÉXICO", "JALISCO", "YUCATÁN", "YUCATÁN…
    $ NACIONALIDAD          <chr> "MEXICANA", "MEXICANA", "MEXICANA", "MEXICANA", "ME…
    $ RESULTADO_LAB         <chr> "NO APLICA (CASO SIN MUESTRA)", "RESULTADO NO ADECU…
    $ RESULTADO_ANTIGENO    <chr> "NO APLICA (CASO SIN MUESTRA)", "NO APLICA (CASO SI…
    $ CLASIFICACIÓN         <chr> "CASO SOSPECHOSO", "CASO SOSPECHOSO", "NEGATIVO A S…
    $ DESCRIPCIÓN           <chr> "Sospechoso aplica cuando: \r\nEl caso no tienen as…
```

sin embargo el diccionario se incluye en la lista devuelta

```{r}
datos_covid$dict
```

Si ya los descargaste una vez puedes usar la siguiente función para leerlos de memoria: 

```{r}
datos_covid$dict <- read_datos_abiertos()
```

# Introducción rápida

Obtener casos agrupados por estado

```{r}
datos_covid %>% casos()

#> # A tibble: 
#>   FECHA_SINTOMAS ENTIDAD_UM       n ENTIDAD_FEDERATIVA   ABREVIATURA
#>   <date>         <chr>      <int64> <chr>                <chr>      
#> 1 2020-01-01     01               5 AGUASCALIENTES       AS         
#> 2 2020-01-01     02               4 BAJA CALIFORNIA      BC         
#> 3 2020-01-01     03               4 BAJA CALIFORNIA SUR  BS         
#> 4 2020-01-01     05               6 COAHUILA DE ZARAGOZA CL         
#> 5 2020-01-01     06               3 COLIMA               CM         
#> 6 2020-01-01     08              11 CHIHUAHUA            CH
```

o bien sólo para algunas entidades de la unidad médica:

```{r}
datos_covid %>% 
  casos(entidades = c("QUINTANA ROO","AGUASCALIENTES"))
  
#> # A tibble:
#>   FECHA_SINTOMAS ENTIDAD_UM       n ENTIDAD_FEDERATIVA ABREVIATURA
#>   <date>         <chr>      <int64> <chr>              <chr>      
#> 1 2020-01-01     01               5 AGUASCALIENTES     AS         
#> 2 2020-01-01     23               7 QUINTANA ROO       QR         
#> 3 2020-01-02     23               9 QUINTANA ROO       QR         
#> 4 2020-01-03     01               1 AGUASCALIENTES     AS         
#> 5 2020-01-03     23              11 QUINTANA ROO       QR         
#> 6 2020-01-04     01               2 AGUASCALIENTES     AS  
```

Para evitar agrupar por estado y que se generen los nacionales:

```{r}
datos_covid %>% 
  casos(group_by_entidad = FALSE)
  
#> # A tibble: 
#>   FECHA_SINTOMAS   n 
#>   <date>         <int64> 
#> 1 2020-01-01       10  
#> 2 2020-01-02       14  
#> 3 2020-01-03       22  
#> 4 2020-01-04       13  
#> 5 2020-01-05       15  
#> 6 2020-01-06       18
```

Todas las opciones:

```{r}
datos_covid %>% 
  casos(
    #Lista de entidades que deseas
    entidades = c("AGUASCALIENTES", "BAJA CALIFORNIA", 
                  "BAJA CALIFORNIA SUR","CAMPECHE", "CHIAPAS", 
                  "CHIHUAHUA","CIUDAD DE M\u00c9XICO",
                  "COAHUILA DE ZARAGOZA" , "COLIMA", "DURANGO", 
                  "GUANAJUATO", "GUERRERO","HIDALGO", "JALISCO", 
                  "M\u00c9XICO", "MICHOAC\u00c1N DE OCAMPO", 
                  "MORELOS","NAYARIT", "NUEVO LE\u00d3N", "OAXACA", 
                  "PUEBLA", "QUER\u00c9TARO", "QUINTANA ROO", 
                  "SAN LUIS POTOS\u00cd", "SINALOA", "SONORA",
                  "TABASCO", "TAMAULIPAS", "TLAXCALA", 
                  "VERACRUZ DE IGNACIO DE LA LLAVE", 
                  "YUCAT\u00c1N", "ZACATECAS"),
    
    #Si quieres que los resultados salgan por entidad = TRUE o ya agregados = FALSE
    group_by_entidad    = TRUE,
    
    #Selecciona esas entidades a qué tipo de entidad refieren: Unidad Médica, 
    #Residencia o Nacimiento
    entidad_tipo        = "Residencia", 
    
    #Selecciona la fecha para la base de datos: Síntomas, Ingreso, Defunción
    fecha_tipo          = "Ingreso",
     
    #Selecciona todas las variables de clasificación que deseas agregar:
    tipo_clasificacion  = c("Sospechosos","Confirmados COVID", 
                            "Negativo a COVID", "Inválido", 
                            "No realizado"),
    
    #Selecciona si deseas agrupar por la variable tipo_clasificacion
    group_by_tipo_clasificacion = TRUE,
    
    #Selecciona todos los pacientes quieres incluir:
    tipo_paciente      = c("AMBULATORIO", "HOSPITALIZADO", 
                           "NO ESPECIFICADO"),
    
    #Selecciona si agrupar por tipo de paciente
    group_by_tipo_paciente = TRUE,
    
    #Selecciona todas las opciones de Unidad de Cuidado Intensivo
    #del paciente:
    tipo_uci           = c("SI","NO","NO APLICA","SE IGNORA",
                           "NO ESPECIFICADO"),
    
    #Selecciona si agrupar por tipo de unidad
    group_by_tipo_uci  = TRUE,
    
    #Selecciona los sectores del sistema de salud a incluir
    tipo_sector   = c("CRUZ ROJA", "DIF", "ESTATAL", "IMSS", 
                      "IMSS-BIENESTAR", "ISSSTE", 
                      "MUNICIPAL", "PEMEX", "PRIVADA", 
                      "SEDENA", "SEMAR", "SSA", 
                      "UNIVERSITARIO","NO ESPECIFICADO"),
    
    #Selecciona si deseas agrupar por tipo de sector
    group_by_tipo_sector = FALSE,
    
    #Selecciona si deseas sólo los que tuvieron defunción
    defunciones   = TRUE,
    
    #Selecciona los grupos de edad que deseas incluir en rango
    edad_cut      = c(20, 40, 60), #Edades 20-40 y 40-60
    
    #Selecciona si devolver el objeto como tibble
    as_tibble     = TRUE,
    
    #Selecciona si rellenar los conteos (n) con ceros 
    #cuando no haya observaciones.
    fill_zeros    = TRUE,
    
    #Nombre para llamarle en el objeto lista que regresa
    list_name     = "Ejemplo defunciones",
    
    #Otras variables para agrupar no incluidas
    .grouping_vars = c("DIABETES", "SEXO"))
    
#> # A tibble: 
#>    FECHA_INGRESO DIABETES  SEXO EDAD_CAT ENTIDAD_RES CLASIFICACION_FINAL
#>    <date>           <dbl> <dbl> <chr>    <chr>                     <dbl>
#>  1 2020-01-01           2     1 (40,60]  30                            7
#>  2 2020-01-02           2     1 (40,60]  30                            7
#>  3 2020-01-02           2     2 (20,40]  11                            7
#>  4 2020-01-02           2     2 (20,40]  26                            7
#>  5 2020-01-02           2     2 (40,60]  22                            5
#>  6 2020-01-03           1     1 (40,60]  05                            7
#>  7 2020-01-03           1     2 (40,60]  26                            7
#>  8 2020-01-03           1     2 (40,60]  28                            7
#>  9 2020-01-03           2     1 (40,60]  15                            6
#> 10 2020-01-03           2     2 (40,60]  13                            7
#> 11 2020-01-04           2     1 (20,40]  05                            7
#> 12 2020-01-04           2     2 (40,60]  21                            7
#> 13 2020-01-05           1     1 (40,60]  30                            7
#> 14 2020-01-05           1     2 (40,60]  09                            5
#> 15 2020-01-05           2     2 (20,40]  26                            6
#> 16 2020-01-05           2     2 (40,60]  28                            7
#> 17 2020-01-06           1     1 (40,60]  02                            7
#> 18 2020-01-06           1     2 (40,60]  15                            7
#> 19 2020-01-06           2     1 (40,60]  08                            6
#> 20 2020-01-06           2     1 (40,60]  09                            7
#> # … with 8 more variables: TIPO_PACIENTE <dbl>, UCI <dbl>, n <int64>,
#> #   ENTIDAD_FEDERATIVA <chr>, ABREVIATURA <chr>, CLASIFICACIÓN <chr>,
#> #   DESCRIPCION_TIPO_PACIENTE <chr>, DESCRIPCION_TIPO_UCI <chr> 
```


