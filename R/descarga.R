#' Descarga de datos abiertos
#'
#' @description Conjunto de funciones para la descarga de datos abiertos de
#' la Direccion General de Epidemiologia
#'
#' La funcion de descarga principal es `descarga_datos_abiertos` llama las siguientes funciones
#' en orden:
#'
#' * [descarga_diccionario()]
#' * [descarga_db()]
#' * [pega_db_datos_abiertos_tbl_y_diccionario()]
#'
#' A su vez `descarga_diccionario` ejecuta las siguientes para obtener el diccionario de datos:
#' * [descarga_db_diccionario_ssa()]
#' * [unzip_db_diccionario_ssa()]
#' * [parse_db_diccionario_ssa()]
#'
#' A su vez `descarga_db` ejecuta las siguientes para obtener los datos abiertos:
#' * [descarga_db_datos_abiertos_tbl()]
#' * [unzip_db_datos_abiertos_tbl()]
#' * [parse_db_datos_abiertos_tbl()]
#'
#' Si en algun momento se interrumpio la descarga o hubo problemas de conexion o detuviste
#' el proceso de generacion de la base de datos abiertos puedes llamar a las funciones
#' de [read_datos_abiertos()]
#'
#' @details
#'
#' Necesitas tener una instalacion funcionando de [`MariaDB`](https://mariadb.com/). El programa
#' se encarga de descargar la base de datos de la DGE, abrir el archivo `.zip` y crear una
#' tabla de nombre `covidmx` dentro de tu database `dbname`. Asegurate de que tu usuario `user`
#' tenga permisos de escritura. Para mas informacion sobre instalacion y uso de `MariaDB`
#' consulta [el articulo correspondiente](https://rodrigozepeda.github.io/covidmx/articles/Instalacion_de_MARIADB.html).
#'
#' Si tienes RAM que te sobre puedes no crear una base de datos en `MariaDB` sino leer directo
#' el archivo `csv`. Esto se logra con `read_format = tibble`. No lo recomiendo pues puedes
#' terminar con tu sesion de `R` si se te acaba la memoria.
#'
#' _Windows_ Para abrir el archivo `.zip` requieres tambien descargar e instalar [`7Zip`](https://www.7-zip.org/)
#' por default el sistema lo busca en `C:\\Program Files\\7-Zip\\7z.exe` pero si no esta ese
#' directorio es necesario que en `unzip_command` especifiques el camino donde se instalo `7z.exe`.
#'
#' @section Uso de `pins`:
#'
#' Para almacenar los datos se utiliza un pequenio cambio sobre la libreria `pins`. Los datos
#' se descargan y se almacenan en cache junto con informacion sobre cuando fue la descarga. Si
#' no ha pasado un dia desde la ultima descarga no se descarga nada nuevo. Si los datos que
#' se tienen no han cambiado respecto a lo que esta en linea tampoco se vuelven a descargar aunque
#' haya pasado mas de un dia.
#'
#' **Si se te fue el Internet** No te preocupes, `pins` lee tu descarga mas reciente.
#'
#' Para ver donde estan descargados tus datos usa `pins::board_cache_path()`. Para borrarlos usa
#' `pins::cache_prune()`.
#'
#' @note No te recomiendo borrar el cache con `clear_zip` o editarlo por cualquier otro medio si
#' estas usando `pins` pues puede romperse la dependencia. Si accidentalmente lo borraste
#' usa `pins::board_cache_path()` para ir al `path` y borrar manualmente toda la carpeta.
#'
#' @param read_format \code{"MariaDB"} o \code{"tibble"} establece el formato
#' de lectura de la base de datos. En la mayoria de los casos \code{"tibble"} va a
#' resultar en un error de memoria.
#'
#' @param download_process (**opcional**)  Metodo para descargar ya sea `pins` o `download.file`.
#' Se recomienda `pins` pues guarda en memoria la fecha de la ultima descarga y analiza
#' si ha pasado mas de un dia desde la descarga. En caso afirmativo verifica si el
#' archivo ha cambiado y si hubo cambios entonces lo descarga.
#'
#' @param quiet (**opcional**) Variable para no mostrar mensajes
#'
#' @param user Usuario para \code{dbConnect} i.e. tu usuario de `MariaDB`.
#' Puedes ponerlo como variable ambiental con `Sys.setenv("MariaDB_user" = "tu_usuario")`
#'
#' @param password password para \code{dbConnect} i.e. tu password de `MariaDB`
#' Puedes ponerlo como variable ambiental con `Sys.setenv("MariaDB_password" = "tu_passwodrd")`
#'
#' @param dbname Nombre de la base de datos \code{DATABASE} para\code{dbConnect} puedes ponerlo
#' con `Sys.setenv("MariaDB_dbname" = "covidmx")`
#'
#' @param host (**opcional**) Host para \code{dbConnect}. Si no sabes cual tienes seguro es `localhost`. Puedes
#' ponerlo con `Sys.setenv("MariaDB_host" = "localhost")`
#'
#' @param group (**opcional**)  Grupo de \code{dbConnect} i.e. tu grupo de `MariaDB`. Ponlo como variable ambiental
#' usando `Sys.setenv("MariaDB_group" = "tu_grupo")`
#'
#' @param port Puerto de la conexion con `MariaDB`. Puedes ponerlo con
#' `Sys.setenv("MariaDB_group" = "tu_grupo")`
#'
#' @param tblname Nombre de la tabla dentro de el \code{DATABASE} `dbname` donde guardar los datos
#' por default se llama `covidmx`.
#'
#' @param cache_datos Direccion donde guardar los datos en memoria usando `pins` para no tener
#' que volver a descargarlos si nada ha cambiado
#'
#' @param cache_diccionario Direccion donde guardar el diccionario en memoria usando `pins` para no tener
#' que volver a descargarlo si nada ha cambiado
#'
#' @param use_cache_on_failure Booleana. Establece que si no se pueden descargar datos nuevos
#' utilice los que tenga en memoria.
#'
#' @param board_url_name Establece el nombre del `pins::board_url` para los datos abiertos
#'
#' @param board_url_name_dict Establece el nombre del `pins::board_url` para los datos abiertos
#'
#' @param site.covid Sitio web con el vinculo al archivo `.zip` de los datos abiertos. Puedes
#' cambiarlo por uno de los historicos, por ejemplo.
#'
#' @param site.covid.dic Sitio para descarga del diccionario de datos.
#'
#' @param clear_zip Si borrar los archivos `.zip` descargados para el diccionario y los datos
#' abiertos. No se recomienda si estas usando `pins`. Ve la nota para mas informacion.
#'
#' @param clear_csv Si borrar los archivos `.csv` que se generan despues de abrir el zip. El
#' default es que si.
#'
#' @param use_dict Si descargar el diccionario de `site.covid.dic`.
#'
#' @param nthreads Número de `threads` para escribir el archivo en `MariaDB`.
#'
#' @param unzip_command Forma de extraer la base de datos de datos abiertos. La forma de
#' llamarla es con `system2(unzip_command, args = c(unzip_args, file_download_data))`.
#'
#' @param unzip_args Argumentos de extraccion de la base de datos de datos abiertos. La forma de
#' llamarla es con `system2(unzip_command, args = c(unzip_args, file_download_data))`.
#'
#' @param unzip_args_dict Lista de argumentos para usar `utils::unzip` en el diccionario de datos.
#'
#' @param prestatement Opciones adicionales para `MariaDB` yo pongo las que me han funcionado
#' en que sea mas veloz la carta.
#'
#' @param additional_mysqlimport_flags La instalacion se hace con el siguiente comando para `MariaDB`:
#' \preformatted{
#' mysqlimport --default-character-set=UTF8 --fields-terminated-by=',' --ignore-lines=1
#' --fields-enclosed-by='\"' --lines-terminated-by='\\n' --user={user}" --password={password}
#' --use-threads={nthreads} {silent_flag} {additional_mysqlimport_flags} {dbname} ./{tblname}.csv
#' }
#'
#' @param load_data_infile_options Lista de opciones por si la importacion con `mysqlimport` no
#' funciona se use un `LOAD DATA INFILE` como aquí \url{https://dev.mysql.com/doc/refman/8.0/en/load-data.html}
#' Las variables se pasan como lista asi por ejemplo si queremos agregar la variable local se
#' pone `list("LOCAL" = "LOCAL")` y si no la queremos `list("LOCAL" = "")`.
#'
#' @param check_unzip_install Bandera de verificacion para checar si tienes lo necesario para
#' unzippear los datos.
#'
#' @param descarga_db_datos_abiertos_tbl_args Lista con argumentos adicionales para el
#' `pins::pin_download` de datos abiertos
#'
#' @param download_file_args Lista de argumentos adicionales para `download.file` de los datos
#' si se elige este metodo para descargar.
#'
#' @param descarga_db_diccionario_ssa_args Lista con argumentos adicionales para el
#' `pins::pin_download` de datos abiertos
#'
#' @param download_file_args_dict Lista de argumentos adicionales para `download.file` del diccionario
#' si se elige este metodo de descarga.
#'
#' @param force_download analiza si cambio el pin y descarga datos nuevos en caso afirmativo
#'
#' @param show_warnings si arrojar `warnings`
#'
#' @param datos_abiertos_zip_path Camino a los datos abiertos si ya los descargaste en `zip`
#'
#' @param datos_abiertos_unzipped_path Camino a los datos abiertos `csv` si ya los descargaste y
#' descomprimiste el archivo `zip` en un `csv`
#'
#' @param datos_abiertos_tbl Lo que resulta de realizar una descarga de los datos abiertos
#' usando `descarga_db`
#'
#' @param diccionario_zip_path Camino al diccionario si ya losdescargaste en `zip`
#'
#' @param diccionario_unzipped_path Camino al diccionario `csv` si ya lo descargaste y
#' descomprimiste el archivo `zip` en un `csv`
#'
#' @param diccionario Lo que resulta de realizar una descarga del diccionario
#' usando `descarga_diccionario`
#'
#' @param ... Parametros adicionales para `DBI::dbConnect`.
#'
#' @return List of values:
#' \itemize{
#'   \item dats        - Tabla conectada mediante `DBI::dbConnect` (si `MariaDB`) o
#'                       tibble (si `tibble`)
#'   \item disconnect  - Funcion para cerrar la conexion a la base de datos.
#'   \item dict        - Lista de `tibble`s con el diccionario de datos para cada variable
#' }
#' @examples
#' \dontrun{
#' #Descarga de la base de datos junto con diccionario en MariaDB
#' datos_covid <- descarga_datos_abiertos()
#'
#' #Luego haces algo con esos datos...
#'
#' #Cuando terminas cierras la sesión:
#' datos_covid$disconnect()
#'
#' #Descarga solo el diccionario
#' diccionario <- descarga_diccionario()
#'
#' #O bien descarga solo los datos abiertos
#' datos_abiertos <- descarga_db()
#'
#' #Pegalos en el formato que se necesita para el resto de funciones
#' datos_covid <- pega_db_datos_abiertos(datos_abiertos, diccionario)
#'
#' #Tambien puedes descargar paso por paso
#' datos_abiertos <- descarga_db_datos_abiertos_tbl() %>% #Descarga
#'                      unzip_db_datos_abiertos_tbl() %>% #Unzippea
#'                      parse_db_datos_abiertos_tbl()     #MariaDB
#'
#' #O bien el diccionario
#' diccionario <- descarga_db_diccionario_ssa()   %>% #Descarga
#'                     unzip_db_diccionario_ssa() %>% #Unzippea
#'                     parse_db_diccionario_ssa()     #Tibble
#' }
#' @encoding UTF-8
#' @seealso [descarga_datos_red_irag] [descarga_datos_variantes_GISAID] [read_datos_abiertos] [casos]
#' @export

descarga_datos_abiertos <- function(read_format      = c("MariaDB", "tibble"),
                                    user             = Sys.getenv("MariaDB_user"),
                                    password         = Sys.getenv("MariaDB_password"),
                                    dbname           = Sys.getenv("MariaDB_dbname"),
                                    host             = Sys.getenv("MariaDB_host"),
                                    group            = Sys.getenv("MariaDB_group"),
                                    port             = Sys.getenv("MariaDB_port"),
                                    tblname          = "covidmx",
                                    nthreads         = max(parallel::detectCores() - 1, 1),
                                    download_process = c("pins", "download.file"),
                                    site.covid       = paste0("http://datosabiertos.salud.gob.mx",
                                                              "/gobmx/salud/datos_abiertos/datos",
                                                              "_abiertos_covid19.zip"),
                                    site.covid.dic   = paste0("http://datosabiertos.salud.",
                                                              "gob.mx/gobmx/salud/datos_a",
                                                              "biertos/diccionario_datos_",
                                                              "covid19.zip"),
                                    unzip_command       = Sys.getenv("unzip_command"),
                                    unzip_args          = Sys.getenv("unzip_args"),
                                    unzip_args_dict     = list("exdir" = ".", "overwrite" = TRUE),
                                    check_unzip_install = TRUE,
                                    clear_zip           = download_process[1] != "pins",
                                    clear_csv           = TRUE,
                                    use_dict       = TRUE,
                                    datos_abiertos_zip_path      = NULL,
                                    datos_abiertos_unzipped_path = NULL,
                                    datos_abiertos_tbl           = NULL,
                                    diccionario_zip_path         = NULL,
                                    diccionario_unzipped_path    = NULL,
                                    diccionario          = NULL,
                                    quiet                = FALSE,
                                    cache_datos          = NULL,
                                    use_cache_on_failure = TRUE,
                                    cache_diccionario    = NULL,
                                    force_download       = FALSE,
                                    show_warnings        = TRUE,
                                    board_url_name       = "datos_abiertos",
                                    board_url_name_dict  = "diccionario_covid",
                                    download_file_args = list(
                                      method   = "curl",
                                      destfile = tempfile(),
                                      quiet    = quiet
                                    ),
                                    descarga_db_datos_abiertos_tbl_args = list(),
                                    descarga_db_diccionario_ssa_args    = list(),
                                    prestatement   = paste0("SET sql_mode = 'NO_ENGINE_SUBSTITUTION,",
                                                            "NO_AUTO_CREATE_USER';"),
                                    additional_mysqlimport_flags = c("--local"),
                                    load_data_infile_options = list(
                                      "LOW_PRIORITY" = "",
                                      "CONCURRENT"   = "",
                                      "LOCAL"        = "LOCAL",
                                      "PARTITION"    = ""),
                                    ...){



  datos_abiertos_tbl <- descarga_db(read_format      = read_format,
                           user             = user,
                           password         = password,
                           dbname           = dbname,
                           host             = host,
                           group            = group,
                           port             = port,
                           tblname          = tblname,
                           nthreads         = nthreads,
                           download_process = download_process,
                           site.covid       = site.covid,
                           unzip_command    = unzip_command,
                           unzip_args       = unzip_args,
                           check_unzip_install          = check_unzip_install,
                           clear_zip                    = clear_zip,
                           clear_csv                    = clear_csv,
                           force_download               = force_download,
                           show_warnings                = show_warnings,
                           datos_abiertos_zip_path      = datos_abiertos_zip_path,
                           datos_abiertos_unzipped_path = datos_abiertos_unzipped_path,
                           datos_abiertos_tbl           = datos_abiertos_tbl,
                           quiet                        = quiet,
                           board_url_name               = board_url_name,
                           use_cache_on_failure         = use_cache_on_failure,
                           cache_diccionario            = cache_diccionario,
                           download_file_args           = download_file_args,
                           descarga_db_datos_abiertos_tbl_args   = descarga_db_datos_abiertos_tbl_args,
                           prestatement                 = prestatement,
                           additional_mysqlimport_flags = additional_mysqlimport_flags,
                           load_data_infile_options     = load_data_infile_options, ...)


  if (use_dict){
    diccionario <- descarga_diccionario(
      download_process = download_process,
      site.covid.dic   = site.covid.dic,
      quiet            = quiet,
      diccionario_zip_path = diccionario_zip_path,
      diccionario_unzipped_path = diccionario_unzipped_path,
      diccionario               = diccionario,
      board_url_name_dict       = board_url_name_dict,
      cache_diccionario         = cache_diccionario,
      use_cache_on_failure      = use_cache_on_failure,
      clear_zip                 = clear_zip,
      clear_csv                 = clear_csv,
      download_file_args_dict   = download_file_args,
      unzip_args_dict           = unzip_args_dict,
      force_download            = force_download,
      show_warnings             = show_warnings,
      descarga_db_diccionario_ssa_args = descarga_db_diccionario_ssa_args
    )
  } else {
    diccionario <- NULL
  }

  #Pegamos todo
  datos_covid <- pega_db_datos_abiertos_tbl_y_diccionario(datos_abiertos_tbl = datos_abiertos_tbl, diccionario = diccionario)


  return(datos_covid)

}

#' @export
#' @rdname descarga_datos_abiertos
#' @param cache parametro para el cache de `pins::board_url`
#' @param ... Parametros adicionales para `BI::dbConnect()` con  conexion de `RMariaDB::MariaDB()`
descarga_db <- function(read_format      = c("MariaDB", "tibble"),
                        user             = Sys.getenv("MariaDB_user"),
                        password         = Sys.getenv("MariaDB_password"),
                        dbname           = Sys.getenv("MariaDB_dbname"),
                        host             = Sys.getenv("MariaDB_host"),
                        group            = Sys.getenv("MariaDB_group"),
                        port             = Sys.getenv("MariaDB_port"),
                        tblname          = "covidmx",
                        nthreads         = max(parallel::detectCores() - 1, 1),
                        download_process = c("pins", "download.file"),
                        site.covid       = paste0("http://datosabiertos.salud.gob.mx",
                                                  "/gobmx/salud/datos_abiertos/datos",
                                                  "_abiertos_covid19.zip"),
                        unzip_command       = Sys.getenv("unzip_command"),
                        unzip_args          = Sys.getenv("unzip_args"),
                        check_unzip_install = TRUE,
                        clear_zip           = download_process[1] != "pins",
                        clear_csv           = TRUE,
                        force_download      = FALSE,
                        show_warnings       = TRUE,
                        datos_abiertos_zip_path      = NULL,
                        datos_abiertos_unzipped_path = NULL,
                        datos_abiertos_tbl        = NULL,
                        quiet                     = FALSE,
                        board_url_name            = "datos_abiertos",
                        cache                = NULL,
                        use_cache_on_failure = TRUE,
                        download_file_args = list(
                          method   = "curl",
                          destfile = tempfile(),
                          quiet    = quiet
                        ),
                        descarga_db_datos_abiertos_tbl_args = list(),
                        prestatement   = paste0("SET sql_mode = 'NO_ENGINE_SUBSTITUTION,",
                                                "NO_AUTO_CREATE_USER';"),
                        additional_mysqlimport_flags = c("--local"),
                        load_data_infile_options = list(
                          "LOW_PRIORITY" = "",
                          "CONCURRENT"   = "",
                          "LOCAL"        = "LOCAL",
                          "PARTITION"    = ""),
                        ...){

  #Descargamos los datos de la ssa
  if (is.null(datos_abiertos_zip_path) & is.null(datos_abiertos_unzipped_path) & is.null(datos_abiertos_tbl)){

    descarga_db_datos_abiertos_tbl_args <- list(
      "download_process"     = download_process,
      "site.covid"           = site.covid,
      "quiet"                = quiet,
      "board_url_name"       = board_url_name,
      "cache"                = cache,
      "use_cache_on_failure" = use_cache_on_failure,
      "force_download"       = force_download,
      "show_warnings"        = show_warnings,
      "download_file_args"   = download_file_args) %>%
      append(descarga_db_datos_abiertos_tbl_args)

    datos_abiertos_zip_path <- do.call(descarga_db_datos_abiertos_tbl,
                                       descarga_db_datos_abiertos_tbl_args)
  }

  #Liberamos el zip
  if (!is.null(datos_abiertos_zip_path) & is.null(datos_abiertos_unzipped_path) & is.null(datos_abiertos_tbl)){

    datos_abiertos_unzipped_path <- unzip_db_datos_abiertos_tbl(datos_abiertos_zip_path = datos_abiertos_zip_path,
                                                       unzip_command = unzip_command,
                                                       unzip_args    = unzip_args,
                                                       clear_zip     = clear_zip,
                                                       quiet         = quiet,
                                                       check_unzip_install = check_unzip_install)
  }

  #Parseamos el file en tibble o MARIADB
  if (!is.null(datos_abiertos_unzipped_path) & is.null(datos_abiertos_tbl)){

    datos_abiertos_tbl <- parse_db_datos_abiertos_tbl(
      datos_abiertos_unzipped_path = datos_abiertos_unzipped_path,
      read_format    = read_format,
      user           = user,
      password       = password,
      dbname         = dbname,
      host           = host,
      group          = group,
      port           = port,
      tblname        = tblname,
      nthreads       = nthreads,
      clear_csv      = clear_csv,
      quiet          = quiet,
      prestatement   = prestatement,
      additional_mysqlimport_flags = additional_mysqlimport_flags,
      load_data_infile_options     = load_data_infile_options,
      ...
    )
  }

  return(datos_abiertos_tbl)
}

#' @export
#' @rdname descarga_datos_abiertos
descarga_diccionario <- function(download_process = c("pins", "download.file"),
                                 site.covid.dic   = paste0("http://datosabiertos.salud.",
                                                           "gob.mx/gobmx/salud/datos_a",
                                                           "biertos/diccionario_datos_",
                                                           "covid19.zip"),
                                 quiet                = FALSE,
                                 clear_zip            = download_process[1] != "pins",
                                 clear_csv            = TRUE,
                                 diccionario_zip_path     = NULL,
                                 diccionario_unzipped_path    = NULL,
                                 diccionario          = NULL,
                                 board_url_name_dict  = "diccionario_covid",
                                 cache_diccionario    = NULL,
                                 use_cache_on_failure = TRUE,
                                 force_download       = FALSE,
                                 show_warnings        = TRUE,
                                 download_file_args_dict = list(
                                   method   = "curl",
                                   destfile = tempfile(),
                                   quiet    = quiet
                                 ),
                                 unzip_args_dict = list("exdir" = ".", "overwrite" = TRUE),
                                 descarga_db_diccionario_ssa_args = list()){


  #Descargamos el diccionario
  if (is.null(diccionario_zip_path) & is.null(diccionario_unzipped_path) & is.null(diccionario)){

    descarga_db_diccionario_ssa_args <- list(
      "download_process"        = download_process,
      "site.covid.dic"          = site.covid.dic,
      "quiet"                   = quiet,
      "board_url_name_dict"     = board_url_name_dict,
      "cache_diccionario"       = cache_diccionario,
      "force_download"          = force_download,
      "show_warnings"           = show_warnings,
      "use_cache_on_failure"    = use_cache_on_failure,
      "download_file_args_dict" = download_file_args_dict) %>%
      append(descarga_db_diccionario_ssa_args)

    diccionario_zip_path <- do.call(descarga_db_diccionario_ssa, descarga_db_diccionario_ssa_args)
  }

  #Liberamos el diccionario
  if (!is.null(diccionario_zip_path) & is.null(diccionario_unzipped_path) & is.null(diccionario)){

    diccionario_unzipped_path <- unzip_db_diccionario_ssa(diccionario_zip_path = diccionario_zip_path,
                                                          unzip_args_dict  = unzip_args_dict,
                                                          clear_zip        = clear_zip)
  }

  #Leemos el diccionario
  if (!is.null(diccionario_unzipped_path)){

    diccionario <- parse_db_diccionario_ssa(
      diccionario_unzipped_path = diccionario_unzipped_path,
      clear_csv = clear_csv
    )
  }

  return(diccionario)

}

#' @export
#' @rdname descarga_datos_abiertos
#' @param cache parametro para el cache de `pins::board_url`
#' @param ... Parametros adicionales para `pins::pin_download`
descarga_db_datos_abiertos_tbl <- function(download_process = c("pins", "download.file"),
                                  site.covid       = paste0("http://datosabiertos.salud.gob.mx",
                                                            "/gobmx/salud/datos_abiertos/datos",
                                                            "_abiertos_covid19.zip"),
                                  quiet                = FALSE,
                                  board_url_name       = "datos_abiertos",
                                  cache                = NULL,
                                  use_cache_on_failure = TRUE,
                                  force_download       = FALSE,
                                  show_warnings        = TRUE,
                                  download_file_args   = list(
                                    method   = "curl",
                                    destfile = tempfile(),
                                    quiet    = quiet
                                  ),
                                  ...){

  #Method for download
  download_process <- ifelse(download_process[1] == "download.file", "download.file", "pins")

  #Check site exists
  if (!RCurl::url.exists(site.covid)){
    stop(glue::glue("El sitio {site.covid} no existe o no puede ser encontrado. ",
                    "Verifica exista y tu conexion a Internet sea estable."))
  }

  #Add message
  if (!quiet){
    message(glue::glue("Descargando. Ten paciencia esto puede tardar bastante.
                        Downloading. Be patient this could last a long time"))
  }

  if (download_process == "download.file"){

    #Attempt to download
    download_file_args <- append(list("url" = site.covid), download_file_args)
    do.call(download.file, download_file_args)

    #Return path
    pathname <- download_file_args$destfile

  } else {

    #Attempt to create board
    names(site.covid) <- board_url_name
    board_url_args    <- list("urls" = site.covid, "cache" = cache,
                              "use_cache_on_failure" = use_cache_on_failure)
    board             <- do.call(pins::board_url, board_url_args)

    #Obtenemos la diferencia de tiempo de cuando se bajó por vez últia
    tdif <- pin_get_download_time(board, board_url_name)

    if (!force_download & tdif < 1){

      if (show_warnings){
        warning(glue::glue("
                          La descarga mas reciente fue hace {tdif} dias. Como tiene menos de un dia
                          usare esa. Escribe force_download = TRUE si quieres descargar de
                          todas formas. Para desactivar este mensaje show_warnings = FALSE.

                          Most recent download was {tdif} days ago. It has less than a day hence
                          I'll use that one. Write force_download = TRUE if you want to
                          download anyway. To turn off this message show_warnings = FALSE."))
      }

      #Lee de memoria
      pathname <- pin_path_from_memory(board, board_url_name)

    } else {
      #Descarga si cambió
      pathname <- pins::pin_download(board = board, name = board_url_name, ...)
    }

    #Escribimos en el pin que ya descargamos
    pin_write_download_time(board, board_url_name)
  }


  #Add message
  if (!quiet){
    message(glue::glue("Descargado en / Downloaded at {pathname}"))
  }

  return(pathname)

}

#' @export
#' @rdname descarga_datos_abiertos
#' @param ... Parametros adicionales para `pins::pin_download`
descarga_db_diccionario_ssa <- function(download_process = c("pins", "download.file"),
                                        site.covid.dic   = paste0("http://datosabiertos.salud.",
                                                                  "gob.mx/gobmx/salud/datos_a",
                                                                  "biertos/diccionario_datos_",
                                                                  "covid19.zip"),
                                       quiet            = FALSE,
                                       board_url_name_dict  = "diccionario_covid",
                                       cache_diccionario    = NULL,
                                       use_cache_on_failure = TRUE,
                                       force_download       = FALSE,
                                       show_warnings        = TRUE,
                                       download_file_args_dict = list(
                                         method   = "curl",
                                         destfile = tempfile(),
                                         quiet    = quiet
                                       ),
                                       ...){

  #Corremos el mismo programa sólo que con la base de diccionario
  descarga_db_datos_abiertos_tbl(download_process        = download_process,
                        site.covid              = site.covid.dic,
                        board_url_name          = board_url_name_dict,
                        cache                   = cache_diccionario,
                        use_cache_on_failure    = use_cache_on_failure,
                        download_file_args      = download_file_args_dict,
                        quiet                   = quiet,
                        force_download          = force_download,
                        show_warnings           = show_warnings,
                        ...)
}

#' @export
#' @rdname descarga_datos_abiertos
unzip_db_datos_abiertos_tbl <- function(datos_abiertos_zip_path,
                               unzip_command       = Sys.getenv("unzip_command"),
                               unzip_args          = Sys.getenv("unzip_args"),
                               check_unzip_install = TRUE,
                               quiet               = FALSE,
                               clear_zip           = FALSE){

  #Establecemos el comando para unzipear
  if (is.null(unzip_command) | unzip_command == ""){
    unzip_command <- ifelse(tolower(.Platform$OS.type) == "windows",
                            "\"C:\\Program Files\\7-Zip\\7z.exe\"", "unzip")
  }

  #Establecemos argumentos adicionales
  if (is.null(unzip_args) | unzip_args == ""){
    unzip_args    <- ifelse(tolower(.Platform$OS.type) == "windows","-x", "-o")
  }

  #Checamos que exusta la herramienta para unzippear
  if (check_unzip_install & stringr::str_detect(R.version$os,"darwin|linux")){
    is_unzip <- system2("which", unzip_command, stdout = T, stderr = T)
    if (length(is_unzip) == 0){
      stop(glue::glue("Por favor instala unzip:
                       [OSX]: brew install unzip
                       [Debian/Ubuntu]: apt install unzip"))
    }
  } else if (check_unzip_install & tolower(.Platform$OS.type) == "windows"){
    is_unzip <- shell(glue::glue('if exist {unzip_command} echo yes'), intern = T)
    if (is_unzip != "yes"){
      stop(glue::glue("Por favor instala 7zip de https://www.7-zip.org/
                       y en unzip_command pon el camino hacia el archivo 7z.exe
                       Ej: unzip_command=\"'C:\\Program Files\\7-Zip\\7z.exe'\""))
    }
  }

  #Unzip file
  filecon <- tryCatch({
    unzip(datos_abiertos_zip_path, overwrite = TRUE)
    list.files(pattern = "*COVID19MEXICO.csv", full.names = T)[1]
  },
  warning = function(cond) {

    system2(unzip_command, args = c(unzip_args, datos_abiertos_zip_path), stdout = !quiet)
    list.files(pattern = "*COVID19MEXICO.csv", full.names = T)[1]

  },
  error = function(cond){

    stop(glue::glue("No se puede leer {datos_abiertos_zip_path}
                     Unable to read {datos_abiertos_zip_path}"))

  })

  if (clear_zip & file.exists(datos_abiertos_zip_path)){
    file.remove(datos_abiertos_zip_path)
  }

  return(filecon)

}

#' @export
#' @rdname descarga_datos_abiertos
unzip_db_diccionario_ssa <- function(diccionario_zip_path,
                                     unzip_args_dict = list("exdir" = ".", "overwrite" = TRUE),
                                     clear_zip       = FALSE
                                     ){

  filenames  <- unzip(zipfile = diccionario_zip_path, list = TRUE)
  fname      <- filenames[which(stringr::str_detect(filenames$Name, "Cat.*logo.*")), "Name"]

  unzip_args <- append(list("zipfile" = diccionario_zip_path, "files" = fname), unzip_args_dict)
  filecon    <- do.call(unzip, unzip_args)

  if (clear_zip & file.exists(diccionario_zip_path)){
    unlink(diccionario_zip_path)
  }

  return(filecon)
}

#' @export
#' @rdname descarga_datos_abiertos
parse_db_diccionario_ssa <- function(diccionario_unzipped_path, clear_csv = FALSE){

  if (!file.exists(diccionario_unzipped_path)){
    stop(glue::glue("No puedo encontrar {diccionario_unzipped_path}
                     I can't find {diccionario_unzipped_path}"))
  }

  diccionario <- list()

  diccionario <- diccionario %>% append(
    list("ORIGEN"     = readxl::read_excel(diccionario_unzipped_path,
                                           sheet = "Cat\u00e1logo ORIGEN",
                                           col_types = c("numeric", "text")))) %>%
    append(
      list("SECTOR"   = readxl::read_excel(diccionario_unzipped_path,
                                           sheet = "Cat\u00e1logo SECTOR",
                                           col_types = c("numeric", "text")))) %>%
    append(
      list("SEXO"     = readxl::read_excel(diccionario_unzipped_path,
                                           sheet = "Cat\u00e1logo SEXO",
                                           col_types = c("numeric", "text")))) %>%
    append(
      list("PACIENTE" = readxl::read_excel(diccionario_unzipped_path,
                                           sheet = "Cat\u00e1logo TIPO_PACIENTE",
                                           col_types = c("numeric", "text")))) %>%
    append(
      list("NACIONALIDAD"  = readxl::read_excel(diccionario_unzipped_path,
                                                sheet = "Cat\u00e1logo NACIONALIDAD",
                                                col_types = c("numeric", "text")))) %>%
    append(
      list("RESULTADO_LAB" = readxl::read_excel(diccionario_unzipped_path,
                                                sheet = "Cat\u00e1logo RESULTADO_LAB",
                                                col_types = c("numeric", "text")))) %>%
    append(
      list("RESULTADO_ANTIGENO"  = readxl::read_excel(diccionario_unzipped_path,
                                                      sheet = "Cat\u00e1logo RESULTADO_ANTIGENO",
                                                      col_types = c("numeric", "text")))) %>%
    append(
      list("CLASIFICACION_FINAL" = readxl::read_excel(diccionario_unzipped_path,
                                     sheet = "Cat\u00e1logo CLASIFICACION_FINAL",
                                     col_types = c("numeric", "text", "text")))) %>%
    append(
      list("MUNICIPIO_RES" = readxl::read_excel(diccionario_unzipped_path,
                                                sheet = "Cat\u00e1logo MUNICIPIOS",
                                                col_types = c("text", "text", "text"))))



  #CATALOGO SI NO
  lista_si_no <- list(readxl::read_excel(diccionario_unzipped_path,
                                         sheet = "Cat\u00e1logo SI_NO",
                                         col_types = c("numeric", "text")))

  for (variable in c("INTUBADO", "NEUMONIA", "EMBARAZO", "HABLA LENGUA INDIGENA", "INDIGENA",
                     "DIABETES", "EPOC", "ASMA", "INMUSUPR", "HIPERTENSION",
                     "CARDIOVASCULAR","OTRO_CASO","TOMA_MUESTRA_LAB", "TOMA_MUESTRA_ANTIGENO",
                     "OTRA_COMORBILIDAD", "OBESIDAD","RENAL_CRONICA","TABAQUISMO","UCI")){

    names(lista_si_no) <- variable
    diccionario        <- diccionario %>% append(lista_si_no)

  }

  #> CATALOGO ENTIDAD
  lista_entidad <- list(readxl::read_excel(diccionario_unzipped_path,
                                           sheet = "Cat\u00e1logo de ENTIDADES",
                                           col_types = c("text", "text", "text")))

  for (variable in c("ENTIDAD_UM", "ENTIDAD_RES", "ENTIDAD_NAC")){

    names(lista_entidad) <- variable
    diccionario          <- diccionario %>% append(lista_entidad)

  }

  if (clear_csv & file.exists(diccionario_unzipped_path)){
    unlink(diccionario_unzipped_path)
  }

  return(diccionario)

}

#' @export
#' @rdname descarga_datos_abiertos
parse_db_datos_abiertos_tbl <- function(datos_abiertos_unzipped_path,
                               read_format    = c("MariaDB", "tibble"),
                               user           = Sys.getenv("MariaDB_user"),
                               password       = Sys.getenv("MariaDB_password"),
                               dbname         = Sys.getenv("MariaDB_dbname"),
                               host           = Sys.getenv("MariaDB_host"),
                               group          = Sys.getenv("MariaDB_group"),
                               port           = Sys.getenv("MariaDB_port"),
                               tblname        = "covidmx",
                               nthreads       = max(parallel::detectCores() - 1, 1),
                               quiet          = TRUE,
                               clear_csv      = FALSE,
                               prestatement   = paste0("SET sql_mode = 'NO_ENGINE_SUBSTITUTION,",
                                                       "NO_AUTO_CREATE_USER';"),
                               additional_mysqlimport_flags = c("--local"),
                               load_data_infile_options = list(
                                 "LOW_PRIORITY" = "",
                                 "CONCURRENT"   = "",
                                 "LOCAL"        = "LOCAL",
                                 "PARTITION"    = ""
                               ),
                               ...){

  #Formato de lectura
  if (tolower(read_format[1]) %in% c("mariadb","tibble")){
    read_format <- tolower(read_format[1])
  } else {
    stop("Formato invalido. Selecciona MariaDB o tibble")
  }

  #Check we have mariadb
  if (read_format == "mariadb" & !RMariaDB::mariadbHasDefault()) {
    url_maria <- "https://rodrigozepeda.github.io/covidmx/articles/Instalacion_de_MARIADB.html"
    stop(glue::glue("No puedo encontrar la conexion con MariaDB.
                     Ve a {url_maria} para mas informacion sobre como instalar."))
  }

  if (read_format == "mariadb" & !quiet){
    message(glue::glue("Por favor se paciente todo el proceso toma aprox 20 minutos.
                        Please be patient the whole process takes around 20 minutes."))
  }

  #Si es MARIADB regresa sólo la primera entrada si es tibble regresa todo
  if (read_format == "mariadb"){
    n_max <- 1
  } else {
    n_max <- Inf
  }

  #Desactivamos que nos hable
  readr_progress_old <- getOption("readr.show_progress")
  options(readr.show_progress = !quiet)

  #Desactivamos las parsing warnings
  old_warning <- getOption("warn")
  options(warn = -1)

  #Leemos el archivo
  header <- readr::read_csv(datos_abiertos_unzipped_path,
                            locale  = readr::locale(encoding = "UTF-8"),
                            n_max   = n_max,
                            trim_ws = TRUE,
                            col_types = readr::cols(
                              .default              = readr::col_character(),
                              FECHA_ACTUALIZACION   = readr::col_date(format = "%Y-%m-%d"),
                              ORIGEN                = readr::col_double(),
                              SECTOR                = readr::col_double(),
                              SEXO                  = readr::col_double(),
                              TIPO_PACIENTE         = readr::col_double(),
                              FECHA_INGRESO         = readr::col_date(format = "%Y-%m-%d"),
                              FECHA_SINTOMAS        = readr::col_date(format = "%Y-%m-%d"),
                              FECHA_DEF             = readr::col_date(format = "%Y-%m-%d"),
                              INTUBADO              = readr::col_double(),
                              NEUMONIA              = readr::col_double(),
                              EDAD                  = readr::col_double(),
                              NACIONALIDAD          = readr::col_double(),
                              EMBARAZO              = readr::col_double(),
                              HABLA_LENGUA_INDIG    = readr::col_double(),
                              INDIGENA              = readr::col_double(),
                              DIABETES              = readr::col_double(),
                              EPOC                  = readr::col_double(),
                              ASMA                  = readr::col_double(),
                              INMUSUPR              = readr::col_double(),
                              HIPERTENSION          = readr::col_double(),
                              OTRA_COM              = readr::col_double(),
                              CARDIOVASCULAR        = readr::col_double(),
                              OBESIDAD              = readr::col_double(),
                              RENAL_CRONICA         = readr::col_double(),
                              TABAQUISMO            = readr::col_double(),
                              OTRO_CASO             = readr::col_double(),
                              TOMA_MUESTRA_LAB      = readr::col_double(),
                              RESULTADO_LAB         = readr::col_double(),
                              TOMA_MUESTRA_ANTIGENO = readr::col_double(),
                              RESULTADO_ANTIGENO    = readr::col_double(),
                              CLASIFICACION_FINAL   = readr::col_double(),
                              MIGRANTE              = readr::col_double(),
                              UCI                   = readr::col_double()
                            ))

  #Set readr progress
  options(readr.show_progress = readr_progress_old)
  options(warn = old_warning)

  #Si estamos en tibble ya acabamos
  if (read_format == "tibble"){
    dats <- header;  disconnect <- function(){message("Happy coding!")}

  #Si es MARIADB cargamos la base
  } else {

    #Creamos la conexión de MARIADB
    dbname <- ifelse(dbname == "", "covidmx", dbname)
    host   <- ifelse(host   == "", "localhost", host)
    port   <- ifelse(port   == "", "8787",      port)

    if(!quiet){
      message(
        glue::glue("DATASET {dbname}
                    at {host}
                    using port = {port}")
      )
    }

    #Get file connection
    con    <- DBI::dbConnect(RMariaDB::MariaDB(),
                             host     = host,
                             port     = port,
                             user     = user,
                             group    = group,
                             password = password,
                             dbname   = dbname,
                             ...)

    #Creating MARIADB table format
    dbres <- DBI::dbSendStatement(conn = con, statement = prestatement)
    DBI::dbClearResult(dbres)

    #Overwriting table
    DBI::dbWriteTable(conn = con, name = tblname, value = header, overwrite = T)

    #Deleting what has been written
    dbres <- DBI::dbSendStatement(conn = con, statement = glue::glue("DELETE FROM {tblname};"))
    DBI::dbClearResult(dbres)

    #Rename file because mysql only works with specific name in MARIADB
    if (file.exists(glue::glue("./{tblname}.csv")) &
        datos_abiertos_unzipped_path != glue::glue("./{tblname}.csv")){

      file.remove(glue::glue("./{tblname}.csv"))

    }
    file.rename(datos_abiertos_unzipped_path, glue::glue("./{tblname}.csv"))

    #Then we create the table
    silent_flag <- ifelse(quiet, "--silent", "")
    other_flags <- paste0(additional_mysqlimport_flags, collapse = " ")

    tryCatch({

      if (!quiet){
        message(glue::glue("Intentando crear tabla en paralelo con mysqlimport | ",
                           "Attempting to create table in parallel with mysqlimport"))
      }

      system(glue::glue("mysqlimport",
                        " --default-character-set=UTF8",
                        " --fields-terminated-by=','",
                        " --ignore-lines=1",
                        " --fields-enclosed-by='\"'",
                        " --lines-terminated-by='\\n'",
                        " --user={user}",
                        " --password={password}",
                        " --use-threads={nthreads}",
                        " {silent_flag}",
                        " {other_flags}",
                        " {dbname} ./{tblname}.csv"), ignore.stdout = !quiet)
    },
    error=function(e) {

      if (!quiet){
        message(glue::glue("Intentando crear tabla con LOAD DATA INFILE | ",
                           "Attempting to create table in parallel with LOAD DATA INFILE"))
      }
      #Writing to table
      query_to_writedata <- glue::glue("LOAD DATA ",
                                       "{load_data_infile_options$LOW_PRIORITY} ",
                                       "{load_data_infile_options$CONCURRENT} ",
                                       "{load_data_infile_options$LOCAL} ",
                                       "INFILE \'{tblname}.csv\' ",
                                       "REPLACE INTO TABLE {tblname} ",
                                       "CHARACTER SET UTF8 ",
                                       "COLUMNS TERMINATED BY ',' ",
                                       "ENCLOSED BY '\"'",
                                       "LINES TERMINATED BY '\\n' ",
                                       "IGNORE 1 LINES;")

      dbres <- DBI::dbSendStatement(conn = con, statement = query_to_writedata)
      DBI::dbClearResult(dbres)

    })

    if (!quiet){
      message(glue::glue("Cree la tabla {tblname} | Created {tblname} table"))
    }
    dats  <- dplyr::tbl(con, tblname)

    #Creamos función de desconexión
    disconnect <- function(){DBI::dbDisconnect(con)}

    #Mensaje de desconexión
    if (!quiet){
      message(glue::glue("No olvides desconectar la base con datos_covid$disconnect() al final.

                          Don't forget to disconnect the dataset with datos_covid$disconnect() at
                          the end."))
    }
  }

  if (clear_csv & file.exists(glue::glue("{tblname}.csv"))){
    file.remove(glue::glue("{tblname}.csv"))
  }

  return(list(dats = dats, disconnect = disconnect))
}

#' @export
#' @rdname descarga_datos_abiertos
pega_db_datos_abiertos_tbl_y_diccionario <- function(datos_abiertos_tbl, diccionario){
  return(append(datos_abiertos_tbl, list("dict" = diccionario)))
}
