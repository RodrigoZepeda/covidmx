#' LEE LA BASE DE DATOS ABIERTOS DE LA DIRECCION GENERAL DE EPIDEMIOLOGIA QUE YA DESCARGASTE
#'
#' @description
#' `read_datos_abiertos` Lee los datos abiertos almacenados en tu base de `MariaDB` que
#' bajaste con `descarga_datos_abiertos`. Intenta de manera automática determinar
#' si los lee de `MariaDB`, `csv` ó `zip`
#'
#' La funcion principal es [read_datos_abiertos()] la cual decide si los lee de zip, MARIADB o csv
#' Tambien puedes usar las auxiliares respectivas
#' * [read_datos_abiertos_zip()]     Si sólo descargaste los datos de la DGE en `.zip`
#' * [read_datos_abiertos_csv()]     Si descargaste los datos de la DGE en `.zip` y los descomprimiste.
#' * [read_datos_abiertos_MariaDB()] Si ya creaste tu table en `MariaDB`

#'
#' @return List of values:
#' \itemize{
#'   \item dats - Database table (if MARIADB) or database in tibble (if tibble)
#'   \item con - Database connection (if MARIADB) or \code{NULL} (if tibble)
#'   \item dict - List of tibbles containing the whole dictionary
#' }
#' @inheritParams descarga_datos_abiertos
#' @param datos_abiertos_path Camino a los datos abiertos si son un `zip` o un `csv` o bien
#' dejar en `NULL` si se quiere leer de `MariaDB`
#' @examples
#' \dontrun{
#' # Lee los datos de MariaDB una vez descargados
#' descarga_datos_abiertos()
#' datos_covid <- read_datos_abiertos()
#'
#' # Es lo mismo que:
#' datos_covid <- read_datos_abiertos_MariaDB()
#'
#' # Descarga los datos y lee de un zip
#' direccion_zip <- descarga_db_datos_abiertos_tbl()
#' datos_covid <- read_datos_abiertos(direccion_zip)
#'
#' # Es lo mismo que:
#' datos_covid <- read_datos_abiertos_zip(direccion_zip)
#'
#' # Descarga los datos y lee de un csv
#' direccion_zip <- descarga_db_datos_abiertos_tbl()
#' direccion_csv <- unzip_db_datos_abiertos_tbl(direccion_zip)
#' datos_covid <- read_datos_abiertos(direccion_csv)
#'
# #Es lo mismo que:
#' datos_covid <- read_datos_abiertos_csv(direccion_csv)
#' }
#' @encoding UTF-8
#' @export
read_datos_abiertos <- function(datos_abiertos_path = NULL,
                                driver = RMariaDB::MariaDB(),
                                sqlimport = "mysqlimport",
                                read_format = c("MariaDB", "tibble"),
                                user = Sys.getenv("MariaDB_user"),
                                password = Sys.getenv("MariaDB_password"),
                                dbname = Sys.getenv("MariaDB_dbname"),
                                host = Sys.getenv("MariaDB_host"),
                                group = Sys.getenv("MariaDB_group"),
                                port = Sys.getenv("MariaDB_port"),
                                tblname = "covidmx",
                                nthreads = max(parallel::detectCores() - 1, 1),
                                ...) {
  if (is.null(datos_abiertos_path)) {
    datos_covid <- read_datos_abiertos_MariaDB(
      driver      = driver,
      read_format = read_format,
      user = user,
      password = password,
      dbname = dbname,
      host = host,
      group = group,
      port = port,
      tblname = tblname,
      nthreads = nthreads,
      ...
    )
  } else if (tools::file_ext(datos_abiertos_path) == "csv") {
    datos_covid <- read_datos_abiertos_csv(
      datos_abiertos_unzipped_path = datos_abiertos_path,
      sqlimport = sqlimport,
      driver = driver,
      read_format = read_format,
      user = user,
      password = password,
      dbname = dbname,
      host = host,
      group = group,
      port = port,
      tblname = tblname,
      nthreads = nthreads,
      ...
    )
  } else if (tools::file_ext(datos_abiertos_path) == "zip") {
    datos_covid <- read_datos_abiertos_zip(
      datos_abiertos_zip_path = datos_abiertos_path,
      sqlimport = sqlimport,
      driver = driver,
      read_format = read_format,
      user = user,
      password = password,
      dbname = dbname,
      host = host,
      group = group,
      port = port,
      tblname = tblname,
      nthreads = nthreads,
      ...
    )
  }

  return(datos_covid)
}


#' @export
#' @rdname read_datos_abiertos
#' @inheritParams descarga_datos_abiertos
# Wrapper para leer los datos abiertos de un zip
read_datos_abiertos_zip <- function(datos_abiertos_zip_path,
                                    diccionario_zip_path = NULL,
                                    diccionario_unzipped_path = NULL,
                                    diccionario = NULL,
                                    read_format = c("MariaDB", "tibble"),
                                    driver = RMariaDB::MariaDB(),
                                    sqlimport = "mysqlimport",
                                    user = Sys.getenv("MariaDB_user"),
                                    password = Sys.getenv("MariaDB_password"),
                                    dbname = Sys.getenv("MariaDB_dbname"),
                                    host = Sys.getenv("MariaDB_host"),
                                    group = Sys.getenv("MariaDB_group"),
                                    port = Sys.getenv("MariaDB_port"),
                                    tblname = "covidmx",
                                    nthreads = max(parallel::detectCores() - 1, 1),
                                    download_process = c("pins", "download.file"),
                                    site.covid.dic = paste0(
                                      "http://datosabiertos.salud.",
                                      "gob.mx/gobmx/salud/datos_a",
                                      "biertos/diccionario_datos_",
                                      "covid19.zip"
                                    ),
                                    unzip_command = Sys.getenv("unzip_command"),
                                    unzip_args = Sys.getenv("unzip_args"),
                                    unzip_args_dict = list("exdir" = ".", "overwrite" = TRUE),
                                    check_unzip_install = TRUE,
                                    clear_zip = download_process[1] != "pins",
                                    clear_csv = TRUE,
                                    use_dict = TRUE,
                                    quiet = FALSE,
                                    cache_datos = NULL,
                                    use_cache_on_failure = TRUE,
                                    cache_diccionario = NULL,
                                    force_download = FALSE,
                                    show_warnings = TRUE,
                                    board_url_name = "datos_abiertos",
                                    board_url_name_dict = "diccionario_covid",
                                    download_file_args = list(
                                      method   = "curl",
                                      destfile = tempfile(),
                                      quiet    = quiet
                                    ),
                                    descarga_db_diccionario_ssa_args = list(),
                                    prestatement = paste0(
                                      "SET sql_mode = 'NO_ENGINE_SUBSTITUTION,",
                                      "NO_AUTO_CREATE_USER';"
                                    ),
                                    additional_mysqlimport_flags = c("--local"),
                                    load_data_infile_options = list(
                                      "LOW_PRIORITY" = "",
                                      "CONCURRENT"   = "",
                                      "LOCAL"        = "LOCAL",
                                      "PARTITION"    = ""
                                    ),
                                    ...) {
  do.call(descarga_datos_abiertos, as.list(environment()))
}

#' @export
#' @rdname read_datos_abiertos
#' @inheritParams descarga_datos_abiertos
# Wrapper para leer los datos abiertos de un csv
read_datos_abiertos_csv <- function(datos_abiertos_unzipped_path,
                                    diccionario_zip_path = NULL,
                                    diccionario_unzipped_path = NULL,
                                    diccionario = NULL,
                                    read_format = c("MariaDB", "tibble"),
                                    driver = RMariaDB::MariaDB(),
                                    sqlimport = "mysqlimport",
                                    user = Sys.getenv("MariaDB_user"),
                                    password = Sys.getenv("MariaDB_password"),
                                    dbname = Sys.getenv("MariaDB_dbname"),
                                    host = Sys.getenv("MariaDB_host"),
                                    group = Sys.getenv("MariaDB_group"),
                                    port = Sys.getenv("MariaDB_port"),
                                    tblname = "covidmx",
                                    nthreads = max(parallel::detectCores() - 1, 1),
                                    download_process = c("pins", "download.file"),
                                    site.covid.dic = paste0(
                                      "http://datosabiertos.salud.",
                                      "gob.mx/gobmx/salud/datos_a",
                                      "biertos/diccionario_datos_",
                                      "covid19.zip"
                                    ),
                                    unzip_args_dict = list("exdir" = ".", "overwrite" = TRUE),
                                    clear_csv = TRUE,
                                    quiet = FALSE,
                                    use_cache_on_failure = TRUE,
                                    cache_diccionario = NULL,
                                    force_download = FALSE,
                                    show_warnings = TRUE,
                                    board_url_name_dict = "diccionario_covid",
                                    download_file_args = list(
                                      method   = "curl",
                                      destfile = tempfile(),
                                      quiet    = quiet
                                    ),
                                    descarga_db_diccionario_ssa_args = list(),
                                    prestatement = paste0(
                                      "SET sql_mode = 'NO_ENGINE_SUBSTITUTION,",
                                      "NO_AUTO_CREATE_USER';"
                                    ),
                                    additional_mysqlimport_flags = c("--local"),
                                    load_data_infile_options = list(
                                      "LOW_PRIORITY" = "",
                                      "CONCURRENT"   = "",
                                      "LOCAL"        = "LOCAL",
                                      "PARTITION"    = ""
                                    ),
                                    ...) {
  do.call(descarga_datos_abiertos, as.list(environment()))
}

#' @export
#' @rdname read_datos_abiertos
#' @inheritParams descarga_datos_abiertos
read_datos_abiertos_MariaDB <- function(user = Sys.getenv("MariaDB_user"),
                                        password = Sys.getenv("MariaDB_password"),
                                        dbname = Sys.getenv("MariaDB_dbname"),
                                        host = Sys.getenv("MariaDB_host"),
                                        group = Sys.getenv("MariaDB_group"),
                                        port = Sys.getenv("MariaDB_port"),
                                        driver = RMariaDB::MariaDB(),
                                        tblname = "covidmx",
                                        diccionario_zip_path = NULL,
                                        diccionario_unzipped_path = NULL,
                                        diccionario = NULL,
                                        download_process = c("pins", "download.file"),
                                        site.covid.dic = paste0(
                                          "http://datosabiertos.salud.",
                                          "gob.mx/gobmx/salud/datos_a",
                                          "biertos/diccionario_datos_",
                                          "covid19.zip"
                                        ),
                                        unzip_args_dict = list("exdir" = ".", "overwrite" = TRUE),
                                        clear_zip = download_process[1] != "pins",
                                        clear_csv = TRUE,
                                        use_dict = TRUE,
                                        quiet = FALSE,
                                        use_cache_on_failure = TRUE,
                                        cache_diccionario = NULL,
                                        force_download = FALSE,
                                        show_warnings = TRUE,
                                        board_url_name_dict = "diccionario_covid",
                                        download_file_args = list(
                                          method   = "curl",
                                          destfile = tempfile(),
                                          quiet    = quiet
                                        ),
                                        descarga_db_diccionario_ssa_args = list(),
                                        ...) {

  # Creamos la conexión de MARIADB
  dbname <- ifelse(dbname == "", "covidmx", dbname)
  host <- ifelse(host == "", "localhost", host)
  port <- ifelse(port == "", "8787", port)

  if (!quiet) {
    message(paste0("host: ", host))
    message(paste0("DATASET: ", dbname))
    message(paste0("port: ", port))
  }

  # Get file connection
  con <- DBI::dbConnect(
    drv = driver,
    host = host,
    port = port,
    user = user,
    group = group,
    password = password,
    dbname = dbname,
    ...
  )

  dats <- dplyr::tbl(con, tblname)

  if (!quiet) {
    message(glue::glue("No olvides desconectar la base con datos_covid$disconnect() al final.

                        Don't forget to disconnect the dataset with datos_covid$disconnect() at
                        the end."))
  }

  # Creamos función de desconexión
  disconnect <- function() {
    DBI::dbDisconnect(con)
  }

  datos_abiertos_tbl <- list("dats" = dats, "disconnect" = disconnect)


  # Checamos si hay que usar diccionario
  if (use_dict) {
    diccionario <- descarga_diccionario(
      download_process = download_process,
      site.covid.dic = site.covid.dic,
      quiet = quiet,
      clear_zip = clear_zip,
      clear_csv = clear_csv,
      diccionario_zip_path = diccionario_zip_path,
      diccionario_unzipped_path = diccionario_unzipped_path,
      diccionario = diccionario,
      board_url_name_dict = board_url_name_dict,
      cache_diccionario = cache_diccionario,
      use_cache_on_failure = use_cache_on_failure,
      download_file_args_dict = download_file_args,
      unzip_args_dict = unzip_args_dict,
      force_download = force_download,
      show_warnings = show_warnings,
      descarga_db_diccionario_ssa_args = descarga_db_diccionario_ssa_args
    )
  } else {
    diccionario <- NULL
  }

  # Pegamos todo
  datos_covid <- pega_db_datos_abiertos_tbl_y_diccionario(
    datos_abiertos_tbl = datos_abiertos_tbl,
    diccionario = diccionario
  )
}
