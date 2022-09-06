#' LEE LA BASE DE DATOS ABIERTOS DE LA DIRECCION GENERAL DE EPIDEMIOLOGIA QUE YA DESCARGASTE
#'
#' @description
#' `read_datos_abiertos` Lee los datos abiertos almacenados en tu base de `duckdb` que
#' bajaste con `descarga_datos_abiertos`. Intenta de manera automática determinar
#' si los lee de `duckdb`, `csv` ó `zip`
#'
#' La funcion principal es [read_datos_abiertos()] la cual decide si los lee de `zipp , `duckdb` o `csv`
#' Tambien puedes usar las auxiliares respectivas
#' * [read_datos_abiertos_zip()]     Si sólo descargaste los datos de la DGE en `.zip`
#' * [read_datos_abiertos_csv()]     Si descargaste los datos de la DGE en `.zip` y los descomprimiste.
#' * [read_datos_abiertos_duckdb()]  Si ya creaste tu table en `duckdb`
#'
#' @note Para guardar tu base con `duckdb` cambia el `dbdir` a un archivo `.duckdb`. Como ejemplo
#' `dbdir = "ejemplo.duckdb"`.
#'
#' @return Lista de valores:
#' \itemize{
#'   \item dats        - Tabla conectada mediante `DBI::dbConnect` (si `duckdb`) o
#'                       tibble (si `tibble`)
#'   \item disconnect  - Funcion para cerrar la conexion a la base de datos.
#'   \item dict        - Lista de `tibble`s con el diccionario de datos para cada variable
#' }
#' @inheritParams descarga_datos_abiertos
#' @param datos_abiertos_path Camino a los datos abiertos si son un `zip`, un `csv` o un `.duckdb`
#' @examples
#' \dontrun{
#' # Lee los datos de duckdb una vez descargados
#' descarga_datos_abiertos()
#' datos_covid <- read_datos_abiertos()
#'
#' # Es lo mismo que:
#' datos_covid <- read_datos_abiertos_duckdb()
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
read_datos_abiertos <- function(datos_abiertos_path,
                                tblname = "covidmx",
                                pragma_memory_limit = "1GB",
                                drv = duckdb::duckdb(),
                                dbdir = tempfile(fileext = ".duckdb"),
                                colClasses = get_col_class(),
                                read_format = c("duckdb", "tibble"),
                                ...) {
  if (tools::file_ext(datos_abiertos_path) == "duckdb") {
    datos_covid <- read_datos_abiertos_duckdb(
      dbdir    = datos_abiertos_path,
      pragma_memory_limit = pragma_memory_limit,
      drv      = drv,
      tblname  = tblname,
      ...
    )
  } else if (tools::file_ext(datos_abiertos_path) == "csv") {
    datos_covid <- read_datos_abiertos_csv(
      datos_abiertos_unzipped_path = datos_abiertos_path,
      drv      = drv,
      tblname  = tblname,
      dbdir    = dbdir,
      read_format = read_format,
      colClasses = colClasses,
      ...
    )
  } else if (tools::file_ext(datos_abiertos_path) == "zip") {
    datos_covid <- read_datos_abiertos_zip(
      datos_abiertos_zip_paths = datos_abiertos_path,
      drv         = drv,
      tblname     = tblname,
      dbdir       = dbdir,
      read_format = read_format,
      colClasses  = colClasses,
      ...
    )
  }

  return(datos_covid)
}


#' @export
#' @rdname read_datos_abiertos
#' @inheritParams descarga_datos_abiertos
# Wrapper para leer los datos abiertos de un zip
read_datos_abiertos_zip <- function(datos_abiertos_zip_paths,
                                    diccionario_zip_path = NULL,
                                    diccionario_unzipped_path = NULL,
                                    diccionario = NULL,
                                    read_format = c("duckdb", "tibble"),
                                    tblname = "covidmx",
                                    drv = duckdb::duckdb(),
                                    dbdir = tempfile(fileext = ".duckdb"),
                                    colClasses = get_col_class(),
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
                                    clear_zip = (download_process[1] != "pins"),
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
                                    read_format = c("duckdb", "tibble"),
                                    tblname = "covidmx",
                                    drv = duckdb::duckdb(),
                                    dbdir = tempfile(fileext = ".duckdb"),
                                    colClasses = get_col_class(),
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
                                    ...) {
  do.call(descarga_datos_abiertos, as.list(environment()))
}

#' @export
#' @rdname read_datos_abiertos
#' @inheritParams descarga_datos_abiertos
read_datos_abiertos_duckdb <- function(dbdir,
                                       drv = duckdb::duckdb(),
                                       tblname = "covidmx",
                                       pragma_memory_limit = "1GB",
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



  # Get file connection
  con <- duckdb::dbConnect(
    drv   = drv,
    dbdir = dbdir,
    ...
  )

  # Memory limit
  DBI::dbExecute(con, paste0("PRAGMA memory_limit='", pragma_memory_limit, "'"))

  dats <- dplyr::tbl(con, tblname)

  # Formateo
  dats <- dats |>
    dplyr::mutate(dplyr::across(
      dplyr::all_of(c(
        "ORIGEN", "SECTOR", "SEXO",
        "TIPO_PACIENTE", "INTUBADO", "NEUMONIA", "EDAD", "NACIONALIDAD", "EMBARAZO",
        "HABLA_LENGUA_INDIG", "INDIGENA", "DIABETES", "EPOC", "ASMA", "INMUSUPR", "HIPERTENSION", "OTRA_COM",
        "CARDIOVASCULAR", "OBESIDAD", "RENAL_CRONICA", "TABAQUISMO", "OTRO_CASO", "TOMA_MUESTRA_LAB",
        "RESULTADO_LAB", "TOMA_MUESTRA_ANTIGENO", "RESULTADO_ANTIGENO", "CLASIFICACION_FINAL", "MIGRANTE",
        "UCI"
      )),
      ~ as.integer(.)
    )) |>
    dplyr::mutate(dplyr::across(dplyr::starts_with("FECHA"), ~ dplyr::if_else(. == "9999-99-99", NA_character_, .))) |>
    dplyr::mutate(dplyr::across(dplyr::starts_with("FECHA"), ~ dplyr::if_else(. == "-001-11-30", NA_character_, .))) |>
    dplyr::mutate(dplyr::across(dplyr::starts_with("FECHA"), ~ strptime(., "%Y-%m-%d")))

  # Creamos función de desconexión
  disconnect <- function() {
    duckdb::dbDisconnect(con, shutdown = TRUE)
    cli::cli_alert_success("Desconectado")
  }

  # Mensaje de desconexión
  if (!quiet) {
    cli::cli_alert_info(
      "No olvides desconectar la base con {.code datos_covid$disconnect()} al final."
    )
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
