#' DESCARGA BASE DE DATOS ABIERTOS DE LA DIRECCION GENERAL DE EPIDEMIOLOGIA
#'
#' @description
#' `descarga_datos_abiertos` downloads and labels
#' all COVID-19 data for Mexico from the Direccion General de EpidemiologIa at
#' \url{https://www.gob.mx/salud/documentos/datos-abiertos-152127}.
#'
#' `descarga_datos_abiertos` descarga y etiqueta todos los datos de COVID-19
#' de Mexico de la Direccion General de EpidemiologIa en
#' \url{https://www.gob.mx/salud/documentos/datos-abiertos-152127}.
#'
#' @details
#' While reading the data it is perfectly normal to get a `Warning` for
#' `parsing`: the dataset has issues.
#'
#' En la lectura de datos es normal tener un `Warning` por `parsing`:
#' la base viene chueca.
#'
#' This is not an official product / este no es un producto oficial
#'
#' @param download_method Methods for download file (default = "curl"). Other
#' options are "internal", "wininet" (Windows) "libcurl", "wget", "curl". See
#' \code{\link[utils]{download.file}} / Metodos para descargar el archivo
#' (default = "curl"). Otras opciones incluyen "interno","wininet" (Windows) "
#' libcurl", "wget", "curl". Vease \code{\link[utils]{download.file}}.
#' @param file_download_data Name of file to save the data / Nombre del archivo
#' para guardar los datos.
#' @param file_download_dictionary Name of file to save the dictionary /
#' Nombre del archivo donde guardar el diccioanrio.
#' @param parse_dictionary Change labels to values of dictionary? Default = TRUE
#' else the data is downloaded and presented as is. / Se agregan etiquetas
#' a partir del diccionario? `default = TRUE` en caso contrario solo se presentan los
#' datos descargados como estan.
#' @param remove_zip_after_download If the downloaded zip file should be saved /
#' Si los archivos zip descargados deben ser almacenados.
#' @param quiet Show messages? / Mostrar mensajes?
#' @param parse_warnings If show parsing errors when reading csv /
#' Si muestra errores cuando lee el csv
#' @param language ('Español', 'English') Message languages/ Idiomas del mensaje.
#' @param read_format \code{"MariaDB"} or \code{"tibble"} establishes how the database
#' should be stored. In most machines \code{"tibble"} will result in a memory error
#' @param user User for \code{dbConnect} i.e. your `MariaDB` user
#' @param password password for \code{dbConnect} i.e. your `MariaDB` password
#' @param dbname Database name for \code{dbConnect} i.e. what database is going to be stored
#' @param host Host for \code{dbConnect} i.e. your `MariaDB` host (usually localhost)
#' @param group Group for \code{dbConnect} i.e. your `MariaDB` group (can be NULL)
#' @param port Port connection for `MariaDB`
#' @param tblname Name of table to save in `MariaDB`
#' @param nthreads Number of threads for writing to `MariaDB`.
#' @return List of values:
#' \itemize{
#'   \item dats - Database table (if MARIADB) or database in tibble (if tibble)
#'   \item con - Database connection (if MARIADB) or \code{NULL} (if tibble)
#'   \item dict - List of tibbles containing the whole dictionary
#' }
#'
#'@examples
#'\dontrun{
#'datos_covid <- descarga_datos_abiertos(language = "Espanol")
#'}
#' @encoding UTF-8
#' @export
descarga_datos_abiertos <- function(download_method           = "curl",
                                    file_download_data        = tempfile(),
                                    file_download_dictionary  = tempfile(),
                                    remove_zip_after_download = TRUE,
                                    quiet                     = FALSE,
                                    parse_warnings            = FALSE,
                                    language                  = c("English", "Espa\u00f1ol"),
                                    read_format               = c("MariaDB","tibble"),
                                    user                      = Sys.getenv("MariaDB_user"),
                                    password                  = Sys.getenv("MariaDB_password"),
                                    dbname                    = Sys.getenv("MariaDB_dbname"),
                                    host                      = Sys.getenv("MariaDB_host"),
                                    group                     = Sys.getenv("MariaDB_group"),
                                    port                      = Sys.getenv("MariaDB_port"),
                                    tblname                   = "covidmx",
                                    nthreads                  = max(parallel::detectCores() - 1, 1)){

  #Check inputs----
  if (stringr::str_detect(toupper(language[1]),"ESPA.*OL")){
    language <- "Espa\u00f1ol"
  } else {
    language <- language[1]
  }

  if (read_format[1] %in% c("MariaDB","tibble")){
    read_format <- read_format[1]
  } else {
    if (language == "English"){
      stop("Invalid format select MariaDB or tibble")
    } else {
      stop("Formato invalido. Selecciona MariaDB o tibble")
    }
  }

  if (!quiet){
    if (language == "Espa\u00f1ol"){
      message("Por favor se paciente todo el proceso toma aprox 20 minutos.")
    } else {
      message("Please be patient, the whole process takes aprox 20 minutes.")
    }
  }

  #Download dataset----
  site.covid <- paste0(
    "http://datosabiertos.salud.gob.mx/gobmx/salud",
    "/datos_abiertos/datos_abiertos_covid19.zip"
  )

  if (!quiet) {
    if (language == "Espa\u00f1ol"){
      message(paste0("Descargando datos de:\n", site.covid))
    } else {
      message(paste0("Downloading data from:\n", site.covid))
    }
  }

  if (RCurl::url.exists(site.covid)){

    download.file(site.covid, file_download_data,
      method = download_method,
      quiet = quiet
    )

    if (!quiet) {
      if (language == "Espa\u00f1ol"){
        message(paste0("Descargado en ",file_download_data))
      } else {
        message(paste0("Downloaded in ",file_download_data))
      }
    }

    options(readr.show_progress = !quiet)

    #Unzip file-----
    filecon <- tryCatch({
      filecon <- unzip(file_download_data)
      },
      warning = function(cond) {
        system2("unzip", args = c("-o",file_download_data))
        filecon <- list.files(pattern = "*COVID19MEXICO.csv", full.names = T)[1]
        return(filecon)
      },
      error = function(cond){
        if (language == "Espa\u00f1ol"){
          stop("No se puede leer el archivo hubo un problema con la descarga.")
        } else {
          stop("Unable to read file. Problem with download.")
        }
    })

    #Read dataset-----
    if (read_format == "MariaDB"){
      # > MariaDB-----

      if (dbname == ""){
        dbname <- "COVID"
        message(paste0("Creating dataset named ", dbname))
      }

      if (host == ""){
        host <- "localhost"
        message(paste0("Using host ", host))
      }

      if (port == ""){
        port <- "8787"
        message(paste0("Using port ", port))
      }

      #Get file connection
      con    <- DBI::dbConnect(RMariaDB::MariaDB(),
                          host     = host,
                          port     = port,
                          user     = user,
                          group    = group,
                          password = password,
                          dbname   = dbname)

      header <- readr::read_csv(filecon,
                                locale = readr::locale(encoding = "UTF-8"),
                                n_max = 1,
                                trim_ws = TRUE,
                                col_types = readr::cols(
                                  FECHA_ACTUALIZACION   = readr::col_date(format = "%Y-%m-%d"),
                                  ID_REGISTRO           = readr::col_character(),
                                  ORIGEN                = readr::col_double(),
                                  SECTOR                = readr::col_double(),
                                  ENTIDAD_UM            = readr::col_character(),
                                  SEXO                  = readr::col_double(),
                                  ENTIDAD_NAC           = readr::col_character(),
                                  ENTIDAD_RES           = readr::col_character(),
                                  MUNICIPIO_RES         = readr::col_character(),
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
                                  PAIS_NACIONALIDAD     = readr::col_character(),
                                  PAIS_ORIGEN           = readr::col_character(),
                                  UCI                   = readr::col_double()
                                ))

      #____Creating MARIADB table format-----
      dbres <- DBI::dbSendStatement(conn = con,
                      statement = "SET sql_mode = 'NO_ENGINE_SUBSTITUTION,NO_AUTO_CREATE_USER';")
      DBI::dbClearResult(dbres)

      DBI::dbWriteTable(conn = con, name = tblname, value = header, overwrite = T)

      dbres <- DBI::dbSendStatement(conn = con, statement = glue::glue("DELETE FROM {tblname};"))
      DBI::dbClearResult(dbres)

      file.rename(filecon, glue::glue("./{tblname}.csv"))

      tryCatch({
        message("Intentando crear tabla en paralelo | Attempting to create table in parallel")
        system(glue::glue("mysqlimport --default-character-set=UTF8",
                          " --fields-terminated-by=','",
                          " --ignore-lines=1",
                          " --fields-enclosed-by='\"'",
                          " --lines-terminated-by='\\n'",
                          " --user={user}",
                          " --password={password}",
                          " --use-threads={nthreads}",
                          " --local COVID ./{tblname}.csv"))
      },
      error=function(e) {

        #____Writing to table-----
        query_to_writedata <- glue::glue("LOAD DATA LOCAL INFILE \'{filecon}\' ",
                                         "REPLACE INTO TABLE {tblname} ",
                                         "CHARACTER SET UTF8 ",
                                         "COLUMNS TERMINATED BY ',' ",
                                         "ENCLOSED BY '\"'",
                                         "LINES TERMINATED BY '\\n' ",
                                         "IGNORE 1 LINES;")

        message(glue::glue("Created {tblname} table | Cree la tabla {tblname}"))
        dbres <- DBI::dbSendStatement(conn = con, statement = query_to_writedata)
        DBI::dbClearResult(dbres)

      })

      dats <- dplyr::tbl(con, tblname)
      if (language == "Espa\u00f1ol"){
        message(glue::glue("No olvides desconectar la base con ",
                "DBI::dbDisconnect(datos_covid$con) cuando termines."))
      } else {
        message("Don't forget to DBI::dbDisconnect(datos_covid$con) at the end")
      }

    } else {
      #> CSV----
      dats <- readr::read_csv(con,
        locale = readr::locale(encoding = "UTF-8"),
        col_types = readr::cols(
          FECHA_ACTUALIZACION   = readr::col_date(format = "%Y-%m-%d"),
          ID_REGISTRO           = readr::col_character(),
          ORIGEN                = readr::col_double(),
          SECTOR                = readr::col_double(),
          ENTIDAD_UM            = readr::col_character(),
          SEXO                  = readr::col_double(),
          ENTIDAD_NAC           = readr::col_character(),
          ENTIDAD_RES           = readr::col_character(),
          MUNICIPIO_RES         = readr::col_character(),
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
          PAIS_NACIONALIDAD     = readr::col_character(),
          PAIS_ORIGEN           = readr::col_character(),
          UCI                   = readr::col_double()
          ))

      con <- NULL
    }

    # Descarga de diccionario de datos para ver el nombre del estado
    site.covid.dic <- paste0(
      "http://datosabiertos.salud.gob.mx/gobmx/salud/",
      "datos_abiertos/diccionario_datos_covid19.zip"
    )

    #Dictionary----
    if (RCurl::url.exists(site.covid.dic)) {

      if (!quiet) {
        if (language == "Espa\u00f1ol"){
          message(paste0("Descargando diccionario de:\n", site.covid.dic))
        } else {
          message(paste0("Downloading dictionary from:\n", site.covid.dic))
        }
      }

      download.file(site.covid.dic, file_download_dictionary,
        method = download_method, quiet = quiet
      )

      if (language == "Espa\u00f1ol"){
        message(paste0("Descarga de diccionario en ",file_download_dictionary))
      } else {
        message(paste0("Downloaded dictionary in ",file_download_dictionary))
      }

      filenames <- unzip(zipfile = file_download_dictionary, list = TRUE)
      fname     <- filenames[
        which(stringr::str_detect(filenames$Name, "Cat.*logo.*")),
        "Name"
      ]

      unzip(zipfile = file_download_dictionary, files = fname, exdir = ".")

      dic <- list()

      #> ORIGEN----
      if (!quiet) {
        message("+ ORIGEN")
      }

      diccionario.covid.origen <- list("ORIGEN" =
        readxl::read_excel(fname,
          sheet = "Cat\u00e1logo ORIGEN",
          col_types = c("numeric", "text")
        )
      )

      #> SECTOR----
      if (!quiet) {
        message("+ SECTOR")
      }

      diccionario.covid.sector <- list("SECTOR" =
        readxl::read_excel(fname,
          sheet = "Cat\u00e1logo SECTOR",
          col_types = c("numeric", "text")
        )
      )

      #> SEXO----
      if (!quiet) {
        message("+ SEXO")
      }

      diccionario.covid.sexo <- list("SEXO" =
        readxl::read_excel(fname,
          sheet = "Cat\u00e1logo SEXO",
          col_types = c("numeric", "text")
        )
      )

      #> TIPO DE PACIENTE----
      if (!quiet) {
        message("+ TIPO_PACIENTE")
      }

      diccionario.covid.paciente <- list("PACIENTE" =
        readxl::read_excel(fname,
          sheet = "Cat\u00e1logo TIPO_PACIENTE",
          col_types = c("numeric", "text")
        )
      )

      #> NACIONALIDAD----
      if (!quiet) {
        message("+ NACIONALIDAD")
      }

      diccionario.covid.nacionalidad <-
        list("NACIONALIDAD" = readxl::read_excel(fname,
                                              sheet = "Cat\u00e1logo NACIONALIDAD",
                                              col_types = c("numeric", "text"))
        )


      #> RESULTADO_LAB----
      if (!quiet) {
        message("+ RESULTADO_LAB")
      }

      diccionario.covid.resultado_lab <-
        list("RESULTADO_LAB" =
               readxl::read_excel(fname,
                                  sheet = "Cat\u00e1logo RESULTADO_LAB",
                                  col_types = c("numeric", "text")
               )
             )

      #> RESULTADO_ANTIGENO----
      if (!quiet) {
        message("+ RESULTADO_ANTIGENO")
      }

      diccionario.covid.resutlado_antigeno <-
        list("RESULTADO_ANTIGENO" = readxl::read_excel(fname,
                                              sheet = "Cat\u00e1logo RESULTADO_ANTIGENO",
                                              col_types = c("numeric", "text"))
        )

      #> CLASIFICACION_FINAL----
      if (!quiet) {
        message("+ CLASIFICACION_FINAL")
      }

      diccionario.covid.clasificacion_final <- list(
        "CLASIFICACION_FINAL" = readxl::read_excel(fname,
                                                   sheet = "Cat\u00e1logo CLASIFICACION_FINAL",
                                                   col_types = c("numeric", "text", "text")
      ))

      if (!quiet) {
        message("+ MUNICIPIO_RES")
      }

      #> CATALOGO DE MUNICIPIO----
      diccionario.covid.municipio_res <- list("MUNICIPIO_RES" =
                                  readxl::read_excel(fname,
                                                     sheet = "Cat\u00e1logo MUNICIPIOS",
                                                     col_types = c("text", "text", "text")
      ))


      #> CATALOGO SI/NO:----
      # INTUBADO, NEUMONIA, EMBARAZO, HABLA LENGUA INDIGENA, INDIGENA, DIABETES
      # EPOC, ASMA, INMUSUPR, HIPERTENSION, OTRA_COMORBILIDAD, OBESIDAD,
      # RENAL_CRONICA, TABAQUISMO, MIGRANTE, UCI
      for (variable in c("INTUBADO", "NEUMONIA", "EMBARAZO", "HABLA LENGUA INDIGENA", "INDIGENA",
                          "DIABETES", "EPOC", "ASMA", "INMUSUPR", "HIPERTENSION",
                         "CARDIOVASCULAR","OTRO_CASO","TOMA_MUESTRA_LAB", "TOMA_MUESTRA_ANTIGENO",
                         "OTRA_COMORBILIDAD", "OBESIDAD","RENAL_CRONICA","TABAQUISMO","UCI")){

        if (!quiet) {
          message(paste0("+ ", variable))
        }

        temp_list <- list()
        temp_list[[variable]] <- readxl::read_excel(fname, sheet = "Cat\u00e1logo SI_NO",
                                            col_types = c("numeric", "text"))

        assign(paste0("diccionario.covid.", tolower(variable)), temp_list)
      }

      #> CATALOGO ENTIDAD----
      # ENTIDAD_UM, ENTIDAD_RES, ENTIDAD_NAC
      for (variable in c("ENTIDAD_UM", "ENTIDAD_RES", "ENTIDAD_NAC")){

        if (!quiet) {
          message(paste0("+ ", variable))
        }

        temp_list <- list()
        temp_list[[variable]] <- readxl::read_excel(fname,
                                                    sheet = "Cat\u00e1logo de ENTIDADES",
                                                    col_types = c("text", "text", "text"))

        assign(paste0("diccionario.covid.", tolower(variable)), temp_list)

      }

      dict <- c(diccionario.covid.asma, diccionario.covid.cardiovascular,
               diccionario.covid.clasificacion_final, diccionario.covid.diabetes,
               diccionario.covid.embarazo, diccionario.covid.entidad_nac,
               diccionario.covid.entidad_res, diccionario.covid.entidad_um,
               diccionario.covid.epoc, `diccionario.covid.habla lengua indigena`,
               diccionario.covid.hipertension, diccionario.covid.indigena,
               diccionario.covid.inmusupr, diccionario.covid.intubado,
               diccionario.covid.municipio_res, diccionario.covid.nacionalidad,
               diccionario.covid.neumonia, diccionario.covid.obesidad,
               diccionario.covid.origen, diccionario.covid.otra_comorbilidad,
               diccionario.covid.otro_caso, diccionario.covid.paciente,
               diccionario.covid.renal_cronica, diccionario.covid.resultado_lab,
               diccionario.covid.resutlado_antigeno, diccionario.covid.sector,
               diccionario.covid.sexo, diccionario.covid.tabaquismo,
               diccionario.covid.toma_muestra_antigeno, diccionario.covid.toma_muestra_lab,
               diccionario.covid.uci)

      if (remove_zip_after_download) {
        unlink(file_download_dictionary)
        unlink(fname)
      }

    } else if (!RCurl::url.exists(site.covid.dic)) {
      if (language == "Espa\u00f1ol"){
        stop(glue::glue("No se pudo encontrar el diccionario en {site.covid.dic}"))
      } else {
        stop(glue::glue("Unable to locate dictionary try again in {site.covid.dic}"))
      }
    }

    if (remove_zip_after_download) {
      unlink(file_download_data)
    }

    if (!quiet) {
      if (language == "Espa\u00f1ol"){
        message("Termine de leer los datos.")
      } else {
        message("Finished reading data.")
      }
    }

    if (language == "Espa\u00f1ol"){
      message("Por favor no olvides citar este paquete. Ve citation('covidmx').")
    } else {
      message("Please don't forget to cite the package. See citation('covidmx').")
    }

  } else {
    if (language == "Espa\u00f1ol"){
      stop(paste0("No pude encontrar los datos en ", site.covid))
    } else {
      stop(paste0("Unable to locate dataset at ", site.covid))
    }
    dats <- NULL; con <- NULL
  }
  return(list(dats = dats, con = con, dict = dict))
}
