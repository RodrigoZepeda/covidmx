#' LEE LA BASE DE DATOS ABIERTOS DE LA DIRECCION GENERAL DE EPIDEMIOLOGIA QUE YA DESCARGASTE
#'
#' @description
#' `read_datos_abiertos` Lee los datos abiertos almacenados en tu base de `MariaDB` que
#' bajaste con `descarga_datos_abiertos`
#'
#' @details
#'
#' _This is not an official product / este no es un producto oficial_
#'
#' **Spanish**
#' Necesitas tener una instalación funcionando de [`MariaDB`](https://mariadb.com/). El programa
#' se encarga de leer la base de datos de la DGE que bajamos con `descarga_datos_abiertos`.
#' Basta con leer la tabla de nombre `covidmx` dentro de tu database `dbname`.
#' Asegúrate de que tu usuario `user` tenga permisos de lectura.
#' Para más información sobre instalación y uso de `MariaDB` consulta la viñeta **rellenar**.
#'
#' @param user User for \code{dbConnect} i.e. your `MariaDB` user
#' @param password password for \code{dbConnect} i.e. your `MariaDB` password
#' @param dbname Database name for \code{dbConnect} i.e. what database is going to be stored
#' @param host Host for \code{dbConnect} i.e. your `MariaDB` host (usually localhost)
#' @param group Group for \code{dbConnect} i.e. your `MariaDB` group (can be NULL)
#' @param port Port connection for `MariaDB`
#' @param tblname Name of table to save in `MariaDB`
#' @param file_download_dictionary Name of file to save the dictionary /
#' Nombre del archivo donde guardar el diccioanrio.
#' @param remove_zip_after_download If the downloaded zip file should be saved /
#' Si los archivos zip descargados deben ser almacenados.
#' @param site.covid.dic Site for download the covid dictionary for the data
#' @param dict_file RData File where to save/read the dictionary.
#' @param download_dict If download the dictionary from `site.covid.dic`. If
#' `download_dict` is `FALSE` then `dict_file` must be specified to read the dictionary.
#' @param download_method Methods for download file (default = "curl"). Other
#' options are "internal", "wininet" (Windows) "libcurl", "wget", "curl". See
#' \code{\link[utils]{download.file}} / Metodos para descargar el archivo
#' (default = "curl"). Otras opciones incluyen "interno","wininet" (Windows) "
#' libcurl", "wget", "curl". Vease \code{\link[utils]{download.file}}.

#' @param save_dict If the downloaded dictionary is to be saved in file `dict_file`
#' @return List of values:
#' \itemize{
#'   \item dats - Database table (if MARIADB) or database in tibble (if tibble)
#'   \item con - Database connection (if MARIADB) or \code{NULL} (if tibble)
#'   \item dict - List of tibbles containing the whole dictionary
#' }
#'
#'@examples
#'\dontrun{
#'datos_covid <- read_datos_abiertos()
#'}
#' @encoding UTF-8
#' @export
read_datos_abiertos <- function(
    user                      = Sys.getenv("MariaDB_user"),
    password                  = Sys.getenv("MariaDB_password"),
    dbname                    = Sys.getenv("MariaDB_dbname"),
    host                      = Sys.getenv("MariaDB_host"),
    group                     = Sys.getenv("MariaDB_group"),
    port                      = Sys.getenv("MariaDB_port"),
    tblname                   = "covidmx",
    dict_file                 = "diccionario_covid.RData",
    download_dict             = FALSE,
    download_method           = "curl",
    save_dict                 = download_dict,
    file_download_dictionary  = tempfile(),
    remove_zip_after_download = TRUE,
    site.covid.dic            = paste0("http://datosabiertos.salud.gob.mx/gobmx/salud/",
                                       "datos_abiertos/diccionario_datos_covid19.zip")){

    quiet <- FALSE

    #Define dictionary global vars
    diccionario.covid.asma                    <- NULL
    diccionario.covid.cardiovascular          <- NULL
    diccionario.covid.diabetes                <- NULL
    diccionario.covid.embarazo                <- NULL
    diccionario.covid.entidad_nac             <- NULL
    diccionario.covid.entidad_res             <- NULL
    diccionario.covid.entidad_um              <- NULL
    diccionario.covid.epoc                    <- NULL
    diccionario.covid.hipertension            <- NULL
    diccionario.covid.indigena                <- NULL
    diccionario.covid.inmusupr                <- NULL
    diccionario.covid.intubado                <- NULL
    diccionario.covid.neumonia                <- NULL
    diccionario.covid.obesidad                <- NULL
    diccionario.covid.otra_comorbilidad       <- NULL
    diccionario.covid.otro_caso               <- NULL
    diccionario.covid.renal_cronica           <- NULL
    diccionario.covid.tabaquismo              <- NULL
    diccionario.covid.toma_muestra_antigeno   <- NULL
    diccionario.covid.toma_muestra_lab        <- NULL
    diccionario.covid.uci                     <- NULL
    `diccionario.covid.habla lengua indigena` <- NULL

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


    dats <- dplyr::tbl(con, tblname)

    message(glue::glue("No olvides desconectar la base con ",
                       "datos_covid$disconnect() cuando termines."))

    #Dictionary----
    if (download_dict & RCurl::url.exists(site.covid.dic)) {

      if (!quiet) {
        message(paste0("Descargando diccionario de:\n", site.covid.dic))
      }

      download.file(site.covid.dic, file_download_dictionary,
                    method = download_method)


      message(paste0("Descarga de diccionario en ",file_download_dictionary))


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

      if (save_dict){
        message(glue::glue("Saving / Guardando en {save_dict}"))
        save(dict, file = dict_file)
      }

    } else if (!download_dict) {

      load(dict_file)

    } else if (!RCurl::url.exists(site.covid.dic)) {
        stop(glue::glue("No se pudo encontrar el diccionario en {site.covid.dic}"))
    }


  return(list(dats = dats, disconnect = function(){DBI::dbDisconnect(con)}, dict = dict))
}
