#' Descarga base de datos abiertos de la DGE
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
#' En la lectura de datos es normal tener un `Warning` por `parsing`:
#' la base viene chueca.
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
#' a partir del diccionario? Default = TRUE en caso contrario solo se presentan los
#' datos descargados como estan.
#' @param remove_zip_after_download If the downloaded zip file should be saved /
#' Si los archivos zip descargados deben ser almacenados.
#' @param quiet Show messages? / Mostrar mensajes?
#' @param parse_warnings If show parsing errors when reading csv /
#' Si muestra errores cuando lee el csv
#' @param language ('Espanol', 'English') Message languages/ Idiomas del mensaje.
#'
#'@examples
#'\dontrun{
#'datos_covid <- descarga_datos_abiertos(language = "Espanol")
#'}
#'
#' @export
descarga_datos_abiertos <- function(download_method = "curl",
                                    file_download_data = tempfile(),
                                    file_download_dictionary = tempfile(),
                                    remove_zip_after_download = TRUE,
                                    quiet = FALSE,
                                    parse_dictionary = TRUE,
                                    parse_warnings = FALSE,
                                    language = "English") {

  if (stringr::str_detect(toupper(language),"ESPA.*OL")){
    language <- "Espa\u00f1ol"
  }

  if (!quiet){
    if (language == "Espa\u00f1ol"){
      message("Por favor se paciente todo el proceso toma ~10 minutos.")
    } else {
      message("Please be patient, the whole process takes ~10 minutes.")
    }
  }
  # Descarga de la base principal
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

  download.file(site.covid, file_download_data,
    method = download_method,
    quiet = quiet
  )

  if (!quiet) {
    if (language == "Espa\u00f1ol"){
      message("Descarga completada. Estoy leyendo los datos...")
    } else {
      message("Completed download. Reading data...")
    }
  }

  # Lectura de la base
  options(readr.show_progress = !quiet)
  if (!parse_warnings){
    suppressWarnings({
      dats <- readr::read_csv(unz(
        file_download_data,
        unzip(file_download_data, list = TRUE)["Name"]
      ),
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
        )
      )
    })
  }

  if (language == "Espa\u00f1ol"){
    dats$Fuente         <- site.covid
    dats$Fecha_descarga <- date()
  } else {
    dats$Source        <- site.covid
    dats$Download_date <- date()
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

  if (parse_dictionary) {

    # Descarga de diccionario de datos para ver el nombre del estado
    site.covid.dic <- paste0(
      "http://datosabiertos.salud.gob.mx/gobmx/salud/",
      "datos_abiertos/diccionario_datos_covid19.zip"
    )

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
      message("Etiquetando datos")
    } else {
      message("Labeling data")
    }

    filenames <- unzip(zipfile = file_download_dictionary, list = TRUE)
    fname <- filenames[
      which(stringr::str_detect(filenames$Name, "Cat.*logo.*")),
      "Name"
    ]

    unzip(zipfile = file_download_dictionary, files = fname, exdir = ".")

    # ORIGEN
    if (!quiet) {
      message("+ ORIGEN")
    }

    diccionario.covid <- readxl::read_excel(fname,
      sheet = "Cat\u00e1logo ORIGEN",
      col_types = c("numeric", "text")
    )
    dats <- dats %>%
      dplyr::left_join(diccionario.covid, by = c("ORIGEN" = "CLAVE")) %>%
      dplyr::select(-.data$ORIGEN) %>%
      dplyr::rename_with(.cols = dplyr::matches("DESCRIPCI\u00d3N"),
                         function(x) "ORIGEN")

    # SECTOR
    if (!quiet) {
      message("+ SECTOR")
    }

    diccionario.covid <- readxl::read_excel(fname,
      sheet = "Cat\u00e1logo SECTOR",
      col_types = c("numeric", "text")
    )
    dats <- dats %>%
      dplyr::left_join(diccionario.covid, by = c("SECTOR" = "CLAVE")) %>%
      dplyr::select(-.data$SECTOR) %>%
      dplyr::rename_with(.cols = dplyr::matches("DESCRIPCI\u00d3N"),
                         function(x) "SECTOR")

    # SEXO
    if (!quiet) {
      message("+ SEXO")
    }

    diccionario.covid <- readxl::read_excel(fname,
      sheet = "Cat\u00e1logo SEXO",
      col_types = c("numeric", "text")
    )
    dats <- dats %>%
      dplyr::left_join(diccionario.covid, by = c("SEXO" = "CLAVE")) %>%
      dplyr::select(-.data$SEXO) %>%
      dplyr::rename_with(.cols = dplyr::matches("DESCRIPCI\u00d3N"),
                         function(x) "SEXO")

    # TIPO DE PACIENTE
    if (!quiet) {
      message("+ TIPO_PACIENTE")
    }

    diccionario.covid <- readxl::read_excel(fname,
      sheet = "Cat\u00e1logo TIPO_PACIENTE",
      col_types = c("numeric", "text")
    )
    dats <- dats %>%
      dplyr::left_join(diccionario.covid, by = c("TIPO_PACIENTE" = "CLAVE")) %>%
      dplyr::select(-.data$TIPO_PACIENTE) %>%
      dplyr::rename_with(.cols = dplyr::matches("DESCRIPCI\u00d3N"),
                         function(x) "TIPO_PACIENTE")


    # CATaLOGO SI/NO:
    # INTUBADO, NEUMONIA, EMBARAZO, HABLA LENGUA INDIGENA, INDIGENA, DIABETES
    # EPOC, ASMA, INMUSUPR, HIPERTENSION, OTRA_COMORBILIDAD, OBESIDAD,
    # RENAL_CRONICA, TABAQUISMO, MIGRANTE, UCI

    diccionario.covid <- readxl::read_excel(fname,
      sheet = "Cat\u00e1logo SI_NO",
      col_types = c("numeric", "text")
    )


    for (col in c(
      "INTUBADO", "NEUMONIA", "EMBARAZO", "HABLA_LENGUA_INDIG",
      "INDIGENA", "DIABETES", "EPOC", "ASMA", "INMUSUPR", "HIPERTENSION",
      "OTRA_COM", "OBESIDAD", "RENAL_CRONICA", "TABAQUISMO",
      "MIGRANTE", "UCI", "CARDIOVASCULAR","OTRO_CASO","TOMA_MUESTRA_LAB",
      "TOMA_MUESTRA_ANTIGENO"
    )) {
      if (!quiet) {
        message(paste0("+ ", col))
      }

      join_cols <- c("CLAVE")
      names(join_cols) <- col

      dats <- dats %>%
        dplyr::left_join(diccionario.covid, by = join_cols) %>%
        dplyr::select(-dplyr::matches(col)) %>%
        dplyr::rename_with(.cols = dplyr::matches("DESCRIPCI\u00d3N"), function(x) {
          col
        })
    }

    # CATaLOGO DE MUNICIPIO
    diccionario.covid <- readxl::read_excel(fname,
      sheet = "Cat\u00e1logo MUNICIPIOS",
      col_types = c("text", "text", "text")
    )

    if (!quiet) {
      message("+ MUNICIPIO_RES")
    }

    dats <- dats %>%
      dplyr::left_join(diccionario.covid,
        by = c(
          "MUNICIPIO_RES" = "CLAVE_MUNICIPIO",
          "ENTIDAD_RES" = "CLAVE_ENTIDAD"
        )
      ) %>%
      dplyr::select(-.data$MUNICIPIO_RES) %>%
      dplyr::rename_with(.cols = dplyr::matches("MUNICIPIO"),
                         function(x) "MUNICIPIO_RES")

    # CATaLOGO ENTIDAD
    # ENTIDAD_UM, ENTIDAD_RES, ENTIDAD_NAC
    diccionario.covid <- readxl::read_excel(fname,
      sheet = "Cat\u00e1logo de ENTIDADES",
      col_types = c("text", "text", "text")
    )
    ent_col <- colnames(dats)[stringr::str_detect(colnames(dats), "ENTIDAD")]

    for (col in ent_col) {
      if (!quiet) {
        message(paste0("+ ", col))
      }

      join_cols <- c("CLAVE_ENTIDAD")
      names(join_cols) <- col

      dats <- dats %>%
        dplyr::left_join(diccionario.covid, by = join_cols) %>%
        dplyr::select(-dplyr::matches(col), -dplyr::matches("ABREVIATURA")) %>%
        dplyr::rename_with(
          .cols = dplyr::matches("ENTIDAD_FEDERATIVA"),
          function(x) {
            col
          }
        )
    }

    # NACIONALIDAD
    if (!quiet) {
      message("+ NACIONALIDAD")
    }

    diccionario.covid <- readxl::read_excel(fname,
      sheet = "Cat\u00e1logo NACIONALIDAD",
      col_types = c("numeric", "text")
    )
    dats <- dats %>%
      dplyr::left_join(diccionario.covid, by = c("NACIONALIDAD" = "CLAVE")) %>%
      dplyr::select(-.data$NACIONALIDAD) %>%
      dplyr::rename_with(.cols = dplyr::matches("DESCRIPCI\u00d3N"),
                         function(x) "NACIONALIDAD")

    # RESULTADO_LAB
    if (!quiet) {
      message("+ RESULTADO_LAB")
    }

    diccionario.covid <- readxl::read_excel(fname,
      sheet = "Cat\u00e1logo RESULTADO_LAB",
      col_types = c("numeric", "text")
    )
    dats <- dats %>%
      dplyr::left_join(diccionario.covid, by = c("RESULTADO_LAB" = "CLAVE")) %>%
      dplyr::select(-.data$RESULTADO_LAB) %>%
      dplyr::rename_with(.cols = dplyr::matches("DESCRIPCI\u00d3N"),
                         function(x) "RESULTADO_LAB")

    # RESULTADO_ANTIGENO
    if (!quiet) {
      message("+ RESULTADO_ANTIGENO")
    }

    diccionario.covid <- readxl::read_excel(fname,
      sheet = "Cat\u00e1logo RESULTADO_ANTIGENO",
      col_types = c("numeric", "text")
    )
    dats <- dats %>%
      dplyr::left_join(diccionario.covid,
                       by = c("RESULTADO_ANTIGENO" = "CLAVE")) %>%
      dplyr::select(-.data$RESULTADO_ANTIGENO) %>%
      dplyr::rename_with(.cols = dplyr::matches("DESCRIPCI\u00d3N"),
                         function(x) "RESULTADO_ANTIGENO")

    # CLASIFICACI\u00d3N_FINAL
    if (!quiet) {
      message("+ CLASIFICACION_FINAL")
    }

    diccionario.covid <- readxl::read_excel(fname,
      sheet = "Cat\u00e1logo CLASIFICACION_FINAL",
      col_types = c("numeric", "text", "text")
    )
    dats <- dats %>%
      dplyr::left_join(diccionario.covid,
                       by = c("CLASIFICACION_FINAL" = "CLAVE")) %>%
      dplyr::select(-.data$CLASIFICACION_FINAL) %>%
      dplyr::rename_with(.cols = dplyr::matches("\bCLASIFICACI\u00d3N\b"),
                         function(x) "CLASIFICACION_FINAL")

    if (remove_zip_after_download) {
      unlink(file_download_dictionary)
      unlink(fname)
    }
  }

  if (language == "Espa\u00f1ol"){
    message("Por favor no olvides citar este paquete. Ve citation('covidmx').")
  } else {
    message("Please don't forget to cite the package. See citation('covidmx').")
  }
  return(dats)
}
