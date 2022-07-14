#' LEE LA BASE DE DATOS DE OCUPACION HOSPITALARIA DE RED IRAG
#'
#' @description
#' `descarga_datos_red_irag` Lee los datos de ocupacion hospitalaria de la `RED IRAG`
#' disponibles en [https://www.gits.igg.unam.mx/red-irag-dashboard/reviewHome#](https://www.gits.igg.unam.mx/red-irag-dashboard/reviewHome#)
#'
#'
#' @details
#'
#' Los datos de Red IRAG son descargados diariamente de manera automatica en Github:
#' [RodrigoZepeda/CapacidadHospitalariaMX](https://github.com/RodrigoZepeda/CapacidadHospitalariaMX)
#' y esta funcion los lee de ahi
#'
#' @param nivel "Estatal" o "Unidad Medica"
#' @param quiet booleana para no imprimir mensajes en la consola.
#' @param cache cache para `pins::board_url`.
#' @param use_cache_on_failure parametro para `pins::board_url`.
#' @param ...  parametros adicionales para `pins::pin_download`.
#' @return `data.frame` con los datos de ocupacion hospitalaria
#'
#'@examples
#'\dontrun{
#'#Descarga de datos estatales
#'ocupacion_hospitalaria <- descarga_datos_red_irag("Estatal")
#'
#'#Descarga de datos por unidad medica
#'ocupacion_hospitalaria <- descarga_datos_red_irag("Unidad Medica")
#'}
#' @encoding UTF-8
#' @seealso descarga_datos_variantes_GISAID descarga_datos_abiertos
#' @export
descarga_datos_red_irag <- function(nivel = c("Estatal","Unidad M\u00e9dica"),
                                    cache = NULL,
                                    use_cache_on_failure = rlang::is_interactive(),
                                    quiet = TRUE,
                                    ...){

  github <- "https://media.githubusercontent.com/media/"
  cuenta <- "RodrigoZepeda/CapacidadHospitalariaMX/master/processed/"

  nivel  <- ifelse(tolower(nivel[1]) == "estatal", "estatal", "unidad_medica")
  fname  <- glue::glue("{github}{cuenta}HospitalizacionesMX_{nivel}.csv")

  if (!quiet){
    message(glue::glue("Descargando/downloading: {fname}"))
  }

  #Creamos el board
  board <- pins::board_url(
    urls = c("estatal"       = glue::glue("{github}{cuenta}HospitalizacionesMX_estatal.csv"),
             "unidad_medica" = glue::glue("{github}{cuenta}HospitalizacionesMX_unidad_medica.csv")
    ),
    cache                = cache,
    use_cache_on_failure = use_cache_on_failure)

  dats <- board %>%
    pins::pin_download(nivel[1], ...) %>%
    readr::read_csv(
      locale    = readr::locale(encoding = "UTF-8"),
      col_types = readr::cols(
          Estado        = readr::col_character(),
          Fecha         = readr::col_date(format = "%Y-%m-%d"),
          Actualizacion = readr::col_datetime(format = '%Y-%m-%dT%H:%M:%SZ')
      )
    ) %>%
    janitor::clean_names()

  if (!quiet){
    message(glue::glue("
            No olvides citar la fuente de los datos | Don't forget to cite the data sources
            --------------------------------------------------------------------------------------

            Secretaria de Salud. ({lubridate::year(lubridate::today())}).
            Sistema de Informacion de la Red IRAG.
            URL: https://www.gits.igg.unam.mx/red-irag-dashboard/reviewHome#

            Zepeda-Tello, R. ({lubridate::year(lubridate::today())}).
            Datos de Ocupacion Hospitalaria.
            Actualizacion del {dats$Actualizacion[1]}.
            URL: https://github.com/RodrigoZepeda/CapacidadHospitalaria
            DOI: 10.17605/OSF.IO/9NU2D
            "))
  }


  return(dats)
}
