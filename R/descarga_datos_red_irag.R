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
#' @param force_download analiza si cambio el pin y descarga datos nuevos en caso afirmativo
#' @param show_warnings si arrojar `warnings`
#' @param ...  parametros adicionales para `pins::pin_download`.
#' @return `data.frame` con los datos de ocupacion hospitalaria
#'
#' @examples
#' \dontrun{
#' # Descarga de datos estatales
#' ocupacion_hospitalaria <- descarga_datos_red_irag("Estatal")
#'
#' # Descarga de datos por unidad medica
#' ocupacion_hospitalaria <- descarga_datos_red_irag("Unidad Medica")
#'
#' # Si ya descargaste hace menos de un día el programa solito se da cuenta y lee de memoria
#' # sin verificar que el contenido en Internet haya cambiado
#' ocupacion_hospitalaria <- descarga_datos_red_irag("Unidad Medica")
#'
#' # Puedes forzarlo a checar el contenido en Internet usando
#' variantes_covid <- descarga_datos_red_irag("Unidad Medica", force_download = TRUE)
#' }
#' @encoding UTF-8
#' @seealso [descarga_datos_variantes_GISAID] [descarga_datos_abiertos] [read_datos_abiertos]
#' @export
descarga_datos_red_irag <- function(nivel = c("Estatal", "Unidad M\u00e9dica"),
                                    cache = NULL,
                                    use_cache_on_failure = TRUE,
                                    quiet = TRUE,
                                    force_download = FALSE,
                                    show_warnings = TRUE,
                                    ...) {
  github <- "https://media.githubusercontent.com/media/"
  cuenta <- "RodrigoZepeda/CapacidadHospitalariaMX/master/processed/"

  nivel <- ifelse(tolower(nivel[1]) == "estatal", "estatal", "unidad_medica")
  fname <- glue::glue("{github}{cuenta}HospitalizacionesMX_{nivel}.csv")

  if (!quiet) {
    message(glue::glue("Descargando/downloading: {fname}"))
  }

  # Creamos el board
  board <- pins::board_url(
    urls = c(
      "estatal" = glue::glue("{github}{cuenta}HospitalizacionesMX_estatal.csv"),
      "unidad_medica" = glue::glue("{github}{cuenta}HospitalizacionesMX_unidad_medica.csv")
    ),
    cache = cache,
    use_cache_on_failure = use_cache_on_failure
  )

  # FIXME
  # This is a workaround as the pins package doesn't have metadata for downloads
  # Checamos si está descargado y cuándo lo descargaste si fue hace menos de un dia te
  # dejo con el mismo
  tdif <- pin_get_download_time(board, nivel[1])

  if (!force_download & tdif < 0.9) {
    if (show_warnings) {
      warning(glue::glue("
                          La descarga mas reciente fue hace {tdif} dias. Como tiene menos de un dia
                          usare esa. Escribe force_download = TRUE si quieres descargar de
                          todas formas. Para desactivar este mensaje show_warnings = FALSE.

                          Most recent download was {tdif} days ago. It has less than a day hence
                          I'll use that one. Write force_download = TRUE if you want to
                          download anyway. To turn off this message show_warnings = FALSE."))
    }

    # Lee de memoria
    dfile <- pin_path_from_memory(board, nivel[1])
  } else {
    # Descarga si cambió
    dfile <- pins::pin_download(board = board, name = nivel[1], ...)
  }

  dats <- dfile %>%
    readr::read_csv(
      locale = readr::locale(encoding = "UTF-8"),
      col_types = readr::cols(
        Estado        = readr::col_character(),
        Fecha         = readr::col_date(format = "%Y-%m-%d"),
        Actualizacion = readr::col_datetime(format = "%Y-%m-%dT%H:%M:%SZ")
      )
    ) %>%
    janitor::clean_names()

  # Escribimos en el pin que ya descargamos
  pin_write_download_time(board, nivel[1])

  return(dats)
}
