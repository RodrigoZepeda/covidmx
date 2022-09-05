#' LEE LA BASE DE DATOS DE VARIANTES DE COVID-19 DE GISAID
#'
#' @description
#' `descarga_datos_variantes_GISAID` Lee los datos de variantes del reporte nacional diario en
#'  https://github.com/RodrigoZepeda/VariantesCovid  creado a partir de la informacion de la
#'  Global Initiative on Sharing Avian Influenza Data (GISAID)
#'
#' @details
#' Cada vez que uses estos datos necesitas citar a **GISAID** (ver referencias) asi como
#' el reporte en https://github.com/RodrigoZepeda/VariantesCovid.
#'
#' Los datos son descargados de manera automatica en mi Github:
#' [RodrigoZepeda/VariantesCovid](https://github.com/RodrigoZepeda/VariantesCovid) el programa
#' `descarga_datos_variantes_GISAID` se conecta a dicho repositorio, busca si la informacion esta
#' actualizada y si si la descarga, si no, utiliza informacion almacenada en el `cache` local.
#'
#' La descarga usa el paquete `pins`
#'
#' @param nivel si se desea descargar `"nacional"` o `"cdmx"`.
#' @param quiet booleana para no imprimir mensajes en la consola.
#' @param cache cache para `pins::board_url`.
#' @param use_cache_on_failure parametro para `pins::board_url`.
#' @param force_download analiza si cambio el pin y descarga datos nuevos en caso afirmativo
#' @param show_warnings si arrojar `warnings`
#' @param ...  parametros adicionales para `pins::pin_download`.
#' @return `data.frame` con los datos porcentuales y de conteo de variantes
#'
#' @examples
#' \dontrun{
#' # Descarga de variantes a nivel nacional
#' variantes_covid <- descarga_datos_variantes_GISAID("nacional")
#'
#' # Descarga de variantes para CDMX
#' variantes_covid <- descarga_datos_variantes_GISAID("cdmx")
#'
#' # Si ya descargaste hace menos de un día el programa solito se da cuenta y lee de memoria
#' # sin verificar que el contenido en Internet haya cambiado
#' variantes_covid <- descarga_datos_variantes_GISAID("nacional")
#'
#' # Puedes forzarlo a checar el contenido en Internet usando
#' variantes_covid <- descarga_datos_variantes_GISAID("nacional", force_download = TRUE)
#' }
#' @encoding UTF-8
#' @references
#'
#' Khare, S., et al (2021) GISAID's Role in Pandemic Response. China CDC Weekly, 3(49):
#' 1049-1051. doi:10.46234/ccdcw2021.255 PMCID: 8668406
#'
#' Elbe, S. and Buckland-Merrett, G. (2017) Data, disease and diplomacy: GISAID’s
#' innovative contribution to global health. Global Challenges, 1:33-46.
#' doi:10.1002/gch2.1018 PMCID: 31565258
#'
#' Shu, Y. and McCauley, J. (2017) GISAID: from vision to reality. EuroSurveillance,
#' 22(13) doi:10.2807/1560-7917.ES.2017.22.13.30494 PMCID: PMC5388101
#'
#' Zepeda-Tello, R. (2022). Reporte Nacional de Variantes de COVID-19.
#' URL: \url{https://github.com/RodrigoZepeda/VariantesCovid}
#'
#' @seealso [descarga_datos_red_irag] [descarga_datos_abiertos] [read_datos_abiertos]
#' @export
descarga_datos_variantes_GISAID <- function(nivel = c("nacional", "cdmx"),
                                            cache = NULL,
                                            use_cache_on_failure = TRUE,
                                            quiet = FALSE,
                                            force_download = FALSE,
                                            show_warnings = TRUE,
                                            ...) {

  # Ponemos el diccionario
  github <- "https://raw.githubusercontent.com/"
  cuenta <- "RodrigoZepeda/VariantesCovid/main/tablas/"
  fname <- paste0(github, cuenta, "Proporcion_variantes_", nivel[1], ".csv")

  if (!quiet) {
    cli::cli_alert("Descargando/downloading: {fname}")
  }

  # Creamos el board
  board <- pins::board_url(
    urls = c(
      "nacional" = paste0(github, cuenta, "Proporcion_variantes_nacional.csv"),
      "cdmx"     = paste0(github, cuenta, "Proporcion_variantes_cdmx.csv")
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
      cli::cli_warn(
        paste(
          "La descarga mas reciente fue",
          "hace {round(tdif,5)} dias. Como tiene menos de un dia usare esa.",
          "Escribe {.code force_download = TRUE} si quieres descargar de",
          "todas formas. Para desactivar este mensaje {.code show_warnings = FALSE.}"
        )
      )
    }

    # Lee de memoria
    dfile <- pin_path_from_memory(board, nivel[1])
  } else {
    # Descarga si cambió
    dfile <- pins::pin_download(board = board, name = nivel[1], ...)
  }

  dats <- dfile |>
    readr::read_csv(
      locale = readr::locale(encoding = "UTF-8"),
      col_types = readr::cols(
        .default      = readr::col_character(),
        ano           = readr::col_integer(),
        semana        = readr::col_integer(),
        n             = readr::col_integer(),
        freq          = readr::col_double(),
        Actualizacion = readr::col_datetime(format = "%Y/%m/%d %H:%M:%S")
      )
    )

  # Escribimos en el pin que ya descargamos
  pin_write_download_time(board, nivel[1])

  return(dats)
}
