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
#' @param ...  parametros adicionales para `pins::pin_download`.
#' @return `data.frame` con los datos porcentuales y de conteo de variantes
#'
#'@examples
#'\dontrun{
#'#Descarga de variantes a nivel nacional
#'variantes_covid <- descarga_datos_variantes_GISAID('nacional')
#'
#'#Descarga de variantes para CDMX
#'variantes_covid <- descarga_datos_variantes_GISAID('cdmx')
#'}
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
#' @seealso descarga_datos_red_irag descarga_datos_abiertos
#' @export
descarga_datos_variantes_GISAID <- function(nivel = c("nacional", "cdmx"),
                                            cache = NULL,
                                            use_cache_on_failure = rlang::is_interactive(),
                                            quiet = FALSE,
                                            ...){

  #Ponemos el diccionario
  github <- "https://raw.githubusercontent.com/"
  cuenta <- "RodrigoZepeda/VariantesCovid/main/tablas/"
  fname  <- glue::glue("{github}{cuenta}Proporcion_variantes_{nivel[1]}.csv")

  if (!quiet){
    message(glue::glue("Descargando/downloading: {fname}"))
  }

  #Creamos el board
  board <- pins::board_url(
    urls = c("nacional" = glue::glue("{github}{cuenta}Proporcion_variantes_nacional.csv"),
             "cdmx"     = glue::glue("{github}{cuenta}Proporcion_variantes_cdmx.csv")
             ),
    cache                = cache,
    use_cache_on_failure = use_cache_on_failure)


  dats <- board %>%
    pins::pin_download(name = nivel[1], ...) %>%
    readr::read_csv(locale    = readr::locale(encoding = "UTF-8"),
                    col_types = readr::cols(
                      .default      = readr::col_character(),
                      ano           = readr::col_integer(),
                      semana        = readr::col_integer(),
                      n             = readr::col_integer(),
                      freq          = readr::col_double(),
                      Actualizacion = readr::col_datetime(format = '%Y/%m/%d %H:%M:%S')
                    ))

  if (!quiet){
    message(glue::glue("
            No olvides citar las referencias de GISAID | Don't forget to cite GISAID's references
            --------------------------------------------------------------------------------------

            Khare, S., et al (2021) GISAID's Role in Pandemic Response. China CDC Weekly, 3(49):
            1049-1051. doi:10.46234/ccdcw2021.255 PMCID: 8668406

            Elbe, S. and Buckland-Merrett, G. (2017) Data, disease and diplomacy: GISAID's
            innovative contribution to global health. Global Challenges, 1:33-46.
            doi:10.1002/gch2.1018 PMCID: 31565258

            Shu, Y. and McCauley, J. (2017) GISAID: from vision to reality. EuroSurveillance,
            22(13) doi:10.2807/1560-7917.ES.2017.22.13.30494 PMCID: PMC5388101

            Zepeda-Tello, R. ({lubridate::year(lubridate::today())}).
            Reporte Nacional de Variantes de COVID-19.
            Actualizado el {dats$Actualizacion[1]}. URL:
            https://github.com/RodrigoZepeda/VariantesCovid

            "))
  }

  return(dats)
}
