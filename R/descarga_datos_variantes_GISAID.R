#' LEE LA BASE DE DATOS DE VARIANTES DE GISAID
#'
#' @description
#' `descarga_datos_variantes_GISAID` Lee los datos de variantes publicadas por `GISAID`
#'
#'
#' @details
#' Cada vez que uses estos datos necesitas citar a **GISAID** así como esta publicación producto
#' del análisis de dichos datos.
#'
#' _This is not an official product / este no es un producto oficial_
#'
#' **Spanish**
#' Los datos son descargados de manera automatica en mi Github:
#' [RodrigoZepeda/VariantesCovid](https://github.com/RodrigoZepeda/VariantesCovid)
#' y esta funcion los lee de ahí
#' @param type si se desea descargar "nacional" o sólo "cdmx"
#' @return `data.frame` con los datos porcentuales y de conteo de variantes
#'
#'@examples
#'\dontrun{
#'variantes_covid <- descarga_datos_variantes_GISAID()
#'}
#' @encoding UTF-8
#' @references
#'
#' Khare, S., et al (2021) GISAID’s Role in Pandemic Response. China CDC Weekly, 3(49): 1049-1051. doi:10.46234/ccdcw2021.255 PMCID: 8668406
#'
#' Elbe, S. and Buckland-Merrett, G. (2017) Data, disease and diplomacy: GISAID’s innovative contribution to global health. Global Challenges, 1:33-46. doi:10.1002/gch2.1018 PMCID: 31565258
#'
#' Shu, Y. and McCauley, J. (2017) GISAID: from vision to reality. EuroSurveillance, 22(13) doi:10.2807/1560-7917.ES.2017.22.13.30494 PMCID: PMC5388101
#'
#' @export
descarga_datos_variantes_GISAID <- function(type = c("nacional","cdmx")){

  github <- "https://raw.githubusercontent.com/"
  cuenta <- "RodrigoZepeda/VariantesCovid/"
  fname <- glue::glue("{github}{cuenta}main/tablas/Proporcion_variantes_{type[1]}.csv")

  message(glue::glue("Estoy descargando datos de:
                     {fname}"))

  dats <- readr::read_csv(fname,
                          locale = readr::locale(encoding = "UTF-8"),
                          col_types = readr::cols(
                            .default                     = readr::col_character(),
                            ano                          = readr::col_integer(),
                            semana                       = readr::col_integer(),
                            n                            = readr::col_integer(),
                            freq                         = readr::col_double(),
                            Actualizacion                = readr::col_datetime(format = '%Y/%m/%d %H:%M:%S')
                          )
  )

  return(dats)
}
