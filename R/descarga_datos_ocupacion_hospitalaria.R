#' LEE LA BASE DE DATOS DE OCUPACIoN HOSPITALARIA
#'
#' @description
#' `descarga_datos_ocupacion_hospitalaria` Lee los datos de ocupacion hospitalaria de la `RED IRAG`
#' disponibles en [https://www.gits.igg.unam.mx/red-irag-dashboard/reviewHome#](https://www.gits.igg.unam.mx/red-irag-dashboard/reviewHome#)
#'
#'
#' @details
#'
#' _This is not an official product / este no es un producto oficial_
#'
#' **Spanish**
#' Los datos son descargados de manera automatica en mi Github:
#' [RodrigoZepeda/CapacidadHospitalariaMX](https://github.com/RodrigoZepeda/CapacidadHospitalariaMX)
#' y esta funcion los lee de ahí
#'
#' @param nivel "Estatal" o "Unidad Médica"

#' @return `data.frame` con los datos de ocupacion hospitalaria
#'
#'@examples
#'\dontrun{
#'ocupacion_hospitalaria <- descarga_datos_ocupacion_hospitalaria()
#'}
#' @encoding UTF-8
#' @export
descarga_datos_ocupacion_hospitalaria <- function(nivel = c("Estatal","Unidad M\u00e9dica")){

  github <- "https://media.githubusercontent.com/media/"
  cuenta <- "RodrigoZepeda/CapacidadHospitalariaMX/"

  if (nivel[1] == "Estatal"){
    fname <- glue::glue("{github}{cuenta}master/processed/HospitalizacionesMX_estatal.csv")
  } else if (nivel[1] == "Unidad M\u00e9dica"){
    fname <- glue::glue("{github}{cuenta}master/processed/HospitalizacionesMX_unidad_medica.csv")
  }

  message(glue::glue("Estoy descargando datos de:
                     {fname}"))


  dats <- readr::read_csv(fname,
    locale = readr::locale(encoding = "UTF-8"),
    col_types = readr::cols(
          Estado                       = readr::col_character(),
          Fecha                        = readr::col_date(format = "%Y-%m-%d"),
          Actualizacion                = readr::col_datetime(format = '%Y-%m-%dT%H:%M:%SZ')
      )
    ) %>%
    janitor::clean_names()


  return(dats)
}
