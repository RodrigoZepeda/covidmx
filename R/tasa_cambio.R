#' TASA DE CAMBIO
#'
#' @description
#' `tasa_cambio` Calcula la diferencia de casos del dia t y el t-1
#'
#' @details
#' This is not an official product / este no es un producto oficial
#'
#' @param covid_dataset Datos provenientes de `positividad`, `casos`,
#' `numero_pruebas`, etc.
#' @param variable Variable sobre la cual calcular la tasa de cambio
#' @param quiet Show warning?
#'
#'@examples
#'\dontrun{
#'datos_covid <- descarga_datos_abiertos(language = "Espanol")
#'
#'#Positividad nacional
#'datos_covid %>% casos() %>% tasa_cambio() %>% covid_plot()
#'
#'}
#'
#' @export

tasa_cambio <- function(covid_dataset, variable = NULL, quiet = FALSE){


  fecha_col <- colnames(covid_dataset)[
    stringr::str_detect(colnames(covid_dataset), "FECHA")]

  if (is.null(variable)){
    if (!quiet){
      warning(paste0("Para la tasa de cambio se sugiere",
                     "suavizar primero con smooth: \n",
                     "covid_dataset %>% smooth() %>% tasa_cambio('SMOOTH')"))
    }
    variable <- stringr::str_detect(colnames(covid_dataset),
                                    "ENTIDAD|FECHA|\\bSMOOTH\\b",
                                    negate = T)
    variable <- colnames(covid_dataset)[variable]
  }

  if ((ncol(covid_dataset) == 3 & !("SMOOTH" %in% colnames(covid_dataset))) |
       ncol(covid_dataset) == 4){

    entidad_col <- colnames(covid_dataset)[
      stringr::str_detect(colnames(covid_dataset), "ENTIDAD")]

    covid_dataset <- covid_dataset %>%
      dplyr::group_by_at(entidad_col) %>%
      dplyr::arrange_at(fecha_col) %>%
      dplyr::mutate(TASA_CAMBIO =  get(variable) - dplyr::lag(get(variable),
                                                              default = 0)) %>%
      dplyr::select_at(c(entidad_col,fecha_col,"TASA_CAMBIO")) %>%
      dplyr::rename_with(.cols = dplyr::matches("TASA_CAMBIO"),
                         function(x) paste0(x,"_", variable))
  } else if (ncol(covid_dataset) == 2){
    covid_dataset <- covid_dataset %>%
      dplyr::arrange_at(fecha_col) %>%
      dplyr::mutate(TASA_CAMBIO =  get(variable) - dplyr::lag(get(variable),
                                                              default = 0))
      dplyr::select_at(c(fecha_col,"TASA_CAMBIO")) %>%
      dplyr::rename_with(.cols = dplyr::matches("TASA_CAMBIO"),
                           function(x) paste0(x,"_", variable))
  } else if (ncol(covid_dataset) == 3 & ("SMOOTH" %in% colnames(covid_dataset))) {
    covid_dataset <- covid_dataset %>%
      dplyr::arrange_at(fecha_col) %>%
      dplyr::mutate(TASA_CAMBIO =  get(variable) - dplyr::lag(get(variable),
                                                              default = 0))
      dplyr::select_at(c(fecha_col,"TASA_CAMBIO")) %>%
      dplyr::rename_with(.cols = dplyr::matches("TASA_CAMBIO"),
                         function(x) paste0(x,"_", variable))
  }

  return(covid_dataset)

}
