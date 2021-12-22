#' ACUMULADOS
#'
#' @description
#' `acumulados` Calcula los casos acumulados
#'
#' @details
#' This is not an official product / este no es un producto oficial
#'
#' @param covid_dataset Datos provenientes de `positividad`, `casos`,
#' `numero_pruebas`, etc.
#' @param variable Variable a acumular
#'
#'@examples
#'\dontrun{
#'datos_covid <- descarga_datos_abiertos(language = "Espanol")
#'
#'Casos acumulados en COAHUILA
#'datos_covid %>% casos(entidades = c("COAHUILA")) %>% acumulados() %>%
#'covid_plot()
#'
#'datos_covid %>% casos() %>% acumulados() %>% covid_plot()
#'}
#'
#' @export

acumulados <- function(covid_dataset, variable = NULL){

  fecha_col <- colnames(covid_dataset)[
    stringr::str_detect(colnames(covid_dataset), "FECHA")]

  if (is.null(variable)){
    variable <- stringr::str_detect(colnames(covid_dataset),
                                    "ENTIDAD|FECHA|\\bSMOOTH\\b",
                                    negate = T)
    variable <- colnames(covid_dataset)[variable]
  }

  if (any(stringr::str_detect(variable,"POSITIVIDAD"))){
    warning("Acumular la positividad no tiene un resultado interpretable")
  }

  if (ncol(covid_dataset) == 3){

    entidad_col <- colnames(covid_dataset)[
      stringr::str_detect(colnames(covid_dataset), "ENTIDAD")]

    covid_dataset <- covid_dataset %>%
      dplyr::group_by_at(entidad_col) %>%
      dplyr::arrange_at(fecha_col) %>%
      dplyr::mutate(ACUMULADOS =  cumsum(get(variable))) %>%
      dplyr::select_at(c(entidad_col,fecha_col,"ACUMULADOS")) %>%
      dplyr::rename_with(.cols = dplyr::matches("ACUMULADOS"),
                         function(x) paste0(x,"_", variable))
  } else if (ncol(covid_dataset) == 2){
    covid_dataset <- covid_dataset %>%
      dplyr::arrange_at(fecha_col) %>%
      dplyr::mutate(ACUMULADOS =  cumsum(get(variable))) %>%
      dplyr::select_at(c(fecha_col,"ACUMULADOS")) %>%
      dplyr::rename_with(.cols = dplyr::matches("ACUMULADOS"),
                         function(x) paste0(x,"_", variable))
  }

  return(covid_dataset)

}
