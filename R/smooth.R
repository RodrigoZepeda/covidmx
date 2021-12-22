#' SUAVIZAMIENTO DE LOS CASOS
#'
#' @description
#' `smooth` Suaviza los casos reportados
#'
#' @details
#' Por ahora solo esta la opcion de splines, lowess, kernel y media movil (rollmean).
#'
#' This is not an official product / este no es un producto oficial
#'
#' @param covid_dataset Datos provenientes de `positividad`, `casos`,
#' `numero_pruebas`, etc.
#' @param variable Variable a suavizar.
#' @param method `splines`, `lowess`, `kernel`, `rollmean`. Metodo de suavizamiento
#' @param df Degrees of freedom for spline smoothing
#' @param bw Banwidth for kernel
#' @param f Parameter for lowess
#' @param k Days to consider in rolling mean
#'
#'@examples
#'\dontrun{
#'datos_covid <- descarga_datos_abiertos(language = "Espanol")
#'
#'
#'#Positividad en Jalisco y Colima
#'datos_covid %>% casos(entidades = c("JALISCO","COLIMA")) %>%
#'          smooth() %>%
#'          covid_plot()
#'datos_covid %>% casos(entidades = c("JALISCO","COLIMA")) %>%
#'          smooth(method = "kernel") %>%
#'          covid_plot()
#'datos_covid %>% casos(entidades = c("JALISCO","COLIMA")) %>%
#'          smooth(method = "lowess", f = 0.1) %>%
#'          covid_plot()
#'datos_covid %>% casos(entidades = c("JALISCO","COLIMA")) %>%
#'          smooth(method = "rollmean", k = 14) %>%
#'          covid_plot()
#'}
#'
#' @export

smooth <- function(covid_dataset,
                   variable = NULL,
                   method = c("splines","kernel","lowess","rollmean"),
                   df = 10, bw = 100, f = 0.1, k = 7){

  covid_dataset <- covid_dataset %>%
    tidyr::drop_na()

  fecha_col <- colnames(covid_dataset)[
    stringr::str_detect(colnames(covid_dataset), "FECHA")]

  if (is.null(variable)){
    variable <- stringr::str_detect(colnames(covid_dataset),
                                    "ENTIDAD|FECHA|\\bSMOOTH\\b",
                                    negate = T)
    variable <- colnames(covid_dataset)[variable]
  }


  if (ncol(covid_dataset) == 3 | ncol(covid_dataset) == 4){

    entidad_col <- colnames(covid_dataset)[
      stringr::str_detect(colnames(covid_dataset), "ENTIDAD")]

    covid_dataset <- covid_dataset %>%
      dplyr::group_by_at(entidad_col) %>%
      dplyr::arrange_at(fecha_col)

  } else if (ncol(covid_dataset) == 2){

    covid_dataset <- covid_dataset %>%
      dplyr::arrange_at(fecha_col)

  }

  if (method[1] == "splines"){
    covid_dataset <- covid_dataset %>%
      dplyr::mutate(t = as.numeric(get(fecha_col) - as.Date("2020/01/01", format = "%Y/%m/%d"))) %>%
      dplyr::mutate(SMOOTH = predict(
          smooth.spline(.data$t,get(variable), df = df)
          )$y
      ) %>%
      dplyr::select(-.data$t)
  } else if (method[1] == "kernel"){
    covid_dataset <- covid_dataset %>%
      dplyr::mutate(t = as.numeric(get(fecha_col) - as.Date("2020/01/01", format = "%Y/%m/%d"))) %>%
      dplyr::mutate(SMOOTH =
        ksmooth(.data$t,get(variable), kernel = "normal", bandwidth = bw,
                x.points = .data$t)$y
      ) %>%
      dplyr::select(-.data$t)
  } else if (method[1] == "lowess"){
    covid_dataset <- covid_dataset %>%
      dplyr::mutate(t = as.numeric(get(fecha_col) - as.Date("2020/01/01", format = "%Y/%m/%d"))) %>%
      dplyr::mutate(SMOOTH =
                      lowess(x = .data$t, y = get(variable), f = f)$y
      ) %>%
      dplyr::select(-.data$t)
  } else if (method[1] == "rollmean"){
    covid_dataset <- covid_dataset %>%
      dplyr::mutate(t = as.numeric(get(fecha_col) - as.Date("2020/01/01", format = "%Y/%m/%d"))) %>%
      dplyr::mutate(SMOOTH =
           zoo::rollmean(get(variable), k = k, fill = 0, align = "right")
      ) %>%
      dplyr::select(-.data$t)
  } else {
    stop(paste0("Invalid smoothing method. Choose kernel. lowess, ",
                "rollmean, or splines"))
  }

  return(covid_dataset)

}
