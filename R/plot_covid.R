#' PLOT DATOS DE COVID
#'
#' @description
#' `plot_covid` Grafica automaticamente la base de datos de covid
#'
#' @details
#' Por default calcula el total de casos.
#' This is not an official product / este no es un producto oficial
#'
#' @param datos_covid If no data is available it automatically downloads COVID-19
#' information y calcula los casos por entidad
#'
#' @param df_name Nombre de la base de datos dentro de la lista datos_covid
#'
#' @param df_date_index Nombre de la variable que contiene la fecha
#'
#' @param df_variable Nombre de la variable que se va a graficar en el eje y
#'
#' @param df_covariates Covariables para el facet (maximo 2)
#'
#' @param facet_scale Escala para el facet_wrap
#'
#' @param facet_ncol Numero de columnas para el facet_wrap
#'
#' @param date_break_format Breaks para el eje x
#'
#' @param date_labels_format Formato de fecha para el eje x
#'
#' @param type Tipo de grafica (`line`, `area`, `spline` o `point`)
#'
#' @param plot_theme Tema para el `ggplot2`
#'
#' @param ... Parametros adicionales para `ggformula::geom_spline` en caso de elegir
#'  `type="spline"`
#' @examples
#'\dontrun{
#' #Grafica de casos por entidad
#' datos_covid %>% casos() %>% plot_covid()
#'
#' #Grafica de casos nacional
#' datos_covid %>% casos(group_by_entidad = F) %>% plot_covid()
#'
#' #Grafica de casos nacional
#' datos_covid %>% casos(group_by_entidad = F) %>% plot_covid()
#'
#' #Ajuste mediante splines
#' datos_covid %>% casos(group_by_entidad = F) %>% plot_covid(type = "spline", spar = 0.5)
#'
#' #Ajuste mediante splines
#' datos_covid %>% casos(group_by_entidad = T, group_by_tipo_clasificacion = T, df_variable = "n") %>%
#'        plot_covid()
#'}
#' @importFrom ggformula geom_spline
#' @export

plot_covid <- function(datos_covid = NULL,
                       df_name = "casos",
                       df_date_index = stringr::str_subset(colnames(datos_covid[df_name][[1]]),
                                                           "FECHA"),
                       df_variable   = NULL,
                       df_covariates = colnames(datos_covid[df_name][[1]])[
                         !(colnames(datos_covid[df_name][[1]]) %in% c(df_date_index,
                                                                      df_variable, "ENTIDAD_UM",
                                                                      "ABREVIATURA"))],
                       facet_scale = "free_y",
                       facet_ncol  = 4,
                       date_break_format  = "2 months",
                       date_labels_format = "%B-%y",
                       type = c("point","line","spline","area"),
                       plot_theme = ggplot2::theme(
                         panel.background = ggplot2::element_rect(fill = "white"),
                         plot.background = ggplot2::element_rect(fill = "white"),
                         axis.text.x = ggplot2::element_text(angle = 90, hjust = 1),
                         axis.line.x = ggplot2::element_line(color = "black"),
                         legend.position = "none"
                       ), ...){

  utils::globalVariables(".data")

  #Checar la descarga
  if (is.null(datos_covid)){
    datos_covid <- descarga_datos_abiertos() %>% casos()
  }


  #Checamos la variable 1
  if (is.null(df_variable)){
    df_variable <- colnames(datos_covid[df_name][[1]] %>% dplyr::select_if(is.numeric))[1]
    message(glue::glue("df_variable no fue especificada. Usaremos la columna `{df_variable}`"))
  }

  #Checamos la variable 1
  if (length(df_date_index) > 1){
    stop("Hay dos indices de fecha. Especifica df_date_index para seleccionar el adecuado")
  }

  if (length(df_covariates) > 2){
    warning("No se recomiendan mas de dos covariables para los plots automaticos")
  }

  #Hack para cuando es el nacional y no tiene covariables
  if (length(df_covariates) == 0){
    datos_covid[df_name][[1]][,"covar"] <- ""
    df_covariates <- "covar"
  }

  plot <- datos_covid[df_name][[1]] %>%
    dplyr::mutate_at(df_variable, as.numeric) %>%
    ggplot2::ggplot() +
    ggplot2::facet_wrap(as.formula(paste("~", paste(df_covariates, collapse = " + "))),
                        scales = facet_scale, ncol = facet_ncol) +
    ggplot2::scale_x_date(date_breaks = date_break_format, date_labels = date_labels_format) +
    ggplot2::labs(
      x       = stringr::str_replace_all(df_date_index,"_", " "),
      y       = stringr::str_replace_all(df_variable,"_", " "),
      title   = toupper(df_name),
      caption = "Elaborado mediante covidmx | Github: RodrigoZepeda/covidmx"
    )


  #Specific format for some variables
  if (df_variable == "n"){
    plot <- plot +
      ggplot2::scale_y_continuous(labels = scales::comma)
  }

  type <- tolower(type)
  if (type[1] == "point"){
    plot <- plot +
      ggplot2::geom_point(ggplot2::aes(x = .data[[df_date_index]], y = .data[[df_variable]],
                                       color = .data[[df_covariates[1]]]))
  } else if (type[1] == "line"){
    plot <- plot +
      ggplot2::geom_line(ggplot2::aes(x = .data[[df_date_index]], y = .data[[df_variable]],
                                      color = .data[[df_covariates[1]]]))
  } else if (type[1] == "spline"){
    plot <- plot +
      ggformula::geom_spline(ggplot2::aes(x = .data[[df_date_index]], y = .data[[df_variable]],
                                          color = .data[[df_covariates[1]]]), ...)
  } else if (type[1] == "area"){
    plot <- plot +
      ggplot2::geom_area(ggplot2::aes(x = .data[[df_date_index]], y = .data[[df_variable]],
                                          fill = .data[[df_covariates[1]]]), ...)
  }

  plot <- plot + plot_theme

  return(plot)

}
