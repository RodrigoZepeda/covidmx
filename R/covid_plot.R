#' COVID PLOT: Graficacion de las tablas calculadas por el paquete
#'
#' @description
#' `covid_plot` Grafica las tablas calculadas por el paquete
#'
#' @details
#' This is not an official product / este no es un producto oficial
#'
#' @param covid_dataset Datos provenientes de `positividad`, `casos`,
#' `numero_pruebas`, etc
#'
#' @param scales Scales parameter for \code{\link[ggplot2]{facet_wrap}}
#'
#'@examples
#'\dontrun{
#'datos_covid <- descarga_datos_abiertos(language = "Espanol")
#'
#'#Positividad nacional
#'datos_covid %>% positividad() %>% covid_plot()
#'
#'#Positividad en Jalisco y Colima
#'datos_covid %>% numero_pruebas(entidades = c("JALISCO","COLIMA")) %>%
#'          covid_plot()
#'datos_covid %>% numero_pruebas(entidades = c("JALISCO","COLIMA"),
#'                             group_by_entidad = T) %>%
#'          covid_plot()
#'}
#'

covid_plot <- function(covid_dataset, scales = "free"){

  if ((ncol(covid_dataset) == 3 & !("SMOOTH" %in% colnames(covid_dataset))) |
      ncol(covid_dataset) == 4){
    covid_dataset <- covid_dataset %>%
      dplyr::rename_with(.cols = dplyr::matches("ENTIDAD"),
                         function(x) "ENTIDAD") %>%
      dplyr::rename_with(.cols = dplyr::matches("FECHA"),
                         function(x) "FECHA")
  } else if (ncol(covid_dataset) == 2 | ncol(covid_dataset) == 3){
    covid_dataset <- covid_dataset %>%
      dplyr::rename_with(.cols = dplyr::matches("FECHA"),
                         function(x) "FECHA")
  }

  variable <- stringr::str_detect(colnames(covid_dataset),
                                  "ENTIDAD|FECHA|\\bSMOOTH\\b",
                                  negate = T)
  variable   <- colnames(covid_dataset)[variable]
  title_var  <- stringr::str_to_title(
                      stringr::str_replace_all(variable,"_"," ")
                )

  plot_covid <- ggplot2::ggplot(covid_dataset,
                  ggplot2::aes_string(x = "FECHA", y = variable)) +
    ggplot2::theme_classic() +
    ggplot2::labs(
      x = "Fecha",
      y = stringr::str_to_title(title_var),
      subtitle = "Fuente: Datos abiertos DGE",
      caption = "Elaborado con covidmx Github: RodrigoZepeda/covidmx"
    )

  if (ncol(covid_dataset) == 3 & !("SMOOTH" %in% colnames(covid_dataset))){
    plot_covid <- plot_covid +
      ggplot2::geom_point(color = "firebrick", alpha = 0.25) +
      ggplot2::geom_line(color = "firebrick", alpha = 0.25) +
      ggplot2::facet_wrap(as.formula("~ENTIDAD"), scales = scales) +
      ggplot2::ggtitle(paste0(title_var, " en M\u00e9xico"))
  } else if (ncol(covid_dataset) == 2){
    plot_covid <- plot_covid +
      ggplot2::geom_point(color = "firebrick", alpha = 0.25) +
      ggplot2::geom_line(color = "firebrick", alpha = 0.25) +
      ggplot2::ggtitle(title_var)
  } else if (ncol(covid_dataset) == 4){
    plot_covid <- plot_covid +
      ggplot2::geom_point(color = "firebrick", alpha = 0.25) +
      ggplot2::geom_line(ggplot2::aes_string(x = "FECHA", y = "SMOOTH"),
                         color = "deepskyblue4", size = 1) +
      ggplot2::facet_wrap(as.formula("~ENTIDAD"), scales = scales) +
      ggplot2::ggtitle(paste0(title_var, " en M\u00e9xico"))
  } else if (ncol(covid_dataset) == 3 & ("SMOOTH" %in% colnames(covid_dataset))) {
    plot_covid <- plot_covid +
      ggplot2::geom_point(color = "firebrick", alpha = 0.25) +
      ggplot2::geom_line(ggplot2::aes_string(x = "FECHA", y = "SMOOTH"),
                         color = "deepskyblue4", size = 1) +
      ggplot2::ggtitle(paste0(title_var, " en M\u00e9xico"))
  }

  if (stringr::str_detect(variable,"POSITIVIDAD")){
    plot_covid <- plot_covid +
      ggplot2::scale_y_continuous(labels = scales::percent)
  } else {
    plot_covid <- plot_covid +
      ggplot2::scale_y_continuous(labels = scales::comma)
  }

  return(plot_covid)

}
