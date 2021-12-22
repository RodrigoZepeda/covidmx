#' POSITIVIDAD POR PCR
#'
#' @description
#' `positividad_pcr` Calcula la positividad definida como el radio de
#' \deqn{\frac{Pruebas positivas}{Total de pruebas}} para las pruebas de PCR
#'
#' @details
#' Por default calcula la positividad total en pruebas de laboratorio (PCR)
#'
#' @seealso \code{positividad_antigeno} y \code{positividad}
#' This is not an official product / este no es un producto oficial
#'
#' @param datos_covid If no data is available it automatically downloads COVID-19
#' information.
#'
#' @param entidades Vector con las entidades de las unidades medicas a analizar.
#' Opciones: `AGUASCALIENTES`, `BAJA CALIFORNIA`, `BAJA CALIFORNIA SUR`,
#' `CAMPECHE`, `CHIAPAS`, `CHIHUAHUA`, `CIUDAD DE MEXICO`,
#' `COAHUILA DE ZARAGOZA` , `COLIMA`, `DURANGO`, `GUANAJUATO`, `GUERRERO`,
#' `HIDALGO`, `JALISCO`, `MEXICO`, `MICHOACAN DE OCAMPO`, `MORELOS`,`NAYARIT`
#' `NUEVO LEON`, `OAXACA` ,`PUEBLA`, `QUERETARO`,`QUINTANA ROO`,
#' `SAN LUIS POTOSI`, `SINALOA`, `SONORA`, `TABASCO`, `TAMAULIPAS`,`TLAXCALA`,
#' `VERACRUZ DE IGNACIO DE LA LLAVE`, `YUCATAN`, `ZACATECAS`
#' @param group_by_entidad Si junta las entidades en una sola
#' o bien las muestra por separado sin agrupar.
#' @param entidad_tipo Selecciona `Unidad Medica`, `Nacimiento` o `Residencia`.
#' por default incluye `Unidad Medica`
#' @param fecha_tipo Selecciona `Ingreso`, `Sintomas` o `Defuncion` por default
#' incluye fecha de `Sintomas`
#'@examples
#'\dontrun{
#'datos_covid <- descarga_datos_abiertos(language = "Espanol")
#'
#'#Positividad nacional por PCR
#'datos_covid %>% positividad_pcr()
#'
#'#Positividad en Jalisco y Colima
#'datos_covid %>% positividad_pcr(entidades = c("JALISCO","COLIMA")) %>%
#'        covid_plot()
#'datos_covid %>% positividad_pcr(entidades = c("JALISCO","COLIMA"),
#'                                 group_by_entidad = T) %>%
#'        covid_plot()
#'}
#'
#' @export

positividad_pcr <- function(datos_covid = NULL,
                            entidades = c("AGUASCALIENTES",
                                          "BAJA CALIFORNIA",
                                          "BAJA CALIFORNIA SUR",
                                          "CAMPECHE",
                                          "CHIAPAS",
                                          "CHIHUAHUA",
                                          "CIUDAD DE M\u00c9XICO",
                                          "COAHUILA DE ZARAGOZA" ,
                                          "COLIMA",
                                          "DURANGO",
                                          "GUANAJUATO",
                                          "GUERRERO",
                                          "HIDALGO",
                                          "JALISCO",
                                          "M\u00c9XICO",
                                          "MICHOAC\u00c1N DE OCAMPO",
                                          "MORELOS",
                                          "NAYARIT",
                                          "NUEVO LE\u00d3N",
                                          "OAXACA" ,
                                          "PUEBLA",
                                          "QUER\u00c9TARO",
                                          "QUINTANA ROO",
                                          "SAN LUIS POTOS\u00cd",
                                          "SINALOA",
                                          "SONORA",
                                          "TABASCO",
                                          "TAMAULIPAS",
                                          "TLAXCALA",
                                          "VERACRUZ DE IGNACIO DE LA LLAVE",
                                          "YUCAT\u00c1N",
                                          "ZACATECAS"),
                        group_by_entidad = TRUE,
                        entidad_tipo = c("Unidad M\u00e9dica",
                                         "Residencia",
                                         "Nacimiento"),
                        fecha_tipo = c("S\u00edntomas",
                                       "Ingreso",
                                       "Defunci\u00f3n")){

  #Checar la descarga
  if (is.null(datos_covid)){
    datos_covid <- descarga_datos_abiertos()
  }

  positividad_pcr <- datos_covid %>%
    dplyr::filter(
      stringr::str_detect(get("RESULTADO_LAB"),
                      "NO POSITIVO A SARS-COV-2|POSITIVO A SARS-COV-2")) %>%
    positividad(entidades, group_by_entidad, entidad_tipo, fecha_tipo) %>%
    dplyr::rename_with(.cols = dplyr::matches("POSITIVIDAD"),
                       function(x) "POSITIVIDAD_PCR")

  return(positividad_pcr)

}
