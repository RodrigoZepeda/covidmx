#' NUMERO DE PRUEBAS ANTÍGENO
#'
#' @description
#' `numero_pruebas_antigeno` Calcula el numero de pruebas que se realizaron de tipo
#' antigeno
#'
#' @details
#' Por default calcula el numero de pruebas de antigeno
#' vease \code{numero_pruebas} y
#' \code{numero_pruebas_pcr} para los numeros especificos de estas pruebas.
#'
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
#'#Positividad nacional
#'datos_covid %>% positividad()
#'
#'#Positividad en Jalisco y Colima
#'datos_covid %>% numero_pruebas_antigeno(entidades = c("JALISCO","COLIMA")) %>%
#'          covid_plot()
#'datos_covid %>% numero_pruebas_antigeno(entidades = c("JALISCO","COLIMA"),
#'                             group_by_entidad = T) %>%
#'          covid_plot()
#'}


numero_pruebas_antigeno <- function(datos_covid = NULL,
                               entidades = c("AGUASCALIENTES", "BAJA CALIFORNIA",
                                             "BAJA CALIFORNIA SUR", "CAMPECHE",
                                             "CHIAPAS", "CHIHUAHUA",
                                             "CIUDAD DE M\u00c9XICO",
                                             "COAHUILA DE ZARAGOZA" , "COLIMA",
                                             "DURANGO", "GUANAJUATO", "GUERRERO",
                                             "HIDALGO", "JALISCO", "M\u00c9XICO",
                                             "MICHOAC\u00c1N DE OCAMPO", "MORELOS",
                                             "NAYARIT", "NUEVO LE\u00d3N", "OAXACA" ,
                                             "PUEBLA", "QUER\u00c9TARO",
                                             "QUINTANA ROO",
                                             "SAN LUIS POTOS\u00cd", "SINALOA",
                                             "SONORA", "TABASCO", "TAMAULIPAS",
                                             "TLAXCALA",
                                             "VERACRUZ DE IGNACIO DE LA LLAVE",
                                             "YUCAT\u00c1N", "ZACATECAS"),
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

  total_pruebas_antigeno <- datos_covid %>%
    dplyr::filter(
      stringr::str_detect(get("RESULTADO_ANTIGENO"),
                          "POSITIVO A SARS-COV-2|NEGATIVO A SARS-COV-2")) %>%
    numero_pruebas(entidades, group_by_entidad, entidad_tipo, fecha_tipo) %>%
    dplyr::rename_with(.cols = dplyr::matches("TOTAL_PRUEBAS"),
                       function(x) "TOTAL_PRUEBAS_ANTIGENO")

  return(total_pruebas_antigeno)

}
