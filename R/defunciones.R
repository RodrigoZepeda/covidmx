#' DEFUNCIONES
#'
#' @description
#' `defunciones` Calcula el numero de casos registrados por fecha de defunción
#'
#' @details
#' Por default calcula el numero de defunciones
#' vease \code{defunciones_ambulatorios},
#' \code{defunciones_hospitalizados} y \code{defunciones_uci}
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
#' @param tipo_caso Selecciona `Todos`, `Confirmados COVID`,`Sospechosos`,
#' `Sospechosos y Confirmados COVID`, `Negativos a COVID`
#'@examples
#'\dontrun{
#'datos_covid <- descarga_datos_abiertos(language = "Espanol")
#'
#'#Positividad nacional
#'datos_covid %>% defunciones()
#'
#'#Positividad en Jalisco y Colima
#'datos_covid %>% defunciones(entidades = c("JALISCO","COLIMA")) %>%
#'          covid_plot()
#'
#'datos_covid %>%
#'     defunciones(entidades = c("JALISCO","COLIMA"), tipo_caso == "Confirmados COVID") %>%
#'     acumulados() %>%
#'     covid_plot()
#'
#'datos_covid %>% defunciones(entidades = c("JALISCO","COLIMA"),
#'                             group_by_entidad = T) %>%
#'          acumulados() %>%
#'          covid_plot()
#'}
#'
#' @export

defunciones <- function(datos_covid = NULL,
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
                  fecha_tipo = c("Defunci\u00f3n",
                                 "S\u00edntomas",
                                 "Ingreso"),
                  tipo_caso = c("Todos",
                                "Sospechosos",
                                "Confirmados COVID",
                                "Sospechosos y Confirmados COVID",
                                "Negativos a COVID"
                  )){

  #Checar la descarga
  if (is.null(datos_covid)){
    datos_covid <- descarga_datos_abiertos()
  }

  defunciones <- datos_covid %>%
    dplyr::filter(!is.na(get("FECHA_DEF"))) %>%
    casos(entidades, group_by_entidad, entidad_tipo, fecha_tipo, tipo_caso) %>%
    dplyr::rename_with(.cols = dplyr::matches("Casos"),
                       function(x) paste0("Defunciones_",x))


  return(defunciones)

}
