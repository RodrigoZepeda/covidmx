#' CASOS HOSPITALIZADOS
#'
#' @description
#' `casos_hospitalizados` Calcula el numero de casos registrados por fecha
#' con TIPO_PACIENTE = HOSPITALIZADO
#'
#' @details
#' Por default calcula el numero de casos con paciente hospitalizado véase
#' \code{casos}, \code{casos_ambulatorios}, \code{casos_uci}
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
#'datos_covid %>% casos_hospitalizados()
#'
#'#Positividad en Jalisco y Colima
#'datos_covid %>% casos_hospitalizados(entidades = c("JALISCO","COLIMA")) %>%
#'          covid_plot()
#'
#'datos_covid %>%
#'     casos_hospitalizados(entidades = c("JALISCO","COLIMA"),
#'                          tipo_caso == "Confirmados COVID") %>%
#'     covid_plot()
#'
#'datos_covid %>% casos_hospitalizados(entidades = c("JALISCO","COLIMA"),
#'                             group_by_entidad = T) %>%
#'          covid_plot()
#'}
#'
#' @export

casos_hospitalizados <- function(datos_covid = NULL,
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
                                 "Defunci\u00f3n"),
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

  casos_hosp <- datos_covid %>%
    dplyr::filter(stringr::str_detect(get("TIPO_PACIENTE"), "HOSPITALIZADO")) %>%
    casos(entidades, group_by_entidad, entidad_tipo, fecha_tipo, tipo_caso) %>%
    dplyr::rename_with(.cols = dplyr::starts_with("Casos_"),
                       function(x) paste0("Hospitalizado_", x))

  return(casos_hosp)

}
