#' CASOS
#'
#' @description
#' `casos` Calcula el numero de casos registrados por fecha
#'
#' @details
#' Por default calcula el total de casos
#' vease \code{casos_ambulatorios} y
#' \code{casos_uci}, \code{casos_hospitalizados}, \code{defunciones}
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
#'datos_covid %>% casos()
#'
#'#Positividad en Jalisco y Colima
#'datos_covid %>% casos(entidades = c("JALISCO","COLIMA")) %>%
#'          covid_plot()
#'
#'datos_covid %>%
#'     casos(entidades = c("JALISCO","COLIMA"), tipo_caso == "Confirmados COVID") %>%
#'     covid_plot()
#'
#'datos_covid %>% casos(entidades = c("JALISCO","COLIMA"),
#'                             group_by_entidad = T) %>%
#'          covid_plot()
#'}
#'
#' @export

casos <- function(datos_covid = NULL,
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
                                             "Negativo a COVID"
                                             )){

  #Checar la descarga
  if (is.null(datos_covid)){
    datos_covid <- descarga_datos_abiertos()
  }

  #Entidades en mayúscula
  entidades <- toupper(entidades)

  #Seleccionar la entidad
  entidad_tipo <-
    switch(entidad_tipo[1],
           "Unidad M\u00e9dica" = "ENTIDAD_UM",
           "Unidad Medica"      = "ENTIDAD_UM",
           "UM"                 = "ENTIDAD_UM",
           "Residencia"         = "ENTIDAD_RES",
           "RES"                = "ENTIDAD_RES",
           "Nacimiento"         = "ENTIDAD_NAC",
           "NAC"                = "ENTIDAD_NAC",
           stop(paste0("Seleccione entidad_tipo como: Unidad M\u00e9dica /",
                       "Residencia / Nacimiento"))
    )

  #Seleccionar la entidad
  fecha_tipo <-
    switch(fecha_tipo[1],
           "Ingreso"          = "FECHA_INGRESO",
           "INGRESO"          = "FECHA_INGRESO",
           "S\u00edntomas"    = "FECHA_SINTOMAS",
           "Sintomas"         = "FECHA_SINTOMAS",
           "SINTOMAS"         = "FECHA_SINTOMAS",
           "Defunci\u00f3n"   = "FECHA_DEF",
           "Defuncion"        = "FECHA_DEF",
           "DEFUNCION"        = "FECHA_DEF",
           "DEF"              = "FECHA_DEF",
           stop(paste0("Seleccione fecha_tipo como: Ingreso / S\u00edntomas",
                       "/ Defunci\u00f3n"))
    )

  tipo_caso <- toupper(tipo_caso[1])

  #Checamos la variable de entidades
  if (any(stringr::str_detect(entidades,"CIUDAD DE MEXICO|CDMX"))){
    entidades[stringr::str_detect(entidades,"CIUDAD DE MEXICO")] <-
      "CIUDAD DE M\u00c9XICO"
  }

  if (any(stringr::str_detect(entidades,"COAHUILA"))){
    entidades[stringr::str_detect(entidades,"COAHUILA")] <-
      "COAHUILA DE ZARAGOZA"
  }

  if (any(stringr::str_detect(entidades,"\\bMEXICO\\b"))){
    entidades[stringr::str_detect(entidades,"\\bMEXICO\\b")] <-
      "M\u00c9XICO"
  }

  if (any(stringr::str_detect(entidades,"MICHOAC\u00c1N|MICHOACAN"))){
    entidades[stringr::str_detect(entidades,"MICHOAC\u00c1N|MICHOACAN")] <-
      "MICHOAC\u00c1N DE OCAMPO"
  }

  if (any(stringr::str_detect(entidades,"NUEVO LEON"))){
    entidades[stringr::str_detect(entidades,"NUEVO LEON")] <-
      "NUEVO LE\u00d3N"
  }

  if (any(stringr::str_detect(entidades,"QUERETARO"))){
    entidades[stringr::str_detect(entidades,"QUERETARO")] <-
      "QUER\u00c9TARO"
  }

  if (any(stringr::str_detect(entidades,"SAN LUIS POTOSI"))){
    entidades[stringr::str_detect(entidades,"SAN LUIS POTOSI")] <-
      "SAN LUIS POTOS\u00cd"
  }

  if (any(stringr::str_detect(entidades,"VERACRUZ"))){
    entidades[stringr::str_detect(entidades,"VERACRUZ")] <-
      "VERACRUZ DE IGNACIO DE LA LLAVE"
  }

  if (any(stringr::str_detect(entidades,"YUCATAN|YUC"))){
    entidades[stringr::str_detect(entidades,"YUCATAN")] <-
      "YUCAT\u00c1N"
  }

  #Filtramos por entidad
  entidades <- paste0("\\b", paste0(entidades, collapse = "\\b|\\b"),"\\b")
  base_por_entidad <- datos_covid %>%
    dplyr::filter(
      stringr::str_detect(get(entidad_tipo), entidades)
    )


  #Calculamos los casos totales
  if (tipo_caso == "TODOS"){
    casos <- base_por_entidad
  } else if (tipo_caso == "SOSPECHOSOS"){
    casos <- base_por_entidad %>%
      dplyr::filter(stringr::str_detect(get("CLASIFICACI\u00d3N"),
                                        "CASO SOSPECHOSO"))
  } else if (tipo_caso == "CONFIRMADOS COVID"){
    casos <- base_por_entidad %>%
      dplyr::filter(stringr::str_detect(get("CLASIFICACI\u00d3N"),
                                        "CONFIRMADO"))
  } else if (tipo_caso == "SOSPECHOSOS Y CONFIRMADOS COVID"){
    casos <- base_por_entidad %>%
      dplyr::filter(stringr::str_detect(get("CLASIFICACI\u00d3N"),
                                        "CONFIRMADO|SOSPECHOSO"))
  } else if (tipo_caso == "NEGATIVO A COVID"){
    casos <- base_por_entidad %>%
      dplyr::filter(stringr::str_detect(get("CLASIFICACI\u00d3N"),
                                        "NEGATIVO"))
  } else {
    stop(paste0("Seleccione tipo_caso como: TODOS / SOSPECHOSOS / ",
                "SOSPECHOSOS + CONFIRMADOS COVID / NEGATIVO A COVID"))
  }

  #Tomamos el grupo
  if (group_by_entidad){
    casos <- casos %>%
      dplyr::group_by_at(c(entidad_tipo, fecha_tipo))
  } else {
    casos <- casos %>%
      dplyr::group_by_at(fecha_tipo)
  }

  tipo_caso <- stringr::str_replace_all(tipo_caso,"[:space:]","_")

  casos <- casos %>%
    dplyr::tally() %>%
    dplyr::rename_with(.cols = dplyr::matches("\\bn\\b"),
                       function(x) paste0("Casos_", paste0(tipo_caso)))


  return(casos)

}
