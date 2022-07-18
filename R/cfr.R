#' Case Fatality Rate (CFR)
#'
#' @description
#' `chr` Calcula la proporción de enfermos que fallecen sobre todos los enfermos confirmados
#' en distintas categorías (residencia / edad / etc)
#'
#' @details
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
#'
#' @param group_by_entidad Si junta las entidades en una sola
#' o bien las muestra por separado sin agrupar.
#'
#' @param entidad_tipo Selecciona `Unidad Medica`, `Nacimiento` o `Residencia`.
#' por default incluye `Unidad Medica`
#'
#' @param fecha_tipo Selecciona `Ingreso`, `Sintomas` o `Defuncion` por default
#' incluye fecha de `Sintomas`
#'
#' @param tipo_clasificacion Vector con el tipo de clasificaciones a incluir:
#' `Sospechosos`,`Confirmados COVID`, `Negativo a COVID`,
#' `Inv\u00e1lido`, `No realizado`
#'
#' @param group_by_tipo_clasificacion Boolean determinando si regresa la base
#' con cada entrada agrupada por tipo de clasificación (es decir cada fecha
#' se generan tantos observaciones como grupos de tipo de clasificación)
#'
#' @param tipo_paciente Vector con el tipo de pacientes a incluir. Opciones:
#'  `AMBULATORIO`, `HOSPITALIZADO`, `NO ESPECIFICADO`
#'
#' @param group_by_tipo_paciente Boolean determinando si regresa la base
#' con cada entrada agrupada por tipo de paciente (es decir cada fecha
#' se generan tantos observaciones como grupos de tipo de paciente)
#'
#' @param tipo_uci Vector con el tipo de valores para Unidad de Cuidado Intensivo a incluir:
#'  `SI`,`NO`,`NO APLICA`,`SE IGNORA`,`NO ESPECIFICADO`
#'
#' @param group_by_tipo_uci Boolean determinando si regresa la base
#' con cada entrada agrupada por tipo de uci (es decir cada fecha
#' se generan tantos observaciones como grupos de tipo de uci)
#'
#' @param tipo_sector Vector con los sectores del sistema de salud a incluir:
#' `CRUZ ROJA`,`DIF`,`ESTATAL`,`IMSS`,`IMSS-BIENESTAR`,`ISSSTE`, `MUNICIPAL`,`PEMEX`,
#' `PRIVADA`,`SEDENA`,`SEMAR`,`SSA`, `UNIVERSITARIO`,`NO ESPECIFICADO`.
#'
#' @param group_by_tipo_sector Boolean determinando si regresa la base
#' con cada entrada agrupada por tipo de sector (es decir cada fecha
#' se generan tantos observaciones como grupos de tipo de sector)
#'
#' @param fill_NA Regresa observaciones
#' para todas las combinaciones de variables incluyendo como NA donde no se observaron casos
#' y por tanto el denominador y el `chr` es indefinido.
#'
#' @param edad_cut Vector con secuencia de edades para hacer grupos. Por ejemplo
#' `edad_cut = c(0, 10, Inf)` arma dos grupos de edad de 0 a 10 y de 10 a infinito o bien
#' `edad_cut = c(15, 20)` deja sólo los registros entre 15 y 20 años. Por default es NULL
#'
#' @param .grouping_vars Vector de variables adicionales de agrupacion de los conteos
#'
#' @param list_name Asigna un nombre en la lista de datos a la base generada
#' 
#' @param quiet No arroja ningun mensaje 
#'
#' @importFrom rlang :=
#'
#' @return Appends a la lista de `datos_covid` una nueva entrada de nombre `list_name`
#' (default: `casos`) con una base de datos (`tibble` o `dbConnection`) con los
#' resultados agregados.
#' \itemize{
#'   \item `case hospitalization rate` - Base de datos generara con los datos agregados (el nombre cambia si
#'   se usa `list_name`).
#'   \item dict - Diccionario de datos
#'   \item dats - Datos originales (conexion a DB)
#'   \item disconnect  - Función para desconectarte de DB
#' }
#'
#'@examples
#'\dontrun{
#'datos_covid <- descarga_datos_abiertos(language = "Espanol")
#'
#'#Casos a nivel nacional
#'datos_covid <- datos_covid %>% cfr()
#'head(datos_covid$`case fatality rate`)
#'
#'#Nacional
#'datos_covid <- datos_covid %>% cfr(list_name = "cfr_nacional", group_by_entidad = F)
#'
#'#CFR en Jalisco y Colima
#'datos_covid <- datos_covid %>%
#'                     cfr(entidades = c("JALISCO","COLIMA"), list_name = "cfr_Jaliscolima")
#'
#'#Calcula el CFR suponiendo toda la base son confirmados
#'datos_covid <- datos_covid %>%
#'     cfr(entidades = c("JALISCO","COLIMA"),
#'           tipo_clasificacion = c("Sospechosos","Confirmados COVID",
#'           "Negativo a COVID","Inv\u00e1lido", "No realizado"),
#'           group_by_tipo_clasificacion = TRUE, list_name = "Jaliscolima2_cfr")
#'
#'#Distinguiendo entre ambulatorio y hospitalizado
#'datos_covid <- datos_covid %>%
#'     casos(entidades = c("JALISCO","COLIMA"),
#'           tipo_paciente = c("AMBULATORIO", "HOSPITALIZADO"),
#'           group_by_tipo_paciente = TRUE,
#'           list_name = "Jalisco + colima cfr")
#'
#'#CFR en ambulatorios y hospitalizados
#'datos_covid %>%
#' cfr(
#'   tipo_paciente = c("AMBULATORIO", "HOSPITALIZADO"),
#'   group_by_tipo_paciente = TRUE,
#'   group_by_entidad = FALSE,
#'   list_name = "CFR_hosp_amb"
#' )  %>%
#' plot_covid(df_name = "CFR_hosp_amb", type = "line", facet_ncol = 3)
#'
#'#Si deseas agrupar por una variable que no este en las opciones
#'datos_covid <- datos_covid %>%
#'     cfr(.grouping_vars = c("DIABETES"), list_name = "cfr_diab")
#'}
#'
#' @export

cfr <- function(datos_covid,
                entidades   = c("AGUASCALIENTES", "BAJA CALIFORNIA", "BAJA CALIFORNIA SUR",
                                "CAMPECHE", "CHIAPAS", "CHIHUAHUA",
                                "CIUDAD DE M\u00c9XICO","COAHUILA DE ZARAGOZA" , "COLIMA",
                                "DURANGO", "GUANAJUATO", "GUERRERO","HIDALGO",
                                "JALISCO", "M\u00c9XICO", "MICHOAC\u00c1N DE OCAMPO",
                                "MORELOS","NAYARIT","NUEVO LE\u00d3N", "OAXACA",
                                "PUEBLA", "QUER\u00c9TARO", "QUINTANA ROO",
                                "SAN LUIS POTOS\u00cd", "SINALOA", "SONORA",
                                "TABASCO", "TAMAULIPAS", "TLAXCALA",
                                "VERACRUZ DE IGNACIO DE LA LLAVE",
                                "YUCAT\u00c1N", "ZACATECAS"),
                group_by_entidad   = TRUE,
                entidad_tipo       = c("Unidad Medica", "Residencia",
                                       "Nacimiento"),
                fecha_tipo         = c("Sintomas", "Ingreso",
                                       "Defuncion"),
                tipo_uci = c("SI","NO","NO APLICA","SE IGNORA","NO ESPECIFICADO"),
                group_by_tipo_uci = FALSE,
                tipo_clasificacion = c("Confirmados COVID"),
                group_by_tipo_clasificacion = FALSE,
                tipo_paciente = c("AMBULATORIO", "HOSPITALIZADO", "NO ESPECIFICADO"),
                group_by_tipo_paciente = FALSE,
                tipo_sector   = c("CRUZ ROJA","DIF","ESTATAL","IMSS",
                                  "IMSS-BIENESTAR","ISSSTE", "MUNICIPAL",
                                  "PEMEX","PRIVADA","SEDENA","SEMAR","SSA",
                                  "UNIVERSITARIO","NO ESPECIFICADO"),
                group_by_tipo_sector = FALSE,
                edad_cut             = NULL,
                fill_NA              = TRUE,
                quiet                = FALSE,
                list_name            = "case fatality rate",
                .grouping_vars       = c()){


  #Finally bind to previous object
  if (any(stringr::str_detect(names(datos_covid), list_name))){
    stop(glue::glue("Impossible to create variable {list_name} ",
                    "in datos_covid as it already exists"))
  }

  if (!quiet){
    message("Calculando los casos totales")
  }

  #Trick to get new name
  name_1         <- paste(c(names(datos_covid),"1"), collapse = "")
  name_2         <- paste(c(names(datos_covid),"2"), collapse = "")
  .casos_totales <- casos(datos_covid        = datos_covid,
                          entidades          = entidades,
                          group_by_entidad   = group_by_entidad,
                          entidad_tipo       = entidad_tipo,
                          fecha_tipo         = fecha_tipo,
                          tipo_clasificacion = tipo_clasificacion,
                          group_by_tipo_clasificacion = group_by_tipo_clasificacion,
                          tipo_paciente          = tipo_paciente,
                          group_by_tipo_paciente = group_by_tipo_paciente,
                          tipo_uci               = tipo_uci,
                          group_by_tipo_uci      = group_by_tipo_uci,
                          tipo_sector            = tipo_sector,
                          group_by_tipo_sector   = group_by_tipo_sector,
                          defunciones            = FALSE,
                          edad_cut               = edad_cut,
                          as_tibble              = TRUE,
                          fill_zeros             = fill_NA,
                          list_name              = name_1,
                          .grouping_vars       = c())[[name_1]]

  if (!quiet){
    message("Calculando las defunciones")
  }
  .casos_defunciones <- casos(datos_covid = datos_covid,
                                 entidades   = entidades,
                                 group_by_entidad   = group_by_entidad,
                                 entidad_tipo       = entidad_tipo,
                                 fecha_tipo         = fecha_tipo,
                                 tipo_clasificacion = tipo_clasificacion,
                                 group_by_tipo_clasificacion = group_by_tipo_clasificacion,
                                 tipo_paciente          = tipo_paciente,
                                 group_by_tipo_paciente = group_by_tipo_paciente,
                                 tipo_uci               = tipo_uci,
                                 group_by_tipo_uci      = group_by_tipo_uci,
                                 tipo_sector            = tipo_sector,
                                 group_by_tipo_sector   = group_by_tipo_sector,
                                 defunciones            = TRUE,
                                 edad_cut               = edad_cut,
                                 fill_zeros             = TRUE,
                                 list_name              = name_2,
                                 .grouping_vars         = c())[[name_2]]

  if (!quiet){
    message("Calculando el cfr")
  }
  
  .casos_totales <- .casos_totales %>%
    dplyr::left_join(.casos_defunciones %>%
                       dplyr::rename(!!as.symbol("d") := !!as.symbol("n")),
                     by = colnames(.casos_totales)[which(colnames(.casos_totales) != "n")]) %>%
    dplyr::mutate(!!as.symbol("CASE FATALITY RATE") :=
                    dplyr::if_else(!is.na(!!as.symbol("n")),
                                   as.numeric(!!as.symbol("d")) / as.numeric(!!as.symbol("n")),
                                   NA_real_)) %>%
    dplyr::select(-!!as.symbol("n"), -!!as.symbol("d"))


  .casos_totales        <- list(.casos_totales)
  names(.casos_totales) <- list_name

  return(append(datos_covid, .casos_totales))

}
