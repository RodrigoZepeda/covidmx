#' Positividad
#'
#' @description
#' `positividad` Calcula la positividad por fecha agrupando (o sin hacerlo)
#' por covariables.
#'
#' @details
#' Por default calcula la positividad de Antigeno + PCR por estado
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
#' @param tipo_prueba Vector con el tipo de pruebas a incluir
#' `Antigeno`, `PCR`
#'
#' @param group_by_tipo_prueba Boolean determinando si regresa la base
#' con cada entrada agrupada por tipo de pureba (es decir cada fecha
#' y entidad reporta separado positividad en PCR y Antigeno)
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
#' @param defunciones Boolean si incluir solo defunciones `TRUE` o a todos
#' `FALSE`.
#'
#' @param remove_inconclusive No considera las pruebas cuyo resultado es inconcluso o aún
#' no ha sido otorgado
#'
#' @param edad_cut Vector con secuencia de edades para hacer grupos. Por ejemplo
#' `edad_cut = c(0, 10, Inf)` arma dos grupos de edad de 0 a 10 y de 10 a infinito o bien
#' `edad_cut = c(15, 20)` deja solo los registros entre 15 y 20 años. Por default es NULL
#'
#' @param .grouping_vars Vector de variables adicionales de agrupacion de los conteos
#'
#' @param fill_NA Regresa observaciones
#' para todas las combinaciones de variables incluyendo como NA donde no se observaron casos en el
#' denominador. En caso contrario no se incluyen las filas donde no se observaron casos.
#'
#' @param list_name Asigna un nombre en la lista de datos a la base generada
#'
#' @importFrom rlang :=
#'
#' @return Appends a la lista de `datos_covid` una nueva entrada de nombre `list_name`
#' (default: `casos`) con una base de datos (`tibble`) con los
#' resultados agregados.
#' \itemize{
#'   \item positividad - Base de datos generara con los datos agregados (el nombre cambia si
#'   se usa `list_name`).
#'   \item dict - Diccionario de datos
#'   \item dats - Datos originales (conexion a DB)
#'   \item disconnect  - Funcion para desconectarte de DB
#' }
#'
#'@examples
#'\dontrun{
#'datos_covid <- descarga_datos_abiertos(language = "Espanol")
#'
#'#Casos a nivel nacional por estado por tipo de prueba
#'datos_covid <- datos_covid %>% positividad()
#'head(datos_covid$casos)
#'
#'#Total nacional
#'defunciones <- datos_covid %>% positividad(group_by_entidad = FALSE)
#'
#'#Positivos en Jalisco y Colima
#'casos_col_jal <- datos_covid %>% positividad(entidades = c("JALISCO","COLIMA"))
#'
#'#Agrupando ambas pruebas en una sola
#'confirmados <- datos_covid %>%
#'     positividad(entidades = c("JALISCO","COLIMA"),
#'           group_by_tipo_prueba = FALSE)
#'
#'#Regresa la suma de los de COLIMA + JALISCO
#'casos_col_jal <- datos_covid %>%
#'     positividad(entidades = c("JALISCO","COLIMA"),
#'           tipo_paciente = c("AMBULATORIO", "HOSPITALIZADO"),
#'           group_by_tipo_paciente = TRUE)
#'
#'#Si deseas agrupar por una variable que no este en las opciones
#'casos_col_jal <- datos_covid %>%
#'     positividad(entidades = c("JALISCO","COLIMA"),
#'           tipo_paciente = c("AMBULATORIO", "HOSPITALIZADO"),
#'           group_by_tipo_paciente = TRUE,
#'           .grouping_vars = c("DIABETES"))
#'}
#'
#' @export

positividad <- function(datos_covid = NULL,
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
                  group_by_entidad     = TRUE,
                  entidad_tipo         = c("Unidad Medica", "Residencia",
                                           "Nacimiento"),
                  fecha_tipo           = c("Sintomas", "Ingreso",
                                           "Defuncion"),
                  tipo_prueba          = c("Antigeno","PCR"),
                  group_by_tipo_prueba = TRUE,
                  tipo_paciente = c("AMBULATORIO", "HOSPITALIZADO", "NO ESPECIFICADO"),
                  group_by_tipo_paciente = FALSE,
                  tipo_uci      = c("SI","NO","NO APLICA","SE IGNORA","NO ESPECIFICADO"),
                  group_by_tipo_uci  = FALSE,
                  tipo_sector   = c("CRUZ ROJA","DIF","ESTATAL","IMSS",
                                    "IMSS-BIENESTAR","ISSSTE", "MUNICIPAL",
                                    "PEMEX","PRIVADA","SEDENA","SEMAR","SSA",
                                    "UNIVERSITARIO","NO ESPECIFICADO"),
                  group_by_tipo_sector = FALSE,
                  defunciones          = FALSE,
                  edad_cut             = NULL,
                  fill_NA              = TRUE,
                  list_name            = "positividad",
                  remove_inconclusive  = TRUE,
                  .grouping_vars       = c()){


  if (any(stringr::str_detect(names(datos_covid), list_name))){
    stop(glue::glue("Impossible to create variable {list_name} ",
                    "in datos_covid as it already exists"))
  }

  #Calculamos el total de pruebas
  is_pcr  <- any(stringr::str_detect(tolower(tipo_prueba), "pcr"))
  if (is_pcr){
    .grouping_vars  <- c(.grouping_vars, "RESULTADO_LAB")
  }

  is_anti <- any(stringr::str_detect(tolower(tipo_prueba), "ant.*geno"))
  if (is_anti){
    .grouping_vars  <- c(.grouping_vars, "RESULTADO_ANTIGENO")
  }

  message("Leyendo la base...")
  .numero_pruebas <- numero_pruebas(datos_covid = datos_covid, entidades = entidades,
                                    group_by_entidad = group_by_entidad,
                                    entidad_tipo = entidad_tipo,
                                    fecha_tipo   = fecha_tipo,
                                    tipo_prueba  = tipo_prueba,
                                    group_by_tipo_prueba = group_by_tipo_prueba,
                                    tipo_paciente = tipo_paciente,
                                    group_by_tipo_paciente = group_by_tipo_paciente,
                                    tipo_uci = tipo_uci,
                                    group_by_tipo_uci = group_by_tipo_uci,
                                    tipo_sector = tipo_sector,
                                    group_by_tipo_sector = group_by_tipo_sector,
                                    defunciones = defunciones,
                                    edad_cut = edad_cut,
                                    as_tibble = TRUE,
                                    fill_zeros = fill_NA,
                                    list_name = list_name,
                                    .grouping_vars = .grouping_vars)

  if (is_pcr){

    message("Calculando PCR...")

    #Filtramos los totales
    .pcr_totales   <- .numero_pruebas[list_name][[1]] %>%
      dplyr::filter(!!as.symbol("TIPO_PRUEBA") == "PCR")

    if (remove_inconclusive){
      .pcr_totales <- .pcr_totales %>%
        dplyr::filter(!!as.symbol("RESULTADO_LAB") %in% c(1,2)) %>%
        dplyr::select(-dplyr::matches("RESULTADO_ANTIGENO|RESULTADO_LAB"))
    }

    #Filtramos los positivos
    .pcr_positivos <- .numero_pruebas[list_name][[1]] %>%
      dplyr::filter(!!as.symbol("TIPO_PRUEBA") == "PCR" & !!as.symbol("RESULTADO_LAB") == 1) %>%
      dplyr::select(-dplyr::matches("RESULTADO_ANTIGENO|RESULTADO_LAB"))

    #Agrupamos por covariables y contamos
    groups         <- stringr::str_subset(colnames(.pcr_positivos),
                                          "\\bn\\b", negate = T)

    #Obtenemos los positivos
    .pcr_positivos <- .pcr_positivos %>%
      dplyr::group_by_at(groups) %>%
      dplyr::summarise(!!as.symbol("n_positivos") := sum(!!as.symbol("n")), .groups = "keep")

    #Obtenemos los totales
    .pcr_totales <- .pcr_totales %>%
      dplyr::group_by_at(groups) %>%
      dplyr::summarise(!!as.symbol("n_pruebas") := sum(!!as.symbol("n")), .groups = "keep")

    .pcr <- .pcr_totales %>%
      dplyr::left_join(.pcr_positivos, by = groups) %>%
      dplyr::mutate(!!as.symbol("Positividad") := dplyr::if_else(
        !!as.symbol("n_pruebas") != 0,
        as.numeric(!!as.symbol("n_positivos")) / as.numeric(!!as.symbol("n_pruebas")), NA_real_))

  }

  if (is_anti){

    message("Calculando Antigeno...")

    #Filtramos los totales
    .anti_totales   <- .numero_pruebas[list_name][[1]] %>%
      dplyr::filter(!!as.symbol("TIPO_PRUEBA") == "ANTIGENO")

    if (remove_inconclusive){
      .anti_totales <- .anti_totales %>%
        dplyr::filter(!!as.symbol("RESULTADO_ANTIGENO") %in% c(1,2)) %>%
        dplyr::select(-dplyr::matches("RESULTADO_ANTIGENO|RESULTADO_LAB"))
    }

    #Filtramos los positivos
    .anti_positivos <- .numero_pruebas[list_name][[1]] %>%
      dplyr::filter(!!as.symbol("TIPO_PRUEBA") == "ANTIGENO" & !!as.symbol("RESULTADO_ANTIGENO") == 1) %>%
      dplyr::select(-dplyr::matches("RESULTADO_ANTIGENO|RESULTADO_LAB"))

    #Agrupamos por covariables y contamos
    groups         <- stringr::str_subset(colnames(.anti_positivos),
                                          "\\bn\\b", negate = T)

    #Obtenemos los positivos
    .anti_positivos <- .anti_positivos %>%
      dplyr::group_by_at(groups) %>%
      dplyr::summarise(!!as.symbol("n_positivos") := sum(!!as.symbol("n")), .groups = "keep")

    #Obtenemos los totales
    .anti_totales <- .anti_totales %>%
      dplyr::group_by_at(groups) %>%
      dplyr::summarise(!!as.symbol("n_pruebas") := sum(!!as.symbol("n")), .groups = "keep")

    .anti <- .anti_totales %>%
      dplyr::left_join(.anti_positivos, by = groups) %>%
      dplyr::mutate(!!as.symbol("Positividad") := dplyr::if_else(
        !!as.symbol("n_pruebas") != 0,
        as.numeric(!!as.symbol("n_positivos")) / as.numeric(!!as.symbol("n_pruebas")), NA_real_))

  }

  message("Terminando de construir la base")
  if (is_pcr & is_anti){
    .positividad <- .pcr %>%
      dplyr::bind_rows(.anti)
  } else  if (is_pcr & !is_anti){
    .positividad <- .pcr
  } else  if (is_pcr & !is_anti){
    .positividad <- .anti
  } else {
    stop("Selecciona PCR o Antigeno en pruebas")
  }

  .positividad <- .positividad %>%
    dplyr::relocate(!!as.symbol("Positividad"))

  message("Terminado")
  .positividad        <- list(.positividad)
  names(.positividad) <- list_name

  return(append(datos_covid, .positividad))

}
