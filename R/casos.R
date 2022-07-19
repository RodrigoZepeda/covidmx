#' CASOS
#'
#' @description
#' `casos` Calcula el numero de casos registrados por fecha agrupando (o sin hacerlo)
#' por covariables.
#'
#' @details
#' Por default calcula el total de casos.
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
#' @param defunciones Boolean si incluir sólo defunciones `TRUE` o a todos
#' `FALSE`.
#'
#' @param edad_cut Vector con secuencia de edades para hacer grupos. Por ejemplo
#' `edad_cut = c(0, 10, Inf)` arma dos grupos de edad de 0 a 10 y de 10 a infinito o bien
#' `edad_cut = c(15, 20)` deja sólo los registros entre 15 y 20 años. Por default es NULL
#'
#' @param .grouping_vars Vector de variables adicionales de agrupacion de los conteos
#'
#' @param as_tibble Regresar como `tibble` el resultado. En caso de que `as_tibble`
#' sea `FALSE` se devuelve como conexion en `MARIADB`.
#'
#' @param fill_zeros En caso de que el resultado sea un `tibble` regresa observaciones
#' para todas las combinaciones de variables incluyendo como 0 donde no se observaron casos. En
#' caso contrario no se incluyen las filas donde no se observaron casos.
#'
#' @param list_name Asigna un nombre en la lista de datos a la base generada
#'
#' @importFrom rlang :=
#'
#' @return Appends a la lista de `datos_covid` una nueva entrada de nombre `list_name`
#' (default: `casos`) con una base de datos (`tibble` o `dbConnection`) con los
#' resultados agregados.
#' \itemize{
#'   \item casos - Base de datos generara con los datos agregados (el nombre cambia si
#'   se usa `list_name`).
#'   \item dict - Diccionario de datos
#'   \item dats - Datos originales (conexion a DB)
#'   \item disconnect  - Función para desconectarte de DB
#' }
#'
#' @examples
#' \dontrun{
#' datos_covid <- descarga_datos_abiertos(language = "Espanol")
#'
#' # Casos a nivel nacional
#' datos_covid <- datos_covid %>% casos()
#' head(datos_covid$casos)
#'
#' # Defunciones nacional
#' defunciones <- datos_covid %>% casos(defunciones = TRUE, list_name = "Defunciones")
#'
#' # Positivos en Jalisco y Colima
#' casos_col_jal <- datos_covid %>%
#'   casos(entidades = c("JALISCO", "COLIMA"), list_name = "Jaliscolima")
#'
#' # Agrupando casos por tipo de clasificacion
#' confirmados <- datos_covid %>%
#'   casos(
#'     entidades = c("JALISCO", "COLIMA"),
#'     group_by_tipo_clasificacion = TRUE, list_name = "Jaliscolima2"
#'   )
#'
#' # Regresa la suma de los de COLIMA + JALISCO
#' casos_col_jal <- datos_covid %>%
#'   casos(
#'     entidades = c("JALISCO", "COLIMA"),
#'     group_by_tipo_clasificacion = TRUE,
#'     tipo_paciente = c("AMBULATORIO", "HOSPITALIZADO"),
#'     group_by_tipo_paciente = TRUE,
#'     list_name = "Jalisco + colima"
#'   )
#'
#' # Si deseas agrupar por una variable que no este en las opciones
#' casos_col_jal <- datos_covid %>%
#'   casos(
#'     entidades = c("JALISCO", "COLIMA"),
#'     group_by_tipo_clasificacion = TRUE,
#'     tipo_paciente = c("AMBULATORIO", "HOSPITALIZADO"),
#'     group_by_tipo_paciente = TRUE,
#'     list_name = "Jalisco + colima diabetes",
#'     .grouping_vars = c("DIABETES")
#'   )
#' }
#'
#' @export

casos <- function(datos_covid,
                  entidades = c(
                    "AGUASCALIENTES", "BAJA CALIFORNIA", "BAJA CALIFORNIA SUR",
                    "CAMPECHE", "CHIAPAS", "CHIHUAHUA",
                    "CIUDAD DE M\u00c9XICO", "COAHUILA DE ZARAGOZA", "COLIMA",
                    "DURANGO", "GUANAJUATO", "GUERRERO", "HIDALGO",
                    "JALISCO", "M\u00c9XICO", "MICHOAC\u00c1N DE OCAMPO",
                    "MORELOS", "NAYARIT", "NUEVO LE\u00d3N", "OAXACA",
                    "PUEBLA", "QUER\u00c9TARO", "QUINTANA ROO",
                    "SAN LUIS POTOS\u00cd", "SINALOA", "SONORA",
                    "TABASCO", "TAMAULIPAS", "TLAXCALA",
                    "VERACRUZ DE IGNACIO DE LA LLAVE",
                    "YUCAT\u00c1N", "ZACATECAS"
                  ),
                  group_by_entidad = TRUE,
                  entidad_tipo = c(
                    "Unidad Medica", "Residencia",
                    "Nacimiento"
                  ),
                  fecha_tipo = c(
                    "Sintomas", "Ingreso",
                    "Defuncion"
                  ),
                  tipo_clasificacion = c(
                    "Sospechosos", "Confirmados COVID",
                    "Negativo a COVID", "Inv\u00e1lido",
                    "No realizado"
                  ),
                  group_by_tipo_clasificacion = FALSE,
                  tipo_paciente = c("AMBULATORIO", "HOSPITALIZADO", "NO ESPECIFICADO"),
                  group_by_tipo_paciente = FALSE,
                  tipo_uci = c("SI", "NO", "NO APLICA", "SE IGNORA", "NO ESPECIFICADO"),
                  group_by_tipo_uci = FALSE,
                  tipo_sector = c(
                    "CRUZ ROJA", "DIF", "ESTATAL", "IMSS",
                    "IMSS-BIENESTAR", "ISSSTE", "MUNICIPAL",
                    "PEMEX", "PRIVADA", "SEDENA", "SEMAR", "SSA",
                    "UNIVERSITARIO", "NO ESPECIFICADO"
                  ),
                  group_by_tipo_sector = FALSE,
                  defunciones = FALSE,
                  edad_cut = NULL,
                  as_tibble = TRUE,
                  fill_zeros = as_tibble,
                  list_name = "casos",
                  .grouping_vars = c()) {


  # Finally bind to previous object
  if (any(stringr::str_detect(names(datos_covid), list_name))) {
    stop(glue::glue(
      "Impossible to create variable {list_name} ",
      "in datos_covid as it already exists"
    ))
  }

  if (!as_tibble & fill_zeros) {
    warning("Cannot fill zeros yet if return is not a tibble. Set as_tibble = TRUE")
    fill_zeros <- FALSE
  }

  # Entidades en mayuscula
  entidades <- toupper(entidades)

  #> ENTIDAD----
  # Seleccionar la entidad
  entidad_tipo <- dplyr::case_when(
    stringr::str_detect(tolower(entidad_tipo[1]), "m.*dica|entidad_um") ~ "ENTIDAD_UM",
    stringr::str_detect(tolower(entidad_tipo[1]), "residencia|entidad_res") ~ "ENTIDAD_RES",
    stringr::str_detect(tolower(entidad_tipo[1]), "nacimiento|entidad_nac") ~ "ENTIDAD_NAC",
  )

  # Seleccionar la entidad
  fecha_tipo <-
    dplyr::case_when(
      stringr::str_detect(tolower(fecha_tipo[1]), "ingreso") ~ "FECHA_INGRESO",
      stringr::str_detect(tolower(fecha_tipo[1]), "s.*ntomas") ~ "FECHA_SINTOMAS",
      stringr::str_detect(tolower(fecha_tipo[1]), "defunci.*n|fecha_def") ~ "FECHA_DEF"
    )

  # Checamos la variable de entidades
  if (any(stringr::str_detect(entidades, "CIUDAD DE MEXICO|CDMX"))) {
    entidades[stringr::str_detect(entidades, "CIUDAD DE MEXICO")] <-
      "CIUDAD DE M\u00c9XICO"
  }

  if (any(stringr::str_detect(entidades, "COAHUILA"))) {
    entidades[stringr::str_detect(entidades, "COAHUILA")] <-
      "COAHUILA DE ZARAGOZA"
  }

  if (any(stringr::str_detect(entidades, "\\bMEXICO\\b|EDOMEX"))) {
    entidades[stringr::str_detect(entidades, "\\bMEXICO\\b|EDOMEX")] <-
      "M\u00c9XICO"
  }

  if (any(stringr::str_detect(entidades, "MICHOAC\u00c1N|MICHOACAN"))) {
    entidades[stringr::str_detect(entidades, "MICHOAC\u00c1N|MICHOACAN")] <-
      "MICHOAC\u00c1N DE OCAMPO"
  }

  if (any(stringr::str_detect(entidades, "NUEVO LEON"))) {
    entidades[stringr::str_detect(entidades, "NUEVO LEON")] <-
      "NUEVO LE\u00d3N"
  }

  if (any(stringr::str_detect(entidades, "QUERETARO"))) {
    entidades[stringr::str_detect(entidades, "QUERETARO")] <-
      "QUER\u00c9TARO"
  }

  if (any(stringr::str_detect(entidades, "SAN LUIS POTOSI"))) {
    entidades[stringr::str_detect(entidades, "SAN LUIS POTOSI")] <-
      "SAN LUIS POTOS\u00cd"
  }

  if (any(stringr::str_detect(entidades, "VERACRUZ"))) {
    entidades[stringr::str_detect(entidades, "VERACRUZ")] <-
      "VERACRUZ DE IGNACIO DE LA LLAVE"
  }

  if (any(stringr::str_detect(entidades, "YUCATAN|YUC"))) {
    entidades[stringr::str_detect(entidades, "YUCATAN")] <-
      "YUCAT\u00c1N"
  }

  # Filtramos por entidad
  entidades <-
    datos_covid$dict[entidad_tipo][[1]] %>%
    dplyr::filter(
      stringr::str_detect(
        get("ENTIDAD_FEDERATIVA"),
        paste0(paste0("^", paste0(entidades, "$")), collapse = "|")
      )
    )

  if (nrow(entidades) < 1) {
    stop("No logramos encontrar esas entidades")
  }

  lista_entidades <- paste0(entidades$CLAVE_ENTIDAD, collapse = "|")
  .casos <- datos_covid$dats %>%
    dplyr::filter(
      stringr::str_detect(paste0("\\^", !!as.symbol(entidad_tipo), "\\$"), lista_entidades)
    )

  #> CLASIFICACION FINAL----
  # Filtramos por tipo de caso
  clasificaciones_finales <- c()

  if (any(stringr::str_detect(tipo_clasificacion, "Sospechoso"))) {
    clasificaciones_finales <- c(clasificaciones_finales, 6) # Clasificacion final = 6: sospechoso
  }

  if (any(stringr::str_detect(tipo_clasificacion, "Confirmado"))) {
    # Clasificacion final = 1,2,3: confirmado
    clasificaciones_finales <- c(clasificaciones_finales, c(1, 2, 3))
  }

  if (any(stringr::str_detect(tipo_clasificacion, "Negativo"))) {
    # Clasificacion final = 7: negativo
    clasificaciones_finales <- c(clasificaciones_finales, 7)
  }

  if (any(stringr::str_detect(tipo_clasificacion, "Inv.*lido"))) {
    # Clasificacion final = 4: inv\u00e1lido
    clasificaciones_finales <- c(clasificaciones_finales, 4)
  }

  if (any(stringr::str_detect(tipo_clasificacion, "No realizado"))) {
    # Clasificacion final = 4: inv\u00e1lido
    clasificaciones_finales <- c(clasificaciones_finales, 5)
  }

  if (length(clasificaciones_finales) > 0) {
    .casos <- .casos %>%
      dplyr::filter(!!as.symbol("CLASIFICACION_FINAL") %in% clasificaciones_finales)
  }

  #> TIPO DE PACIENTE----
  # Filtramos por tipo de paciente
  pacientes <-
    datos_covid$dict["PACIENTE"][[1]] %>%
    dplyr::filter(
      stringr::str_detect(
        get("DESCRIPCI\u00d3N"),
        paste0(
          "\\b",
          paste0(tipo_paciente, collapse = "\\b|\\b"), "\\b"
        )
      )
    )

  lista_claves <- as.numeric(pacientes$CLAVE)
  .casos <- .casos %>%
    dplyr::filter(!!as.symbol("TIPO_PACIENTE") %in% lista_claves)

  #> TIPO DE UCI----
  # Filtramos por tipo de uci
  ucis <-
    datos_covid$dict["UCI"][[1]] %>%
    dplyr::filter(
      stringr::str_detect(
        get("DESCRIPCI\u00d3N"),
        paste0(paste0("^", tipo_uci, "$"), collapse = "|")
      )
    )

  lista_claves <- as.numeric(ucis$CLAVE)
  .casos <- .casos %>%
    dplyr::filter(!!as.symbol("UCI") %in% lista_claves)

  #> TIPO DE SECTOR----
  # Filtramos por tipo de uci
  sectores <-
    datos_covid$dict["SECTOR"][[1]] %>%
    dplyr::filter(
      stringr::str_detect(
        get("DESCRIPCI\u00d3N"),
        paste0(paste0("^", tipo_sector, "$"), collapse = "|")
      )
    )

  lista_claves <- as.numeric(sectores$CLAVE)
  .casos <- .casos %>%
    dplyr::filter(!!as.symbol("SECTOR") %in% lista_claves)

  #> DEFUNCIONES
  if (defunciones) {
    .casos <- .casos %>%
      dplyr::filter(!!as.symbol("FECHA_DEF") >= as.Date("2000/01/01"))
  }

  #> EDADES
  if (!is.null(edad_cut)) {
    .casos <- .casos %>%
      dplyr::mutate(!!as.symbol("EDAD_CAT") := cut(!!as.symbol("EDAD"),
        breaks = edad_cut
      )) %>%
      dplyr::filter(!is.na(!!as.symbol("EDAD_CAT")))
  }

  #> AGRUPACI\u00d3N
  .casos <- .casos %>%
    dplyr::group_by_at(fecha_tipo)

  if (length(.grouping_vars) > 0) {
    for (var in .grouping_vars) {
      .casos <- .casos %>%
        dplyr::group_by_at(var, .add = TRUE)
    }
  }

  #> AGRUPACI\u00d3N EDAD
  if (!is.null(edad_cut)) {
    .casos <- .casos %>%
      dplyr::group_by_at("EDAD_CAT", .add = TRUE)
  }

  # Tomamos el grupo
  if (group_by_entidad) {
    .casos <- .casos %>%
      dplyr::group_by_at(entidad_tipo, .add = TRUE)
  }

  # Tomamos el grupo
  if (group_by_tipo_clasificacion) {
    .casos <- .casos %>%
      dplyr::group_by_at("CLASIFICACION_FINAL", .add = TRUE)
  }

  if (group_by_tipo_paciente) {
    .casos <- .casos %>%
      dplyr::group_by_at("TIPO_PACIENTE", .add = TRUE)
  }

  if (group_by_tipo_sector) {
    .casos <- .casos %>%
      dplyr::group_by_at("SECTOR", .add = TRUE)
  }

  if (group_by_tipo_uci) {
    .casos <- .casos %>%
      dplyr::group_by_at("UCI", .add = TRUE)
  }

  # Conteo de los .casos
  .casos <- .casos %>%
    dplyr::tally() %>%
    dplyr::ungroup()

  if (as_tibble) {
    .casos <- .casos %>%
      dplyr::collect()

    if (fill_zeros) {

      # Select the other variables to expand grid
      .grouping_vars <- .casos %>%
        dplyr::select(-dplyr::matches("\\bn\\b")) %>%
        dplyr::select(-dplyr::starts_with("FECHA")) %>%
        dplyr::distinct()

      # Check the dates to expand
      .fechasminmax <- datos_covid$dats %>%
        dplyr::select_at(fecha_tipo) %>%
        dplyr::summarise(
          fechamin = min(!!as.symbol(fecha_tipo), na.rm = TRUE),
          fechamax = max(!!as.symbol(fecha_tipo), na.rm = TRUE)
        ) %>%
        dplyr::collect()

      .datesq <- seq(.fechasminmax$fechamin[1], .fechasminmax$fechamax[1], by = "1 day")

      # Create grid of all possible dates
      .grid_casos <- tidyr::expand_grid(!!as.symbol(fecha_tipo) := .datesq, .grouping_vars)

      # Full join
      .casos <- .casos %>%
        dplyr::full_join(.grid_casos, by = colnames(.grid_casos)) %>%
        dplyr::mutate(!!as.symbol("n") := tidyr::replace_na(!!as.symbol("n"), 0)) %>%
        dplyr::ungroup()
    }
  }

  if (nrow(entidades) > 0 & group_by_entidad) {
    name_join <- c("CLAVE_ENTIDAD")
    names(name_join) <- entidad_tipo
    .casos <- .casos %>% dplyr::left_join(datos_covid$dict[entidad_tipo][[1]], by = name_join)
  }

  if (length(clasificaciones_finales) > 0 & group_by_tipo_clasificacion) {
    name_join <- c("CLAVE")
    names(name_join) <- "CLASIFICACION_FINAL"
    .casos <- .casos %>%
      dplyr::left_join(datos_covid$dict["CLASIFICACION_FINAL"][[1]][, c(1, 2)], by = name_join)
  }

  if (nrow(pacientes) > 0 & group_by_tipo_paciente) {
    name_join <- c("CLAVE")
    names(name_join) <- "TIPO_PACIENTE"
    paciente_df <- datos_covid$dict["PACIENTE"][[1]]
    colnames(paciente_df) <- c("CLAVE", "DESCRIPCION_TIPO_PACIENTE")
    .casos <- .casos %>%
      dplyr::left_join(paciente_df, by = name_join)
  }

  if (nrow(ucis) > 0 & group_by_tipo_uci) {
    name_join <- c("CLAVE")
    names(name_join) <- "UCI"
    uci_df <- datos_covid$dict["UCI"][[1]]
    colnames(uci_df) <- c("CLAVE", "DESCRIPCION_TIPO_UCI")
    .casos <- .casos %>%
      dplyr::left_join(uci_df, by = name_join)
  }

  if (nrow(sectores) > 0 & group_by_tipo_sector) {
    name_join <- c("CLAVE")
    names(name_join) <- "SECTOR"
    sector_df <- datos_covid$dict["SECTOR"][[1]]
    colnames(sector_df) <- c("CLAVE", "DESCRIPCION_TIPO_SECTOR")
    .casos <- .casos %>%
      dplyr::left_join(sector_df, by = name_join)
  }

  .casos <- list(.casos)
  names(.casos) <- list_name

  return(append(datos_covid, .casos))
}
