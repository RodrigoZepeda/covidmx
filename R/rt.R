#' RT: Número efectivo de reproducción
#'
#' @description
#' `estima_rt` Calcula el número efectivo de reproducción por fecha y entidad
#'
#' @details
#' Por default calcula el número efectivo de reproducción por estado
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
#' @param tipo_paciente Vector con el tipo de pacientes a incluir. Opciones:
#'  `AMBULATORIO`, `HOSPITALIZADO`, `NO ESPECIFICADO`
#'
#' @param method Method for estimating RT with `EpiEstim::estimate_R`
#'
#' @param config Configuration for estimating RT with `EpiEstim::estimate_R`
#'
#' @param tipo_clasificacion Vector con el tipo de clasificaciones a incluir:
#' `Sospechosos`,`Confirmados COVID`, `Negativo a COVID`,
#' `Inv\u00e1lido`, `No realizado`
#'
#' @param min_date Mínima fecha a partir de la cual estimar el RT
#'
#' @param max_date Máxima fecha a partir de la cual estimar el RT
#'
#' @param list_name Asigna un nombre en la lista de datos a la base generada
#'
#' @param ... Parámetros adicionales para `EpiEstim::estimate_R`.
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
#' @examples
#' \dontrun{
#' datos_covid <- descarga_datos_abiertos(language = "Espanol")
#'
#' # Casos a nivel nacional en los confirmados
#' datos_covid <- datos_covid %>%
#'   estima_rt(tipo_clasificacion == "Confirmados COVID",
#'     group_by_entidad = FALSE
#'   )
#'
#' # Casos en AGS, CHI en los confirmados
#' datos_covid <- datos_covid %>%
#'   estima_rt(
#'     entidades = c("CHIHUAHUA", "AGUASCALIENTES"),
#'     tipo_clasificacion == "Confirmados COVID",
#'     group_by_entidad = TRUE
#'   )
#' }
#'
#' @export

estima_rt <- function(datos_covid,
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
                      tipo_paciente = c("AMBULATORIO", "HOSPITALIZADO", "NO ESPECIFICADO"),
                      list_name = "estima_rt",
                      min_date = as.Date("2021/11/21", format = "%Y/%m/%d"),
                      max_date = as.Date(Sys.time()),
                      method = "parametric_si",
                      config = EpiEstim::make_config(
                        list(
                          mean_si = 3.5,
                          std_si = 1.5
                        )
                      ),
                      ...) {
  if (any(stringr::str_detect(names(datos_covid), list_name))) {
    stop(glue::glue(
      "Impossible to create variable {list_name} ",
      "in datos_covid as it already exists"
    ))
  }

  message("Estimando casos")
  .casos <- casos(
    datos_covid = datos_covid,
    entidades = entidades,
    group_by_entidad = group_by_entidad,
    fecha_tipo = fecha_tipo,
    tipo_clasificacion = tipo_clasificacion,
    group_by_tipo_clasificacion = FALSE,
    tipo_paciente = tipo_paciente,
    group_by_tipo_paciente = FALSE,
    tipo_uci = c("SI", "NO", "NO APLICA", "SE IGNORA", "NO ESPECIFICADO"),
    group_by_tipo_uci = FALSE,
    tipo_sector = c(
      "CRUZ ROJA", "DIF", "ESTATAL", "IMSS", "IMSS-BIENESTAR", "ISSSTE",
      "MUNICIPAL", "PEMEX", "PRIVADA", "SEDENA", "SEMAR", "SSA", "UNIVERSITARIO",
      "NO ESPECIFICADO"
    ),
    group_by_tipo_sector = FALSE,
    defunciones = FALSE,
    edad_cut = NULL,
    as_tibble = TRUE,
    fill_zeros = TRUE,
    list_name = list_name,
    .grouping_vars = c()
  )[[list_name]]

  message("Estimando RT")

  # Detectamos cuál es la fecha
  fecha_name <- stringr::str_subset(colnames(.casos), "FECHA")
  col_gp <- stringr::str_subset(colnames(.casos), "FECHA|\\bn\\b", negate = TRUE)

  if (length(col_gp) > 0) {
    for (col in col_gp) {
      .casos <- .casos %>%
        dplyr::group_by_at(col, .add = TRUE)
    }
  }

  .casos <- .casos %>%
    dplyr::filter(!!as.symbol(fecha_name) >= !!min_date) %>%
    dplyr::filter(!!as.symbol(fecha_name) <= !!max_date)

  mfec <- .casos[fecha_name] %>%
    dplyr::summarise(!!as.symbol("min") := min(!!as.symbol(fecha_name))) %>%
    as.vector()

  message("Calculando el RT")
  df_rt <- .casos %>%
    dplyr::arrange(!!as.symbol(fecha_name)) %>%
    dplyr::summarise(EpiEstim::estimate_R(as.numeric(!!as.symbol("n")),
      method = method, config = config, ...
    )$R) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(!!as.symbol(paste0(fecha_name, "_start")) := mfec$min[1] + lubridate::days(!!as.symbol("t_start"))) %>%
    dplyr::mutate(!!as.symbol(paste0(fecha_name, "_end")) := mfec$min[1] + lubridate::days(!!as.symbol("t_end"))) %>%
    dplyr::mutate(!!as.symbol(fecha_name) := mfec$min[1] + lubridate::days((!!as.symbol("t_start") + !!as.symbol("t_end")) / 2))

  message("Terminado")
  df_rt <- list(df_rt)
  names(df_rt) <- list_name

  return(append(datos_covid, df_rt))
}
