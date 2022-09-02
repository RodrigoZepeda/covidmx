#' Case Hospitalization Rate (CHR)
#'
#' @description
#' `chr` Calcula la proporción de enfermos que resultan hospitalizados sobre todos los enfermos
#' confirmados en distintas categorías (residencia / edad / etc)
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
#' @param incluir_paciente_no_especificado Si en el denominador se incluyen los pacientes
#' cuyo tipo es  `NO ESPECIFICADO`. Por default es `FALSE` por lo que sólo se incluyen
#'  `AMBULATORIO`, `HOSPITALIZADO`.
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
#' @param fill_NA Regresa observaciones
#' para todas las combinaciones de variables incluyendo como NA donde no se observaron casos
#' y por tanto el denominador y el `chr` es indefinido.
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
#' @examples
#' \dontrun{
#' datos_covid <- descarga_datos_abiertos()
#'
#' # Casos a nivel nacional
#' datos_covid <- datos_covid |> chr()
#' head(datos_covid$`case hospitalization rate`)
#'
#' # Nacional
#' datos_covid <- datos_covid |> chr(list_name = "chr nacional", group_by_entidad = F)
#'
#' # CFR en Jalisco y Colima
#' datos_covid <- datos_covid |>
#'   chr(entidades = c("JALISCO", "COLIMA"), list_name = "chr_Jaliscolima")
#'
#' # Calcula el CHR suponiendo toda la base son confirmados
#' datos_covid <- datos_covid |>
#'   chr(
#'     entidades = c("JALISCO", "COLIMA"),
#'     tipo_clasificacion = c(
#'       "Sospechosos", "Confirmados COVID",
#'       "Negativo a COVID", "Inv\u00e1lido", "No realizado"
#'     ),
#'     group_by_tipo_clasificacion = TRUE, list_name = "Jaliscolima2_chr"
#'   )
#'
#' # Distinguiendo sólo entre defunciones
#' datos_covid <- datos_covid |>
#'   chr(
#'     entidades = c("JALISCO", "COLIMA"),
#'     defunciones = TRUE,
#'     list_name = "Jalisco + colima chr"
#'   )
#'
#' # Si deseas agrupar por una variable que no este en las opciones
#' datos_covid <- datos_covid |>
#'   chr(.grouping_vars = c("DIABETES"), list_name = "chr_diab")
#' }
#'
#' @export

chr <- function(datos_covid,
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
                tipo_clasificacion = c("Confirmados COVID"),
                group_by_tipo_clasificacion = FALSE,
                incluir_paciente_no_especificado = FALSE,
                tipo_sector = c(
                  "CRUZ ROJA", "DIF", "ESTATAL", "IMSS",
                  "IMSS-BIENESTAR", "ISSSTE", "MUNICIPAL",
                  "PEMEX", "PRIVADA", "SEDENA", "SEMAR", "SSA",
                  "UNIVERSITARIO", "NO ESPECIFICADO"
                ),
                group_by_tipo_sector = FALSE,
                defunciones = FALSE,
                edad_cut = NULL,
                fill_NA = TRUE,
                list_name = "case hospitalization rate",
                quiet = FALSE,
                .grouping_vars = c()) {


  # Finally bind to previous object
  if (any(stringr::str_detect(names(datos_covid), list_name))) {
    cli::cli_abort(
      "Imposible crear elemento {list_name} pues ya existe en la lista.
       Utiliza {.code list_name = 'nuevo_nombre'} para generar otro elemento"
    )
  }

  if (incluir_paciente_no_especificado) {
    tp <- c("AMBULATORIO", "HOSPITALIZADO", "NO ESPECIFICADO")
  } else {
    tp <- c("AMBULATORIO", "HOSPITALIZADO")
  }

  if (!quiet) {
    cli::cli_alert("Calculando los casos totales")
  }

  # Trick to get new name
  name_1 <- paste(c(names(datos_covid), "1"), collapse = "")
  name_2 <- paste(c(names(datos_covid), "2"), collapse = "")
  .casos_totales <- casos(
    datos_covid = datos_covid,
    entidades = entidades,
    group_by_entidad = group_by_entidad,
    entidad_tipo = entidad_tipo,
    fecha_tipo = fecha_tipo,
    tipo_clasificacion = tipo_clasificacion,
    group_by_tipo_clasificacion = group_by_tipo_clasificacion,
    tipo_paciente = tp,
    group_by_tipo_paciente = FALSE,
    tipo_uci = c("SI", "NO", "NO APLICA", "SE IGNORA", "NO ESPECIFICADO"),
    group_by_tipo_uci = FALSE,
    tipo_sector = tipo_sector,
    group_by_tipo_sector = group_by_tipo_sector,
    defunciones = defunciones,
    edad_cut = edad_cut,
    as_tibble = TRUE,
    fill_zeros = fill_NA,
    list_name = name_1,
    .grouping_vars = c()
  )[[name_1]]

  if (!quiet) {
    cli::cli_alert("Calculando los casos hospitalizados")
  }
  .casos_hospitalizados <- casos(
    datos_covid = datos_covid,
    entidades = entidades,
    group_by_entidad = group_by_entidad,
    entidad_tipo = entidad_tipo,
    fecha_tipo = fecha_tipo,
    tipo_clasificacion = tipo_clasificacion,
    group_by_tipo_clasificacion = group_by_tipo_clasificacion,
    tipo_paciente = "HOSPITALIZADO",
    group_by_tipo_paciente = FALSE,
    tipo_uci = c("SI", "NO", "NO APLICA", "SE IGNORA", "NO ESPECIFICADO"),
    group_by_tipo_uci = FALSE,
    tipo_sector = tipo_sector,
    group_by_tipo_sector = group_by_tipo_sector,
    defunciones = defunciones,
    edad_cut = edad_cut,
    fill_zeros = TRUE,
    list_name = name_2,
    .grouping_vars = c()
  )[[name_2]]

  if (!quiet) {
    cli::cli_alert("Calculando el chr")
  }
  .casos_totales <- .casos_totales |>
    dplyr::left_join(.casos_hospitalizados |>
      dplyr::rename(!!as.symbol("h") := !!as.symbol("n")),
    by = colnames(.casos_totales)[which(colnames(.casos_totales) != "n")]
    ) |>
    dplyr::mutate(!!as.symbol("CASE HOSPITALIZATION RATE") :=
      dplyr::if_else(!is.na(!!as.symbol("n")),
        as.numeric(!!as.symbol("h")) / as.numeric(!!as.symbol("n")),
        NA_real_
      )) |>
    dplyr::select(-!!as.symbol("n"), -!!as.symbol("h"))


  .casos_totales <- list(.casos_totales)
  names(.casos_totales) <- list_name

  return(append(datos_covid, .casos_totales))
}
