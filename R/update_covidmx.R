#' Actualiza el paquete covidmx
#'
#' @description Descarga e instala la version mas reciente de covidmx desde Github
#' \url{https://github.com/RodrigoZepeda/covidmx}
#'
#' @param quiet (**opcional**) Determina si instalar en silencio
#' @param force (**opcional**) Determina si forzar la reinstalacion
#' @param ...   (**opcional**) Parametros adicionales para [remotes::install_github()]
#'
#' @examples
#' \dontrun{
#' # Actualiza el paquete de coivdmx
#' update_covidmx()
#' }
#'
#' @note
#' Actualiza el paquete instalando todas las dependencias necesarias.
#' @export

update_covidmx <- function(quiet = FALSE, force = FALSE, ...) {
  if (requireNamespace("remotes", quietly = TRUE)) {

    # Verificamos la url exista
    url <- "https://github.com/RodrigoZepeda/covidmx"

    if (!RCurl::url.exists(url)) {
      cli::cli_abort(
        "No puedo encontrar el repositorio de Github en {.url {url}}"
      )
    }

    # Instalaciom
    remotes::install_github("RodrigoZepeda/covidmx", quiet = quiet, force = force, ...)
  } else {

    # Sugerimos instalar
    cli::cli_alert_warning(
      c(
        "Instala el paquete {.code remotes} con {.code install.packages('remotes')} para poder",
        " utilizar esta funcion"
      )
    )
  }
}
