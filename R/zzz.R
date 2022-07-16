# Adaptado de
# https://github.com/AlbertoAlmuinha/bayesmodels/blob/master/R/zzz.R
.onAttach <- function(libname, pkgname) {
  bsu_color_1 <- "#2c3e50"
  bsu_color_2 <- "#1f78b4"

  # Check Theme: If Dark, Update Colors
  if (requireNamespace("rstudioapi", quietly = TRUE) & requireNamespace("crayon", quietly = TRUE)) {
    tryCatch(
      {
        if (rstudioapi::isAvailable()) {
          theme <- rstudioapi::getThemeInfo()
          if (is.null(theme)) {
            bsu_color_1 <- "#2c3e50"
            bsu_color_2 <- "#1f78b4"
          }
          if (theme$dark) {
            bsu_color_1 <- "#7FD2FF"
            bsu_color_2 <- "#18bc9c"
          }
        }
      },
      error = function(e) {
        bsu_color_1 <- "#2c3e50"
        bsu_color_2 <- "#1f78b4"
      },
      finally = {
        bsu_title <- crayon::make_style(bsu_color_1)
        bsu_main <- crayon::make_style(bsu_color_2)

        msg <- paste0(
          bsu_main("Hola! si me usas no te olvides de citarme:\n\n"),
          bsu_title("Paquete covidmx:\n"),
          bsu_main("+ https://rodrigozepeda.github.io/covidmx/authors.html#citation\n\n"),
          bsu_title("Datos abiertos (casos):\n"),
          bsu_main("+ https://www.gob.mx/salud/documentos/datos-abiertos-152127\n\n"),
          bsu_title("Datos de variantes (GISAID):\n"),
          bsu_main("+ https://www.gisaid.org/help/publish-with-data-from-gisaid/\n"),
          bsu_main("+ https://github.com/RodrigoZepeda/VariantesCovid\n\n"),
          bsu_title("Datos de ocupacion hospitalaria (Red IRAG):\n"),
          bsu_main("+ https://www.gits.igg.unam.mx/red-irag-dashboard/reviewHome#\n"),
          bsu_main("+ https://github.com/RodrigoZepeda/CapacidadHospitalariaMX\n\n")
        )
      }
    )
  } else {
    msg <- paste0(
      paste0("Hola! si me usas no te olvides de citarme:\n\n"),
      paste0("Paquete covidmx:\n"),
      paste0("+ https://rodrigozepeda.github.io/covidmx/authors.html#citation\n\n"),
      paste0("Datos abiertos (casos):\n"),
      paste0("+ https://www.gob.mx/salud/documentos/datos-abiertos-152127\n\n"),
      paste0("Datos de variantes (GISAID):\n"),
      paste0("+ https://www.gisaid.org/help/publish-with-data-from-gisaid/\n"),
      paste0("+ https://github.com/RodrigoZepeda/VariantesCovid\n\n"),
      paste0("Datos de ocupacion hospitalaria (Red IRAG):\n"),
      paste0("+ https://www.gits.igg.unam.mx/red-irag-dashboard/reviewHome#\n"),
      paste0("+ https://github.com/RodrigoZepeda/CapacidadHospitalariaMX\n\n")
    )
  }
  packageStartupMessage(msg)
}
