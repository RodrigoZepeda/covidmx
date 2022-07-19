test_that("Lectura y descarga datos abiertos", {

  skip_if_offline()
  setwd(tempdir())
  
  #Checamos que sí se descarguen cosas
  dlink    <- "https://github.com/RodrigoZepeda/covidmx/raw/main/datos_abiertos_covid19.zip"
  download <- descarga_datos_abiertos(cache_datos = tempfile(), cache_diccionario = tempfile(),
                                      site.covid = dlink, force_download = TRUE,
                                      show_warnings = TRUE, read_format = "tibble", quiet = TRUE)
  expect_length(download, 3)

  #Paso a paso descarga de datos
  download <- descarga_db_datos_abiertos_tbl(cache =  tempfile(), site.covid = dlink,
                                             force_download = TRUE, show_warnings = TRUE,
                                             quiet = TRUE)
  expect_true(file.exists(download))
  expect_true(tools::file_ext(download) == "zip")

  #Lectura desde el zip v1
  csv_dssa <- unzip_db_datos_abiertos_tbl(download)
  expect_true(file.exists(csv_dssa))
  expect_true(tools::file_ext(csv_dssa) == "csv")

  #Lectura desde el zip v2
  datos_covid <- read_datos_abiertos_zip(download, read_format = "tibble", show_warnings = FALSE,
                                         cache_datos = tempfile(), force_download = TRUE, quiet = TRUE,
                                         cache_diccionario = tempfile(), clear_csv = FALSE)
  expect_true(nrow(datos_covid$dats) > 0)

  #Lectura desde el zip v3
  datos_covid <- read_datos_abiertos(download, read_format = "tibble", show_warnings = FALSE,
                                     cache_datos = tempfile(), force_download = TRUE, quiet = TRUE,
                                     cache_diccionario = tempfile(), clear_zip = TRUE,
                                     clear_csv = FALSE)
  expect_true(nrow(datos_covid$dats) > 0)

  #Lectura desde el csv v1------
  datos_abiertos <- parse_db_datos_abiertos_tbl(csv_dssa, read_format = "tibble",
                                                clear_csv = FALSE)
  expect_true(nrow(datos_covid$dats) > 0)

  #Lectura desde el csv v2------
  datos_covid <- read_datos_abiertos_csv(csv_dssa, read_format = "tibble", show_warnings = FALSE,
                                         cache_datos = tempfile(), force_download = TRUE,
                                         clear_csv = FALSE,
                                         quiet = TRUE, cache_diccionario = tempfile())
  expect_true(nrow(datos_covid$dats) > 0)

  #Lectura desde el csv v3------
  datos_covid <- read_datos_abiertos(csv_dssa, read_format = "tibble", show_warnings = FALSE,
                                     cache_datos = tempfile(), force_download = TRUE,
                                     clear_csv = FALSE,
                                     quiet = TRUE, cache_diccionario = tempfile())
  expect_true(nrow(datos_covid$dats) > 0)

  #Checamos que se pueda leer y limpiar la memoria de los temporales
  datos_covid <- descarga_db(read_format = "tibble", show_warnings = FALSE, site.covid = dlink,
                             cache = tempfile(), force_download = TRUE, quiet = TRUE,
                             clear_zip = TRUE, clear_csv = TRUE)
  expect_true(nrow(datos_covid$dats) > 0)

  #Descarga de datos covid como tibble
  dlink       <- "https://github.com/RodrigoZepeda/covidmx/raw/main/datos_abiertos_covid19.zip"
  datos_covid <- descarga_db(read_format = "tibble", site.covid = dlink, tblname = "tutorial",
                             quiet = TRUE, show_warnings = F, force_download = T, cache = tempfile())
  expect_gt(as.numeric(dplyr::collect(dplyr::tally(datos_covid$dats))[[1]]), 0)
  
  #Descarga de diccionario
  skip_if(!RCurl::url.exists(eval(formals(descarga_diccionario)$site.covid.dic)))
  diccionario <- descarga_diccionario(quiet = TRUE, force_download = T, show_warnings = F,
                                      cache_diccionario = tempfile())
  
  #Descarga de sitio que no es
  expect_error(descarga_db(quiet = TRUE, force_download = T, show_warnings = F,
                           site.covid = "ahsfiugow"))
  
})
