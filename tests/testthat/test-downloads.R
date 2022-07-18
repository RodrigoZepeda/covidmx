test_that("Download processes", {
  #Estos teste verifican que las bases descargadas tengan numero de filas > 0
  skip_if_offline()

  #Descarga de datos de red irag
  redirag     <- descarga_datos_red_irag("nacional", quiet = T, show_warnings = F)
  expect_gt(nrow(redirag), 0)

  variantes   <- descarga_datos_variantes_GISAID("nacional", quiet = T, show_warnings = F)
  expect_gt(nrow(variantes), 0)

  #Descarga de datos de variantes NACIONAL y CDMX
  variantes   <- descarga_datos_variantes_GISAID("nacional", quiet = T, show_warnings = F)
  expect_gt(nrow(variantes), 0)

  variantes   <- descarga_datos_variantes_GISAID("cdmx", quiet = T, show_warnings = F)
  expect_gt(nrow(variantes), 0)

  #Descarga de datos covid como tibble
  dlink       <- "https://github.com/RodrigoZepeda/covidmx/raw/main/datos_abiertos_covid19.zip"
  datos_covid <- descarga_db(read_format = "tibble", site.covid = dlink, tblname = "tutorial",
                             quiet = TRUE, show_warnings = F, force_download = T)
  expect_gt(as.numeric(dplyr::collect(dplyr::tally(datos_covid$dats))[[1]]), 0)

  #Descarga de diccionario
  skip_if(!RCurl::url.exists(eval(formals(descarga_diccionario)$site.covid.dic)))
  diccionario <- descarga_diccionario(quiet = TRUE, force_download = T, show_warnings = F)

  #Descarga de datos covid como MariaDB
  skip_if(!RMariaDB::mariadbHasDefault())
  dlink       <- "https://github.com/RodrigoZepeda/covidmx/raw/main/datos_abiertos_covid19.zip"
  datos_covid <- descarga_db(read_format = "MariaDB", site.covid = dlink, tblname = "tutorial",
                             quiet = TRUE, show_warnings = F, force_download = T)
  expect_gt(as.numeric(dplyr::collect(dplyr::tally(datos_covid$dats))[[1]]), 0)

})
