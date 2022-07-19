test_that("Maria DB", {
  
  setwd(tempdir())
  
  #Check we are willing to run tests they add a new dataset to mariadb
  skip_if_not(Sys.getenv("MariaDB_test_covidmx") == "TRUE" & RMariaDB::mariadbHasDefault(),
              message = 'Set Sys.setenv(MariaDB_test_covidmx="TRUE") tu run MARIADB tests')
  
  #Descarga con mariadb
  dlink       <- "https://github.com/RodrigoZepeda/covidmx/raw/main/datos_abiertos_covid19.zip"
  datos_covid <- descarga_db(read_format = "MariaDB", site.covid = dlink, tblname = "test",
                             quiet = TRUE, show_warnings = F, force_download = T, cache = tempfile())
  expect_gt(as.numeric(dplyr::collect(dplyr::tally(datos_covid$dats))[[1]]), 0)
  datos_covid$disconnect()
  
  #Descarga con mariadb
  dlink       <- "https://github.com/RodrigoZepeda/covidmx/raw/main/datos_abiertos_covid19.zip"
  datos_covid <- descarga_datos_abiertos(read_format = "MariaDB", site.covid = dlink, tblname = "test",
                             quiet = TRUE, show_warnings = F, force_download = T, 
                             cache_datos  = tempfile(), cache_diccionario = tempfile())
  expect_gt(as.numeric(dplyr::collect(dplyr::tally(datos_covid$dats))[[1]]), 0)
  datos_covid$disconnect()
  
  #Lectura con mariadb
  dlink       <- "https://github.com/RodrigoZepeda/covidmx/raw/main/datos_abiertos_covid19.zip"
  datos_covid <- read_datos_abiertos(read_format = "MariaDB", tblname = "test", quiet = TRUE, 
                                     show_warnings = F, force_download = T, cache_diccionario = tempfile())
  expect_gt(as.numeric(dplyr::collect(dplyr::tally(datos_covid$dats))[[1]]), 0)
  datos_covid$disconnect()
  
  
  
})
