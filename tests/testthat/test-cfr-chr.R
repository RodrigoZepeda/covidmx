test_that("Test cfr and chr", {
  
  #Leemos los datos
  datos_covid <- covidmx::datosabiertos
  
  #Error de lista
  cfr_data <- cfr(datos_covid, quiet = TRUE)
  expect_error(cfr(cfr_data))
  
  #Error de lista
  chr_data <- chr(datos_covid, quiet = TRUE)
  expect_error(chr(chr_data))
  
  #Mensaje
  expect_message(chr(datos_covid))
  expect_message(cfr(datos_covid))
  
})
