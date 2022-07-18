test_that("Positividad", {

  #Leemos los datos
  datos_covid <- covidmx::datosabiertos


  #Chequeo de que sólo lea el Baja California correcto-----
  positividad_agrupados <- datos_covid %>%
    positividad(entidades =  "BAJA CALIFORNIA", quiet = TRUE)
  entidades       <- unique(positividad_agrupados$positividad$ENTIDAD_FEDERATIVA)
  expect_true(length(entidades) == 1 & entidades == "BAJA CALIFORNIA")

  #Chequeo de que sólo lea el Baja California correcto-----
  positividad_agrupados <- datos_covid %>%
    positividad(entidades =  "BAJA CALIFORNIA SUR", quiet = TRUE)
  entidades       <- unique(positividad_agrupados$positividad$ENTIDAD_FEDERATIVA)
  expect_true(length(entidades) == 1 & entidades == "BAJA CALIFORNIA SUR")

  #Chequeo de que sólo lea el IMSS correcto 1----
  positividad_agrupados <- datos_covid %>%
    positividad(tipo_sector =  "IMSS", group_by_tipo_sector = TRUE, fill_NA = FALSE, quiet = TRUE)
  sectores       <- unique(positividad_agrupados$positividad$DESCRIPCION_TIPO_SECTOR)
  expect_true(length(sectores) == 1 & sectores == "IMSS")

  #Chequeo de que sólo lea el IMSS correcto 2----
  positividad_agrupados <- datos_covid %>%
    positividad(tipo_sector =  "IMSS-BIENESTAR", group_by_tipo_sector = TRUE,
                fill_NA = FALSE, quiet = TRUE)
  sectores       <- unique(positividad_agrupados$positividad$DESCRIPCION_TIPO_SECTOR)
  expect_true(length(sectores) == 1 & sectores == "IMSS-BIENESTAR")

  #Chequeo de que sólo lea el UCI correcto 1----
  positividad_agrupados <- datos_covid %>%
    positividad(tipo_uci =  "NO", group_by_tipo_uci = TRUE, fill_NA = FALSE, quiet = TRUE)
  ucis       <- unique(positividad_agrupados$positividad$DESCRIPCION_TIPO_UCI)
  expect_true(length(ucis) == 1 & ucis == "NO")

  #Chequeo de que sólo lea el UCI correcto 1----
  positividad_agrupados <- datos_covid %>%
    positividad(tipo_uci =  "NO ESPECIFICADO", group_by_tipo_uci = TRUE,
                fill_NA = FALSE, quiet = TRUE)
  ucis       <- unique(positividad_agrupados$positividad$DESCRIPCION_TIPO_UCI)
  expect_true(length(ucis) == 1 & ucis == "NO ESPECIFICADO")

  #Chequeo del list_name----
  positividad_prueba <- datos_covid %>% positividad(list_name = "Prueba", quiet = TRUE)
  expect_true("Prueba" %in% names(positividad_prueba))

})
