test_that("Casos", {

  #Leemos los datos
  datos_covid <- covidmx::datosabiertos

  #Vemos que la agrupacion coincida con la "normal"
  casos_agrupados <- datos_covid %>%
    casos(fill_zeros = FALSE)

  #Agrupamos usando dplyr clásico
  casos_agrupados_al_natural <- datos_covid$dats %>%
    dplyr::group_by_at("FECHA_SINTOMAS") %>%
    dplyr::group_by_at("ENTIDAD_UM", .add = TRUE) %>%
    dplyr::tally() %>%
    dplyr::left_join(datos_covid$dict$ENTIDAD_UM, by = c("ENTIDAD_UM" = "CLAVE_ENTIDAD"))

  #Verificamos igualdad
  expect_true(dplyr::all_equal(casos_agrupados$casos, casos_agrupados_al_natural))


})
