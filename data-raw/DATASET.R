library(covidmx)
library(tidyverse)
library(lubridate)

#Leemos los datos
covid      <- descarga_datos_abiertos(force_download = TRUE)

#Filtramos BC y BCS para el ejemplo
covid$dats <- covid$dats %>%
  filter(ENTIDAD_UM %in% c("02", "03"))

#Obtenemos la info
covid$dats   <- covid$dats %>% collect()

#Limpiamos fecha de sintomas
covid$dats   <- covid$dats %>%
  filter(year(FECHA_SINTOMAS) > 2020) %>%
  filter(FECHA_SINTOMAS >= ymd("2021/07/01") & FECHA_SINTOMAS <= ymd("2021/09/01"))

covid$disconnect()

datosabiertos <- covid
datosabiertos$disconnect <- function(){TRUE}

#Creamos los datos del paquete
usethis::use_data(datosabiertos, overwrite = TRUE, version = 3)

#Almacenamos para generar el zip de testing
datosabiertos$dats %>% write_excel_csv("pruebaCOVID19MEXICO.csv")
zip("datos_abiertos_covid19.zip","pruebaCOVID19MEXICO.csv")

#Reacomodamos todo el panorama
file.remove("pruebaCOVID19MEXICO.csv")
file.copy("datos_abiertos_covid19.zip", "data-raw/datos_abiertos_covid19.zip")
file.remove("datos_abiertos_covid19.zip")
