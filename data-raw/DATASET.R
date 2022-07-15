library(covidmx)
library(tidyverse)
library(lubridate)
covid      <- descarga_datos_abiertos(force_download = TRUE)

covid$dats <- covid$dats %>%
  filter(ENTIDAD_UM %in% c("02", "03"))

covid$dats   <- covid$dats %>% collect()
covid$dats   <- covid$dats %>%
  filter(year(FECHA_SINTOMAS) > 2020) %>%
  filter(FECHA_SINTOMAS >= ymd("2021/07/01") & FECHA_SINTOMAS <= ymd("2021/09/01"))

covid$disconnect()
datosabiertos <- covid
datosabiertos$disconnect <- function(){TRUE}

usethis::use_data(datosabiertos, overwrite = TRUE, version = 3)
datosabiertos$dats %>% write_excel_csv("data-raw/pruebaCOVID19MEXICO.csv")
zip("data-raw/datos_abiertos_covid19.zip","data-raw/pruebaCOVID19MEXICO.csv")
file.remove("data-raw/pruebaCOVID19MEXICO.csv")
