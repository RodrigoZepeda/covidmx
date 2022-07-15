library(covidmx)

covid      <- read_datos_abiertos()
covid$dats <- covid$dats %>%
  filter(FECHA_SINTOMAS >= as.Date("2020/05/01") & FECHA_SINTOMAS < as.Date("2020/07/01")) %>%
  filter(ENTIDAD_UM %in% c(18, 19))

covid$dats   <- covid$dats %>% collect()

datosabiertos <- covid
datosabiertos$disconnect <- function(){TRUE}

usethis::use_data(datosabiertos, overwrite = TRUE, version = 3)
datosabiertos$dats %>% write_excel_csv("pruebaCOVID19MEXICO.csv")
