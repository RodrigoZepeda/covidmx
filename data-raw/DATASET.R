library(covidmx)
library(tidyverse)
library(lubridate)
library(stringi)

# Leemos los datos
covid <- descarga_datos_abiertos("patricio.duckdb")
covid <- read_datos_abiertos("patricio.duckdb")

# Obtenemos la info
covid$dats <- covid$dats %>% collect()

# Filtramos BC y BCS para el ejemplo
covid$dats <- covid$dats %>%
  filter(ENTIDAD_UM %in% c("02", "03"))

# Limpiamos fecha de sintomas
covid$dats <- covid$dats %>%
  filter(year(FECHA_SINTOMAS) > 2020) %>%
  filter(FECHA_SINTOMAS >= ymd("2021/07/01") & FECHA_SINTOMAS <= ymd("2021/08/01"))


covid$dats <- covid$dats %>%
  select(
    -FECHA_ACTUALIZACION, -ID_REGISTRO, -ORIGEN, -INDIGENA, -NEUMONIA, -c(EPOC:OTRO_CASO),
    -MIGRANTE, -PAIS_NACIONALIDAD, -PAIS_ORIGEN, -MUNICIPIO_RES, -INTUBADO, -NACIONALIDAD,
    -EMBARAZO
  )

covid$dats <- covid$dats |> sample_n(20000)

covid$disconnect()

datosabiertos <- covid
datosabiertos$disconnect <- function(quiet = FALSE) {
  if (!quiet) {
    cli::cli_alert_success("Desconectado")
  }
}

for (listname in names(datosabiertos$dict)) {
  message(listname)
  datosabiertos$dict[[listname]] <-
    datosabiertos$dict[[listname]] |>
    mutate_if(is.character, ~ stri_enc_toascii(.))
  colnames(datosabiertos$dict[[listname]]) <-
    stri_enc_toascii(colnames(datosabiertos$dict[[listname]]))
}

for (listname in names(datosabiertos$dict)) {
  if (!(listname %in% c(colnames(datosabiertos$dats), "PACIENTE"))) {
    datosabiertos$dict <- datosabiertos$dict[names(datosabiertos$dict) != listname]
  }
}


datosabiertos$dats <- datosabiertos$dats |>
  mutate_if(is.character, ~ stri_enc_toascii(.))

colnames(datosabiertos$dats) <- stri_enc_toascii(colnames(datosabiertos$dats))

# Creamos los datos del paquete
usethis::use_data(datosabiertos, overwrite = TRUE, version = 3, ascii = TRUE)

# Almacenamos para generar el zip de testing
datosabiertos$dats %>% write_excel_csv("pruebaCOVID19MEXICO.csv")
zip("datos_abiertos_covid19.zip", "pruebaCOVID19MEXICO.csv")

# Reacomodamos todo el panorama
file.remove("pruebaCOVID19MEXICO.csv")
