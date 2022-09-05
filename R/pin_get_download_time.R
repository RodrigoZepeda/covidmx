# Import from pins the meta
pin_meta.pins_board_url <- utils::getFromNamespace("pin_meta.pins_board_url", "pins")

# Checks a pin's last download date
pin_get_download_time <- function(board, name) {

  # If hasn't been downloaded
  tdif <- Inf

  if (name %in% pins::pin_list(board)) {
    fdir <- pin_meta.pins_board_url(board, name)
    pd   <- base::file.path(fdir$local$dir, "descarga.txt")

    if (file.exists(pd)) {
      # Leemos cuando fue la descarga
      fileConn <- file(pd)
      descarga <- base::as.POSIXlt(base::readLines(fileConn, n = 1))
      close(fileConn)

      # Obtenemos la diferencia de tiempo
      tdif <- base::difftime(base::as.POSIXlt(descarga), base::Sys.time(), units = "days")
      tdif <- base::abs(base::as.numeric(tdif))
    }
  }

  return(tdif)
}

# Función que devuelve el path al archivo principal de un pin
pin_path_from_memory <- function(board, name) {
  fdir  <- pin_meta.pins_board_url(board, name)
  dfile <- base::file.path(fdir$local$dir, fdir$file)

  return(dfile)
}

# Función que escribe la fecha de la ultima descarga del pin
pin_write_download_time <- function(board, name) {
  if (name %in% pins::pin_list(board)) {
    pd <- base::file.path(pin_meta.pins_board_url(board, name)$local$dir, "descarga.txt")

    # Leemos el archivo y agregamos en la ultima linea la fecha de descarga
    fileConn <- base::file(pd)
    base::writeLines(format(Sys.time(), "%F %R %Z"), fileConn)
    base::close(fileConn)
  }
}

get_col_class <- function() {
  c(
    FECHA_ACTUALIZACION = "character",
    ID_REGISTRO = "character",
    ORIGEN = "character",
    SECTOR = "character",
    ENTIDAD_UM = "character",
    SEXO = "character",
    ENTIDAD_NAC = "character",
    ENTIDAD_RES = "character",
    MUNICIPIO_RES = "character",
    TIPO_PACIENTE = "character",
    FECHA_INGRESO = "character",
    FECHA_SINTOMAS = "character",
    FECHA_DEF = "character",
    INTUBADO = "character",
    NEUMONIA = "character",
    EDAD = "character",
    NACIONALIDAD = "character",
    EMBARAZO = "character",
    HABLA_LENGUA_INDIG = "character",
    INDIGENA = "character",
    DIABETES = "character",
    EPOC = "character",
    ASMA = "character",
    INMUSUPR = "character",
    HIPERTENSION = "character",
    OTRA_COM = "character",
    CARDIOVASCULAR = "character",
    OBESIDAD = "character",
    RENAL_CRONICA = "character",
    TABAQUISMO = "character",
    OTRO_CASO = "character",
    TOMA_MUESTRA_LAB = "character",
    RESULTADO_LAB = "character",
    TOMA_MUESTRA_ANTIGENO = "character",
    RESULTADO_ANTIGENO = "character",
    CLASIFICACION_FINAL = "character",
    MIGRANTE = "character",
    PAIS_NACIONALIDAD = "character",
    PAIS_ORIGEN = "character",
    UCI = "character"
  )
}
