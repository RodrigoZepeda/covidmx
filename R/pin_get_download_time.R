#Import from pins the meta
pin_meta.pins_board_url <- utils::getFromNamespace("pin_meta.pins_board_url", "pins")

#Checks a pin's last download date
pin_get_download_time <- function(board, name){

  #If hasn't been downloaded
  tdif <- Inf

  if (name %in% pins::pin_list(board)){

    fdir <- pin_meta.pins_board_url(board, name)
    pd   <- file.path(fdir$local$dir, "descarga.txt")

    if (file.exists(pd)){
      #Leemos cuando fue la descarga
      fileConn <- file(pd)
      descarga <- as.POSIXlt(readLines(fileConn, n = 1))
      close(fileConn)

      #Obtenemos la diferencia de tiempo
      tdif <- difftime(as.POSIXlt(descarga), Sys.time(), units = "days")
      tdif <- abs(as.numeric(tdif))
    }
  }

  return(tdif)
}

#Función que devuelve el path al archivo principal de un pin
pin_path_from_memory <- function(board, name){
  fdir  <- pin_meta.pins_board_url(board, name)
  dfile <- file.path(fdir$local$dir, fdir$file)

  return(dfile)
}

#Función que escribe la fecha de la ultima descarga del pin
pin_write_download_time <- function(board, name){
  if (name %in% pins::pin_list(board)){
    pd <- file.path(pin_meta.pins_board_url(board, name)$local$dir, "descarga.txt")

    #Leemos el archivo y agregamos en la ultima linea la fecha de descarga
    fileConn <- file(pd)
    writeLines(format(Sys.time(), "%F %R %Z"), fileConn)
    close(fileConn)
  }
}
