% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cfr.R
\name{cfr}
\alias{cfr}
\title{Case Fatality Rate (CFR)}
\usage{
cfr(
  datos_covid,
  entidades = c("AGUASCALIENTES", "BAJA CALIFORNIA", "BAJA CALIFORNIA SUR", "CAMPECHE",
    "CHIAPAS", "CHIHUAHUA", "CIUDAD DE MÉXICO", "COAHUILA DE ZARAGOZA", "COLIMA",
    "DURANGO", "GUANAJUATO", "GUERRERO", "HIDALGO", "JALISCO", "MÉXICO",
    "MICHOACÁN DE OCAMPO", "MORELOS", "NAYARIT", "NUEVO LEÓN", "OAXACA", "PUEBLA",
    "QUERÉTARO", "QUINTANA ROO", "SAN LUIS POTOSÍ", "SINALOA", "SONORA", "TABASCO",
    "TAMAULIPAS", "TLAXCALA", "VERACRUZ DE IGNACIO DE LA LLAVE", "YUCATÁN", "ZACATECAS"),
  group_by_entidad = TRUE,
  entidad_tipo = c("Unidad Medica", "Residencia", "Nacimiento"),
  fecha_tipo = c("Sintomas", "Ingreso", "Defuncion"),
  tipo_uci = c("SI", "NO", "NO APLICA", "SE IGNORA", "NO ESPECIFICADO"),
  group_by_tipo_uci = FALSE,
  tipo_clasificacion = c("Confirmados COVID"),
  group_by_tipo_clasificacion = FALSE,
  tipo_paciente = c("AMBULATORIO", "HOSPITALIZADO", "NO ESPECIFICADO"),
  group_by_tipo_paciente = FALSE,
  tipo_sector = c("CRUZ ROJA", "DIF", "ESTATAL", "IMSS", "IMSS-BIENESTAR", "ISSSTE",
    "MUNICIPAL", "PEMEX", "PRIVADA", "SEDENA", "SEMAR", "SSA", "UNIVERSITARIO",
    "NO ESPECIFICADO"),
  group_by_tipo_sector = FALSE,
  edad_cut = NULL,
  fill_NA = TRUE,
  quiet = FALSE,
  list_name = "case fatality rate",
  .grouping_vars = c()
)
}
\arguments{
\item{datos_covid}{If no data is available it automatically downloads COVID-19
information.}

\item{entidades}{Vector con las entidades de las unidades medicas a analizar.
Opciones: \code{AGUASCALIENTES}, \verb{BAJA CALIFORNIA}, \verb{BAJA CALIFORNIA SUR},
\code{CAMPECHE}, \code{CHIAPAS}, \code{CHIHUAHUA}, \verb{CIUDAD DE MEXICO},
\verb{COAHUILA DE ZARAGOZA} , \code{COLIMA}, \code{DURANGO}, \code{GUANAJUATO}, \code{GUERRERO},
\code{HIDALGO}, \code{JALISCO}, \code{MEXICO}, \verb{MICHOACAN DE OCAMPO}, \code{MORELOS},\code{NAYARIT}
\verb{NUEVO LEON}, \code{OAXACA} ,\code{PUEBLA}, \code{QUERETARO},\verb{QUINTANA ROO},
\verb{SAN LUIS POTOSI}, \code{SINALOA}, \code{SONORA}, \code{TABASCO}, \code{TAMAULIPAS},\code{TLAXCALA},
\verb{VERACRUZ DE IGNACIO DE LA LLAVE}, \code{YUCATAN}, \code{ZACATECAS}}

\item{group_by_entidad}{Si junta las entidades en una sola
o bien las muestra por separado sin agrupar.}

\item{entidad_tipo}{Selecciona \verb{Unidad Medica}, \code{Nacimiento} o \code{Residencia}.
por default incluye \verb{Unidad Medica}}

\item{fecha_tipo}{Selecciona \code{Ingreso}, \code{Sintomas} o \code{Defuncion} por default
incluye fecha de \code{Sintomas}}

\item{tipo_uci}{Vector con el tipo de valores para Unidad de Cuidado Intensivo a incluir:
\code{SI},\code{NO},\verb{NO APLICA},\verb{SE IGNORA},\verb{NO ESPECIFICADO}}

\item{group_by_tipo_uci}{Boolean determinando si regresa la base
con cada entrada agrupada por tipo de uci (es decir cada fecha
se generan tantos observaciones como grupos de tipo de uci)}

\item{tipo_clasificacion}{Vector con el tipo de clasificaciones a incluir:
\code{Sospechosos},\verb{Confirmados COVID}, \verb{Negativo a COVID},
\verb{Inv\\u00e1lido}, \verb{No realizado}}

\item{group_by_tipo_clasificacion}{Boolean determinando si regresa la base
con cada entrada agrupada por tipo de clasificación (es decir cada fecha
se generan tantos observaciones como grupos de tipo de clasificación)}

\item{tipo_paciente}{Vector con el tipo de pacientes a incluir. Opciones:
\code{AMBULATORIO}, \code{HOSPITALIZADO}, \verb{NO ESPECIFICADO}}

\item{group_by_tipo_paciente}{Boolean determinando si regresa la base
con cada entrada agrupada por tipo de paciente (es decir cada fecha
se generan tantos observaciones como grupos de tipo de paciente)}

\item{tipo_sector}{Vector con los sectores del sistema de salud a incluir:
\verb{CRUZ ROJA},\code{DIF},\code{ESTATAL},\code{IMSS},\code{IMSS-BIENESTAR},\code{ISSSTE}, \code{MUNICIPAL},\code{PEMEX},
\code{PRIVADA},\code{SEDENA},\code{SEMAR},\code{SSA}, \code{UNIVERSITARIO},\verb{NO ESPECIFICADO}.}

\item{group_by_tipo_sector}{Boolean determinando si regresa la base
con cada entrada agrupada por tipo de sector (es decir cada fecha
se generan tantos observaciones como grupos de tipo de sector)}

\item{edad_cut}{Vector con secuencia de edades para hacer grupos. Por ejemplo
\code{edad_cut = c(0, 10, Inf)} arma dos grupos de edad de 0 a 10 y de 10 a infinito o bien
\code{edad_cut = c(15, 20)} deja sólo los registros entre 15 y 20 años. Por default es NULL}

\item{fill_NA}{Regresa observaciones
para todas las combinaciones de variables incluyendo como NA donde no se observaron casos
y por tanto el denominador y el \code{chr} es indefinido.}

\item{quiet}{No arroja ningun mensaje}

\item{list_name}{Asigna un nombre en la lista de datos a la base generada}

\item{.grouping_vars}{Vector de variables adicionales de agrupacion de los conteos}
}
\value{
Appends a la lista de \code{datos_covid} una nueva entrada de nombre \code{list_name}
(default: \code{casos}) con una base de datos (\code{tibble} o \code{dbConnection}) con los
resultados agregados.
\itemize{
\item \verb{case hospitalization rate} - Base de datos generara con los datos agregados (el nombre cambia si
se usa \code{list_name}).
\item dict - Diccionario de datos
\item dats - Datos originales (conexion a DB)
\item disconnect  - Función para desconectarte de DB
}
}
\description{
\code{chr} Calcula la proporción de enfermos que fallecen sobre todos los enfermos confirmados
en distintas categorías (residencia / edad / etc)
}
\details{
This is not an official product / este no es un producto oficial
}
\examples{
\dontrun{
datos_covid <- descarga_datos_abiertos(language = "Espanol")

# Casos a nivel nacional
datos_covid <- datos_covid \%>\% cfr()
head(datos_covid$`case fatality rate`)

# Nacional
datos_covid <- datos_covid \%>\% cfr(list_name = "cfr_nacional", group_by_entidad = F)

# CFR en Jalisco y Colima
datos_covid <- datos_covid \%>\%
  cfr(entidades = c("JALISCO", "COLIMA"), list_name = "cfr_Jaliscolima")

# Calcula el CFR suponiendo toda la base son confirmados
datos_covid <- datos_covid \%>\%
  cfr(
    entidades = c("JALISCO", "COLIMA"),
    tipo_clasificacion = c(
      "Sospechosos", "Confirmados COVID",
      "Negativo a COVID", "Inv\u00e1lido", "No realizado"
    ),
    group_by_tipo_clasificacion = TRUE, list_name = "Jaliscolima2_cfr"
  )

# Distinguiendo entre ambulatorio y hospitalizado
datos_covid <- datos_covid \%>\%
  casos(
    entidades = c("JALISCO", "COLIMA"),
    tipo_paciente = c("AMBULATORIO", "HOSPITALIZADO"),
    group_by_tipo_paciente = TRUE,
    list_name = "Jalisco + colima cfr"
  )

# CFR en ambulatorios y hospitalizados
datos_covid \%>\%
  cfr(
    tipo_paciente = c("AMBULATORIO", "HOSPITALIZADO"),
    group_by_tipo_paciente = TRUE,
    group_by_entidad = FALSE,
    list_name = "CFR_hosp_amb"
  ) \%>\%
  plot_covid(df_name = "CFR_hosp_amb", type = "line", facet_ncol = 3)

# Si deseas agrupar por una variable que no este en las opciones
datos_covid <- datos_covid \%>\%
  cfr(.grouping_vars = c("DIABETES"), list_name = "cfr_diab")
}

}
