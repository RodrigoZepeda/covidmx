% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/numero_pruebas.R
\name{numero_pruebas}
\alias{numero_pruebas}
\title{Numero de Pruebas}
\usage{
numero_pruebas(
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
  tipo_prueba = c("Antigeno", "PCR"),
  group_by_tipo_prueba = TRUE,
  tipo_paciente = c("AMBULATORIO", "HOSPITALIZADO", "NO ESPECIFICADO"),
  group_by_tipo_paciente = FALSE,
  tipo_uci = c("SI", "NO", "NO APLICA", "SE IGNORA", "NO ESPECIFICADO"),
  group_by_tipo_uci = FALSE,
  tipo_sector = c("CRUZ ROJA", "DIF", "ESTATAL", "IMSS", "IMSS-BIENESTAR", "ISSSTE",
    "MUNICIPAL", "PEMEX", "PRIVADA", "SEDENA", "SEMAR", "SSA", "UNIVERSITARIO",
    "NO ESPECIFICADO"),
  group_by_tipo_sector = FALSE,
  defunciones = FALSE,
  edad_cut = NULL,
  as_tibble = TRUE,
  fill_zeros = as_tibble,
  list_name = "numero_pruebas",
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

\item{tipo_prueba}{Vector con el tipo de pruebas a incluir
\code{Antigeno}, \code{PCR}}

\item{group_by_tipo_prueba}{Boolean determinando si regresa la base
con cada entrada agrupada por tipo de pureba (es decir cada fecha
y entidad reporta separado positividad en PCR y Antigeno)}

\item{tipo_paciente}{Vector con el tipo de pacientes a incluir. Opciones:
\code{AMBULATORIO}, \code{HOSPITALIZADO}, \verb{NO ESPECIFICADO}}

\item{group_by_tipo_paciente}{Boolean determinando si regresa la base
con cada entrada agrupada por tipo de paciente (es decir cada fecha
se generan tantos observaciones como grupos de tipo de paciente)}

\item{tipo_uci}{Vector con el tipo de valores para Unidad de Cuidado Intensivo a incluir:
\code{SI},\code{NO},\verb{NO APLICA},\verb{SE IGNORA},\verb{NO ESPECIFICADO}}

\item{group_by_tipo_uci}{Boolean determinando si regresa la base
con cada entrada agrupada por tipo de uci (es decir cada fecha
se generan tantos observaciones como grupos de tipo de uci)}

\item{tipo_sector}{Vector con los sectores del sistema de salud a incluir:
\verb{CRUZ ROJA},\code{DIF},\code{ESTATAL},\code{IMSS},\code{IMSS-BIENESTAR},\code{ISSSTE}, \code{MUNICIPAL},\code{PEMEX},
\code{PRIVADA},\code{SEDENA},\code{SEMAR},\code{SSA}, \code{UNIVERSITARIO},\verb{NO ESPECIFICADO}.}

\item{group_by_tipo_sector}{Boolean determinando si regresa la base
con cada entrada agrupada por tipo de sector (es decir cada fecha
se generan tantos observaciones como grupos de tipo de sector)}

\item{defunciones}{Boolean si incluir sólo defunciones \code{TRUE} o a todos
\code{FALSE}.}

\item{edad_cut}{Vector con secuencia de edades para hacer grupos. Por ejemplo
\code{edad_cut = c(0, 10, Inf)} arma dos grupos de edad de 0 a 10 y de 10 a infinito o bien
\code{edad_cut = c(15, 20)} deja sólo los registros entre 15 y 20 años. Por default es NULL}

\item{as_tibble}{Regresar como \code{tibble} el resultado. En caso de que \code{as_tibble}
sea \code{FALSE} se devuelve como conexion en \code{MARIADB}.}

\item{fill_zeros}{En caso de que el resultado sea un \code{tibble} regresa observaciones
para todas las combinaciones de variables incluyendo como 0 donde no se observaron casos. En
caso contrario no se incluyen las filas donde no se observaron casos.}

\item{list_name}{Asigna un nombre en la lista de datos a la base generada}

\item{.grouping_vars}{Vector de variables adicionales de agrupacion de los conteos}
}
\value{
Appends a la lista de \code{datos_covid} una nueva entrada de nombre \code{list_name}
(default: \code{casos}) con una base de datos (\code{tibble} o \code{dbConnection}) con los
resultados agregados.
\itemize{
\item positividad - Base de datos generara con los datos agregados (el nombre cambia si
se usa \code{list_name}).
\item dict - Diccionario de datos
\item dats - Datos originales (conexion a DB)
\item disconnect  - Función para desconectarte de DB
}
}
\description{
\code{numero_pruebas} Calcula el numero total de pruebas por fecha agrupando (o sin hacerlo)
por covariables.
}
\details{
Por default calcula la el numero de pruebas de Antígeno y PCR por estado y tipo
This is not an official product / este no es un producto oficial
}
\examples{
\dontrun{
datos_covid <- descarga_datos_abiertos(language = "Espanol")

# Número de pruebas PCR/ANTI a nivel nacional por estado
datos_covid <- datos_covid \%>\% numero_pruebas()
head(datos_covid$numero_pruebas)

# Número de pruebas nacionales pero sin separar por tipo ni estado
datos_covid <- datos_covid \%>\%
  numero_pruebas(
    group_by_entidad = FALSE, group_by_tipo_prueba = FALSE,
    list_name = "Todas_las_pruebas"
  )

# Positivos en Jalisco y Colima
casos_col_jal <- datos_covid \%>\%
  numero_pruebas(
    entidades = c("JALISCO", "COLIMA"),
    list_name = "Pruebas_jal_col"
  )

# Si deseas agrupar por una variable que no este en las opciones asi como tipo paciente
casos_col_jal <- datos_covid \%>\%
  numero_pruebas(
    entidades = c("JALISCO", "COLIMA"),
    tipo_paciente = c("AMBULATORIO", "HOSPITALIZADO"),
    group_by_tipo_paciente = TRUE,
    .grouping_vars = c("DIABETES"),
    list_name = "Diabetescoljal"
  )
}

}
