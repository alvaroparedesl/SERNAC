% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/agregar.R
\encoding{UTF-8}
\name{agregar_datos}
\alias{agregar_datos}
\title{Agregar/resumir por variable(s)}
\usage{
agregar_datos(
  df,
  bycols = c("proveedor_rut", "proveedor_mercado_nombre"),
  timep = "mensual",
  prep = FALSE,
  prepIgnore = NULL,
  fillN = TRUE,
  usar_tiempo = TRUE
)
}
\arguments{
\item{df}{data.table data.frame}

\item{bycols}{columnas que agruparán}

\item{timep}{tiempo para hacer la agrupación; mensual o semanal}

\item{prep}{entrega una tabla lista para ser apilada con otras, sin perder información.
Sólo si \code{setdiff(bycols, prepIgnore)} tiene largo igual a 1.}

\item{prepIgnore}{columna de agrupación que ignorar al preparar los datos (columna que no será transformada a formato largo y se mantendrá como ancha).}

\item{fillN}{cambia NAs por 0 (sólo en N, no en diff).}

\item{usar_tiempo}{si es TRUE, se usa la diferencia de tiempo como una de las variables para buscar valores atípicos. Por defecto es TRUE.}
}
\value{
un data.frame de data.table ordenado por tiempo, para llegar y aplicar una serie temporal (filtrando previamente).
}
\description{
Agrupa reclamos por fecha, de forma mensual o semanal. Retorno los tiempos de respuesta entre reclamos, y el número de reclamos
contabilizados para esa agrupación temporal, con diferencias lagueadas definidas por el usuario. También agrupa por variable, dependiendo de
lo que defina el usuario.
}
\examples{
1+1
}
