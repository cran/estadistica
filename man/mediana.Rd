% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mediana.R
\name{mediana}
\alias{mediana}
\title{Mediana.}
\usage{
mediana(x, variable = NULL, pesos = NULL)
}
\arguments{
\item{x}{Conjunto de datos. Puede ser un vector o un dataframe.}

\item{variable}{Es un vector (numérico o carácter) que indica las variables a seleccionar de \code{x}. Si \code{x} se refiere una sola variable, \code{variable = NULL}. En caso contrario, es necesario indicar el nombre o posición (número de columna) de la variable.}

\item{pesos}{Si los datos de la variable están resumidos en una distribución de frecuencias, debe indicarse la columna que representa los valores de la variable y la columna con las frecuencias o pesos.}
}
\value{
Si \code{pesos = NULL}, devuelve la mediana de todas la variables seleccionadas en un \code{vector}. En caso contrario, devuelve únicamente la mediana de la variable para la que se ha facilitado la distribución de frecuencias.
}
\description{
Calcula la mediana.

Lee el código QR para video-tutorial sobre el uso de la función con un ejemplo.

\if{html}{\figure{qrposicion.png}{options: width="25\%" alt="Figure: qricvarianza.png"}}
\if{latex}{\figure{qrposicion.png}{options: width=3cm}}
}
\details{
La mediana se obtiene a partir de la siguiente regla de decisión:

\if{html}{\figure{mediana.png}{options: width="80\%" alt="Figure: mediana.png"}}
\if{latex}{\figure{mediana.png}{options: scale=.8}}

donde: Ni son las frecuencias acumuladas y n el tamaño de la muestra (o N si es la población).
}
\examples{

mediana1 <- mediana(startup[1])
mediana2 <- mediana(startup,variable=1)
mediana3 <- mediana(salarios2018,variable=6,pesos=7)

}
\references{
Esteban García, J. y otros. (2005). Estadística descriptiva y nociones de probabilidad. Paraninfo. ISBN: 9788497323741

Newbold, P, Carlson, W. y Thorne, B. (2019). Statistics for Business and Economics, Global Edition. Pearson. ISBN: 9781292315034

Murgui, J.S. y otros. (2002). Ejercicios de estadística Economía y Ciencias sociales. tirant lo blanch. ISBN: 9788484424673
}
\seealso{
\code{\link{media}}, \code{\link{cuantiles}}
}
\author{
\strong{Vicente Coll-Serrano}.
\emph{Métodos Cuantitativos para la Medición de la Cultura (MC2). Economía Aplicada.}

\strong{Rosario Martínez Verdú}.
\emph{Economía Aplicada.}

\strong{Cristina Pardo-García}.
\emph{Métodos Cuantitativos para la Medición de la Cultura (MC2). Economía Aplicada.}

Facultad de Economía. Universidad de Valencia (España)
}
