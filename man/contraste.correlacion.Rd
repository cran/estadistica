% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/contraste.correlacion.R
\name{contraste.correlacion}
\alias{contraste.correlacion}
\title{Contraste de hipótesis de correlación}
\usage{
contraste.correlacion(x,
                      variable = NULL,
                      introducir = FALSE,
                      hipotesis_nula = 0,
                      tipo_contraste = "bilateral",
                      alfa = 0.05)
}
\arguments{
\item{x}{Conjunto de datos. Puede ser un vector o un dataframe.}

\item{variable}{Es un vector (numérico o carácter) que indica las variables a seleccionar de \code{x}. Si \code{x} se refiere una sola variable, \code{variable = NULL}. En caso contrario, es necesario indicar el nombre o posición (número de columna) de la variable.}

\item{introducir}{Valor lógico. Si \code{introducir = FALSE} (por defecto), el usuario debe indicar el conjunto de datos que desea analizar usando los argumentos \code{x} y/o \code{variable}. Si \code{introducir = TRUE}, se le solicitará al ususario que introduzca la información relevante sobre tamaño muestral, valor de la media muestral, etc.}

\item{hipotesis_nula}{Es un valor numérico. Por defecto el valor está fijado a cero (incorrelación).}

\item{tipo_contraste}{Es un carácter. Indica el tipo de contraste a realizar. Por defecto, \code{tipo_contraste = "bilateral"}.
Si \code{tipo_contraste = "bilateral"}, se contraste la hipótesis nula igual un valor frente a la alternativa distinto de dicho valor.
Si \code{tipo_contraste = "cola derecha"}, se contrasta la hipótesis nula menor o igual a un valor frente a la alternativa mayor a dicho valor.
Si \code{tipo_contraste = "cola izquierda"}, se contrasta la hipótesis nula mayor o igual a un valor frente a la alternativa menos a dicho valor.}

\item{alfa}{Es un valor numérico entre 0 y 1. Indica el nivel de significación. Por defecto, \code{alfa = 0.05} (5 por ciento)}
}
\value{
Esta función devuelve un objeto de la clase \code{data.frame} en el que se incluye la hipótesis nula contrastada, el valor del estadístico de prueba y el p-valor.
}
\description{
Realiza el contraste de hipótesis sobre el coeficiente de correlación.
}
\details{
El estadístico del contraste es:

\if{html}{\figure{ccorrelacion.png}{options: width="30\%" alt="Figure: ccorrelacion.png"}}
\if{latex}{\figure{ccorrelacion.png}{options: width=4cm}}

que se distribuye como una t con n-2 grados de libertad.
}
\references{
Casas José M. (1997) Inferencia estadística. Editorial: Centro de estudios Ramón Areces, S.A. ISBN: 848004263-X

Esteban García, J. et al. (2008). Curso básico de inferencia estadística. ReproExprés, SL. ISBN: 8493036595.

Murgui, J.S. y otros. (2002). Ejercicios de estadística Economía y Ciencias sociales. tirant lo blanch. ISBN: 9788484424673

Newbold, P, Carlson, W. y Thorne, B. (2019). Statistics for Business and Economics, Global Edition. Pearson. ISBN: 9781292315034
}
\seealso{
\code{\link{ic.correlacion}}
}
\author{
\strong{Vicente Coll-Serrano}.
\emph{Métodos Cuantitativos para la Medición de la Cultura (MC2). Economía Aplicada.}

\strong{Rosario Martínez Verdú}.
\emph{Economía Aplicada.}

Facultad de Economía. Universidad de Valencia (España)
}
