#' @title Contraste de hipótesis sobre la razón de varianzas.
#'
#' @description Realiza el contraste de hipótesis sobre la razón de dos varianzas poblacionales.
#'
#' Lee el código QR para video-tutorial sobre el uso de la función con un ejemplo.
#'
#' \if{html}{\figure{qrcrazonvarianzas.png}{options: width="25\%" alt="Figure: qricvarianza.png"}}
#' \if{latex}{\figure{qrcrazonvarianzas.png}{options: width=3cm}}
#'
#' @usage contraste.razon.varianzas(x,
#'                  variable = NULL,
#'                  introducir = FALSE,
#'                  hipotesis_nula = 1,
#'                  tipo_contraste = c("bilateral","cola derecha","cola izquierda"),
#'                  alfa = 0.05,
#'                  grafico = FALSE)
#'
#' @param x Conjunto de datos. Puede ser un vector o un dataframe.
#' @param variable Es un vector (numérico o carácter) que indica las variables a seleccionar de \code{x}. Si \code{x} se refiere solo a dos variables, \code{variable = NULL}. En caso contrario, es necesario indicar el nombre o posición (número de columna) de las variables.
#' @param introducir Valor lógico. Si \code{introducir = FALSE} (por defecto), el usuario debe indicar el conjunto de datos que desea analizar usando los argumentos \code{x} y/o \code{variable}. Si \code{introducir = TRUE}, se le solicitará al ususario que introduzca la información relevante sobre tamaño muestral, valor de la media muestral, etc.
#' @param hipotesis_nula Es un valor numérico. Por defecto el valor está fijado a 1, es decir, igualdad de varianzas.
#' @param tipo_contraste Es un carácter. Indica el tipo de contraste a realizar. Por defecto, \code{tipo_contraste = "bilateral"}.
#'        Si \code{tipo_contraste = "bilateral"}, se contraste la hipótesis nula igual un valor frente a la alternativa distinto de dicho valor.
#'        Si \code{tipo_contraste = "cola derecha"}, se contrasta la hipótesis nula menor o igual a un valor frente a la alternativa mayor a dicho valor.
#'        Si \code{tipo_contraste = "cola izquierda"}, se contrasta la hipótesis nula mayor o igual a un valor frente a la alternativa menos a dicho valor.
#' @param alfa Es un valor numérico entre 0 y 1. Indica el nivel de significación. Por defecto, \code{alfa = 0.05} (5 por ciento)
#' @param grafico Es un valor lógico. Por defecto \code{grafico = FALSE}. Si se quiere obtener una representación gráfica del contraste realizado, cambiar el argumento a \code{grafico = TRUE}. Nota: Esta opción no está implementada para todos los casos.
#'
#' @return La función devuelve un objeto de la clase \code{list}. La lista contendrá información sobre: la hipótesis nula contrastada, el estadístico de prueba, el p-valor y  el intervalo de confianza para la media muestral supuesta cierta la hipótesis nula. Si \code{grafico=TRUE} se incluirá una representación gráfica de la región de aceptación-rechazo con los valores críticos.
#'
#' @author
#' \strong{Vicente Coll-Serrano}.
#' \emph{Métodos Cuantitativos para la Medición de la Cultura (MC2). Economía Aplicada.}
#'
#' \strong{Rosario Martínez Verdú}.
#' \emph{Economía Aplicada.}
#'
#' Facultad de Economía. Universidad de Valencia (España)
#'
#' @details
#'
#' La hipótesis nula que se considera en el contraste bilateral es:
#'
#' \if{html}{\figure{crazonvar.png}{options: width="30\%" alt="Figure: contrastecocientevar.png"}}
#' \if{latex}{\figure{crazonvar.png}{options: width=3cm}}
#'
#' El estadístico F es:
#'
#' (1) Si trabajamos con la varianza muestral:
#'
#'
#' \if{html}{\figure{crazonvarmuestra.png}{options: width="50\%" alt="Figure: contrastecocientevarmuestra.png"}}
#' \if{latex}{\figure{crazonvarmuestra.png}{options: width=5cm}}
#'
#' (2) si trabajamos con la cuasi-varianza muestral:
#'
#' \if{html}{\figure{crazonvarcuasi.png}{options: width="25\%" alt="Figure: contrastecocientevarcuasi.png"}}
#' \if{latex}{\figure{crazonvarcuasi.png}{options: width=3cm}}
#'
#' Tanto en (1) como en (2) el estadístico F se distribuye como una F con (n1-1) grados de libertad en el numerador y (n2-1) grados de libertad en el denominador.
#'
#' @seealso \code{\link{ic.razon.varianzas}}
#'
#' @references
#' Casas José M. (1997) Inferencia estadística. Editorial: Centro de estudios Ramón Areces, S.A. ISBN: 848004263-X
#'
#' Esteban García, J. et al. (2008). Curso básico de inferencia estadística. ReproExprés, SL. ISBN: 8493036595.
#'
#' Murgui, J.S. y otros. (2002). Ejercicios de estadística Economía y Ciencias sociales. tirant lo blanch. ISBN: 9788484424673
#'
#' Newbold, P, Carlson, W. y Thorne, B. (2019). Statistics for Business and Economics, Global Edition. Pearson. ISBN: 9781292315034
#'
#' @importFrom stats na.omit df pf qf
#' @import dplyr ggplot2
#'
#' @export
contraste.razon.varianzas <- function(x,
                                      variable = NULL,
                                      introducir = FALSE,
                                      hipotesis_nula = 1,
                                      tipo_contraste = c("bilateral","cola derecha","cola izquierda"),
                                      alfa = 0.05,
                                      grafico = FALSE){


print("Se calcula el contraste para el cociente de varianzas supuestas desconocidas las medias poblacionales")

tipo_contraste <- tolower(tipo_contraste)
tipo_contraste <- match.arg(tipo_contraste)


if(is.null(hipotesis_nula) | !is.numeric(hipotesis_nula)){

  stop("Tienes que introducir un valor para la hip\u00f3tesis nula")

} else{

  H0 <- hipotesis_nula

}


if(isFALSE(introducir)) {

  x <- data.frame(x)
  varnames <- names(x)

  if(is.null(variable)){

    if(length(x) == 2){
      x <- x
    } else{
      warning("Para calcular este tipo de contraste hay que seleccionar 2 variables")
      stop("El conjunto de datos seleccionado no tiene la dimensi\u00f3n adecuada")
    }
  } else{

    if(length(variable) == 2){
        if(is.numeric(variable)){
          if(all(variable <= length(x))){
            variable <- variable
          } else{
            stop("Selecci\u00f3n err\u00f3nea de variables")
          }
        }
      if(is.character(variable)){
        if(all(variable %in% varnames)){
          variable = match(variable,varnames)
          } else {
            stop("El nombre de la variable no es v\u00e1lido")
          }
        }

      x <- x[,variable] %>% as.data.frame()
      names(x) <- varnames[variable]

      } else{
        warning("Para calcular el contraste de la raz\u00f3n de varianzas hay que seleccionar dos variables")
        stop("El conjunto de datos seleccionado parece ser no v\u00e1lido")
      }
  }

  clase <- sapply(x, class)

  if (!all(clase %in% c("numeric","integer"))){
    stop("No puede calcularse el contraste porque la variable seleccionada no es cuantitativa")
  }

  # tama\u00f1o de la muestra
  n1 <- length(x[1][!is.na(x[1])])
  n2 <- length(x[2][!is.na(x[2])])


  var_muestra <- as.numeric(readline('Selecciona el valor que quieres utilizar? \n 1. "Varianza muestral" \n 2. "Cuasivarianza muestral" \n'))

  if(var_muestra == 1){

    var_mu1 <- as.numeric(varianza(x[1]))
    var_mu2 <- as.numeric(varianza(x[2]))

  } else{

    var_mu1 <- as.numeric(varianza(x[1], tipo = "cuasi"))
    var_mu2 <- as.numeric(varianza(x[2], tipo = "cuasi"))

  }

} else{   # aqu\u00ed empieza introducir datos

  print("A continuaci\u00f3n, vas a introducir los datos de las muestras.")

  n1 <- readline(prompt = "Introducir el tama\u00f1o de la muestra 1: ")
  n1 <- as.numeric(n1)

  n2 <- readline(prompt = "Introducir el tama\u00f1o de la muestra 2: ")
  n2 <- as.numeric(n2)

  var_muestra <- as.numeric(readline('Selecciona el valor que quieres utilizar? \n 1. "Varianza muestral" \n 2. "Cuasivarianza muestral" \n'))

    if(var_muestra == 1){

      var_mu1 <- readline("Introduce el valor de la varianza muestral 1: ")
      var_mu1 <- as.numeric(var_mu1)

      var_mu2 <- readline("Introduce el valor de la varianza muestral 2: ")
      var_mu2 <- as.numeric(var_mu2)

    } else{

      var_mu1 <- readline("Introduce el valor de la cuasivarianza muestral 1: ")
      var_mu1 <- as.numeric(var_mu1)

      var_mu2 <- readline("Introduce el valor de la cuasivarianza muestral 2: ")
      var_mu2 <- as.numeric(var_mu2)

    }

}

if(alfa >= 0 & alfa <=1){

  if(tipo_contraste == "bilateral"){

    valor_critico1 <- round(qf(alfa/2, df1= n2-1, df2 = n1-1,lower.tail = T),4)
    valor_critico2 <- round(qf(alfa/2, df1= n2-1, df2 = n1-1,lower.tail = F),4)

  }

  if(tipo_contraste == "cola izquierda"){

    valor_critico <- round(qf(alfa, df1= n2-1, df2 = n1-1,lower.tail = T),4)
  }

  if(tipo_contraste == "cola derecha"){

    valor_critico <- round(qf(alfa, df1= n2-1, df2 = n1-1,lower.tail = F),4)
  }

} else{

  stop("El nivel de significacion debe fijarse entre 0 y 1")

}


# calculo del contraste

# contraste del cociente de varianzas con medias desconocidas


if(var_muestra == 1){

  # caso 1.1
  estadistico.prueba <- (n1/(n1-1))*((n2-1)/n2)*(var_mu1/var_mu2)*(1/hipotesis_nula)

} else {

  # caso 1.2
  print("Este es el intervalo de confianza que generalmente calculan los softwares")

  estadistico.prueba <- (var_mu1 / var_mu2) * (1/hipotesis_nula)

}

percentil99 <- qf(.9999, df1= n1-1, df2 = n2-1)

data <- data.frame(x=seq(from = 0, to = percentil99, percentil99/200))
data$y <-df(data$x, df1= n1-1, df2 = n2-1)

if(tipo_contraste == "bilateral"){

  pvalor <- 2 * min(pf(estadistico.prueba, df1= n1-1, df2 = n2-1,lower.tail = F), pf(estadistico.prueba, df1= n1-1, df2 = n2-1,lower.tail = T))

  if(estadistico.prueba >= valor_critico1 & estadistico.prueba <=  valor_critico2){

    print(paste("No se rechaza la hip\u00f3tesis nula. La regi\u00f3n de aceptaci\u00f3n viene dada por el intervalo [", valor_critico1," , ",valor_critico2,"]",sep=""))
    print("El valor del estad\u00edstico de prueba (o valor experimental) se encuentra dentro de la regi\u00f3n de aceptaci\u00f3n")

  } else{

    print(paste("Se rechaza la hip\u00f3tesis nula. La regi\u00f3n de aceptaci\u00f3n viene dada por el intervalo [", valor_critico1," , ",valor_critico2,"]",sep=""))
    print("El valor del estad\u00edstico de prueba (o valor experimental) no se encuentra dentro de la regi\u00f3n de aceptaci\u00f3n")

  }

  if(isTRUE(grafico)){

    plot <- ggplot(data, aes(x,y)) +
      geom_area(fill="darkgreen") +
      geom_area(data=subset(data,x<valor_critico1), fill = "red") +
      geom_area(data=subset(data,x>valor_critico2),fill = "red") +
      geom_vline(xintercept = 0, color = "black") +
      geom_vline(xintercept = estadistico.prueba, color = "blue", linetype = "dashed") +
      labs(title = paste("Distribuci\u00f3n F con ", n1-1, " y ",n2-1," grados de libertad",sep=""), x = "", y = "") +
      scale_y_continuous(breaks = NULL) +
      scale_x_continuous(breaks = round(c(0L,estadistico.prueba,valor_critico1,valor_critico2),4)) +
      theme(axis.text.x = element_text(angle = 45))
  }


} else if(tipo_contraste == "cola derecha"){

  pvalor <- pf(estadistico.prueba, df1= n1-1, df2 = n2-1,lower.tail = F)

  if(estadistico.prueba >= valor_critico){

    print(paste("Se rechaza la hip\u00f3tesis nula. La regi\u00f3n de aceptaci\u00f3n viene dada por el intervalo [", 0," , ",valor_critico,"]",sep=""))
    print("El valor del estad\u00edstico de prueba (o valor experimental) no se encuentra dentro de la regi\u00f3n de aceptaci\u00f3n")

  } else{

    print(paste("No Se rechaza la hip\u00f3tesis nula. La regi\u00f3n de aceptaci\u00f3n viene dada por el intervalo [", 0," , ",valor_critico,"]",sep=""))
    print("El valor del estad\u00edstico de prueba (o valor experimental) se encuentra dentro de la regi\u00f3n de aceptaci\u00f3n")

  }

  if(isTRUE(grafico)){

    plot <- ggplot(data, aes(x,y)) +
      geom_area(fill="darkgreen") +
      geom_area(data=subset(data,x>valor_critico),fill = "red") +
      geom_vline(xintercept = 0, color = "black") +
      geom_vline(xintercept = estadistico.prueba, color = "blue", linetype = "dashed") +
      labs(title = paste("Distribuci\u00f3n F con ", n1-1, " y ",n2-1," grados de libertad",sep=""), x = "", y = "") +
      scale_y_continuous(breaks = NULL) +
      scale_x_continuous(breaks = round(c(0L,estadistico.prueba,valor_critico),4)) +
      theme(axis.text.x = element_text(angle = 45))
  }


} else{

  pvalor <- pf(estadistico.prueba, df1= n1-1, df2 = n2-1,lower.tail = T)

  if(estadistico.prueba <= valor_critico){

    print(paste("Se rechaza la hip\u00f3tesis nula. La regi\u00f3n de aceptaci\u00f3n viene dada por el intervalo [", valor_critico," , ",Inf,"]",sep=""))
    print("El valor del estad\u00edstico de prueba (o valor experimental) no se encuentra dentro de la regi\u00f3n de aceptaci\u00f3n")

  } else{

    print(paste("No se rechaza la hip\u00f3tesis nula. La regi\u00f3n de aceptaci\u00f3n viene dada por el intervalo [", valor_critico," , ",Inf,"]",sep=""))
    print("El valor del estad\u00edstico de prueba (o valor experimental) se encuentra dentro de la regi\u00f3n de aceptaci\u00f3n")

  }

  if(isTRUE(grafico)){

    plot <- ggplot(data, aes(x,y)) +
      geom_area(fill="darkgreen") +
      geom_area(data=subset(data,x<valor_critico),fill = "red") +
      geom_vline(xintercept = 0, color = "black") +
      geom_vline(xintercept = estadistico.prueba, color = "blue", linetype = "dashed") +
      labs(title = paste("Distribuci\u00f3n F con ", n1-1, " y ",n2-1," grados de libertad",sep=""), x = "", y = "") +
      scale_y_continuous(breaks = NULL) +
      scale_x_continuous(breaks = round(c(0L,estadistico.prueba,valor_critico),4)) +
      theme(axis.text.x = element_text(angle = 45))
  }

}

  CH <- cbind(H0,estadistico.prueba,pvalor)
  CH <- as.data.frame(CH)
  names(CH) <- c("Hip\u00f3tesis nula", "estad\u00edstico de prueba", "p-valor")
  row.names(CH) <- NULL

  if(isTRUE(grafico)){

    return(list(CH,plot))

  } else{

    return(CH)

  }


}
