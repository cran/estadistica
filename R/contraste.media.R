#' @title Contraste de hipótesis sobre la media.
#'
#' @description Realiza el contraste de hipótesis sobre la media poblacional.
#'
#' Lee el código QR para video-tutorial sobre el uso de la función con un ejemplo.
#'
#' \if{html}{\figure{qrcmedia.png}{options: width="25\%" alt="Figure: qricvarianza.png"}}
#' \if{latex}{\figure{qrcmedia.png}{options: width=3cm}}
#'
#' @usage contraste.media(x,
#'                  variable = NULL,
#'                  introducir = FALSE,
#'                  var_pob = c("conocida","desconocida"),
#'                  hipotesis_nula = NULL,
#'                  tipo_contraste =  c("bilateral","cola derecha","cola izquierda"),
#'                  alfa = 0.05,
#'                  grafico = FALSE)
#'
#' @param x Conjunto de datos. Puede ser un vector o un dataframe.
#' @param variable Es un vector (numérico o carácter) que indica las variables a seleccionar de \code{x}. Si \code{x} se refiere una sola variable, \code{variable = NULL}. En caso contrario, es necesario indicar el nombre o posición (número de columna) de la variable.
#' @param introducir Valor lógico. Si \code{introducir = FALSE} (por defecto), el usuario debe indicar el conjunto de datos que desea analizar usando los argumentos \code{x} y/o \code{variable}. Si \code{introducir = TRUE}, se le solicitará al ususario que introduzca la información relevante sobre tamaño muestral, valor de la media muestral, etc.
#' @param var_pob Es un carácter. Indica si la varianza poblacional es conocida (por defecto, \code{var_pob = "conocida"}) o desconocida. En este último caso debería cambiarse el argumento a \code{var_pob = "desconocida"}.
#' @param hipotesis_nula Es un valor numérico.
#' @param tipo_contraste Es un carácter. Indica el tipo de contraste a realizar. Por defecto, \code{tipo_contraste = "bilateral"}.
#'        Si \code{tipo_contraste = "bilateral"}, se contraste la hipótesis nula igual un valor frente a la alternativa distinto de dicho valor.
#'        Si \code{tipo_contraste = "cola derecha"}, se contrasta la hipótesis nula menor o igual a un valor frente a la alternativa mayor a dicho valor.
#'        Si \code{tipo_contraste = "cola izquierda"}, se contrasta la hipótesis nula mayor o igual a un valor frente a la alternativa menos a dicho valor.
#' @param alfa Es un valor numérico entre 0 y 1. Indica el nivel de significación. Por defecto, \code{alfa = 0.05} (5 por ciento)
#' @param grafico Es un valor lógico. Por defecto \code{grafico = FALSE}. Si se quiere obtener una representación gráfica del contraste realizado, cambiar el argumento a \code{grafico = TRUE}. Nota: Esta opción no está implementada para todos los casos.
#'
#' @return La función devuelve un objeto de la clase \code{list}. La lista contendrá información sobre: la hipótesis nula contrastada, el estadístico de prueba, el p-valor y  el intervalo de confianza para la media muestral supuesta cierta la hipótesis nula. Si \code{grafico=TRUE} se incluirá una representación gráfica de la región de aceptación-rechazo con los valores críticos y otra gráfica con el intervalo para la media muestral (supesta cierta H0).
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
#' (1) Si la varianza poblacional es conocida, el estadístico Z es:
#'
#' \if{html}{\figure{cmediavarcon.png}{options: width="30\%" alt="Figure: cmediavarcon.png"}}
#' \if{latex}{\figure{cmediavarcon.png}{options: width=3cm}}
#'
#' y se distribuye como una N(0,1)
#'
#' Si la varianza poblacional es desconocida pero la muesta es grande, puede utilizarse la varianza (o cuasi-varianza) muestral.
#'
#' (2) Si la varianza poblacional es desconocida, el estadístico T es:
#'
#' (2.1) usando la varianza muestral
#'
#' \if{html}{\figure{cmediavardescmuestra.png}{options: width="30\%" alt="Figure: cmediavardescmuestra.png"}}
#' \if{latex}{\figure{cmediavardescmuestra.png}{options: width=3cm}}
#'
#' (2.2) usando la cuasi-varianza muestral
#'
#' \if{html}{\figure{cmediavardesccuasi.png}{options: width="30\%" alt="Figure: cmediavardesccuasi.png"}}
#' \if{latex}{\figure{cmediavardesccuasi.png}{options: width=3cm}}
#'
#' Nota: en ambos casos el estadístico T se distrubuye como un t con n-1 grados de libertad.
#'
#' @seealso \code{\link{ic.media}}
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
#' @importFrom stats pnorm na.omit dnorm pt qt dt
#' @import dplyr ggplot2
#'
#' @export
contraste.media <- function(x,
                            variable = NULL,
                            introducir = FALSE,
                            var_pob = c("conocida","desconocida"),
                            hipotesis_nula = NULL,
                            tipo_contraste =  c("bilateral","cola derecha","cola izquierda"),
                            alfa = 0.05,
                            grafico = FALSE){

var_pob <- tolower(var_pob)
var_pob <- match.arg(var_pob)


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

    if(length(x) == 1){

      x <- x

    } else{

      warning("Para calcular el intervalo de confianza hay que seleccionar una variable")
      stop("El conjunto de datos seleccionado tiene mas de 1 variable.")

    }

  } else if(length(variable) == 1){

    if(is.numeric(variable)){

      if(variable <= length(x)){

        variable <- variable

      } else{

        stop("Selecci\u00f3n err\u00f3nea de variable")

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

    warning("Para calcular contraste hay que seleccionar una variable")
    stop("El conjunto de datos seleccionado tiene mas de 1 variable.")

  }

  x <- na.omit(x)
  clase <- sapply(x, class)

  if (!clase %in% c("numeric","integer")) {

    stop("No puede calcularse el intervalo de confianza porque la variable seleccionada no es cuantitativa")

  }

  # tama\u00f1o de la muestra
  n1 <- nrow(x)
  n <- n1
  gl <- n-1

  # media muestral
  media <- as.numeric(media(x))

  if(var_pob == "conocida"){

    varianza_pob <- readline(prompt = "Introducir el valor de la varianza poblacional: ")
    varianza_pob <- as.numeric(varianza_pob)
    desv_pob <- as.numeric(sqrt(varianza_pob))

  } else{

    var_muestra <- as.numeric(readline('Selecciona el valor que quieres utilizar: \n 1. "Varianza muestral" \n 2. "Cuasivarianza muestral" \n'))

    if(var_muestra == 1){

      desv_mu = as.numeric(desviacion(x))
      n <- n-1

    } else{

      desv_mu <- as.numeric(desviacion(x,tipo="cuasi"))

      }

  }


} else{   # aqu\u00ed empieza introducir datos

  n <- readline(prompt = "Introducir el tama\u00f1o de la muestra: ")
  n1 <- as.numeric(n)
  n <- n1
  gl <- n-1

  media <- readline(prompt = "Introducir el valor de la media muestral: ")
  media <- as.numeric(media)

  if(var_pob == "conocida"){
    varianza_pob <- readline(prompt = "Introducir el valor de la varianza poblacional: ")
    varianza_pob <- as.numeric(varianza_pob)
    desv_pob <- sqrt(varianza_pob)

  } else{

    var_muestra <- as.numeric(readline('Selecciona el valor que quieres utilizar: \n 1. "Varianza muestral" \n 2. "Cuasivarianza muestral" \n'))

    if(var_muestra == 1){

      varianza_muestral <- readline(prompt = "Introducir el valor de la varianza muestral: ")
      varianza_muestral <- as.numeric(varianza_muestral)
      desv_mu <- sqrt(varianza_muestral)
      n <- n-1

    } else{

      varianza_cuasi <- readline(prompt = "Introducir el valor de la cuasi-varianza muestral: ")
      varianza_cuasi <- as.numeric(varianza_cuasi)
      desv_mu <- sqrt(varianza_cuasi)

    }
  }
}



# calculo contrastes


    if(var_pob == "conocida"){

      aproximacion <- 0

      error_tipico <-  desv_pob / sqrt(n1)

    }

    if(var_pob == "desconocida" & (n>30)) {

      aproximacion <- as.numeric(readline(prompt = 'Quieres utilizar la aproximaci\u00f3n de la t a la normal?: \n 1. "S\u00ed" \n 2. "No, usar la t" \n'))

      if(aproximacion == 1){

        error_tipico <-  desv_mu / sqrt(n1)

      } else {

        error_tipico <-  desv_mu / sqrt(n) #definido como n-1 en var muestra y n en causi

      }
    }

    if(var_pob == "desconocida" & (n<=30)) {

      aproximacion <- 0
      error_tipico <-  desv_mu / sqrt(n) #definido como n-1 en var muestra y n en causi

    }

  estadistico.prueba <- (media - H0) / error_tipico
  estadistico.prueba <- as.numeric(estadistico.prueba)


  if(alfa >= 0 & alfa <=1){

    if(var_pob == "conocida" | aproximacion == 1){

      if(tipo_contraste == "bilateral"){
        valor_critico <- qnorm(alfa/2,lower.tail = F)
      }
      if(tipo_contraste == "cola izquierda"){
        valor_critico <- qnorm(alfa,lower.tail = T)
      }
      if(tipo_contraste == "cola derecha"){
        valor_critico <- qnorm(alfa,lower.tail = F)
      }

    } else{

      if(tipo_contraste == "bilateral"){
        valor_critico <- qt(alfa/2,gl,lower.tail = F)
      }
      if(tipo_contraste == "cola izquierda"){
        valor_critico <- qt(alfa,gl,lower.tail = T)
      }
      if(tipo_contraste == "cola derecha"){
        valor_critico <- qt(alfa,gl,lower.tail = F)
      }

    }

    valor_critico <- round(valor_critico,4)

  } else{

    stop("El nivel de significaci\u00f3n debe fijarse entre 0 y 1")

  }



if(tipo_contraste == "bilateral"){

  estadistico.prueba2 <- abs(estadistico.prueba)
  media_inf <- H0 - valor_critico * error_tipico
  media_sup <- H0 + valor_critico * error_tipico

  if(var_pob == "conocida" | aproximacion == 1){

    pvalor <- 2 * pnorm(estadistico.prueba2,lower.tail=FALSE)

    if(isTRUE(grafico)){

      plot <- ggplot(NULL, aes(c(-4,4))) +
        geom_area(stat = "function", fun = dnorm, fill = "red", xlim = c(-4, -valor_critico)) +
        geom_area(stat = "function", fun = dnorm, fill = "darkgreen", xlim = c(-valor_critico, valor_critico)) +
        geom_area(stat = "function", fun = dnorm, fill = "red", xlim = c(valor_critico, 4)) +
        geom_vline(xintercept = -estadistico.prueba2, linetype = "dashed") +
        geom_vline(xintercept = estadistico.prueba2, linetype = "dashed") +
        labs(x = "", y = "") +
        scale_y_continuous(breaks = NULL) +
        scale_x_continuous(breaks = c(estadistico.prueba2,-estadistico.prueba2,-valor_critico,valor_critico)) +
        theme(axis.text.x = element_text(angle = 45))

      df <- data.frame(x = c(H0-4*error_tipico, H0+4*error_tipico))
      plot2 <- ggplot(df, aes(x)) +
        geom_area(stat = "function", fun = dnorm, args= list(mean = H0, sd = error_tipico), fill = "red", xlim = c(df[1,1], media_inf)) +
        geom_area(stat = "function", fun = dnorm, args= list(mean = H0, sd = error_tipico), fill = "darkgreen", xlim = c(media_inf, media_sup)) +
        geom_area(stat = "function", fun = dnorm, args= list(mean = H0, sd = error_tipico), fill = "red", xlim = c(media_sup, df[2,1])) +
        geom_vline(xintercept = media, linetype = "dashed") +
        labs(x = "", y = "",title="Intervalo de la media muestral\n(supuesta H0 cierta)") +
        scale_y_continuous(breaks = NULL) +
        scale_x_continuous(breaks = c(media_inf,media,media_sup)) +
        theme(axis.text.x = element_text(angle = 45))


    }


  } else{

    pvalor <- 2 * pt(estadistico.prueba2,gl, lower.tail=FALSE)

    if(isTRUE(grafico)){

      plot <- ggplot(NULL, aes(c(-4,4))) +
        geom_area(stat = "function", fun = dt, args = list(df = gl), fill = "darkgreen", xlim = c(-valor_critico, valor_critico)) +
        geom_area(stat = "function", fun = dt, args = list(df = gl), fill = "red", xlim = c(-4, -valor_critico)) +
        geom_area(stat = "function", fun = dt, args = list(df = gl), fill = "red", xlim = c(valor_critico, 4)) +
        geom_vline(xintercept = 0, color = "black") +
        geom_vline(xintercept = -estadistico.prueba2, color = "blue", linetype = "dashed") +
        geom_vline(xintercept = estadistico.prueba2, color = "blue", linetype = "dashed") +
        labs(title = paste("Distribuci\u00f3n t con ", gl, " grados de libertad",sep=""), x = "", y = "") +
        scale_y_continuous(breaks = NULL) +
        scale_x_continuous(breaks = c(-estadistico.prueba2,estadistico.prueba2,valor_critico,-valor_critico)) +
        theme(axis.text.x = element_text(angle = 45))

      plot2 <- NULL


    }

  }

  if(estadistico.prueba >= -valor_critico & estadistico.prueba <=  valor_critico){

    print(paste("No se rechaza la hip\u00f3tesis nula. La regi\u00f3n de aceptaci\u00f3n viene dada por el intervalo [", -valor_critico," , ",valor_critico,"]",sep=""))
    print("El valor del estad\u00edstico de prueba (o valor experimental) se encuentra dentro de la regi\u00f3n de aceptaci\u00f3n")

  } else{

    print(paste("Se rechaza la hip\u00f3tesis nula. La regi\u00f3n de aceptaci\u00f3n viene dada por el intervalo [", -valor_critico," , ",valor_critico,"]",sep=""))
    print("El valor del estad\u00edstico de prueba (o valor experimental) no se encuentra dentro de la regi\u00f3n de aceptaci\u00f3n")

  }

}

if(tipo_contraste == "cola derecha"){

  media_inf <- -Inf
  media_sup <- H0 + valor_critico * error_tipico

  if(var_pob == "conocida" | aproximacion == 1){

    pvalor <- pnorm(estadistico.prueba,lower.tail=FALSE)

    if(isTRUE(grafico)){

      plot <- ggplot(NULL, aes(c(-4,4))) +
        geom_area(stat = "function", fun = dnorm, fill = "darkgreen", xlim = c(-4L,valor_critico)) +
        geom_area(stat = "function", fun = dnorm, fill = "red", xlim = c(valor_critico, 4L)) +
        geom_vline(xintercept = estadistico.prueba, linetype = "dashed") +
        labs(x = "z", y = "") +
        scale_y_continuous(breaks = NULL) +
        scale_x_continuous(breaks = c(estadistico.prueba,valor_critico)) +
        theme(axis.text.x = element_text(angle = 45))

      df <- data.frame(x = c(H0-4*error_tipico, H0+4*error_tipico))
      plot2 <- ggplot(df, aes(x)) +
        geom_area(stat = "function", fun = dnorm, args= list(mean = H0, sd = error_tipico), fill = "darkgreen", xlim = c(df[1,1], media_sup)) +
        geom_area(stat = "function", fun = dnorm, args= list(mean = H0, sd = error_tipico), fill = "red", xlim = c(media_sup, df[2,1])) +
        geom_vline(xintercept = media, linetype = "dashed") +
        labs(x = "", y = "",title="Intervalo de la media muestral\n(supuesta HO cierta)") +
        scale_y_continuous(breaks = NULL) +
        scale_x_continuous(breaks = c(media,media_sup)) +
        theme(axis.text.x = element_text(angle = 45))

    }


  } else{

    pvalor <- pt(estadistico.prueba,gl, lower.tail=FALSE)

    if(isTRUE(grafico)){

      plot <- ggplot(NULL, aes(c(-4,4))) +
        geom_area(stat = "function", fun = dt, args = list(df = gl), fill = "darkgreen", xlim = c(-4, valor_critico)) +
        geom_area(stat = "function", fun = dt, args = list(df = gl), fill = "red", xlim = c(valor_critico, 4)) +
        geom_vline(xintercept = 0, color = "black") +
        geom_vline(xintercept = estadistico.prueba, color = "blue", linetype = "dashed") +
        labs(title = paste("Distribuci\u00f3n t con ", gl, " grados de libertad",sep=""), x = "", y = "") +
        scale_y_continuous(breaks = NULL) +
        scale_x_continuous(breaks = c(estadistico.prueba,valor_critico)) +
        theme(axis.text.x = element_text(angle = 45))

      plot2 <- NULL


    }


  }

  if(estadistico.prueba > valor_critico){

    print(paste("Se rechaza la hip\u00f3tesis nula. La regi\u00f3n de aceptaci\u00f3n viene dada por el intervalo ]-Inf , ", valor_critico,"]",sep=""))
    print("El valor del estad\u00edstico de prueba (o valor experimental) no se encuentra dentro de la regi\u00f3n de aceptaci\u00f3n")

  } else{

    print(paste("No se rechaza la hip\u00f3tesis nula. La regi\u00f3n de aceptaci\u00f3n viene dada por el intervalo ]-Inf , ", valor_critico,"]",sep=""))
    print("El valor del estad\u00edstico de prueba (o valor experimental) se encuentra dentro de la regi\u00f3n de aceptaci\u00f3n")

  }


}

if(tipo_contraste == "cola izquierda"){

  media_inf <- H0 + valor_critico * error_tipico # valor critico negativo
  media_sup <- Inf

  if(var_pob == "conocida" | aproximacion == 1){

    pvalor <- pnorm(estadistico.prueba,lower.tail=TRUE)

    if(isTRUE(grafico)){

        plot <- ggplot(NULL, aes(c(-4,4))) +
          geom_area(stat = "function", fun = dnorm, fill = "red", xlim = c(-4L, valor_critico)) +
          geom_area(stat = "function", fun = dnorm, fill = "darkgreen", xlim = c(valor_critico, 4L)) +
          geom_vline(xintercept = estadistico.prueba, linetype = "dashed") +
          labs(x = "z", y = "") +
          scale_y_continuous(breaks = NULL) +
          scale_x_continuous(breaks = c(estadistico.prueba,-valor_critico)) +
          theme(axis.text.x = element_text(angle = 45))

        df <- data.frame(x = c(H0-4*error_tipico, H0+4*error_tipico))
        plot2 <- ggplot(df, aes(x)) +
          geom_area(stat = "function", fun = dnorm, args= list(mean = H0, sd = error_tipico), fill = "red", xlim = c(df[1,1], media_inf)) +
          geom_area(stat = "function", fun = dnorm, args= list(mean = H0, sd = error_tipico), fill = "darkgreen", xlim = c(media_inf, df[2,1])) +
          geom_vline(xintercept = media, linetype = "dashed") +
          labs(x = "", y = "",title="Intervalo de la media muestral\n(supuesta H0 cierta)") +
          scale_y_continuous(breaks = NULL) +
          scale_x_continuous(breaks = c(media_inf,media)) +
          theme(axis.text.x = element_text(angle = 45))

      }

  } else{

    pvalor <- pt(estadistico.prueba,gl, lower.tail=TRUE)

    if(isTRUE(grafico)){

      plot <- ggplot(NULL, aes(c(-4,4))) +
        geom_area(stat = "function", fun = dt, args = list(df = gl), fill = "darkgreen", xlim = c(valor_critico,4)) +
        geom_area(stat = "function", fun = dt, args = list(df = gl), fill = "red", xlim = c(-4,valor_critico)) +
        geom_vline(xintercept = 0, color = "black") +
        geom_vline(xintercept = estadistico.prueba, color = "blue", linetype = "dashed") +
        labs(title = paste("Distribuci\u00f3n t con ", gl, " grados de libertad",sep=""), x = "", y = "") +
        scale_y_continuous(breaks = NULL) +
        scale_x_continuous(breaks = c(estadistico.prueba,-valor_critico)) +
        theme(axis.text.x = element_text(angle = 45))

      plot2 <- NULL


    }



  }

  if(estadistico.prueba < valor_critico){

    print(paste("Se rechaza la hip\u00f3tesis nula. La regi\u00f3n de aceptaci\u00f3n viene dada por el intervalo [ ",valor_critico," , inf[",sep=""))
    print("El valor del estad\u00edstico de prueba (o valor experimental) no se encuentra dentro de la regi\u00f3n de aceptaci\u00f3n")

  } else{

    print(paste("No se rechaza la hip\u00f3tesis nula. La regi\u00f3n de aceptaci\u00f3n viene dada por el intervalo [ ",valor_critico," , inf[",sep=""))
    print("El valor del estad\u00edstico de prueba (o valor experimental) se encuentra dentro de la regi\u00f3n de aceptaci\u00f3n")

  }

  }



  CH <- cbind(H0,estadistico.prueba,pvalor)
  CH <- as.data.frame(CH)
  names(CH) <- c("Hip\u00f3tesis nula", "estad\u00edstico de prueba", "p-valor")
  row.names(CH) <- NULL

  Imedia <- cbind(`limite_inferior`=media_inf,`limite_superior`=media_sup)


  if(isTRUE(grafico)){

    plots <- list(plot,plot2)

    return(list(`Estadistico`=CH,`Intervalo de la media muestral (supuesta H0 cierta)`= Imedia,`Graficos`= plots))

  } else{

    return(list(`Estadistico`=CH,`Intervalo de la media muestral (supuesta H0 cierta)`= Imedia))

  }

}

