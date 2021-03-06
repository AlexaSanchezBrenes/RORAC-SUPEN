#              
#                                    Consultoría RORAC-SUPEN
#      Función a optimizar para obtener la curva cero cupón mediante el Modelo de Nelson-Siegel y
#                                           Svenssson

# Autores:
# Alexa Sánchez
# Isaac Z. Arias

# El siguiente módulo representa la función de error cuadrático medio que se debe minimizar para 
# obtener los parámetros óptimos de la curva cero cupón para cada mes según Modelo de Nelson-Siegel
# y Svensson modificado.


# Paquetes necesarios:
library(nleqslv)
library(readr)
library(ggplot2)
library(optimization)
library(dygraphs)
library(xts)
library(tictoc)
library(stringi)
library(dplyr)
library(tools)
library(stringr)
library(GA)
library(lubridate)
library(pso)
Sys.setlocale("LC_TIME", "Spanish")
options(stringsAsFactors = FALSE)

#---------------------------------------- Parámetros Generales:

# Dirrección de los datos:
Dic <- "C:/Users/EQUIPO/Desktop/Estudios/RORAC-SUPEN/Boletas"

# Vector de tasas cortas mensuales TRI:
TRI.corta <- log(1+c(1.25,1.28,1.25,1.26,0.76,0.75)/100/52)*52/12

# Tasa de último vencimiento TRI en el primer mes observado (marzo):
TRI.larga <- log(1+9.09/100*5)/(5*12)

# Constante de ponderación:
alpha <- 1

#---------------------------------------- Carga de Datos:

# Función para leer los datos:
lee.boleta <<- function(archivo){
  
  if(file_ext(archivo) =="csv"){ 
    tbl<-as.data.frame(unclass(read.table(archivo, 
                                          header = TRUE, 
                                          sep = ",", 
                                          encoding =  "UTF-8",
                                          stringsAsFactors = FALSE)))
    names<-colnames(tbl)
    names[which(stri_enc_mark(names) == "native")]<-stringi::stri_trans_general(
      names[which(stri_enc_mark(names) == "native")],
      "Latin-ASCII")
  }else{
    tbl <- as.data.frame(unclass(read.table(archivo, 
                                            header = TRUE, 
                                            sep = ",", 
                                            encoding = "Latin1",
                                            stringsAsFactors = F)))
    names <- colnames(tbl)
    names <- stringi::stri_trans_general(colnames(tbl), "Latin-ASCII")
    
  }
  
  
  df <- tbl %>% 
    mutate(Tasa.facial = as.numeric(str_replace(Tasa.facial, ",", ".")),
           Precio = as.numeric(str_replace(Precio, ",", ".")),
           Valor.Transado = as.numeric(str_replace(Valor.Transado, ",", ".")),
           Valor.facial = as.numeric(str_replace(Valor.facial, ",", ".")),
           Tis = as.numeric(str_replace(Tis, ",", ".")),
           Tir = as.numeric(str_replace(Tir, ",", ".")),
           #Fecha.de.Operacion = as.Date(as.character.Date(Fecha.de.Operacion)),
           Rendimiento.de.la.recompra = as.numeric(str_replace
                                                   (Rendimiento.de.la.recompra , ",", ".")))
  colnames(df) <- names  
  return(df)
}

# Función para obtener los datos de las carpetas:
lista.df.boletas<-function(path=Dic){
  folder <- list.files(path,full.name = TRUE )
  n <- length(folder)
  archivo <- numeric(n)
  lista.df <- list()
  
  for(i in 1:n){
    if(length(list.files(path = folder[i], pattern = ".txt")) != 0){
      archivo[i] <- list.files(path = folder[i], pattern = ".txt")
    }else{
      archivo[i] <- list.files(path = folder[i], pattern = ".csv")}
    
    lista.df[[i]] <-lee.boleta(paste0(folder[i],"/",archivo[i])) %>%
      filter(Recompra == "NO",
             Fecha.de.Vencimiento != "",
             Mercado.de.Negociacion == "MERCADO SECUNDARIO",
             Nemotecnico.del.Emisor %in% c("BCCR","G"),
             Moneda.del.instrumento == "Colones Costarricenses",
             !Nemotecnico.del.instrumento %in% c("bemv", "tp$", "tpras", "tptba", "TUDES", "tudes", "bemud", "TPTBA")) %>% 
      select(Numero.de.Contrato.Largo,Periodicidad,Precio, Tasa.facial, Fecha.Ultimo.Pago.Intereses, Fecha.de.Operacion, Fecha.de.Vencimiento,Dias.Acumulados.Intereses) %>% 
      mutate(Fecha.de.Operacion = as.POSIXct(Fecha.de.Operacion, format = "%Y/%m/%d %H:%M:%S"),
             Fecha.de.Vencimiento = as.POSIXct(Fecha.de.Vencimiento, format = "%Y/%m/%d %H:%M:%S"),
             Fecha.Ultimo.Pago.Intereses = as.POSIXct(Fecha.Ultimo.Pago.Intereses, format = "%Y/%m/%d %H:%M:%S"),
             Mes = paste(month(Fecha.de.Operacion),year(Fecha.de.Operacion),sep = "-"),
             Tasa.facial = ifelse(Periodicidad==0, Tasa.facial/100, Tasa.facial/100/Periodicidad), 
             Precio = Precio/100) %>% 
      arrange(Mes) %>% mutate(Precio = ifelse(Periodicidad == 0, 
                                              Precio, 
                                              Precio + Tasa.facial*Dias.Acumulados.Intereses/(360/Periodicidad))) %>% 
      select(-Dias.Acumulados.Intereses)
  }
  
  names(lista.df) <- archivo  
  return(lista.df)
}

# Se cargan los datos:
Lista.Bonos <- bind_rows(lista.df.boletas()) %>%
  split(., .[, "Mes"])

#---------------------------------------- Definición de la Función Objetivo:

# Función para calcular la diferencia de fechas en meses:
Tau <- function(t, Te) {
  ed <- as.POSIXlt(Te)
  sd <- as.POSIXlt(t)
  as.double(12 * (ed$year - sd$year) + (ed$mon - sd$mon) + day(ed)/days_in_month(ed) - day(sd)/days_in_month(sd))
}

# Función para calcular los tiempos de cada cero cupon en cuponado:
Tau.total <- function(fila){
  
  # Creamoe el Tau del ponderador:
  fila <- fila %>% 
    mutate(Diff.semana = ceiling(day(Fecha.de.Operacion)/7))
  if(fila[,"Periodicidad"]==0){
    tabla <- fila %>% mutate(Tau = Tau(Fecha.de.Operacion, Fecha.de.Vencimiento),
                             Fecha.Pago = Fecha.de.Vencimiento)
  }else{
    Taus <- seq.Date(from = as.Date(fila[,"Fecha.Ultimo.Pago.Intereses"]),
                     to = as.Date(fila[,"Fecha.de.Vencimiento"]),
                     by = paste(as.character(12/fila[,"Periodicidad"]),"months",sep=" "))[-1]
    tabla <- fila[rep(seq_len(nrow(fila)), each = length(Taus)), ] %>% mutate(Fecha.Pago = Taus) %>% 
      mutate(Tau = Tau(Fecha.de.Operacion, Fecha.Pago)) 
  }
  return(tabla)
}

# Primer mes para obtener el Tau Aplicado: 
i <- 1 

# Calculamos los Taus para cada cero cupón:
Tau.aplicado <- bind_rows(lapply(split(Lista.Bonos[[i]], seq(nrow(Lista.Bonos[[i]]))), Tau.total))

# Utilizando la versión original del modelo Nelson-Siegel:

# Función que debe ser minimizada para estimar parámetros usando diferencia máxima:
FuncionObjetivo.NS.Max <- function(X){
  
  # Redefinimos parámetros:
  B0 <- X[1]
  B1 <- TRI.corta[i]-B0
  B2 <- X[2]
  n1 <- X[3] 
  
  # Aplicamos a todo el dataframe y creamos los sumandos:
  DiferenciasPrecio.Max <- Tau.aplicado %>% 
    mutate(monto = ifelse(Fecha.Pago == Fecha.de.Vencimiento, 1+Tasa.facial, Tasa.facial), NelsonSiegel = B0 + B1 * ((1-exp(-Tau/n1))/(Tau/n1)) + 
             B2 * (((1-exp(-Tau/n1))/(Tau/n1)) - exp(-Tau/n1))) %>%
    mutate(FactorDesc = exp(-Tau*NelsonSiegel)) %>% 
    group_by(Numero.de.Contrato.Largo) %>%
    mutate(PrecioTeorico = sum(monto*FactorDesc)) %>% 
    ungroup()
  
  # Se crean las fechas de la curva:
  fecha.inicial <- as.Date(Lista.Bonos[[i]]$Fecha.de.Operacion[1])-day(Lista.Bonos[[i]]$Fecha.de.Operacion[1])+1
  fecha.final <- fecha.inicial+years(5)
  fechas <- seq.Date(from = as.Date(fecha.inicial),
                     to = as.Date(fecha.final),
                     by = "days")
  
  # Se generan las observaciones:
  datos.curva <- data.frame(fechas.iniciales = rep(fecha.inicial,length(fechas)),
                            fechas.finales = fechas) %>% 
    mutate(Tau = Tau(fechas.iniciales, fechas.finales)) %>% 
    mutate(NelsonSiegel = B0 + B1 * ((1-exp(-Tau/n1))/(Tau/n1)) + 
             B2 * (((1-exp(-Tau/n1))/(Tau/n1)) - exp(-Tau/n1))) %>% 
    mutate(tasa = exp(12*NelsonSiegel)-1) 
  datos.curva[1,5] <- exp(12*TRI.corta[[i]])-1
  
  # Verificamos si hay tasas negativas:
  TasaNegativa <- sum(datos.curva$tasa<0)
  
  DiferenciasPrecio.Max <- DiferenciasPrecio.Max %>% 
    select(Numero.de.Contrato.Largo, Mes, PrecioTeorico, Precio, Fecha.de.Vencimiento) %>%
    unique() %>% 
    mutate(Error = abs(PrecioTeorico-Precio)) 
  
  # Se calcula el error total:
  ErrorTotal <- max(DiferenciasPrecio.Max$Error)
  
  # Penalización por no estar en restricciones:
  ErrorTotal <- ifelse((0 < B0) & (0==TasaNegativa), ErrorTotal, ErrorTotal+1e4)
  return(ErrorTotal)
}

# Función que debe ser minimizada para estimar parámetros usando ponderación:
FuncionObjetivo.NS.Pon <- function(X){
  
  # Redefinimos parámetros:
  B0 <- X[1]
  B1 <- TRI.corta[i]-B0
  B2 <- X[2]
  n1 <- X[3] 
  
  # Aplicamos a todo el dataframe y creamos los sumandos:
  DiferenciasPrecio.Pon <- Tau.aplicado %>% 
    mutate(monto = ifelse(Fecha.Pago == Fecha.de.Vencimiento, 1+Tasa.facial, Tasa.facial), NelsonSiegel = B0 + B1 * ((1-exp(-Tau/n1))/(Tau/n1)) + 
             B2 * (((1-exp(-Tau/n1))/(Tau/n1)) - exp(-Tau/n1))) %>%
    mutate(FactorDesc = exp(-Tau*NelsonSiegel)) %>% 
    group_by(Numero.de.Contrato.Largo) %>%
    mutate(PrecioTeorico = sum(monto*FactorDesc)) %>% 
    ungroup() 
  
  # Se crean las fechas de la curva:
  fecha.inicial <- as.Date(Lista.Bonos[[i]]$Fecha.de.Operacion[1])-day(Lista.Bonos[[i]]$Fecha.de.Operacion[1])+1
  fecha.final <- fecha.inicial+years(5)
  fechas <- seq.Date(from = as.Date(fecha.inicial),
                     to = as.Date(fecha.final),
                     by = "days")
  
  # Se generan las observaciones:
  datos.curva <- data.frame(fechas.iniciales = rep(fecha.inicial,length(fechas)),
                            fechas.finales = fechas) %>% 
    mutate(Tau = Tau(fechas.iniciales, fechas.finales)) %>% 
    mutate(NelsonSiegel = B0 + B1 * ((1-exp(-Tau/n1))/(Tau/n1)) + 
             B2 * (((1-exp(-Tau/n1))/(Tau/n1)) - exp(-Tau/n1))) %>% 
    mutate(tasa = exp(12*NelsonSiegel)-1) 
  datos.curva[1,5] <- exp(12*TRI.corta[[i]])-1
  
  # Verificamos si hay tasas negativas:
  TasaNegativa <- sum(datos.curva$tasa<0)
  
  DiferenciasPrecio.Pon <- DiferenciasPrecio.Pon %>%  
    select(Numero.de.Contrato.Largo, Mes, PrecioTeorico, Precio, Fecha.de.Vencimiento, Diff.semana) %>%
    unique() %>% 
    mutate(Ponderador = exp(Diff.semana*alpha)) %>% 
    mutate(Ponderador = Ponderador/sum(Ponderador)) %>% 
    mutate(Error = Ponderador*(PrecioTeorico-Precio)^2) 
  
  # Se calcula el error total:
  ErrorTotal <- sum(DiferenciasPrecio.Pon$Error)
  
  # Penalización por no estar en restricciones:
  ErrorTotal <- ifelse((0 < B0) & (0==TasaNegativa), ErrorTotal, ErrorTotal+1e4)
  return(ErrorTotal)
}

# Utilizando la versión alterada del modelo Svensson:

# Función que debe ser minimizada para estimar parámetros usando diferencia máxima:
FuncionObjetivo.SA.Max <- function(X){

  # Redefinimos parámetros:
  B0 <- X[1]
  B1 <- TRI.corta[i]-B0
  B2 <- X[2]
  B3 <- X[3]
  n1 <- X[4]
  n2 <- X[5]
  
  # Aplicamos a todo el dataframe y creamos los sumandos:
  DiferenciasPrecio.Max <- Tau.aplicado %>% 
    mutate(monto = ifelse(Fecha.Pago == Fecha.de.Vencimiento, 1+Tasa.facial, Tasa.facial), 
           SvenssonAlterada = B0 * Tau +
             B1 * ((1-exp(-Tau/n1))*n1) + 
             B2 * (1-(Tau/n1+1)*exp(-Tau/n1))*n1^2 +
             ((B3*n2*n1)/(n1-n2)) * (((1-(Tau/n1+1)*exp(-Tau/n1))*n1^2) - ((1-(Tau/n2+1)*exp(-Tau/n2))*n2^2))) %>%
    mutate(FactorDesc = exp(-SvenssonAlterada)) %>% 
    group_by(Numero.de.Contrato.Largo) %>%
    mutate(PrecioTeorico = sum(monto*FactorDesc)) %>% 
    ungroup()
  
  # Se crean las fechas de la curva:
  fecha.inicial <- as.Date(Lista.Bonos[[i]]$Fecha.de.Operacion[1])-day(Lista.Bonos[[i]]$Fecha.de.Operacion[1])+1
  fecha.final <- fecha.inicial+years(35)
  fechas <- seq.Date(from = as.Date(fecha.inicial),
                     to = as.Date(fecha.final),
                     by = "days")
  
  # Se generan las observaciones:
  datos.curva <- data.frame(fechas.iniciales = rep(fecha.inicial,length(fechas)),
                            fechas.finales = fechas) %>% 
    mutate(Tau = Tau(fechas.iniciales, fechas.finales)) %>% 
    mutate(SvenssonAlterada = B0 * Tau +
             B1 * ((1-exp(-Tau/n1))*n1) + 
             B2 * (1-(Tau/n1+1)*exp(-Tau/n1))*n1^2 +
             ((B3*n2*n1)/(n1-n2)) * (((1-(Tau/n1+1)*exp(-Tau/n1))*n1^2) - ((1-(Tau/n2+1)*exp(-Tau/n2))*n2^2))) %>% 
    mutate(tasa = exp(12*SvenssonAlterada/Tau)-1) 
  datos.curva[1,5] <- exp(12*TRI.corta[[i]])-1
  
  # Verificamos si hay tasas negativas:
  TasaNegativa <- sum(datos.curva$tasa<0)
  
  DiferenciasPrecio.Max <- DiferenciasPrecio.Max %>% 
    select(Numero.de.Contrato.Largo, Mes, PrecioTeorico, Precio, Fecha.de.Vencimiento) %>%
    unique() %>% 
    mutate(Error = abs(PrecioTeorico-Precio)) 
  
  # Se calcula el error total:
  ErrorTotal <- max(DiferenciasPrecio.Max$Error)
  
  # Penalización por no estar en restricciones:
  ErrorTotal <- ifelse((0 < B0) & (0==TasaNegativa), ErrorTotal, ErrorTotal+1e4)
  return(ErrorTotal)
}

# Función que debe ser minimizada para estimar parámetros usando ponderación:
FuncionObjetivo.SA.Pon <- function(X){
 
  # Redefinimos parámetros:
  B0 <- X[1]
  B1 <- TRI.corta[i]-B0
  B2 <- X[2]
  B3 <- X[3]
  n1 <- X[4]
  n2 <- X[5]
  
  # Aplicamos a todo el dataframe y creamos los sumandos:
  DiferenciasPrecio.Pon <- Tau.aplicado %>% 
    mutate(monto = ifelse(Fecha.Pago == Fecha.de.Vencimiento, 1+Tasa.facial, Tasa.facial), 
           SvenssonAlterada = B0 * Tau +
             B1 * ((1-exp(-Tau/n1))*n1) + 
             B2 * (1-(Tau/n1+1)*exp(-Tau/n1))*n1^2 +
             ((B3*n2*n1)/(n1-n2)) * (((1-(Tau/n1+1)*exp(-Tau/n1))*n1^2) - ((1-(Tau/n2+1)*exp(-Tau/n2))*n2^2))) %>%
    mutate(FactorDesc = exp(-SvenssonAlterada)) %>% 
    group_by(Numero.de.Contrato.Largo) %>%
    mutate(PrecioTeorico = sum(monto*FactorDesc)) %>% 
    ungroup() 
  
  # Se crean las fechas de la curva:
  fecha.inicial <- as.Date(Lista.Bonos[[i]]$Fecha.de.Operacion[1])-day(Lista.Bonos[[i]]$Fecha.de.Operacion[1])+1
  fecha.final <- fecha.inicial+years(5)
  fechas <- seq.Date(from = as.Date(fecha.inicial),
                     to = as.Date(fecha.final),
                     by = "days")
  
  # Se generan las observaciones:
  datos.curva <- data.frame(fechas.iniciales = rep(fecha.inicial,length(fechas)),
                            fechas.finales = fechas) %>% 
    mutate(Tau = Tau(fechas.iniciales, fechas.finales)) %>% 
    mutate(SvenssonAlterada = B0 * Tau +
             B1 * ((1-exp(-Tau/n1))*n1) + 
             B2 * (1-(Tau/n1+1)*exp(-Tau/n1))*n1^2 +
             ((B3*n2*n1)/(n1-n2)) * (((1-(Tau/n1+1)*exp(-Tau/n1))*n1^2) - ((1-(Tau/n2+1)*exp(-Tau/n2))*n2^2))) %>% 
    mutate(tasa = exp(12*SvenssonAlterada/Tau)-1) 
  datos.curva[1,5] <- exp(12*TRI.corta[[i]])-1
  
  # Verificamos si hay tasas negativas:
  TasaNegativa <- sum(datos.curva$tasa<0)
  
  DiferenciasPrecio.Pon <- DiferenciasPrecio.Pon %>% 
    select(Numero.de.Contrato.Largo, Mes, PrecioTeorico, Precio, Fecha.de.Vencimiento, Diff.semana) %>%
    unique() %>% 
    mutate(Ponderador = exp(Diff.semana*alpha)) %>% 
    mutate(Ponderador = Ponderador/sum(Ponderador)) %>% 
    mutate(Error = Ponderador*(PrecioTeorico-Precio)^2) 
  
  # Se calcula el error total:
  ErrorTotal <- sum(DiferenciasPrecio.Pon$Error)
  
  # Penalización por no estar en restricciones:
  ErrorTotal <- ifelse((0 < B0) & (0==TasaNegativa), ErrorTotal, ErrorTotal+10000)
  return(ErrorTotal)
}


#---------------------------------------- Prueba para Optimizadores:

# Limite superior del eta_1:
lim.n <- 60

# Limite tasa larga:
lim.tl <- log(20/100+1)/12

# Limite beta1:
lim.beta <- log(0.5+1)/12

# Betas Iniciales:
Beta2Inicial <- 0
Beta3Inicial <- 0

## Particle Swarm Optimization - Nelson Siegel

tic()
# Realizamos la optimización con función objetivo de Ponderación:
prueba.NS.pon.pso <- psoptim(par = c(TRI.larga, Beta2Inicial, (3+lim.n)/2),
                             fn = FuncionObjetivo.NS.Pon,
                             lower = c(TRI.larga, -lim.beta, 3),
                             upper = c(lim.tl, lim.beta, lim.n),
                             control = list(maxit = 1000,s = 15,w = -0.1832,c.p =0.5287,c.g = 3.1913))
toc()

## Particle Swarm Optimization - Svensson

tic()
# Realizamos la optimización con función objetivo de Ponderación:
prueba.SA.pon.pso <- psoptim(par = c(TRI.larga, Beta2Inicial, Beta3Inicial, (3 + 2*12)/2, (lim.n+3*12)/2),
                             fn = FuncionObjetivo.SA.Pon,
                             lower = c(TRI.larga, -lim.beta, -lim.beta, 3, 3*12),
                             upper = c(lim.tl, lim.beta, lim.beta, 2*12, lim.n),
                             control = list(maxit = 1000,s = 20,w = -0.1832,c.p =0.5287,c.g = 3.1913))
toc()

## Simulated Annealing - Nelson Siegel

tic()
# Realizamos la optimización con función objetivo de Ponderación:
prueba.ns.pon <- optim_sa(fun = FuncionObjetivo.NS.Pon,
                          start = c(TRI.larga, Beta2Inicial, (3+lim.n)/2),
                          trace = TRUE,
                          lower = c(TRI.larga, -lim.beta, 3),
                          upper = c(lim.tl, lim.beta, lim.n),
                          control = list(nlimit = 1000))
toc()

## Simulated Annealing - Svensson

# Realizamos la optimización con función objetivo de Ponderación:
tic()
prueba.SA.pon <- optim_sa(fun = FuncionObjetivo.SA.Pon,
                          start = c(TRI.larga, Beta2Inicial, Beta3Inicial, (3 + 2*12)/2, (lim.n+3*12)/2),
                          trace = TRUE,
                          lower = c(TRI.larga, -lim.beta, -lim.beta, 3, 3*12),
                          upper = c(lim.tl, lim.beta, lim.beta, 2*12, lim.n),
                          control = list(nlimit = 1000))
toc()

## Nelder-Mead - Nelson Siegel 

A=matrix(c(1,0,0,0,1,0,0,0,1,-1,0,0,0,-1,0,0,0,-1),nrow = 6,byrow = T)
B=c(TRI.larga*0.99, -lim.beta, 3,-lim.tl, -lim.beta, -lim.n)
tic()
# Realizamos la optimización con función objetivo de Ponderación:
prueba.NS.pon.nm <- constrOptim(theta = c(TRI.larga, Beta2Inicial, (3+lim.n)/2), 
                             f = FuncionObjetivo.NS.Pon, ui=A,
                             ci=B, control = list(maxit=1000),
                             method = "Nelder-Mead")
toc() 

## Nelder-Mead - Svensson 

A=matrix(c(1,0,0,0,0,0,1,0,0,0,0,0,1,0,0,0,0,0,1,0,0,0,0,0,1,-1,0,0,0,0,0,-1,0,0,0,0,0,-1,0,0,0,0,0,-1,0,0,0,0,0,-1),nrow = 10,byrow = T)
B=c(TRI.larga*0.99, -lim.beta, -lim.beta, 3, 3*12, -lim.tl, -lim.beta, -lim.beta, -2*12, -lim.n)
tic()
# Realizamos la optimización con función objetivo de Ponderación:
prueba.SA.pon.nm <- constrOptim(theta = c(TRI.larga, Beta2Inicial, Beta3Inicial,(3+2*12)/2, (lim.n+3*12)/2), 
                             f = FuncionObjetivo.SA.Pon, ui=A,
                             ci=B, control = list(maxit=1000),
                             method = "Nelder-Mead")
toc() 

## Genetic Algorithm - Nelson Siegel

tic()
# Realizamos la optimización con función objetivo de Ponderación:
prueba.NS.pon.ga <- ga(type = "real-valued",fitness = FuncionObjetivo.NS.Pon, 
                    lower = c(TRI.larga, -lim.beta, 3),
                    upper = c(lim.tl, lim.beta, lim.n),maxiter=1000)
toc()  

## Genetic Algorithm - Svensson

tic()
# Realizamos la optimización con función objetivo de Ponderación:
prueba.NS.pon.ga <- ga(type = "real-valued",fitness = FuncionObjetivo.SA.Pon, 
                       lower = c(TRI.larga, -lim.beta, -lim.beta, 3, 3*12),
                       upper = c(lim.tl, lim.beta, lim.beta, 2*12, lim.n),maxiter=1000)
toc()

#---------------------------------------- Calibración de la Constante de Ponderación:

# Inicialización del punto inicial de la calibración:
Beta0Inicial <- TRI.larga
Beta3Inicial <- 0
Beta2Inicial <- 0
Beta1Inicial <- 0
lim.n <- 60
lim.beta <- log(0.5+1)/12
Eta1Inicial <- (3+2*12)/2
Eta2Inicial <- (lim.n+3*12)/2
lim.tl.sup <- log(20/100+1)/12
lim.tl.inf <- Beta0Inicial*0.99

# Función que calibrar visualiza los errores semanales por contante:
Calibrar.error.SA <- function(alpha.cal){

  # Función que debe ser minimizada para estimar parámetros usando ponderación:
  FuncionObjetivo.SA.Pon.cal <- function(X){

    # Redefinimos parámetros:
    B0 <- X[1]
    B1 <- TRI.corta[i]-B0
    B2 <- X[2]
    B3 <- X[3]
    n1 <- X[4]
    n2 <- X[5]
    
    # Aplicamos a todo el dataframe y creamos los sumandos:
    DiferenciasPrecio.Pon <- Tau.aplicado %>% 
      mutate(monto = ifelse(Fecha.Pago == Fecha.de.Vencimiento, 1+Tasa.facial, Tasa.facial), 
             SvenssonAlterada = B0 * Tau +
               B1 * ((1-exp(-Tau/n1))*n1) + 
               B2 * (1-(Tau/n1+1)*exp(-Tau/n1))*n1^2 +
               ((B3*n2*n1)/(n1-n2)) * (((1-(Tau/n1+1)*exp(-Tau/n1))*n1^2) - ((1-(Tau/n2+1)*exp(-Tau/n2))*n2^2))) %>%
      mutate(FactorDesc = exp(-SvenssonAlterada)) %>% 
      group_by(Numero.de.Contrato.Largo) %>%
      mutate(PrecioTeorico = sum(monto*FactorDesc)) %>% 
      ungroup() %>% 
      select(Numero.de.Contrato.Largo, Mes, PrecioTeorico, Precio, Fecha.de.Vencimiento, Diff.semana) %>%
      unique() %>% 
      mutate(Ponderador = exp(Diff.semana*alpha.cal)) %>% 
      mutate(Ponderador = Ponderador/sum(Ponderador)) %>% 
      mutate(Error = Ponderador*(PrecioTeorico-Precio)^2) 
    
    # Se crean las fechas de la curva:
    fecha.inicial <- as.Date(Lista.Bonos[[i]]$Fecha.de.Operacion[1])-day(Lista.Bonos[[i]]$Fecha.de.Operacion[1])+1
    fecha.final <- fecha.inicial+years(5)
    fechas <- seq.Date(from = as.Date(fecha.inicial),
                       to = as.Date(fecha.final),
                       by = "days")
    
    # Se generan las observaciones:
    datos.curva <- data.frame(fechas.iniciales = rep(fecha.inicial,length(fechas)),
                              fechas.finales = fechas) %>% 
      mutate(Tau = Tau(fechas.iniciales, fechas.finales)) %>% 
      mutate(SvenssonAlterada = B0 * Tau +
               B1 * ((1-exp(-Tau/n1))*n1) + 
               B2 * (1-(Tau/n1+1)*exp(-Tau/n1))*n1^2 +
               ((B3*n2*n1)/(n1-n2)) * (((1-(Tau/n1+1)*exp(-Tau/n1))*n1^2) - ((1-(Tau/n2+1)*exp(-Tau/n2))*n2^2))) %>% 
      mutate(tasa = exp(12*SvenssonAlterada/Tau)-1) 
    datos.curva[1,5] <- exp(12*TRI.corta[[i]])-1
    
    # Verificamos si hay tasas negativas:
    TasaNegativa <- sum(datos.curva$tasa<0)
    
    # Se calcula el error total:
    ErrorTotal <- sum(DiferenciasPrecio.Pon$Error)
    
    # Penalización por no estar en restricciones:
    ErrorTotal <- ifelse((0 < B0) & (0==TasaNegativa), ErrorTotal, ErrorTotal+1e4)
    return(ErrorTotal)
  }
  
  A=matrix(c(1,0,0,0,0,0,1,0,0,0,0,0,1,0,0,0,0,0,1,0,0,0,0,0,1,-1,0,0,0,0,0,-1,0,0,0,0,0,-1,0,0,0,0,0,-1,0,0,0,0,0,-1),nrow = 10,byrow = T)
  B=c(lim.tl.inf, -lim.beta, -lim.beta, 3, 3*12, -lim.tl.sup, -lim.beta, -lim.beta, -2*12, -lim.n)
  
  # Realizamos la optimización con función objetivo de Ponderación:
  calibracion.SA.pon.nm <- constrOptim(theta = c(Beta0Inicial, Beta2Inicial, Beta3Inicial, Eta1Inicial, Eta2Inicial), 
                                        f = FuncionObjetivo.SA.Pon.cal, ui=A,
                                        ci=B, control = list(maxit=1000),
                                        method = "Nelder-Mead")
  
  # Redefinimos parámetros:
  B0 <- calibracion.SA.pon.nm$par[1]
  B1 <- TRI.corta[i]-B0
  B2 <- calibracion.SA.pon.nm$par[2]
  B3 <- calibracion.SA.pon.nm$par[3]
  n1 <- calibracion.SA.pon.nm$par[4]
  n2 <- calibracion.SA.pon.nm$par[5]
  
  # Se recaluclan las diferencias para generar el error máximo con los mismos parámetros:
  DiferenciasPrecio.Max.cal <- Tau.aplicado %>% 
    mutate(monto = ifelse(Fecha.Pago == Fecha.de.Vencimiento, 1+Tasa.facial, Tasa.facial), 
           SvenssonAlterada = B0 * Tau +
             B1 * ((1-exp(-Tau/n1))*n1) + 
             B2 * (1-(Tau/n1+1)*exp(-Tau/n1))*n1^2 +
             ((B3*n2*n1)/(n1-n2)) * (((1-(Tau/n1+1)*exp(-Tau/n1))*n1^2) - ((1-(Tau/n2+1)*exp(-Tau/n2))*n2^2))) %>%
    mutate(FactorDesc = exp(-SvenssonAlterada)) %>% 
    group_by(Numero.de.Contrato.Largo) %>%
    mutate(PrecioTeorico = sum(monto*FactorDesc)) %>% 
    ungroup() %>% 
    select(Numero.de.Contrato.Largo, Mes, PrecioTeorico, Precio, Fecha.de.Vencimiento, Diff.semana) %>%
    unique() %>% 
    group_by(Diff.semana) %>% 
    mutate(Error = abs(PrecioTeorico-Precio)) %>% 
    mutate(Error.max = max(Error)) %>% 
    select(Error.max,Diff.semana) %>% ungroup() %>%
    unique() %>% mutate(alpha = alpha.cal)
  
  # Se calcula el error máximo de la minimización del error ponderado:
  Error.Max.Cal <- DiferenciasPrecio.Max.cal
  return(Error.Max.Cal)
}

# Inicializamos variables para calibración:
alphas <- seq(0,40, 2)
lista.graf.cal <- list()

for(i in 1:(length(Lista.Bonos)-1)){
  
  # Calculamos los Taus para cada cero cupón:
  Tau.aplicado <- bind_rows(lapply(split(Lista.Bonos[[i]], seq(nrow(Lista.Bonos[[i]]))), Tau.total))
  
  # Generamos puntos de 0 a 10:
  serie.cal <- data.frame()
  for(al in alphas){
    serie.cal <- rbind(serie.cal,Calibrar.error.SA(al))
    }
  serie.cal <- serie.cal %>% mutate(Diff.semana = as.factor(Diff.semana))
  
  # Gráfico sobre errores por semana:
  graf.cal <- ggplot(data = serie.cal, aes(x = alpha, y = Error.max, colour = Diff.semana)) +
    geom_line(size = 0.7) + ylab("Error Máximo") +
    geom_point() +
    theme_light() +
    labs(title = "Constante de Ponderación",color = "Semana de
       \r Operación")

  # Visualizamos:
  lista.graf.cal[[i]] <- graf.cal
}

#---------------------------------------- Creación de la Curva para cada Mes:

# Función para generar una curva cero cupón con Nelson-Siegel:
Curva.NS <- function(X, fecha.inicial, fecha.final){
 
  # Redefinimos parámetros:
  B0 <- X[1]
  B1 <- TRI.corta[i]-B0
  B2 <- X[2]
  n1 <- X[3]
  
  # Se crean las fechas de la curva:
  fechas <- seq.Date(from = as.Date(fecha.inicial),
                     to = as.Date(fecha.final),
                     by = "days")
  # Se generan las observaciones:
  datos.curva.NS <- data.frame(fechas.iniciales = rep(fecha.inicial,length(fechas)),
                            fechas.finales = fechas) %>% 
    mutate(Tau = Tau(fechas.iniciales, fechas.finales)) %>% 
    mutate(NelsonSiegel = B0 + B1 * ((1-exp(-Tau/n1))/(Tau/n1)) + 
             B2 * (((1-exp(-Tau/n1))/(Tau/n1)) - exp(-Tau/n1))) %>% 
    mutate(tasa = exp(12*NelsonSiegel)-1) 
  datos.curva.NS[1,5] <- exp(12*TRI.corta[[i]])-1
  
  # Se crea la serie de tiempo:
  resultado <- xts(datos.curva.NS$tasa*100, order.by = datos.curva.NS$fechas.finales)
  return(resultado)
}

# Función para generar una curva cero cupón con Svensson Alterado:
Curva.SA <- function(X, fecha.inicial, fecha.final){

  # Redefinimos parámetros:
  B0 <- X[1]
  B1 <- TRI.corta[i]-B0
  B2 <- X[2]
  B3 <- X[3]
  n1 <- X[4]
  n2 <- X[5]
  
  # Se crean las fechas de la curva:
  fechas <- seq.Date(from = as.Date(fecha.inicial),
                     to = as.Date(fecha.final),
                     by = "days")
  # Se generan las observaciones:
  datos.curva.SA <- data.frame(fechas.iniciales = rep(fecha.inicial,length(fechas)),
                            fechas.finales = fechas) %>% 
    mutate(Tau = Tau(fechas.iniciales, fechas.finales)) %>% 
    mutate(SvenssonAlterada = B0 * Tau +
             B1 * ((1-exp(-Tau/n1))*n1) + 
             B2 * (1-(Tau/n1+1)*exp(-Tau/n1))*n1^2 +
             ((B3*n2*n1)/(n1-n2)) * (((1-(Tau/n1+1)*exp(-Tau/n1))*n1^2) - ((1-(Tau/n2+1)*exp(-Tau/n2))*n2^2))) %>% 
    mutate(tasa = exp(12*SvenssonAlterada/Tau)-1) 
  datos.curva.SA[1,5] <- exp(12*TRI.corta[[i]])-1
  
  # Se crea la serie de tiempo:
  resultado <- xts(datos.curva.SA$tasa*100, order.by = datos.curva.SA$fechas.finales)
  return(resultado)
}

# Incializa lista de resultados:
Curvas.Terminadas <- list()

# Inicializacion de alpha ideal:
alpha <- 5

# Inicialización del punto inicial de la calibración anterior:
Beta3Inicial <- 0
Beta2Inicial <- 0
Beta1Inicial <- 0

# Inicialización Límite superior del eta1:
lim.n <- 60

# Inicialización Límite beta1:
lim.beta <- log(0.5+1)/12

# Optimizamos la función objetivo para cada mes en los datos:
for (i in 1:length(Lista.Bonos)) {

  # Calculamos los Taus para cada cero cupón:
  Tau.aplicado <- bind_rows(lapply(split(Lista.Bonos[[i]], seq(nrow(Lista.Bonos[[i]]))), Tau.total))

  # Definimos parámetros inciales:
  if(i == 1){
    Eta1Inicial <- (3+2*12)/2
    Eta2Inicial <- (lim.n+3*12)/2
    Beta0Inicial <- TRI.larga
    lim.tl.sup <- log(20/100+1)/12
    lim.tl.inf <- Beta0Inicial*0.99
  }else{
    Eta1Inicial <- parametros.definitivos$par[4]
    Eta2Inicial <- parametros.definitivos$par[5]
    Beta0Inicial <- parametros.definitivos$par[1]
    lim.tl.sup <- (1+2/100)*Beta0Inicial
    lim.tl.inf <- (1-2/100)*Beta0Inicial
  }
  
  A=matrix(c(1,0,0,0,0,0,1,0,0,0,0,0,1,0,0,0,0,0,1,0,0,0,0,0,1,-1,0,0,0,0,0,-1,0,0,0,0,0,-1,0,0,0,0,0,-1,0,0,0,0,0,-1),nrow = 10,byrow = T)
  B=c(lim.tl.inf, -lim.beta, -lim.beta, 3, 3*12, -lim.tl.sup, -lim.beta, -lim.beta, -2*12, -lim.n)

  # Realizamos la optimización con función objetivo de Ponderación:
  parametros.definitivos <- constrOptim(theta = c(Beta0Inicial, Beta2Inicial, Beta3Inicial, Eta1Inicial, Eta2Inicial), 
                                  f = FuncionObjetivo.SA.Pon, ui=A,
                                  ci=B, control = list(maxit=1000),
                                  method = "Nelder-Mead")
  
  #---------------------------------------- Visualización:  
  
  # Definimos el tiempo de la serie:
  Fecha.Inicial <- as.Date(Lista.Bonos[[i]]$Fecha.de.Operacion[1])-day(Lista.Bonos[[i]]$Fecha.de.Operacion[1])+1
  Fecha.Final <- Fecha.Inicial+years(35)
  
  # Definimos la serie de tiempo:
  curva.SA <- Curva.SA(parametros.definitivos$par,Fecha.Inicial,Fecha.Final)
  
  # Gráfico de las curvas más reciente:
  graf.SA <- dygraph(curva.SA,
                     main = "Curva Cero Cupón - Svensson Modificado", 
                     xlab = "Fecha", ylab = "Tasa Equivalente Anual",width = "100%") %>% 
    dySeries("V1", label = "Tasa")
  
  # Cálculo de Error Máximo:
  errorMax <- FuncionObjetivo.SA.Max(parametros.definitivos$par)
  
  # Guardamos resultados:
  Curvas.Terminadas[[i]] <- list(Parametros = parametros.definitivos$par,
                                 ErrorPon = parametros.definitivos$value,
                                 ErrorMax = errorMax,
                                 Curva = as.data.frame(curva.SA),
                                 Grafico = graf.SA)
}

#---------------------------------------- Gráfico de las Tir - Comparación a corto plazo:

# Inicialización de la lista de Gráficos:
TIR.Terminadas <- list()

# Función para calcular la TIR de un bono:
TIR <- function(fila){
  fila <- Tau.total(fila) %>% arrange(Fecha.Pago)
  calculo.tir <- function(tir){
    fila$Precio[1]-sum((rep(fila$Tasa.facial[1],length(fila$Tau))+c(rep(0,length(fila$Tau)-1),1))*(1+tir)^(-fila$Tau/12))
  }
  resultado <- nleqslv(0.05, # Este es el valor inicial para la búsqueda de la tir.
                       calculo.tir)
  fila <- fila %>% mutate(TIR = resultado$x)
  return(fila)
}

# Cálculo para cada uno de los meses:
for (i in 1:length(Lista.Bonos)) {
 
  # Calculamos la TIR para cada bono para observación:
  Tir.aplicado <- bind_rows(lapply(split(Lista.Bonos[[i]], seq(nrow(Lista.Bonos[[i]]))), TIR)) %>% group_by(Numero.de.Contrato.Largo) %>%
    mutate(cant= n()) %>% ungroup() %>% filter(cant ==1) %>% 
    select(-Fecha.Pago) %>% unique()
  
  data.corta <- Tir.aplicado %>% select(Fecha.de.Vencimiento, TIR, Diff.semana) %>% 
    left_join(Curvas.Terminadas[[i]]$Curva %>% mutate(Fecha.de.Vencimiento = as.Date(rownames(Curvas.Terminadas[[i]]$Curva))), by = "Fecha.de.Vencimiento") %>% 
    mutate(Curva = V1, TIR = TIR*100, Diff.semana = as.factor(Diff.semana)) %>% select(-V1)
  
  # Visualizamos el Corto Plazo:
  graf.corta <- ggplot(data=data.corta, aes(x=Fecha.de.Vencimiento, y=TIR,colour = Diff.semana)) +
    geom_point() +
    geom_line(aes(y=Curva), color = "red") +
    labs(color = "Semana de
       \r Operación", x = "Fecha de Vencimiento", "TIR") 
  
  TIR.Terminadas[[i]] <- graf.corta
}
