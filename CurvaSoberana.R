#              
#                                    Consultoría RORAC-SUPEN
#      Función a optimizar para obtener la curva cero cupón mediante el Modelo de Nelson-Siegel

# Autores:
# Alexa Sánchez
# Laura Campos
# Isaac Z. Arias

# El siguiente módulo representa la función de error cuadrático medio que se debe minimizar para 
# obtener los parámetros óptimos de la curva cero cupón para cada mes según Modelo de Nelson-Siegel.


# Paquetes necesarios:
library(readr)
library(optimization)
library(dygraphs)
library(xts)
library(tictoc)
library(stringi)
library(dplyr)
library("tools")
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
TRI.corta <- c(1.25,1.28,1.25,1.26,0.76,0.75)/100

# Tasa de último vencimiento TRI en el primer mes observado:
TRI.larga <- 9.09/100

# Ajuste de pesos:
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
      select(Numero.de.Contrato.Largo,Periodicidad,Precio, Tasa.facial, Fecha.Ultimo.Pago.Intereses, Fecha.de.Operacion, Fecha.de.Vencimiento) %>% 
      mutate(Fecha.de.Operacion = as.POSIXct(Fecha.de.Operacion, format = "%Y/%m/%d %H:%M:%S"),
             Fecha.de.Vencimiento = as.POSIXct(Fecha.de.Vencimiento, format = "%Y/%m/%d %H:%M:%S"),
             Fecha.Ultimo.Pago.Intereses = as.POSIXct(Fecha.Ultimo.Pago.Intereses, format = "%Y/%m/%d %H:%M:%S"),
             Mes = paste(month(Fecha.de.Operacion),year(Fecha.de.Operacion),sep = "-"),
             Tasa.facial = Tasa.facial/100,
             Precio = Precio/100) %>% 
      arrange(Mes)
  }
  
  names(lista.df) <- archivo  
  return(lista.df)
}

# Se cargan los datos:
Lista.Bonos <- bind_rows(lista.df.boletas()) %>%
  split(., .[, "Mes"])

#---------------------------------------- Definición de la Función Objetivo:

# Función para la convención de fechas:
Tau <- function(t,Te){
  as.double(difftime(ymd(Te),
                     ymd(t),
                     units = "days"))/360 
}

# Factor de descuento utilizando Nelson Siegel: 
FactoresDescuento <- function(X, t, Te){
  
  # Redefinimos parámetros:
  B0 <- X[1]
  B1 <- X[2]
  B2 <- X[3]
  n1 <- X[4] 
  
  NelsonSiegel <- B0 + B1 * ((1-exp(-Tau(t, Te)/n1))/(Tau(t,Te)/n1)) + 
    B2 * (((1-exp(-Tau(t, Te)/n1))/(Tau(t,Te)/n1)) - exp(-Tau(t,Te)/n1))
  
  Factor <- exp(-Tau(t,Te)*NelsonSiegel)
  
  return(Factor)
}

# Funcion para calcular el precio teórico de un bono:
ValorarBonos <- function(FechaUltimoPagoIntereses,FechaOperacion,FechaVencimiento,Periodicidad,TasaFacial,B0,B1,B2,n1){
  
  # Se realiza el calculo segregando por si el bono es cero cupón o no:
  if(Periodicidad==0){
    Precio <- FactoresDescuento(B0,B1,B2,n1,FechaOperacion,FechaVencimiento)
  }else{
    FechasPago <- seq.Date(from = as.Date(FechaUltimoPagoIntereses),
                           to = as.Date(FechaVencimiento),
                           by = paste(as.character(12/Periodicidad),"months",sep=" "))
    FechasPago <- FechasPago[-1]
    FactoresDesc <- Vectorize(FactoresDescuento)(B0,B1,B2,n1,FechaOperacion,FechasPago) 
    
    Precio <- sum(c(rep(TasaFacial,length(FechasPago)-1),1+TasaFacial)*FactoresDesc)
  }
  return(Precio)
}

# Función para generar una curva precio bono cero cupón:
Curva <- function(X, fecha.inicial, fecha.final){
  
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
  datos.curva <- data.frame(fechas.iniciales = rep(fecha.inicial,length(fechas)),
                            fechas.finales = fechas) %>% 
    mutate(Tau = as.double(difftime(ymd(fechas.finales),
                                    ymd(fechas.iniciales),
                                    units = "days"))/360) %>% 
    mutate(NelsonSiegel = B0 + B1 * ((1-exp(-Tau/n1))/(Tau/n1)) + 
             B2 * (((1-exp(-Tau/n1))/(Tau/n1)) - exp(-Tau/n1))) %>% 
    mutate(rho = exp(NelsonSiegel)-1) 
  datos.curva[1,5] <- TRI.corta[[i]]
  
  # Se crea la serie de tiempo:
  resultado <- xts(datos.curva$rho*100, order.by = datos.curva$fechas.finales)
  return(resultado)
}

# Función para calcular los tiempos de cada cero cupon en cuponado:
Tau.total <- function(fila){
  
  # Creamoe el Tau del ponderador:
  fila <- fila %>% 
    mutate(Tau.Vencimiento = as.double(difftime(ymd(as.Date(Fecha.de.Vencimiento)),
                                                ymd(as.Date(Fecha.de.Operacion)),
                                                units = "days"))/360)
  if(fila[,"Periodicidad"]==0){
    tabla <- fila %>% mutate(Tau = as.double(difftime(ymd(Fecha.de.Vencimiento),
                                                      ymd(Fecha.de.Operacion),
                                                      units = "days"))/360,
                             Fecha.Pago = Fecha.de.Vencimiento)
  }else{
    Taus <- seq.Date(from = as.Date(fila[,"Fecha.Ultimo.Pago.Intereses"]),
                     to = as.Date(fila[,"Fecha.de.Vencimiento"]),
                     by = paste(as.character(12/fila[,"Periodicidad"]),"months",sep=" "))[-1]
    tabla <- fila[rep(seq_len(nrow(fila)), each = length(Taus)), ] %>% mutate(Fecha.Pago = Taus) %>% 
      mutate(Tau = as.double(difftime(ymd(Fecha.Pago),
                                      ymd(Fecha.de.Operacion),
                                      units = "days"))/360) 
  }
  return(tabla)
}

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
    ungroup() %>% 
    select(Numero.de.Contrato.Largo, Mes, PrecioTeorico, Precio, Fecha.de.Vencimiento) %>%
    unique() %>% 
    mutate(Error = abs(PrecioTeorico-Precio)) 
  
  # Se calcula el error total:
  ErrorTotal <- max(DiferenciasPrecio.Max$Error)
  
  # Penalización por no estar en restricciones:
  if((0 >= B0) | (n1==0)){
    ErrorTotal <- ErrorTotal+1e4
  }
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
    ungroup() %>% 
    select(Numero.de.Contrato.Largo, Mes, PrecioTeorico, Precio, Fecha.de.Vencimiento, Tau.Vencimiento) %>%
    unique() %>% 
    mutate(Ponderador = exp(Tau.Vencimiento*alpha)) %>% 
    mutate(Ponderador = Ponderador/sum(Ponderador)) %>% 
    mutate(Error = Ponderador*(PrecioTeorico-Precio)^2) 
  
  # Se calcula el error total:
  ErrorTotal <- sum(DiferenciasPrecio.Pon$Error)
  
  # Penalización por no estar en restricciones:
  if((0 >= B0) | (n1==0)){
    ErrorTotal <- ErrorTotal+1e4
  }
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
  n2 <- x[5]
  
  # Aplicamos a todo el dataframe y creamos los sumandos:
  DiferenciasPrecio.Max <- Tau.aplicado %>% 
    mutate(monto = ifelse(Fecha.Pago == Fecha.de.Vencimiento, 1+Tasa.facial, Tasa.facial), 
           SvenssonAlterada = B0 * Tau +
             B1 * ((1-exp(-Tau/n1))*n1) + 
             B2 * (1-(Tau/n1+1)*exp(-Tau/n1))*n1^2 +
             B3 * (((1-(Tau/n1+1)*exp(-Tau/n1))*n1^2) - ((1-(Tau/n2+1)*exp(-Tau/n2))*n2^2))) %>%
    mutate(FactorDesc = exp(-SvenssonAlterada)) %>% 
    group_by(Numero.de.Contrato.Largo) %>%
    mutate(PrecioTeorico = sum(monto*FactorDesc)) %>% 
    ungroup() %>% 
    select(Numero.de.Contrato.Largo, Mes, PrecioTeorico, Precio, Fecha.de.Vencimiento) %>%
    unique() %>% 
    mutate(Error = abs(PrecioTeorico-Precio)) 
  
  # Se calcula el error total:
  ErrorTotal <- max(DiferenciasPrecio.Max$Error)
  
  # Penalización por no estar en restricciones:
  if((0 >= B0) | (n1==0)){
    ErrorTotal <- ErrorTotal+1e4
  }
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
  n2 <- x[5]
  
  # Aplicamos a todo el dataframe y creamos los sumandos:
  DiferenciasPrecio.Pon <- Tau.aplicado %>% 
    mutate(monto = ifelse(Fecha.Pago == Fecha.de.Vencimiento, 1+Tasa.facial, Tasa.facial), 
           SvenssonAlterada = B0 * Tau +
             B1 * ((1-exp(-Tau/n1))*n1) + 
             B2 * (1-(Tau/n1+1)*exp(-Tau/n1))*n1^2 +
             B3 * (((1-(Tau/n1+1)*exp(-Tau/n1))*n1^2) - ((1-(Tau/n2+1)*exp(-Tau/n2))*n2^2))) %>%
    mutate(FactorDesc = exp(-SvenssonAlterada)) %>% 
    group_by(Numero.de.Contrato.Largo) %>%
    mutate(PrecioTeorico = sum(monto*FactorDesc)) %>% 
    ungroup() %>% 
    select(Numero.de.Contrato.Largo, Mes, PrecioTeorico, Precio, Fecha.de.Vencimiento, Tau.Vencimiento) %>%
    unique() %>% 
    mutate(Ponderador = exp(Tau.Vencimiento*alpha)) %>% 
    mutate(Ponderador = Ponderador/sum(Ponderador)) %>% 
    mutate(Error = Ponderador*(PrecioTeorico-Precio)^2) 
  
  # Se calcula el error total:
  ErrorTotal <- sum(DiferenciasPrecio.Pon$Error)
  
  # Penalización por no estar en restricciones:
  if((0 >= B0) | (n1==0)){
    ErrorTotal <- ErrorTotal+1e4
  }
  return(ErrorTotal)
}

#---------------------------------------- Pruebas de tiempo por función:

# Redefinimos puntos iniciales de prueba:
X <- c(0.1109,	5,	0.05695203)

# Primer mes
i <- 1 

# Limite superior del eta_1:
lim.n <- as.double(difftime(ymd(max(Lista.Bonos[[i]]$Fecha.de.Vencimiento)),
                            ymd(min(Lista.Bonos[[i]]$Fecha.de.Operacion)),
                            units = "days"))/360

# Calculamos los Taus para cada cero cupón:
Tau.aplicado <- bind_rows(lapply(split(Lista.Bonos[[i]], seq(nrow(Lista.Bonos[[i]]))), Tau.total))

# Prueba:
tic()
FuncionObjetivo.Max(X)
toc()
tic()
FuncionObjetivo.Pon(X)
toc()

#---------------------------------------- Prueba para Optimizadores:

# Incializa parámetros:
Beta2Inicial <- 0

## Particle Swarm Optimization

tic()
# Realizamos la optimización con función objetivo de Máximo:
prueba.max.pso <- psoptim(par = c(TRI.larga, Beta2Inicial, lim.n/2),
                          fn = FuncionObjetivo.Max,
                          lower = c(-2/100+TRI.larga, -5, 0),
                          upper = c(2/100+TRI.larga, 5, lim.n),
                          control = list(maxit = 1000,s = 15,w = -0.1832,c.p =0.5287,c.g = 3.1913))
toc()
tic()
# Realizamos la optimización con función objetivo de Ponderación:
prueba.pon.pso <- psoptim(par = c(TRI.larga, Beta2Inicial, lim.n/2),
                          fn = FuncionObjetivo.Pon,
                          lower = c(-2/100+TRI.larga, -5, 0),
                          upper = c(2/100+TRI.larga, 5, lim.n),
                          control = list(maxit = 1000,s = 15,w = -0.1832,c.p =0.5287,c.g = 3.1913))
toc()

## Simulated Annealing

tic()
# Realizamos la optimización con función objetivo de Máximo:
prueba.max.sa <- optim_sa(fun = FuncionObjetivo.Max,
                          start = c(TRI.larga, Beta2Inicial, lim.n/2),
                          trace = TRUE,
                          lower = c(-2/100+TRI.larga, -5, 0),
                          upper = c(2/100+TRI.larga, 5, lim.n),
                          control = list(rf = c(0.95, 0.95, 0.95),
                                         nlimit = 1000))
toc()
tic()
# Realizamos la optimización con función objetivo de Ponderación:
prueba.max.sa <- optim_sa(fun = FuncionObjetivo.Pon,
                          start = c(TRI.larga, Beta2Inicial, lim.n/2),
                          trace = TRUE,
                          lower = c(-2/100+TRI.larga, -5, 0),
                          upper = c(2/100+TRI.larga, 5, lim.n),
                          control = list(rf = c(0.95, 0.95, 0.95),
                                         nlimit = 1000))
toc()

## Nelder-Mead

A=matrix(c(1,0,0,0,1,0,0,0,1,-1,0,0,0,-1,0,0,0,-1),nrow = 6,byrow = T)
B=c(-2/100+TRI.larga, -5, 0,-2/100-TRI.larga, -5, -lim.n)

tic()
# Realizamos la optimización con función objetivo de Máximo:
prueba.max.nm <-constrOptim(theta = c(TRI.larga, Beta2Inicial, lim.n/2),
                            f = FuncionObjetivo.Max, grad = NULL, ui=A, 
                            ci=B, control = list(maxit=1000),
                            method = "Nelder-Mead")
toc() 
tic()
# Realizamos la optimización con función objetivo de Ponderación:
prueba.pon.nm <- constrOptim(theta = c(TRI.larga, Beta2Inicial, lim.n/2), 
                             f = FuncionObjetivo.Pon, ui=A,
                             ci=B, control = list(maxit=1000),
                             method = "Nelder-Mead")
toc() 

## Genetic Algorithm 

tic()

# Realizamos la optimización con función objetivo de Máximo:
prueba.max.ga <- ga(type = "real-valued",fitness = FuncionObjetivo.Max, 
                    lower = c(-2/100+TRI.larga, -5, 0),
                    upper = c(2/100+TRI.larga, 5, lim.n),maxiter=1000)
toc() 
tic()
# Realizamos la optimización con función objetivo de Ponderación:
prueba.pon.ga <- ga(type = "real-valued",fitness = FuncionObjetivo.Pon, 
                    lower = c(-2/100+TRI.larga, -5, 0),
                    upper = c(2/100+TRI.larga, 5, lim.n),maxiter=1000)
toc()  

#---------------------------------------- Creación de la Curva para cada Mes:

# Incializa lista de resultados:
Curvas.Terminadas <- list()
Beta2Inicial <- 0

# Optimizamos la función objetivo para cada mes en los datos:
for (i in 1:length(Lista.Bonos)) {
  i <- 1
  # Calculamos los Taus para cada cero cupón:
  Tau.aplicado <- bind_rows(lapply(split(Lista.Bonos[[i]], seq(nrow(Lista.Bonos[[i]]))), Tau.total))
  
  # Limite superior del eta_1:
  lim.n <- as.double(difftime(ymd(max(Lista.Bonos[[i]]$Fecha.de.Vencimiento)),
                              ymd(min(Lista.Bonos[[i]]$Fecha.de.Operacion)),
                              units = "days"))/360
  
  # Definimos el eta1 incial:
  if(i == 1){
    Eta1Inicial <- lim.n/2
  }else{
    Eta1Inicial <- parametros.definitivos$par[3]
  }
  
  # Realizamos la optimización con función objetivo:
  parametros.definitivos <- psoptim(par = c(TRI.larga, Beta2Inicial, Eta1Inicial),
                                    fn = FuncionObjetivo.Pon,
                                    lower = c(-2/100+TRI.larga, -5, 0),
                                    upper = c(2/100+TRI.larga, 5, lim.n),
                                    control = list(maxit = 1000,s = 15,w = -0.1832,c.p =0.5287,c.g = 3.1913))
  
#---------------------------------------- Visualización:  
  
  # Definimos el tiempo de la serie:
  Fecha.Inicial <- as.Date(Lista.Bonos[[i]]$Fecha.de.Operacion[1])-day(Lista.Bonos[[i]]$Fecha.de.Operacion[1])+1
  Fecha.Final <- Fecha.Inicial%m+% years(5) 
  
  # Definimos la serie de tiempo:
  curva.rho <- Curva(c(0.1109, 0.4211761, 2.556457),Fecha.Inicial,Fecha.Final)
  
  # Grafico de las curvas más reciente:
  graf.rho <- dygraph(curva.rho,
                      main = "Curva Cero Cupón", 
                      xlab = "Fecha", ylab = "Tasa Anualizada",width = "100%") %>% 
    dySeries("V1", label = "rho") 
  
  # Guardamos resultados:
  Curvas.Terminadas[[i]] <- list(Parametros = parametros.definitivos$par,
                                 Error = parametros.definitivos$value,
                                 Grafico = graf.rho) 
  
  # Redefinimos puntos iniciales:
  Beta2Inicial <- parametros.definitivos$par[[2]]
}