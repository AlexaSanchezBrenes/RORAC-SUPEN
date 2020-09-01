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
library(dygraphs)
library(xts)
library(stringi)
library(dplyr)
library("tools")
library(stringr)
library(lubridate)
library(pso)
options(stringsAsFactors = FALSE)

#---------------------------------------- Parámetros Generales:

# Dirrección de los datos:
Dic <- "C:/Users/EQUIPO/Desktop/Estudios/RORAC-SUPEN/Boletas"

# Vector de tasas cortas mensuales TRI:
TRI.inicial <- 2.75/100

# Tasa de último vencimiento TRI en el primer mes observado:
TRI.final <- 9.42/100

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
  
  NelsonSiegel <- B0 + B1 * ((1-exp(-Tau(t, Te)))/(Tau(t,Te)/n1)) + 
    B2 * (((1-exp(-Tau(t, Te)))/(Tau(t,Te)/n1)) - exp(-Tau(t,Te)/n1))
  
  Factor <- exp(-Tau(t,Te)*NelsonSiegel)
  
  return(Factor)
}

# Función para generar una curva precio bono cero cupón:
Curva.precio <- function(X, fecha.inicial, fecha.final){

  # Redefinimos parámetros:
  B0 <- X[1]
  B1 <- X[2]
  B2 <- X[3]
  n1 <- X[4]
  
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
    mutate(NelsonSiegel = B0 + B1 * ((1-exp(-Tau))/(Tau/n1)) + 
             B2 * (((1-exp(-Tau))/(Tau/n1)) - exp(-Tau/n1))) %>% 
    mutate(Precio = exp(-Tau*NelsonSiegel)) %>% 
    mutate(Precio = ifelse(is.na(Precio),1,Precio))
  
  # Se crea la serie de tiempo:
  resultado <- xts(datos.curva$Precio, order.by = datos.curva$fechas.finales)
  return(resultado)
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

# Función para calcular los tiempos de cada cero cupon en cuponado:
Tau.total <- function(fila){
  
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

# Función que debe ser minimizada para estimar parámetros:
FuncionObjetivo <- function(X){
  # Redefinimos parámetros:
  B0 <- X[1]
  B1 <- X[2]
  B2 <- X[3]
  n1 <- X[4] 
  
  # Aplicamos a todo el dataframe y creamos los sumandos:
  DiferenciasPrecio <- bind_rows(lapply(split(Lista.Bonos[[i]], seq(nrow(Lista.Bonos[[i]]))), Tau.total)) %>% 
    mutate(monto = ifelse(Fecha.Pago == Fecha.de.Vencimiento, 1+Tasa.facial, Tasa.facial), NelsonSiegel = B0 + B1 * ((1-exp(-Tau))/(Tau/n1)) + 
             B2 * (((1-exp(-Tau))/(Tau/n1)) - exp(-Tau/n1))) %>%
    mutate(FactorDesc = exp(-Tau*NelsonSiegel)) %>% 
    group_by(Numero.de.Contrato.Largo) %>%
    mutate(PrecioTeorico = sum(monto*FactorDesc)) %>% 
    ungroup() %>% 
    mutate(Ponderador = exp(-as.double(difftime(ymd(Fecha.de.Vencimiento),
                                                ymd(Fecha.de.Operacion),
                                                units = "days"))/360*alpha)) %>%
    select(Numero.de.Contrato.Largo, Mes, PrecioTeorico, Precio, Ponderador) %>% 
    unique() %>% 
    mutate(Ponderador = Ponderador/sum(Ponderador)) %>%
    mutate(Error = Ponderador*(PrecioTeorico-Precio)^2) %>% 
    select(Numero.de.Contrato.Largo,Mes,Error)
  
  # Se calcula el error total:
  ErrorTotal <- sum(DiferenciasPrecio$Error)
  
  # Penalización por no estar en restricciones:
  if(abs(TRI.inicial - (B0+B1)) > 1e-13 | 0 >= B0){
    ErrorTotal <- ErrorTotal+1e4
  }
  return(ErrorTotal)
}

#---------------------------------------- Optimización de Parámetros:

# Optimizamos la función objetivo para cada mes en los datos:
for (i in 1:length(Lista.Bonos)) {
  i <- 1
  # Limite superior del eta_1:
  lim.n <- as.double(difftime(ymd(max(Lista.Bonos[[i]]$Fecha.de.Operacion)),
                    ymd(min(Lista.Bonos[[i]]$Fecha.de.Operacion)),
                    units = "days"))
  
  tictoc::tic()
  # Realizamos la optimización:
  parametros <- psoptim(par = c(TRI.final, TRI.inicial-TRI.final, 0, lim.n/2),
                        fn = FuncionObjetivo1,
                        lower = c((1-2/100)*TRI.final, -1, -1, 0),
                        upper = c((1+2/100)*TRI.final, 1, 1, lim.n),
                        control = list(s = 100, hybrid = TRUE))
  tictoc::toc()

}

#---------------------------------------- Visualización:

# Ejemplo de parámetros:
X <- c(0.09, -0.02, 0.05, 96)

# Definimos la serie de tiempo reciente:
curva.pre <- Curva.precio(X,"2020-08-01","2025-08-01")

# Grafico de las curvas más reciente:
graf.pre <- dygraph(curva.pre,main = "Curva Precio Bono Cero Cupón", 
                         xlab = "Fecha", ylab = "Factor de Descuento Esperado",width = "100%") %>% 
  dySeries("V1", label = "Precio") %>% 
  dyOptions(size=2)

# Visualizamos:
graf.pre
