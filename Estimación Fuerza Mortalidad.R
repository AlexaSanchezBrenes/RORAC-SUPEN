#              
#                    Consultoría RORAC-SUPEN 
#           Estimación de la fuerza de "mortalidad" (default)

# Autores:
# Alexa Sánchez
# Laura Campos
# Isaac Z. Arias

# DESCRIPCIÓN DEL MÓDULO


# Paquetes necesarios:
library(dplyr)
library(xfun)
library(stringr)
library(lubridate)
library(stringi)
library(stats)

# Dirrección de los datos:
Dic <- "C:/Users/EQUIPO/Desktop/Estudios/RORAC-SUPEN/Boletas"


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
      filter(Fecha.de.Vencimiento != "",
             !Nemotecnico.del.Emisor %in% c("BCCR","G"),
             Moneda.del.instrumento == "Colones Costarricenses", 
             !Nemotecnico.del.instrumento %in% c("bemv", "tp$", "tpras", "tptba", "TUDES", "tudes", "bemud", "TPTBA")) %>% 
      select(Numero.de.Contrato.Largo,Periodicidad,Valor.Transado, Tasa.facial, Valor.facial,Fecha.Ultimo.Pago.Intereses, Fecha.de.Operacion, Fecha.de.Vencimiento,Dias.Acumulados.Intereses) %>% 
      mutate(Fecha.de.Operacion = as.POSIXct(Fecha.de.Operacion, format = "%Y/%m/%d %H:%M:%S"),
             Fecha.de.Vencimiento = as.POSIXct(Fecha.de.Vencimiento, format = "%Y/%m/%d %H:%M:%S"),
             Fecha.Ultimo.Pago.Intereses = as.POSIXct(Fecha.Ultimo.Pago.Intereses, format = "%Y/%m/%d %H:%M:%S"),
             Mes = paste(month(Fecha.de.Operacion),year(Fecha.de.Operacion),sep = "-"),
             Tasa.facial = ifelse(Periodicidad==0, Tasa.facial/100, Tasa.facial/100/Periodicidad)) 
#      arrange(Mes) %>% mutate(Precio = ifelse(Periodicidad == 0, # OJO: Precio sucio
#                                              Precio, 
#                                              Precio + Tasa.facial*Dias.Acumulados.Intereses/(360/Periodicidad))) %>% 
#      select(-Dias.Acumulados.Intereses)
  }
  
  names(lista.df) <- archivo  
  return(lista.df)
}

# Se cargan los datos:
Lista.Bonos <- bind_rows(lista.df.boletas()) %>%
  split(., .[, "Mes"])


#---------------------------------------- Parámetros Generales:

# Tasa del primer vencimiento TRI en el primer mes observado (marzo):
TRI.corta <- log(1+1.25/100/52)*52/12

# Fijamos los parámetros de la curva Nelson Siegel o Svensson encontrada:
Par.NS <- c(8.337060e-03, TRI.corta-8.337060e-03, -3.330267e-12, 1.811465e+01)
Par.SA <- c(6.253579e-03,-0.005212038,-2.791880e-05,6.6101177e-06,1.357261e+01,4.806624e+01)

# Parámetro que indica si el modelo utilizado es el Svensson (si es Nelson Siegel fijarlo como 0)
Svensson <- 1 

#---------------------------------------- Funciones del Modelo:

# Función que genera el precio de la curva a un tiempo (Tao) dado:
Precio <- function(tao){
  if(tao == 0){
    precio = 1
  }else{
    if(Svensson == 1){
      Delta = Par.SA[1] * tao +
        Par.SA[2] * ((1-exp(-tao/Par.SA[5]))*Par.SA[5]) + 
        Par.SA[3] * (1-(tao/Par.SA[5]+1)*exp(-tao/Par.SA[5]))*Par.SA[5]^2 +
        ((Par.SA[4]*Par.SA[6]*Par.SA[5])/(Par.SA[5]-Par.SA[6])) * (((1-(tao/Par.SA[5]+1)*exp(-tao/Par.SA[5]))*Par.SA[5]^2) - ((1-(tao/Par.SA[6]+1)*exp(-tao/Par.SA[6]))*Par.SA[6]^2))
      
      precio = exp(-Delta)
    }
    
    else{
      
      # Se calculan primero los coeficientes:
      a = (1-exp(-tao/Par.NS[4]))/
        (tao/Par.NS[4])
      
      b = ((1-exp(-tao/Par.NS[4]))/
             (tao/Par.NS[4])) - exp(-tao/Par.NS[4])
      
      precio = exp(-(Par.NS[1] + Par.NS[2]*a + 
                       Par.NS[3]*b)*tao)
    }}
  return(precio)
}



# Función para calcular la diferencia de fechas en meses:
Tau <- function(t, Te) {
  ed <- as.POSIXlt(Te)
  sd <- as.POSIXlt(t)
  as.double(12 * (ed$year - sd$year) + (ed$mon - sd$mon) + day(ed)/days_in_month(ed) - day(sd)/days_in_month(sd))
}

# Función para calcular los tiempos de cada cero cupon en cuponado:
Tau.total <- function(fila){
  
  # Creamoe el Tau del ponderador:
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


# Primer mes
i <- 1 


# Títulos del mes i 

Bonos<-Lista.Bonos[[i]]

# Calculamos los Taus para cada cero cupón:

#Tau.aplicado <- bind_rows(lapply(split(Lista.Bonos[[i]], seq(nrow(Lista.Bonos[[i]]))), Tau.total))

#---------------------------------------- Definición de la Función Objetivo:



# Función que debe ser minimizada para estimar el parámetro mu:

Optimizacion <- function(fila){
  
tabla<-Tau.total(fila)

FuncionObjetivo<- function(mu){
  
  # Aplicamos a todo el dataframe y creamos los sumandos:
  DiferenciasPrecio <- tabla %>% 
    mutate(monto = ifelse(Fecha.Pago == Fecha.de.Vencimiento, 1+Tasa.facial, Tasa.facial), 
           exponencial = exp(-mu*Tau), 
           Precio = Precio(Tau)) %>% 
    mutate(PrecioTeorico = sum(Valor.facial*monto*Precio*exponencial))
  
  return(abs(sum(DiferenciasPrecio$PrecioTeorico)-DiferenciasPrecio$Valor.facial[1]))
}
 
Resultado<-optim(par = 0,fn = FuncionObjetivo)

fila <- cbind(fila,Resultado$par)

return(fila)
}


Bonos <-lapply(split(Bonos,seq(nrow(Bonos))),Optimizacion)

