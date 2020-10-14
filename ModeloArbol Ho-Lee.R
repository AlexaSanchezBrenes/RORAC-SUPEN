# Modelo de Árbol Binomial Ho-Lee

# Paquetes necesarios:
library(dplyr)
library(tidyr)
library(ggplot2)
library(magrittr)
library(readxl)
library(lubridate)
library(purrr)
library(readxl)
library(ggplot2)
library(stringr)
library(dygraphs)
library(zoo)
library(xts)
library(tictoc)

################## Estimación de parámetros de Ho-Lee - Colones #########################

# Para leer el archivo se le debe cambiar el formato a "xlsx"

TRI_colones <- read_excel("TRI colones.xlsx",
                          col_types = c("date", "numeric"))



secuencia<-seq(from=1,to=nrow(TRI_colones),by=7)
TRI_colones <- TRI_colones[secuencia,]
TRI_colones<-TRI_colones %>% mutate(Delta=log((1+`1 semana`/52)))
varianza_mensual_col<-4*var(TRI_colones$Delta)

u_col<-1+sqrt(varianza_mensual_col)
d_col<-1-sqrt(varianza_mensual_col)





                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                ################## Estimación de parámetros de Ho-Lee - Dólares #########################
# Tasas overnight: Para calcular varianza y r0


Overnight <- read.csv("overnightrate.csv",sep=',',dec='.',header = F)
Overnight1 <- read.csv("overnightrate (1).csv",sep=',',dec='.',header = F)
Overnight2 <- read.csv("overnightrate (2).csv",sep=',',dec='.',header = F)
Overnight3 <- read.csv("overnightrate (3).csv",sep=',',dec='.',header = F)
Overnight4 <- read.csv("overnightrate (4).csv",sep=',',dec='.',header = F)
Overnight5 <- read.csv("overnightrate (5).csv",sep=',',dec='.',header = F)
Overnight6 <- read.csv("overnightrate (6).csv",sep=',',dec='.',header = F)
Overnight7 <- read.csv("overnightrate (7).csv",sep=',',dec='.',header = F)



Overnight<-rbind(Overnight,Overnight1,Overnight2,Overnight3,Overnight4,Overnight5,Overnight6,Overnight7)
Overnight <- Overnight[,-3]
colnames(Overnight)=c('Fecha','Tasa')
Overnight %<>%  mutate(Delta=log((1+Tasa/360)))

varianza_mensual_dol<-30*var(Overnight$Delta)


u_dol<-1+sqrt(varianza_mensual_dol) 
d_dol<-1-sqrt(varianza_mensual_dol)




#-----------------------------------------------------------------------------

# Inicialización de C?digo:

# Fecha_Hoy: Se refiere a la fecha inicial que es el 31-05-2018.   

# IPC_Hoy: Es el tipo de cambio del d?a 31-05-2018 (colones).    

# tiempo_T: Cantidad de meses donde se reaizar? el estudio actuarial.


Fecha_Hoy <- date('2018-05-31')

IPC_Hoy <- 104.66

tiempo_T = 12*15 #PREGUNTAR

#-----------------------------------------------------------------------------

#Ejemplo de parámetros para las curvas del modelo Nelson-Siegel 

P_CCC <- c(0.09, -0.02,  0.05, 96)

#Variables diferencias en dólares y colones:

# Donde la u es la proporción de cuanto sube el indicador respectivo, en el 
# segundo periodo si se indica como: u_2. Por otro lado d es la proporción
# de cuánto baja el indicador respectivo, y se denota d_2 la propoción para
# el segundo periodo. Además, el factor k.

u2 = c(u_col, u_dol)

d2 = c(d_col, d_dol)

k = c(d2[1]/u2[1], d2[2]/u2[2]) 

#-----------------------------------------------------------------------------

# Nelson Siegel

# Objetivo: Encontrar los bonos Cero Cup?n de la curva.

# Parámetros que recibe: la fecha inicial del proyecto (fecha_0), fecha donde 
# se quiere obtener el bono cero cupón (fecha_buscar)

#Devuelve: El precio de un bono cero cupón. 

# Detalle: El Bono Cero Cupón en colones utiliza la curva Nelson-Siegel. Donde se obtiene el valor de $\delta(t)$: 

Nelson_Siegel <- function(tao){
  
  if(tao == 0){
    precio = 1
    
  }else{
    
    a = (1-exp(-tao/P_CCC[4]))/
      (tao/P_CCC[4])
    
    b = ((1-exp(-tao/P_CCC[4]))/
           (tao/P_CCC[4])) - exp(-tao/P_CCC[4])
    
    c = ((1-exp(tao))/
           (tao)) - exp(-tao)
    
    precio = exp(-(P_CCC[1] + P_CCC[2]*a + 
                     P_CCC[3]*b)*tao/12)
    
  }
  
  return(precio)
}

############################### PARA CALCULAR EL P(0,T) PARA LA CURVA EN DÓLARES #######################

#Ubicacion del archivo de tasas spot----------------------------------------------------
Dic<- setwd("~/RORAC-SUPEN")

#Periodo para el cual se quiere extraer el precio cero cupón -----------------
periodo <-"2020-03-01"
#Nombre el archivo. se debe mantener el nombre de la descarga pues indica el periodo
#disponible histórico-------------------------------------------------------------------
archivo <- "tnc_18_22.xls"

#1) Información de las tasas en dólares compuestas semianuales------------------------------ 
data <- read_excel(archivo, 
                   col_names = FALSE, skip = 5) %>% filter_all(any_vars(!is.na(.)))
Maduracion <- data[,1]
data <- data [, -c(1,2)]

fecha.inicial <- ymd(paste0("20",str_sub(archivo, start = 5, end = 6),"-", "01","-","01"))
fecha.final <- ymd(paste0("20",str_sub(archivo, start = 8, end = 9),"-", "12","-","01"))
vec.fechas <- seq.Date(from = fecha.inicial, to = fecha.final, by = "month")
names <- vec.fechas[1:ncol(data)]
colnames(data) <- names

#2) Tasas equivalentes semestrales--------------------------------------------------------------------------


rho<-((1+(data))^(1/6)-1) 

rho$Maduracion <- seq(from = 6, to = 1200, by = 6)



#3)Función para obtener la interpolación de las tasas anualizadas---------------------------
interpolacion<-function(x,y){
  Maduracion = seq(from = x[1], to = x[length(x)], by = 1 )
  ApproxFun <- approxfun(x , y)
  rho <- ApproxFun(Maduracion)
  interpolacion<-data.frame(Maduracion, rho)
  return(interpolacion)
}


tabla <- rho %>% select(all_of(periodo),Maduracion)

tabla<-rbind(c(mean(exp(r0$Tasa[which(month(r0$Fecha)==month(periodo))]/30)-1),0),tabla)

#4)Interpolación de los de las tasas cortas para cada mes--------------------------------------

tabla = interpolacion(tabla$Maduracion, tabla[,1]) 

Precio_dol <- function(tao){
  (1+tabla$rho[which(tabla$Maduracion==tao)])^-tao
}

Precio <- function(tao,col_dol){
  if(col_dol==1){
    P=Nelson_Siegel(tao)
  }
  else{
    P=Precio_dol(tao)
  }
  return(P)
}
#-----------------------------------------------------------------------------

# Decrecimiento del Árbol Binomial


# Objetivo: Encontrar el valor de d(t).    

# Par?metros que recibe: el instante de tiempo que se requiere t, 
# la probabilidad p y el valor de k.

# Devuelve: El resultado obtenido al realizar lo siguiente: 


d_t = function(t, p, k){ 
  (k^(t-1))/((1 - p)*(k^(t-1)) + p)
} 

#-----------------------------------------------------------------------------

# Cálculo de Tasa Corta:

# Objetivo: Encontrar el valor de r_t.    

# Parámetros que recibe: Distancia de tiempo t que se representa por tao, 
# si se quiere obtener la tasa corta de los colones o de los dólares,
# cantidad de veces que ha subido hasta ese momento t y la probabidad p.

# Devuelve: El resultado obtenido al realizar lo siguiente: 

Tasa_Corta_t = function(tao, col_dol, cant_sub, p){
    # Se obtiene el precio de acuerdo a la moneda
  P_0_t = Precio(tao,col_dol)
  P_0_T = Precio(tao + 1,col_dol)

  
  # Se obtiene la tasa corta mediante la fórmula.
  r_t = log(P_0_t/P_0_T) - log(d_t(tao + 1, p, k[col_dol])) + cant_sub*log(k[col_dol])
  
  return(r_t)  
}

#-----------------------------------------------------------------------------

# Árbol Ho-Lee

# Objetivo: Encontrar el valor del árbol Ho Lee de descuentos D(0, t).  

# Parámetros que recibe: El indicador si se quiere obtener la tasa corta 
# de los colones o de los dólares.

# Devuelve: El vector de descuentos D(0,t), con t = 0,1,...,T.

Arbol_Ho_Lee_D = function(col_dol){   
  
  # Dado el tipo de árbol crea la probabilidad
  p = (1 - d2[col_dol])/(u2[col_dol] - d2[col_dol]) 
  
  # Obtiene la tasa corta inicial
  r_0 = Tasa_Corta_t(0, col_dol, 0, p)  
  
  # Forma la trayectoria aleatoria 179 veces
  trayectoria = rbernoulli(tiempo_T - 1, p) 
  
  # Obtiene la cantidad de subidas acumuladas.
  vect_cant_sub = cumsum(trayectoria)
  
  # Aplica la funciÓn de la tasa corta para cada instante.   
  vect_r_t = Vectorize(Tasa_Corta_t)(1:(tiempo_T - 1), col_dol, vect_cant_sub, p) 
  
  # Obtiene el vector de los descuentos D(0,t)
  vect_D_0_T = c(exp(-r_0), exp(-cumsum(vect_r_t))*Precio(1, col_dol)) 
                                                                      
  
  return(vect_D_0_T)
}

#-----------------------------------------------------------------------------

# Prueba del Árbol Binomial Ho-Lee

# Colones:
Arbol_Ho_Lee_D(1)

# Dólares
Arbol_Ho_Lee_D(2)


###################### Simulaciones ################################
Simulaciones<-matrix(nrow = tiempo_T,ncol = 100000)

tic()
for(j in 1:100000){
  Simulaciones[,j]<-Arbol_Ho_Lee_D(2)
}
toc() # 68.08 sec (con 10 000 simulaciones)  662.61 sec (con 100 000 simulaciones) 
 
Simulaciones_promedio<-apply(X = Simulaciones,MARGIN = 1,FUN = mean)


## Comparación con respecto al precio

tabla<-tabla %>% mutate(Precio=(1+rho)^-Maduracion) 
tabla %<>% filter(Maduracion >0 & Maduracion <= length(Simulaciones_promedio)) %>% 
 mutate(SimulacionP=Simulaciones_promedio) 


####--------------------------------------------#####
################### GRÁFICOS ###########################

library(xts)
library(dygraphs)


Fecha<-seq.Date(from = ymd(as.Date(periodo) %m+% months(1)),
                by = "month",
                length.out = length(Simulaciones_promedio))



tabla %<>% mutate(Fecha=Fecha)

series <- xts(x = tabla$SimulacionP, order.by = tabla$Fecha)
dygraph(series, main = "'Simulaciones Promedio") %>%
  dyAxis("x","Fecha", drawGrid = FALSE) 


series <- xts(x = tabla[,3:4], order.by = tabla$Fecha)
dygraph(series, main = "Comparación entre curva precio cero cupón y el modelo Ho-Lee") %>%
  dyAxis("x","Fecha", drawGrid = FALSE) 


## Comparación con respecto a la tasa anualizada

tabla<-tabla %>% mutate(Precio=(1+rho)^-Maduracion) %>% mutate(Delta=-log(Precio)) %>% mutate(Tasa=exp(12*Delta)-1)
tabla %<>% mutate(DeltaSP=-log(SimulacionP)) %>% mutate(TasaSP=exp(12*DeltaSP)-1)

tabla2<-tabla %>% select(Fecha,Tasa,TasaSP)

## Gráfico

series <- xts(x = tabla2[,2:3], order.by = tabla2$Fecha)
dygraph(series, main = "Comparación entre tasa anualizada y el modelo Ho-Lee") %>%
  dyAxis("x","Fecha", drawGrid = FALSE) 




