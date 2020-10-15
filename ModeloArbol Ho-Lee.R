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

################## Estimación de parámetros de Ho-Lee - Dólares #########################

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
Overnight %<>%  mutate(Delta=log((1+Tasa/360))*360/12)

varianza_mensual_dol<-30*var(Overnight$Delta)
u_dol<-1+sqrt(varianza_mensual_dol) 
d_dol<-1-sqrt(varianza_mensual_dol)




#-----------------------------------------------------------------------------

# Inicialización de Código:


# tiempo_T: Cantidad de meses máxima para los que se generan las simulaciones de la curva


tiempo_T = 12*15

#-----------------------------------------------------------------------------

#Ejemplo de parámetros para las curvas del modelo Nelson-Siegel 
periodo <-"2020-03-01"

# Donde la u es la proporción de cuanto sube el indicador respectivo, en el 
# segundo periodo si se indica como: u_2. Por otro lado d es la proporción
# de cuánto baja el indicador respectivo, y se denota d_2 la proporción para
# el segundo periodo. Además, el factor k.

u2 = u_dol

d2 = d_dol

k = d2/u2 


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



#3)Función para obtener la interpolación de las tasas equivalentes mensuales---------------------------
interpolacion<-function(x,y){
  Maduracion = seq(from = x[1], to = x[length(x)], by = 1 )
  ApproxFun <- approxfun(x , y)
  rho <- ApproxFun(Maduracion)
  interpolacion<-data.frame(Maduracion, rho)
  return(interpolacion)
}


tabla <- rho %>% select(all_of(periodo),Maduracion)

r0<-Overnight

r0$Fecha<-as.Date(r0$Fecha,format = '%m/%d/%Y')
r0<-r0 %>% filter(year(Fecha)>=2020)

tabla<-rbind(c(mean((1+r0$Tasa[which((month(r0$Fecha)==month(periodo) & year(r0$Fecha)==year(periodo)))]/360)^(360/12)-1),0),tabla) #REVISAR

#4)Interpolación de las tasas equivalentes mensuales--------------------------------------

tabla = interpolacion(tabla$Maduracion, tabla[,1]) 

Precio <- function(tao){
  (1+tabla$rho[which(tabla$Maduracion==tao)])^-tao
}

(1+rho)^

#-----------------------------------------------------------------------------

# Decrecimiento del Árbol Binomial


# Objetivo: Encontrar el valor de d(t).    

# Parámetros que recibe: el instante de tiempo que se requiere t, 
# la probabilidad p y el valor de k.

# Devuelve: El resultado obtenido al realizar lo siguiente: 


d_t = function(t, p, k){ 
  (k^(t-1))/((1 - p)*(k^(t-1)) + p)
} 

#-----------------------------------------------------------------------------

# Cálculo de Tasa Corta:

# Objetivo: Encontrar el valor de r_t.    

# Parámetros que recibe: Distancia de tiempo t que se representa por tao, 
# cantidad de veces que ha subido hasta ese momento t y la probabidad p.

# Devuelve: El resultado obtenido al realizar lo siguiente: 

Tasa_Corta_t = function(tao, cant_sub, p){
    # Se obtiene el precio 
  P_0_t = Precio(tao)
  P_0_T = Precio(tao + 1)

  
  # Se obtiene la tasa corta mediante la fórmula.
  r_t = log(P_0_t/P_0_T) - log(d_t(tao + 1, p, k)) + cant_sub*log(k)
  
  return(r_t)  
}

#-----------------------------------------------------------------------------

# Árbol Ho-Lee

# Objetivo: Encontrar el valor del árbol Ho Lee de descuentos D(0, t).  

# Devuelve: El vector de descuentos D(0,t), con t = 0,1,...,T.

Arbol_Ho_Lee_D = function(){   
  
  # Dado el tipo de árbol crea la probabilidad
  p = (1 - d2)/(u2 - d2) 
  
  # Obtiene la tasa corta inicial
  r_0 = Tasa_Corta_t(0,0,p)  
  
  # Forma la trayectoria aleatoria 179 veces
  trayectoria = rbernoulli(tiempo_T - 1, p) 
  
  # Obtiene la cantidad de subidas acumuladas.
  vect_cant_sub = cumsum(trayectoria)
  
  # Aplica la funciÓn de la tasa corta para cada instante.   
  vect_r_t = Vectorize(Tasa_Corta_t)(1:(tiempo_T - 1), vect_cant_sub, p) 
  
  # Obtiene el vector de los descuentos D(0,t)
  vect_D_0_T = c(exp(-r_0), exp(-cumsum(vect_r_t))*Precio(1)) 
                                                                      
  
  return(vect_D_0_T)
}

#-----------------------------------------------------------------------------

# Prueba del Árbol Binomial Ho-Lee

# Dólares
Arbol_Ho_Lee_D()


###################### Simulaciones ################################

###### DÓLARES
Simulaciones<-matrix(nrow = tiempo_T,ncol = 10000)

tic()
for(j in 1:10000){
  Simulaciones[,j]<-Arbol_Ho_Lee_D()
}
toc() # 74.86 sec (con 10 000 simulaciones)  662.61 sec (con 100 000 simulaciones) 
 
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





