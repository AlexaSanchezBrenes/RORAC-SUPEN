# Modelo de ?rbol Binomial Ho-Lee
# C?digo Adjunto para Oferta SUPEN

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

# El presente código es de uso exclusivo como requisito en la oferta de la
# consultor?a solicitada por la Superintendencia de Pensiones (SUPEN) para la 
# creaci?n de un modelo de estimaci?n de tasas de inter?s y valoraci?n de 
# riesgo en inversiones de capital.  

#-----------------------------------------------------------------------------

# Inicializaci?n de C?digo:

# Fecha_Hoy: Se refiere a la fecha inicial que es el 31-05-2018.   

# IPC_Hoy: Es el tipo de cambio del d?a 31-05-2018 (colones).    

# tiempo_T: Cantidad de meses donde se reaizar? el estudio actuarial.


Fecha_Hoy <- date('2018-05-31')

IPC_Hoy <- 104.66

tiempo_T = 12*15 

#-----------------------------------------------------------------------------

#Ejemplo de par?metros para las curvas del modelo Nelson-Siegel y Svensson: 

P_CCC <- list(c(0.09, -0.02,  0.05,    0, 96, 1), 
              c(0.07, -0.01,  -0.03, 0.05, 48, 64))

#Variables diferencias en d?lares y colones:

# Donde la u es la proporci?n de cuanto sube el indicador respectivo, en el 
# segundo periodo si se indica como: u_2. Por otro lado d es la proporci?n
# de cuanto baja el indicador respectivo, y se denota d_2 la propoci?n para
# el segundo periodo. Adem?s, el factor k.

u2 = c(1.00001, 1.000005)

d2 = c(0.99999, 0.999995)

k = c(d2[1]/u2[1], d2[2]/u2[2]) 

#-----------------------------------------------------------------------------

# Svensson

# Objetivo: Encontrar los bonos Cero Cup?n de la curva.

# Par?metros que recibe: la fecha inicial del proyecto (fecha_0), fecha donde 
# se quiere obtener el bono cero cup?n (fecha_buscar) y  un booleano si se 
# quiere obtener el Svensson de colones(1) o de dolares(2) (col_dol). 

#Devuelve: El precio de un bono cero cup?n. 

# Detalle: El Bono Cero Cup?n en colones utiliza la curva Nelson-Siegel. Donde se obtiene el valor de $\delta(t)$: 

Svensson <- function(tao, col_ude){
  
  if(tao == 0){
    precio = 1
    
  }else{
    
    a = (1-exp(-tao/P_CCC[[col_ude]][5]))/
      (tao/P_CCC[[col_ude]][5])
    
    b = ((1-exp(-tao/P_CCC[[col_ude]][5]))/
           (tao/P_CCC[[col_ude]][5])) - exp(-tao/P_CCC[[col_ude]][5])
    
    c = ((1-exp(-tao/P_CCC[[col_ude]][6]))/
           (tao/P_CCC[[col_ude]][6])) - exp(-tao/P_CCC[[col_ude]][6])
    
    precio = exp(-(P_CCC[[col_ude]][1] + P_CCC[[col_ude]][2]*a + 
                     P_CCC[[col_ude]][3]*b + P_CCC[[col_ude]][4]*c)*tao/12)
    
  }
  
  return(precio)
}

############################### PARA CALCULAR EL P(0,T) PARA LA CURVA EN DÓLARES #######################

#Ubicacion del archivo de tasas spot----------------------------------------------------
Dic<- setwd("~/RORAC SUPEN")
#Periodo para el cual se quiere extraer el precio cero cupón -----------------
periodo <-"2020-06-01"
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

#2) Tasas anualizadas--------------------------------------------------------------------------
rho<-((1+(data/2))^2)-1
rho$Maduracion <- seq(from = 6, to = 1200, by = 6)

#3)Función para obtener la interpolación de las tasas anualizadas---------------------------
interpolacion<-function(x,y){
  Maduracion = seq(from = x[1], to = x[length(x)], by = 1 )
  ApproxFun <- approxfun(x , y)
  rho <- ApproxFun(Maduracion)
  interpolacion<-data.frame(Maduracion, rho)
  return(interpolacion)
}

#4)Interpolación de los de las tasas cortas para cada mes--------------------------------------
lista <-list()
for(i in 1:(ncol(rho)-1)){
  lista[[i]] = interpolacion(rho[,ncol(rho)], rho[,i]) %>% 
    mutate(Periodo = names[i])
}
tabla<-do.call("rbind", lista)

#5) Filtra tabla para periodo----------------------------------------------------------------------
tabla<-tabla %>% 
  filter(Periodo == periodo)

Precio <- function(tao){
  (1+tabla$rho[which(tabla$Maduracion==tao)])^-tao
}


#-----------------------------------------------------------------------------

# Decrecimiento del ?rbol Binomial


# Objetivo: Encontrar el valor de d(t).    

# Par?metros que recibe: el instante de tiempo que se requiere t, 
# la probabilidad p y el valor de k.

# Devuelve: El resultado obtenido al realizar lo siguiente: 

d_t = function(t, p, k){ 
  (k^(t-1))/((1 - p)*(k^(t-1)) + p)
} 

#-----------------------------------------------------------------------------

# C?lculo de Tasa Corta:

# Objetivo: Encontrar el valor de r_t.    

# Par?metros que recibe: Distancia de tiempo t que se representa por tao, 
# si se quiere obtener la tasa corta de los colones o de los d?lares,
# cantidad de veces que ha subido hasta ese momento t y la probabidad p.

# Devuelve: El resultado obtenido al realizar lo siguiente: 

Tasa_Corta_t = function(tao, col_ude, cant_sub, p){
  
  # Se obtiene el precio de acuerdo a las curvas Nelson-Siegel-Svensson
  P_0_t = Svensson(tao, col_ude)
  P_0_T = Svensson(tao + 1, col_ude)
  
  # Se obtiene la tasa corta mediante la f?rmula.
  r_t = log(P_0_t/P_0_T) - log(d_t(tao + 1, p, k[col_ude])) + cant_sub*log(k[col_ude])
  
  return(r_t)  
}

#-----------------------------------------------------------------------------

# ?rbol Ho-Lee

# Objetivo: Encontrar el valor del ?rbol Ho Lee de descuentos D(0, t).  

# Par?metros que recibe: El indicador si se quiere obtener la tasa corta 
# de los colones o de los d?lares.

# Devuelve: El vector de descuentos D(0,t), con t = 0,1,\dots,T.

Arbol_Ho_Lee_D = function(col_ude){   
  
  # Dado el tipo de ?rbol crea la probabilidad
  p = (1 - d2[col_ude])/(u2[col_ude] - d2[col_ude])
  
  # Obtiene la tasa corta inicial
  r_0 = Tasa_Corta_t(0, col_ude, 0, p)    
  
  # Forma la trayectoria aleatoria 179 veces
  trayectoria = rbernoulli(tiempo_T - 1, p) 
  
  # Obtiene la cantidad de subidas acumuladas.
  vect_cant_sub = cumsum(trayectoria)
  
  # Aplica la funci?n de la tasa corta para cada instante.   
  vect_r_t = Vectorize(Tasa_Corta_t)(1:(tiempo_T - 1), col_ude, vect_cant_sub, p)
  
  # Obtiene el vector de los descuentos D(0,t)
  vect_D_0_T = c(exp(-r_0), exp(-cumsum(vect_r_t))*Svensson(1, col_ude))
  
  return(vect_D_0_T)
}

#-----------------------------------------------------------------------------

# Prueba del ?rbol Binomial Ho-Lee

# Colones:
Arbol_Ho_Lee_D(1)

# D?lares
Arbol_Ho_Lee_D(1)