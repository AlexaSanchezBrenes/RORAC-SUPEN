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
library(stringr)
library(dygraphs)
library(zoo)

################## Estimación de parámetros de Ho-Lee - Colones #########################

# Para leer el archivo se le debe cambiar el formato a "xlsx"

TRI_colones <- read_excel("TRI colones.xlsx",col_types = c("date", "numeric"))



secuencia<-seq(from=1,to=nrow(TRI_colones),by=7)
TRI_colones <- TRI_colones[secuencia,]
TRI_colones<-TRI_colones %>% mutate(Efectiva=log(1+`1 semana`))
varianza_mensual_col<-4*var(TRI_colones$Efectiva)

u_col<-1+varianza_mensual_col
d_col<-1-varianza_mensual_col




################## Estimación de parámetros de Ho-Lee - Dólares #########################
# Buscar tasas overnight: Para calcular varianza y r0

# Para leer el archivo se le debe cambiar el formato a "xlsx"

TRI_dolares <- read_excel("TRI dolares.xlsx",col_types = c("date", "numeric"))


secuencia<-seq(from=1,to=nrow(TRI_dolares),by=7)
TRI_dolares<- TRI_dolares[secuencia,]
TRI_dolares<-TRI_dolares %>% mutate(Efectiva=log(1+`1 semana`))
varianza_mensual_dol<-4*var(TRI_dolares$Efectiva)

u_dol<-1+sqrt(varianza_mensual_dol) 
d_dol<-1-sqrt(varianza_mensual_dol)



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

tiempo_T = 12*15 #PREGUNTAR

#-----------------------------------------------------------------------------

#Ejemplo de par?metros para las curvas del modelo Nelson-Siegel y Svensson: 

P_CCC <- list(c(0.09, -0.02,  0.05,    0, 96, 1), 
              c(0.07, -0.01,  -0.03, 0.05, 48, 64))

#Variables diferencias en dólares y colones:

# Donde la u es la proporción de cuanto sube el indicador respectivo, en el 
# segundo periodo si se indica como: u_2. Por otro lado d es la proporci?n
# de cuanto baja el indicador respectivo, y se denota d_2 la propoci?n para
# el segundo periodo. Además, el factor k.

u2 = c(u_col, u_dol)

d2 = c(d_col, d_dol)

k = c(d2[1]/u2[1], d2[2]/u2[2]) 

#-----------------------------------------------------------------------------

# Svensson

# Objetivo: Encontrar los bonos Cero Cup?n de la curva.

# Par?metros que recibe: la fecha inicial del proyecto (fecha_0), fecha donde 
# se quiere obtener el bono cero cup?n (fecha_buscar) y  un booleano si se 
# quiere obtener el Svensson de colones(1) o de dolares(2) (col_dol). 

#Devuelve: El precio de un bono cero cup?n. 

# Detalle: El Bono Cero Cup?n en colones utiliza la curva Nelson-Siegel. Donde se obtiene el valor de $\delta(t)$: 

Svensson <- function(tao, col_dol){
  
  if(tao == 0){
    precio = 1
    
  }else{
    
    a = (1-exp(-tao/P_CCC[[col_dol]][5]))/
      (tao/P_CCC[[col_dol]][5])
    
    b = ((1-exp(-tao/P_CCC[[col_dol]][5]))/
           (tao/P_CCC[[col_dol]][5])) - exp(-tao/P_CCC[[col_dol]][5])
    
    c = ((1-exp(-tao/P_CCC[[col_dol]][6]))/
           (tao/P_CCC[[col_dol]][6])) - exp(-tao/P_CCC[[col_dol]][6])
    
    precio = exp(-(P_CCC[[col_dol]][1] + P_CCC[[col_dol]][2]*a + 
                     P_CCC[[col_dol]][3]*b + P_CCC[[col_dol]][4]*c)*tao/12)
    
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

Precio_dol <- function(tao){
  (1+tabla$rho[which(tabla$Maduracion==tao)])^-tao
}

Precio <- function(tao,col_dol){
  if(col_dol==1){
    P=Svensson(tao)
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
  r_0 = Tasa_Corta_t(6, col_dol, 0, p)    #OJO: PREGUNTAR A VÍQUEZ: 0 EN VEZ DE 6
  
  # Forma la trayectoria aleatoria 179 veces
  trayectoria = rbernoulli(tiempo_T - 1-(6), p) #OJO: QUITAR EL -6
  
  # Obtiene la cantidad de subidas acumuladas.
  vect_cant_sub = cumsum(trayectoria)
  
  # Aplica la funciÓn de la tasa corta para cada instante.   
  vect_r_t = Vectorize(Tasa_Corta_t)(7:(tiempo_T - 1), col_dol, vect_cant_sub, p) #OJO: CAMBIAR EL 7 POR 1
  
  # Obtiene el vector de los descuentos D(0,t)
  vect_D_0_T = c(exp(-r_0), exp(-cumsum(vect_r_t))*Precio(7, col_dol)) # OJO: CAMBIAR 7 POR 1
                                                                      
  
  return(vect_D_0_T)
}

#-----------------------------------------------------------------------------

# Prueba del Árbol Binomial Ho-Lee

# Colones:
Arbol_Ho_Lee_D(1)

# Dólares
Arbol_Ho_Lee_D(2)


###################### Simulaciones ################################
Simulaciones<-matrix(nrow = tiempo_T-6,ncol = 10000) ### BORRAR EL -6

for(j in 1:10000){
  Simulaciones[,j]<-Arbol_Ho_Lee_D(2)
}
 

Simulaciones_promedio<-apply(X = Simulaciones,MARGIN = 1,FUN = mean)
Simulaciones_ordered<-apply(X = Simulaciones,MARGIN = 1,FUN = sort)
#Simulaciones_VAR<-apply(X = Simulaciones_ordered,MARGIN = 2,FUN = function(x){quantile(x = x,probs=c(0.05,0.95))})
Simulaciones500<-Simulaciones_ordered[1:500,]
Simulaciones9500<-Simulaciones_ordered[9501:10000,]
CVAR_5<-apply(Simulaciones500, 2, mean)
CVAR_95<-apply(Simulaciones9500, 2, mean)

tabla<-tabla %>% mutate(Precio=(1+rho)^-Maduracion)

Fecha<-seq.Date(from = ymd(as.Date(periodo) %m+% months(6)),
                by = "month",
                length.out = length(Simulaciones_promedio))

df<-data.frame(Fecha=Fecha,Curva=tabla$Precio[1:length(Simulaciones_promedio)],Simulaciones_prom=Simulaciones_promedio,
               CVAR_5=CVAR_5,CVAR_95=CVAR_95)



library(xts)



series <- xts(x = df[,2:3], order.by = df$Fecha)
#seriesS<-xts(x=df[,3],order.by=df$Fecha)
#seriesC,seriesS)
dygraph(series, main = "Comparación entre curva precio cero cupón y el modelo Ho-Lee") %>%
  dyAxis("x","Fecha", drawGrid = FALSE) %>%
  dySeries(name=c("Curva","Simulaciones_prom"), label="Curva Cero Cupón" ) %>%
  dyOptions(colors = RColorBrewer::brewer.pal(2, "Set1"))

# Grafico de Esperanza - CVaR:
Jor_Graf <- dygraph(Serie_Jor, main = 'VolÃºmen Proyectado de Jornadas', xlab = "AÃ±o", ylab = "Total de Jornadas", width = "100%") %>%
  dyOptions(drawPoints = TRUE, pointSize = 1, pointShape = "circle", includeZero = FALSE, gridLineColor = "lightseagreen", axisLineColor = "skyblue4") %>% 
  dyLegend(show = "follow") %>% 
  dySeries(c("Jor_Graf.CVaR_bajo", "Jor_Graf.Promedio", "Jor_Graf.CVaR_alto"), label = "Esperado") %>% 
  dySeries(c("Serie_Jor"), label = "Observado")




Curva <- zoo(df$Curva, df$Fecha)
Sim_prom <- zoo(df$Simulaciones_prom, df$Fecha)
CVAR5 <- zoo(df$CVAR_5, df$Fecha)
CVAR95 <- zoo(df$CVAR_95, df$Fecha)

Data <- cbind(Curva, Sim_prom,CVAR5,CVAR95)

dygraph(Data, main = "Comparación Curva Precio Cero Cupón y Modelo Ho-Lee") %>% 
  dyOptions(drawGrid = F) %>%
  dyAxis("y", label = "Ho-Lee", independentTicks = TRUE) %>%
   dySeries("Sim_prom", axis=('y')) %>%
  dySeries("CVAR5", axis=('y')) %>%
  dySeries("CVAR95", axis=('y'), stepPlot = T, fillGraph = T)

lungDeaths <- cbind(ldeaths, mdeaths, fdeaths)
dygraph(Data, main = "Deaths from Lung Disease (UK)") %>%
  dyOptions(colors = RColorBrewer::brewer.pal(3, "Set2"))

lungDeaths <- cbind(ldeaths, mdeaths, fdeaths)
dygraph(lungDeaths, main = "Deaths from Lung Disease (UK)") %>%
  dyOptions(colors = RColorBrewer::brewer.pal(3, "Set2"))
