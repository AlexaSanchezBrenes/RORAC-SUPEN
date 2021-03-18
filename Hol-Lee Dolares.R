#              
#                                    Consultoría RORAC-SUPEN
#         Aplicación del modelo Ho-Lee y Simulación de la curva Interpolada del Tesoro

# Autores:
# Alexa Sánchez
# Isaac Z. Arias

# En este módulo se utiliza el árbol binomial del modelo Ho-Lee de tasas de interes aplicado
# a la curva soberana interpolada linealmente del Tesoro (Dólares).

# Paquetes Necesarios:
library(dplyr)
library(dygraphs)
library(xts)
library(purrr)
library(stringr)
library(readxl)
library(lubridate)
Sys.setlocale("LC_TIME", "Spanish")
options(stringsAsFactors = FALSE)
  
#---------------------------------------- Carga de Datos:
  
# Historial de Overnight de Estados Unidos: 
Overnight <- read.csv("overnightrate.csv",sep=',',dec='.',header = F)
Overnight1 <- read.csv("overnightrate (1).csv",sep=',',dec='.',header = F)
Overnight2 <- read.csv("overnightrate (2).csv",sep=',',dec='.',header = F)
Overnight3 <- read.csv("overnightrate (3).csv",sep=',',dec='.',header = F)
Overnight4 <- read.csv("overnightrate (4).csv",sep=',',dec='.',header = F)
Overnight5 <- read.csv("overnightrate (5).csv",sep=',',dec='.',header = F)
Overnight6 <- read.csv("overnightrate (6).csv",sep=',',dec='.',header = F)
Overnight7 <- read.csv("overnightrate (7).csv",sep=',',dec='.',header = F)
Overnight <- rbind(Overnight,Overnight1,Overnight2,Overnight3,Overnight4,Overnight5,Overnight6,Overnight7)
rm(Overnight1,Overnight2,Overnight3,Overnight4,Overnight5,Overnight6,Overnight7)
Overnight <- Overnight[,-3]
colnames(Overnight)=c('Fecha','Tasa')
Overnight <- Overnight %>% mutate(Tasa=Tasa/100) %>%
  mutate(TasaS=((1+Tasa/360)^(360/2)-1)*2) %>% mutate(Fecha=as.Date(Fecha, format = '%m/%d/%Y'))
  
# Se calcula el delta para generar las proporciones u y d:
Overnight <- Overnight %>% mutate(Delta=log((1+Tasa/100/360))*360/12)
  
# Historial de curvas soberanas del Tesoro:
Curvas.Tes <- read_excel("tnc_18_22.xls", col_names = FALSE, skip = 5) %>% filter_all(any_vars(!is.na(.)))
Curvas.Tes <- Curvas.Tes[, -c(1,2)]
nom.col.i <- ymd(paste0("20",str_sub("tnc_18_22.xls", start = 5, end = 6),"-", "01","-","01"))
nom.col.f <- ymd(paste0("20",str_sub("tnc_18_22.xls", start = 8, end = 9),"-", "12","-","01"))
vec.nom <- seq.Date(from = nom.col.i, to = nom.col.f, by = "month")
nombre.col <- vec.nom[1:ncol(Curvas.Tes)]
colnames(Curvas.Tes) <- nombre.col
Curvas.Tes <- Curvas.Tes %>% mutate(Vencimiento = seq(6,1200,6))
  
#---------------------------------------- Parámetros Generales:
  
# Cantidad de simulaciones:
cant.simu <- 10000
  
# Se calcula la varianza mensual:
varianza.mensual.dol<-30*var(Overnight$Delta)
  
# Generamos las proporciones de subida y bajada iniciales:
u2.dol<-1+sqrt(varianza.mensual.dol) 
d2.dol<-1-sqrt(varianza.mensual.dol)
  
# Fijamos el parámetro fijo k del modelo:
k <- d2.dol/u2.dol
  
# Fijamos el tiempo a simular (35 años) en meses:
tiempo <- 12*35
  
# Fechas del periodo a simular:
Fecha.Inicial <- as.Date("03/01/2020", format = '%m/%d/%Y')
Fecha.Final <- Fecha.Inicial+years(tiempo/12)
  
  # Datos de tasas compuestas semestralmente observadas:
data.tasas <- Curvas.Tes %>% select(as.character(Fecha.Inicial), Vencimiento) %>% 
  rename(Tasa = as.character(Fecha.Inicial)) %>% mutate(Tasa=Tasa/100)
data.tasas <- rbind(c(mean(Overnight$TasaS[which((month(Overnight$Fecha)==month(Fecha.Inicial) & year(Overnight$Fecha)==year(Fecha.Inicial)))]),0),data.tasas)
  

#---------------------------------------- Funciones del Modelo:

# Función para interpolar linealmente las tasas
inter.lin.tasas <- function(puntos){
  # Se calculan los precios mensuales interpolados:
  tasas <- approxfun(puntos$Vencimiento, puntos$Tasa)(0:tiempo)
  curva.meses <- data.frame(Vencimiento = 0:tiempo, TasasS = tasas)
  return(curva.meses)
}

# Creamos el vector de tasas interpoladas 
vec.tasas <- inter.lin.tasas(data.tasas)

# Creamos el vector de precios:
data.precios <- vec.tasas %>% mutate(Precio=(1+TasasS/2)^(-Vencimiento/6))
vec.precios <-data.precios %>% select(-TasasS)

# Función del parámetro de bajada:
d.t = function(t, p, k){ 
  (k^(t-1))/((1 - p)*(k^(t-1)) + p)
} 

# Función para encontrar la tasa corta dado un tiempo (Tao):
Tasa.Corta.t = function(tao, cant_sub, p){
  
  # Se obtiene el precio de acuerdo al vector de precios:
  P_0_t = vec.precios[tao,2]
  P_0_T = vec.precios[tao+1,2]
  
  # Se obtiene la tasa corta mediante la fórmula.
  r_t = log(P_0_t/P_0_T) - log(d.t(tao + 1, p, k)) + cant_sub*log(k)
  
  return(r_t)  
}

# Función que realiza la simulación de una trayectoria aleatoria dado un periodo (tiempo):
Arbol.HL.desc <- function(tiempo){   
  
  # Crea la probabilidad:
  p <- (1 - d2.dol)/(u2.dol - d2.dol)
  
  # Forma la trayectoria aleatoria para cada periodo:
  trayectoria = rbernoulli(tiempo, p) 
  
  # Obtiene la cantidad de subidas acumuladas:
  vect_cant_sub = cumsum(trayectoria)
  
  # Aplica la función de la tasa corta para cada instante;
  vect_r_t = Vectorize(Tasa.Corta.t)(1:tiempo, vect_cant_sub, p)
  
  # Obtiene el vector de los descuentos D(0,t):
  vect_D_0_T = exp(-cumsum(vect_r_t))
  
  return(vect_D_0_T)
}

# Función para obtener la validación "Marked to Market": 
Validacion.HL <- function(total.trayec){

  # Se simulan suficientes trayectorias;
  Matriz.trayectorias = matrix(nrow = total.trayec, ncol = tiempo)
  
  for (i in 1:total.trayec){
    Matriz.trayectorias[i,] = Arbol.HL.desc(tiempo)
  }
  
  # Se genera el promedio de la simulación:
  promedios.D = colMeans(Matriz.trayectorias)
  
  return(promedios.D)
}

#---------------------------------------- Resultados:

# Generamos la curva previamente encontrada:
Cero.Cupon.Esp <- data.precios$Precio
Cero.Cupon.Pro <- c(1,Validacion.HL(cant.simu))

# Error Porcentual Promedio:
Error.por.pro <- round(mean(abs(Cero.Cupon.Esp-Cero.Cupon.Pro)/Cero.Cupon.Esp)*100,2)

#---------------------------------------- Visualización:

# Vector de fechas:
Fechas <- seq.Date(from = Fecha.Inicial,
                   to = Fecha.Final,
                   by = "months")

# Creamos la tabla de datos:
Data.Val = data.frame('Esperanza' = Cero.Cupon.Esp, 
                      'Promedio' = Cero.Cupon.Pro)

# Creamos la serie:
Serie.Val <- xts(Data.Val, order.by = Fechas)

# Graficamos:
Graf.Val.HL <- dygraph(Serie.Val,
                       main = "Validación del Modelo Ho-Lee Dólares", 
                       xlab = "Fecha", ylab = "Precio Bono Cero Cupón",width = "100%") 
Graf.Val.HL
