#              
#                                    Consultoría RORAC-SUPEN
#             Aplicación del modelo Ho-Lee y Simulación de la curva Nelson-Siegel 

# Autores:
# Alexa Sánchez
# Laura Campos
# Isaac Z. Arias

# En este módulo se utiliza el árbol binomial del modelo Ho-Lee de tasas de interes aplicado
# a la curva Nelson-Siegel (Colones).

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

# Historial de Tasas TRI de Costa Rica: 
TRI_colones <- read_excel("TRI colones.xlsx",col_types = c("date", "numeric"))
TRI_colones <- TRI_colones[seq(from = 1, to = nrow(TRI_colones), by = 7),]
TRI_colones<-TRI_colones %>% mutate(Delta = log((1+`1 semana`/100/52))*52/12)

#---------------------------------------- Parámetros Generales:

# Cantidad de simulaciones:
cant.simu <- 10000

# Tasa del primer vencimiento TRI en el primer mes observado (marzo):
TRI.corta <- log(1+1.25/100/52)*52/12

# Se calcula la varianza mensual:
varianza.mensual<-4*var(TRI_colones$Delta)

# Generamos las proporciones de subida y bajada iniciales:
u2 <- 1+sqrt(varianza.mensual)
d2 <- 1-sqrt(varianza.mensual)

# Fijamos el parámetro fijo k del modelo:
k <- d2/u2

# Fijamos el tiempo a simular (35 años) en meses:
tiempo <- 12*35

# Fijamos los parámetros de la curva Nelson Siegel encontrada:
Par.NS <- c(8.337060e-03, TRI.corta-8.337060e-03, -3.330267e-12, 1.811465e+01)
Par.SA <- c(6.253579e-03,-0.005212038,-2.791880e-05,6.6101177e-06,1.357261e+01,4.806624e+01)

# Fechas del periodo a simular:
Fecha.Inicial <- as.Date("03/31/2020", format = '%m/%d/%Y')
Fecha.Final <- Fecha.Inicial+years(35)

#---------------------------------------- Funciones del Modelo:

# Función que genera el precio de la curva a un tiempo (Tao) dado:
Precio <- function(tao,Svensson){
  if(tao == 0){
    precio = 1
  }else{
      if(Svensson == 1){
        Delta = Par.SA[1] * tao +
          Par.SA[2] * ((1-exp(-tao/Par.SA[5]))*Par.SA[5]) + 
          Par.SA[3] * (1-(tao/Par.SA[5]+1)*exp(-tao/Par.SA[5]))*Par.SA[5]^2 +
          ((Par.SA[4]*Par.SA[6]*Par.SA[5])/(Par.SA[5]-Par.SA[6])) * (((1-(tao/Par.SA[5]+1)*exp(-tao/Par.SA[5]))*Par.SA[5]^2) - ((1-(tao/Par.SA[6]+1)*exp(-tao/Par.SA[6]))*Par.SA[6]^2))
      
        precio = exp(-Delta*tao)
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

# Función del parámetro de bajada:
d.t = function(t, p, k){ 
  (k^(t-1))/((1 - p)*(k^(t-1)) + p)
} 

# Función para encontrar la tasa corta dado un tiempo (Tao):
Tasa.Corta.t = function(tao, cant_sub, p){
  
  # Se obtiene el precio de acuerdo a las curvas Nelson-Siegel-Svensson
  P_0_t = Precio(tao)
  P_0_T = Precio(tao + 1)
  
  # Se obtiene la tasa corta mediante la fórmula.
  r_t = log(P_0_t/P_0_T) - log(d.t(tao + 1, p, k)) + cant_sub*log(k)
  
  return(r_t)  
}

# Función que realiza la simulación de una trayectoria aleatoria dado un periodo (tiempo):
Arbol.HL.desc <- function(tiempo){   
  
  # Crea la probabilidad:
  p <- (1 - d2)/(u2 - d2)
  
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
Cero.Cupon.Esp <- c(Vectorize(Precio)(0:tiempo))
Cero.Cupon.Pro <- c(1, Validacion.HL(cant.simu)) 

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
        main = "Validación del Modelo Ho-Lee Colones", 
        xlab = "Fecha", ylab = "Precio Bono Cero Cupón",width = "100%") 
Graf.Val.HL
