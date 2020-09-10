library(readxl)
library(dplyr)
library(tidyverse)
library(zoo)
library(lubridate)
library(dygraphs)
library(xts)
#Ubicacion del archivo de tasas spot----------------------------------------------------
Dic<- setwd("~/RORAC SUPEN/Emisiones")
#Periodo para el cual se quiere construir la curva de tasas anualizadas-----------------
periodo <-"2020-07-01"
#Nombre el archivo. se debe mantener el nombre de la descarga pues indica el periodo
#disponible histórico-------------------------------------------------------------------
archivo <- "tnc_18_22.xls"


tasa.anualizada.dolarizada <- function(Dic, periodo, archivo){

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

#3)Función para obtener la interpolación de los factores de descuento---------------------------
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

#5) Filtra tabla para gráfico----------------------------------------------------------------------
tabla.grafico<-tabla %>% 
filter(Periodo == periodo)

Fecha<-seq.Date(from = ymd(as.Date(periodo) %m+% months(6)),
                by = "month",
                length.out = dim(tabla.grafico)[1])

data<-data.frame(Fecha, rho = tabla.grafico$rho)
don <- xts(x = data$rho, order.by = data$Fecha)
g<-  dygraph(don, main = "Tasas Anualizadas en dólares (%)") %>% 
  dyAxis("y", label = "Tasa Anualizada") %>% 
  dyAxis("x", drawGrid = FALSE) %>%
  dyOptions(includeZero = TRUE, 
            axisLineColor = "navy", 
            gridLineColor = "lightblue")
return(list(DatosCurvaDolar = data, Grafico = g ))

}

