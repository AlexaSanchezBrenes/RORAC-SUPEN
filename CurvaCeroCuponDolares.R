library(readxl)
library(dplyr)
library(tidyverse)
library(zoo)
library(lubridate)
library(dygraphs)
library(xts)
# 1) Información de las tasas en dólares compuestas semianuales de 2020
data <- read_excel("tnc_18_22.xls", 
                        col_names = FALSE, skip = 5) %>% filter_all(any_vars(!is.na(.)))
n<-dim(data)[2]
names<-c("Maduracion", "Ene", "Feb", "Mar", "Abr", "May", 
          "Jun", "Jul", "Agos", "Set", "Oct", "Nov", "Dic")
# 2) Se obtienen los factores de descuento
tem<-(1+(data[,27:n]/200))
facDes<-cbind(Maduracion = data[,1], tem) 
facDes[,-1]<-facDes[,-1]^(2*facDes[,1])
colnames(facDes)<-names[1:(dim(facDes)[2])]



# 3) Función para obtener la interpolación de los factores de descuento
interpolacion<-function(x,y){
# Se crea el vector para el cual se quiere obtener los factores de descuento interpolados
  #Se tienen maduraciones de 0.5 a 100, es decir existen los vencimientos semestrales 
  #para los siguientes 100 años. De cierta forma se quisiera obtener los factores de descuento
  # "mensuales" para esos próximos 100 años. Para esto se quiere el siguiente vec de maduraciones:
Maduracion = seq(from = x[1], to = x[length(x)], by = 0.1 )
# Interpolación lineal
ApproxFun <- approxfun(x , y)
FacDes <- ApproxFun(Maduracion)
interpolacion<-data.frame(Maduracion, FacDes)
return(interpolacion)
}

grap.fac.des.dol<-function(){}

mes.act<-interpolacion(facDes$Maduracion, facDes$Jul)

Fecha<-seq.Date(from = ymd("2021-01-01"), to = ymd("2120-07-01"), by = "month")
               # length.out = dim(mes.act)[1])

pb<-data.frame(Fecha)
data<-data.frame(Fecha, FacDes = mes.act$FacDes)
don <- xts(x = data$FacDes, order.by = data$Fecha)
p <- dygraph(don)
p


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
