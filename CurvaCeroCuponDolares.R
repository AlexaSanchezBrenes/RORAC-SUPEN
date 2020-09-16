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
