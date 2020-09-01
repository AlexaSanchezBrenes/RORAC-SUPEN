library(readxl)
library(dplyr)
library(tidyverse)
library(zoo)
library(lubridate)
library(dygraphs)
library(xts)
setwd("~/RORAC SUPEN/Emisiones")
# 1) Información de las tasas en dólares compuestas semianuales de 2020
data <- read_excel("tnc_18_22.xls", 
                        col_names = FALSE, skip = 5) %>% filter_all(any_vars(!is.na(.)))
n<-dim(data)[2]
names<-c("Maduracion", "Ene", "Feb", "Mar", "Abr", "May", 
          "Jun", "Jul", "Agos", "Set", "Oct", "Nov", "Dic")
# 2) Se obtienen los factores de descuento
tem<-(1+(data[,27:n]/200))
facDes<-cbind(Maduracion = data[,1], tem) 
facDes[,-1]<-facDes[,-1]^(-2*facDes[,1])
colnames(facDes)<-names[1:(dim(facDes)[2])]
facDes[,1] <- seq(from = 6, to = 1200, by = 6)

x <- facDes$Maduracion

# 3) Función para obtener la interpolación de los factores de descuento
interpolacion<-function(x,y){
# Se crea el vector para el cual se quiere obtener los factores de descuento interpolados
  #Se tienen maduraciones de 0.5 a 100, es decir existen los vencimientos semestrales 
  #para los siguientes 100 años. De cierta forma se quisiera obtener los factores de descuento
  # "mensuales" para esos próximos 100 años. Para esto se quiere el siguiente vec de maduraciones:
Maduracion = seq(from = x[1], to = x[length(x)], by = 1 )
# Interpolación lineal
ApproxFun <- approxfun(x , y)
FacDes <- ApproxFun(Maduracion)
interpolacion<-data.frame(Maduracion, FacDes)
return(interpolacion)
}


# 4) Gráficos de los factores de descuento

## Se realiza la interpolación de los factores de descuento para cada mes
## y se almacena en un data frame
factoresDescuento <-list()
for(i in 4:dim(facDes)[2]){
  factoresDescuento[[i-3]] = interpolacion(facDes[,1], facDes[,i]) %>% 
         mutate(Mes = names[i])
}
tabla<-do.call("rbind", factoresDescuento) # arreglar el tema de las fechas
primer.mes<-unique(tabla$Mes)[1]
ultimo.mes<-unique(tabla$Mes)[length(unique(tabla$Mes))]

# El eje x del gráfico para varios meses iría de:
# 2020/09 al 2120/07 (primer fac descuento marzo, ultimo fac des julio)
# Gráfico para el último mes disponible

mes.act<-tabla %>% 
  filter(Mes == ultimo.mes)

Fecha<-seq.Date(from = ymd("2021-01-01"),  by = "month",
                length.out = dim(mes.act)[1])


data<-data.frame(Fecha, FacDes = mes.act$FacDes)
don <- xts(x = data$FacDes, order.by = data$Fecha)
p <- dygraph(don, main = "Curva Precio Bonos Cero Cupón ($)") %>% 
  dyAxis("y", label = "Factor de Descuento") %>% 
  dyAxis("x", drawGrid = FALSE) %>%
  dyOptions(includeZero = TRUE, 
            axisLineColor = "navy", 
            gridLineColor = "lightblue")
p

###Gráfico de las curvas Bono Cero Cupón para varios meses


df<- tabla %>% spread(Mes, FacDes) %>% 
        select(Maduracion,Mar, Abr, May, Jun, Jul)

mar <- zoo(df$Mar, seq.Date(from = ymd("2020-09-01"),  by = "month",
                            length.out = dim(mes.act)[1]))
abr <- zoo(df$Abr, seq.Date(from = ymd("2020-10-01"),  by = "month",
                            length.out = dim(mes.act)[1]))
may <- zoo(df$May, seq.Date(from = ymd("2020-11-01"),  by = "month",
                            length.out = dim(mes.act)[1]))
jun <- zoo(df$Jun, seq.Date(from = ymd("2020-12-01"),  by = "month",
                            length.out = dim(mes.act)[1]))
jul <- zoo(df$Jul, seq.Date(from = ymd("2021-01-01"),  by = "month",
                            length.out = dim(mes.act)[1]))
tabla <- cbind(mar, abr, may, jun, jul)
dygraph(tabla, main = "Curva Precio Bonos Cero Cupón ($)") %>% 
  dyOptions(useDataTimezone = TRUE) %>%
  dyAxis("y", label = "Factor de Descuento Esperado", independentTicks = TRUE) %>%
  dyOptions(colors = RColorBrewer::brewer.pal(7, "YlGnBu")[c(3,4,5,6,7)])


