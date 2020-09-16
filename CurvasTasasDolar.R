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

# 2) Curva de tasas anualizadas
tem <- data[,27:n]/200
rho <- cbind(Maduracion = data[,1], tem) 
rho[,-1] <- exp(rho[,-1]*(1/2*rho[,1]))-1
colnames(rho)<-names[1:(dim(rho)[2])]
rho[,1] <- seq(from = 6, to = 1200, by = 6)

x <- rho$Maduracion

# 3) Función para obtener la interpolación de los factores de descuento
interpolacion<-function(x,y){
  # Se crea el vector para el cual se quiere obtener los factores de descuento interpolados
  #Se tienen maduraciones de 0.5 a 100, es decir existen los vencimientos semestrales 
  #para los siguientes 100 años. De cierta forma se quisiera obtener los factores de descuento
  # "mensuales" para esos próximos 100 años. Para esto se quiere el siguiente vec de maduraciones:
  Maduracion = seq(from = x[1], to = x[length(x)], by = 1 )
  # Interpolación lineal
  ApproxFun <- approxfun(x , y)
  rho <- ApproxFun(Maduracion)
  interpolacion<-data.frame(Maduracion, rho)
  return(interpolacion)
}


# 4) Gráficos de los factores de descuento

## Se realiza la interpolación de los factores de descuento para cada mes
## y se almacena en un data frame
tasas <-list()
for(i in 4:dim(rho)[2]){
  tasas[[i-3]] = interpolacion(rho[,1], rho[,i]) %>% 
    mutate(Mes = names[i])
}
tabla<-do.call("rbind", tasas) # arreglar el tema de las fechas
primer.mes<-unique(tabla$Mes)[1]
ultimo.mes<-unique(tabla$Mes)[length(unique(tabla$Mes))]


# Gráfico para el último mes disponible

mes.act<-tabla %>% 
  filter(Mes == ultimo.mes)
data<-data.frame(mes.act$Maduracion, rho = mes.act$rho)
don <- xts(x = data$rho, order.by = data$Maduracion)
p <- dygraph(don, main = "Curva de Tasas en Dólares") %>% 
  dyAxis("y", label = "Factor de Descuento") %>% 
  dyAxis("x", drawGrid = FALSE) %>%
  dyOptions(includeZero = TRUE, 
            axisLineColor = "navy", 
            gridLineColor = "lightblue")
p

###Gráfico de las curvas Bono Cero Cupón para varios meses


df<- tabla %>% spread(Mes, rho) %>% 
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
  dyOptions(colors = RColorBrewer::brewer.pal(10, "YlGnBu")[c(2,4,6,8,10)])






