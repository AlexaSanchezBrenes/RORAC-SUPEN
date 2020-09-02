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

# 2) Curva Tasas anualizadas
tem<-((1+(data[,27:n]/2))^2)-1
rho<-cbind(Maduracion = data[,1], tem) 
colnames(rho)<-names[1:(dim(rho)[2])]
rho[,1] <- seq(from = 6, to = 1200, by = 6)

# 3) Función para obtener la interpolación de los factores de descuento
interpolacion<-function(x,y){
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
lista <-list()
for(i in 4:dim(rho)[2]){
lista[[i-3]] = interpolacion(rho[,1], rho[,i]) %>% 
         mutate(Mes = names[i])
}
tabla<-do.call("rbind", lista) # arreglar el tema de las fechas
primer.mes<-unique(tabla$Mes)[1]
ultimo.mes<-unique(tabla$Mes)[length(unique(tabla$Mes))]

# El eje x del gráfico para varios meses iría de:
# 2020/09 al 2120/07 (primer fac descuento marzo, ultimo fac des julio)
# Gráfico para el último mes disponible

mes.act<-tabla %>% 
  filter(Mes == ultimo.mes)

Fecha<-seq.Date(from = ymd("2021-01-01"),  by = "month",
                length.out = dim(mes.act)[1])


data<-data.frame(Fecha, rho = mes.act$rho)
don <- xts(x = data$rho, order.by = data$Fecha)
  dygraph(don, main = "Tasas Anualizadas ($)") %>% 
  dyAxis("y", label = "Tasa Anualizada") %>% 
  dyAxis("x", drawGrid = FALSE) %>%
  dyOptions(includeZero = TRUE, 
            axisLineColor = "navy", 
            gridLineColor = "lightblue")


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
dygraph(tabla, main = "Tasas Anualizadas ($)") %>% 
  dyOptions(useDataTimezone = TRUE) %>%
  dyAxis("y", label = "Tasa Anualizada", independentTicks = TRUE) %>%
  dyOptions(colors = RColorBrewer::brewer.pal(9, "YlGnBu")[c(4,5,7,8,9)])




