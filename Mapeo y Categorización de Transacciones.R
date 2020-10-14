#              
#                    Consultoría RORAC-SUPEN 
#                 Mapeo y categorización de las transacciones 

# Autores:
# Alexa Sánchez
# Laura Campos
# Isaac Z. Arias

# El siguiente módulo realiza un mapeo de las transacciones, con el fin de categorizar 
#la información

# Paquete necesarios:
library(readr)
library(stringi)
library(dplyr)
library("tools")
library(stringr)
library(lubridate)
library(ggplot2)
library(tidyverse)
options(stringsAsFactors = FALSE)

# Dirrección de los datos:
Dir <- "C:/Users/EQUIPO/Desktop/Estudios/RORAC-SUPEN/Acciones"

# Función para leer los datos

#Input: archivos .txt que contienen la información de las transacciones
lee.datos<<-function(archivo){
  
  tbl<- as.data.frame(unclass(read.table(archivo, 
                                         header = TRUE, 
                                         encoding = "Latin1",
                                         stringsAsFactors = F)))
  return(tbl)
}

# Función para obtener los datos de las carpetas

#Input: dirección ed la carpeta donde se encuentran los archivos con las transacciones
#Output: Lista con todas las transacciones
lista.df<-function(path=Dir){

  folder<-list.files(path,full.name = TRUE )
  n<-length(folder)
  lista.df<-list()
  
  for(i in 1:n){
    lista.df[[i]] <-lee.datos(folder[i])
  }
  return(lista.df)
}
tabla <- lista.df(Dir)
transacciones <- lista.df(Dir)
names.1 <- colnames(transacciones[[14]])
names.2 <- colnames(transacciones[[1]])

# Columnas que se agregan a la data a partir de Enero 2020
col.nuevas <- as.data.frame(names.1[which(!names.1%in%names.2)])
col.seleccion.2 <- names.1[which(names.1%in%names.2)]
col.seleccion <- c("COD_ISIN", "COD_ENT", "COD_FON", "FEC_DAT", "COD_MOD_INV",
                   "FEC_VEN",  "TAS_FAC", "VAL_FAC" )
## Se seleccionan las columnas en común dados los dos grupos de información
transacciones <- lapply(transacciones, 
                          function(x) x%>% select(all_of(col.seleccion)))
#Se quitan los reportos 
transacciones <- do.call("rbind", transacciones) %>% 
                  filter(! COD_MOD_INV %in% c("RE"))

#Se quitan los códigos isin de los títulos que tienen flujos intermedios
isin.sin.flujos.intermedios <- transacciones %>% 
  filter(is.na(FEC_VEN) & is.na(TAS_FAC)) 
unique(isin.sin.flujos.intermedios$COD_MOD_INV)


## Se tienen dos grupos:
 # i. Los registros antes del 01/01/2020
transacciones.1 <- transacciones %>% 
  filter(year(FEC_DAT) < 2020) %>% 
  distinct(COD_ISIN, .keep_all = TRUE) %>% 
  rename(COD_MOD_INV_Inicial = COD_MOD_INV) %>% 
  select(-FEC_DAT)

unique(transacciones.1$COD_MOD_INV_Inicial)

# ii. Los registros después del 01/01/2020
transacciones.2 <- transacciones %>% 
  filter(year(FEC_DAT) >= 2020) %>% 
  distinct(COD_ISIN, .keep_all = TRUE) %>% 
  rename(COD_MOD_INV_Final = COD_MOD_INV) %>% 
   select(-FEC_DAT)

unique(transacciones.2$COD_MOD_INV_Final)

## Revisión de clasificación según COD_ISIN antes y después de  Enero 2020:

# 1. COD_ISIN que aparecen solo antes
g.1 <- anti_join(transacciones.1, transacciones.2, by = "COD_ISIN" )
unique(g.1$COD_MOD_INV_Inicial)

pb <- g.1 %>% filter(COD_MOD_INV_Inicial %in% c("FI" ,"FA"))
# 1. COD_ISIN que aparecen solo después
g.2 <- anti_join(transacciones.2, transacciones.1, by = "COD_ISIN" )
unique(g.2$COD_MOD_INV_Final)

# 3. COD_ISIN que aparecen antes y despues:
repetidos <- inner_join(transacciones.1,transacciones.2,
                               by = "COD_ISIN") %>% 
             select(c(1,2,3,4,5,6,7,10))

g.3 <- repetidos
# 3.1. COD_ISIN que tienen el mismo COD_MOD_INV antes y despúes:

cod.mod.inv.iguales <- repetidos %>% 
                       filter(COD_MOD_INV_Inicial == COD_MOD_INV_Final)
unique(cod.mod.inv.iguales$COD_MOD_INV_Inicial)

# 3.2. COD_ISIN que tienen distinto COD_MOD_INV: se observa que las categorías
#que se reclasificaron corresponden a "DE"  "DI" y "FI".
# Los que son DE y DI no tienen relevancia pues son títulos con flujos intermedios
#Queda revisar FI que parecen estar asociados a Fondos

cod.mod.inv.distinto <- repetidos %>% 
  filter(COD_MOD_INV_Inicial != COD_MOD_INV_Final)

DE <- cod.mod.inv.distinto %>% 
       filter(COD_MOD_INV_Inicial == "DE") %>% 
       group_by(COD_MOD_INV_Final) %>% 
       summarise(CONTEO = n()) ##DD, DO, DR, DT ??

DI <- cod.mod.inv.distinto %>% 
  filter(COD_MOD_INV_Inicial == "DI") %>% 
  group_by(COD_MOD_INV_Final) %>% 
  summarise(CONTEO = n())  

FI <- cod.mod.inv.distinto %>% 
  filter(COD_MOD_INV_Inicial == "FI") %>% 
  group_by(COD_MOD_INV_Final) %>% 
  summarise(CONTEO = n())   ##----M1 Y V2 ????

isin.FI.P2 <- cod.mod.inv.distinto %>% 
  filter(COD_MOD_INV_Inicial == "FI" & COD_MOD_INV_Final == "P2") 
isin.FI.E1 <- cod.mod.inv.distinto %>% 
  filter(COD_MOD_INV_Inicial == "FI" & COD_MOD_INV_Final == "E1") 
###################################################################################
#Características de los FI:

col.seleccion.2 <- names.1[which(names.1%in%names.2)]
tabla <- lapply(tabla, 
                        function(x) x%>% dplyr::select(col.seleccion.2))

revision.FI.P2 <- do.call("rbind", tabla) %>% 
  filter(COD_ISIN %in% isin.FI.P2$COD_ISIN) 
  
revision.FI.E1 <- do.call("rbind", tabla) %>% 
  filter(COD_ISIN %in% isin.FI.E1$COD_ISIN) 

#######################################################################################
########################################################################################

## 1.1) Agrupación por Código de la modalidad de inversión: 
# clase de título valor o instrumento bursátil que se está reportando

cod.mod.inv <- transacciones.2 %>% 
  group_by(FEC_DAT, COD_MOD_INV, COD_MON) %>% 
  summarise(Conteo  = n()) 

prom.mensual.cod.mod.inv <- cod.mod.inv %>% 
  group_by(Anno = year(FEC_DAT),
           Mes = month(FEC_DAT),
           COD_MOD_INV, COD_MON) %>% 
  summarise(PromedioRegistrosMensuales = mean(Conteo)) 

## Gráfica del comportamiento histórico de las modalidad de inversión:

# i. De acuerdo con la documentación (Tablas y Validac RC V6, pag 80) 
# los tipos de inversiones se agrupan en 10 tipos.
# ii .A partir de 2020 (ver Tablas y Validac V5, pag 10 rige un cambio 
# en la clasificación)

#--------------DUDA------------------------------------------
#-----ENTENDER COMO SE REAGRUPARON LAS MODALIDADES-----------
#-----ENTENDER DIFERENCIA ENTRE DEUDA INDIVIDUAL Y ESTANDARIZADA SEGÚN EL NIVEL---
#-----Ver como se van a agrupar esas clasificaciones------------

#Fondos van como acciones
prom.mensual.cod.mod.inv$Fecha = paste(prom.mensual.cod.mod.inv$Anno,
                                       prom.mensual.cod.mod.inv$Mes,
                                       "01", sep="-") %>%
  ymd() %>%
  as.Date()
# Colones
prom.mensual.cod.mod.inv %>% filter(COD_MON == 1) %>% 
  ggplot(aes(Fecha, PromedioRegistrosMensuales)) + 
  geom_col(aes(fill = COD_MOD_INV)) + 
  scale_x_date(date_labels = "%b %Y")+ 
  labs(title = "Promedio mensual de registros de las modalidades de inversión" )

## Gráfica del comportamiento histórico para una modalidad de inversión específica:  

#i. Agrupación por moneda
g.prom.mensual.cod.mod.inv <- function(Modalidad){
  prom.mensual.cod.mod.inv  %>% 
    filter(COD_MOD_INV %in% Modalidad) %>% 
    ggplot(aes(Fecha, PromedioRegistrosMensuales)) + 
    geom_col(aes(fill = COD_MON)) + 
    scale_x_date(date_labels = "%b %Y")+ 
    labs(title = "Promedio mensual de registros por moneda " )
}
#ii. Colones
g.prom.mensual.cod.mod.inv.COL <- function(Modalidad){
  prom.mensual.cod.mod.inv  %>% filter(COD_MON == 1) %>%
    filter(COD_MOD_INV %in% Modalidad) %>% 
    ggplot(aes(Fecha, PromedioRegistrosMensuales)) + 
    geom_col(aes(fill = COD_MOD_INV)) + 
    scale_x_date(date_labels = "%b %Y")+ 
    labs(title = "Promedio mensual de registros (CRC)" )
}
#iii. Dolares
g.prom.mensual.cod.mod.inv.DOL <- function(Modalidad){
  prom.mensual.cod.mod.inv  %>% filter(COD_MON ==2) %>%
    filter(COD_MOD_INV %in% Modalidad) %>% 
    ggplot(aes(Fecha, PromedioRegistrosMensuales)) + 
    geom_col(aes(fill = COD_MOD_INV)) + 
    scale_x_date(date_labels = "%b %Y")+ 
    labs(title = "Promedio mensual de registros ($)" )
}

## Por moneda:
Acciones <- g.prom.mensual.cod.mod.inv("AC")
Acciones

DeudaIndividual <- g.prom.mensual.cod.mod.inv("DI")
DeudaIndividual


## Colones:
DeudaEstandarizada.COL <- g.prom.mensual.cod.mod.inv.COL(c("DE", "D2", "D3"))
DeudaEstandarizada.COL

ParticipacionFondos.COL <- g.prom.mensual.cod.mod.inv.COL(c("P1", "P2", "P3"))
ParticipacionFondos.COL

## Dolares:

ExchangeTradedFunds.DOL <- g.prom.mensual.cod.mod.inv.DOL(c("E1", "E2", "E3"))
ExchangeTradedFunds.DOL

DeudaEstandarizada.DOL <- g.prom.mensual.cod.mod.inv.DOL(c("DE", "D2", "D3"))
DeudaEstandarizada.DOL

ParticipacionFondos.DOL <- g.prom.mensual.cod.mod.inv.DOL(c("P1", "P2", "P3"))
ParticipacionFondos.DOL

