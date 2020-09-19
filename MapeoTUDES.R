#              
#                    Consultoría RORAC-SUPEN 
#                    Mapeo de Datos Actuales para TUDES 

# Autores:
# Alexa Sánchez
# Laura Campos
# Isaac Z. Arias

# El siguiente módulo realiza un mapeo estadístico sobre los datos 
# obtenidos por la Bolsa Nacional de Valores sobre las transacciones 
# de títulos  emitidos en Unidades de Desarrollo
#en fechas recientes para analizarlo la información de 

# Paquete necesarios:
library(readr)
library(stringi)
library(dplyr)
library("tools")
library(stringr)
library(lubridate)
options(stringsAsFactors = FALSE)

# Dirrección de los datos:
Dic <- "~/RORAC SUPEN/Emisiones/Boletas"

# Función para leer los datos

# Recibe un txt o csv con la informacion de la emision para un periodo dado.
lee.boleta<<-function(archivo){
  
  if(file_ext(archivo) =="csv"){ 
    tbl<-as.data.frame(unclass(read.table(archivo, 
                                          header = TRUE, 
                                          sep = ",", 
                                          encoding =  "UTF-8",
                                          stringsAsFactors = FALSE)))
    names<-colnames(tbl)
    names[which(stri_enc_mark(names) == "native")]<-stringi::stri_trans_general(
      names[which(stri_enc_mark(names) == "native")],
      "Latin-ASCII")
  }else{
    tbl<- as.data.frame(unclass(read.table(archivo, 
                                           header = TRUE, 
                                           sep = ",", 
                                           encoding = "Latin1",
                                           stringsAsFactors = F)))
    names<-colnames(tbl)
    names<-stringi::stri_trans_general(colnames(tbl), "Latin-ASCII")
    
  }
  
  
  df<-tbl%>% 
    mutate(Tasa.facial = as.numeric(str_replace(Tasa.facial, ",", ".")),
           Precio = as.numeric(str_replace(Precio, ",", ".")),
           Valor.Transado = as.numeric(str_replace(Valor.Transado, ",", ".")),
           Valor.facial = as.numeric(str_replace(Valor.facial, ",", ".")),
           Tis = as.numeric(str_replace(Tis, ",", ".")),
           Tir = as.numeric(str_replace(Tir, ",", ".")),
           #Fecha.de.Operacion = as.Date(as.character.Date(Fecha.de.Operacion)),
           Rendimiento.de.la.recompra = as.numeric(str_replace
                                                   (Rendimiento.de.la.recompra , ",", ".")))
  
  colnames(df)<-names  
  return(df)
}

# Función para obtener los datos de las carpetas

#Input: dirección ed la carpeta donde se encuentran las boletas
#Output: Lista con las boletas y los cuadros resumen
lista.df.boletas<-function(path=Dic){
  
  folder<-list.files(path,full.name = TRUE )
  n<-length(folder)
  archivo<-numeric(n)
  lista.df<-list()
  
  for(i in 1:n){
    if(length(list.files(path = folder[i], pattern = ".txt")) != 0){
      archivo[i]<- list.files(path = folder[i], pattern = ".txt")
    }else{
      archivo[i]<- list.files(path = folder[i], pattern = ".csv")}
    
    lista.df[[i]]<-lee.boleta(paste0(folder[i],"/",archivo[i]))
  }
  
  names(lista.df)<-archivo  
  return(lista.df)
  
}

# Generamos los datos y lo unimos en un solo dataframe:
Boletas <- bind_rows(lista.df.boletas(), .id = "cod.Boleta")

# Revisión General de datos:
str(Boletas)
summary(Boletas)

# Se encuentra que las bolestas sin txt están repetidas:
prueba.rep <- Boletas %>% group_by(cod.Boleta, Fecha.de.Operacion) %>% 
  summarise(Cantidad = n(),.groups = "keep") %>% 
  ungroup()
Boletas <- Boletas %>% select(-cod.Boleta) %>% unique()


ins.tudes.total <- Boletas %>% filter(Nemotecnico.del.instrumento %in% "tudes")

# Eliminamos variables que momentaneamente no son necesarias:
Boletas <- Boletas %>% select(-Dias.de.vencimiento.del.Instrumento,-Tis,-Fecha.Ultimo.Pago.Intereses,
                              -Fecha.Vencimiento.Plazo,-Plazo.de.la.operacion,-Garantia.de.la.recompra,
                              -Indicador.de.subasta,-Dias.Acumulados.Intereses,
                              -Tir,-Fecha.de.Renovacion,-Dias.de.recompra,-Rendimiento.de.la.recompra,
                              -Operacion.Cruzada, -Numero.de.Contrato) %>% 
  mutate(Fecha.de.Operacion = as.POSIXct(Fecha.de.Operacion, format = "%Y/%m/%d %H:%M:%S"),
         Fecha.de.Vencimiento = as.POSIXct(Fecha.de.Vencimiento, format = "%Y/%m/%d %H:%M:%S")) %>% 
  filter(Mercado.de.Negociacion == "MERCADO SECUNDARIO") %>%
  mutate(titulo = ifelse(is.na(Fecha.de.Vencimiento), "ACCIONES O FONDOS", "BONOS"))

Boletas <- Boletas %>% filter(Recompra == "NO")

ins.tudes.a.usar <- Boletas %>% filter(Nemotecnico.del.instrumento %in% "tudes")

# Cantidad de bonos por emisores e instrumentos al mes:
mes.ve.nemo <- Boletas %>% filter(titulo == "BONOS") %>% 
  group_by(mes = cut(Fecha.de.Vencimiento, "month"),Nemotecnico.del.Emisor,Nemotecnico.del.instrumento) %>% 
  summarise(cantidad = n(), .groups = "keep") %>% 
  ungroup()

# Cantidad de bonos por emisores e instrumentos al mes  (TUDES):
mes.ve.ins.seg.tudes <- mes.ve.nemo %>% mutate(ano = year(as.Date(mes)), mes = month(as.Date(mes))) %>% 
  filter(Nemotecnico.del.Emisor %in% c("BCCR","G"), 
         Nemotecnico.del.instrumento %in% c( "TUDES", "tudes")) %>% 
  group_by(ano, mes) %>% summarise(cantidad = sum(cantidad), .groups = "keep") %>% ungroup() 

# Mediana y Promedio mensual:
media.ve.mes <- mean(mes.ve.ins.seg.tudes$cantidad) 
media.ve.mes
mediana.ve.mes <- median(mes.ve.ins.seg.tudes$cantidad)
mediana.ve.mes

# Segregamos por los nemotécnicos que necesitamos (TUDES):
op.seg.post.tudes <- Boletas %>% 
  filter(Nemotecnico.del.Emisor %in% c("BCCR","G"), 
         Nemotecnico.del.instrumento %in% c("TUDES", "tudes")) %>% 
  group_by(mes.op = month(Fecha.de.Operacion),
           ano = year(Fecha.de.Vencimiento),
           mes.ve =month(Fecha.de.Vencimiento)) %>% 
           summarise(cantidad = n(), .groups = "keep") %>% ungroup() 

#---------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------

# El siguiente módulo realiza un mapeo estadístico sobre los datos 
# obtenidos por la Bolsa Nacional de Valores sobre las transacciones 
# de títulos en fechas anteriores a la pandemia.
setwd("~/RORAC SUPEN/Emisiones")

# Se cargan los datos:
Datos.Curva <- read_excel("Datos Curva Soberana (2009-2019)(1914).xlsx") %>% 
  select(`Fecha de Operación`,`Nemotécnico del Emisor`,`Mercado de Negociación`,
         `Número de Contrato`,`Fecha de Vencimiento`,`Nemotécnico del instrumento`,
         `Fecha de Vencimiento`) %>% 
  filter(`Mercado de Negociación` == "MERCADO SECUNDARIO") %>%
  mutate(titulo = ifelse(is.na(`Fecha de Vencimiento`), "ACCIONES O FONDOS", "BONOS"))


pre.ins.tudes.total <- Datos.Curva %>% filter(`Nemotécnico del instrumento` %in% "tudes")

# Cantidad de bonos por emisores e instrumentos agrupados por mes de vencimiento:
mes.ve.nemo.pre <- Datos.Curva %>% filter(titulo == "BONOS") %>% 
             group_by(mes = cut(`Fecha de Vencimiento`, "month"),
            `Nemotécnico del Emisor`,`Nemotécnico del instrumento`) %>% 
            summarise(cantidad = n(), .groups = "keep") %>% 
            ungroup()

# Cantidad de bonos por emisores e instrumentos al mes TUDES:
pre.mes.ve.ins.seg.tudes <- mes.ve.nemo.pre %>%
                    filter(as.Date("2014-12-01")<as.Date(mes)) %>%
                    mutate(ano = year(as.Date(mes)), mes = month(as.Date(mes))) %>% 
                    filter(`Nemotécnico del Emisor` %in% c("BCCR","G"), 
                   `Nemotécnico del instrumento` %in% c("TUDES", "tudes")) %>% 
                    group_by(ano, mes) %>% 
                    summarise(cantidad = sum(cantidad), .groups = "keep") %>% 
                    ungroup() 

# Mediana y Promedio mensual:
PRE.media.ve.mes <- mean(PRE.mes.ve.ins.seg$cantidad) 
PRE.media.ve.mes
PRE.mediana.ve.mes <- median(PRE.mes.ve.ins.seg$cantidad)
PRE.mediana.ve.mes

# Segregamos por los nemotécnicos que necesitamos:
pre.op.seg.pre.tudes <- Datos.Curva %>% 
                    mutate(mes.op = month(as.Date(`Fecha de Operación`))) %>% 
                    filter(`Nemotécnico del Emisor` %in% c("BCCR","G"), 
                    `Nemotécnico del instrumento` %in% c("TUDES", "tudes")) %>% 
                     group_by(ano.op = year(as.Date(`Fecha de Operación`)),
                               mes.op = month(as.Date(`Fecha de Operación`)), 
                               ano.ve = year(`Fecha de Vencimiento`),
                              mes.ve =month(`Fecha de Vencimiento`)) %>% 
                      summarise(cantidad = n(), .groups = "keep") %>% 
                      ungroup() 
