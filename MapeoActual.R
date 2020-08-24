#              
#                    Consultoría RORAC-SUPEN 
#                        Mapeo de Datos 

# Autores:
# Alexa Sánchez
# Laura Campos
# Isaac Z. Arias

# El siguiente módulo realiza un mapeo estadístico sobre los datos 
# obtenidos por la Bolsa Nacional de Valores sobre las transacciones 
# de títulos.

# Paquete necesarios:
library(readr)
library(stringi)
library(dplyr)
library("tools")
library(stringr)
library(lubridate)
options(stringsAsFactors = FALSE)

# Dirrección de los datos:
Dic <- "C:/Users/EQUIPO/Desktop/Estudios/RORAC-SUPEN/Boletas"

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

# Eliminamos variables que momentaneamente no son necesarias:
Boletas <- Boletas %>% select(-Dias.de.vencimiento.del.Instrumento,-Tis,-Fecha.Ultimo.Pago.Intereses,
                              -Fecha.Vencimiento.Plazo,-Plazo.de.la.operacion,-Garantia.de.la.recompra,
                              -Indicador.de.subasta,-Moneda.de.liquidacion,-Dias.Acumulados.Intereses,
                              -Tir,-Fecha.de.Renovacion,-Dias.de.recompra,-Rendimiento.de.la.recompra,
                              -Operacion.Cruzada, -Numero.de.Contrato) %>% 
  mutate(Fecha.de.Operacion = as.POSIXct(Fecha.de.Operacion, format = "%Y/%m/%d %H:%M:%S"),
         Fecha.de.Vencimiento = as.POSIXct(Fecha.de.Vencimiento, format = "%Y/%m/%d %H:%M:%S")) %>% 
  filter(Mercado.de.Negociacion == "MERCADO SECUNDARIO") %>%
  mutate(titulo = ifelse(is.na(Fecha.de.Vencimiento), "ACCIONES O FONDOS", "BONOS"))

#-------------------#
# Analisis por día: #
#-------------------#

# Por Fecha de Operación:

# Cantidad de titulos por día:
dia.op.titulos <- Boletas %>% group_by(Fecha.de.Operacion, titulo) %>% 
  summarise(cantidad = n(), .groups = "keep") %>% ungroup()

# Titulos promedio diario:
dia.op.prom <- dia.op.titulos %>% group_by(titulo) %>% summarise(promedio = mean(cantidad), .groups = "keep") %>% ungroup()

# Cantidad de títulos por emisores e instrumento al día:
dia.op.nemo <- Boletas %>% 
  group_by(titulo,Fecha.de.Operacion,Nemotecnico.del.Emisor,Nemotecnico.del.instrumento) %>% 
  summarise(cantidad = n(), .groups = "keep") %>% 
  ungroup()

# Cantidad de títulos por instrumento al día:
dia.op.ins <- dia.op.nemo %>% 
  group_by(titulo,Fecha.de.Operacion,Nemotecnico.del.instrumento) %>% 
  summarise(cantidad = sum(cantidad), .groups = "keep") %>% 
  ungroup()

# Cantidad de títulos por emisor al día:
dia.op.emi <- dia.op.nemo %>% 
  group_by(titulo,Fecha.de.Operacion,Nemotecnico.del.Emisor) %>% 
  summarise(cantidad = sum(cantidad), .groups = "keep") %>% 
  ungroup()

# Promedio diario de títulos por emisor:
dia.op.emi.prom <- dia.op.emi %>% group_by(titulo,Nemotecnico.del.Emisor) %>% 
  summarise(promedio = mean(cantidad), .groups = "keep") %>% 
  ungroup()

# Promedio diario de títulos por instrumento:
dia.op.ins.prom <- dia.op.ins %>% group_by(titulo,Nemotecnico.del.instrumento) %>% 
  summarise(promedio = mean(cantidad), .groups = "keep") %>% 
  ungroup()

# Por Fecha de Vencimiento:

# Cantidad de bonos por día:
dia.ve.titulos <- Boletas %>% filter(titulo == "BONOS") %>% group_by(Fecha.de.Vencimiento) %>% 
  summarise(cantidad = n(), .groups = "keep") %>% ungroup()

# Bonos promedio diario:
dia.ve.prom <- dia.ve.titulos %>% summarise(promedio = mean(cantidad), .groups = "keep")

# Cantidad de bonos por emisores e instrumento al día:
dia.ve.nemo <- Boletas %>% filter(titulo == "BONOS") %>% 
  group_by(Fecha.de.Vencimiento,Nemotecnico.del.Emisor,Nemotecnico.del.instrumento) %>% 
  summarise(cantidad = n(), .groups = "keep") %>% 
  ungroup()

# Cantidad de bonos por instrumento al día:
dia.ve.ins <- dia.ve.nemo %>% 
  group_by(Fecha.de.Vencimiento,Nemotecnico.del.instrumento) %>% 
  summarise(cantidad = sum(cantidad), .groups = "keep") %>% 
  ungroup()

# Cantidad de bonos por emisor al día:
dia.ve.emi <- dia.ve.nemo %>% 
  group_by(Fecha.de.Vencimiento,Nemotecnico.del.Emisor) %>% 
  summarise(cantidad = sum(cantidad), .groups = "keep") %>% 
  ungroup()

# Promedio diario de bonos por emisor:
dia.ve.emi.prom <- dia.ve.emi %>% group_by(Nemotecnico.del.Emisor) %>% 
  summarise(promedio = mean(cantidad), .groups = "keep") %>% 
  ungroup()

# Promedio diario de bonos por instrumento:
dia.ve.ins.prom <- dia.ve.ins %>% group_by(Nemotecnico.del.instrumento) %>% 
  summarise(promedio = mean(cantidad), .groups = "keep") %>% 
  ungroup()

#----------------------#
# Analisis por semana: #
#----------------------#

# Por Fecha de Operación:

# Cantidad de titulos por semana:
sem.op.titulos <- Boletas %>% group_by(semana = cut(Fecha.de.Operacion, "week"), titulo) %>% 
  summarise(cantidad = n(), .groups = "keep") %>% ungroup()

# Titulos promedio semanal:
sem.op.prom <- sem.op.titulos %>% group_by(titulo) %>% 
  summarise(promedio = mean(cantidad), .groups = "keep") %>% ungroup()

# Cantidad de títulos por emisores e instrumento al mes:
sem.op.nemo <- Boletas %>% 
  group_by(titulo,semana = cut(Fecha.de.Operacion, "week"),Nemotecnico.del.Emisor,Nemotecnico.del.instrumento) %>% 
  summarise(cantidad = n(), .groups = "keep") %>% 
  ungroup()

# Cantidad de títulos por instrumento al mes:
sem.op.ins <- sem.op.nemo %>% 
  group_by(titulo,semana,Nemotecnico.del.instrumento) %>% 
  summarise(cantidad = sum(cantidad), .groups = "keep") %>% 
  ungroup()

# Cantidad de títulos por emisor a la semana:
sem.op.emi <- sem.op.nemo %>% 
  group_by(titulo,semana,Nemotecnico.del.Emisor) %>% 
  summarise(cantidad = sum(cantidad), .groups = "keep") %>% 
  ungroup()

# Promedio semanal de títulos por emisor:
sem.op.emi.prom <- sem.op.emi %>% group_by(titulo,Nemotecnico.del.Emisor) %>% 
  summarise(promedio = mean(cantidad), .groups = "keep") %>% 
  ungroup()

# Promedio semanal de títulos por instrumento:
sem.op.ins.prom <- sem.op.ins %>% group_by(titulo,Nemotecnico.del.instrumento) %>% 
  summarise(promedio = mean(cantidad), .groups = "keep") %>% 
  ungroup()

# Por Fecha de Vencimiento:

# Cantidad de bonos por semana:
sem.ve.titulos <- Boletas %>% filter(titulo == "BONOS") %>% group_by(semana = cut(Fecha.de.Vencimiento, "week")) %>% 
  summarise(cantidad = n(), .groups = "keep") %>% ungroup()

# Bonos promedio semanal:
sem.ve.prom <- sem.ve.titulos %>% summarise(promedio = mean(cantidad), .groups = "keep")

# Cantidad de bonos por emisores e instrumento a al semana:
sem.ve.nemo <- Boletas %>% filter(titulo == "BONOS") %>% 
  group_by(semana = cut(Fecha.de.Vencimiento, "week"),Nemotecnico.del.Emisor,Nemotecnico.del.instrumento) %>% 
  summarise(cantidad = n(), .groups = "keep") %>% 
  ungroup()

# Cantidad de bonos por instrumento a la semana:
sem.ve.ins <- sem.ve.nemo %>% 
  group_by(semana,Nemotecnico.del.instrumento) %>% 
  summarise(cantidad = sum(cantidad), .groups = "keep") %>% 
  ungroup()

# Cantidad de bonos por emisor a la semana:
sem.ve.emi <- sem.ve.nemo %>% 
  group_by(semana,Nemotecnico.del.Emisor) %>% 
  summarise(cantidad = sum(cantidad), .groups = "keep") %>% 
  ungroup()

# Promedio semanal de bonos por emisor:
sem.ve.emi.prom <- sem.ve.emi %>% group_by(Nemotecnico.del.Emisor) %>% 
  summarise(promedio = mean(cantidad), .groups = "keep") %>% 
  ungroup()

# Promedio semanal de bonos por instrumento:
sem.ve.ins.prom <- sem.ve.ins %>% group_by(Nemotecnico.del.instrumento) %>% 
  summarise(promedio = mean(cantidad), .groups = "keep") %>% 
  ungroup()

#-------------------#
# Analisis por mes: #
#-------------------#

# Por Fecha de Operación:

# Cantidad de titulos por mes:
mes.op.titulos <- Boletas %>% group_by(mes = cut(Fecha.de.Operacion, "month"), titulo) %>% 
  summarise(cantidad = n(), .groups = "keep") %>% ungroup()

# Titulos promedio mensual:
mes.op.prom <- mes.op.titulos %>% group_by(titulo) %>% 
  summarise(promedio = mean(cantidad), .groups = "keep") %>% ungroup()

# Cantidad de títulos por emisores e instrumento al mes:
mes.op.nemo <- Boletas %>% 
  group_by(titulo,mes = cut(Fecha.de.Operacion, "month"),Nemotecnico.del.Emisor,Nemotecnico.del.instrumento) %>% 
  summarise(cantidad = n(), .groups = "keep") %>% 
  ungroup()

# Cantidad de títulos por instrumento al mes:
mes.op.ins <- mes.op.nemo %>% 
  group_by(titulo,mes,Nemotecnico.del.instrumento) %>% 
  summarise(cantidad = sum(cantidad), .groups = "keep") %>% 
  ungroup()

# Cantidad de títulos por emisor al mes:
mes.op.emi <- mes.op.nemo %>% 
  group_by(titulo,mes,Nemotecnico.del.Emisor) %>% 
  summarise(cantidad = sum(cantidad), .groups = "keep") %>% 
  ungroup()

# Promedio mensual de títulos por emisor:
mes.op.emi.prom <- mes.op.emi %>% group_by(titulo,Nemotecnico.del.Emisor) %>% 
  summarise(promedio = mean(cantidad), .groups = "keep") %>% 
  ungroup()

# Promedio mensual de títulos por instrumento:
mes.op.ins.prom <- mes.op.ins %>% group_by(titulo,Nemotecnico.del.instrumento) %>% 
  summarise(promedio = mean(cantidad), .groups = "keep") %>% 
  ungroup()

# Por Fecha de Vencimiento:

# Cantidad de bonos por mes:
mes.ve.titulos <- Boletas %>% filter(titulo == "BONOS") %>% group_by(mes = cut(Fecha.de.Vencimiento, "month")) %>% 
  summarise(cantidad = n(), .groups = "keep") %>% ungroup()

# Bonos promedio mensual:
mes.ve.prom <- mes.ve.titulos %>% summarise(promedio = mean(cantidad), .groups = "keep")

# Cantidad de bonos por emisores e instrumento al mes:
mes.ve.nemo <- Boletas %>% filter(titulo == "BONOS") %>% 
  group_by(mes = cut(Fecha.de.Vencimiento, "month"),Nemotecnico.del.Emisor,Nemotecnico.del.instrumento) %>% 
  summarise(cantidad = n(), .groups = "keep") %>% 
  ungroup()

# Cantidad de bonos por instrumento al mes:
mes.ve.ins <- mes.ve.nemo %>% 
  group_by(mes,Nemotecnico.del.instrumento) %>% 
  summarise(cantidad = sum(cantidad), .groups = "keep") %>% 
  ungroup()

# Cantidad de bonos por emisor al mes:
mes.ve.emi <- mes.ve.nemo %>% 
  group_by(mes,Nemotecnico.del.Emisor) %>% 
  summarise(cantidad = sum(cantidad), .groups = "keep") %>% 
  ungroup()

# Promedio mensual de bonos por emisor:
mes.ve.emi.prom <- mes.ve.emi %>% group_by(Nemotecnico.del.Emisor) %>% 
  summarise(promedio = mean(cantidad), .groups = "keep") %>% 
  ungroup()

# Promedio mensual de bonos por instrumento:
mes.ve.ins.prom <- mes.ve.ins %>% group_by(Nemotecnico.del.instrumento) %>% 
  summarise(promedio = mean(cantidad), .groups = "keep") %>% 
  ungroup()

