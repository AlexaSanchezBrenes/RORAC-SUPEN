#Recibe un txt o csv con la informacion de la emision para un periodo dado.
lee.boleta<<-function(archivo){
  library(readr)
  library(stringr)
  library(dplyr)
  
if(substring(archivo, 1, 6) =="Boleta"){ 
  tbl<-as.data.frame(unclass(read.table(archivo, 
                                                header = TRUE, sep = ",", 
                                                fileEncoding = "UTF-8")))
 }else{
   tbl<- as.data.frame(unclass(read.table(archivo, 
                                        header = TRUE, sep = ",")))
 }
  
  
  
               
  df<-tbl%>% 
    rename(Numero.de.Contracto = "Número.de.Contrato",
           Numero.de.Contracto.Largo = "Número.de.Contrato.Largo",
           Mercado.de.Negociacion = "Mercado.de.Negociación",
           Nemotecnico.del.Emisor = "Nemotécnico.del.Emisor",
           Nemotecnico.del.instrumento = "Nemotécnico.del.instrumento",
           Fecha.de.Operacion = "Fecha.de.Operación",
           Fecha.de.Renovacion = "Fecha.de.Renovación",
           Fecha.Ultimo.Pago.Intereses = "Fecha.Último.Pago.Intereses",
           Plazo.de.la.operacion = "Plazo.de.la.operación",
           Moneda.de.liquidacion = "Moneda.de.liquidación",
           Operacion.Cruzada = "Operación.Cruzada") %>% 
   mutate(Mercado.de.Negociacion = as.character( unlist(Mercado.de.Negociacion)),
           Nemotecnico.del.Emisor  = as.character(unlist(Nemotecnico.del.Emisor)),
           Nemotecnico.del.instrumento = as.character(unlist(Nemotecnico.del.instrumento)),
           Tasa.facial = as.numeric(str_replace(Tasa.facial, ",", ".")),
           Precio = as.numeric(str_replace(Precio, ",", ".")),
           Valor.Transado = as.numeric(str_replace(Valor.Transado, ",", ".")),
           Valor.facial = as.numeric(str_replace(Valor.facial, ",", ".")),
           Tis = as.numeric(str_replace(Tis, ",", ".")),
           Tir = as.numeric(str_replace(Tir, ",", ".")),
           Fecha.de.Operacion = as.Date(as.character.Date(Fecha.de.Operacion)),
           Fecha.de.Vencimiento = as.Date(as.character.Date(Fecha.de.Vencimiento)),
           Plazo.de.la.operacion = as.character(Plazo.de.la.operacion),
           Rendimiento.de.la.recompra = as.numeric(str_replace(Rendimiento.de.la.recompra , ",", ".")),
           Moneda.del.instrumento = as.character(unlist(Moneda.del.instrumento)),
           Moneda.de.liquidacion = as.character(unlist( Moneda.de.liquidacion)),
           Operacion.Cruzada = as.character(unlist( Operacion.Cruzada)),
           Recompra = as.character(unlist( Recompra))) 
  
  return(as.data.frame(df))
}
