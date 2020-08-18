#Recibe un txt o csv con la informacion de la emision para un periodo dado.
lee.boleta<<-function(archivo){
  library(readr)
  library(stringr)
  library(dplyr)
  
  tbl<-as.data.frame(unclass(read.table(archivo, 
                                        header = TRUE, sep = ","))) %>% 
    mutate(Mercado.de.Nego = as.character( unlist(tbl[,3])),
           Emisor = as.character(unlist(tbl[,4])),
           Instrumento = as.character(unlist(tbl[,5])),
           Tasa.facial = as.numeric(str_replace(Tasa.facial, ",", ".")),
           Precio = as.numeric(str_replace(Precio, ",", ".")),
           Valor.Transado = as.numeric(str_replace(Valor.Transado, ",", ".")),
           Valor.facial = as.numeric(str_replace(Valor.facial, ",", ".")),
           Tis = as.numeric(str_replace(Tis, ",", ".")),
           Tir = as.numeric(str_replace(Tir, ",", ".")),
           Fecha.de.Operacion = as.Date(as.character.Date(tbl[,15])),
           Fecha.de.Vencimiento = as.Date(as.character.Date(Fecha.de.Vencimiento)),
           Plazo.Operacion= as.character(tbl[,21]),
           Rendimiento.de.la.recompra = as.numeric(str_replace(Rendimiento.de.la.recompra , ",", ".")),
           Moneda.del.instrumento = as.character(unlist(Moneda.del.instrumento)),
           Moneda.de.liquidacion = as.character(unlist( tbl[,27])),
           Operacion.Cruzada = as.character(unlist( tbl[,28])),
           Recompra = as.character(unlist( Recompra))) %>% 
    select(-c(3,4,5,15,21,27,28))
  
  return(as.data.frame(tbl))
}
