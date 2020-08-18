lee.boleta<<-function(archivo){
  library(readr)
  library(stringr)
  library(dplyr)
  
  tbl<-as.data.frame(unclass(read.table(archivo, 
                                        header = TRUE, sep = ",",
                                        fileEncoding = "UTF-8"))) %>% 
    mutate(Mercado.de.Negociaci?n = as.character( unlist(Mercado.de.Negociaci?n)),
           Nemot?cnico.del.Emisor = as.character(unlist(Nemot?cnico.del.Emisor)),
           Nemot?cnico.del.instrumento = as.character(unlist(Nemot?cnico.del.instrumento)),
           Tasa.facial = as.numeric(str_replace(Tasa.facial, ",", ".")),
           Precio = as.numeric(str_replace(Precio, ",", ".")),
           Valor.Transado = as.numeric(str_replace(Valor.Transado, ",", ".")),
           Valor.facial = as.numeric(str_replace(Valor.facial, ",", ".")),
           Tis = as.numeric(str_replace(Tis, ",", ".")),
           Tir = as.numeric(str_replace(Tir, ",", ".")),
           Fecha.de.Operaci?n = as.Date(as.character.Date(Fecha.de.Operaci?n)),
           Fecha.de.Vencimiento = as.Date(as.character.Date(Fecha.de.Vencimiento)),
           Plazo.de.la.operaci?n = as.character(Plazo.de.la.operaci?n),
           Rendimiento.de.la.recompra = as.numeric(str_replace(Rendimiento.de.la.recompra , ",", ".")),
           Moneda.del.instrumento = as.character(unlist(Moneda.del.instrumento)),
           Moneda.de.liquidaci?n = as.character(unlist( Moneda.de.liquidaci?n)),
           Operaci?n.Cruzada = as.character(unlist( Operaci?n.Cruzada)),
           Recompra = as.character(unlist( Recompra)))
  return(as.data.frame(tbl))
}