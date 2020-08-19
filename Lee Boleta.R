#Recibe un txt o csv con la informacion de la emision para un periodo dado.
lee.boleta<<-function(archivo){
  library(readr)
  library(stringr)
  library(dplyr)
  options(stringsAsFactors = FALSE)
if(substring(archivo, 1, 6) =="Boleta"){ 
  tbl<-as.data.frame(unclass(read.table(archivo, 
                                        header = TRUE, 
                                        sep = ",", 
                                        encoding =  "UTF-8",
                                        stringsAsFactors = FALSE)))
 }else{
   tbl<- as.data.frame(unclass(read.table(archivo, 
                                          header = TRUE, 
                                          sep = ",", 
                                          encoding = "Latin1",
                                          stringsAsFactors = F)))
 }
  
  names<-stringi::stri_trans_general(colnames(tbl), "Latin-ASCII")
  colnames(tbl)<-names
  
  df<-tbl%>% 
   mutate(Tasa.facial = as.numeric(str_replace(Tasa.facial, ",", ".")),
           Precio = as.numeric(str_replace(Precio, ",", ".")),
           Valor.Transado = as.numeric(str_replace(Valor.Transado, ",", ".")),
           Valor.facial = as.numeric(str_replace(Valor.facial, ",", ".")),
           Tis = as.numeric(str_replace(Tis, ",", ".")),
           Tir = as.numeric(str_replace(Tir, ",", ".")),
           Fecha.de.Operacion = as.Date(as.character.Date(Fecha.de.Operacion)),
           Plazo.de.la.operacion = as.character(Plazo.de.la.operacion),
           Rendimiento.de.la.recompra = as.numeric(str_replace
                                        (Rendimiento.de.la.recompra , ",", ".")))
 
  
  return(as.data.frame(df))
}
