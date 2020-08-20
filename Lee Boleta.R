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
   colnames(tbl)<-names
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

