#Crea lista de data frames con las boletas
lista.df.boletas<-function(path="~/RORAC SUPEN/Emisiones/Boletas"){
 
  folder<-list.files(path,full.name = TRUE )
  n<-length(folder)
  archivo<-numeric(n)
  lista.df<-list()
  lista.resumenes<-list()
  
  for(i in 1:n){
    if(length(list.files(path = folder[i], pattern = ".txt")) != 0){
      archivo[i]<- list.files(path = folder[i], pattern = ".txt")
       }else{
      archivo[i]<- list.files(path = folder[i], pattern = ".csv")}
    
     lista.df[[i]]<-lee.boleta(paste0(folder[i],"/",archivo[i]))
     #lista.resumenes[[i]]<-resumen.boleta(lista.df[[i]])
  }
  
 names(lista.df)<-archivo  
return(lista.df)
  
}