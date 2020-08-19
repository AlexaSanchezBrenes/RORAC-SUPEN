#Crea lista de data frames con las boletas
lista.df.boletas<-function(path="~/RORAC SUPEN/Emisiones/Boletas"){
  
 
  folder<-list.files(path,full.name = TRUE )
  n<-length(folder)
  archivo<-numeric(n)
  lista.df<-list()
  
  for(i in 1:n){
    
    if(length(list.files(path = folder[i], pattern = ".txt")) != 0){
      archivo[i]<- list.files(path = folder[i], pattern = ".txt")
      lista.df[[i]]<-lee.boleta(paste0(folder[i],"/",archivo[i]))
       }else{
      archivo[i]<- list.files(path = folder[i], pattern = ".csv")}
    
    
  }
 names(lista.df)<-archivo  
return(lista.df)
  
}