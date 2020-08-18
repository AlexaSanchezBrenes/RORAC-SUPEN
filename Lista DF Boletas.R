#Crea lista de data frames con las boletas
lista.df.boletas<-function(path="C:/Users/Alexa/Desktop/RORAC/Datos emisiones 1/Boletas"){
  setwd(path)
  folder<-list.files(path,full.name = TRUE )
  n<-length(folder)
  archivo<-numeric(n)
  lista.df<-list()
  
  for(i in 1:n){
    
    if(length(list.files(path = folder[i], pattern = ".txt")) != 0){
      archivo[i]<- list.files(path = folder[i], pattern = ".txt")
      }else{
      archivo[i]<- list.files(path = folder[i], pattern = ".csv")}
  }
    
#for(i in 1:n){
#  lista.df[[i]]<-lee.boleta(archivo[i])
#}
  
}