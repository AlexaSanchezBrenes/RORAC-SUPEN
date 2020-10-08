#              
#                    Consultoría RORAC-SUPEN 
#                 Mapeo y categorización de las transacciones 

# Autores:
# Alexa Sánchez
# Laura Campos
# Isaac Z. Arias

# El siguiente módulo realiza un mapeo de las transacciones, con el fin de categorizar 
#la información

# Paquete necesarios:
library(readr)
library(stringi)
library(dplyr)
library("tools")
library(stringr)
library(lubridate)
options(stringsAsFactors = FALSE)

# Dirrección de los datos:
Dir <- "~/RORAC SUPEN/Transacciones"

# Función para leer los datos

#Input: archivos .txt que contienen la información de las transacciones
lee.datos<<-function(archivo){
  
    tbl<- as.data.frame(unclass(read.table(archivo, 
                                           header = TRUE, 
                                           encoding = "Latin1",
                                           stringsAsFactors = F)))
  return(tbl)
}

# Función para obtener los datos de las carpetas

#Input: dirección ed la carpeta donde se encuentran los archivos con las transacciones
#Output: Lista con todas las transacciones
lista.df<-function(path=Dir){
  
  folder<-list.files(path,full.name = TRUE )
  n<-length(folder)
  lista.df<-list()
  
  for(i in 1:n){
    lista.df[[i]] <-lee.datos(folder[i])
  }
  return(lista.df)
}

transacciones <- lista.df(Dir)
names.1 <- colnames(transacciones[[14]])
names.2 <- colnames(transacciones[[1]])

# Columnas que se agregan a la data a partir de Enero 2020
col.nuevas <- as.data.frame(names.1[which(!names.1%in%names.2)])
col.seleccion <- names.1[which(names.1%in%names.2)]

## Se seleccionan las columnas en común dados los dos grupos de información
transacciones.2 <- lapply(transacciones, 
                        function(x) x%>% select(col.seleccion))
transacciones.2 <- do.call("rbind", transacciones.2)
