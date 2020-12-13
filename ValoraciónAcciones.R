#              
#                            Consultoría RORAC-SUPEN 
#                 Metodología RORAC para títulos clasificados como 
#                                  Acciones

# Autores:
# Alexa Sánchez
# Isaac Z. Arias

# El siguiente módulo realiza el modelage de las transacciones clasificadas 
# con la metodología de acción con el fin de categorizar la información y crear el RORAC. 

# Paquete necesarios:
library(stringr)
library(readxl)
library(readr)
library(matlib)
library(stringi)
library(dplyr)
library(purrr)
library(tools)
library(wrMisc)
library(tidyr)
library(stringr)
library(lubridate)
library(ggplot2)
library(ClustImpute)
options(stringsAsFactors = FALSE)
options(scipen=999, digits = 8)

#------------------------------------------ Carga de Datos:

# Dirreccion de los datos:
Dir <- "C:/Users/EQUIPO/Desktop/Estudios/RORAC-SUPEN/Transacciones"

# Función para leer los datos

#Input: archivos .txt que contienen la información de las transacciones
lee.datos <<- function(archivo){
  
  tbl<- as.data.frame(unclass(read.table(archivo, 
                                         header = TRUE, 
                                         encoding = "Latin1",
                                         stringsAsFactors = F)))
  return(tbl)
}

# Funcion para obtener los datos de las carpetas

#Input: dirección de la carpeta donde se encuentran los archivos con las transacciones
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

transacciones.lista <- lista.df(Dir)
names.1 <- colnames(transacciones.lista[[14]])
names.2 <- colnames(transacciones.lista[[1]])
col.nuevas <- as.data.frame(names.1[which(!names.1%in%names.2)])
col.select <- names.1[which(names.1%in%names.2)]

# Se juntan todos los datos:
data.inicial <- map(transacciones.lista, 
                    function(x) x %>% select(all_of(col.select)))

# Se utiliza la nueva codificación:
data.inicial <- do.call("rbind", data.inicial) %>%
  mutate(COD_MOD_ESTANDAR = 
           case_when(
             COD_MOD_INV %in% c("DR", "DO", "DT", "DD","DC") ~ "D2",
             COD_MOD_INV %in% c("A1", "V1", "M1") ~ "P1",
             COD_MOD_INV %in% c("A2", "V2", "M2") ~ "P2",
             COD_MOD_INV %in% c("A3", "V3", "M3") ~ "P3",
             COD_MOD_INV %in% c("FP", "FT", "WC", "WP","WT",
                                "OC", "OP", "OT", 
                                "SC", "SP", "ST") ~ "FC",
             TRUE ~ COD_MOD_INV))

#---- Ho-Lee Dólares

# Historial de Overnight de Estados Unidos: 
Overnight <- read.csv("overnightrate.csv",sep=',',dec='.',header = F)
Overnight1 <- read.csv("overnightrate (1).csv",sep=',',dec='.',header = F)
Overnight2 <- read.csv("overnightrate (2).csv",sep=',',dec='.',header = F)
Overnight3 <- read.csv("overnightrate (3).csv",sep=',',dec='.',header = F)
Overnight4 <- read.csv("overnightrate (4).csv",sep=',',dec='.',header = F)
Overnight5 <- read.csv("overnightrate (5).csv",sep=',',dec='.',header = F)
Overnight6 <- read.csv("overnightrate (6).csv",sep=',',dec='.',header = F)
Overnight7 <- read.csv("overnightrate (7).csv",sep=',',dec='.',header = F)
Overnight <- rbind(Overnight,Overnight1,Overnight2,Overnight3,Overnight4,Overnight5,Overnight6,Overnight7)
rm(Overnight1,Overnight2,Overnight3,Overnight4,Overnight5,Overnight6,Overnight7)
Overnight <- Overnight[,-3]
colnames(Overnight)=c('Fecha','Tasa')
Overnight <- Overnight %>% mutate(Tasa=Tasa/100) %>% mutate(Tasa_equi_Semes=((1+Tasa/360)^(360/2)-1)*2) %>% mutate(Fecha=as.Date(Fecha, format = '%m/%d/%Y'))

# Se calcula el delta para generar las proporciones u y d:
Overnight <- Overnight %>% mutate(Delta=log((1+Tasa/360))*360/12)

# Historial de curvas soberanas del Tesoro:
Curvas.Tes <- read_excel("tnc_18_22.xls", col_names = FALSE, skip = 5) %>% filter_all(any_vars(!is.na(.)))
Curvas.Tes <- Curvas.Tes[, -c(1,2)]
nom.col.i <- ymd(paste0("20",str_sub("tnc_18_22.xls", start = 5, end = 6),"-", "01","-","01"))
nom.col.f <- ymd(paste0("20",str_sub("tnc_18_22.xls", start = 8, end = 9),"-", "12","-","01"))
vec.nom <- seq.Date(from = nom.col.i, to = nom.col.f, by = "month")
nombre.col <- vec.nom[1:ncol(Curvas.Tes)]
colnames(Curvas.Tes) <- nombre.col
Curvas.Tes <- Curvas.Tes %>% mutate(Vencimiento = seq(6,1200,6))

#---- Ho-Lee Colones

# Historial de Tasas TRI de Costa Rica: 
TRI_colones <- read_excel("TRI colones.xlsx",col_types = c("date", "numeric"))
TRI_colones <- TRI_colones[seq(from = 1, to = nrow(TRI_colones), by = 7),]
TRI_colones<-TRI_colones %>% mutate(Delta = log((1+`1 semana`/100/52))*52/12)

#------------------------------------------- Parámetros del Módulo:

# Fecha Inicial:
Fecha.Inicial <- as.Date("03/31/2020", format = '%m/%d/%Y')

# Fijamos el tiempo a simular (12 meses):
tiempo <- 12

# Cantidad de simulaciones:
cant.simu <- 10000

#---- Ho-Lee Dólares

# Se calcula la varianza mensual:
varianza.mensual.dol<-30*var(Overnight$Delta)

# Generamos las proporciones de subida y bajada iniciales:
u2.dol<-1+sqrt(varianza.mensual.dol) 
d2.dol<-1-sqrt(varianza.mensual.dol)

# Fijamos el parámetro fijo k del modelo:
k.dol <- d2.dol/u2.dol

# Se crea la probabilidad martingala:
p.neutra.dol <- (1 - d2.dol)/(u2.dol - d2.dol)

# Se crea la probabilidad objetiva: 
p.objetiva.dol <- sum((Overnight$Tasa_equi_Semes[-nrow(Overnight)]-Overnight$Tasa_equi_Semes[-1]) < 0)/nrow(Overnight)

# Datos de tasas compuestas semestralmente observadas:
data.tasas <- Curvas.Tes %>% select(as.character(Fecha.Inicial-day(Fecha.Inicial)+1), Vencimiento) %>% 
  rename(Tasa = as.character(Fecha.Inicial-day(Fecha.Inicial)+1)) %>% mutate(Tasa=Tasa/100)
data.tasas <- rbind(c(mean(Overnight$Tasa_equi_Semes[which((month(Overnight$Fecha)==month(Fecha.Inicial) & year(Overnight$Fecha)==year(Fecha.Inicial)))]),0),data.tasas)

#---- Ho-Lee Colones

# Tasa del primer vencimiento TRI en el primer mes observado (marzo):
TRI.corta <- log(1+1.25/100/52)*52/12

# Se calcula la varianza mensual:
varianza.mensual.col <- 4*var(TRI_colones$Delta)

# Generamos las proporciones de subida y bajada iniciales:
u2.col <- 1+sqrt(varianza.mensual.col)
d2.col <- 1-sqrt(varianza.mensual.col)

# Se crea la probabilidad martingala:
p.neutra.col <- (1 - d2.col)/(u2.col - d2.col)

# Se crea la probabilidad objetiva:
p.objetiva.col <- sum((TRI_colones$`1 semana`[-nrow(TRI_colones)]-TRI_colones$`1 semana`[-1]) < 0)/nrow(TRI_colones)

# Fijamos el parámetro fijo k del modelo:
k.col <- d2.col/u2.col

# Fijamos los parámetros de la curva Nelson Siegel o Svensson encontrada:
Par.NS <- c(8.337060e-03, TRI.corta-8.337060e-03, -3.330267e-12, 1.811465e+01)
Par.SA <- c(6.253579e-03,-0.005212038,-2.791880e-05,6.6101177e-06,1.357261e+01,4.806624e+01)

# Parámetro que indica si el modelo utilizado es el Svensson (si es Nelson Siegel fijarlo como 0)
Svensson <- 1 

#------------------------------------------ Modelo de Acciones:

# Se extraen los títulos con categoría de acción:
matriz.R <- data.inicial  %>% 
  select(COD_MOD_ESTANDAR, COD_MOD_INV , everything()) %>% 
  filter(COD_MOD_INV %in% c("AC", "FA","FI", "P1", "P2" )) %>% 
  filter(is.na(FEC_VEN), !is.na(COD_ISIN), as.POSIXct(FEC_DAT) <= Fecha.Inicial) 

# Se encuentran cuales son los títulos a valorar:
titulos.ul <- matriz.R %>% 
  mutate(fec.valoracion = paste(year(FEC_DAT),month(FEC_DAT),sep="-")) %>% 
  filter(fec.valoracion==paste(year(Fecha.Inicial),month(Fecha.Inicial),sep="-")) %>% 
  select(COD_ISIN) %>% 
  unique()

# Se segregan los títulos:
matriz.R <- as.matrix(matriz.R %>% 
                        filter(COD_ISIN %in% titulos.ul$COD_ISIN) %>% 
                        select(COD_ISIN, FEC_DAT, VEC_PRE_MON) %>% 
                        group_by(COD_ISIN, FEC_DAT) %>%
                        mutate(VEC_PRE_MON = as.numeric(mean(VEC_PRE_MON))) %>% 
                        ungroup %>%
                        unique() %>% 
                        group_by(COD_ISIN, FEC_DAT) %>%
                        mutate(rn = row_number()) %>% 
                        ungroup %>%
                        spread(FEC_DAT, VEC_PRE_MON) %>% 
                        select(-rn))

# Precio Inicial sin segregar:
Ini.pre <- matriz.R[, c(1,ncol(matriz.R))] 

# Cambian los índices:
indices <- matriz.R[,1]
matriz.R <- matriz.R[,-1]
matriz.R <- apply(matriz.R, 2, as.numeric)
row.names(matriz.R) <- indices

# Total de observaciones por título:
titulo.ob <- as.data.frame(rowSums(!is.na(matriz.R)) > (nrow(titulos.ul)+1))
colnames(titulo.ob) <- "Criterio"

# Cantidad de titulos:
cant.tit <- sum(titulo.ob$Criterio)

# Se calculan los pesos de los elementos no validos y validos:
rep.acciones.val <- round(100*sum(as.numeric(matriz.R[titulo.ob$Criterio, ncol(matriz.R)]))/sum(as.numeric(matriz.R[,ncol(matriz.R)])),2)
rep.acciones.no.val <- round(100*sum(as.numeric(matriz.R[!titulo.ob$Criterio, ncol(matriz.R)]))/sum(as.numeric(matriz.R[,ncol(matriz.R)])),2)

# Se segregan los elementos a valorar:
matriz.R <- matriz.R[titulo.ob$Criterio,]

# Se calculan los rendimientos:
matriz.R <- (1+(matriz.R[,-1]-matriz.R[,-ncol(matriz.R)])/matriz.R[,-ncol(matriz.R)])^30
matriz.R[which(!is.finite(matriz.R))] <- NA

# Se imputan los datos:
matriz.R <- ClustImpute(as.data.frame(matriz.R), nr_cluster = round((cant.tit+1)/2), nr_iter = 10)$complete_data

# Se traspone la matriz:
matriz.R <- t(as.matrix(matriz.R))

# Se crean los conjuntos de eventos con clasificación jerarquica:
#acc.clas <- hclust(dist(matriz.R, method = "euclidean"), method = "ward.D2")
#acc.clas <- cutree(acc.clas, k = cant.tit+1)

# Se crean los conjuntos de eventos con clasificación por k-means:
acc.clas <- kmeans(matriz.R, cant.tit+1, iter.max = 1000, nstart = 1000, algorithm = "MacQueen")$cluste

# Se aplica la nueva clasificación:
matriz.R <- t(matriz.R)

# Porbabilidad Objetiva:
prob.objetiva <- as.data.frame(table(acc.clas))$Freq/sum(as.data.frame(table(acc.clas))$Freq)

# Se calculan los representantes:
matriz.R <- rowGrpMeans(matriz.R, as.factor(acc.clas))

# Segregamos los precios iniciales:
Ini.pre <- as.data.frame(Ini.pre) %>% filter(COD_ISIN %in% rownames(matriz.R))
rownames(Ini.pre) <- Ini.pre$COD_ISIN
Ini.pre <- as.matrix(as.data.frame(Ini.pre) %>% select(-COD_ISIN))
colnames(Ini.pre) <- "acción"
Ini.pre <- apply(Ini.pre, 2, as.numeric)

# Moneda por título:
cod.moneda <- data.inicial %>% filter(COD_ISIN %in% rownames(matriz.R)) %>% select(COD_ISIN, COD_MON) %>% unique()

#------------------------------------------ Funciones Ho-Lee Dólares

# Función para interpolar linealmente las tasas
inter.lin.tasas <- function(puntos){
  # Se calculan los precios mensuales interpolados:
  tasas <- approxfun(puntos$Vencimiento, puntos$Tasa)(0:tiempo)
  curva.meses <- data.frame(Vencimiento = 0:tiempo, TasasS = tasas)
  return(curva.meses)
}

# Creamos el vector de tasas interpoladas 
vec.tasas <- inter.lin.tasas(data.tasas)

# Creamos el vector de precios:
data.precios <- vec.tasas %>% mutate(Precio=(1+TasasS/2)^(-Vencimiento/6))
vec.precios <-data.precios %>% select(-TasasS)

# Función del parámetro de bajada:
d.t <- function(t, p, k){ 
  (k^(t-1))/((1 - p)*(k^(t-1)) + p)
} 

# Función para encontrar la tasa corta dado un tiempo (Tao):
Tasa.Corta.t.dol = function(tao, cant_sub, p){
  
  # Se obtiene el precio de acuerdo al vector de precios:
  P_0_t = vec.precios[tao,2]
  P_0_T = vec.precios[tao+1,2]
  
  # Se obtiene la tasa corta mediante la fórmula.
  r_t = log(P_0_t/P_0_T) - log(d.t(tao + 1, p, k.dol)) + cant_sub*log(k.dol)
  
  return(r_t)  
}

# Función que realiza la simulación de una trayectoria aleatoria dado un periodo (tiempo):
Arbol.HL.desc.dol <- function(tiempo, p){   
  
  # Forma la trayectoria aleatoria para cada periodo:
  trayectoria = rbernoulli(tiempo, p) 
  
  # Obtiene la cantidad de subidas acumuladas:
  vect_cant_sub = cumsum(trayectoria)
  
  # Aplica la función de la tasa corta para cada instante;
  vect_r_t = Vectorize(Tasa.Corta.t.dol)(1:tiempo, vect_cant_sub, p)
  
  return(vect_r_t)
}

#------------------------------------------ Funciones Ho-Lee Colones

# Función que genera el precio de la curva a un tiempo (Tao) dado:
Precio <- function(tao){
  if(tao == 0){
    precio = 1
  }else{
    if(Svensson == 1){
      Delta = Par.SA[1] * tao +
        Par.SA[2] * ((1-exp(-tao/Par.SA[5]))*Par.SA[5]) + 
        Par.SA[3] * (1-(tao/Par.SA[5]+1)*exp(-tao/Par.SA[5]))*Par.SA[5]^2 +
        ((Par.SA[4]*Par.SA[6]*Par.SA[5])/(Par.SA[5]-Par.SA[6])) * (((1-(tao/Par.SA[5]+1)*exp(-tao/Par.SA[5]))*Par.SA[5]^2) - ((1-(tao/Par.SA[6]+1)*exp(-tao/Par.SA[6]))*Par.SA[6]^2))
      
      precio = exp(-Delta)
    }
    
    else{
      
      # Se calculan primero los coeficientes:
      a = (1-exp(-tao/Par.NS[4]))/
        (tao/Par.NS[4])
      
      b = ((1-exp(-tao/Par.NS[4]))/
             (tao/Par.NS[4])) - exp(-tao/Par.NS[4])
      
      precio = exp(-(Par.NS[1] + Par.NS[2]*a + 
                       Par.NS[3]*b)*tao)
    }}
  return(precio)
}

# Función para encontrar la tasa corta dado un tiempo (Tao):
Tasa.Corta.t.col = function(tao, cant_sub, p){
  
  # Se obtiene el precio de acuerdo a las curvas Nelson-Siegel-Svensson
  P_0_t = Precio(tao)
  P_0_T = Precio(tao + 1)
  
  # Se obtiene la tasa corta mediante la fórmula.
  r_t = log(P_0_t/P_0_T) - log(d.t(tao + 1, p, k.col)) + cant_sub*log(k.col)
  
  return(r_t)  
}

# Función que realiza la simulación de una trayectoria aleatoria dado un periodo (tiempo):
Arbol.HL.desc.col <- function(tiempo, p){   
  
  # Forma la trayectoria aleatoria para cada periodo:
  trayectoria = rbernoulli(tiempo, p) 
  
  # Obtiene la cantidad de subidas acumuladas:
  vect_cant_sub = cumsum(trayectoria)
  
  # Aplica la función de la tasa corta para cada instante;
  vect_r_t = Vectorize(Tasa.Corta.t.col)(1:tiempo, vect_cant_sub, p)
  
  return(vect_r_t)
}

#------------------------------------------ Simulación de Portafolio:

# Se crea una función que genere una trayectoria para el valor del portafolio:
Trayec.portafolio.obj <- function(tiempo){
  
  # Trayectoria de tasas cortas en dólares:
  trayec.corta.dol <- Arbol.HL.desc.dol(tiempo, p.objetiva.dol)
  
  # Trayectoria de tasas cortas en colones:
  trayec.corta.dol <- Arbol.HL.desc.col(tiempo, p.objetiva.col)
  
  # Inicializamos el precio de los títulos:
  precio.tit <- Ini.pre
  
  for(periodo in 1:tiempo){
    # Matriz de Posibilidades futuras:
    matriz.D <- matriz.R*matrix(rep(precio.tit, ncol(matriz.R)), 
                                nrow = ncol(matriz.R)-1)
    
    # Se generan los precios del periodo con probabilidad objetiva:
    precio.tit <- matriz.D%*%rmultinom(1, 1, prob.objetiva)
  }
  
  # Separación de Portafolios por moneda:
  port.col <- rbind(c("Cuenta Bancaria", trayec.corta.dol[tiempo]),
                    data.frame(COD_ISIN = (cod.moneda %>% filter(COD_MON==1))$COD_ISIN, 
                               valor = precio.tit[row.names(precio.tit) %in% (cod.moneda %>% 
                                                                                filter(COD_MON==1))$COD_ISIN,]))
  port.dol <- rbind(c("Cuenta Bancaria", trayec.corta.dol[tiempo]),
                    data.frame(COD_ISIN = (cod.moneda %>% filter(COD_MON==2))$COD_ISIN, 
                               valor = precio.tit[row.names(precio.tit) %in% (cod.moneda %>% 
                                                                                filter(COD_MON==2))$COD_ISIN,]))
  # Se corrigen los indices en caso de un único título:
  if(nrow(port.col)==2){
    row.names(port.col) <- port.col$COD_ISIN
  }
  if(nrow(port.dol)==2){
    row.names(port.dol) <- port.dol$COD_ISIN
  }
  port.col <- port.col %>% select(-COD_ISIN)
  port.dol <- port.dol %>% select(-COD_ISIN)
  row.names(port.col)[1] <- row.names(port.dol)[1] <- "Cuenta Bancaria"
  
  # Valor del portafolio:
  valor.port <- list(Colones = port.col, Dólares = port.dol)
  return(valor.port)
}

# Se genera la simulación estocástica:
simu.port <- Vectorize(Trayec.portafolio.obj)(rep(tiempo, cant.simu))
