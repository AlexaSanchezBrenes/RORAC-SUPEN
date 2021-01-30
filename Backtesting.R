#                     Consultoría RORAC-SUPEN 
#     Backtesting del Cálculo de Rendimientos del Portafolio

# Autores:
# Alexa Sánchez
# Isaac Z. Arias

# En este script se calculan los rendimientos esperados del portafolio para ser 
# comparados con el valor del portafolio observado bajo ciertas hipótesis del
# modelo, de manera que se visualice la capacidad predictiva del modelo. 
# El código se distribuye de la siguiente manera:
#             
#
#
#
#
#############################################################################


                       #################################
                       ###                           ###
                       ###    1. Módulo General      ###
                       ###                           ###
                       #################################


############################ Paquetes Utilizados #############################


# Paquetes necesarios:
library(dplyr)
library(lubridate)
library(readxl)
library(stringr)
library(tictoc)
library(tidyr)
library(purrr)
library(ggplot2)
library(wrMisc)
library(ClustImpute)
library(nleqslv)
options(stringsAsFactors = FALSE)
options(scipen=999, digits = 8)


############################ Parámetros Generales ###########################


# Mes a partir del cual se parte para realizar el backtesting:
mes <- 3 

# Año del mes del cual se parte para realizar el backtesting:
anno <- 2020 

# Tasa del primer vencimiento TRI en el mes del cual se parte para realizar el backtetsing (marzo):
TRI.corta <- log(1+1.25/100/52)*52/12

# Fijamos los parámetros de la curva Nelson Siegel o Svensson encontrada para el mes inicial a partir del cual se realiza el backtetsing:
Par.NS <- c(8.337060e-03, TRI.corta-8.337060e-03, -3.330267e-12, 1.811465e+01)
Par.SA <- c(6.253579e-03,-0.005212038,-2.791880e-05,6.6101177e-06,1.357261e+01,4.806624e+01)

# Parámetro que indica si el modelo utilizado es el Svensson (si es Nelson Siegel fijarlo como 0)
Svensson <- 1 

# Cantidad de simulaciones:
cant.simu <- 10000

# Tipo de cambio de compra al cierre del mes en el que se inicia el backtesting:
TC <- 579.5 

# Se define el nivel de confianza:
confianza <- 1/100

# Cantidad de periodos hacia adelante para hacer backtesting:
Periodo_bt <- 5 

# Límite de rendimiento de acciones permitido:
lim.rend <- 700


############################## Carga de Datos ################################


# Dirrección de los datos:
Dir <- "C:/Users/EQUIPO/Desktop/Estudios/RORAC-SUPEN/Títulos"

# Funcion para leer los datos:
#Input: archivos .txt que contienen la información de las transacciones
lee.datos <<- function(archivo){
  
  tbl<- as.data.frame(unclass(read.table(archivo, 
                                         header = TRUE, 
                                         encoding = "Latin1",
                                         stringsAsFactors = F)))
  return(tbl)
}

# Funcion para obtener los datos de las carpetas

# Input: dirección de la carpeta donde se encuentran los archivos con las 
# transacciones
# Output: Lista con todas las transacciones
lista.df<-function(path=Dir){
  
  folder<-list.files(path,full.name = TRUE )
  n<-length(folder)
  #n<-14
  lista.df<-list()
  
  for(i in 1:n){
    lista.df[[i]] <-lee.datos(folder[i])
  }
  return(lista.df)
}

# Se ejecuta la carga de datos:
tabla <- lista.df(Dir)

# Carga de tipo de cambio:
tipo.Cambio.hist <- read_excel("Tipo de Cambio.xlsx", col_names = TRUE,
                               skip = 4) %>% 
  select(-`TIPO DE CAMBIO VENTA`)         


################ Manipulación Y Limpieza de Datos Generales ##################


# Unificación de variables dado el cambio de la codificación realizada por supen
titulos.viejos <- tabla[1:13]
titulos.nuevos <- tabla[14:26]
titulos.viejos <- do.call("rbind", titulos.viejos)
titulos.nuevos <- do.call("rbind", titulos.nuevos)
titulos.viejos <- titulos.viejos %>% select(COD_ENT,FEC_DAT,COD_MOD_INV,COD_INS,VAL_FAC,MAR,FEC_VEN,COD_ISIN,TAS_FAC,PER,COD_MON,VAL_MER,VEC_PRE_POR,VEC_PRE_MON,COD_EMI,COD_FON)
titulos.nuevos <- titulos.nuevos %>% select(COD_ENT,FEC_DAT,COD_MOD_INV,COD_INS,VAL_FAC,MAR_FIJ,FEC_VEN,COD_ISIN,TAS_FAC,TIP_PER,COD_MON,VAL_MER,VEC_PRE_POR,VEC_PRE_MON,COD_EMI,COD_FON,ES_REDE,ADMIN,COD_SEC)
titulos.viejos <- cbind(titulos.viejos, 
                        matrix(rep('NA', nrow(titulos.viejos)*(ncol(titulos.nuevos)-ncol(titulos.viejos))), ncol = (ncol(titulos.nuevos)-ncol(titulos.viejos))))
colnames(titulos.viejos) <- colnames(titulos.nuevos)

titulos.viejos <- titulos.viejos %>% filter(COD_ISIN %in% unique(titulos.nuevos$COD_ISIN))

titulos <- rbind(titulos.viejos,titulos.nuevos)

titulos <- titulos %>% filter(!(COD_FON %in% c('05','06')),COD_INS != 'index') %>% 
  mutate(TIP_PER=ifelse(COD_INS=='tp0' | COD_INS=='bem0',0,TIP_PER))

titulos <- titulos %>% mutate(COD_MOD_INV = 
                                case_when(COD_MOD_INV %in% c("DR","DO","DT", "DD") ~ "D2",
                                          COD_MOD_INV %in% c("M1","v1","A1") ~ "P1",
                                          COD_MOD_INV == "V2" ~ "P2",
                                          TRUE ~ COD_MOD_INV))


# Eliminamos recompras
titulos <- titulos %>% filter(COD_MOD_INV != 'RE')

# Filtramos los títulos que corresponden a bonos
BONOS <- titulos %>% 
  select(COD_ENT,FEC_DAT,COD_MOD_INV,COD_INS,VAL_FAC,MAR_FIJ,FEC_VEN,COD_ISIN,TAS_FAC,TIP_PER,COD_MON,VAL_MER,VEC_PRE_POR,VEC_PRE_MON,COD_EMI,ES_REDE,ADMIN,COD_SEC) %>%
  filter(!is.na(TIP_PER),!is.na(VAL_FAC),!is.na(FEC_VEN)) %>% mutate(PRECIO=VAL_MER/VAL_FAC)
BONOS.PER.NA <- titulos %>% 
  filter(COD_MOD_INV %in% c("DE","D1","D2","D3","DI","IE"),
         is.na(TIP_PER),!is.na(VAL_FAC),!is.na(FEC_VEN)) %>%
  select(COD_ENT,FEC_DAT,COD_MOD_INV,COD_INS,VAL_FAC,MAR_FIJ,FEC_VEN,COD_ISIN,TAS_FAC,TIP_PER,COD_MON,VAL_MER,VEC_PRE_POR,VEC_PRE_MON,COD_EMI,ES_REDE,ADMIN,COD_SEC) %>%
  mutate(TIP_PER=0) %>% mutate(PRECIO=VAL_MER/VAL_FAC)

#
BONOS <- rbind(BONOS,BONOS.PER.NA)
ACCIONES <- titulos %>% 
  filter(!(COD_ISIN %in% unique(BONOS$COD_ISIN))) 


# Se agregan los tipos de cambio del BCCR:
ACCIONES <- ACCIONES %>% ungroup() %>%  
  mutate(FEC_DAT=as.Date(FEC_DAT)) %>% 
  left_join(tipo.Cambio.hist, by = c("FEC_DAT" = "Fecha")) %>% 
  mutate(Precio = ifelse(COD_MON==2, (VAL_MER/`TIPO CAMBIO COMPRA`)/VAL_FAC, VAL_MER/VAL_FAC)) 

# Función para calcular la diferencia de fechas en meses:
Tau <- function(t, Te) {
  ed <- as.POSIXlt(Te)
  sd <- as.POSIXlt(t)
  as.double(12 * (ed$year - sd$year) + (ed$mon - sd$mon) + day(ed)/days_in_month(ed) - day(sd)/days_in_month(sd))
}

# Se borran los datos inecesarios
rm(tabla,titulos.viejos,titulos.nuevos,titulos)


#############################################################################


                       #################################
                       ###                           ###
                       ###   2. Módulo de Bonos      ###
                       ###                           ###
                       #################################


######################## Parámetros Del Modelo de Bonos ######################


# Fecha Inicial como objeto fecha:
Fecha.Inicial <- as.Date(paste0(mes,"/01/",anno), format = '%m/%d/%Y')

# Fecha final observada:
Fecha.Final <- max(BONOS$FEC_VEN)

# Distancia en Meses: 
tiempo <- ceiling(Tau(Fecha.Inicial, Fecha.Final))

# Curva de Espectativas de Inflación:
Inflacion <- rep(0.05, times=tiempo)
Inflacion <- as.data.frame(cbind(0:(tiempo-1),Inflacion))
colnames(Inflacion) <- c("Tau","Inflacion")

# Curva de Espectativas de Tasas de Interes:
Curva.Espectativas <- data.frame(Tau = 0:(tiempo-1), Tasa = rep(0.05, times = tiempo))


################## Carga y Manipulación Del Modelo de Bonos ##################


# Curva Soberana del Tesoro de Estados Unidos
# Historial de curvas soberanas del Tesoro:
Curvas.Tes <- read_excel("tnc_18_22.xls", col_names = FALSE, skip = 5) %>% filter_all(any_vars(!is.na(.)))
Curvas.Tes <- Curvas.Tes[, -c(1,2)]
nom.col.i <- ymd(paste0("20",str_sub("tnc_18_22.xls", start = 5, end = 6),"-", "01","-","01"))
nom.col.f <- ymd(paste0("20",str_sub("tnc_18_22.xls", start = 8, end = 9),"-", "12","-","01"))
vec.nom <- seq.Date(from = nom.col.i, to = nom.col.f, by = "month")
nombre.col <- vec.nom[1:ncol(Curvas.Tes)]
colnames(Curvas.Tes) <- nombre.col
Curvas.Tes <- Curvas.Tes %>% mutate(Vencimiento = seq(6,1200,6))

# Tasas overnight de Estados Unidades
Overnight <- read.csv("overnightrate (7).csv",sep=',',dec='.',header = F)
Overnight <- Overnight[,-3]
colnames(Overnight)=c('Fecha','Tasa')
Overnight <- Overnight %>% mutate(Tasa=Tasa/100) %>% mutate(TasaS=((1+Tasa/360)^(360/2)-1)*2) %>%
  mutate(Fecha=as.Date(Fecha, format = '%m/%d/%Y'))

# Datos de tasas compuestas semestralmente observadas:
data.tasas <- Curvas.Tes %>% select(as.character(Fecha.Inicial), Vencimiento) %>% 
  rename(Tasa = as.character(Fecha.Inicial)) %>% mutate(Tasa=Tasa/100)
data.tasas <- rbind(c(mean(Overnight$TasaS[which((month(Overnight$Fecha)==month(Fecha.Inicial) & year(Overnight$Fecha)==year(Fecha.Inicial)))]),0),data.tasas)

# Filtramos las observaciones del mes a valorar:
BONOS <- BONOS %>% mutate(PRECIO=ifelse(PRECIO>400,VEC_PRE_POR/100,PRECIO))
BONOS.TODO <- BONOS
Periodo <- 1 
BONOS <- BONOS %>% filter(year(FEC_DAT)==anno & month(FEC_DAT)==mes) %>% 
  filter(Tau(FEC_DAT,FEC_VEN)>=Periodo)

# Filtramos las observaciones por tasa fija y variable:
BONOS.TV <- BONOS %>% filter(MAR_FIJ!=0 | COD_INS %in% c('bemv')) 
BONOS.TF <- BONOS %>% filter(!(COD_ISIN %in% unique(BONOS.TV$COD_ISIN)))
BONOS.TF <- BONOS.TF %>% 
  filter(!is.na(TAS_FAC), TAS_FAC!=0) 


######################## Funciones Del Modelo de Bonos #######################


# Función para interpolar linealmente las tasas:
inter.lin.tasas <- function(puntos){
  # Se calculan los precios mensuales interpolados:
  tasas <- approxfun(puntos$Vencimiento, puntos$Tasa)(0:tiempo)
  curva.meses <- data.frame(Vencimiento = 0:tiempo, TasasS = tasas)
  return(curva.meses)
}

# Función que genera el precio de la curva a un tiempo (Tao) dado:
Precio <- function(tao,col_dol){
  if(tao == 0){
    precio = 1
  }else{
    if(col_dol==1){
      if(Svensson == 1){
        Delta = Par.SA[1] * tao +
          Par.SA[2] * ((1-exp(-tao/Par.SA[5]))*Par.SA[5]) + 
          Par.SA[3] * (1-(tao/Par.SA[5]+1)*exp(-tao/Par.SA[5]))*Par.SA[5]^2 +
          ((Par.SA[4]*Par.SA[6]*Par.SA[5])/(Par.SA[5]-Par.SA[6])) *
          (((1-(tao/Par.SA[5]+1)*exp(-tao/Par.SA[5]))*Par.SA[5]^2) - 
             ((1-(tao/Par.SA[6]+1)*exp(-tao/Par.SA[6]))*Par.SA[6]^2))
        
        precio = exp(-Delta)
      } else{
        
        # Se calculan primero los coeficientes:
        a = (1-exp(-tao/Par.NS[4]))/
          (tao/Par.NS[4])
        
        b = ((1-exp(-tao/Par.NS[4]))/
               (tao/Par.NS[4])) - exp(-tao/Par.NS[4])
        
        precio = exp(-(Par.NS[1] + Par.NS[2]*a + 
                         Par.NS[3]*b)*tao)
      }}
    else{
      precio <- (1+(approxfun(data.tasas$Vencimiento, data.tasas$Tasa)(tao))/2)^(-2*tao/12)
      
    }
    return(precio)
  }}

# Vectorización de la funcion del precio:
V_Precio <- Vectorize(Precio) 

# Función para calcular el precio teórico de cada bono considerando su riesgo crediticio:
PRECIO.TEORICO.TF <- function(fila,mu){
  if(fila[,"COD_INS"] %in% c("tudes","TUDES")){
    # Creamos el Tau del ponderador:
    if(fila[,"TIP_PER"]==0){
      tabla <- fila %>%  mutate(tau = Tau(FEC_DAT, FEC_VEN),
                                Fecha.Pago = FEC_VEN,Pago=1+(1+Inflacion_Interp(tau))*TAS_FAC/100)
    }else{
      Taus <- seq.Date(from = as.Date(fila[,"FEC_VEN"]),
                       to = as.Date(fila[,"FEC_DAT"]),
                       by = paste(as.character(-12/fila[,"TIP_PER"]),"months",sep=" "))
      tabla <- fila[rep(seq_len(nrow(fila)), each = length(Taus)), ] %>% 
        mutate(Fecha.Pago = Taus) %>% 
        mutate(tau = Tau(FEC_DAT, Fecha.Pago)) %>% 
        mutate(Pago=ifelse(Fecha.Pago==FEC_VEN,1+(1+Inflacion_Interp(tau))*TAS_FAC/100,(1+Inflacion_Interp(tau))*TAS_FAC/100))
    }
  } else if(fila[,"TIP_PER"]==0){
    tabla <- fila %>%  mutate(tau = Tau(FEC_DAT, FEC_VEN),
                              Fecha.Pago = FEC_VEN,Pago=1+TAS_FAC/100)
  }else{
    Taus <- seq.Date(from = as.Date(fila[,"FEC_VEN"]),
                     to = as.Date(fila[,"FEC_DAT"]),
                     by = paste(as.character(-12/fila[,"TIP_PER"]),"months",sep=" "))
    tabla <- fila[rep(seq_len(nrow(fila)), each = length(Taus)), ] %>% 
      mutate(Fecha.Pago = Taus) %>% 
      mutate(tau = Tau(FEC_DAT, Fecha.Pago)) %>% 
      mutate(Pago=ifelse(Fecha.Pago==FEC_VEN,1+TAS_FAC/100,TAS_FAC/100))
  }
  
  tabla<-tabla %>% mutate(exponencial=exp(-mu*tau),PrecioCC=V_Precio(tau,COD_MON)) %>% 
    mutate(PrecioTeorico = Pago*PrecioCC*exponencial) 
  Precio.Teorico=sum(tabla$PrecioTeorico)
  return(Precio.Teorico)
}

# Función de interpolación para la curva de espectativas de inflación:
Inflacion_Interp <- function(tau){
  return(approxfun(Inflacion$Tau,Inflacion$Inflacion)(tau))
}


########################## Cálculo de la Curva Par ###########################


#------------------------------ Tasa Fija ------------------------------------


TIR.TF <- function(fila){
    if(fila[,"COD_INS"] %in% c("tudes","TUDES")){
      # Creamos el Tau del ponderador:
      if(fila[,"TIP_PER"]==0){
        tabla <- fila %>%  
          mutate(tau = Tau(FEC_DAT, FEC_VEN),
                                  Fecha.Pago = FEC_VEN,Pago=1+(1+Inflacion_Interp(tau))*TAS_FAC/100)
      }else{
        Taus <- seq.Date(from = as.Date(fila[,"FEC_VEN"]),
                         to = as.Date(fila[,"FEC_DAT"]),
                         by = paste(as.character(-12/fila[,"TIP_PER"]),"months",sep=" "))
        tabla <- fila[rep(seq_len(nrow(fila)), each = length(Taus)), ] %>%
          mutate(Fecha.Pago = Taus) %>% 
          mutate(tau = Tau(FEC_DAT, Fecha.Pago)) %>% 
          mutate(Pago=ifelse(Fecha.Pago==FEC_VEN,1+(1+Inflacion_Interp(tau))*TAS_FAC/100,(1+Inflacion_Interp(tau))*TAS_FAC/100))
      }
    } else if(fila[,"TIP_PER"]==0){
      tabla <- fila %>%  mutate(tau = Tau(FEC_DAT, FEC_VEN),
                                Fecha.Pago = FEC_VEN,Pago=1+TAS_FAC/100)
    }else{
      Taus <- seq.Date(from = as.Date(fila[,"FEC_VEN"]),
                       to = as.Date(fila[,"FEC_DAT"]),
                       by = paste(as.character(-12/fila[,"TIP_PER"]),"months",sep=" "))
      tabla <- fila[rep(seq_len(nrow(fila)), each = length(Taus)), ] %>% mutate(Fecha.Pago = Taus) %>% 
        mutate(tau = Tau(FEC_DAT, Fecha.Pago)) %>%
        mutate(Pago=ifelse(Fecha.Pago==FEC_VEN,1+TAS_FAC/100,TAS_FAC/100))
    }
    
    TIR <- function(tasa){
      tabla <- tabla %>% mutate(desc=(1+tasa)^-tau) %>%
        mutate(Pago_desc=Pago*desc)
      1-sum(tabla$Pago_desc)
    }
    
    Opt <- nleqslv(0,TIR)
    return(cbind(fila,TIR=Opt$x))
    
}

#
BONOS.TF.RESUMEN <- BONOS.TF %>%
  distinct(COD_ISIN,TIP_PER,FEC_DAT,FEC_VEN,TAS_FAC,COD_INS)

#
BONOS.TF2 <- lapply(split(BONOS.TF.RESUMEN,seq(nrow(BONOS.TF.RESUMEN))),TIR.TF)

#
BONOS.TF2 <- data.frame(matrix(unlist(BONOS.TF2), nrow=length(BONOS.TF2), byrow=T))
colnames(BONOS.TF2) <- c(colnames(BONOS.TF.RESUMEN),'TIR')
BONOS.TF2 <- BONOS.TF2 %>% 
  mutate(TAS_FAC=as.numeric(TAS_FAC),TIP_PER=as.numeric(TIP_PER),TIR=as.numeric(TIR))

#
BONOS.TF <- left_join(BONOS.TF,BONOS.TF2, by = c("FEC_DAT", "COD_INS", "FEC_VEN", "COD_ISIN", "TAS_FAC", "TIP_PER"))


#----------------------------- Tasa Variable ---------------------------------


# Se agrega la curva de espectativas de tasa de interés:
curva.tasas <- list(Tasa = Curva.Espectativas)
curva.tasas <- rep(curva.tasas, times = length(unique(BONOS.TV$COD_INS)))
names(curva.tasas) <- unique(BONOS.TV$COD_INS)

# 
Cupones.variables <- function(tau,nemotec){
  tasa <- approxfun(curva.tasas[[nemotec]]$Tau,curva.tasas[[nemotec]]$Tasa)(tau)
  return(tasa)
}

# 
V_Cupones.variables <- Vectorize(Cupones.variables)

#
TIR.TV <- function(fila){
  # Creamos el Tau del ponderador:
  if(fila[,"TIP_PER"]==0){
    tabla <- fila %>%  mutate(tau = Tau(FEC_DAT, FEC_VEN)) %>%
      mutate(Fecha.Pago = FEC_VEN,Pago=1+(V_Cupones.variables(tau,COD_INS)+MAR_FIJ)/100)
  }else{
    Taus <- seq.Date(from = as.Date(fila[,"FEC_VEN"]),
                     to = as.Date(fila[,"FEC_DAT"]),
                     by = paste(as.character(-12/fila[,"TIP_PER"]),"months",sep=" "))
    tabla <- fila[rep(seq_len(nrow(fila)), each = length(Taus)), ] %>% mutate(Fecha.Pago = Taus) %>% 
      mutate(tau = Tau(FEC_DAT, Fecha.Pago)) %>% mutate(Pago=ifelse(Fecha.Pago==FEC_VEN,1+(V_Cupones.variables(tau,COD_INS)+MAR_FIJ)/100,(V_Cupones.variables(tau,COD_INS)+MAR_FIJ)/100))
  }
  
  TIR<-function(tasa){
    tabla<-tabla %>% 
      mutate(desc=(1+tasa)^-tau) %>% mutate(Pago_desc=Pago*desc)
    1-sum(tabla$Pago_desc)
  }
  
  Opt<-nleqslv(0,TIR)
  return(cbind(fila,TIR=Opt$x))
}

#
BONOS.TV.RESUMEN <- BONOS.TV %>% 
  distinct(COD_ISIN,FEC_DAT,FEC_VEN,COD_INS,TIP_PER,MAR_FIJ)

#
BONOS.TV2 <- lapply(split(BONOS.TV.RESUMEN, seq(nrow(BONOS.TV.RESUMEN))),TIR.TV)

#
BONOS.TV2 <- data.frame(matrix(unlist(BONOS.TV2), nrow = length(BONOS.TV2), byrow = T))
colnames(BONOS.TV2) <- c(colnames(BONOS.TV.RESUMEN), 'TIR')

#
BONOS.TV2 <- BONOS.TV2 %>% 
  mutate(TIP_PER = as.numeric(TIP_PER), MAR_FIJ = as.numeric(MAR_FIJ), TIR = as.numeric(TIR))

#
BONOS.TV <- left_join(BONOS.TV,BONOS.TV2, by = c("FEC_DAT", "COD_INS", "MAR_FIJ", "FEC_VEN", "COD_ISIN", "TIP_PER"))


##################### Estimación del Riesgo de Crédito #######################


#------------------------------ Tasa Fija ------------------------------------

# Función que debe ser minimizada para estimar el parámetro mu:
Optimizacion.F <- function(fila){
  FuncionObjetivo <- function(mu){  ## Agrupar por cod isin
    return(abs(PRECIO.TEORICO.TF(fila,mu)-fila[,"PRECIO.OBS"]))
    }
  if(fila[,"COD_MON"]==1 & fila[,"COD_EMI"] %in% c('BCCR','G')){
    Optimizacion <- list(x=0,fvec=0)
    }else{
      Optimizacion<-nleqslv(0,FuncionObjetivo)
      }  #ARREGLAR
  return(cbind(fila,Parametro=Optimizacion$x,Error=Optimizacion$fvec))
}

#
PRECIOS.OBS <- BONOS.TF %>% group_by(COD_ISIN) %>%
  mutate(PONDERADOR=VAL_FAC/sum(VAL_FAC)) %>% 
  mutate(PRECIO.POND=PONDERADOR*PRECIO) %>% 
  summarise(PRECIO.OBS=sum(PRECIO.POND), .groups = "keep")
BONOS.TF.RESUMEN<- BONOS.TF %>%
  distinct(COD_ISIN,TIP_PER,FEC_DAT,FEC_VEN,TAS_FAC,COD_MON,COD_EMI,COD_INS)
BONOS.TF.RESUMEN <- left_join(BONOS.TF.RESUMEN,PRECIOS.OBS, by = "COD_ISIN")

#
Optimizacion.TF <- lapply(split(BONOS.TF.RESUMEN, seq(nrow(BONOS.TF.RESUMEN))), Optimizacion.F)

#
Optimizacion.TF <- data.frame(matrix(unlist(Optimizacion.TF), 
                                     nrow = length(Optimizacion.TF), byrow = T))
colnames(Optimizacion.TF) <- c(colnames(BONOS.TF.RESUMEN),"Parametro",
                               "Error")
Optimizacion.TF$Parametro <- as.numeric(Optimizacion.TF$Parametro)

#
BONOS.TF <- left_join(BONOS.TF,Optimizacion.TF[,c("COD_ISIN","Parametro")],
                      by = "COD_ISIN")


#----------------------------- Tasa Variable ---------------------------------


# Función para calcular el precio teórico de cada bono considerando su riesgo crediticio:
PRECIO.TEORICO.TV <- function(fila,mu){
  
  # Creamos el Tau del ponderador:
  if(fila[,"TIP_PER"]==0){
    tabla <- fila %>%  mutate(tau = Tau(FEC_DAT, 
                                        FEC_VEN)) %>%
                             mutate(Fecha.Pago = FEC_VEN,
                                    Pago=1+(V_Cupones.variables(tau,COD_INS)+MAR_FIJ)/100)
  }else{
    Taus <- seq.Date(from = as.Date(fila[,"FEC_VEN"]),
                     to = as.Date(fila[,"FEC_DAT"]),
                     by = paste(as.character(-12/fila[,"TIP_PER"]),"months",sep=" "))
    tabla <- fila[rep(seq_len(nrow(fila)), each = length(Taus)), ] %>% 
      mutate(Fecha.Pago = Taus) %>% 
      mutate(tau = Tau(FEC_DAT, Fecha.Pago)) %>% 
      mutate(Pago=ifelse(Fecha.Pago==FEC_VEN,1+(V_Cupones.variables(tau,COD_INS)+
                                                  MAR_FIJ)/100,(V_Cupones.variables(tau,COD_INS)+
                                                                  MAR_FIJ)/100))
  }
  
  tabla<-tabla %>% mutate(exponencial=exp(-mu*tau),PrecioCC=V_Precio(tau,COD_MON)) %>% 
    mutate(PrecioTeorico = Pago*PrecioCC*exponencial) 
  Precio.Teorico=sum(tabla$PrecioTeorico)
  return(Precio.Teorico)
}

# Función que debe ser minimizada para estimar el parámetro mu:
Optimizacion.V <- function(fila){
  
  FuncionObjetivo<- function(mu){
    
    return(abs(PRECIO.TEORICO.TV(fila,mu)-fila[,"PRECIO.OBS"]))
  }
  
  if(fila[,"COD_MON"]==1 & fila[,"COD_EMI"] %in% c('BCCR','G')){
    Optimizacion <- list(x=0,
                         fvec=0)
  }else{
  Optimizacion <- nleqslv(0, FuncionObjetivo)}
  return(cbind(fila, Parametro = Optimizacion$x,
               Error = Optimizacion$fvec))
}

#
PRECIOS.OBS <- BONOS.TV %>% 
  group_by(COD_ISIN) %>% 
  mutate(PONDERADOR = VAL_FAC/sum(VAL_FAC)) %>% 
  mutate(PRECIO.POND = PONDERADOR*PRECIO) %>% 
  summarise(PRECIO.OBS = sum(PRECIO.POND), 
            .groups = "keep")

#
BONOS.TV.RESUMEN<- BONOS.TV %>% 
  distinct(COD_ISIN, TIP_PER, FEC_DAT, FEC_VEN, COD_MON, MAR_FIJ, COD_INS, COD_EMI)
BONOS.TV.RESUMEN <- left_join(BONOS.TV.RESUMEN, PRECIOS.OBS, by = "COD_ISIN")

#
Optimizacion.TV <-lapply(split(BONOS.TV.RESUMEN, seq(nrow(BONOS.TV.RESUMEN))), Optimizacion.V)

#
Optimizacion.TV <- data.frame(matrix(unlist(Optimizacion.TV),
                                     nrow = length(Optimizacion.TV), byrow = T))
colnames(Optimizacion.TV) <- c(colnames(BONOS.TV.RESUMEN),
                               "Parametro", "Error")

#
BONOS.TV <- left_join(BONOS.TV, Optimizacion.TV[, c("COD_ISIN", "Parametro")],
                      by = "COD_ISIN")
BONOS.TV <- BONOS.TV %>% mutate(Parametro = as.numeric(Parametro))


######################## Cálculo del Precio Teórico #########################


#------------------------------ Tasa Fija ------------------------------------

# 
PRECIO.TEORICO.TF2 <- function(fila){
  
  # Creamos el Tau del ponderador:
  if(fila[,"TIP_PER"]==0){
    tabla <- fila %>%  mutate(tau = Tau(FEC_DAT, FEC_VEN),
                              Fecha.Pago = FEC_VEN, Pago=1+TAS_FAC/100)
  }else{
    Taus <- seq.Date(from = as.Date(fila[,"FEC_VEN"]),
                     to = as.Date(fila[,"FEC_DAT"]),
                     by = paste(as.character(-12/fila[,"TIP_PER"]), "months",sep=" "))
    tabla <- fila[rep(seq_len(nrow(fila)), each = length(Taus)), ] %>%
      mutate(Fecha.Pago = Taus) %>% 
      mutate(tau = Tau(FEC_DAT, Fecha.Pago)) %>% 
      filter(tau >= Periodo) %>%
      mutate(Pago = ifelse(Fecha.Pago == FEC_VEN,
                           1+TAS_FAC/100,TAS_FAC/100))
  }
  
    
  
  tabla <- tabla %>% mutate(exponencial = exp(-Parametro*tau), 
                            PrecioCC = V_Precio(tau,COD_MON)) %>% 
    mutate(PrecioTeorico = Pago*PrecioCC*exponencial) 
  Precio.Teorico = sum(tabla$PrecioTeorico)
  return(cbind(fila, Precio.Teorico))
}

#
BONOS.TF.RESUMEN <- BONOS.TF %>%
  distinct(COD_ISIN,TIP_PER,FEC_DAT,FEC_VEN,TAS_FAC,COD_MON,COD_EMI,Parametro,ES_REDE)

#
PRECIO.TF <- lapply(split(BONOS.TF.RESUMEN, seq(nrow(BONOS.TF.RESUMEN))),
                    PRECIO.TEORICO.TF2)

#
PRECIO.TF <- data.frame(matrix(unlist(PRECIO.TF), nrow=length(PRECIO.TF), 
                               byrow = T))
colnames(PRECIO.TF) <- c(colnames(BONOS.TF.RESUMEN), "PRECIO_TEORICO_0")

#
BONOS.TF <- left_join(BONOS.TF,PRECIO.TF[,c('COD_ISIN','PRECIO_TEORICO_0')],
                      by = "COD_ISIN")
BONOS.TF <- BONOS.TF %>% mutate(FACTOR_TC = ifelse(COD_MON == 1, 1, TC)) %>% 
  mutate(PRECIO_TEORICO_0 = as.numeric(PRECIO_TEORICO_0)) %>%
 mutate(PRECIO_TEORICO_0 = PRECIO_TEORICO_0*FACTOR_TC)


#----------------------------- Tasa Variable ---------------------------------


#
PRECIO.TEORICO.TV2 <- function(fila){
  
  # Creamos el Tau del ponderador:
  if(fila[,"TIP_PER"] == 0){
    tabla <- fila %>%  mutate(tau = Tau(FEC_DAT, FEC_VEN)) %>%
      mutate(Fecha.Pago = FEC_VEN,Pago = 1+(V_Cupones.variables(tau, COD_INS)+MAR_FIJ)/100)
  }else{
    Taus <- seq.Date(from = as.Date(fila[,"FEC_VEN"]),
                     to = as.Date(fila[,"FEC_DAT"]),
                     by = paste(as.character(-12/fila[,"TIP_PER"]),"months",sep=" "))
    tabla <- fila[rep(seq_len(nrow(fila)), each = length(Taus)), ] %>% mutate(Fecha.Pago = Taus) %>% 
      mutate(tau = Tau(FEC_DAT, Fecha.Pago)) %>% filter(tau >= Periodo) %>%
   mutate(Pago = ifelse(Fecha.Pago == FEC_VEN, 1+
                          (V_Cupones.variables(tau,COD_INS)+MAR_FIJ)/100,
                        (V_Cupones.variables(tau,COD_INS)+MAR_FIJ)/100))
  }
  
  tabla <- tabla %>% 
    mutate(exponencial = exp(-Parametro*tau), PrecioCC = V_Precio(tau,COD_MON)) %>% 
    mutate(PrecioTeorico = Pago*PrecioCC*exponencial) 
  Precio.Teorico = sum(tabla$PrecioTeorico)
  return(cbind(fila,Precio.Teorico))
}

#
BONOS.TV.RESUMEN <- BONOS.TV %>%
  distinct(COD_ISIN,TIP_PER,FEC_DAT,FEC_VEN,TAS_FAC,COD_MON,
           COD_EMI,COD_INS,MAR_FIJ, Parametro,ES_REDE) %>%
  mutate(Parametro = as.numeric(Parametro))

#
PRECIO.TV <-lapply(split(BONOS.TV.RESUMEN,seq(nrow(BONOS.TV.RESUMEN))),
                   PRECIO.TEORICO.TV2)

#
PRECIO.TV <- data.frame(matrix(unlist(PRECIO.TV), nrow=length(PRECIO.TV), 
                               byrow=T))
colnames(PRECIO.TV)<-c(colnames(BONOS.TV.RESUMEN),"PRECIO_TEORICO_0") 

#
BONOS.TV <- left_join(BONOS.TV,PRECIO.TV[,c('COD_ISIN','PRECIO_TEORICO_0')], 
                      by = "COD_ISIN")
BONOS.TV <- BONOS.TV %>% mutate(FACTOR_TC=ifelse(COD_MON==1,1,TC)) %>%
  mutate(PRECIO_TEORICO_0=as.numeric(PRECIO_TEORICO_0)) %>%
  mutate(PRECIO_TEORICO_0=PRECIO_TEORICO_0*FACTOR_TC)


############################# Simulación Ho-Lee ##############################


#------------------------ Carga de Datos Ho-Lee CRC --------------------------


# Historial de Tasas TRI de Costa Rica: 
TRI_colones <- read_excel("TRI colones.xlsx",col_types = c("date", "numeric"))
TRI_colones <- TRI_colones[seq(from = 1, to = nrow(TRI_colones), by = 7),]
TRI_colones<-TRI_colones %>% mutate(Delta = log((1+`1 semana`/100/52))*52/12)


#------------------------ Parámetros Ho-Lee CRC ------------------------------


# Se calcula la varianza mensual:
varianza.mensual <- 4*var(TRI_colones$Delta)

# Generamos las proporciones de subida y bajada iniciales:
u2.col <- 1+sqrt(varianza.mensual)
d2.col <- 1-sqrt(varianza.mensual)

# Fijamos el parámetro fijo k del modelo:
k.col <- d2.col/u2.col


#------------------------- Funciones Ho-Lee CRC ------------------------------


# Función que genera el precio de la curva a un tiempo (Tao) dado:
Precio_col <- function(tao){
  if(tao == 0){
    precio = 1
  }else{
    if(Svensson == 1){
      Delta = Par.SA[1] * tao +
        Par.SA[2] * ((1-exp(-tao/Par.SA[5]))*Par.SA[5]) + 
        Par.SA[3] * (1-(tao/Par.SA[5]+1)*exp(-tao/Par.SA[5]))*Par.SA[5]^2 +
        ((Par.SA[4]*Par.SA[6]*Par.SA[5])/(Par.SA[5]-Par.SA[6])) * 
        (((1-(tao/Par.SA[5]+1)*exp(-tao/Par.SA[5]))*Par.SA[5]^2) - 
           ((1-(tao/Par.SA[6]+1)*exp(-tao/Par.SA[6]))*Par.SA[6]^2))
      
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

# Función del parámetro de bajada:
d.t = function(t, p, k.col){ 
  (k.col^(t-1))/((1 - p)*(k.col^(t-1)) + p)
} 

# Función para encontrar la tasa corta dado un tiempo (Tao):
Tasa.Corta.t = function(tao, cant_sub, p){
  
  # Se obtiene el precio de acuerdo a las curvas Nelson-Siegel-Svensson
  P_0_t = Precio_col(tao)
  P_0_T = Precio_col(tao + 1)
  
  # Se obtiene la tasa corta mediante la fórmula.
  r_t = log(P_0_t/P_0_T) - log(d.t(tao + 1, p, k.col)) + cant_sub*log(k.col)
  
  return(r_t)  
}

# Función que realiza la simulación de una trayectoria aleatoria dado un periodo (tiempo):
Arbol.HL.desc <- function(tiempo){   
  
  # Crea la probabilidad:
  p <- (1 - d2.col)/(u2.col - d2.col)
  
  # Forma la trayectoria aleatoria para cada periodo:
  trayectoria = rbernoulli(tiempo, p) 
  
  # Obtiene la cantidad de subidas acumuladas:
  vect_cant_sub = cumsum(trayectoria)
  
  # Aplica la función de la tasa corta para cada instante;
  vect_r_t = Vectorize(Tasa.Corta.t)(1:tiempo, vect_cant_sub, p)
  
  # Obtiene el vector de los descuentos D(0,t):
  vect_D_0_T = exp(-cumsum(vect_r_t))
  
  return(vect_D_0_T)
}

# 
Matriz.trayectorias.col = matrix(nrow = cant.simu, ncol = tiempo)
  
for (i in 1:cant.simu){
  Matriz.trayectorias.col[i,] = Arbol.HL.desc(tiempo)
}
  
  
#------------------------ Carga de Datos Ho-Lee USD --------------------------


# Historial de Overnight de Estados Unidos: 
Overnight <- read.csv("overnightrate.csv",sep=',',dec='.',header = F)
Overnight1 <- read.csv("overnightrate (1).csv",sep=',',dec='.',header = F)
Overnight2 <- read.csv("overnightrate (2).csv",sep=',',dec='.',header = F)
Overnight3 <- read.csv("overnightrate (3).csv",sep=',',dec='.',header = F)
Overnight4 <- read.csv("overnightrate (4).csv",sep=',',dec='.',header = F)
Overnight5 <- read.csv("overnightrate (5).csv",sep=',',dec='.',header = F)
Overnight6 <- read.csv("overnightrate (6).csv",sep=',',dec='.',header = F)
Overnight7 <- read.csv("overnightrate (7).csv",sep=',',dec='.',header = F)
Overnight <- rbind(Overnight,Overnight1,Overnight2,
                   Overnight3,Overnight4,Overnight5,Overnight6,Overnight7)
rm(Overnight1,Overnight2,Overnight3,Overnight4,Overnight5,Overnight6,Overnight7)
Overnight <- Overnight[,-3]
colnames(Overnight)=c('Fecha','Tasa')
Overnight <- Overnight %>% mutate(Tasa=Tasa/100) %>% 
  mutate(TasaS=((1+Tasa/360)^(360/2)-1)*2) %>%
  mutate(Fecha=as.Date(Fecha, format = '%m/%d/%Y'))

# Se calcula el delta para generar las proporciones u y d:
Overnight <- Overnight %>% 
  mutate(Delta=log((1+Tasa/100/360))*360/12)
  
  
#------------------------ Parámetros Ho-Lee USD ------------------------------
  

# Se calcula la varianza mensual:
varianza.mensual.dol<-30*var(Overnight$Delta)
  
# Generamos las proporciones de subida y bajada iniciales:
u2.dol<-1+sqrt(varianza.mensual.dol) 
d2.dol<-1-sqrt(varianza.mensual.dol)
  
# Fijamos el parámetro fijo k del modelo:
k.dol <- d2.dol/u2.dol


#------------------------- Funciones Ho-Lee USD ------------------------------
  

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
d.t = function(t, p, k.dol){ 
  (k.dol^(t-1))/((1 - p)*(k.dol^(t-1)) + p)
 } 
  
# Función para encontrar la tasa corta dado un tiempo (Tao):
Tasa.Corta.t = function(tao, cant_sub, p){
  
  # Se obtiene el precio de acuerdo al vector de precios:
  P_0_t = vec.precios[tao,2]
  P_0_T = vec.precios[tao+1,2]
    
  # Se obtiene la tasa corta mediante la fórmula.
  r_t = log(P_0_t/P_0_T) - log(d.t(tao + 1, p, k.dol)) + cant_sub*log(k.dol)
    
  return(r_t)  
}
  
# Función que realiza la simulación de una trayectoria aleatoria dado un periodo (tiempo):
Arbol.HL.desc <- function(tiempo){   
  # Crea la probabilidad:
  p <- (1 - d2.dol)/(u2.dol - d2.dol)
    
  # Forma la trayectoria aleatoria para cada periodo:
  trayectoria = rbernoulli(tiempo, p) 
    
  # Obtiene la cantidad de subidas acumuladas:
  vect_cant_sub = cumsum(trayectoria)
    
  # Aplica la función de la tasa corta para cada instante;
  vect_r_t = Vectorize(Tasa.Corta.t)(1:tiempo, vect_cant_sub, p)
    
  # Obtiene el vector de los descuentos D(0,t):
  vect_D_0_T = exp(-cumsum(vect_r_t))
    
  return(vect_D_0_T)
}
  
 
# Se simulan suficientes trayectorias;
Matriz.trayectorias.dol = matrix(nrow = cant.simu, ncol = tiempo)
    
for (i in 1:cant.simu){
  Matriz.trayectorias.dol[i,] = Arbol.HL.desc(tiempo)
}
  
   
########################## Valoración de Bonos ###############################
    
#   
FactDesc <- function(fila){
  if (fila[,'COD_MON'] == 1){
    Factor<-Matriz.trayectorias.col[,floor(fila[,'tau'])]/
      Matriz.trayectorias.col[,Periodo]
    }else{
      Factor<-Matriz.trayectorias.dol[,floor(fila[,'tau'])]*
        TC/Matriz.trayectorias.col[,Periodo]
      }
  return(Factor)
}

#
BONOS.TF.RESUMEN <- BONOS.TF.RESUMEN %>%
  mutate(Probabilidad=exp(-Parametro*Periodo))
 
#     
Tau.total.TF <- function(fila){
  if(fila[,"TIP_PER"]==0){
     tabla <- fila %>%  mutate(tau = Tau(FEC_DAT, FEC_VEN),
                              Fecha.Pago = FEC_VEN,Pago=1+TAS_FAC/100)
     }else{
       Taus <- seq.Date(from = as.Date(fila[,"FEC_VEN"]),
                        to = as.Date(fila[,"FEC_DAT"]),
                        by = paste(as.character(-12/fila[,"TIP_PER"]),
                                   "months",sep=" "))
       tabla <- fila[rep(seq_len(nrow(fila)), each = length(Taus)), ] %>%
         mutate(Fecha.Pago = Taus) %>% 
          mutate(tau = Tau(FEC_DAT, Fecha.Pago)) %>% 
         filter(tau>=Periodo) %>%
          mutate(Pago=ifelse(Fecha.Pago==FEC_VEN,1+TAS_FAC/100,TAS_FAC/100))
       }
  
  #    
  Precio.Teorico <- lapply(split(tabla,seq(nrow(tabla))),FactDesc)
  Precio.Teorico <- data.frame(matrix(unlist(Precio.Teorico),
                                      nrow=length(Precio.Teorico), byrow=T))
  #
  Precio.Teorico <- Precio.Teorico*tabla$Pago
  Precio.Teorico<- apply(Precio.Teorico,2,sum)
   
  return(Precio.Teorico)
}
    
#
BONOS.TF.PRECIOS <-lapply(split(BONOS.TF.RESUMEN,seq(nrow(BONOS.TF.RESUMEN))),
                          Tau.total.TF)

#
BONOS.TF.PRECIOS <- data.frame(matrix(unlist(BONOS.TF.PRECIOS),
                                      nrow=length(BONOS.TF.PRECIOS), 
                                      byrow=T))

#   
DEFAULT <- function(p){
  rbernoulli(n = cant.simu,p = p)
}

#   
V_DEFAULT <- Vectorize(DEFAULT)

#
Bernoullis.TF1 <- t(V_DEFAULT(BONOS.TF.RESUMEN$Probabilidad))

#   
BONOS.TF.PRECIOS <- BONOS.TF.PRECIOS*Bernoullis
BONOS.TF.PRECIOS <- cbind(BONOS.TF.RESUMEN$COD_ISIN,BONOS.TF.PRECIOS)
colnames(BONOS.TF.PRECIOS)[1] <- 'COD_ISIN'

#
BONOS.TF.RESULTADOS <- BONOS.TF[,c('COD_ISIN',
                                   'COD_ENT',
                                   'COD_MOD_INV',
                                   'VAL_FAC',
                                   'PRECIO_TEORICO_0')] %>% 
  group_by(COD_ISIN, COD_ENT, PRECIO_TEORICO_0) %>% 
  mutate(VAL_FAC = sum(VAL_FAC)) %>% ungroup() %>% 
  unique() %>% 
  right_join(BONOS.TF.PRECIOS, by = "COD_ISIN")


#--------------- Ajuste para Redención Anticipada Tasa Fija ------------------


#
RESUMEN.TF <- BONOS.TF %>% distinct(FEC_DAT,FEC_VEN,COD_ISIN,TAS_FAC,
                                    TIP_PER,COD_MON,ES_REDE,TIR,
                                    Parametro,COD_INS)
RESUMEN.TF <- RESUMEN.TF %>% filter(ES_REDE=='S')

#
Redencion_TF <- function(fila){

  #
  Precio.gatillo <- function(fila,Contador){
      if(fila[,"COD_INS"] %in% c("tudes","TUDES")){
      # Creamos el Tau del ponderador:
      if(fila[,"TIP_PER"]==0){
        tabla <- fila %>%  mutate(tau = Tau(FEC_DAT, FEC_VEN),
                                  Fecha.Pago = FEC_VEN,Pago=1+
                                    (1+Inflacion_Interp(tau))*TAS_FAC/100)
      }else{
        Taus <- seq.Date(from = as.Date(fila[,"FEC_VEN"]),
                         to = as.Date(fila[,"FEC_DAT"]),
                         by = paste(as.character(-12/fila[,"TIP_PER"]),"months",sep=" "))
        tabla <- fila[rep(seq_len(nrow(fila)), each = length(Taus)), ] %>%
          mutate(Fecha.Pago = Taus) %>% 
          mutate(tau = Tau(FEC_DAT, Fecha.Pago)) %>%
          mutate(Pago=ifelse(Fecha.Pago==FEC_VEN,1+
                               (1+Inflacion_Interp(tau))*
                               TAS_FAC/100,(1+Inflacion_Interp(tau))*
                               TAS_FAC/100))
      }
    } else if(fila[,"TIP_PER"]==0){
      tabla <- fila %>%  mutate(tau = Tau(FEC_DAT, FEC_VEN),
                                Fecha.Pago = FEC_VEN,Pago=1+TAS_FAC/100)
    }else{
      Taus <- seq.Date(from = as.Date(fila[,"FEC_VEN"]),
                       to = as.Date(fila[,"FEC_DAT"]),
                       by = paste(as.character(-12/fila[,"TIP_PER"]),
                                  "months",sep=" "))
      
      tabla <- fila[rep(seq_len(nrow(fila)), each = length(Taus)), ] %>%
        mutate(Fecha.Pago = Taus) %>% 
        mutate(tau = Tau(FEC_DAT, Fecha.Pago)) %>% 
        mutate(Pago=ifelse(Fecha.Pago==FEC_VEN,1+TAS_FAC/100,TAS_FAC/100)) 
      
    }
    tabla <- tabla %>% filter(Fecha.Pago>=Contador) %>%
      mutate(desc = (1+as.numeric(fila[,'TIR']))^-(tau-Contador)) %>%
      mutate(Pago_desc=Pago*desc)
    
    P_gatillo<-sum(tabla$Pago_des)  
    return(P_gatillo)
    }
  
  #
  V_Precio.gatillo <- Vectorize(Precio.gatillo,"Contador")
  P_gatillo <- V_Precio.gatillo(fila,1:(Periodo-1))
  
  #
  Tau.total.TF_reden <- function(fila,Contador){
    if(fila[,"TIP_PER"]==0){
      tabla <- fila %>%  mutate(tau = Tau(FEC_DAT, FEC_VEN),
                                Fecha.Pago = FEC_VEN,Pago=1+TAS_FAC/100)
    }else{
      Taus <- seq.Date(from = as.Date(fila[,"FEC_VEN"]),
                       to = as.Date(fila[,"FEC_DAT"]),
                       by = paste(as.character(-12/fila[,"TIP_PER"]),
                                  "months",sep=" "))
      tabla <- fila[rep(seq_len(nrow(fila)), each = length(Taus)), ] %>%
        mutate(Fecha.Pago = Taus) %>% 
        mutate(tau = Tau(FEC_DAT, Fecha.Pago)) %>% filter(tau>=Contador) %>%
        mutate(Pago=ifelse(Fecha.Pago==FEC_VEN,1+TAS_FAC/100,TAS_FAC/100))
    }
    
    #
    FactDesc_reden <- function(fila){
      if (fila[,'COD_MON'] == 1){
        Factor<-Matriz.trayectorias.col[,floor(fila[,'tau'])]/
          Matriz.trayectorias.col[,Contador] 
      }else{
        Factor<-Matriz.trayectorias.dol[,floor(fila[,'tau'])]/
          Matriz.trayectorias.dol[,Contador] 
      }
      return(Factor)
    }
    
    
    Precio.Teorico <- lapply(split(tabla,seq(nrow(tabla))),FactDesc_reden)
    Precio.Teorico <- data.frame(matrix(unlist(Precio.Teorico), 
                                        nrow=length(Precio.Teorico), byrow=T))
    Precio.Teorico <- Precio.Teorico*tabla$Pago
    Precio.Teorico<- apply(Precio.Teorico,2,sum)
    
    return(Precio.Teorico)
  }
  
  
 
  data.frame<-as.data.frame(rep(NA,cant.simu))
  colnames(data.frame)<-'Periodo_Venta'
  
  
  Periodo_Venta1<-NA
  k=1
  while(k<=length(P_gatillo) & sum(is.na(Periodo_Venta1))>0){
    data.frame<-data.frame %>% mutate(P1 = Tau.total.TF_reden(fila,k)) %>%
      mutate(P1<P_gatillo[k])
    colnames(data.frame)<-c(colnames(data.frame)[-ncol(data.frame)],
                            paste0('VENTA',k))
    Periodo_Venta1<-ifelse(is.na(data.frame[,'Periodo_Venta']),
                           ifelse(data.frame[,ncol(data.frame)]==TRUE,k,NA),
                           data.frame[,'Periodo_Venta'])
    data.frame <- data.frame %>% 
      mutate(Periodo_Venta=Periodo_Venta1)
    k=k+1
  }
  
  if(sum(is.na(data.frame$Periodo_Venta))<nrow(data.frame)){
    Precio1<-function(k,Contador){
      if(is.na(Contador)){
        PRECIO1<-NA
      }else{
        FactDesc_reden <- function(fila){
          if (fila[,'COD_MON'] == 1){
            Factor<-Matriz.trayectorias.col[k,floor(fila[,'tau'])]/
              Matriz.trayectorias.col[k,Periodo]
          }else{
            Factor<-Matriz.trayectorias.dol[k,floor(fila[,'tau'])]*
              TC/Matriz.trayectorias.col[k,Periodo]       }
          return(Factor)
        }
        
        Tau.total.TF_reden <- function(fila){
          if(fila[,"TIP_PER"]==0){
            tabla <- fila %>%  mutate(tau = Tau(FEC_DAT, FEC_VEN),
                                      Fecha.Pago = FEC_VEN,Pago=1+TAS_FAC/100)
          }else{
            Taus <- seq.Date(from = as.Date(fila[,"FEC_VEN"]),
                             to = as.Date(fila[,"FEC_DAT"]),
                             by = paste(as.character(-12/fila[,"TIP_PER"]),
                                        "months",sep=" "))
            tabla <- fila[rep(seq_len(nrow(fila)), each = length(Taus)), ] %>%
              mutate(Fecha.Pago = Taus) %>% 
              mutate(tau = Tau(FEC_DAT, Fecha.Pago)) %>% 
              filter(tau>=Contador) %>%
              mutate(Pago=ifelse(Fecha.Pago==FEC_VEN,
                                 1+TAS_FAC/100,
                                 TAS_FAC/100))
          }
          
          Precio.Teorico <- lapply(split(tabla,seq(nrow(tabla))),
                                   FactDesc_reden)
          Precio.Teorico <- data.frame(matrix(unlist(Precio.Teorico),
                                              nrow=length(Precio.Teorico),
                                              byrow=T))
          Precio.Teorico <- Precio.Teorico*tabla$Pago
          Precio.Teorico<- sum(Precio.Teorico)
          
          return(Precio.Teorico)
        }
        
         PRECIO1<-Tau.total.TF_reden(fila)*
           rbernoulli(1,exp(Contador*fila[,'Parametro']))
        
        
      }
      
      return(PRECIO1)
    }
    
    #
    V_Precio1 <- Vectorize(Precio1,vectorize.args = c('k','Contador'))
    
    #
    data.frame <- data.frame %>% mutate(k=1:nrow(data.frame)) %>% 
      mutate(PRECIO1=V_Precio1(k,Periodo_Venta),
             PrecioInicial=t(as.matrix(BONOS.TF.RESULTADOS[which(BONOS.TF.RESULTADOS$COD_ISIN==
                                                                   fila[,'COD_ISIN'])[1],
                                                           8:ncol(BONOS.TF.RESULTADOS)]))) %>%
      mutate(PRECIOFINAL=ifelse(is.na(PRECIO1),PrecioInicial,PRECIO1))
    
    #
    BONOS.TF.RESULTADOS[which(BONOS.TF.RESULTADOS$COD_ISIN==fila[,'COD_ISIN']),
                        8:ncol(BONOS.TF.RESULTADOS)]<-matrix(rep(data.frame$PRECIOFINAL,
                                                          each=length(which(BONOS.TF.RESULTADOS$COD_ISIN==
                                                                              fila[,'COD_ISIN']))),byrow = F,nrow = length(which(BONOS.TF.RESULTADOS$COD_ISIN==
                                                                                                                                   fila[,'COD_ISIN'])))   
  }
  
}

#
for(j in 1:nrow(RESUMEN.TF)){
  Redencion_TF(RESUMEN.TF[j,])
}


#------------- Ajuste para Redención Anticipada Tasa Variable ----------------


#
BONOS.TV.RESUMEN <- BONOS.TV.RESUMEN %>% 
  mutate(Probabilidad=exp(-Parametro*Periodo))
 
#   
Tau.total.TV <- function(fila){
  if(fila[,"TIP_PER"]==0){
    tabla <- fila %>%  mutate(tau = Tau(FEC_DAT, FEC_VEN),
                              Fecha.Pago = FEC_VEN,Pago=1+
                                (V_Cupones.variables(tau,COD_INS)+MAR_FIJ)/100)
    }else{
      Taus <- seq.Date(from = as.Date(fila[,"FEC_VEN"]),
                       to = as.Date(fila[,"FEC_DAT"]),
                      by = paste(as.character(-12/fila[,"TIP_PER"]),"months",sep=" "))
      tabla <- fila[rep(seq_len(nrow(fila)), each = length(Taus)), ] %>% mutate(Fecha.Pago = Taus) %>%
        mutate(tau = Tau(FEC_DAT, Fecha.Pago)) %>% filter(tau>=Periodo) %>%
        mutate(Pago=ifelse(Fecha.Pago==FEC_VEN,1+
                             (V_Cupones.variables(tau,COD_INS)+
                                MAR_FIJ)/100,
                           (V_Cupones.variables(tau,COD_INS)+MAR_FIJ)/100))
    }
  
  
 
  Precio.Teorico <- lapply(split(tabla,seq(nrow(tabla))),FactDesc)
  Precio.Teorico <- data.frame(matrix(unlist(Precio.Teorico),
                                      nrow=length(Precio.Teorico), 
                                      byrow=T))
  Precio.Teorico <- Precio.Teorico*tabla$Pago
  Precio.Teorico<- apply(Precio.Teorico,2,sum)
  return(Precio.Teorico)
}
    
#
BONOS.TV.PRECIOS <-lapply(split(BONOS.TV.RESUMEN,seq(nrow(BONOS.TV.RESUMEN))),
                          Tau.total.TV)

#
BONOS.TV.PRECIOS <- data.frame(matrix(unlist(BONOS.TV.PRECIOS), 
                                      nrow=length(BONOS.TV.PRECIOS),
                                      byrow=T))
    
#    
Bernoullis.TV1 <- t(V_DEFAULT(BONOS.TV.RESUMEN$Probabilidad))

#  
BONOS.TV.PRECIOS <- BONOS.TV.PRECIOS*Bernoullis
BONOS.TV.PRECIOS <- cbind(BONOS.TV.RESUMEN$COD_ISIN,
                          BONOS.TV.PRECIOS)
colnames(BONOS.TV.PRECIOS)[1] <- 'COD_ISIN'
    
#
BONOS.TV.RESULTADOS <- BONOS.TV[,c('COD_ISIN',
                                   'COD_ENT',
                                   'COD_MOD_INV',
                                   'VAL_FAC',
                                   'PRECIO_TEORICO_0')] %>% 
  group_by(COD_ISIN, COD_ENT, PRECIO_TEORICO_0) %>% 
  mutate(VAL_FAC = sum(VAL_FAC)) %>% ungroup() %>% 
  unique() %>% 
  right_join(BONOS.TV.PRECIOS, by = "COD_ISIN")

#
RESUMEN.TV <- BONOS.TV %>% distinct(FEC_DAT,FEC_VEN,COD_ISIN,MAR_FIJ,
                                    TIP_PER,COD_MON,ES_REDE,TIR,
                                    Parametro,COD_INS)
RESUMEN.TV <- RESUMEN.TV %>% filter(ES_REDE=='S')

#
Redencion_TV <- function(fila){
  
    Precio.gatillo <- function(fila,Contador){
      
      if(fila[,"TIP_PER"]==0){
        tabla <- fila %>%  mutate(tau = Tau(FEC_DAT, FEC_VEN)) %>%
          mutate(Fecha.Pago = FEC_VEN,Pago=1+
                   (V_Cupones.variables(tau,COD_INS)+MAR_FIJ)/100)
      }else{
        Taus <- seq.Date(from = as.Date(fila[,"FEC_VEN"]),
                         to = as.Date(fila[,"FEC_DAT"]),
                         by = paste(as.character(-12/fila[,"TIP_PER"]),
                                    "months",sep=" "))
        tabla <- fila[rep(seq_len(nrow(fila)), each = length(Taus)), ] %>%
          mutate(Fecha.Pago = Taus) %>% 
          mutate(tau = Tau(FEC_DAT, Fecha.Pago)) %>%
          mutate(Pago=ifelse(Fecha.Pago==FEC_VEN,1+
                               (V_Cupones.variables(tau,COD_INS)+MAR_FIJ)/100,
                             (V_Cupones.variables(tau,COD_INS)+MAR_FIJ)/100))
      }
      
      tabla<- tabla %>% filter(Fecha.Pago>=Contador) %>%
        mutate(desc = (1+as.numeric(fila[,'TIR']))^-(tau-Contador)) %>%
        mutate(Pago_desc=Pago*desc)
      P_gatillo<-sum(tabla$Pago_des)  
      
      return(P_gatillo)
    }
    
    V_Precio.gatillo<-Vectorize(Precio.gatillo,"Contador")
    P_gatillo<-V_Precio.gatillo(fila,1:(Periodo-1))
    
    #
    Tau.total.TV_reden <- function(fila,Contador){
      if(fila[,"TIP_PER"]==0){
        tabla <- fila %>%  mutate(tau = Tau(FEC_DAT, FEC_VEN),
                                  Fecha.Pago = FEC_VEN,Pago=1+
                                    (V_Cupones.variables(tau,COD_INS)+
                                       MAR_FIJ)/100)
      }else{
        Taus <- seq.Date(from = as.Date(fila[,"FEC_VEN"]),
                         to = as.Date(fila[,"FEC_DAT"]),
                         by = paste(as.character(-12/fila[,"TIP_PER"]),"months",
                                    sep=" "))
        tabla <- fila[rep(seq_len(nrow(fila)), each = length(Taus)), ] %>%
          mutate(Fecha.Pago = Taus) %>%
          mutate(tau = Tau(FEC_DAT, Fecha.Pago)) %>% filter(tau>=Contador) %>%
          mutate(Pago=ifelse(Fecha.Pago==FEC_VEN,1+
                               (V_Cupones.variables(tau,COD_INS)+MAR_FIJ)/100,
                             (V_Cupones.variables(tau,COD_INS)+MAR_FIJ)/100))
      }
      
      #
      FactDesc_reden <- function(fila){
        if (fila[,'COD_MON'] == 1){
          Factor<-Matriz.trayectorias.col[,floor(fila[,'tau'])]/
            Matriz.trayectorias.col[,Contador] 
        }else{
          Factor<-Matriz.trayectorias.dol[,floor(fila[,'tau'])]/
            Matriz.trayectorias.dol[,Contador] 
        }
        return(Factor)
      }
      
      #
      Precio.Teorico <- lapply(split(tabla,seq(nrow(tabla))),FactDesc_reden)
      Precio.Teorico <- data.frame(matrix(unlist(Precio.Teorico),
                                          nrow=length(Precio.Teorico),
                                          byrow=T))
      Precio.Teorico <- Precio.Teorico*tabla$Pago
      Precio.Teorico <- apply(Precio.Teorico,2,sum)
      
      return(Precio.Teorico)
    }
    
    #
    data.frame <- as.data.frame(rep(NA,cant.simu))
    colnames(data.frame) <- 'Periodo_Venta'
    
    #
    Periodo_Venta1 <- NA
    k=1
    while(k <= length(P_gatillo) & sum(is.na(Periodo_Venta1))>0){
      data.frame<-data.frame %>% mutate(P1 = Tau.total.TV_reden(fila,k)) %>%
        mutate(P1<P_gatillo[k])
      colnames(data.frame) <- c(colnames(data.frame)[-ncol(data.frame)],
                                paste0('VENTA',k))
      Periodo_Venta1 <- ifelse(is.na(data.frame[,'Periodo_Venta']),
                               ifelse(data.frame[,ncol(data.frame)]==TRUE,k,NA),
                               data.frame[,'Periodo_Venta'])
      data.frame <- data.frame %>% mutate(Periodo_Venta=Periodo_Venta1)
      k=k+1
    }
    
    #
    if(sum(is.na(data.frame$Periodo_Venta))<nrow(data.frame)){
      Precio1<-function(k,Contador){
        if(is.na(Contador)){
          PRECIO1<-NA
        }else{
          FactDesc_reden <- function(fila){
            if (fila[,'COD_MON'] == 1){
              Factor<-Matriz.trayectorias.col[k,floor(fila[,'tau'])]/
                Matriz.trayectorias.col[k,Periodo]
            }else{
              Factor<-Matriz.trayectorias.dol[k,floor(fila[,'tau'])]*
                TC/Matriz.trayectorias.col[k,Periodo]       }
            return(Factor)
          }
          
          #
          Tau.total.TV_reden <- function(fila){
            if(fila[,"TIP_PER"]==0){
              tabla <- fila %>%  mutate(tau = Tau(FEC_DAT, FEC_VEN),
                                        Fecha.Pago = FEC_VEN,Pago=1+
                                          (V_Cupones.variables(tau,COD_INS)+
                                             MAR_FIJ)/100)
            }else{
              Taus <- seq.Date(from = as.Date(fila[,"FEC_VEN"]),
                               to = as.Date(fila[,"FEC_DAT"]),
                               by = paste(as.character(-12/fila[,"TIP_PER"]),"months",sep=" "))
              tabla <- fila[rep(seq_len(nrow(fila)), each = length(Taus)), ] %>% mutate(Fecha.Pago = Taus) %>%
                mutate(tau = Tau(FEC_DAT, Fecha.Pago)) %>% filter(tau>=Contador) %>%
                mutate(Pago=ifelse(Fecha.Pago==FEC_VEN,1+
                                     (V_Cupones.variables(tau,COD_INS)+MAR_FIJ)/100,
                                   (V_Cupones.variables(tau,COD_INS)+MAR_FIJ)/100))
            }
            
            #
            Precio.Teorico <- lapply(split(tabla,seq(nrow(tabla))),FactDesc_reden)
            Precio.Teorico <- data.frame(matrix(unlist(Precio.Teorico),
                                                nrow=length(Precio.Teorico), 
                                                byrow=T))
            Precio.Teorico <- Precio.Teorico*tabla$Pago
            Precio.Teorico <- sum(Precio.Teorico)
            
            return(Precio.Teorico)
          }
            
          #
          PRECIO1 <- Tau.total.TV_reden(fila)*rbernoulli(1,exp(-Contador*fila[,'Parametro']))
          
        }
        
        #
        return(PRECIO1)
        }
      
      # 
      V_Precio1 <- Vectorize(Precio1,vectorize.args = c('k','Contador'))
      
      #
      data.frame <- data.frame %>% mutate(k=1:nrow(data.frame)) %>% 
        mutate(PRECIO1=V_Precio1(k,Periodo_Venta),
               PrecioInicial=t(as.matrix(BONOS.TV.RESULTADOS[which(BONOS.TV.RESULTADOS$COD_ISIN==fila[,'COD_ISIN'])[1],
                                                             8:ncol(BONOS.TV.RESULTADOS)]))) %>%
        mutate(PRECIOFINAL=ifelse(is.na(PRECIO1),PrecioInicial,PRECIO1))
       
      #
      BONOS.TV.RESULTADOS[which(BONOS.TV.RESULTADOS$COD_ISIN==fila[,'COD_ISIN']),
                          8:ncol(BONOS.TV.RESULTADOS)]<-matrix(rep(data.frame$PRECIOFINAL,
                                                            each=length(which(BONOS.TV.RESULTADOS$COD_ISIN==fila[,'COD_ISIN']))),byrow = F,nrow = length(which(BONOS.TV.RESULTADOS$COD_ISIN==fila[,'COD_ISIN'])))   
  }
}

#
for(j in 1:nrow(RESUMEN.TV)){
  Redencion_TV(RESUMEN.TV[j,])
}


#############################################################################


                      #################################
                      ###                           ###
                      ###  4. Módulo de Acciones    ###
                      ###                           ###
                      #################################


################## Manipulación e Imputación de Datos ########################


# Se inicializa la matriz R:
matriz.R <- ACCIONES %>% group_by(FEC_DAT, COD_ISIN, COD_ENT) %>% 
  mutate(nr=row_number()) %>% mutate(MAX=max(nr)) %>% filter(MAX==nr) %>%
  ungroup() %>% 
  select(-nr,-MAX)

# Se encuentran cuales son los títulos a valorar:
titulos.ul <- matriz.R %>% 
  mutate(fec.valoracion = paste(year(FEC_DAT),month(FEC_DAT),sep="-")) %>% 
  filter(fec.valoracion==paste(anno,mes,sep="-"))
exa.fec <- max(titulos.ul$FEC_DAT)
titulos.ul <- titulos.ul %>% filter(FEC_DAT == exa.fec) %>% 
  select(COD_ISIN) %>% unique()

# Se segregan los títulos:
matriz.R <- as.matrix(matriz.R %>% filter(exa.fec>=FEC_DAT) %>% 
                        filter(COD_ISIN %in% titulos.ul$COD_ISIN) %>% 
                        select(COD_ISIN, FEC_DAT, Precio) %>% 
                        group_by(COD_ISIN, FEC_DAT) %>%
                        mutate(Precio = as.numeric(mean(Precio, na.rm = TRUE))) %>% 
                        ungroup %>%
                        unique() %>% 
                        group_by(COD_ISIN, FEC_DAT) %>%
                        mutate(rn = row_number()) %>% 
                        ungroup %>%
                        spread(FEC_DAT, Precio) %>% 
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
rep.acciones.val <- round(100*sum(as.numeric(matriz.R[titulo.ob$Criterio,
                                                      ncol(matriz.R)]),
                                  na.rm = TRUE)/sum(as.numeric(matriz.R[,ncol(matriz.R)]),
                                                    na.rm = TRUE),2)
rep.acciones.no.val <- round(100*sum(as.numeric(matriz.R[!titulo.ob$Criterio, 
                                                         ncol(matriz.R)]),
                                     na.rm = TRUE)/sum(as.numeric(matriz.R[,ncol(matriz.R)]),
                                                       na.rm = TRUE),2)

# Se segregan los elementos a valorar:
matriz.R <- matriz.R[titulo.ob$Criterio,]

# Se calculan los rendimientos:
matriz.R1 <- (matriz.R[,-c(1, (ncol(matriz.R)-2):ncol(matriz.R))]/
                matriz.R[,-((ncol(matriz.R)-3):ncol(matriz.R))])^30
matriz.R1[which(!is.finite(matriz.R1))] <- NA
matriz.R2 <- matriz.R[,((ncol(matriz.R)-2):(ncol(matriz.R)-1))]/
  matriz.R[,((ncol(matriz.R)-1):ncol(matriz.R))]
matriz.R2[which(!is.finite(matriz.R2))] <- NA
matriz.R <- cbind(matriz.R1, matriz.R2)

# Se imputan los datos:
matriz.R <- ClustImpute(as.data.frame(matriz.R), 
                        nr_cluster = round((cant.tit+1)/2),
                        nr_iter = 10)$complete_data

# Se traspone la matriz:
matriz.R <- t(as.matrix(matriz.R))

# Se crean los conjuntos de eventos con clasificación por k-means:
acc.clas <- kmeans(matriz.R, cant.tit+1, iter.max = 1000, 
                   nstart = 1000, algorithm = "MacQueen")$cluste

# Se aplica la nueva clasificación:
matriz.R <- t(matriz.R)

# Porbabilidad Objetiva:
prob.objetiva <- as.data.frame(table(acc.clas))$Freq/
  sum(as.data.frame(table(acc.clas))$Freq)

# Se calculan los representantes:
matriz.R <- rowGrpMeans(matriz.R, as.factor(acc.clas))

# Segregamos los precios iniciales:
Ini.pre <- as.data.frame(Ini.pre) %>% 
  filter(COD_ISIN %in% rownames(matriz.R))
indexA <- Ini.pre$COD_ISIN
Ini.pre <- as.matrix(as.data.frame(Ini.pre) %>% select(-COD_ISIN))
colnames(Ini.pre) <- "acción"
Ini.pre <- apply(Ini.pre, 2, as.numeric)
row.names(Ini.pre) <- indexA

# Corrección de la matriz R:
matriz.R[(lim.rend < matriz.R)] <- 1


##############################################################################


                    #################################
                    ###                           ###
                    ###     6. Backtesting        ###
                    ###                           ###
                    #################################


################################# Bonos ######################################


# Lista de almacenamieto de resultados:
LISTA.RESULTADOS <- list()


PRECIOS_OBS <- BONOS.TODO %>% 
  filter(year(FEC_DAT)==anno, month(FEC_DAT)==mes) %>% 
  select(COD_ISIN,COD_ENT,VAL_FAC,PRECIO)
Bernoullis.TF2 <- matrix(1,nrow = nrow(BONOS.TF.RESUMEN),ncol = cant.simu)
Bernoullis.TV2 <- matrix(1,nrow = nrow(BONOS.TV.RESUMEN),ncol = cant.simu)

#
LISTA.RESULTADOS[[1]] <- list(PrecioTF1=BONOS.TF.RESULTADOS,
                              PrecioTV1=BONOS.TV.RESULTADOS,
                              PreciosObs=PRECIOS_OBS)


#
for(h in 2:Periodo_bt){

  Periodo<-h
  BONOS.TF.RESUMEN <- BONOS.TF.RESUMEN %>% filter(Tau(FEC_DAT,FEC_VEN)>=Periodo)
  BONOS.TV.RESUMEN <- BONOS.TV.RESUMEN %>% filter(Tau(FEC_DAT,FEC_VEN)>=Periodo)
  
  if(mes < 12){
    
    #
    mes <- mes+1
    

    #
    BONOS.TF.PRECIOS <- lapply(split(BONOS.TF.RESUMEN,seq(nrow(BONOS.TF.RESUMEN))),
                               Tau.total.TF)
    
    #
    BONOS.TF.PRECIOS <- data.frame(matrix(unlist(BONOS.TF.PRECIOS),
                                          nrow=length(BONOS.TF.PRECIOS),
                                          byrow=T))
    
    #
    Bernoullis.TF2<-Bernoullis.TF1*Bernoullis.TF2
    Bernoullis.TF1 <- t(V_DEFAULT(exp(-BONOS.TF.RESUMEN$Parametro)))
    
    #
    BONOS.TF.PRECIOS <- BONOS.TF.PRECIOS*Bernoullis.TF1*Bernoullis.TF2
    BONOS.TF.PRECIOS <- cbind(BONOS.TF.RESUMEN$COD_ISIN,
                              BONOS.TF.PRECIOS)
    colnames(BONOS.TF.PRECIOS)[1] <- 'COD_ISIN'
    
    #
    BONOS.TF.RESULTADOS <- right_join(BONOS.TF[,c('COD_ISIN',
                                                  'COD_ENT',
                                                  'VAL_FAC',
                                                  'PRECIO_TEORICO_0')],
                                      BONOS.TF.PRECIOS, 
                                      by = "COD_ISIN")
    
    #
    for(j in 1:nrow(RESUMEN.TF)){
      Redencion_TF(RESUMEN.TF[j,])
    }

    #
    BONOS.TV.PRECIOS <-lapply(split(BONOS.TV.RESUMEN,
                                    seq(nrow(BONOS.TV.RESUMEN))),
                              Tau.total.TV)
    
    #
    BONOS.TV.PRECIOS <- data.frame(matrix(unlist(BONOS.TV.PRECIOS),
                                          nrow=length(BONOS.TV.PRECIOS),
                                          byrow=T))
    
    #
    Bernoullis.TV2 <- Bernoullis.TV1*Bernoullis.TV2
    Bernoullis.TV1 <- t(V_DEFAULT(exp(-BONOS.TV.RESUMEN$Parametro))) 
    
    #
    BONOS.TV.PRECIOS <- BONOS.TV.PRECIOS*Bernoullis.TV2*Bernoullis.TV1
    BONOS.TV.PRECIOS <- cbind(BONOS.TV.RESUMEN$COD_ISIN,
                              BONOS.TV.PRECIOS)
    colnames(BONOS.TV.PRECIOS)[1] <- 'COD_ISIN'
    
    #
    BONOS.TV.RESULTADOS <- right_join(BONOS.TV[,c('COD_ISIN',
                                                  'COD_ENT',
                                                  'VAL_FAC',
                                                  'PRECIO_TEORICO_0')],
                                      BONOS.TV.PRECIOS,
                                      by = "COD_ISIN")
    
    #
    for(j in 1:nrow(RESUMEN.TV)){
      Redencion_TV(RESUMEN.TV[j,])
    }
    
    #
    PRECIOS_OBS <- BONOS.TODO %>% 
      filter(year(FEC_DAT)==anno, month(FEC_DAT)==mes) %>% 
      select(COD_ISIN,COD_ENT,VAL_FAC,PRECIO)
    
    #
    LISTA.RESULTADOS[[h]] <- list(PrecioTF1=BONOS.TF.RESULTADOS,
                                  PrecioTV1=BONOS.TV.RESULTADOS,
                                  PreciosObs=PRECIOS_OBS)
    
    #
  } else{
    mes<-1
    anno<-anno+1
    
   
    #
    BONOS.TF.PRECIOS <-lapply(split(BONOS.TF.RESUMEN,
                                    seq(nrow(BONOS.TF.RESUMEN))),Tau.total.TF)
    
    #
    BONOS.TF.PRECIOS <- data.frame(matrix(unlist(BONOS.TF.PRECIOS), nrow=length(BONOS.TF.PRECIOS), byrow=T))
    
    #
    Bernoullis.TF2<-Bernoullis.TF1*Bernoullis.TF2
    Bernoullis.TF1 <- t(V_DEFAULT(exp(-BONOS.TF.RESUMEN$Parametro)))
    
    #
    BONOS.TF.PRECIOS <- BONOS.TF.PRECIOS*Bernoullis.TF1*Bernoullis.TF2
    BONOS.TF.PRECIOS <- cbind(BONOS.TF.RESUMEN$COD_ISIN,BONOS.TF.PRECIOS)
    colnames(BONOS.TF.PRECIOS)[1] <- 'COD_ISIN'
    
    #
    BONOS.TF.RESULTADOS <- right_join(BONOS.TF[,c('COD_ISIN','COD_ENT','VAL_FAC','PRECIO_TEORICO_0')],
                                      BONOS.TF.PRECIOS,
                                      by = "COD_ISIN")
    
    #
    for(j in 1:nrow(RESUMEN.TF)){
      Redencion_TF(RESUMEN.TF[j,])
    }
    
    #
    BONOS.TV.PRECIOS <- lapply(split(BONOS.TV.RESUMEN,
                                     seq(nrow(BONOS.TV.RESUMEN))),
                               Tau.total.TV)
    
    #
    BONOS.TV.PRECIOS <- data.frame(matrix(unlist(BONOS.TV.PRECIOS),
                                          nrow=length(BONOS.TV.PRECIOS),
                                          byrow=T))
    
    #
    Bernoullis.TV2<-Bernoullis.TV1*Bernoullis.TV2
    Bernoullis.TV1 <- t(V_DEFAULT(exp(-BONOS.TV.RESUMEN$Parametro))) 
    
    #
    BONOS.TV.PRECIOS <- BONOS.TV.PRECIOS*Bernoullis.TV2*
      as.numeric(Bernoullis.TV1)
    BONOS.TV.PRECIOS <- cbind(BONOS.TV.RESUMEN$COD_ISIN,
                              BONOS.TV.PRECIOS)
    colnames(BONOS.TV.PRECIOS)[1] <- 'COD_ISIN'
    
    #
    BONOS.TV.RESULTADOS <- right_join(BONOS.TV[,c('COD_ISIN',
                                                  'COD_ENT',
                                                  'VAL_FAC',
                                                  'PRECIO_TEORICO_0')],
                                      BONOS.TV.PRECIOS,by = "COD_ISIN")
    
    #
    for(j in 1:nrow(RESUMEN.TV)){
      Redencion_TV(RESUMEN.TV[j,])
    }
    
    #
    PRECIOS_OBS <- BONOS.TODO %>% filter(year(FEC_DAT)==anno,
                                         month(FEC_DAT)==mes) %>% 
      select(COD_ISIN,COD_ENT,VAL_FAC,PRECIO)
    
    #
    LISTA.RESULTADOS[[h]] <- list(PrecioTF1=BONOS.TF.RESULTADOS,
                                  PrecioTV1=BONOS.TV.RESULTADOS,
                                  PreciosObs=PRECIOS_OBS)
  }
}


################################ Acciones ####################################


# Se inicializa la matriz:
simu.matriz <- matrix(row.names(matriz.R), ncol = 1)
colnames(simu.matriz) <- "COD_ISIN"

# Se generan las simulaciones:
for(mesi in 1:cant.simu){
  
  # Se ejecuta la simulación para un periodo:
  ejec <- matriz.R%*%rmultinom(Periodo, 1, prob.objetiva)
  ejec <- apply(ejec, 1, prod)
  
  # Se guardan los resultados:
  simu.matriz <- cbind(simu.matriz, ejec)
  colnames(simu.matriz)[ncol(simu.matriz)] <- paste("X",mesi, sep = "")
}

# Se extrae la información de la simulación:
ACCIONES.RESULTADOS <- ACCIONES %>%
  filter(FEC_DAT==exa.fec, COD_ISIN%in%row.names(titulo.ob %>%
                                                   filter(Criterio == TRUE))) %>% 
  select(COD_ISIN, COD_ENT, COD_MOD_INV, ADMIN, COD_EMI, COD_SEC, VAL_FAC, Precio) %>% 
  group_by(COD_ISIN, COD_ENT) %>% 
  mutate(VAL_FAC = sum(VAL_FAC), Precio = mean(Precio)) %>% 
  ungroup() %>% unique() %>%  
  left_join(as.data.frame(simu.matriz), by="COD_ISIN") %>% 
  rename(PRECIO_TEORICO_0 = Precio)
