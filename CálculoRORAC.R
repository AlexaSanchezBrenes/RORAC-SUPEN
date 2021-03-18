#                     Consultoria RORAC-SUPEN 
#     Calculo de distribucion de rendimientos del portafolio de bonos 

# Autores:
# Alexa Sanchez
# Isaac Z. Arias

# En este script calculamos los rendimientos del portafolio de bonos segun 
# la entidad que posea los titulos a un periodo de 12 meses (parametro que
# se puede cambiar). El codigo se distribuye de la siguiente manera:
#
#             1. Modulo General
#                    - Paquetes Utilizados  
#                    - Parametros Generales
#                    - Carga de Datos Generales
#                    - Manipulacion Y Limpieza de Datos Generales
#             2. Modulo de Bonos
#                    - Parametros de Datos Bonos
#                    - Carga y Manipulacion de Datos para Bonos
#                    - Funciones del Modelo de Bonos
#                    - Calculo de Curva Par
#                         • Tasa Fija
#                         • Tasa Variable
#                    - Estimacion del Riesgo de Credito
#                         • Tasa Fija
#                         • Tasa Variable
#                    - Calculo del Precio Teorico
#                         • Tasa Fija
#                         • Tasa Variable
#                    - Simulacion Ho-Lee
#                         • Carga de Datos Ho-Lee CRC
#                         • Parametros Ho-Lee CRC
#                         • Funciones Ho-Lee CRC
#                         • Carga de Datos Ho-Lee USD
#                         • Parametros Ho-Lee USD
#                         • Funciones Ho-Lee USD
#                    - Valoracion de Bonos
#                         • Ajuste para Redencion Anticipada Tasa Fija
#                         • Ajuste para Redencion Anticipada Tasa Variable
#             3. Modulo de Acciones
#                    - Manipulacion e Imputacion de Datos
#                    - Simulacion de Acciones
#             4. Calculo RORAC
#                    - Optimizacion del Portafolio de Mercado
#                    - Calculo del Benchmark de Mercado
#                    - Portafolio Individual por Regimen
#                         • Parametros de Almacenamiento 
#                         • Calculo de RORACs
#                    - Visualizacion
#
#############################################################################


                       #################################
                       ###                           ###
                       ###    1. Modulo General      ###
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
library(optimization)
library(pso)
library(purrr)
library(ggplot2)
library(wrMisc)
library(ClustImpute)
library(nleqslv)
options(stringsAsFactors = FALSE)
options(scipen=999, digits = 8)


############################ Parametros Generales ###########################


# Mes a Valorar:
mes <- 3

# Anno a valorar:
anno <- 2020 

# Cantidad de periodos en meses al cual se va a calcular el RORAC
Periodo <- 12 

# Tasa del primer vencimiento TRI en el primer mes observado (marzo):
TRI.corta <- log(1+1.25/100/52)*52/12

# Fijamos los parametros de la curva Nelson Siegel o Svensson encontrada:
Par.NS <- c(8.337060e-03, TRI.corta-8.337060e-03, -3.330267e-12, 1.811465e+01)
Par.SA <- c(6.253579e-03,-0.005212038,-2.791880e-05,6.6101177e-06,1.357261e+01,4.806624e+01)

# Parametro que indica si el modelo utilizado es el Svensson (si es Nelson Siegel fijarlo como 0)
Svensson <- 1 

# Cantidad de simulaciones:
cant.simu <- 10000

# Tipo de cambio de compra al cierre del mes a valorar:
TC <- 579.5 

# Se define el nivel de significancia:
significancia <- 1/100

# Limite de rendimiento de acciones permitido:
lim.rend <- 700

# Intensidad de Optimizacion de Portafolio:
Int.Port <- 7

############################## Carga de Datos ################################


# Dirreccion de los datos:
Dir <- "C:/Users/EQUIPO/Desktop/Estudios/RORAC-SUPEN/Títulos"

# Funcion para leer los datos:
# Input: archivos .txt que contienen la informacion de las transacciones
lee.datos <<- function(archivo){
  
  tbl<- as.data.frame(unclass(read.table(archivo, 
                                         header = TRUE, 
                                         encoding = "Latin1",
                                         stringsAsFactors = F)))
  return(tbl)
}

# Funcion para obtener los datos de las carpetas

# Input: direccion de la carpeta donde se encuentran los archivos con las 
# transacciones
# Output: Lista con todas las transacciones
lista.df<-function(path=Dir){
  
  folder<-list.files(path,full.name = TRUE )
  n<-length(folder)
  
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


################ Manipulacion Y Limpieza de Datos Generales ##################


# Unificacion de variables dado el cambio de la codificacion realizada por supen
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

# Filtramos los titulos que corresponden a bonos
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

# Funcion para calcular la diferencia de fechas en meses:
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
                       ###   2. Modulo de Bonos      ###
                       ###                           ###
                       #################################


######################## Parametros Del Modelo de Bonos ######################


# Fecha Inicial como objeto fecha:
Fecha.Inicial <- as.Date(paste0(mes,"/01/",anno), format = '%m/%d/%Y')

# Fecha final observada:
Fecha.Final <- max(BONOS$FEC_VEN)

# Distancia en Meses: 
tiempo <- ceiling(Tau(Fecha.Inicial, Fecha.Final))

# Curva de Espectativas de Inflacion:
Inflacion <- rep(0.05, times=tiempo)
Inflacion <- as.data.frame(cbind(0:(tiempo-1),Inflacion))
colnames(Inflacion) <- c("Tau","Inflacion")

# Curva de Espectativas de Tasas de Interes:
Curva.Espectativas <- data.frame(Tau = 0:(tiempo-1), Tasa = rep(0.05, times = tiempo))


################## Carga y Manipulacion Del Modelo de Bonos ##################


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
BONOS <- BONOS %>% filter(year(FEC_DAT)==anno & month(FEC_DAT)==mes) %>% 
  filter(Tau(FEC_DAT,FEC_VEN)>=Periodo)

# Filtramos las observaciones por tasa fija y variable:
BONOS.TV <- BONOS %>% filter(MAR_FIJ!=0 | COD_INS %in% c('bemv')) 
BONOS.TF <- BONOS %>% filter(!(COD_ISIN %in% unique(BONOS.TV$COD_ISIN)))
BONOS.TF <- BONOS.TF %>% 
  filter(!is.na(TAS_FAC), TAS_FAC!=0) 


######################## Funciones Del Modelo de Bonos #######################


# Funcion para interpolar linealmente las tasas:
inter.lin.tasas <- function(puntos){
  # Se calculan los precios mensuales interpolados:
  tasas <- approxfun(puntos$Vencimiento, puntos$Tasa)(0:tiempo)
  curva.meses <- data.frame(Vencimiento = 0:tiempo, TasasS = tasas)
  return(curva.meses)
}

# Funcion que genera el precio de la curva a un tiempo (Tao) dado:
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

# Vectorizacion de la funcion del precio:
V_Precio <- Vectorize(Precio) 

# Funcion para calcular el precio teorico de cada bono considerando su riesgo crediticio:
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

# Funcion de interpolacion para la curva de espectativas de inflacion:
Inflacion_Interp <- function(tau){
  return(approxfun(Inflacion$Tau,Inflacion$Inflacion)(tau))
}


########################## Calculo de la Curva Par ###########################


#------------------------------ Tasa Fija ------------------------------------

# Funcion que calcula la tasa par para los bonos tasa fija:
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
    
  # Esta funcion calcula el error de la tasa par:
    TIR <- function(tasa){
      tabla <- tabla %>% mutate(desc=(1+tasa)^-tau) %>%
        mutate(Pago_desc=Pago*desc)
      1-sum(tabla$Pago_desc)
    }
    
    Opt <- nleqslv(0,TIR)
    return(cbind(fila,TIR=Opt$x))
    
}

# Se seleccionan las observaciones con caracteristicas unicas de los bonos tasa fija:
BONOS.TF.RESUMEN <- BONOS.TF %>%
  distinct(COD_ISIN,TIP_PER,FEC_DAT,FEC_VEN,TAS_FAC,COD_INS)

# Se encuentra la tasa par para cada bono tasa fija con caracteristicas unicas:
BONOS.TF2 <- lapply(split(BONOS.TF.RESUMEN,seq(nrow(BONOS.TF.RESUMEN))),TIR.TF)

# Se convierte a data frame la base que consolida las otras caracteristicas de los bonos tasa fija con su respectiva tasa par:
BONOS.TF2 <- data.frame(matrix(unlist(BONOS.TF2), nrow=length(BONOS.TF2), byrow=T))
colnames(BONOS.TF2) <- c(colnames(BONOS.TF.RESUMEN),'TIR')

# Se convierten en variables numericas la tasa facial, la periodicidad y la tasa par:
BONOS.TF2 <- BONOS.TF2 %>% 
  mutate(TAS_FAC=as.numeric(TAS_FAC),TIP_PER=as.numeric(TIP_PER),TIR=as.numeric(TIR))

# Se agrega la tasa par de cada bono con caracteristicas unicas a la base que contiene todos los bonos tasa fija:
BONOS.TF <- left_join(BONOS.TF,BONOS.TF2, by = c("FEC_DAT", "COD_INS", "FEC_VEN", "COD_ISIN", "TAS_FAC", "TIP_PER"))


#----------------------------- Tasa Variable ---------------------------------


# Se agrega la curva de espectativas de tasa de interes:
curva.tasas <- list(Tasa = Curva.Espectativas)
curva.tasas <- rep(curva.tasas, times = length(unique(BONOS.TV$COD_INS)))
names(curva.tasas) <- unique(BONOS.TV$COD_INS)

# Funcion que devuelve la tasa de interes a un plazo tau correspondiente a un nemotecnico especifico:
Cupones.variables <- function(tau,nemotec){
  tasa <- approxfun(curva.tasas[[nemotec]]$Tau,curva.tasas[[nemotec]]$Tasa)(tau)
  return(tasa)
}

# Funcion vectorizada que devuelve la tasa de interes a un plazo tau correspondiente a un nemotecnico especifico:
V_Cupones.variables <- Vectorize(Cupones.variables)

# Funcion que calcula la tasa par para los bonos tasa variable:
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
  
  # Esta funcion calcula el error de la tasa par:
  TIR<-function(tasa){
    tabla<-tabla %>% 
      mutate(desc=(1+tasa)^-tau) %>% mutate(Pago_desc=Pago*desc)
    1-sum(tabla$Pago_desc)
  }
  
  Opt<-nleqslv(0,TIR)
  return(cbind(fila,TIR=Opt$x))
}

# Se seleccionan las observaciones con caracteristicas unicas de los bonos tasa variable:
BONOS.TV.RESUMEN <- BONOS.TV %>% 
  distinct(COD_ISIN,FEC_DAT,FEC_VEN,COD_INS,TIP_PER,MAR_FIJ)

# Se encuentra la tasa par para cada bono tasa variable con caracteristicas unicas:
BONOS.TV2 <- lapply(split(BONOS.TV.RESUMEN, seq(nrow(BONOS.TV.RESUMEN))),TIR.TV)

# # Se convierte a data frame la base que consolida las otras caracteristicas de los bonos tasa variable con su respectiva tasa par:
BONOS.TV2 <- data.frame(matrix(unlist(BONOS.TV2), nrow = length(BONOS.TV2), byrow = T))
colnames(BONOS.TV2) <- c(colnames(BONOS.TV.RESUMEN), 'TIR')

# Se convierten en variables numericas la periodicidad, el margen y la tasa par:
BONOS.TV2 <- BONOS.TV2 %>% 
  mutate(TIP_PER = as.numeric(TIP_PER), MAR_FIJ = as.numeric(MAR_FIJ), TIR = as.numeric(TIR))

# Se agrega la tasa par de cada bono con caracteristicas unicas a la base que contiene todos los bonos tasa variable:
BONOS.TV <- left_join(BONOS.TV,BONOS.TV2, by = c("FEC_DAT", "COD_INS", "MAR_FIJ", "FEC_VEN", "COD_ISIN", "TIP_PER"))


##################### Estimacion del Riesgo de Credito #######################


#------------------------------ Tasa Fija ------------------------------------

# Funcion que debe ser minimizada para estimar el parametro mu:
Optimizacion.F <- function(fila){
  FuncionObjetivo <- function(mu){  ## Agrupar por cod isin
    return(abs(PRECIO.TEORICO.TF(fila,mu)-fila[,"PRECIO.OBS"]))
    }
  if(fila[,"COD_MON"]==1 & fila[,"COD_EMI"] %in% c('BCCR','G')){
    Optimizacion <- list(x=0,fvec=0)
    }else{
      Optimizacion<-nleqslv(0,FuncionObjetivo)
      }  
  return(cbind(fila,Parametro=Optimizacion$x,Error=Optimizacion$fvec))
}

# Se pondera el precio observado de cada bono tasa fija en funcion de su valor facial:
PRECIOS.OBS <- BONOS.TF %>% group_by(COD_ISIN) %>%
  mutate(PONDERADOR=VAL_FAC/sum(VAL_FAC)) %>% 
  mutate(PRECIO.POND=PONDERADOR*PRECIO) %>% 
  summarise(PRECIO.OBS=sum(PRECIO.POND), .groups = "keep")

# Se seleccionan las observaciones con caracteristicas unicas de los bonos tasa fija y se adiciona el precio ponderado:
BONOS.TF.RESUMEN<- BONOS.TF %>%
  distinct(COD_ISIN,TIP_PER,FEC_DAT,FEC_VEN,TAS_FAC,COD_MON,COD_EMI,COD_INS)
BONOS.TF.RESUMEN <- left_join(BONOS.TF.RESUMEN,PRECIOS.OBS, by = "COD_ISIN")

# Se aplica la funcion de optimizacion para determinar el parametro mu optimo de cada bono tasa fija con caracteristicas unicas:
Optimizacion.TF <- lapply(split(BONOS.TF.RESUMEN, seq(nrow(BONOS.TF.RESUMEN))), Optimizacion.F)

# Se convierte a data frame la base que consolida las otras caracteristicas de los bonos tasa fija con su parametro mu optimo y el error de optimizacion:
Optimizacion.TF <- data.frame(matrix(unlist(Optimizacion.TF), 
                                     nrow = length(Optimizacion.TF), byrow = T))
colnames(Optimizacion.TF) <- c(colnames(BONOS.TF.RESUMEN),"Parametro",
                               "Error")
Optimizacion.TF$Parametro <- as.numeric(Optimizacion.TF$Parametro)

# Se agrega el parametro mu optimo de cada bono con caracteristicas unicas a la base que contiene todos los bonos tasa fija:
BONOS.TF <- left_join(BONOS.TF,Optimizacion.TF[,c("COD_ISIN","Parametro")],
                      by = "COD_ISIN")


#----------------------------- Tasa Variable ---------------------------------


# Funcion para calcular el precio teorico de cada bono considerando su riesgo crediticio:
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

# Funcion que debe ser minimizada para estimar el parametro mu:
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

# Se pondera el precio observado de cada bono tasa variable en funcion de su valor facial:
PRECIOS.OBS <- BONOS.TV %>% 
  group_by(COD_ISIN) %>% 
  mutate(PONDERADOR = VAL_FAC/sum(VAL_FAC)) %>% 
  mutate(PRECIO.POND = PONDERADOR*PRECIO) %>% 
  summarise(PRECIO.OBS = sum(PRECIO.POND), 
            .groups = "keep")

# Se seleccionan las observaciones con caracteristicas unicas de los bonos tasa variable y se adiciona el precio ponderado:
BONOS.TV.RESUMEN<- BONOS.TV %>% 
  distinct(COD_ISIN, TIP_PER, FEC_DAT, FEC_VEN, COD_MON, MAR_FIJ, COD_INS, COD_EMI)
BONOS.TV.RESUMEN <- left_join(BONOS.TV.RESUMEN, PRECIOS.OBS, by = "COD_ISIN")

# Se aplica la funcion de optimizacion para determinar el parametro mu optimo de cada bono tasa variable con caracteristicas unicas:
Optimizacion.TV <-lapply(split(BONOS.TV.RESUMEN, seq(nrow(BONOS.TV.RESUMEN))), Optimizacion.V)

# Se convierte a data frame la base que consolida las otras caracteristicas de los bonos tasa variable con su parametro mu optimo y el error de optimizacion:
Optimizacion.TV <- data.frame(matrix(unlist(Optimizacion.TV),
                                     nrow = length(Optimizacion.TV), byrow = T))
colnames(Optimizacion.TV) <- c(colnames(BONOS.TV.RESUMEN),
                               "Parametro", "Error")

# Se agrega el parametro mu optimo de cada bono con caracteristicas unicas a la base que contiene todos los bonos tasa variable:
BONOS.TV <- left_join(BONOS.TV, Optimizacion.TV[, c("COD_ISIN", "Parametro")],
                      by = "COD_ISIN")
BONOS.TV <- BONOS.TV %>% mutate(Parametro = as.numeric(Parametro))


######################## Calculo del Precio Teorico #########################


#------------------------------ Tasa Fija ------------------------------------

# Funcion que calcula el precio teorico de un bono tasa fija incluyendo su riesgo crediticio:
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

# Se seleccionan las observaciones con caracteristicas unicas de los bonos tasa fija:
BONOS.TF.RESUMEN <- BONOS.TF %>%
  distinct(COD_ISIN,TIP_PER,FEC_DAT,FEC_VEN,TAS_FAC,COD_MON,COD_EMI,Parametro,ES_REDE)

# Se calcula el precio teorico de cada bono tasa fija con caracteristicas unicas: 
PRECIO.TF <- lapply(split(BONOS.TF.RESUMEN, seq(nrow(BONOS.TF.RESUMEN))),
                    PRECIO.TEORICO.TF2)

# Se convierte a data frame la base que consolida las otras caracteristicas de los bonos tasa fija con su precio teorico:
PRECIO.TF <- data.frame(matrix(unlist(PRECIO.TF), nrow=length(PRECIO.TF), 
                               byrow = T))
colnames(PRECIO.TF) <- c(colnames(BONOS.TF.RESUMEN), "PRECIO_TEORICO_0")

# Se agrega el precio teorico de cada bono con caracteristicas unicas a la base que contiene todos los bonos tasa fija:
BONOS.TF <- left_join(BONOS.TF,PRECIO.TF[,c('COD_ISIN','PRECIO_TEORICO_0')],
                      by = "COD_ISIN")

# Se convierte a colones el precio teorico en el caso de los bonos tasa fija en dolares:
BONOS.TF <- BONOS.TF %>% mutate(FACTOR_TC = ifelse(COD_MON == 1, 1, TC)) %>% 
  mutate(PRECIO_TEORICO_0 = as.numeric(PRECIO_TEORICO_0)) %>%
 mutate(PRECIO_TEORICO_0 = PRECIO_TEORICO_0*FACTOR_TC)


#----------------------------- Tasa Variable ---------------------------------


# Funcion que calcula el precio teorico de un bono tasa variable incluyendo su riesgo crediticio:
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

# Se seleccionan las observaciones con caracteristicas unicas de los bonos tasa variable:
BONOS.TV.RESUMEN <- BONOS.TV %>%
  distinct(COD_ISIN,TIP_PER,FEC_DAT,FEC_VEN,TAS_FAC,COD_MON,
           COD_EMI,COD_INS,MAR_FIJ, Parametro,ES_REDE) %>%
  mutate(Parametro = as.numeric(Parametro))

# Se calcula el precio teorico de cada bono tasa variable con caracteristicas unicas: 
PRECIO.TV <-lapply(split(BONOS.TV.RESUMEN,seq(nrow(BONOS.TV.RESUMEN))),
                   PRECIO.TEORICO.TV2)

# Se convierte a data frame la base que consolida las otras caracteristicas de los bonos tasa variable con su precio teorico:
PRECIO.TV <- data.frame(matrix(unlist(PRECIO.TV), nrow=length(PRECIO.TV), 
                               byrow=T))
colnames(PRECIO.TV)<-c(colnames(BONOS.TV.RESUMEN),"PRECIO_TEORICO_0") 

# Se agrega el precio teorico de cada bono con caracteristicas unicas a la base que contiene todos los bonos tasa variable:
BONOS.TV <- left_join(BONOS.TV,PRECIO.TV[,c('COD_ISIN','PRECIO_TEORICO_0')], 
                      by = "COD_ISIN")

# Se convierte a colones el precio teorico en el caso de los bonos tasa variable en dolares:
BONOS.TV <- BONOS.TV %>% mutate(FACTOR_TC=ifelse(COD_MON==1,1,TC)) %>%
  mutate(PRECIO_TEORICO_0=as.numeric(PRECIO_TEORICO_0)) %>%
  mutate(PRECIO_TEORICO_0=PRECIO_TEORICO_0*FACTOR_TC)


############################# Simulacion Ho-Lee ##############################


#------------------------ Carga de Datos Ho-Lee CRC --------------------------


# Historial de Tasas TRI de Costa Rica: 
TRI_colones <- read_excel("TRI colones.xlsx",col_types = c("date", "numeric"))
TRI_colones <- TRI_colones[seq(from = 1, to = nrow(TRI_colones), by = 7),]
TRI_colones<-TRI_colones %>% mutate(Delta = log((1+`1 semana`/100/52))*52/12)


#------------------------ Parametros Ho-Lee CRC ------------------------------


# Se calcula la varianza mensual:
varianza.mensual <- 4*var(TRI_colones$Delta)

# Generamos las proporciones de subida y bajada iniciales:
u2.col <- 1+sqrt(varianza.mensual)
d2.col <- 1-sqrt(varianza.mensual)

# Fijamos el parametro fijo k del modelo:
k.col <- d2.col/u2.col


#------------------------- Funciones Ho-Lee CRC ------------------------------


# Funcion que genera el precio de la curva a un tiempo (Tao) dado:
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

# Funcion del parametro de bajada:
d.t = function(t, p, k.col){ 
  (k.col^(t-1))/((1 - p)*(k.col^(t-1)) + p)
} 

# Funcion para encontrar la tasa corta dado un tiempo (Tao):
Tasa.Corta.t = function(tao, cant_sub, p){
  
  # Se obtiene el precio de acuerdo a las curvas Nelson-Siegel-Svensson
  P_0_t = Precio_col(tao)
  P_0_T = Precio_col(tao + 1)
  
  # Se obtiene la tasa corta mediante la formula.
  r_t = log(P_0_t/P_0_T) - log(d.t(tao + 1, p, k.col)) + cant_sub*log(k.col)
  
  return(r_t)  
}

# Funcion que realiza la simulacion de una trayectoria aleatoria dado un periodo (tiempo):
Arbol.HL.desc <- function(tiempo){   
  
  # Crea la probabilidad:
  p <- (1 - d2.col)/(u2.col - d2.col)
  
  # Forma la trayectoria aleatoria para cada periodo:
  trayectoria = rbernoulli(tiempo, p) 
  
  # Obtiene la cantidad de subidas acumuladas:
  vect_cant_sub = cumsum(trayectoria)
  
  # Aplica la funcion de la tasa corta para cada instante;
  vect_r_t = Vectorize(Tasa.Corta.t)(1:tiempo, vect_cant_sub, p)
  
  # Obtiene el vector de los descuentos D(0,t):
  vect_D_0_T = exp(-cumsum(vect_r_t))
  
  return(vect_D_0_T)
}

# Matriz que consolida las trayectorias de las curvas de tasas de interes en colones:
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
  
  
#------------------------ Parametros Ho-Lee USD ------------------------------
  

# Se calcula la varianza mensual:
varianza.mensual.dol<-30*var(Overnight$Delta)
  
# Generamos las proporciones de subida y bajada iniciales:
u2.dol<-1+sqrt(varianza.mensual.dol) 
d2.dol<-1-sqrt(varianza.mensual.dol)
  
# Fijamos el parametro fijo k del modelo:
k.dol <- d2.dol/u2.dol


#------------------------- Funciones Ho-Lee USD ------------------------------
  

# Funcion para interpolar linealmente las tasas
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
  
# Funcion del parametro de bajada:
d.t = function(t, p, k.dol){ 
  (k.dol^(t-1))/((1 - p)*(k.dol^(t-1)) + p)
 } 
  
# Funcion para encontrar la tasa corta dado un tiempo (Tao):
Tasa.Corta.t = function(tao, cant_sub, p){
  
  # Se obtiene el precio de acuerdo al vector de precios:
  P_0_t = vec.precios[tao,2]
  P_0_T = vec.precios[tao+1,2]
    
  # Se obtiene la tasa corta mediante la formula.
  r_t = log(P_0_t/P_0_T) - log(d.t(tao + 1, p, k.dol)) + cant_sub*log(k.dol)
    
  return(r_t)  
}
  
# Funcion que realiza la simulacion de una trayectoria aleatoria dado un periodo (tiempo):
Arbol.HL.desc <- function(tiempo){   
  # Crea la probabilidad:
  p <- (1 - d2.dol)/(u2.dol - d2.dol)
    
  # Forma la trayectoria aleatoria para cada periodo:
  trayectoria = rbernoulli(tiempo, p) 
    
  # Obtiene la cantidad de subidas acumuladas:
  vect_cant_sub = cumsum(trayectoria)
    
  # Aplica la funcion de la tasa corta para cada instante;
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
  
   
########################## Valoracion de Bonos ###############################
    
# Funcion que devuleve un vector con los factores de descuento que se debe aplicar a un flujo especifico:  
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

# Se agrega al data frame de bonos tasa fija con caracteristicas unicas la probabilidad de sobrevivencia (de no caer en default) por el plazo del periodo:
BONOS.TF.RESUMEN <- BONOS.TF.RESUMEN %>%
  mutate(Probabilidad=exp(-Parametro*Periodo))
 
# Funcion para calcular el precio teorico de un bono tasa fija dadas las simulaciones de las trayectorias de tasas de interes:
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
  
  # A la tabla de los flujos del bono tasa fija, se le aplica los factores de descuento para obtener los precios teoricos:    
  Precio.Teorico <- lapply(split(tabla,seq(nrow(tabla))),FactDesc)
  Precio.Teorico <- data.frame(matrix(unlist(Precio.Teorico),
                                      nrow=length(Precio.Teorico), byrow=T))
  
  # Se multiplica el factor de descuento con el pago de cada flujo para obtener el precio teorico final del bono tasa fija:
  Precio.Teorico <- Precio.Teorico*tabla$Pago
  Precio.Teorico<- apply(Precio.Teorico,2,sum)
   
  return(Precio.Teorico)
}
    
# Se calcula los precios teoricos de cada trayectoria a cada bono tasa fija con caracetristicas unicas:
BONOS.TF.PRECIOS <-lapply(split(BONOS.TF.RESUMEN,seq(nrow(BONOS.TF.RESUMEN))),
                          Tau.total.TF)

# Se convierte a data frame la base que consolida las otras caracteristicas de los bonos tasa fija con sus precios teoricos:
BONOS.TF.PRECIOS <- data.frame(matrix(unlist(BONOS.TF.PRECIOS),
                                      nrow=length(BONOS.TF.PRECIOS), 
                                      byrow=T))

# Funcion que devuelve las variable bernoullis indicando si el bono cayo o no en default en cada trayectoria:
DEFAULT <- function(p){
  rbernoulli(n = cant.simu,p = p)
}

# Funcion vectorizada que devuelve las variable bernoullis indicando si el bono cayo o no en default en cada trayectoria:
V_DEFAULT <- Vectorize(DEFAULT)

# Variables bernoullis que indican si el bono tasa fija cayo o no en default en cada trayectoria de acuerdo a su probabilidad de sobrevivencia:
Bernoullis <- t(V_DEFAULT(BONOS.TF.RESUMEN$Probabilidad))

# Data frame con los precios de los bonos tasa fija incluyendo el riesgo de default:  
BONOS.TF.PRECIOS <- BONOS.TF.PRECIOS*Bernoullis
BONOS.TF.PRECIOS <- cbind(BONOS.TF.RESUMEN$COD_ISIN,BONOS.TF.PRECIOS)
colnames(BONOS.TF.PRECIOS)[1] <- 'COD_ISIN'

# Data frame que consolida los resultados de los precios de los bonos tasa fija para cada trayectoria asi como su precio esperado y sus caracteristicas:
BONOS.TF.RESULTADOS <- BONOS.TF[,c('COD_ISIN',
                                   'COD_ENT',
                                   'COD_MOD_INV',
                                   'ADMIN',
                                   'COD_EMI',
                                   'COD_SEC',
                                   'VAL_FAC',
                                   'PRECIO_TEORICO_0')] %>% 
  group_by(COD_ISIN, COD_ENT, PRECIO_TEORICO_0) %>% 
  mutate(VAL_FAC = sum(VAL_FAC)) %>% ungroup() %>% 
  unique() %>% 
  left_join(BONOS.TF.PRECIOS, by = "COD_ISIN")


#--------------- Ajuste para Redencion Anticipada Tasa Fija ------------------


# Bonos tasa fija con caracteristicas unicas para calcular su redencion:
RESUMEN.TF <- BONOS.TF %>% distinct(FEC_DAT,FEC_VEN,COD_ISIN,TAS_FAC,
                                    TIP_PER,COD_MON,ES_REDE,TIR,
                                    Parametro,COD_INS)
RESUMEN.TF <- RESUMEN.TF %>% filter(ES_REDE=='S')

# Funcion que calcula si un bono tasa fija se redime o no y su precio en ese periodo:
Redencion_TF <- function(fila){

  # Calcula el precio con el cual se debe comparar el precio del bono tasa fija para determinar si hay o no redencion anticipada:
  Precio.gatillo <- function(fila,Contador){
      if(fila[,"COD_INS"] %in% c("tudes","TUDES")){
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
  
  # Funcion vectorizada que calcula el precio con el cual se debe comparar el precio del bono tasa fija para determinar si hay o no redencion anticipada:
  V_Precio.gatillo <- Vectorize(Precio.gatillo,"Contador")
  
  # Se determina el precio para cada periodo con el cual se debe comparar el precio del bono tasa fija para determinar si hay o no redencion anticipada:
  P_gatillo <- V_Precio.gatillo(fila,1:(Periodo-1))
  
  # Funcion que calcula el precio teorico de un bono tasa fija con la opcion de rendencion anticipada para un periodo determinado:
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
    
    # Vector de factores de descuento que se debe aplicar a un flujo determinado de un bono:
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
    
    # A la tabla de los flujos del bono tasa fija con redencion anticipada, se le aplica los factores de descuento para obtener los precios teoricos:    
    Precio.Teorico <- lapply(split(tabla,seq(nrow(tabla))),FactDesc_reden)
    Precio.Teorico <- data.frame(matrix(unlist(Precio.Teorico), 
                                        nrow=length(Precio.Teorico), byrow=T))
    
    # Se multiplica el factor de descuento con el pago de cada flujo para obtener el precio teorico final del bono tasa fija con redencion anticipada:
    Precio.Teorico <- Precio.Teorico*tabla$Pago
    Precio.Teorico<- apply(Precio.Teorico,2,sum)
    
    return(Precio.Teorico)
  }
  
  
  # Data frame para incluir el periodo de venta de cada bono tasa fija con rendencion anticipada y su precio respectivo:
  data.frame<-as.data.frame(rep(NA,cant.simu))
  colnames(data.frame)<-'Periodo_Venta'
  
  # Se determina el periodo de venta del bono tasa fija con redencion anticipada:
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
  
  # En caso de que el bono tasa fija se venda dentro del plazo del periodo (12 meses), se determina su precio de venta:
  if(sum(is.na(data.frame$Periodo_Venta))<nrow(data.frame)){
    # Funcion que calcula para cada trayectoria el precio de un bono tasa fija con redencion anticipada en un periodo determinado:
    Precio1<-function(k,Contador){
      if(is.na(Contador)){
        PRECIO1<-NA
      }else{
        # Calcula el vector de los factores de descuento que se deben aplicar a un flujo determinado: 
        FactDesc_reden <- function(fila){
          if (fila[,'COD_MON'] == 1){
            Factor<-Matriz.trayectorias.col[k,floor(fila[,'tau'])]/
              Matriz.trayectorias.col[k,Periodo]
          }else{
            Factor<-Matriz.trayectorias.dol[k,floor(fila[,'tau'])]*
              TC/Matriz.trayectorias.col[k,Periodo]       }
          return(Factor)
        }
        
        # Calcula el precio de un bono tasa fija con redencion anticipada para una trayectoria:
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
          
          # Se aplica la funcion de factores de descuento a cada flujo del bono tasa fija:
          Precio.Teorico <- lapply(split(tabla,seq(nrow(tabla))),
                                   FactDesc_reden)
          Precio.Teorico <- data.frame(matrix(unlist(Precio.Teorico),
                                              nrow=length(Precio.Teorico),
                                              byrow=T))
          
          # Se multiplica el pago en cada periodo por el factor de descuento asociado: 
          Precio.Teorico <- Precio.Teorico*tabla$Pago
          Precio.Teorico<- sum(Precio.Teorico)
          
          return(Precio.Teorico)
        }
        
         PRECIO1<-Tau.total.TF_reden(fila)*
           rbernoulli(1,exp(Contador*fila[,'Parametro']))
        
        
      }
      
      return(PRECIO1)
    }
    
    # Se vectoriza la funcion que calcula el precio en cada periodo de un bono con redencion anticipada: 
    V_Precio1 <- Vectorize(Precio1,vectorize.args = c('k','Contador'))
    
    # Data frame que consolida el escenario, el precio del bono tasa fija en cada periodo, el periodo de venta y el precio final del bono tasa fija con redencion anticipada: 
    data.frame <- data.frame %>% mutate(k=1:nrow(data.frame)) %>% 
      mutate(PRECIO1=V_Precio1(k,Periodo_Venta),
             PrecioInicial=t(as.matrix(BONOS.TF.RESULTADOS[which(BONOS.TF.RESULTADOS$COD_ISIN==
                                                                   fila[,'COD_ISIN'])[1],
                                                           9:ncol(BONOS.TF.RESULTADOS)]))) %>%
      mutate(PRECIOFINAL=ifelse(is.na(PRECIO1),PrecioInicial,PRECIO1))
    
    # Se modifica el precio de los bonos tasa fija con redencion anticipada en el data frame de resultados:
    BONOS.TF.RESULTADOS[which(BONOS.TF.RESULTADOS$COD_ISIN==fila[,'COD_ISIN']),
                        9:ncol(BONOS.TF.RESULTADOS)]<-matrix(rep(data.frame$PRECIOFINAL,
                                                          each=length(which(BONOS.TF.RESULTADOS$COD_ISIN==
                                                                              fila[,'COD_ISIN']))),byrow = F,nrow = length(which(BONOS.TF.RESULTADOS$COD_ISIN==
                                                                                                                                   fila[,'COD_ISIN'])))   
  }
  
}

# Se aplica la funcion de rendencion a los bonos tasa fija con posibilidad de redencion anticipada: 
for(j in 1:nrow(RESUMEN.TF)){
  Redencion_TF(RESUMEN.TF[j,])
}


#--------------- Ajuste para Redencion Anticipada Tasa Variable ------------------


# Se agrega la probabilidad de sobrevivencia (no caer en deafult) al data frame de los bonos tasa variable con caracteristicas unicas:
BONOS.TV.RESUMEN <- BONOS.TV.RESUMEN %>% 
  mutate(Probabilidad=exp(-Parametro*Periodo))
 
# Funcion para calcular el precio teorico de un bono tasa variable dadas las simulaciones de las trayectorias de tasas de interes:
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
  
  
  # A la tabla de los flujos del bono tasa variable, se le aplica los factores de descuento para obtener los precios teoricos:    
  Precio.Teorico <- lapply(split(tabla,seq(nrow(tabla))),FactDesc)
  Precio.Teorico <- data.frame(matrix(unlist(Precio.Teorico),
                                      nrow=length(Precio.Teorico), 
                                      byrow=T))
  
  # Se multiplica el factor de descuento con el pago de cada flujo para obtener el precio teorico final del bono tasa variable:
  Precio.Teorico <- Precio.Teorico*tabla$Pago
  Precio.Teorico<- apply(Precio.Teorico,2,sum)
  return(Precio.Teorico)
}
    
# Se calcula los precios teoricos de cada trayectoria a cada bono tasa variable con caracetristicas unicas:
BONOS.TV.PRECIOS <-lapply(split(BONOS.TV.RESUMEN,seq(nrow(BONOS.TV.RESUMEN))),
                          Tau.total.TV)

# Se convierte a data frame la base que consolida las otras caracteristicas de los bonos tasa variable con sus precios teoricos:
BONOS.TV.PRECIOS <- data.frame(matrix(unlist(BONOS.TV.PRECIOS), 
                                      nrow=length(BONOS.TV.PRECIOS),
                                      byrow=T))
    
# Variables bernoullis que indican si el bono tasa variable cayo o no en default en cada trayectoria de acuerdo a su probabilidad de sobrevivencia:
Bernoullis <- t(V_DEFAULT(BONOS.TV.RESUMEN$Probabilidad))

# Data frame con los precios de los bonos tasa variable incluyendo el riesgo de default: 
BONOS.TV.PRECIOS <- BONOS.TV.PRECIOS*Bernoullis
BONOS.TV.PRECIOS <- cbind(BONOS.TV.RESUMEN$COD_ISIN,
                          BONOS.TV.PRECIOS)
colnames(BONOS.TV.PRECIOS)[1] <- 'COD_ISIN'
    
# Data frame que consolida los resultados de los precios de los bonos tasa variable para cada trayectoria asi como su precio esperado y sus caracteristicas:
BONOS.TV.RESULTADOS <- BONOS.TV[,c('COD_ISIN',
                                   'COD_ENT',
                                   'COD_MOD_INV',
                                   'ADMIN',
                                   'COD_EMI',
                                   'COD_SEC',
                                   'VAL_FAC',
                                   'PRECIO_TEORICO_0')] %>% 
  group_by(COD_ISIN, COD_ENT, PRECIO_TEORICO_0) %>% 
  mutate(VAL_FAC = sum(VAL_FAC)) %>% ungroup() %>% 
  unique() %>% 
  right_join(BONOS.TV.PRECIOS, by = "COD_ISIN")

# Bonos tasa variable con caracteristicas unicas para calcular su redencion:
RESUMEN.TV <- BONOS.TV %>% distinct(FEC_DAT,FEC_VEN,COD_ISIN,MAR_FIJ,
                                    TIP_PER,COD_MON,ES_REDE,TIR,
                                    Parametro,COD_INS)
RESUMEN.TV <- RESUMEN.TV %>% filter(ES_REDE=='S')

# Funcion que calcula si un bono tasa variable se redime o no y su precio en ese periodo:
Redencion_TV <- function(fila){
  # Calcula el precio con el cual se debe comparar el precio del bono tasa variable para determinar si hay o no redencion anticipada:
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
    
    # Funcion vectorizada que calcula el precio con el cual se debe comparar el precio del bono tasa variable para determinar si hay o no redencion anticipada:
    V_Precio.gatillo<-Vectorize(Precio.gatillo,"Contador")
    
    # Se determina el precio para cada periodo con el cual se debe comparar el precio del bono tasa variable para determinar si hay o no redencion anticipada:
    P_gatillo<-V_Precio.gatillo(fila,1:(Periodo-1))
    
    # Funcion que calcula el precio teorico de un bono tasa variable con la opcion de rendencion anticipada para un periodo determinado:
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
      
      # Vector de factores de descuento que se debe aplicar a un flujo determinado de un bono:
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
      
      # A la tabla de los flujos del bono tasa variable con redencion anticipada, se le aplica los factores de descuento para obtener los precios teoricos:    
      Precio.Teorico <- lapply(split(tabla,seq(nrow(tabla))),FactDesc_reden)
      Precio.Teorico <- data.frame(matrix(unlist(Precio.Teorico),
                                          nrow=length(Precio.Teorico),
                                          byrow=T))
      
      # Se multiplica el factor de descuento con el pago de cada flujo para obtener el precio teorico final del bono tasa variable con redencion anticipada:
      Precio.Teorico <- Precio.Teorico*tabla$Pago
      Precio.Teorico <- apply(Precio.Teorico,2,sum)
      
      return(Precio.Teorico)
    }
    
    # Data frame para incluir el periodo de venta de cada bono tasa variable con rendencion anticipada y su precio respectivo:
    data.frame <- as.data.frame(rep(NA,cant.simu))
    colnames(data.frame) <- 'Periodo_Venta'

    # Se determina el periodo de venta del bono tasa variable con redencion anticipada:
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
    
    # En caso de que el bono tasa variable se venda dentro del plazo del periodo (12 meses), se determina su precio de venta:
    if(sum(is.na(data.frame$Periodo_Venta))<nrow(data.frame)){
      # Funcion que calcula para cada trayectoria el precio de un bono tasa variable con redencion anticipada en un periodo determinado:
      Precio1<-function(k,Contador){
        if(is.na(Contador)){
          PRECIO1<-NA
        }else{
          # Calcula el vector de los factores de descuento que se deben aplicar a un flujo determinado: 
          FactDesc_reden <- function(fila){
            if (fila[,'COD_MON'] == 1){
              Factor<-Matriz.trayectorias.col[k,floor(fila[,'tau'])]/
                Matriz.trayectorias.col[k,Periodo]
            }else{
              Factor<-Matriz.trayectorias.dol[k,floor(fila[,'tau'])]*
                TC/Matriz.trayectorias.col[k,Periodo]       }
            return(Factor)
          }
          
          # Funcion que calcula para cada trayectoria el precio de un bono tasa variable con redencion anticipada en un periodo determinado:
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
            
            # Se aplica la funcion de factores de descuento a cada flujo del bono tasa variable:
            Precio.Teorico <- lapply(split(tabla,seq(nrow(tabla))),FactDesc_reden)
            Precio.Teorico <- data.frame(matrix(unlist(Precio.Teorico),
                                                nrow=length(Precio.Teorico), 
                                                byrow=T))
            # Se multiplica el pago en cada periodo por el factor de descuento asociado: 
            Precio.Teorico <- Precio.Teorico*tabla$Pago
            Precio.Teorico <- sum(Precio.Teorico)
            
            return(Precio.Teorico)
          }
            
          PRECIO1 <- Tau.total.TV_reden(fila)*rbernoulli(1,exp(-Contador*fila[,'Parametro']))
          
        }
        
        return(PRECIO1)
        }
      
      # Se vectoriza la funcion que calcula el precio en cada periodo de un bono con redencion anticipada: 
      V_Precio1 <- Vectorize(Precio1,vectorize.args = c('k','Contador'))
      
      # Data frame que consolida el escenario, el precio del bono tasa variable en cada periodo, el periodo de venta y el precio final del bono tasa variable con redencion anticipada: 
      data.frame <- data.frame %>% mutate(k=1:nrow(data.frame)) %>% 
        mutate(PRECIO1=V_Precio1(k,Periodo_Venta),
               PrecioInicial=t(as.matrix(BONOS.TV.RESULTADOS[which(BONOS.TV.RESULTADOS$COD_ISIN==fila[,'COD_ISIN'])[1],
                                                             9:ncol(BONOS.TV.RESULTADOS)]))) %>%
        mutate(PRECIOFINAL=ifelse(is.na(PRECIO1),PrecioInicial,PRECIO1))
       
      # Se modifica el precio de los bonos tasa variable con redencion anticipada en el data frame de resultados:
      BONOS.TV.RESULTADOS[which(BONOS.TV.RESULTADOS$COD_ISIN==fila[,'COD_ISIN']),
                          9:ncol(BONOS.TV.RESULTADOS)]<-matrix(rep(data.frame$PRECIOFINAL,
                                                            each=length(which(BONOS.TV.RESULTADOS$COD_ISIN==fila[,'COD_ISIN']))),byrow = F,nrow = length(which(BONOS.TV.RESULTADOS$COD_ISIN==fila[,'COD_ISIN'])))   
  }
}

# Se aplica la funcion de rendencion a los bonos tasa variable con posibilidad de redencion anticipada: 
for(j in 1:nrow(RESUMEN.TV)){
  Redencion_TV(RESUMEN.TV[j,])
}


#############################################################################


                      #################################
                      ###                           ###
                      ###  4. Modulo de Acciones    ###
                      ###                           ###
                      #################################


################## Manipulacion e Imputacion de Datos ########################


# Se inicializa la matriz R:
matriz.R <- ACCIONES %>% group_by(FEC_DAT, COD_ISIN, COD_ENT) %>% 
  mutate(nr=row_number()) %>% mutate(MAX=max(nr)) %>% filter(MAX==nr) %>%
  ungroup() %>% 
  select(-nr,-MAX)
    
# Se encuentran cuales son los titulos a valorar:
titulos.ul <- matriz.R %>% 
  mutate(fec.valoracion = paste(year(FEC_DAT),month(FEC_DAT),sep="-")) %>% 
  filter(fec.valoracion==paste(anno,mes,sep="-"))
exa.fec <- max(titulos.ul$FEC_DAT)
titulos.ul <- titulos.ul %>% filter(FEC_DAT == exa.fec) %>% 
  select(COD_ISIN) %>% unique()

# Se segregan los titulos:
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
    
# Cambian los indices:
indices <- matriz.R[,1]
matriz.R <- matriz.R[,-1]
matriz.R <- apply(matriz.R, 2, as.numeric)
row.names(matriz.R) <- indices
    
# Total de observaciones por titulo:
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

# Correccion de la matriz R:
matriz.R[(lim.rend < matriz.R)] <- 1

# Se imputan los datos:
matriz.R <- ClustImpute(as.data.frame(matriz.R), 
                        nr_cluster = round((cant.tit+1)/2),
                        nr_iter = 10)$complete_data
    
# Se traspone la matriz:
matriz.R <- t(as.matrix(matriz.R))
    
# Se crean los conjuntos de eventos con clasificacion por k-means:
acc.clas <- kmeans(matriz.R, cant.tit+1, iter.max = 1000, 
                   nstart = 1000, algorithm = "MacQueen")$cluste
    
# Se aplica la nueva clasificacion:
matriz.R <- t(matriz.R)
    
# Probabilidad Objetiva:
prob.objetiva <- as.data.frame(table(acc.clas))$Freq/
  sum(as.data.frame(table(acc.clas))$Freq)
    
# Se calculan los representantes:
matriz.R <- rowGrpMeans(matriz.R, as.factor(acc.clas))
    
# Segregamos los precios iniciales:
Ini.pre <- as.data.frame(Ini.pre) %>% 
  filter(COD_ISIN %in% rownames(matriz.R))
indexA <- Ini.pre$COD_ISIN
Ini.pre <- as.matrix(as.data.frame(Ini.pre) %>% select(-COD_ISIN))
colnames(Ini.pre) <- "accion"
Ini.pre <- apply(Ini.pre, 2, as.numeric)
row.names(Ini.pre) <- indexA


######################## Simulacion de Acciones ##############################

# Se inicializa la matriz:
simu.matriz <- matrix(row.names(matriz.R), ncol = 1)
colnames(simu.matriz) <- "COD_ISIN"

# Se generan las simulaciones: 
for(mesi in 1:cant.simu){
  
  # Se ejecuta la simulacion para un periodo:
  ejec <- matriz.R%*%rmultinom(Periodo, 1, prob.objetiva)
  ejec <- apply(ejec, 1, prod)
  
  # Se guardan los resultados:
  simu.matriz <- cbind(simu.matriz, ejec)
  colnames(simu.matriz)[ncol(simu.matriz)] <- paste("X",mesi, sep = "")
}

# Se extrae la informacion de la simulacion:
ACCIONES.RESULTADOS <- ACCIONES %>%
  filter(FEC_DAT==exa.fec, COD_ISIN%in%row.names(titulo.ob %>%
                                                   filter(Criterio == TRUE))) %>% 
  select(COD_ISIN, COD_ENT, COD_MOD_INV, ADMIN, COD_EMI, COD_SEC, VAL_FAC, Precio) %>% 
  group_by(COD_ISIN, COD_ENT) %>% 
  mutate(VAL_FAC = sum(VAL_FAC), Precio = mean(Precio)) %>% 
  ungroup() %>% unique() %>%  
  left_join(as.data.frame(simu.matriz), by="COD_ISIN") %>% 
  rename(PRECIO_TEORICO_0 = Precio)


##############################################################################


                      #################################
                      ###                           ###
                      ###   5. Calculo del RORAC    ###
                      ###                           ###
                      #################################


##################### Calculo del Benchmark de Mercado #######################


# Se calcula el valor del portafolio hoy:
val.portafolio.hoy <- sum(BONOS.TF.RESULTADOS$PRECIO_TEORICO_0*BONOS.TF.RESULTADOS$VAL_FAC) +
  sum(BONOS.TV.RESULTADOS$PRECIO_TEORICO_0*BONOS.TV.RESULTADOS$VAL_FAC) +
  sum(ACCIONES.RESULTADOS$VAL_FAC*ACCIONES.RESULTADOS$PRECIO_TEORICO_0)

# Se calculan los valores del portafolio futuro:
bonos.tf.fut.b <- colSums(matrix(rep(BONOS.TF.RESULTADOS$VAL_FAC, ncol(BONOS.TF.RESULTADOS)-8), 
                               nrow = nrow(BONOS.TF.RESULTADOS))*as.matrix(BONOS.TF.RESULTADOS[,-(1:8)]))

bonos.tv.fut.b <- colSums(matrix(rep(BONOS.TV.RESULTADOS$VAL_FAC, ncol(BONOS.TV.RESULTADOS)-8), 
                               nrow = nrow(BONOS.TV.RESULTADOS))*as.matrix(BONOS.TV.RESULTADOS[,-(1:8)]))

acciones.fut.b <- colSums(matrix(rep(ACCIONES.RESULTADOS$PRECIO_TEORICO_0*ACCIONES.RESULTADOS$VAL_FAC,
                                   ncol(ACCIONES.RESULTADOS)-8), 
                               nrow = nrow(ACCIONES.RESULTADOS))*
                          apply(as.matrix(ACCIONES.RESULTADOS[,-(1:8)]),
                                2,
                                as.numeric))

# Rendimiento del portafolio promedio:
REND.Bench <- (mean(acciones.fut.b+bonos.tf.fut.b+bonos.tv.fut.b)-
             val.portafolio.hoy)

# CVAR del valor del portafolio futuro:
CVAR.Bench <- val.portafolio.hoy-mean(sort(acciones.fut.b+bonos.tf.fut.b+bonos.tv.fut.b, decreasing = TRUE)[(cant.simu*(1-significancia)):cant.simu])

# RORAC del portafolio:
RORAC.Bench <- REND.Bench/CVAR.Bench
CVAR.Bench <- CVAR.Bench/val.portafolio.hoy
REND.Bench <- REND.Bench/val.portafolio.hoy


###################### Portafolio Individual por Regimen #####################


#------------------------ Parametros de Almacenamiento -----------------------


# Se dan las entidades observadas:
Entidades <- c(BONOS.TF.RESULTADOS$COD_ENT,BONOS.TV.RESULTADOS$COD_ENT,
               ACCIONES.RESULTADOS$COD_ENT) %>% unique()
rendimientos <- c()
roracs <- c()
cvars <- c()
valorhoy <- c()
cont <- 1


#---------------------------- Calculo de RORACs ------------------------------

# se calculan iterativamente los RORACS de cada entidad:
for(reg in Entidades){
  
  # Se segrega el portafolio
  bonos.tf.reg <- BONOS.TF.RESULTADOS %>% filter(COD_ENT==reg)
  bonos.tv.reg <- BONOS.TV.RESULTADOS %>% filter(COD_ENT==reg)
  accion.reg <- ACCIONES.RESULTADOS %>% filter(COD_ENT==reg)
  
  # Valor Facial Total del Portafolio:
  val.total.por.reg <- sum(bonos.tf.reg$VAL_FAC*bonos.tf.reg$PRECIO_TEORICO_0)+
    sum(bonos.tv.reg$VAL_FAC*bonos.tv.reg$PRECIO_TEORICO_0)+
    sum(accion.reg$VAL_FAC*accion.reg$VAL_FAC)
  
  # Se calculan los valores del protafolio futuro:
  bonos.tf.fut.reg <- colSums(matrix(rep(bonos.tf.reg$VAL_FAC, 
                                         ncol(bonos.tf.reg)-8), 
                                 nrow = nrow(bonos.tf.reg))*
                                as.matrix(bonos.tf.reg[,-(1:8)]))
  
  bonos.tv.fut.reg <- colSums(matrix(rep(bonos.tv.reg$VAL_FAC, 
                                         ncol(bonos.tv.reg)-8), 
                                 nrow = nrow(bonos.tv.reg))*
                                as.matrix(bonos.tv.reg[,-(1:8)]))
  
  acciones.fut.reg <- colSums(matrix(rep(accion.reg$PRECIO_TEORICO_0*accion.reg$VAL_FAC, 
                                         ncol(accion.reg)-8), 
                                 nrow = nrow(accion.reg))*
                                apply(as.matrix(accion.reg[,-(1:8)]),
                                      2,
                                      as.numeric))
  
  # Se corrigen en caso de ausencia:
  if(length(acciones.fut.reg)==0){
    acciones.fut.reg <- 0
  }
  if(length(bonos.tv.fut.reg)==0){
    bonos.tv.fut.reg <- 0
  }
  if(length(bonos.tf.fut.reg)==0){
    bonos.tf.fut.reg <- 0
  }
  
  # Rendimiento del portafolio promedio:
  REND.reg <- (mean(acciones.fut.reg+bonos.tf.fut.reg+bonos.tv.fut.reg)-
                 val.total.por.reg)
  
  # CVAR del valor del portafolio futuro:
  CVAR.reg <- (val.total.por.reg-mean(sort(acciones.fut.reg+bonos.tf.fut.reg+bonos.tv.fut.reg, decreasing = TRUE)[(cant.simu*(1-significancia)):cant.simu]))
  
  # RORAC del protafolio:
  RORAC.reg <- REND.reg/CVAR.reg
  CVAR.reg <- CVAR.reg/val.total.por.reg
  REND.reg <- REND.reg/val.total.por.reg
  
  #guardamos la informacion:
  cvars[cont] <- CVAR.reg
  rendimientos[cont] <- REND.reg
  valorhoy[cont] <- val.total.por.reg
  roracs[cont] <- RORAC.reg
  
  # contador:
  cont <- cont+1
}


################### Optimizacion del Portafolio de Mercado ###################

# Cantidad de capital a distribuir:
masaDis <- 1

# Valor Facial Total del Portafolio:
val.total.por <- val.portafolio.hoy/masaDis

# Se unifican los titulos:
Portafolio.total <- rbind(BONOS.TF.RESULTADOS, BONOS.TV.RESULTADOS, ACCIONES.RESULTADOS)

# Funcion de Optimizacion de Portafolio:
Port.Optim <- function(X){
  
  # se define el portafolio total:
  Titulos.optim <- Portafolio.total
  
  # Se escalan las cantidades de cada título:
  Titulos.optim$VAL_FAC <- Titulos.optim$VAL_FAC/val.total.por
  
  # Se multiplican los valores faciales:
  Titulos.optim <- cbind(Titulos.optim[,1:8], 
                         apply(as.matrix(Titulos.optim[,-(1:8)]),
                               2,
                               as.numeric)*as.vector(X))
  
  # Valores futuros:
  val.portafolio.fut <- colSums(Titulos.optim[,-(1:8)])
  
  # Restricciones y Limites:
  
  # Por sector:
  Sector.tit <- Titulos.optim %>% filter(COD_SEC == 1)
  Sector.prop <- sum(0.8<colSums(Sector.tit[,-(1:8)])/val.portafolio.fut)
  if(0<Sector.prop){
    return(10000)
  }
  
  # Por emisor:
  Emisor.tit <- t(rowsum(as.matrix(Titulos.optim[,-(1:8)]), 
                         Titulos.optim$COD_EMI))/val.portafolio.fut
  Emisor.prop <- sum(0.1 < Emisor.tit)
  if(0<Emisor.prop){
    return(10000)
  }
  
  # Por administracion externa:
  Admin.tit <- Titulos.optim %>% filter(ADMIN != 1)
  Admin.prop <- sum(0.1<colSums(Admin.tit[,-(1:8)])/val.portafolio.fut)
  if(0<Admin.prop){
    return(10000)
  }
  
  # Por titulos emitidos en el extrangero:
  Extr.tit <- Titulos.optim %>% filter(!substring(COD_ISIN, 1, 2) %in% c("CR","00"))
  Extr.prop <- sum(0.25<(colSums(Extr.tit[,-(1:8)])/val.portafolio.fut))
  if(0<Extr.prop){
    return(10000)
  }
  
  # Por modalidad de inversion:
  Modal.tit <- t(rowsum(as.matrix(Titulos.optim[,-(1:8)]), 
                        Titulos.optim$COD_MOD_INV))/val.portafolio.fut
  Modal.prop1 <- sum(0.1 < Modal.tit[,c("DI","P2","E1")])
  Modal.prop2 <- sum(0.25 < Modal.tit[,c("P1")])
  if(0<(Modal.prop1+Modal.prop2)){
    return(10000)
  }
  
  # Valore del portafolio actual:
  val.portafolio.act <- sum(Titulos.optim$PRECIO_TEORICO_0*X)
  
  # CVAR del valor del portafolio futuro:
  CVAR.G <- mean(sort(val.portafolio.fut, decreasing = TRUE)[(cant.simu*(1-significancia)):cant.simu])
  
  # RORAC del portafolio:
  RORAC.G <- -((mean(val.portafolio.fut)-
                  val.portafolio.act)/(val.portafolio.act-CVAR.G))
  
  return(RORAC.G)
}

# Valores Iniciales:
val.inicial.op <- Portafolio.total %>% filter(COD_ENT==Entidades[which.max(roracs)])
val.inicial.op <- sum(val.inicial.op$PRECIO_TEORICO_0*val.inicial.op$VAL_FAC)
val.inicial.op <- (Portafolio.total %>% mutate(vec.inicial = ifelse(COD_ENT==Entidades[which.max(roracs)],
                                                                   VAL_FAC, 
                                                                   0)))$vec.inicial/val.inicial.op

# Optimizacion por SA:
VAL.FAC.SA <- optim_sa(fun = Port.Optim,
                       start = val.inicial.op,
                       trace = TRUE,
                       lower = rep(0, sum(nrow(BONOS.TF.RESULTADOS)+
                                            nrow(BONOS.TV.RESULTADOS)+
                                            nrow(ACCIONES.RESULTADOS))),
                       upper = rep(300, sum(nrow(BONOS.TF.RESULTADOS)+
                                              nrow(BONOS.TV.RESULTADOS)+
                                              nrow(ACCIONES.RESULTADOS))),
                       control = list(nlimit = Int.Port))

# Revision de Limites:
Titulos.optim.r <- cbind(Portafolio.total[,1:8], 
                         apply(as.matrix(Portafolio.total[,-(1:8)]),
                               2,
                               as.numeric)*VAL.FAC.SA$par*val.total.por)
# Valores futuros:
val.portafolio.fut.r <- colSums(Titulos.optim.r[,-(1:8)])

# Por sector:
Sector.tit.r <- Titulos.optim.r %>% filter(COD_SEC == 1)
Sector.prop.r <- 1-sum(0.8<colSums(Sector.tit.r[,-(1:8)]))/cant.simu

# Por emisor:
Emisor.tit.r <- t(rowsum(as.matrix(Titulos.optim.r[,-(1:8)]), 
                         Titulos.optim.r$COD_EMI))
Emisor.prop.r <- 1-sum(0.1 < Emisor.tit.r)/(cant.simu*dim(Emisor.tit.r)[2])

# Por administracion externa:
Admin.tit.r <- Titulos.optim.r %>% filter(ADMIN != 1)
Admin.prop.r <- 1-sum(0.1<colSums(Admin.tit.r[,-(1:8)]))/cant.simu

# Por titulos emitidos en el extrangero:
Extr.tit.r <- Titulos.optim.r %>% filter(!substring(COD_ISIN, 1, 2) %in% c("CR","00"))
Extr.prop.r <- 1-sum(0.25<(colSums(Extr.tit.r[,-(1:8)])))/cant.simu

# Por modalidad de inversion:
Modal.tit.r <- t(rowsum(as.matrix(Titulos.optim.r[,-(1:8)]), 
                        Titulos.optim.r$COD_MOD_INV))
Modal.prop1.r <- 1-sum(0.1 < Modal.tit.r[,c("DI","P2","E1")])/(cant.simu*3)
Modal.prop2.r <- 1-sum(0.25 < Modal.tit.r[,c("P1")])/cant.simu

# Datos de revision:
Datos.limites <- data.frame(Limite = c("Sector Publico",
                                       "Emisor", 
                                       "Administracion Externa",
                                       "Titulos Emitidos en el Extrangero",
                                       "Modalidad de inversion",
                                       "Modalidad de inversion"),
                            `Criterio de Limite` = c("No se debe obtener mas del 80%",
                                                     "Maximo de 10% por emisor",
                                                     "Maximo de 10% en administracion externa",
                                                     "Maximo de 25% en titulos Extrangeros",
                                                     "Maximo de 10% en DI, P2 y E1",
                                                     "Maximo de 25% en P1"),
                            Ajuste = c(Sector.prop.r,
                                       Emisor.prop.r, 
                                       Admin.prop.r,
                                       Extr.prop.r,
                                       Modal.prop1.r,
                                       Modal.prop2.r))

# Valor del portafolio actual optimo:
val.act.opt <- sum(Portafolio.total$PRECIO_TEORICO_0*VAL.FAC.SA$par*val.total.por)

# CVAR del portafolio futuro optimo:
CVAR.opti <- (val.act.opt-mean(sort(colSums(apply(as.matrix(Portafolio.total[,-(1:8)]),
                                                  2,
                                                  as.numeric)*
                                              VAL.FAC.SA$par*val.total.por), decreasing = TRUE)[(cant.simu*(1-significancia)):cant.simu]))

# Rendimiento optimo:
REND.opti <- (mean(colSums(apply(as.matrix(Portafolio.total[,-(1:8)]),
                                 2,
                                 as.numeric)*
                             VAL.FAC.SA$par*val.total.por))-
                val.act.opt)

# RORAC del portafolio futuro optimo:
RORAC.opti <- REND.opti/CVAR.opti

# Actualizamos los valores:
CVAR.opti <- CVAR.opti/val.act.opt
REND.opti <- REND.opti/val.act.opt


############################### Visualizacion ################################


# se crean los datos finales:
Resultados.rorac <- data.frame(Entidad = Entidades, Valor.Actual = valorhoy,
                               Rendimiento = rendimientos, 
                               Riesgo = cvars, RORAC = roracs) 

# Agregamos la informacion general:
Resultados.rorac <- rbind(Resultados.rorac, 
                          c("Benchmark",
                            val.portafolio.hoy, 
                            REND.Bench,
                            CVAR.Bench,
                            RORAC.Bench),
                          c("Óptimo",
                            val.act.opt,
                            REND.opti,
                            CVAR.opti,
                            RORAC.opti)) %>% 
  mutate(RORAC=100*round(as.numeric(RORAC),5), 
         Rendimiento=100*round(as.numeric(Rendimiento),5)) %>% 
  mutate(Valor.Actual = as.numeric(Valor.Actual)/val.portafolio.hoy,
         Rendimiento = as.numeric(Rendimiento),
         Riesgo = as.numeric(Riesgo)) %>%
  arrange(RORAC)

# visualizamos los resultados:
graf.rorac <- ggplot(Resultados.rorac %>% 
                       filter(!Entidad %in% c("Benchmark", "Óptimo"))) +
  geom_point(aes(x = seq(0,0.2, 0.2/(nrow(Resultados.rorac)-3)),
                 y=RORAC,color=Entidad), size=3) +
  geom_text(data = Resultados.rorac %>%
              filter(Entidad %in% c("Benchmark")), 
            aes(x = 0.19, y=RORAC,label = Entidad), color = "red",
            nudge_x = 0.01, nudge_y = 0.9) +
  geom_text(data = Resultados.rorac %>%
              filter(Entidad %in% c("Óptimo")), 
            aes(x = 0, y=RORAC,label = Entidad), color = "steelblue",
            nudge_x = 0.01, nudge_y = 0.9) +
  xlim(0,0.2) +
  theme_bw() +
  ylim(as.numeric(min(Resultados.rorac$RORAC)*(1+0.5)),
       as.numeric(max(Resultados.rorac$RORAC))*(1+0.5)) +
  geom_hline(yintercept= 100*RORAC.Bench, 
             linetype="dashed", color = "red") +
  geom_hline(yintercept= 100*RORAC.opti, 
             linetype="dashed", color = "steelblue") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  ylab("RORAC (%)") 
graf.rorac

# Visualizamos la Frontera Eficiente:
graf.efi <- ggplot(Resultados.rorac %>% 
                     filter(!Entidad %in% c("Benchmark", "Óptimo")), 
                   aes(x = Riesgo, y = Rendimiento)) +
  geom_point(aes(color = Entidad), size=3) +
theme(axis.text.x=element_blank(),
      axis.ticks.x=element_blank()) +
  ylab("Rendimiento (%)") +
  xlab("Riesgo") +
  geom_text(data = Resultados.rorac %>% filter(Entidad == "Benchmark"),
            aes(x = Riesgo, y=Rendimiento,label = Entidad),
            color = "red", nudge_y = 11) +
  geom_point(data = Resultados.rorac %>% filter(Entidad == "Benchmark"),
            aes(x = Riesgo, y=Rendimiento)) +
  geom_text(data = Resultados.rorac %>% filter(Entidad == "Óptimo"),
            aes(x = Riesgo, y=Rendimiento,label = Entidad),
            color = "steelblue", nudge_y = 11) +
  geom_point(data = Resultados.rorac %>% filter(Entidad == "Óptimo"),
             aes(x = Riesgo, y=Rendimiento))
graf.efi


##############################################################################


                       ###########################
                       ###                     ###
                       ###        FIN          ###
                       ###                     ###
                       ###########################