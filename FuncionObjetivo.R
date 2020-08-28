#              
#                                    Consultoría RORAC-SUPEN
#      Función a optimizar para obtener la curva cero cupón mediante el Modelo de Nelson-Siegel

# Autores:
# Alexa Sánchez
# Laura Campos
# Isaac Z. Arias

# El siguiente módulo representa la función de error cuadrático medio que se debe minimizar para 
# obtener los parámetros óptimos de la curva cero cupón para cada mes según Modelo de Nelson-Siegel.


# Paquetes necesarios:
# library(dplyr)

# Decirle a Isaac que no elimine la variable de último pago de intereses
# tener cuidado con la moneda de liquidacion 
Tau <- function(t,T){
  as.double(difftime(lubridate::ymd(T),
                     lubridate::ymd(t),
                     units = "days"))/360 
}

NelsonSieguel<-function(b0, b1, b2, n1, t, T){
  
  delta <- b0 + b1 * ((1-exp(-Tau(t, T)))/(Tau(t,T)/n1)) + 
    b2 * (((1-exp(-Tau(t, T)))/(Tau(t,T)/n1)) - exp(-Tau(t,T)/n1))
  return(delta)
}

FactoresDescuento <- function(t,T,B0,B1,B2,n1){
  exp(-Tau(t,T)*NelsonSiegel(B0,B1,B2,n1,t,T))
}

ValorarBonos <- function(FechaUltimoPagoIntereses,FechaOperacion,FechaVencimiento,Periodicidad,TasaFacial,B0,B1,B2,n1) {
  if(Periodicidad==0){
    Precio<-FactoresDescuento(FechaOperacion,FechaVencimiento,B0,B1,B2,n1)
  }
  else{
    FechasPago<-seq.Date(from = FechaUltimoPagoIntereses,to = FechaVencimiento,by = paste(as.character(12/Periodicidad),"months",sep=" "))
    FechasPendientes<-FechasPago[-1]
    FactoresDesc<-Vectorize(FactoresDescuento)(FechaOperacion,FechasPendientes,B0,B1,B2,n1) 
    
    Precio<-c(rep(TasaFacial,length(FechasPendientes)-1),1+TasaFacial)*FactoresDescuento
    return(sum(Precio))
  }
}


# El Data Frame de BoletasMes debe contener las siguientes variables (para una única fecha de operación y una moneda de liquidación):
# Precio
# Tasa facial
# Ultimo pago de interes 
# Fecha vencimiento
# Fecha de operación


FuncionObjetivo <- function(BoletasMes,B0,B1,B2,n1,alpha){
BoletasBonos<-BoletasMes %>% filter(titulo=='BONOS',Nemotecnico.del.Emisor %in% c("BCCR","G"),
                                    !Nemotecnico.del.instrumento %in% c("bemv", "tp$", "tpras", "tptba", "TUDES", "tudes", "bemud", "TPTBA"))

## Además los filtros de la Tabla de Isaac

BoletasBonos<-BoletasBonos %>% mutate(PrecioTeorico=ValorarBonos(Fecha.Ultimo.Pago.Intereses,Fecha.de.Operacion,Fecha.de.Vencimiento,
                                                                Periodicidad,Tasa.facial,B0,B1,B2,n1), Ponderador=exp(-Tau(Fecha.de.Operacion,Fecha.de.Vencimiento)*alpha)) %>% 
                              mutate(Ponderador=Ponderador/sum(Ponderador)) %>% mutate( Error=Ponderador*(PrecioTeorico-Precio)^2)
                              #CORROBORAR QUE EL PONDERADOR DEBA SUMAR 1   
                              
ErrorTotal<-sum(Boletas$Error)
return(ErrorTotal)
}
