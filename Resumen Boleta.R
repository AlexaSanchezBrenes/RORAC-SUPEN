#Recibe un archivo txt o csv con la boleta de un periodo dado

resumen.boleta<-function(archivo){
  
tbl<-lee.boleta(archivo) %>% 
       filter(Plazo.de.la.operacion != "A PLAZO")
  
fecha.operacion<-tbl$Fecha.de.Operacion[1]
  
resumen.emisor<-tbl %>% 
             group_by(Nemotecnico.del.Emisor,
                     Dias.de.vencimiento.del.Instrumento,
                     Periodicidad,
                     Tasa.facial,
                     Moneda.del.instrumento) %>% 
             summarise(Cantidad.instrumentos = n()) %>% 
             arrange(desc(Dias.de.vencimiento.del.Instrumento)) 
  
moneda.emision<-tbl %>% 
                group_by(Moneda.del.instrumento) %>% 
                summarise(Conteo = n())

moneda.liquidacion<-tbl %>% 
                group_by(Moneda.de.liquidacion) %>% 
                summarise(Conteo = n())

periodicidad<- tbl %>% 
               group_by(Periodicidad) %>% 
                summarise(Conteo = n())

return(list(Fecha.Operacion.Boleta =  fecha.operacion,
              ResumenEmisor = resumen.emisor,
              Moneda.Emision = moneda.emision,
              Moneda.Liquidacion = moneda.liquidacion, 
              Periodicidad = periodicidad))
}
pb<-resumen.boleta(archivo)
