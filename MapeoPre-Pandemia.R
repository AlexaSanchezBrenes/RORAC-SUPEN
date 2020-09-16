#              
#                    Consultoría RORAC-SUPEN 
#                  Mapeo de Datos Pre-Pandemia 

# Autores:
# Alexa Sánchez
# Laura Campos
# Isaac Z. Arias

# El siguiente módulo realiza un mapeo estadístico sobre los datos 
# obtenidos por la Bolsa Nacional de Valores sobre las transacciones 
# de títulos en fechas anteriores a la pandemia.

# Paquete necesarios:
library(readxl)
library(dplyr)

# Se cargan los datos:
Datos.Curva <- read_excel("Datos Curva Soberana (2009-2019)(1914).xlsx") %>% 
  select(`Fecha de Operación`,`Nemotécnico del Emisor`,`Mercado de Negociación`,
         `Número de Contrato`,`Fecha de Vencimiento`,`Nemotécnico del instrumento`,
         `Fecha de Vencimiento`) %>% 
  filter(`Mercado de Negociación` == "MERCADO SECUNDARIO") %>%
  mutate(titulo = ifelse(is.na(`Fecha de Vencimiento`), "ACCIONES O FONDOS", "BONOS"))

#-------------------#
# Analisis por día: #
#-------------------#

# Por Fecha de Operación:

# Cantidad de titulos por día:
dia.op.titulos <- Datos.Curva %>% group_by(`Fecha de Operación`, titulo) %>% 
  summarise(cantidad = n(), .groups = "keep") %>% ungroup()

# Titulos promedio diario:
dia.op.prom <- dia.op.titulos %>% group_by(titulo) %>% summarise(promedio = mean(cantidad), .groups = "keep") %>% ungroup()

# Cantidad de títulos por emisores e instrumento al día:
dia.op.nemo <- Datos.Curva %>% 
  group_by(titulo,`Fecha de Operación`,`Nemotécnico del Emisor`,`Nemotécnico del instrumento`) %>% 
  summarise(cantidad = n(), .groups = "keep") %>% 
  ungroup()

# Cantidad de títulos por instrumento al día:
dia.op.ins <- dia.op.nemo %>% 
  group_by(titulo,`Fecha de Operación`,`Nemotécnico del instrumento`) %>% 
  summarise(cantidad = sum(cantidad), .groups = "keep") %>% 
  ungroup()

# Cantidad de títulos por emisor al día:
dia.op.emi <- dia.op.nemo %>% 
  group_by(titulo,`Fecha de Operación`,`Nemotécnico del Emisor`) %>% 
  summarise(cantidad = sum(cantidad), .groups = "keep") %>% 
  ungroup()

# Promedio diario de títulos por emisor:
dia.op.emi.prom <- dia.op.emi %>% group_by(titulo,`Nemotécnico del Emisor`) %>% 
  summarise(promedio = mean(cantidad), .groups = "keep") %>% 
  ungroup()

# Promedio diario de títulos por instrumento:
dia.op.ins.prom <- dia.op.ins %>% group_by(titulo,`Nemotécnico del instrumento`) %>% 
  summarise(promedio = mean(cantidad), .groups = "keep") %>% 
  ungroup()

# Por Fecha de Vencimiento:

# Cantidad de bonos por día:
dia.ve.titulos <- Datos.Curva %>% filter(titulo == "BONOS") %>% group_by(`Fecha de Vencimiento`) %>% 
  summarise(cantidad = n(), .groups = "keep") %>% ungroup()

# Bonos promedio diario:
dia.ve.prom <- dia.ve.titulos %>% summarise(promedio = mean(cantidad), .groups = "keep")

# Cantidad de bonos por emisores e instrumentos al día:
dia.ve.nemo <- Datos.Curva %>% filter(titulo == "BONOS") %>% 
  group_by(`Fecha de Vencimiento`,`Nemotécnico del Emisor`,`Nemotécnico del instrumento`) %>% 
  summarise(cantidad = n(), .groups = "keep") %>% 
  ungroup()

# Cantidad de bonos por instrumento al día:
dia.ve.ins <- dia.ve.nemo %>% 
  group_by(`Fecha de Vencimiento`,`Nemotécnico del instrumento`) %>% 
  summarise(cantidad = sum(cantidad), .groups = "keep") %>% 
  ungroup()

# Cantidad de bonos por emisor al día:
dia.ve.emi <- dia.ve.nemo %>% 
  group_by(`Fecha de Vencimiento`,`Nemotécnico del Emisor`) %>% 
  summarise(cantidad = sum(cantidad), .groups = "keep") %>% 
  ungroup()

# Promedio diario de bonos por emisor:
dia.ve.emi.prom <- dia.ve.emi %>% group_by(`Nemotécnico del Emisor`) %>% 
  summarise(promedio = mean(cantidad), .groups = "keep") %>% 
  ungroup()

# Promedio diario de bonos por instrumento:
dia.ve.ins.prom <- dia.ve.ins %>% group_by(`Nemotécnico del instrumento`) %>% 
  summarise(promedio = mean(cantidad), .groups = "keep") %>% 
  ungroup()

#----------------------#
# Analisis por semana: #
#----------------------#

# Por Fecha de Operación:

# Cantidad de titulos por semana:
sem.op.titulos <- Datos.Curva %>% group_by(semana = cut(`Fecha de Operación`, "week"), titulo) %>% 
  summarise(cantidad = n(), .groups = "keep") %>% ungroup()

# Titulos promedio semanal:
sem.op.prom <- sem.op.titulos %>% group_by(titulo) %>% 
  summarise(promedio = mean(cantidad), .groups = "keep") %>% ungroup()

# Cantidad de títulos por emisores e instrumento al mes:
sem.op.nemo <- Datos.Curva %>% 
  group_by(titulo,semana = cut(`Fecha de Operación`, "week"),`Nemotécnico del Emisor`,`Nemotécnico del instrumento`) %>% 
  summarise(cantidad = n(), .groups = "keep") %>% 
  ungroup()

# Cantidad de títulos por instrumento al mes:
sem.op.ins <- sem.op.nemo %>% 
  group_by(titulo,semana,`Nemotécnico del instrumento`) %>% 
  summarise(cantidad = sum(cantidad), .groups = "keep") %>% 
  ungroup()

# Cantidad de títulos por emisor a la semana:
sem.op.emi <- sem.op.nemo %>% 
  group_by(titulo,semana,`Nemotécnico del Emisor`) %>% 
  summarise(cantidad = sum(cantidad), .groups = "keep") %>% 
  ungroup()

# Promedio semanal de títulos por emisor:
sem.op.emi.prom <- sem.op.emi %>% group_by(titulo,`Nemotécnico del Emisor`) %>% 
  summarise(promedio = mean(cantidad), .groups = "keep") %>% 
  ungroup()

# Promedio semanal de títulos por instrumento:
sem.op.ins.prom <- sem.op.ins %>% group_by(titulo,`Nemotécnico del instrumento`) %>% 
  summarise(promedio = mean(cantidad), .groups = "keep") %>% 
  ungroup()

# Por Fecha de Vencimiento:

# Cantidad de bonos por semana:
sem.ve.titulos <- Datos.Curva %>% filter(titulo == "BONOS") %>% group_by(semana = cut(`Fecha de Vencimiento`, "week")) %>% 
  summarise(cantidad = n(), .groups = "keep") %>% ungroup()

# Bonos promedio semanal:
sem.ve.prom <- sem.ve.titulos %>% summarise(promedio = mean(cantidad), .groups = "keep")

# Cantidad de bonos por emisores e instrumentos a al semana:
sem.ve.nemo <- Datos.Curva %>% filter(titulo == "BONOS") %>% 
  group_by(semana = cut(`Fecha de Vencimiento`, "week"),`Nemotécnico del Emisor`,`Nemotécnico del instrumento`) %>% 
  summarise(cantidad = n(), .groups = "keep") %>% 
  ungroup()

# Cantidad de bonos por instrumento a la semana:
sem.ve.ins <- sem.ve.nemo %>% 
  group_by(semana,`Nemotécnico del instrumento`) %>% 
  summarise(cantidad = sum(cantidad), .groups = "keep") %>% 
  ungroup()

# Cantidad de bonos por emisor a la semana:
sem.ve.emi <- sem.ve.nemo %>% 
  group_by(semana,`Nemotécnico del Emisor`) %>% 
  summarise(cantidad = sum(cantidad), .groups = "keep") %>% 
  ungroup()

# Promedio semanal de bonos por emisor:
sem.ve.emi.prom <- sem.ve.emi %>% group_by(`Nemotécnico del Emisor`) %>% 
  summarise(promedio = mean(cantidad), .groups = "keep") %>% 
  ungroup()

# Promedio semanal de bonos por instrumento:
sem.ve.ins.prom <- sem.ve.ins %>% group_by(`Nemotécnico del instrumento`) %>% 
  summarise(promedio = mean(cantidad), .groups = "keep") %>% 
  ungroup()

#-------------------#
# Analisis por mes: #
#-------------------#

# Por Fecha de Operación:

# Cantidad de titulos por mes:
mes.op.titulos <- Datos.Curva %>% group_by(mes = cut(`Fecha de Operación`, "month"), titulo) %>% 
  summarise(cantidad = n(), .groups = "keep") %>% ungroup()

# Titulos promedio mensual:
mes.op.prom <- mes.op.titulos %>% group_by(titulo) %>% 
  summarise(promedio = mean(cantidad), .groups = "keep") %>% ungroup()

# Cantidad de títulos por emisores e instrumento al mes:
mes.op.nemo <- Datos.Curva %>% 
  group_by(titulo,mes = cut(`Fecha de Operación`, "month"),`Nemotécnico del Emisor`,`Nemotécnico del instrumento`) %>% 
  summarise(cantidad = n(), .groups = "keep") %>% 
  ungroup()

# Cantidad de títulos por instrumento al mes:
mes.op.ins <- mes.op.nemo %>% 
  group_by(titulo,mes,`Nemotécnico del instrumento`) %>% 
  summarise(cantidad = sum(cantidad), .groups = "keep") %>% 
  ungroup()

# Cantidad de títulos por emisor al mes:
mes.op.emi <- mes.op.nemo %>% 
  group_by(titulo,mes,`Nemotécnico del Emisor`) %>% 
  summarise(cantidad = sum(cantidad), .groups = "keep") %>% 
  ungroup()

# Promedio mensual de títulos por emisor:
mes.op.emi.prom <- mes.op.emi %>% group_by(titulo,`Nemotécnico del Emisor`) %>% 
  summarise(promedio = mean(cantidad), .groups = "keep") %>% 
  ungroup()

# Promedio mensual de títulos por instrumento:
mes.op.ins.prom <- mes.op.ins %>% group_by(titulo,`Nemotécnico del instrumento`) %>% 
  summarise(promedio = mean(cantidad), .groups = "keep") %>% 
  ungroup

# Segregamos por los nemotécnicos que necesitamos:
mes.op.ins.seg <- mes.op.nemo %>% filter(as.Date("2014-12-01")<as.Date(mes)) %>%
  mutate(ano = year(as.Date(mes)), mes = month(as.Date(mes))) %>% 
  filter(`Nemotécnico del Emisor` %in% c("BCCR","G"), 
         !`Nemotécnico del instrumento` %in% c("bemv", "tp$", "tpras", "tptba", "TUDES", "tudes", "bemud", "TPTBA")) %>% 
  group_by(ano, mes) %>% summarise(cantidad = sum(cantidad), .groups = "keep") %>% ungroup() 

# Mediana y Promedio mensual:
media.op.mes <- mean(mes.op.ins.seg$cantidad) 
media.op.mes
mediana.op.mes <- median(mes.op.ins.seg$cantidad)
mediana.op.mes

# Por Fecha de Vencimiento:

# Cantidad de bonos por mes:
mes.ve.titulos <- Datos.Curva %>% filter(titulo == "BONOS") %>% group_by(mes = cut(`Fecha de Vencimiento`, "month")) %>% 
  summarise(cantidad = n(), .groups = "keep") %>% ungroup()

# Bonos promedio mensual:
mes.ve.prom <- mes.ve.titulos %>% summarise(promedio = mean(cantidad), .groups = "keep")

# Cantidad de bonos por emisores e instrumentos al mes:
mes.ve.nemo <- Datos.Curva %>% filter(titulo == "BONOS") %>% 
  group_by(mes = cut(`Fecha de Vencimiento`, "month"),`Nemotécnico del Emisor`,`Nemotécnico del instrumento`) %>% 
  summarise(cantidad = n(), .groups = "keep") %>% 
  ungroup()

# Cantidad de bonos por instrumento al mes:
mes.ve.ins <- mes.ve.nemo %>% 
  group_by(mes,`Nemotécnico del instrumento`) %>% 
  summarise(cantidad = sum(cantidad), .groups = "keep") %>% 
  ungroup()

# Cantidad de bonos por emisor al mes:
mes.ve.emi <- mes.ve.nemo %>% 
  group_by(mes,`Nemotécnico del Emisor`) %>% 
  summarise(cantidad = sum(cantidad), .groups = "keep") %>% 
  ungroup()

# Promedio mensual de bonos por emisor:
mes.ve.emi.prom <- mes.ve.emi %>% group_by(`Nemotécnico del Emisor`) %>% 
  summarise(promedio = mean(cantidad), .groups = "keep") %>% 
  ungroup()

# Promedio mensual de bonos por instrumento:
mes.ve.ins.prom <- mes.ve.ins %>% group_by(`Nemotécnico del instrumento`) %>% 
  summarise(promedio = mean(cantidad), .groups = "keep") %>% 
  ungroup()

# Segregamos por los nemotécnicos que necesitamos:
mes.ve.ins.seg <- mes.ve.nemo %>% filter(as.Date("2014-12-01")<as.Date(mes)) %>%
  mutate(ano = year(as.Date(mes)), mes = month(as.Date(mes))) %>% 
  filter(`Nemotécnico del Emisor` %in% c("BCCR","G"), 
         !`Nemotécnico del instrumento` %in% c("bemv", "tp$", "tpras", "tptba", "TUDES", "tudes", "bemud", "TPTBA")) %>% 
  group_by(ano, mes) %>% summarise(cantidad = sum(cantidad), .groups = "keep") %>% ungroup() 

# Mediana y Promedio mensual:
media.ve.mes <- mean(mes.ve.ins.seg$cantidad) 
media.ve.mes
mediana.ve.mes <- median(mes.ve.ins.seg$cantidad)
mediana.ve.mes

# Segregamos por los nemotécnicos que necesitamos:
op.seg.pre <- Datos.Curva %>% mutate(mes.op = month(as.Date(`Fecha de Operación`))) %>% 
  filter(`Nemotécnico del Emisor` %in% c("BCCR","G"), 
         !`Nemotécnico del instrumento` %in% c("bemv", "tp$", "tpras", "tptba", "TUDES", "tudes", "bemud", "TPTBA")) %>% 
  group_by(ano.op = year(as.Date(`Fecha de Operación`)),mes.op = month(as.Date(`Fecha de Operación`)), ano.ve = year(`Fecha de Vencimiento`), mes.ve =month(`Fecha de Vencimiento`)) %>% summarise(cantidad = n(), .groups = "keep") %>% ungroup() 
