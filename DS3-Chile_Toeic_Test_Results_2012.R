## Analisis de Datos Toeic en base a Becas de Ingles CORFO Chile 2012
## Daniel Sepulveda - 30 Marzo 2014
## Data set se obiene de : http://datos.gob.cl/datasets/ver/4963
## Cargamos los Datos a R
setwd("~/R")
test <- read.csv("DS3.csv",sep=";")
## Analizamos la estructura:
str(test)
##'data.frame':  7567 obs. of  7 variables:
##  $ A?o                        : int  2012 2012 2012 2012 2012 2012 2012 2012 2012 2012 ...
## $ region_curso               : int  9 9 2 2 2 2 2 2 2 2 ...
## $ Region_domicilio_registrado: Factor w/ 16 levels "1","10","11",..: 15 15 8 8 8 8 5 8 8 8 ...
## $ localidad                  : Factor w/ 31 levels "Angol","Antofagasta",..: 1 1 2 2 2 2 2 2 2 2 ...
## $ tipo_beca                  : Factor w/ 2 levels "100 Horas","200 Horas": 1 1 1 1 1 1 1 1 1 1 ...
## $ Toeic_Inicial              : int  230 265 400 395 485 660 620 675 595 535 ...
## $ Toeic_Final                : int  0 0 0 0 0 0 0 0 0 0 ...

## Algunas visualizaciones gr?ficas
hist(test$region_curso, col="steelblue")
hist(test$Toeic_Final)
hist(test$Toeic_Inicial)

## Generamos campo $delta para medir diferencia en desempe?o
test$delta <- test$Toeic_Final - test$Toeic_Inicial
## Analizamos graficamente el campo generado
hist(test$delta)
hist(test$region[test$delta<0])
hist(test$region[test$delta>0])
hist(test$region[test$delta=0])

## Analizamos las localidades con peor desempe?o
table(test$localidad[test$delta<=0]) # primero en numero absoluto de casos
table(test$localidad[test$delta<=0])/table(test$localidad) #Luego en fraccion a los casos totales para esa localidad
## Finalmente lo expresamos en porcentaje y en orden decreciente para identificar las peores localidades  
sort(format(table(test$localidad[test$delta<=0])/table(test$localidad)*100,digits=2),decreasing=TRUE)
## y las mejores:
sort(format(table(test$localidad[test$delta>0])/table(test$localidad)*100,digits=2),decreasing=TRUE)

## Podemos analizarlo por tipo de Test. Consideramos los $Toeic_Final=0 como sin antecedentes finales de desempe?o
## Para los cursos de 100 horas
sort(format(table(test$localidad[test$delta<=0 & test$tipo_beca=="100 Horas" & test$Toeic_Final!=0])/table(test$localidad)*100,digits=2),decreasing=TRUE)
## Para los cursos de 200 Horas
sort(format(table(test$localidad[test$delta<=0 & test$tipo_beca=="200 Horas"& test$Toeic_Final!=0])/table(test$localidad)*100,digits=2),decreasing=TRUE)

## Identificando las localidades con mayor cantidad de falta de datos de desempe?o final (medida de desercion)
sort(format(table(test$localidad[test$Toeic_Final==0])/table(test$localidad)*100,digits=2),decreasing=TRUE)
sort(format(table(test$localidad[test$tipo_beca=="100 Horas" & test$Toeic_Final==0])/table(test$localidad)*100,digits=2),decreasing=TRUE)
sort(format(table(test$localidad[test$tipo_beca=="200 Horas" & test$Toeic_Final==0])/table(test$localidad)*100,digits=2),decreasing=TRUE)

