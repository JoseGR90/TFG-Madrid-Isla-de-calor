#Clustering
rm(list=ls())
library(imputeR)
#Cargamos los datos que hemos limpiado y los cuales hemos dado formato.
allData<-read.csv("Datos Finales/datos21_byDay.csv",sep=";",dec=",")
meteo21<-read.csv("Meteo/Diario/meteoDiario21_byDay.csv",sep=";", dec=",")
#KMeans
#Puesto que desde el portal de datos del ayuntamiento hay numerosos valores nulos y este algoritmo
#requiere que sean datos cuantitativos, es decir no puden haber NA, hemos decidido 
#imputar datos la mediana de los datos. 
#https://www.uv.es/webgid/Descriptiva/23_valores_faltantes.html
#Problemas-> como faltan una cantidad increible de datos, creo que es mejor idea hacer el kmeans
#por cada dataframe, es decir, un kmeans por contaminacion y otro por meteorologia.
for(i in 6:ncol(meteo21)) {
  meteo21[,i][is.na(meteo21[,i])]<-median(meteo21[,i],na.rm=TRUE)
}
#Una vez que tenemos rellenado el dataframe al completo escalamos los datos
myKmeans<-data.frame(scale(meteo21[,6:12]))
library(cluster)
library(factoextra)
#Ahora vamos a calcular el numero optimo de clusters para el kMeans
#Para ello recurrimos al diagrama del codo.
fviz_nbclust(myKmeans, kmeans, method = "wss")#Tarda en ejecutarse; el "codo" se encuentra en 3 clusters, por lo que lo ejecutamos tal que asi.
set.seed(123)
#Analizar si esta bien con 3 clusters, 2 grupos muy grandes y uno muy pequeño.
kmeansResults<-kmeans(myKmeans, centers=3)
#Inercia entre grupos, nos dice la varianza entre los grupos, cuanto mayor mejor es la clasificación. Valor a maximizar.
kmeansResults$betweenss
#Inercia intragrupos, nos indica como de agrupados esta cada punto dentro de cada grupo, cuanto menor, mejor.
kmeansResults$withinss
#Inercia total intragrupos, es un valor a minimizar, cuanto menor es mejor.
kmeansResults$tot.withinss

fecha<-meteo21$Fecha
estacion<-meteo21$Estacion
fechEst<-paste(meteo21$Fecha,meteo21$Estacion)
grupo<-kmeansResults$cluster
datosRepresentacion<-data.frame(fechEst, grupo)

library(ggplot2)
#Se ve basatante mal.
ggplot(datosRepresentacion)+
  geom_point(mapping=aes(x=fechEst, y=grupo), color=grupo, size=1)






# AGRUPACION DE LOS DATOS POR Estaciones 
#agrupamos por estaciones, se obtienen los casos totales
library(dplyr)
names(meteo21)
names(meteo21)[6]<-"Radiacion"
names(meteo21)[7]<-"Viento"
names(meteo21)[8]<-"Temperatura"
names(meteo21)[9]<-"Humedad"
names(meteo21)[10]<-"Presion"
names(meteo21)[11]<-"Solar"
names(meteo21)[12]<-"Precipitacion"

Estacioncontaminacion<-group_by(meteo21, Estacion)
Estacioncontaminacion<-summarize(Estacioncontaminacion, count=n(),
                                 Radiacion=median(Radiacion, na.rm=T),
                                 Viento=median(Viento, na.rm=T),
                                 Temperatura=median(Temperatura, na.rm=T),
                                 Humedad=median(Humedad, na.rm=T),
                                 Presion=median(Presion,na.rm = T),
                                 Solar=median(Solar, na.rm=T),
                                 Precipitacion=median(Precipitacion,na.rm = T))




#QUINTA PARTE ***CLUSTERING MST

porEstacions<-hclust(dist(Estacioncontaminacion,method="euclidean"),method="single")
plot(porEstacions)
plot(hclust(dist(Estacioncontaminacion,method="euclidean"),method="single"))


