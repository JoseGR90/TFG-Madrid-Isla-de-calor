#https://wiki.openstreetmap.org/wiki/Map_features#Others

#cargamos las librerías
rm(list=ls())
library(ggplot2)
library(rgeos)
library(maptools)
library(sp)
library(ggmap)
library(rgdal)
library(readxl)
library(tidyverse)
library(osmdata)
library(sf)
library(readxl)



#######################CODIGO DE ESTACIONES
estacionesControl <- read_excel("Meteo/Estaciones_control_datos_meteorologicos.xls")
estCon<-estacionesControl[,c(2,3,20,21,22,23)]
estCon$LONGITUD<-as.numeric(estCon$LONGITUD)
estCon$LATITUD<-as.numeric(estCon$LATITUD)
aprox1 <- read.csv("Mapas/Clustering/aprox1.csv",sep=";")
aprox2 <- read.csv("Mapas/Clustering/aprox2.csv",sep=";")
aprox3 <- read.csv("Mapas/Clustering/aprox3.csv",sep=";")
asocNombresNum<-read.csv("Meteo/estacion_distrito.csv",sep=";", dec=",")
x<-rep(NA,nrow(aprox1))
y<-rep(NA,nrow(aprox1))
aprox1<-cbind(aprox1, x, y)
for(myrow in 1:nrow(aprox1)){#rellenamos coordenadas
  myx<-estCon[estCon$CÓDIGO_CORTO==aprox1[myrow,]$numEst,]$LONGITUD
  myy<-estCon[estCon$CÓDIGO_CORTO==aprox1[myrow,]$numEst,]$LATITUD
  aprox1[myrow,]$x<-myx
  aprox1[myrow,]$y<-myy
}

x<-rep(NA,nrow(aprox2))
y<-rep(NA,nrow(aprox2))
aprox2<-cbind(aprox2, x, y)
for(myrow in 1:nrow(aprox2)){#rellenamos coordenadas
  myx<-estCon[estCon$CÓDIGO_CORTO==aprox2[myrow,]$numEst,]$LONGITUD
  myy<-estCon[estCon$CÓDIGO_CORTO==aprox2[myrow,]$numEst,]$LATITUD
  aprox2[myrow,]$x<-myx
  aprox2[myrow,]$y<-myy
}

x<-rep(NA,nrow(aprox3))
y<-rep(NA,nrow(aprox3))
aprox3<-cbind(aprox3, x, y)
for(myrow in 1:nrow(aprox3)){#rellenamos coordenadas
  myx<-estCon[estCon$CÓDIGO_CORTO==aprox3[myrow,]$numEst,]$LONGITUD
  myy<-estCon[estCon$CÓDIGO_CORTO==aprox3[myrow,]$numEst,]$LATITUD
  aprox3[myrow,]$x<-myx
  aprox3[myrow,]$y<-myy
}

###Todas las estaciones
Estacion<-c(102, 103, 106, 107, 109, 110, 112, 8, 24, 35, 36, 38, 54, 56, 58, 59)
mimapa<-NULL
Nombre<-rep(NA,length(Estacion))
x<-rep(NA,length(Estacion))
y<-rep(NA,length(Estacion))
mimapa<-data.frame(cbind(Nombre, Estacion, x, y))
for(myrow in 1:nrow(mimapa)){#rellenamos coordenadas
  myx<-estCon[estCon$CÓDIGO_CORTO==mimapa[myrow,]$Estacion,]$LONGITUD
  myy<-estCon[estCon$CÓDIGO_CORTO==mimapa[myrow,]$Estacion,]$LATITUD
  mimapa[myrow,]$x<-myx
  mimapa[myrow,]$y<-myy
  mimapa[myrow,]$Nombre<-asocNombresNum[asocNombresNum$ESTACION==mimapa[myrow,]$Estacion,]$NombreEstacion
}
###############################################################

#Ponemos un background en el que se vea Madrid de una manera adecuada
mad_map <- get_stamenmap(getbb("Madrid"), zoom = 12, maptype = "terrain")



ggmap(mad_map)+geom_point(data=mimapa, aes(x = x , y = y),colour="red", size=3)+
  geom_text(data=mimapa,aes(x = x , y = y,label = Nombre), vjust = 1.5, hjust = 0.5, size=2.5)


#Le pasamos los resultados de clustering al mapa
ggmap(mad_map)+geom_point(data=aprox1, aes(x = x , y = y),colour = aprox1$grupoKmeans, size=3)
ggmap(mad_map)+geom_point(data=aprox1, aes(x = x , y = y),colour = aprox1$grupoJerarquico, size=3)
ggmap(mad_map)+geom_point(data=aprox2, aes(x = x , y = y),colour = aprox2$grupoKmeans, size=3)
ggmap(mad_map)+geom_point(data=aprox2, aes(x = x , y = y),colour = aprox2$grupoJerarquico, size=3)
ggmap(mad_map)+geom_point(data=aprox3, aes(x = x , y = y),colour = aprox3$grupoKmeans, size=3)
ggmap(mad_map)+geom_point(data=aprox3, aes(x = x , y = y),colour = aprox3$grupoJerarquico, size=3)


Estacion<-c(102, 103, 106, 107, 109, 110, 112, 8, 24, 35, 36, 38, 54, 56, 58, 59)
mimapa<-NULL
Nombre<-rep(NA,length(Estacion))
x<-rep(NA,length(Estacion))
y<-rep(NA,length(Estacion))
mimapa<-data.frame(cbind(Nombre, Estacion, x, y))
for(myrow in 1:nrow(mimapa)){#rellenamos coordenadas
  myx<-estCon[estCon$CÓDIGO_CORTO==mimapa[myrow,]$Estacion,]$COORDENADA_X_ETRS89
  myy<-estCon[estCon$CÓDIGO_CORTO==mimapa[myrow,]$Estacion,]$COORDENADA_Y_ETRS89
  mimapa[myrow,]$x<-myx
  mimapa[myrow,]$y<-myy
  mimapa[myrow,]$Nombre<-asocNombresNum[asocNombresNum$ESTACION==mimapa[myrow,]$Estacion,]$NombreEstacion
}

ED50 <- CRS(paste("+proj=utm +zone=30 +ellps=intl +units=m +no_defs"))
barriosMadrid <- readShapePoly("Mapas/Barrios/200001909.shp",proj4string = ED50)

coordinates(mimapa) <- c("x","y")
proj4string(mimapa)<-ED50
plot(barriosMadrid)
plot(mimapa, pch=20, cex=1,col="red", add=TRUE)
