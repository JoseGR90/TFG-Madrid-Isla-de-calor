#install.packages(c("rgeos", "sp", "maptools", "RColorBrewer", "classInt", "rgdal", "ggmap", "googleVis"))
rm(list=ls())
library(rgeos)
library(maptools)
library(sp)
library(ggmap)
library(rgdal)
library(readxl)
asocNombresNum<-read.csv("Meteo/estacion_distrito.csv",sep=";", dec=",")
estacionesControl <- read_excel("Meteo/Estaciones_control_datos_meteorologicos.xls")
estCon<-estacionesControl[,c(2,3, 20, 21)]
estCon$COORDENADA_X_ETRS89<-as.numeric(estCon$COORDENADA_X_ETRS89)
estCon$COORDENADA_Y_ETRS89<-as.numeric(estCon$COORDENADA_Y_ETRS89)
aprox1 <- read.csv("Mapas/Clustering/aprox1.csv",sep=";")
aprox2 <- read.csv("Mapas/Clustering/aprox2.csv",sep=";")
aprox3 <- read.csv("Mapas/Clustering/aprox3.csv",sep=";")
x<-rep(NA,8)
y<-rep(NA,8)
aprox1<-cbind(aprox1, x, y)
for(myrow in 1:nrow(aprox1)){#rellenamos coordenadas
  myx<-estCon[estCon$CÓDIGO_CORTO==aprox1[myrow,]$numEst,]$COORDENADA_X_ETRS89
  myy<-estCon[estCon$CÓDIGO_CORTO==aprox1[myrow,]$numEst,]$COORDENADA_Y_ETRS89
  aprox1[myrow,]$x<-myx
  aprox1[myrow,]$y<-myy
}

x<-rep(NA,16)
y<-rep(NA,16)
aprox2<-cbind(aprox2, x, y)
for(myrow in 1:nrow(aprox2)){#rellenamos coordenadas
  myx<-estCon[estCon$CÓDIGO_CORTO==aprox2[myrow,]$numEst,]$COORDENADA_X_ETRS89
  myy<-estCon[estCon$CÓDIGO_CORTO==aprox2[myrow,]$numEst,]$COORDENADA_Y_ETRS89
  aprox2[myrow,]$x<-myx
  aprox2[myrow,]$y<-myy
}

x<-rep(NA,11)
y<-rep(NA,11)
aprox3<-cbind(aprox3, x, y)
for(myrow in 1:nrow(aprox3)){#rellenamos coordenadas
  myx<-estCon[estCon$CÓDIGO_CORTO==aprox3[myrow,]$numEst,]$COORDENADA_X_ETRS89
  myy<-estCon[estCon$CÓDIGO_CORTO==aprox3[myrow,]$numEst,]$COORDENADA_Y_ETRS89
  aprox3[myrow,]$x<-myx
  aprox3[myrow,]$y<-myy
}


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


getinfo.shape("Mapas/Barrios/200001909.shp")

ED50 <- CRS(paste("+proj=utm +zone=30 +ellps=intl +units=m +no_defs"))
barriosMadrid <- readShapePoly("Mapas/Barrios/200001909.shp",proj4string = ED50)
str(barriosMadrid, max.level = 2)
head(coordinates(barriosMadrid))
barriosMadrid@bbox
head(barriosMadrid@data)
plot(barriosMadrid)

#Enseña barrios en vez de distritos
#distritosMadrid <- readShapePoly("Mapas/Barrios/200001909.shp",proj4string = ED50)
#plot(distritosMadrid)

coordinates(mimapa) <- c("x","y")
proj4string(mimapa)<-ED50
plot(barriosMadrid)
plot(mimapa, pch=20, cex=1,col="red", add=TRUE)


#Visualizacion de los resultados de la aproximacion 1
coordinates(aprox1) <- c("x","y")
proj4string(aprox1)<-ED50

plot(barriosMadrid)
plot(aprox1, pch=20, cex=2,col=aprox1$grupoKmeans, add=TRUE)

plot(barriosMadrid)
plot(aprox1, pch=20, cex=2,col=aprox1$grupoJerarquico, add=TRUE)


#Visualizacion de los resultados de la aproximacion 2
coordinates(aprox2) <- c("x","y")
proj4string(aprox2)<-ED50

plot(barriosMadrid)
plot(aprox2, pch=20, cex=2,col=aprox2$grupoKmeans, add=TRUE)

plot(barriosMadrid)
plot(aprox2, pch=20, cex=2,col=aprox2$grupoJerarquico, add=TRUE)


#Visualizacion de los resultados de la aproximacion 2
coordinates(aprox3) <- c("x","y")
proj4string(aprox3)<-ED50

plot(barriosMadrid)
plot(aprox3, pch=20, cex=2,col=aprox3$grupoKmeans, add=TRUE)

plot(barriosMadrid)
plot(aprox3, pch=20, cex=2,col=aprox3$grupoJerarquico, add=TRUE)




