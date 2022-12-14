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
estCon<-estacionesControl[,c(2,3,22,23)]
estCon$LONGITUD<-as.numeric(estCon$LONGITUD)
estCon$LATITUD<-as.numeric(estCon$LATITUD)
aprox1 <- read.csv("Mapas/Clustering/aprox1.csv",sep=";")
aprox2 <- read.csv("Mapas/Clustering/aprox2.csv",sep=";")
aprox3 <- read.csv("Mapas/Clustering/aprox3.csv",sep=";")
x<-rep(NA,8)
y<-rep(NA,8)
aprox1<-cbind(aprox1, x, y)
for(myrow in 1:nrow(aprox1)){#rellenamos coordenadas
  myx<-estCon[estCon$CÓDIGO_CORTO==aprox1[myrow,]$numEst,]$LONGITUD
  myy<-estCon[estCon$CÓDIGO_CORTO==aprox1[myrow,]$numEst,]$LATITUD
  aprox1[myrow,]$x<-myx
  aprox1[myrow,]$y<-myy
}

x<-rep(NA,16)
y<-rep(NA,16)
aprox2<-cbind(aprox2, x, y)
for(myrow in 1:nrow(aprox2)){#rellenamos coordenadas
  myx<-estCon[estCon$CÓDIGO_CORTO==aprox2[myrow,]$numEst,]$LONGITUD
  myy<-estCon[estCon$CÓDIGO_CORTO==aprox2[myrow,]$numEst,]$LATITUD
  aprox2[myrow,]$x<-myx
  aprox2[myrow,]$y<-myy
}
x<-rep(NA,11)
y<-rep(NA,11)
aprox3<-cbind(aprox3, x, y)
for(myrow in 1:nrow(aprox3)){#rellenamos coordenadas
  myx<-estCon[estCon$CÓDIGO_CORTO==aprox3[myrow,]$numEst,]$LONGITUD
  myy<-estCon[estCon$CÓDIGO_CORTO==aprox3[myrow,]$numEst,]$LATITUD
  aprox3[myrow,]$x<-myx
  aprox3[myrow,]$y<-myy
}

###############################################################

mad_map <- get_stamenmap(getbb("Madrid"), zoom = 12, maptype = "terrain")

ggmap(mad_map)+geom_point(data=aprox1, aes(x = x , y = y),colour = aprox1$grupoKmeans)
ggmap(mad_map)+geom_point(data=aprox1, aes(x = x , y = y),colour = aprox1$grupoJerarquico)
ggmap(mad_map)+geom_point(data=aprox2, aes(x = x , y = y),colour = aprox2$grupoKmeans)
ggmap(mad_map)+geom_point(data=aprox2, aes(x = x , y = y),colour = aprox2$grupoJerarquico)
ggmap(mad_map)+geom_point(data=aprox3, aes(x = x , y = y),colour = aprox3$grupoKmeans)
ggmap(mad_map)+geom_point(data=aprox3, aes(x = x , y = y),colour = aprox3$grupoJerarquico)



############cARRETERAS DE MADRID
carreteras <- getbb("Madrid Spain")%>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("road", "motorway", "primary")) %>%
  osmdata_sf()


##########RIOS DE MADRID
rios <- getbb("Madrid Spain")%>%
  opq()%>%
  add_osm_feature(key = "waterway", 
                  value = c("river", "canal")) %>%
  osmdata_sf()

############VERDE MADRID
verde <- getbb("Madrid Spain")%>%
  opq()%>%
  add_osm_feature(key = "landuse", 
                  value = c("forest")) %>%

  osmdata_sf()
############Naturaleza MADRID
naturaleza <- getbb("Madrid Spain")%>%
  opq()%>%
add_osm_feature(key = "natural", 
                value = c("wood")) %>%
  osmdata_sf()

############

ggplot() +
  geom_sf(data = carreteras$osm_lines,
          inherit.aes = FALSE,
          color = "black",
          size = .2,
          alpha = .8)+
  geom_sf(data = rios$osm_lines,
          inherit.aes = FALSE,
          color = "#7fc0ff",
          size = .8,
          alpha = .8) +  
  geom_sf(data = verde$osm_points,
          inherit.aes = FALSE,
          color = "green",
          size = .8,
          alpha = .8) +
  geom_sf(data = naturaleza$osm_points,
          inherit.aes = FALSE,
          color = "green",
          size = .8,
          alpha = .8) + 
  coord_sf(xlim = c(-3.88, -3.51), 
           ylim = c(40.26, 40.59),
           expand = FALSE) +
  theme_void() +
  labs(title = "Madrid")


