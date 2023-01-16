rm(list=ls())
#Primer analisis
library(tidyverse)
library(ggplot2)
library(dplyr)
library(hrbrthemes)
meteo21<-read.csv("Meteo/Diario/meteoDiario21_byDay.csv",sep=";",dec=",")
contamina21<-read.csv("contaminacion/Diario/contaminaDiario21_byDay.csv",sep=";",dec=",")
#Este archivo es el sacado a partir de la limpieza, en el que los meses que no tienen valores
#por fallo o mantenimiento son rellenados a NA.

#Puesto que el factor más importante a la hora de analizar es ver la temperatura, vamos a comenzar
#por esta magnitud. La idea es crear un DataFrame para cada una de las estaciones e ir metiendo de
#forma ordenada todos los dias del año para ver su evolucion a lo largo de este.

#Estaciones: 102,103,104,106,107,108,109,110,111,112,113,114,115,4,8,16,18,24,35,36,38,39,54,56,58,59
estaciones<-unique(meteo21[c("Estacion")])

#Recorremos el dataframe con los datos y vamos almacenando en un vector de 365 posiciones los 
#valores obtenidos a lo largo del año.


ggplot(meteo21[meteo21$Estacion=="102",], aes(x=as.Date(Fecha), y = X81, group=1)) +
  geom_line(color="grey")+
  geom_point(shape=21, color="black", fill="#69b3a2", size=0.5)+
  labs(x = "Días del año", y = "m/s") +
  ggtitle("Evolución de la velocidad del viento Moratalaz")

ggplot(meteo21[meteo21$Estacion=="102",], aes(x=as.Date(Fecha), y = X82, group=1)) +
  geom_line(color="grey")+
  geom_point(shape=21, color="black", fill="#69b3a2", size=0.5)+
  labs(x = "Días del año", y = "0º-360º") +
  ggtitle("Evolución de la direccion del viento Moratalaz")

ggplot(meteo21[meteo21$Estacion=="102",], aes(x=as.Date(Fecha), y = X83, group=1)) +
  geom_line(color="grey")+
  geom_point(shape=21, color="black", fill="#69b3a2", size=0.5)+
  labs(x = "Días del año", y = "ºC") +
  ggtitle("Evolución de las temperaturas Moratalaz")

ggplot(meteo21[meteo21$Estacion=="102",], aes(x=as.Date(Fecha), y = X86, group=1)) +
  geom_line(color="grey")+
  geom_point(shape=21, color="black", fill="#69b3a2", size=0.5)+
  labs(x = "Días del año", y = "% humedad") +
  ggtitle("Evolución de la humedad relativa Moratalaz")

ggplot(meteo21[meteo21$Estacion=="102",], aes(x=as.Date(Fecha), y = X87, group=1)) +
  geom_line(color="grey")+
  geom_point(shape=21, color="black", fill="#69b3a2", size=0.5)+
  labs(x = "Días del año", y = "% humedad") +
  ggtitle("Evolución de la presión barométrica Moratalaz")

ggplot(meteo21[meteo21$Estacion=="102",], aes(x=as.Date(Fecha), y = X88, group=1)) +
  geom_line(color="grey")+
  geom_point(shape=21, color="black", fill="#69b3a2", size=0.5)+
  labs(x = "Días del año", y = "W/m2") +
  ggtitle("Evolución de la radiacion solar Moratalaz")

ggplot(meteo21[meteo21$Estacion=="102",], aes(x=as.Date(Fecha), y = X89, group=1)) +
  geom_line(color="grey")+
  geom_point(shape=21, color="black", fill="#69b3a2", size=0.5)+
  labs(x = "Días del año", y = "L/m2") +
  ggtitle("Evolución de las precipitaciones Moratalaz")


##################################################################################################
##Ahora vamos a ver si hay similitud entre radiacion solar y precipitaciones

dfComp<-NULL

my102<-meteo21[meteo21$Estacion=="102",]
dfComp<-cbind(my102$Fecha, my102$X88, my102$X89)
dfComp<-data.frame(dfComp)
names(dfComp)[1] <- 'Fecha'
names(dfComp)[2] <- 'Radiacion'
names(dfComp)[3] <- 'Precipitacion'
dfComp$Radiacion<-as.numeric(dfComp$Radiacion)
dfComp$Precipitacion<-as.numeric(dfComp$Precipitacion)
colors <- c("Radiacion" = "darkred", "Precipitacion" = "steelblue")

ggplot(dfComp, aes(x=as.Date(Fecha), group=1)) +
  geom_line(aes(y = Radiacion, color="Radiacion"))+ 
  geom_line(aes(y = Precipitacion, color="Precipitacion"))+
  labs(x = "Días del año", y = "W/L m2",  color = "Legend") +
  ggtitle("Correlacion LLuvia Radiacion")+
  scale_color_manual(values = colors)+
  theme(legend.key.size = unit(0.3, 'cm'),
        legend.title = element_text(size=8), #change legend title font size
        legend.text = element_text(size=5))


##################################################################################################
##Ahora vamos a ver las diferencias entre las temperaturas mas extremas
##Por ejemplo, remarcar que las temperaturas están más suavizadas en las estaciones 
##más exteriores que en las interiores. Y es que de media las temperaturas de Moratalaz 
##(cuyo distrito es exterior y contiene una mayor cantidad de árboles -14.000 contra 36.000- y
##de zonas azules) son del orden de 2ºC más bajas, haciendo que el fenómeno de la ICU se suavice.
dfComp<-NULL

my102<-meteo21[meteo21$Estacion=="102",]
my38<-meteo21[meteo21$Estacion=="38",]
dfComp<-cbind(my102$Fecha, my102$X83, my38$X83)
dfComp<-data.frame(dfComp)
names(dfComp)[1] <- 'Fecha'
names(dfComp)[2] <- 'Moratalaz'
names(dfComp)[3] <- 'CuatroCaminos'
dfComp$Moratalaz<-as.numeric(dfComp$Moratalaz)
dfComp$CuatroCaminos<-as.numeric(dfComp$CuatroCaminos)
colors <- c("Moratalaz" = "darkred", "CuatroCaminos" = "steelblue")

ggplot(dfComp, aes(x=as.Date(Fecha), group=1)) +
  geom_line(aes(y = Moratalaz, color="Moratalaz"))+ 
  geom_line(aes(y = CuatroCaminos, color="CuatroCaminos"))+
  labs(x = "Días del año", y = "ºC",  color = "Legend") +
  ggtitle("Evolución de las temperaturas")+
  scale_color_manual(values = colors)+
  theme(legend.key.size = unit(0.3, 'cm'),
        legend.title = element_text(size=8), #change legend title font size
        legend.text = element_text(size=5))

##################################################################################################
##Ahora vamos a ver las diferencias entre todas las estaciones
#
dfComp<-NULL

my102<-meteo21[meteo21$Estacion=="102",]
my103<-meteo21[meteo21$Estacion=="103",]
my106<-meteo21[meteo21$Estacion=="106",]
my109<-meteo21[meteo21$Estacion=="109",]
my112<-meteo21[meteo21$Estacion=="112",]


dfComp<-cbind(my102$Fecha,my102$X83, my103$X83, my106$X83,my109$X83,my112$X83)
dfComp<-data.frame(dfComp)
names(dfComp)[1] <- 'Fecha'
names(dfComp)[2] <- 'my102'
names(dfComp)[3] <- 'my103'
names(dfComp)[4] <- 'my106'
names(dfComp)[5] <- 'my109'
names(dfComp)[6] <- 'my112'

dfComp$my102<-as.numeric(dfComp$my102)
dfComp$my103<-as.numeric(dfComp$my103)
dfComp$my106<-as.numeric(dfComp$my106)
dfComp$my109<-as.numeric(dfComp$my109)
dfComp$my112<-as.numeric(dfComp$my112)

colors <- c("my102" = "magenta", "my103" = "darkred","my106" = "darkblue", "my109" = "aquamarine4")
#my112 y 109 la temperatura mas alta 106 de las mas bajas
ggplot(dfComp, aes(x=as.Date(Fecha), group=1)) +
  geom_line(aes(y = my102, color="my102"))+ 
  geom_line(aes(y = my103, color="my103"))+
  geom_line(aes(y = my106, color="my106"))+
  geom_line(aes(y = my109, color="my109"))+
  geom_line(aes(y = my112))+
  labs(x = "Días del año", y = "ºC",  color = "Legend") +
  scale_color_manual(values = colors)+
  theme(legend.key.size = unit(1.9, 'cm'),
        legend.title = element_text(size=8), #change legend title font size
        legend.text = element_text(size=5))



#######################################
#######################################
#vamos a comparar temperatura y ozono

##################################################################################################
#######################este esta a medias

dfComp<-NULL
my102<-meteo21[meteo21$Estacion=="102",]
my4<-contamina21[contamina21$Estacion=="4",]
dfComp<-cbind(my102$Fecha, my102$X83, my4$X14)
dfComp<-data.frame(dfComp)
names(dfComp)[1] <- 'Fecha'
names(dfComp)[2] <- 'estacion102'
names(dfComp)[3] <- 'estacion4'
dfComp$estacion102<-as.numeric(dfComp$estacion102)
dfComp$estacion4<-as.numeric(dfComp$estacion4)
colors <- c("estacion102" = "darkred", "estacion4" = "magenta")

ggplot(dfComp, aes(x=as.Date(Fecha), group=1)) +
  geom_line(aes(y = estacion102, color="estacion102"))+ 
  geom_line(aes(y = estacion4, color="estacion4"))+
  labs(x = "Días del año", y = "ºC",  color = "Legend") +
  ggtitle("Temperatura - ozono")+
  scale_color_manual(values = colors)+
  theme(legend.key.size = unit(0.3, 'cm'),
        legend.title = element_text(size=8), #change legend title font size
        legend.text = element_text(size=5))

##################################################################################################
##Ahora vamos a ver las diferencias entre estaciones de ozono
#
dfComp<-NULL

my4<-contamina21[contamina21$Estacion=="4",]
my8<-contamina21[contamina21$Estacion=="8",]
my27<-contamina21[contamina21$Estacion=="27",]
my35<-contamina21[contamina21$Estacion=="35",]
my57<-contamina21[contamina21$Estacion=="57",]

dfComp<-cbind(my4$Fecha, my4$X14, my8$X14, my27$X14, my35$X14, my57$X14)

dfComp<-data.frame(dfComp)
names(dfComp)[1] <- 'Fecha'
names(dfComp)[2] <- 'my4'
names(dfComp)[3] <- 'my8'
names(dfComp)[4] <- 'my27'
names(dfComp)[5] <- 'my35'
names(dfComp)[6] <- 'my57'
dfComp$my4<-as.numeric(dfComp$my4)
dfComp$my8<-as.numeric(dfComp$my8)
dfComp$my27<-as.numeric(dfComp$my27)
dfComp$my35<-as.numeric(dfComp$my35)
dfComp$my57<-as.numeric(dfComp$my57)


colors <- c("my4" = "magenta", "my8" = "darkred","my11" = "darkblue", "my16" = "aquamarine4")
#my112 y 109 la temperatura mas alta 106 de las mas bajas
ggplot(dfComp, aes(x=as.Date(Fecha), group=1)) +
  geom_line(aes(y = my4, color="my4"))+ 
  geom_line(aes(y = my8, color="my8"))+
  geom_line(aes(y = my57, color="my11"))+
  geom_line(aes(y = my27, color="my16"))+
  geom_line(aes(y = my35))+
  labs(x = "Días del año", y = "o3",  color = "Legend") +
  scale_color_manual(values = colors)+
  theme(legend.key.size = unit(1.9, 'cm'),
        legend.title = element_text(size=8), #change legend title font size
        legend.text = element_text(size=5))






################################################################################################
#Lo que vamos a hacer ahora es ver si existen correlaciones entre las distintas variables.
#Correlación de temperaturas y zonas verdes. Para ello primero sacamos la media de las 
#temperaturas por estaciones, ignorando la primera columna que es el id de la estacion.
dfFinal<-NULL
correTempVerde<-data.frame(estacion=dfFinal[,1], Means=rowMeans(dfFinal[,c(2:373)], na.rm=TRUE))

estacionDistrito<-read.csv("Meteo/estacion_distrito.csv",sep=";",dec=",")
ZonasVerdes<-read.csv("Zonas Verdes/masaArborea21.csv",sep=";",dec=",")
#Puesto que hay más estaciones (26) ue distritos (21), tenemos que asignar a cada estacion
#las zonas verdes de su distrito.

masaArb<-NULL
for(myrow in 1:nrow(correTempVerde)){#Seleccionamos el distrito de la estacion y buscamos su valor
  #despues lo añadimos a un vector 
  est<-correTempVerde[myrow,]$estacion
  distr<-estacionDistrito[estacionDistrito$ESTACION==est,]$DISTRITO
  masa<-ZonasVerdes[ZonasVerdes$NumDistr==distr,]$Superficiem2
  masaArb<-c(masaArb,masa)#SONIA::: Aqui tipo de masa double, pero aparece vector.
}


#Combinamos como una nueva columna el vector con las masas arboreas.
correTempVerde<-cbind(correTempVerde,masaArb)
cTvNA<-NULL
cTvNA<-correTempVerde[correTempVerde$masaArb>0,]
#Antes de representar hay que normalizar los datos
aa<-as.data.frame(scale(cTvNA[,c(2,3)]))

ggplot(aa, aes(x=Means, y=masaArb)) + 
  geom_point( color="#69b3a2") +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  labs(x = "Media de temperaturas ºC", y = "Masa Arbórea m2") +
  ggtitle("Relación masa arborea y temperaturas")


################################################################
#Correlación temperaturas y humedad relativa.
#Para ello usamos la parte superior del script para obtener todos los datos de temperaturas
#y humedad relativa y sacamos sus medias.
vec<-NULL#Acumula todos los valores de la estacion y magnitud
dfTemp<-data.frame()
found<-NULL#Booleano que indica si la estacion analizada tiene o no valores para esa magnitud
for(estacion in 1:nrow(estaciones)){#Recorremos las 26 estaciones
  est<-estaciones[estacion,] 
  vec<-c(vec,est)
  for(row1 in 1:nrow(meteo21)){#Por cada estacion recorremos el dataframe con los datos para ir recolectandolos
    if((meteo21[row1,]$estacion == est)&(meteo21[row1,]$magnitud==81)){#En este caso serviría para analizar la magnitud 81
      found<-TRUE
      #Si lo encontramos vamos recolectando en vec todos los valores. al final debe tener 372 (31 dias*12 meses)
      a<-meteo21[row1,c(5:35)]
      for(day in 1:ncol(a)){
        vec<-c(vec,a[,day])
      }
    }
  }
  if(found){#Unimos las filas en caso de que existan valores
    dfTemp<-rbind(dfTemp,vec)
  }
  vec<- NULL
  found<-FALSE
}

vec<-NULL#Acumula todos los valores de la estacion y magnitud
dfHum<-data.frame()
found<-NULL#Booleano que indica si la estacion analizada tiene o no valores para esa magnitud
for(estacion in 1:nrow(estaciones)){#Recorremos las 26 estaciones
  est<-estaciones[estacion,] 
  vec<-c(vec,est)
  for(row1 in 1:nrow(meteo21)){#Por cada estacion recorremos el dataframe con los datos para ir recolectandolos
    if((meteo21[row1,]$estacion == est)&(meteo21[row1,]$magnitud==86)){#En este caso serviría para analizar la magnitud 81
      found<-TRUE
      #Si lo encontramos vamos recolectando en vec todos los valores. al final debe tener 372 (31 dias*12 meses)
      a<-meteo21[row1,c(5:35)]
      for(day in 1:ncol(a)){
        vec<-c(vec,a[,day])
      }
    }
  }
  if(found){#Unimos las filas en caso de que existan valores
    dfHum<-rbind(dfHum,vec)
  }
  vec<- NULL
  found<-FALSE
}
#Una vez que tenemos los 2 dataframes con los datos de temperaturas y humedades relativas
#sacamos la media de cada uno de ellos
correTempHume<-data.frame(estacion=dfTemp[,1], Temps=rowMeans(dfTemp[,c(2:373)], na.rm=TRUE),
                          Humedad=rowMeans(dfHum[,c(2:373)], na.rm=TRUE))
#normalizamos el dataframe
aa<-as.data.frame(scale(correTempHume[,c(2,3)]))

#representamos la correlacion
ggplot(aa, aes(x=Temps, y=Humedad)) + 
  geom_point( color="#69b3a2") +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  labs(x = "Media de temperaturas ºC", y = "Media Humedad relativa") +
  ggtitle("Relación temperaturas y humedad")



