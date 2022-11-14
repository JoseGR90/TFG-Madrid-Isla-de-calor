rm(list=ls())
#Primer analisis
library(tidyverse)
library(ggplot2)
library(dplyr)
library(hrbrthemes)
METEO21<-read.csv("Meteo/Diario/meteoDiario21_RellenadoNA.csv",sep=";",dec=",")
#Este archivo es el sacado a partir de la limpieza, en el que los meses que no tienen valores
#por fallo o mantenimiento son rellenados a NA.

#Puesto que el factor más importante a la hora de analizar es ver la temperatura, vamos a comenzar
#por esta magnitud. La idea es crear un DataFrame para cada una de las estaciones e ir metiendo de
#forma ordenada todos los dias del año para ver su evolucion a lo largo de este.

#Estaciones: 102,103,104,106,107,108,109,110,111,112,113,114,115,4,8,16,18,24,35,36,38,39,54,56,58,59
estaciones<-unique(METEO21[c("estacion")])

#Recorremos el dataframe con los datos y vamos almacenando en un vector de 365 posiciones los 
#valores obtenidos a lo largo del año.


#######################################################
##Estacion de ejemplo para gráficas simples
vec<-NULL#Acumula todos los valores de la estacion y magnitud
dfFinal<-data.frame()
found<-NULL#Booleano que indica si la estacion analizada tiene o no valores para esa magnitud
for(estacion in 1:nrow(estaciones)){#Recorremos las 26 estaciones
  est<-estaciones[estacion,] 
  vec<-c(vec,est)
  for(row1 in 1:nrow(METEO21)){#Por cada estacion recorremos el dataframe con los datos para ir recolectandolos
    if((METEO21[row1,]$estacion == est)&(METEO21[row1,]$magnitud==81)){#En este caso serviría para analizar la magnitud 81
      found<-TRUE
      #Si lo encontramos vamos recolectando en vec todos los valores. al final debe tener 372 (31 dias*12 meses)
      a<-METEO21[row1,c(5:35)]
      for(day in 1:ncol(a)){
        vec<-c(vec,a[,day])
      }
    }
  }
  if(found){#Unimos las filas en caso de que existan valores
    dfFinal<-rbind(dfFinal,vec)
  }
  vec<- NULL
  found<-FALSE
}

dias<-NULL
for(x in 1:372){
  dias<-c(dias,x)
}
a<-dfFinal[1,c(2:373)]
rownames(a) <- c("Row1")
afin<-t(a)
afin<-cbind(dias,afin)


ggplot(data.frame(afin), aes(x=dias, y = Row1)) +
  geom_line(color="grey")+
  geom_point(shape=21, color="black", fill="#69b3a2", size=0.5)+
  labs(x = "Días del año", y = "m/s") +
  ggtitle("Evolución de la velocidad del viento Moratalaz")+
  theme_ipsum()
################################################
vec<-NULL#Acumula todos los valores de la estacion y magnitud
dfFinal<-data.frame()
found<-NULL#Booleano que indica si la estacion analizada tiene o no valores para esa magnitud
for(estacion in 1:nrow(estaciones)){#Recorremos las 26 estaciones
  est<-estaciones[estacion,] 
  vec<-c(vec,est)
  for(row1 in 1:nrow(METEO21)){#Por cada estacion recorremos el dataframe con los datos para ir recolectandolos
    if((METEO21[row1,]$estacion == est)&(METEO21[row1,]$magnitud==82)){#En este caso serviría para analizar la magnitud 81
      found<-TRUE
      #Si lo encontramos vamos recolectando en vec todos los valores. al final debe tener 372 (31 dias*12 meses)
      a<-METEO21[row1,c(5:35)]
      for(day in 1:ncol(a)){
        vec<-c(vec,a[,day])
      }
    }
  }
  if(found){#Unimos las filas en caso de que existan valores
    dfFinal<-rbind(dfFinal,vec)
  }
  vec<- NULL
  found<-FALSE
}


a<-dfFinal[1,c(2:373)]
rownames(a) <- c("Row1")
afin<-t(a)
afin<-cbind(dias,afin)


ggplot(data.frame(afin), aes(x=dias, y = Row1)) +
  geom_line(color="grey")+
  geom_point(shape=21, color="black", fill="#69b3a2", size=0.5)+
  labs(x = "Días del año", y = "0º-360º") +
  ggtitle("Evolución de la direccion del viento Moratalaz")+
  theme_ipsum()
#################################################
vec<-NULL#Acumula todos los valores de la estacion y magnitud
dfFinal<-data.frame()
found<-NULL#Booleano que indica si la estacion analizada tiene o no valores para esa magnitud
for(estacion in 1:nrow(estaciones)){#Recorremos las 26 estaciones
  est<-estaciones[estacion,] 
  vec<-c(vec,est)
  for(row1 in 1:nrow(METEO21)){#Por cada estacion recorremos el dataframe con los datos para ir recolectandolos
    if((METEO21[row1,]$estacion == est)&(METEO21[row1,]$magnitud==83)){#En este caso serviría para analizar la magnitud 81
      found<-TRUE
      #Si lo encontramos vamos recolectando en vec todos los valores. al final debe tener 372 (31 dias*12 meses)
      a<-METEO21[row1,c(5:35)]
      for(day in 1:ncol(a)){
        vec<-c(vec,a[,day])
      }
    }
  }
  if(found){#Unimos las filas en caso de que existan valores
    dfFinal<-rbind(dfFinal,vec)
  }
  vec<- NULL
  found<-FALSE
}

a<-dfFinal[1,c(2:373)]
rownames(a) <- c("Row1")
afin<-t(a)
afin<-cbind(dias,afin)


ggplot(data.frame(afin), aes(x=dias, y = Row1)) +
  geom_line(color="grey")+
  geom_point(shape=21, color="black", fill="#69b3a2", size=0.5)+
  labs(x = "Días del año", y = "ºC") +
  ggtitle("Evolución de las temperaturas Moratalaz")+
  theme_ipsum()
############################################
vec<-NULL#Acumula todos los valores de la estacion y magnitud
dfFinal<-data.frame()
found<-NULL#Booleano que indica si la estacion analizada tiene o no valores para esa magnitud
for(estacion in 1:nrow(estaciones)){#Recorremos las 26 estaciones
  est<-estaciones[estacion,] 
  vec<-c(vec,est)
  for(row1 in 1:nrow(METEO21)){#Por cada estacion recorremos el dataframe con los datos para ir recolectandolos
    if((METEO21[row1,]$estacion == est)&(METEO21[row1,]$magnitud==86)){#En este caso serviría para analizar la magnitud 81
      found<-TRUE
      #Si lo encontramos vamos recolectando en vec todos los valores. al final debe tener 372 (31 dias*12 meses)
      a<-METEO21[row1,c(5:35)]
      for(day in 1:ncol(a)){
        vec<-c(vec,a[,day])
      }
    }
  }
  if(found){#Unimos las filas en caso de que existan valores
    dfFinal<-rbind(dfFinal,vec)
  }
  vec<- NULL
  found<-FALSE
}

a<-dfFinal[1,c(2:373)]
rownames(a) <- c("Row1")
afin<-t(a)
afin<-cbind(dias,afin)

ggplot(data.frame(afin), aes(x=dias, y = Row1)) +
  geom_line(color="grey")+
  geom_point(shape=21, color="black", fill="#69b3a2", size=0.5)+
  labs(x = "Días del año", y = "% humedad") +
  ggtitle("Evolución de la humedad relativa Moratalaz")+
  theme_ipsum()

###############################
vec<-NULL#Acumula todos los valores de la estacion y magnitud
dfFinal<-data.frame()
found<-NULL#Booleano que indica si la estacion analizada tiene o no valores para esa magnitud
for(estacion in 1:nrow(estaciones)){#Recorremos las 26 estaciones
  est<-estaciones[estacion,] 
  vec<-c(vec,est)
  for(row1 in 1:nrow(METEO21)){#Por cada estacion recorremos el dataframe con los datos para ir recolectandolos
    if((METEO21[row1,]$estacion == est)&(METEO21[row1,]$magnitud==87)){#En este caso serviría para analizar la magnitud 81
      found<-TRUE
      #Si lo encontramos vamos recolectando en vec todos los valores. al final debe tener 372 (31 dias*12 meses)
      a<-METEO21[row1,c(5:35)]
      for(day in 1:ncol(a)){
        vec<-c(vec,a[,day])
      }
    }
  }
  if(found){#Unimos las filas en caso de que existan valores
    dfFinal<-rbind(dfFinal,vec)
  }
  vec<- NULL
  found<-FALSE
}

a<-dfFinal[1,c(2:373)]
rownames(a) <- c("Row1")
afin<-t(a)
afin<-cbind(dias,afin)

ggplot(data.frame(afin), aes(x=dias, y = Row1)) +
  geom_line(color="grey")+
  geom_point(shape=21, color="black", fill="#69b3a2", size=0.5)+
  labs(x = "Días del año", y = "% humedad") +
  ggtitle("Evolución de la presión barométrica Moratalaz")+
  theme_ipsum()
#######################################
vec<-NULL#Acumula todos los valores de la estacion y magnitud
dfFinal<-data.frame()
found<-NULL#Booleano que indica si la estacion analizada tiene o no valores para esa magnitud
for(estacion in 1:nrow(estaciones)){#Recorremos las 26 estaciones
  est<-estaciones[estacion,] 
  vec<-c(vec,est)
  for(row1 in 1:nrow(METEO21)){#Por cada estacion recorremos el dataframe con los datos para ir recolectandolos
    if((METEO21[row1,]$estacion == est)&(METEO21[row1,]$magnitud==88)){#En este caso serviría para analizar la magnitud 81
      found<-TRUE
      #Si lo encontramos vamos recolectando en vec todos los valores. al final debe tener 372 (31 dias*12 meses)
      a<-METEO21[row1,c(5:35)]
      for(day in 1:ncol(a)){
        vec<-c(vec,a[,day])
      }
    }
  }
  if(found){#Unimos las filas en caso de que existan valores
    dfFinal<-rbind(dfFinal,vec)
  }
  vec<- NULL
  found<-FALSE
}

a<-dfFinal[1,c(2:373)]
rownames(a) <- c("Row1")
afin<-t(a)
afin<-cbind(dias,afin)

ggplot(data.frame(afin), aes(x=dias, y = Row1)) +
  geom_line(color="grey")+
  geom_point(shape=21, color="black", fill="#69b3a2", size=0.5)+
  labs(x = "Días del año", y = "W/m2") +
  ggtitle("Evolución de la radiacion solar Moratalaz")+
  theme_ipsum()
##################################
vec<-NULL#Acumula todos los valores de la estacion y magnitud
dfFinal<-data.frame()
found<-NULL#Booleano que indica si la estacion analizada tiene o no valores para esa magnitud
for(estacion in 1:nrow(estaciones)){#Recorremos las 26 estaciones
  est<-estaciones[estacion,] 
  vec<-c(vec,est)
  for(row1 in 1:nrow(METEO21)){#Por cada estacion recorremos el dataframe con los datos para ir recolectandolos
    if((METEO21[row1,]$estacion == est)&(METEO21[row1,]$magnitud==89)){#En este caso serviría para analizar la magnitud 81
      found<-TRUE
      #Si lo encontramos vamos recolectando en vec todos los valores. al final debe tener 372 (31 dias*12 meses)
      a<-METEO21[row1,c(5:35)]
      for(day in 1:ncol(a)){
        vec<-c(vec,a[,day])
      }
    }
  }
  if(found){#Unimos las filas en caso de que existan valores
    dfFinal<-rbind(dfFinal,vec)
  }
  vec<- NULL
  found<-FALSE
}

a<-dfFinal[1,c(2:373)]
rownames(a) <- c("Row1")
afin<-t(a)
afin<-cbind(dias,afin)

ggplot(data.frame(afin), aes(x=dias, y = Row1)) +
  geom_line(color="grey")+
  geom_point(shape=21, color="black", fill="#69b3a2", size=0.5)+
  labs(x = "Días del año", y = "L/m2") +
  ggtitle("Evolución de las precipitaciones Moratalaz")+
  theme_ipsum()




##################################################################################################
##Ahora vamos a ver las diferencias entre 2 estaciones, Moratalaz y centro
##Por ejemplo, remarcar que las temperaturas están más suavizadas en las estaciones 
##más exteriores que en las interiores. Y es que de media las temperaturas de Moratalaz 
##(cuyo distrito es exterior y contiene una mayor cantidad de árboles -14.000 contra 36.000- y
##de zonas azules) son del orden de 2ºC más bajas, haciendo que el fenómeno de la ICU se suavice.
vec<-NULL#Acumula todos los valores de la estacion y magnitud
dfFinal<-data.frame()
found<-NULL#Booleano que indica si la estacion analizada tiene o no valores para esa magnitud
for(estacion in 1:nrow(estaciones)){#Recorremos las 26 estaciones
  est<-estaciones[estacion,] 
  vec<-c(vec,est)
  for(row1 in 1:nrow(METEO21)){#Por cada estacion recorremos el dataframe con los datos para ir recolectandolos
    if((METEO21[row1,]$estacion == est)&(METEO21[row1,]$magnitud==83)){#En este caso serviría para analizar la magnitud 81
      found<-TRUE
      #Si lo encontramos vamos recolectando en vec todos los valores. al final debe tener 372 (31 dias*12 meses)
      a<-METEO21[row1,c(5:35)]
      for(day in 1:ncol(a)){
        vec<-c(vec,a[,day])
      }
    }
  }
  if(found){#Unimos las filas en caso de que existan valores
    dfFinal<-rbind(dfFinal,vec)
  }
  vec<- NULL
  found<-FALSE
}

#Estacion 1 Moratalaz
a<-dfFinal[1,c(2:373)]#Nos quedamos los valores para la estacion 1.
rownames(a) <- c("Moratalaz")#Renombramos
afin<-t(a)#Hacemos la traspuesta para representar
#Estacion 2 Centro
b<-dfFinal[7,c(2:373)]#Nos quedamos los valores para la estacion 7.
rownames(b) <- c("Centro")#Renombramos
afin2<-t(b)#Hacemos la traspuesta para representar
#Insertamos todo
df<-NULL
df<-cbind(df,dias,afin,afin2)

#X como los dias del año, Y como las temperaturas
colors <- c("Moratalaz" = "darkred", "Centro" = "steelblue")

ggplot(data.frame(df), aes(x=dias)) +
  geom_line(aes(y = Moratalaz, color="Moratalaz")) + 
  geom_line(aes(y = Centro, color="Centro"))+
  labs(x = "Días del año", y = "ºC",  color = "Legend") +
  ggtitle("Evolución de las temperaturas")+
  scale_color_manual(values = colors)+
  theme(legend.key.size = unit(0.3, 'cm'),
        legend.title = element_text(size=8), #change legend title font size
        legend.text = element_text(size=5))



################################################################################################
#Lo que vamos a hacer ahora es ver si existen correlaciones entre las distintas variables.
#Correlación de temperaturas y zonas verdes. Para ello primero sacamos la media de las 
#temperaturas por estaciones, ignorando la primera columna que es el id de la estacion.
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
  for(row1 in 1:nrow(METEO21)){#Por cada estacion recorremos el dataframe con los datos para ir recolectandolos
    if((METEO21[row1,]$estacion == est)&(METEO21[row1,]$magnitud==83)){#En este caso serviría para analizar la magnitud 81
      found<-TRUE
      #Si lo encontramos vamos recolectando en vec todos los valores. al final debe tener 372 (31 dias*12 meses)
      a<-METEO21[row1,c(5:35)]
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
  for(row1 in 1:nrow(METEO21)){#Por cada estacion recorremos el dataframe con los datos para ir recolectandolos
    if((METEO21[row1,]$estacion == est)&(METEO21[row1,]$magnitud==86)){#En este caso serviría para analizar la magnitud 81
      found<-TRUE
      #Si lo encontramos vamos recolectando en vec todos los valores. al final debe tener 372 (31 dias*12 meses)
      a<-METEO21[row1,c(5:35)]
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

