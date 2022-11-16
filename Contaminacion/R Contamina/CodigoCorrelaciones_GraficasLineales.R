rm(list=ls())
#Primer analisis
library(tidyverse)
library(ggplot2)
library(dplyr)
library(hrbrthemes)
CONTAMINA21<-read.csv("Contaminacion/Diario/contamina21RellenoNA.csv",sep=";",dec=",")

#Vamos a crear un DataFrame para cada una de las estaciones e ir metiendo de
#forma ordenada todos los dias del año para ver su evolucion a lo largo de este.

#Estaciones: 
estaciones<-unique(CONTAMINA21[c("estacion")])

#Recorremos el dataframe con los datos y vamos almacenando en un vector de 365 posiciones los 
#valores obtenidos a lo largo del año.

#################################################################

vec<-NULL#Acumula todos los valores de la estacion y magnitud
dfFinal<-data.frame()
found<-NULL#Booleano que indica si la estacion analizada tiene o no valores para esa magnitud
for(estacion in 1:nrow(estaciones)){#Recorremos las estaciones
  est<-estaciones[estacion,] 
  vec<-c(vec,est)
  for(row1 in 1:nrow(CONTAMINA21)){#Por cada estacion recorremos el dataframe con los datos para ir recolectandolos
    if((CONTAMINA21[row1,]$estacion == est)&(CONTAMINA21[row1,]$magnitud==1)){#En este caso serviría para analizar la magnitud 1
      found<-TRUE
      #Si lo encontramos vamos recolectando en vec todos los valores. al final debe tener 372 (31 dias*12 meses)
      a<-CONTAMINA21[row1,c(5:35)]
      for(day in 1:ncol(a)){
        vec<-c(vec,a[,day])
      }
    }
  }
  if(found){#Unimos las filas en caso de que existan valores
    dfFinalAZUFRE<-rbind(dfFinal,vec)
  }
  vec<- NULL
  found<-FALSE
}

#################################################################

vec<-NULL#Acumula todos los valores de la estacion y magnitud
dfFinal<-data.frame()
found<-NULL#Booleano que indica si la estacion analizada tiene o no valores para esa magnitud
for(estacion in 1:nrow(estaciones)){#Recorremos las estaciones
  est<-estaciones[estacion,] 
  vec<-c(vec,est)
  for(row1 in 1:nrow(CONTAMINA21)){#Por cada estacion recorremos el dataframe con los datos para ir recolectandolos
    if((CONTAMINA21[row1,]$estacion == est)&(CONTAMINA21[row1,]$magnitud==8)){#En este caso serviría para analizar la magnitud 8
      found<-TRUE
      #Si lo encontramos vamos recolectando en vec todos los valores. al final debe tener 372 (31 dias*12 meses)
      a<-CONTAMINA21[row1,c(5:35)]
      for(day in 1:ncol(a)){
        vec<-c(vec,a[,day])
      }
    }
  }
  if(found){#Unimos las filas en caso de que existan valores
    dfFinalNITRO<-rbind(dfFinal,vec)
  }
  vec<- NULL
  found<-FALSE
}

#################################################################

vec<-NULL#Acumula todos los valores de la estacion y magnitud
dfFinal<-data.frame()
found<-NULL#Booleano que indica si la estacion analizada tiene o no valores para esa magnitud
for(estacion in 1:nrow(estaciones)){#Recorremos las estaciones
  est<-estaciones[estacion,] 
  vec<-c(vec,est)
  for(row1 in 1:nrow(CONTAMINA21)){#Por cada estacion recorremos el dataframe con los datos para ir recolectandolos
    if((CONTAMINA21[row1,]$estacion == est)&(CONTAMINA21[row1,]$magnitud==9)){#En este caso serviría para analizar la magnitud 9
      found<-TRUE
      #Si lo encontramos vamos recolectando en vec todos los valores. al final debe tener 372 (31 dias*12 meses)
      a<-CONTAMINA21[row1,c(5:35)]
      for(day in 1:ncol(a)){
        vec<-c(vec,a[,day])
      }
    }
  }
  if(found){#Unimos las filas en caso de que existan valores
    dfFinalPARTICULA2<-rbind(dfFinal,vec)
  }
  vec<- NULL
  found<-FALSE
}

#################################################################

vec<-NULL#Acumula todos los valores de la estacion y magnitud
dfFinal<-data.frame()
found<-NULL#Booleano que indica si la estacion analizada tiene o no valores para esa magnitud
for(estacion in 1:nrow(estaciones)){#Recorremos las estaciones
  est<-estaciones[estacion,] 
  vec<-c(vec,est)
  for(row1 in 1:nrow(CONTAMINA21)){#Por cada estacion recorremos el dataframe con los datos para ir recolectandolos
    if((CONTAMINA21[row1,]$estacion == est)&(CONTAMINA21[row1,]$magnitud==10)){#En este caso serviría para analizar la magnitud 10
      found<-TRUE
      #Si lo encontramos vamos recolectando en vec todos los valores. al final debe tener 372 (31 dias*12 meses)
      a<-CONTAMINA21[row1,c(5:35)]
      for(day in 1:ncol(a)){
        vec<-c(vec,a[,day])
      }
    }
  }
  if(found){#Unimos las filas en caso de que existan valores
    dfFinalPARTICULA10<-rbind(dfFinal,vec)
  }
  vec<- NULL
  found<-FALSE
}

#################################################################

vec<-NULL#Acumula todos los valores de la estacion y magnitud
dfFinal<-data.frame()
found<-NULL#Booleano que indica si la estacion analizada tiene o no valores para esa magnitud
for(estacion in 1:nrow(estaciones)){#Recorremos las estaciones
  est<-estaciones[estacion,] 
  vec<-c(vec,est)
  for(row1 in 1:nrow(CONTAMINA21)){#Por cada estacion recorremos el dataframe con los datos para ir recolectandolos
    if((CONTAMINA21[row1,]$estacion == est)&(CONTAMINA21[row1,]$magnitud==14)){#En este caso serviría para analizar la magnitud 14
      found<-TRUE
      #Si lo encontramos vamos recolectando en vec todos los valores. al final debe tener 372 (31 dias*12 meses)
      a<-CONTAMINA21[row1,c(5:35)]
      for(day in 1:ncol(a)){
        vec<-c(vec,a[,day])
      }
    }
  }
  if(found){#Unimos las filas en caso de que existan valores
    dfFinalOZONO<-rbind(dfFinal,vec)
  }
  vec<- NULL
  found<-FALSE
}


#dfFinalAZUFRE
#dfFinalNITRO
#dfFinalPARTICULA2
#dfFinalPARTICULA10
#dfFinalOZONO

#################################################################
library(ggplot2)
library(dplyr)
library(hrbrthemes)
#Primera estacion
dias<-NULL
for(x in 1:372){
  dias<-c(dias,x)
}
#Estacion 1 
a<-dfFinalAZUFRE[1,c(2:373)]#Nos quedamos los valores para la estacion 1.
rownames(a) <- c("Distrito1")#Renombramos
afin<-t(a)#Hacemos la traspuesta para representar
#Estacion 2 
b<-dfFinalAZUFRE[7,c(2:373)]#Nos quedamos los valores para la estacion 7.
rownames(b) <- c("Distrito2")#Renombramos
afin2<-t(b)#Hacemos la traspuesta para representar
#Insertamos todo
df<-NULL
df<-cbind(df,dias,afin,afin2)

#X como los dias del año, Y como la SO2
colors <- c("Distrito1" = "darkred", "Distrito2" = "steelblue")

ggplot(data.frame(df), aes(x=dias)) +
  geom_line(aes(y = Distrito1, color="Distrito1")) + 
  geom_line(aes(y = Distrito2, color="Distrito2"))+
  labs(x = "Días del año", y = "SO2",  color = "Legend") +
  ggtitle("Evolución del Dioxido de Azufre")+
  scale_color_manual(values = colors)+
  theme(legend.key.size = unit(0.3, 'cm'),
        legend.title = element_text(size=8), #change legend title font size
        legend.text = element_text(size=5))
  



#######################################################
#Lo que vamos a hacer ahora es ver si existen correlaciones entre las distintas variables.
#Correlación de Dioxido de Azufre y zonas verdes. Para ello primero sacamos la media de las 
#Dioxido de Azufre por estaciones, ignorando la primera columna que es el id de la estacion.
correTempVerde<-data.frame(estacion=dfFinalAZUFRE[,1], Means=rowMeans(dfFinalAZUFRE[,c(2:373)], na.rm=TRUE))

estacionDistrito<-read.csv("Contaminacion/estacion_distrito.csv",sep=";",dec=",")
ZonasVerdes<-read.csv("Zonas Verdes/masaArborea21.csv",sep=",",dec=".")
#Puesto que hay más estaciones (26) ue distritos (21), tenemos que asignar a cada estacion
#las zonas verdes de su distrito.

masaArb<-NULL
for(myrow in 1:nrow(correTempVerde)){#Seleccionamos el distrito de la estacion y buscamos su valor
  #despues lo añadimos a un vector 
  est<-correTempVerde[myrow,]$estacion
  distr<-estacionDistrito[estacionDistrito$ESTACION==est,]$DISTRITO
  masa<-ZonasVerdes[ZonasVerdes$Nº.DISTRITO==distr,]$Superficie.Masa.arbórea.m2
  masaArb<-c(masaArb,masa[1])
}
#Combinamos como una nueva columna el vector con las masas arboreas.
correTempVerde<-cbind(correTempVerde,masaArb)
cTvNA<-correTempVerde[correTempVerde$masaArb>0,]
#Antes de representar hay que normalizar los datos
aa<-as.data.frame(scale(cTvNA[,c(2,3)]))

ggplot(aa, aes(x=Means, y=masaArb)) + 
  geom_point( color="#69b3a2") +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  labs(x = "Media de Dioxido de Azufre ºC", y = "Masa Arbórea m2") +
  ggtitle("Relación masa arborea y Dioxido de Azufre")
  

################################################################
#Correlación Dioxido de Azufre y Ozono.
#Para ello usamos la parte superior del script para obtener todos los datos de Dioxido de Azufre
#y Ozono y sacamos sus medias.
vec<-NULL#Acumula todos los valores de la estacion y magnitud
dfTemp<-data.frame()
found<-NULL#Booleano que indica si la estacion analizada tiene o no valores para esa magnitud
for(estacion in 1:nrow(estaciones)){#Recorremos las estaciones
  est<-estaciones[estacion,] 
  vec<-c(vec,est)
  for(row1 in 1:nrow(CONTAMINA21)){#Por cada estacion recorremos el dataframe con los datos para ir recolectandolos
    if((CONTAMINA21[row1,]$estacion == est)&(CONTAMINA21[row1,]$magnitud==1)){#En este caso serviría para analizar la magnitud 1
      found<-TRUE
      #Si lo encontramos vamos recolectando en vec todos los valores. al final debe tener 372 (31 dias*12 meses)
      a<-CONTAMINA21[row1,c(5:35)]
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
for(estacion in 1:nrow(estaciones)){#Recorremos las estaciones
  est<-estaciones[estacion,] 
  vec<-c(vec,est)
  for(row1 in 1:nrow(CONTAMINA21)){#Por cada estacion recorremos el dataframe con los datos para ir recolectandolos
    if((CONTAMINA21[row1,]$estacion == est)&(CONTAMINA21[row1,]$magnitud==9)){#En este caso serviría para analizar la magnitud 9
      found<-TRUE
      #Si lo encontramos vamos recolectando en vec todos los valores. al final debe tener 372 (31 dias*12 meses)
      a<-CONTAMINA21[row1,c(5:35)]
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
#Una vez que tenemos los 2 dataframes con los datos de Dioxido de Azufre y Ozono
#sacamos la media de cada uno de ellos
correTempHume<-data.frame(estacion=dfTemp[,1], Azuf=rowMeans(dfTemp[,c(2:373)], na.rm=TRUE),
                          Ozono=rowMeans(dfHum[,c(2:373)], na.rm=TRUE))
#normalizamos el dataframe
aa<-as.data.frame(scale(correTempHume[,c(2,3)]))

#representamos la correlacion
ggplot(aa, aes(x=Azuf, y=Ozono)) + 
  geom_point( color="#69b3a2") +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  labs(x = "Media de Dioxido de Azufre SO2", y = "Media Ozono") +
  ggtitle("Relación Dioxido de Azufre y Ozono")
