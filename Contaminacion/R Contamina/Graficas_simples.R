#Primer analisis
library(tidyverse)
CONTAMINA21<-read.csv("contaminaDiario21_RellenoNA.csv",sep=";",dec=",")

#Vamos a crear un DataFrame para cada una de las estaciones e ir metiendo de
#forma ordenada todos los dias del año para ver su evolucion a lo largo de este.

#Estaciones: 
estaciones<-unique(CONTAMINA21[c("estacion")])

#Recorremos el dataframe con los datos y vamos almacenando en un vector de 365 posiciones los 
#valores obtenidos a lo largo del año.


##################################Dioxido de Azufre################################################
vec<-NULL#Acumula todos los valores de la estacion y magnitud
dfFinal<-data.frame()
found<-NULL#Booleano que indica si la estacion analizada tiene o no valores para esa magnitud
for(estacion in 1:nrow(estaciones)){#Recorremos las estaciones
  est<-estaciones[estacion,] 
  vec<-c(vec,est)
  for(row1 in 1:nrow(CONTAMINA21)){#Por cada estacion recorremos el dataframe con los datos para ir recolectandolos
    if((CONTAMINA21[row1,]$estacion == est)&(CONTAMINA21[row1,]$magnitud=1)){#En este caso serviría para analizar la magnitud 1
      found<-TRUE
      #Si lo encontramos vamos recolectando en vec todos los valores. al final debe tener 372 (31 dias*12 meses)
      a<-CONTAMINA21[row1,c(5:35)]
      for(day in 1:ncol(a)){
        vec<-c(vec,a[,day])
      }
    }
  }
  if(found){#Unimos las filas en caso de que existan valores
    dfFinalAzufre<-rbind(dfFinal,vec)
  }
  vec<- NULL
  found<-FALSE
}

#################################################################
library(ggplot2)
#Primera estacion
dias<-NULL
for(x in 1:372){
  dias<-c(dias,x)
}
a<-dfFinalAzufre[1,c(2:373)]
rownames(a) <- c("Row1")
afin<-t(a)
afin<-cbind(dias,afin)
matplot(afin,type="l")


#HAY QUE MIRAR COMO HACER QUE GGPLOT NO QUITE LOS VALORES NULOS, NOSE
#X como los dias del año, Y como las temperaturas
ggplot(data.frame(afin), aes(x=dias, y = Row1)) +
  geom_line()+
  geom_point()
#####################################################################################

######################################Dioxido de Nitrogeno############################################
vec<-NULL#Acumula todos los valores de la estacion y magnitud
dfFinal<-data.frame()
found<-NULL#Booleano que indica si la estacion analizada tiene o no valores para esa magnitud
for(estacion in 1:nrow(estaciones)){#Recorremos las estaciones
  est<-estaciones[estacion,] 
  vec<-c(vec,est)
  for(row1 in 1:nrow(CONTAMINA21)){#Por cada estacion recorremos el dataframe con los datos para ir recolectandolos
    if((CONTAMINA21[row1,]$estacion == est)&(CONTAMINA21[row1,]$magnitud=8)){#En este caso serviría para analizar la magnitud 1
      found<-TRUE
      #Si lo encontramos vamos recolectando en vec todos los valores. al final debe tener 372 (31 dias*12 meses)
      a<-CONTAMINA21[row1,c(5:35)]
      for(day in 1:ncol(a)){
        vec<-c(vec,a[,day])
      }
    }
  }
  if(found){#Unimos las filas en caso de que existan valores
    dfFinalNitrogeno<-rbind(dfFinal,vec)
  }
  vec<- NULL
  found<-FALSE
}

#################################################################
library(ggplot2)
#Primera estacion
dias<-NULL
for(x in 1:372){
  dias<-c(dias,x)
}
a<-dfFinalNitrogeno[1,c(2:373)]
rownames(a) <- c("Row1")
afin<-t(a)
afin<-cbind(dias,afin)
matplot(afin,type="l")


#HAY QUE MIRAR COMO HACER QUE GGPLOT NO QUITE LOS VALORES NULOS, NOSE
#X como los dias del año, Y como las temperaturas
ggplot(data.frame(afin), aes(x=dias, y = Row1)) +
  geom_line()+
  geom_point()
#####################################################################################

#######################################Partículas < 2.5 um###########################################
vec<-NULL#Acumula todos los valores de la estacion y magnitud
dfFinal<-data.frame()
found<-NULL#Booleano que indica si la estacion analizada tiene o no valores para esa magnitud
for(estacion in 1:nrow(estaciones)){#Recorremos las estaciones
  est<-estaciones[estacion,] 
  vec<-c(vec,est)
  for(row1 in 1:nrow(CONTAMINA21)){#Por cada estacion recorremos el dataframe con los datos para ir recolectandolos
    if((CONTAMINA21[row1,]$estacion == est)&(CONTAMINA21[row1,]$magnitud=9)){#En este caso serviría para analizar la magnitud 1
      found<-TRUE
      #Si lo encontramos vamos recolectando en vec todos los valores. al final debe tener 372 (31 dias*12 meses)
      a<-CONTAMINA21[row1,c(5:35)]
      for(day in 1:ncol(a)){
        vec<-c(vec,a[,day])
      }
    }
  }
  if(found){#Unimos las filas en caso de que existan valores
    dfFinalParticula2<-rbind(dfFinal,vec)
  }
  vec<- NULL
  found<-FALSE
}

#################################################################
library(ggplot2)
#Primera estacion
dias<-NULL
for(x in 1:372){
  dias<-c(dias,x)
}
a<-dfFinalParticula2[1,c(2:373)]
rownames(a) <- c("Row1")
afin<-t(a)
afin<-cbind(dias,afin)
matplot(afin,type="l")


#HAY QUE MIRAR COMO HACER QUE GGPLOT NO QUITE LOS VALORES NULOS, NOSE
#X como los dias del año, Y como las temperaturas
ggplot(data.frame(afin), aes(x=dias, y = Row1)) +
  geom_line()+
  geom_point()
#####################################################################################

#########################################Partículas < 10 um#########################################
vec<-NULL#Acumula todos los valores de la estacion y magnitud
dfFinal<-data.frame()
found<-NULL#Booleano que indica si la estacion analizada tiene o no valores para esa magnitud
for(estacion in 1:nrow(estaciones)){#Recorremos las estaciones
  est<-estaciones[estacion,] 
  vec<-c(vec,est)
  for(row1 in 1:nrow(CONTAMINA21)){#Por cada estacion recorremos el dataframe con los datos para ir recolectandolos
    if((CONTAMINA21[row1,]$estacion == est)&(CONTAMINA21[row1,]$magnitud=10)){#En este caso serviría para analizar la magnitud 1
      found<-TRUE
      #Si lo encontramos vamos recolectando en vec todos los valores. al final debe tener 372 (31 dias*12 meses)
      a<-CONTAMINA21[row1,c(5:35)]
      for(day in 1:ncol(a)){
        vec<-c(vec,a[,day])
      }
    }
  }
  if(found){#Unimos las filas en caso de que existan valores
    dfFinalParticula10<-rbind(dfFinal,vec)
  }
  vec<- NULL
  found<-FALSE
}

#################################################################
library(ggplot2)
#Primera estacion
dias<-NULL
for(x in 1:372){
  dias<-c(dias,x)
}
a<-dfFinalParticula10[1,c(2:373)]
rownames(a) <- c("Row1")
afin<-t(a)
afin<-cbind(dias,afin)
matplot(afin,type="l")


#HAY QUE MIRAR COMO HACER QUE GGPLOT NO QUITE LOS VALORES NULOS, NOSE
#X como los dias del año, Y como las temperaturas
ggplot(data.frame(afin), aes(x=dias, y = Row1)) +
  geom_line()+
  geom_point()
#####################################################################################

#########################################  Ozono  #########################################
vec<-NULL#Acumula todos los valores de la estacion y magnitud
dfFinal<-data.frame()
found<-NULL#Booleano que indica si la estacion analizada tiene o no valores para esa magnitud
for(estacion in 1:nrow(estaciones)){#Recorremos las estaciones
  est<-estaciones[estacion,] 
  vec<-c(vec,est)
  for(row1 in 1:nrow(CONTAMINA21)){#Por cada estacion recorremos el dataframe con los datos para ir recolectandolos
    if((CONTAMINA21[row1,]$estacion == est)&(CONTAMINA21[row1,]$magnitud=14)){#En este caso serviría para analizar la magnitud 1
      found<-TRUE
      #Si lo encontramos vamos recolectando en vec todos los valores. al final debe tener 372 (31 dias*12 meses)
      a<-CONTAMINA21[row1,c(5:35)]
      for(day in 1:ncol(a)){
        vec<-c(vec,a[,day])
      }
    }
  }
  if(found){#Unimos las filas en caso de que existan valores
    dfFinalOzono<-rbind(dfFinal,vec)
  }
  vec<- NULL
  found<-FALSE
}

#################################################################
library(ggplot2)
#Primera estacion
dias<-NULL
for(x in 1:372){
  dias<-c(dias,x)
}
a<-dfFinalOzono[1,c(2:373)]
rownames(a) <- c("Row1")
afin<-t(a)
afin<-cbind(dias,afin)
matplot(afin,type="l")


#HAY QUE MIRAR COMO HACER QUE GGPLOT NO QUITE LOS VALORES NULOS, NOSE
#X como los dias del año, Y como las temperaturas
ggplot(data.frame(afin), aes(x=dias, y = Row1)) +
  geom_line()+
  geom_point()
#####################################################################################
