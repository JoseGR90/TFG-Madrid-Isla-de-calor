#Box plots para analizar los outliers
rm(list=ls())
library(tidyverse)
library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(viridis)
METEO21<-read.csv("Meteo/Diario/meteoDiario21_RellenadoNA.csv",sep=";",dec=",")
#Este archivo es el sacado a partir de la limpieza, en el que los meses que no tienen valores
#por fallo o mantenimiento son rellenados a NA.

#Para ello vamos a agrupar los datos de cada estacion por cada magnitud
#Para poder repesentar los box plots necesitamos un DF con 2 columnas, una del id de las magnitudes
#y la otra con el valor de dicha magnitud.
dfGlob<-NULL
df81<-NULL
df82<-NULL
df83<-NULL
df86<-NULL
df87<-NULL
df88<-NULL
df89<-NULL
for(myrow in 1:nrow(METEO21)){#Recorremos todas las filas
  for(mycol in 5:ncol(METEO21)){#Solo las columnas con datos
    magnitud<-METEO21[myrow,]$magnitud
    valor<-METEO21[myrow,mycol]
    aux<-cbind(magnitud,valor)
    if(magnitud==81){
      df81<-rbind(df81,aux)
    }
    else if(magnitud==82){
      df82<-rbind(df82,aux)
    }
    else if(magnitud==83){
      df83<-rbind(df83,aux)
    }
    else if(magnitud==86){
      df86<-rbind(df86,aux)
    }
    else if(magnitud==87){
      df87<-rbind(df87,aux)
    }
    else if(magnitud==88){
      df88<-rbind(df88,aux)
    }
    else{
      df89<-rbind(df89,aux)
    }
    dfGlob<-rbind(dfGlob,aux)
    aux<-NULL
  }
}

#Boxplot velocidad viento
ggplot(data.frame(df81,na.rm=TRUE), aes(x=magnitud, y=valor, fill=factor(magnitud))) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11),
  ) +
  scale_fill_manual(values=c("#69b3a2", "grey")) +
  scale_alpha_manual(values=c(1,0.1)) +
  ggtitle("Analisis outliers Velocidad viento") +
  labs(x = "Magnitud",y = "Distribucion")


#Boxplot direccion viento
ggplot(data.frame(df82,na.rm=TRUE), aes(x=magnitud, y=valor, fill=factor(magnitud))) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Analisis outliers direccion del viento") +
  labs(x = "Magnitud",y = "Distribucion")

#Boxplot temperatura
ggplot(data.frame(df83,na.rm=TRUE), aes(x=magnitud, y=valor, fill=factor(magnitud))) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Analisis outliers temperaturas") +
  labs(x = "Magnitud", y = "Distribucion")


#Boxplot humedad relativa
ggplot(data.frame(df86,na.rm=TRUE), aes(x=magnitud, y=valor, fill=factor(magnitud))) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Analisis outliers humedad relativa") +
  labs(x = "Magnitud", y = "Distribucion")


#Boxplot presion barometrica
ggplot(data.frame(df87,na.rm=TRUE), aes(x=magnitud, y=valor, fill=factor(magnitud))) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Analisis outliers presion barometrica") +
  labs(x = "Magnitud", y = "Distribucion")


#Boxplot radiacion solar
ggplot(data.frame(df88,na.rm=TRUE), aes(x=magnitud, y=valor, fill=factor(magnitud))) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Analisis outliers radiacion solar") +
  labs(x = "Magnitud", y = "Distribucion")


#Boxplot precipitacion
ggplot(data.frame(df89,na.rm=TRUE), aes(x=magnitud, y=valor, fill=factor(magnitud))) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Analisis outliers precipitaciones") +
  labs(x = "Magnitud", y = "Distribucion")


#Representacion global
ggplot(data.frame(dfGlob,na.rm=TRUE), aes(x=magnitud, y=valor, fill=factor(magnitud))) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Analisis outliers globales") +
  labs(x = "Magnitudes", y = "Distribucion")


