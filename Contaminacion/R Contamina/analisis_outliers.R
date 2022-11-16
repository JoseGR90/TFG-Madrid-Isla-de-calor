#Box plots para analizar los outliers
rm(list=ls())
library(tidyverse)
library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(viridis)
CONTAMINA21<-read.csv("Contaminacion/Diario/contamina21RellenoNA.csv",sep=";",dec=",")
#Este archivo es el sacado a partir de la limpieza, en el que los meses que no tienen valores
#por fallo o mantenimiento son rellenados a NA.

#Para ello vamos a agrupar los datos de cada estacion por cada magnitud
#Para poder repesentar los box plots necesitamos un DF con 2 columnas, una del id de las magnitudes
#y la otra con el valor de dicha magnitud.
dfGlob<-NULL
df1<-NULL
df8<-NULL
df9<-NULL
df10<-NULL
df14<-NULL

for(myrow in 1:nrow(CONTAMINA21)){#Recorremos todas las filas
  for(mycol in 5:ncol(CONTAMINA21)){#Solo las columnas con datos
    magnitud<-CONTAMINA21[myrow,]$magnitud
    valor<-CONTAMINA21[myrow,mycol]
    aux<-cbind(magnitud,valor)
    if(magnitud==1){
      df1<-rbind(df1,aux)
    }
    else if(magnitud==8){
      df8<-rbind(df8,aux)
    }
    else if(magnitud==9){
      df9<-rbind(df9,aux)
    }
    else if(magnitud==10){
      df10<-rbind(df10,aux)
    }
    else{
      df14<-rbind(df14,aux)
    }
    dfGlob<-rbind(dfGlob,aux)
    aux<-NULL
  }
}

#Boxplot Dioxido de Azufre
ggplot(data.frame(df1,na.rm=TRUE), aes(x=magnitud, y=valor, fill=factor(magnitud))) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11),
  ) +
  scale_fill_manual(values=c("#69b3a2", "grey")) +
  scale_alpha_manual(values=c(1,0.1)) +
  ggtitle("Analisis outliers de Dioxido de Azufre ") +
  labs(x = "Magnitud",y = "Distribucion")


#Boxplot Dioxido de Nitrogeno
ggplot(data.frame(df8,na.rm=TRUE), aes(x=magnitud, y=valor, fill=factor(magnitud))) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Analisis outliers de Dioxido de Nitrogeno ") +
  labs(x = "Magnitud",y = "Distribucion")

#Boxplot Particulas 2.5
ggplot(data.frame(df9,na.rm=TRUE), aes(x=magnitud, y=valor, fill=factor(magnitud))) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Analisis outliers de Particulas 2.5") +
  labs(x = "Magnitud", y = "Distribucion")


#Boxplot Particulas 10
ggplot(data.frame(df10,na.rm=TRUE), aes(x=magnitud, y=valor, fill=factor(magnitud))) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Analisis outliers de Particulas 10") +
  labs(x = "Magnitud", y = "Distribucion")


#Boxplot Ozono
ggplot(data.frame(df14,na.rm=TRUE), aes(x=magnitud, y=valor, fill=factor(magnitud))) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Analisis outliers de Ozono") +
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


