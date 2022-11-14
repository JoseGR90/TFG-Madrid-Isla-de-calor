rm(list=ls())
#Analisis valores nulos y distribuciones
library(ggplot2)
library(tidyr)
library(dplyr)
library(ggpubr)
#Archivo con los datos a analizar
METEO21<-read.csv("Meteo/Diario/meteoDiario21.csv",sep=";", dec=",")
VALNA<-read.csv("Meteo/Diario/datosInvalidosDiarios.csv",sep=";", dec=",")#csv que contiene la tabla con 
#la distribucion de fallos por cada tupla estacion, mes, magnitud. Este fichero se crea en el
#script limpieza.r
#Vamos a calcular las distribuciones de los datos, una por cada magnitud. Primero sacamos
#todos los valores del DF principal con 2 bucles y los insertamos en una variable.
#Agrupamos en un dataframe y representamos esos valores en un diagrama de densidad.

#Fallos mensuales por estacion y magnitud, con esta gráfica se pretende saber como se reparten los
#datos nulos regogidos por la tupla(estación, magnitud, mes)
plotNA<-ggplot(data.frame(VALNA),aes(x=contInv)) +
  geom_density(fill="red", color="red")+
  xlim(-1,10)+
  labs(x = "NA (estación, magnitud, mes)", y = "Densidad") +
  ggtitle("Distribución de los valores NA")

#Velocidad del viento
hist81<-NULL
for (myrow in 1:nrow(METEO21)){
  if((METEO21[myrow,]$magnitud == 81)){
    for(mycol in 6:ncol(METEO21[myrow,])){
      hist81<-rbind(hist81,METEO21[myrow,mycol])#Lo guarda como un double
    }
  }
}

plot81<-ggplot(data.frame(hist81),aes(x=hist81)) +
  geom_density(fill="#69b3a2", color="#e9ecef")+
  labs(x = "Velocidad del viento (m/s)", y = "Densidad") +
  ggtitle("Distribución de la velocidad del viento")


#Direccion del viento, de 0º a 360º
hist82<-NULL
for (myrow in 1:nrow(METEO21)){
  if((METEO21[myrow,]$magnitud == 82)){
    for(mycol in 6:ncol(METEO21[myrow,])){
      hist82<-rbind(hist82,METEO21[myrow,mycol])
    }
  }
}
plot82<-ggplot(data.frame(hist82),aes(x=hist82)) +
  geom_density(fill="cadetblue3", color="cadetblue3")+
  labs(x = "Dirección del viento (0º-360º)", y = "Densidad") +
  ggtitle("Distribución de la dirección del viento")
#Predomina el viento del suroeste

#Temperatura
hist83<-NULL
for (myrow in 1:nrow(METEO21)){
  if((METEO21[myrow,]$magnitud == 83)){
    for(mycol in 6:ncol(METEO21[myrow,])){
      hist83<-rbind(hist83,METEO21[myrow,mycol])
    }
  }
}
plot83<-ggplot(data.frame(hist83),aes(x=hist83))+
  geom_density(fill="coral2", color="#e9ecef")+
  labs(x = "Temperatura (ºC)", y = "Densidad") +
  ggtitle("Distribución de la Temperatura")


#Humedad relativa
hist86<-NULL
for (myrow in 1:nrow(METEO21)){
  if((METEO21[myrow,]$magnitud == 86)){
    for(mycol in 6:ncol(METEO21[myrow,])){
      hist86<-rbind(hist86,METEO21[myrow,mycol])
    }
  }
}
plot86<-ggplot(data.frame(hist86),aes(x=hist86))+
  geom_density(fill="cyan2", color="#e9ecef")+
  labs(x = "Humedad (%)", y = "Densidad") +
  ggtitle("Distribución de la humedad relativa")


#Presion barometrica
hist87<-NULL
for (myrow in 1:nrow(METEO21)){
  if((METEO21[myrow,]$magnitud == 87)){
    for(mycol in 6:ncol(METEO21[myrow,])){
      hist87<-rbind(hist87,METEO21[myrow,mycol])
    }
  }
}
plot87<-ggplot(data.frame(hist87),aes(x=hist87))+
  geom_density(fill="darkgrey", color="#e9ecef")+
  labs(x = "Presión (mbar)", y = "Densidad") +
  ggtitle("Distribución de la presión barométrica")

#Radiación solar
hist88<-NULL
for (myrow in 1:nrow(METEO21)){
  if((METEO21[myrow,]$magnitud == 88)){
    for(mycol in 6:ncol(METEO21[myrow,])){
      hist88<-rbind(hist88,METEO21[myrow,mycol])
    }
  }
}
plot88<-ggplot(data.frame(hist88),aes(x=hist88))+
  geom_density(fill="darkgoldenrod1", color="#e9ecef")+
  labs(x = "Radiación (W/m2)", y = "Densidad") +
  xlim(-20,400)+
  ggtitle("Distribución de la radiación solar")

#Precipitacion
hist89<-NULL
for (myrow in 1:nrow(METEO21)){
  if((METEO21[myrow,]$magnitud == 89)){
    for(mycol in 6:ncol(METEO21[myrow,])){
      hist89<-rbind(hist89,METEO21[myrow,mycol])
    }
  }
}
plot89<-ggplot(data.frame(hist89),aes(x=hist89))+
  geom_density(fill="cyan2", color="#e9ecef")+
  xlim(-3,15)+
  labs(x = "Precipitación (L/m2)", y = "Densidad") +
  ggtitle("Distribución de las precipitaciones")


ggarrange(plotNA,plot81, plot82, plot83,plot86, plot87,plot88, plot89,
          ncol = 4, nrow = 2)

