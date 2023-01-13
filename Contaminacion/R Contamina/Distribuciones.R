#Analisis valores nulos y outliers
library(ggplot2)
library(tidyr)
library(dplyr)
library(ggpubr)
#Archivo con los datos a analizar
CONTAMINA21<-read.csv("Contaminacion/Diario/contamina21Final.csv",sep=";", dec=",")
VALNA<-read.csv("Contaminacion/Diario/datosInvalidosDiarios.csv",sep=";", dec=",")
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

#Dioxido de Azufre
hist1<-NULL
for (myrow in 1:nrow(CONTAMINA21)){
  if((CONTAMINA21[myrow,]$magnitud == 1)){
    for(mycol in 6:ncol(CONTAMINA21[myrow,])){
      hist1<-rbind(hist1,CONTAMINA21[myrow,mycol])#Lo guarda como un double
    }
  }
}

plot1<-ggplot(data.frame(hist1),aes(x=hist1)) +
  geom_density(fill="#69b3a2", color="#e9ecef")+
  labs(x = "Cantidad de SO2", y = "Densidad") +
  ggtitle("Distribución de Dioxido de Azufre")


#Dioxido de Nitrogeno
hist8<-NULL
for (myrow in 1:nrow(CONTAMINA21)){
  if((CONTAMINA21[myrow,]$magnitud == 8)){
    for(mycol in 6:ncol(CONTAMINA21[myrow,])){
      hist8<-rbind(hist8,CONTAMINA21[myrow,mycol])
    }
  }
}
plot8<-ggplot(data.frame(hist8),aes(x=hist8)) +
  geom_density(fill="cadetblue3", color="cadetblue3")+
  labs(x = "Cantidad de NO2", y = "Densidad") +
  ggtitle("Distribución de Dioxido de Nitrogeno")


#Particulas 2.5
hist9<-NULL
for (myrow in 1:nrow(CONTAMINA21)){
  if((CONTAMINA21[myrow,]$magnitud == 9)){
    for(mycol in 6:ncol(CONTAMINA21[myrow,])){
      hist9<-rbind(hist9,CONTAMINA21[myrow,mycol])
    }
  }
}
plot9<-ggplot(data.frame(hist9),aes(x=hist9))+
  geom_density(fill="coral2", color="#e9ecef")+
  labs(x = "Cantidad de PM 2.5", y = "Densidad") +
  ggtitle("Distribución Particulas 2.5 mm")


#Particulas 10
hist10<-NULL
for (myrow in 1:nrow(CONTAMINA21)){
  if((CONTAMINA21[myrow,]$magnitud == 10)){
    for(mycol in 6:ncol(CONTAMINA21[myrow,])){
      hist10<-rbind(hist10,CONTAMINA21[myrow,mycol])
    }
  }
}
plot10<-ggplot(data.frame(hist10),aes(x=hist10))+
  geom_density(fill="cyan2", color="#e9ecef")+
  labs(x = "Cantidad de PM 10", y = "Densidad") +
  ggtitle("Distribución Particulas 10 mm")


#Ozono
hist14<-NULL
for (myrow in 1:nrow(CONTAMINA21)){
  if((CONTAMINA21[myrow,]$magnitud == 14)){
    for(mycol in 6:ncol(CONTAMINA21[myrow,])){
      hist14<-rbind(hist14,CONTAMINA21[myrow,mycol])
    }
  }
}
plot14<-ggplot(data.frame(hist14),aes(x=hist14))+
  geom_density(fill="blue", color="#e9ecef")+
  labs(x = "Cantidad de PM O3", y = "Densidad") +
  ggtitle("Distribucion de Ozono")



ggarrange(plotNA,plot1, plot8, plot9,plot10, plot14,
          ncol = 4, nrow = 2)

