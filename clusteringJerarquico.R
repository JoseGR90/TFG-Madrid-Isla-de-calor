#Clustering
rm(list=ls())
library(imputeR)
#Cargamos los datos que hemos limpiado y los cuales hemos dado formato.
allData<-read.csv("Datos Finales/datos21_byDay.csv",sep=";",dec=",")
meteo21<-read.csv("Meteo/Diario/meteoDiario21_byDay.csv",sep=";", dec=",")
contaminacion21<-read.csv("Contaminacion/Diario/contaminaDiario21_byDay.csv",sep=";", dec=",")





# AGRUPACION DE LOS DATOS POR Estaciones 
#agrupamos por estaciones, se obtienen los casos totales
library(dplyr)
names(meteo21)
names(meteo21)[6]<-"Radiacion"
names(meteo21)[7]<-"Viento"
names(meteo21)[8]<-"Temperatura"
names(meteo21)[9]<-"Humedad"
names(meteo21)[10]<-"Presion"
names(meteo21)[11]<-"Solar"
names(meteo21)[12]<-"Precipitacion"

EstacionMeteo<-group_by(meteo21, Estacion)
EstacionMeteo<-summarize(EstacionMeteo, count=n(),
                                 Radiacion=median(Radiacion, na.rm=T),
                                 Viento=median(Viento, na.rm=T),
                                 Temperatura=median(Temperatura, na.rm=T),
                                 Humedad=median(Humedad, na.rm=T),
                                 Presion=median(Presion,na.rm = T),
                                 Solar=median(Solar, na.rm=T),
                                 Precipitacion=median(Precipitacion,na.rm = T))
#########CLUSTERING MST

porEstacions<-hclust(dist(EstacionMeteo,method="euclidean"),method="single")
plot(porEstacions)
plot(hclust(dist(EstacionMeteo,method="euclidean"),method="single"))

# AGRUPACION DE LOS DATOS POR Estaciones 
#agrupamos por estaciones, se obtienen los casos totales
library(dplyr)
names(contaminacion21)
names(contaminacion21)[6]<-"Azufre"
names(contaminacion21)[7]<-"Nitrogeno"
names(contaminacion21)[8]<-"Particulas2.5"
names(contaminacion21)[9]<-"Particulas10"
names(contaminacion21)[10]<-"Ozono"


Estacioncontaminacion<-group_by(contaminacion21, Estacion)
Estacioncontaminacion<-summarize(Estacioncontaminacion, count=n(),
                                   Azufre=sum(Azufre, na.rm=T),
                                   Nitrogeno=sum(Nitrogeno, na.rm=T),
                                   Particulas2.5=sum(Particulas2.5, na.rm=T),
                                   Particulas10=sum(Particulas10, na.rm=T),
                                   Ozono=sum(Ozono,na.rm = T))


#########CLUSTERING MST

porEstacions<-hclust(dist(Estacioncontaminacion,method="euclidean"),method="single")
plot(porEstacions)
plot(hclust(dist(Estacioncontaminacion,method="euclidean"),method="single"))


















#########CLUSTERING K-Vecinos

install.packages("rpart.plot")
install.packages("useful")
install.packages("randomForest")


# Para representar gráficamente la relación entre variables
library("ggplot2")
# Para clasificar con K-NN
library("class")
# Para clasificar con rpart
library("rpart")
library("rpart.plot")
# Para clasificar con randomForest
library("useful")
library("randomForest")

# 1.Calculamos los descriptivos univariables de las variables del fichero
summary(contaminacion21)

# 2.Representamos gráficamente las variables del fichero mediante histogramas

#Histograma Ozono
f1 <- hist(contaminacion21$Ozono, main="Histograma Ozono", col = "gray", labels = TRUE) 

#Histograma Azufre
f2 <- hist(contaminacion21$Azufre, main="Histograma Azufre", col = "gray", labels = TRUE) 

#Histograma Particulas10
f3 <- hist(contaminacion21$Particulas10, main="Histograma Particulas10", col = "gray", labels = TRUE) 




# Estudiamos la relación entre variables mediante gráficos de dispersión
f7<-ggplot(contaminacion21, aes(x=Ozono, y=Azufre)) + geom_point()
f7


# 3.Estudiamos la existencia de diferencias por estaciones

# promedio variables por estacion 
tapply(contaminacion21$Ozono,contaminacion21$Estacion,mean)

# Relación entre variables Ozono y Particulas con tamaño Temperatura y Color según Estacion
#f10<-ggplot(contaminacion21, aes(x=Ozono, y=Particulas10, color=Estacioncontaminacion)) + geom_point(aes(size=meteo21.Temperatura))
#f10



# Dividimos el fichero en 70% entreno y 30% validación  #
set.seed(1234)
ind <- sample(2, nrow(contaminacion21), replace=TRUE, prob=c(0.7, 0.3))
trainData <- contaminacion21[ind==1,]
testData <- contaminacion21[ind==2,]

# Aplicamos el algoritmo K-NN seleccionando 1 como k inicial
KnnTestPrediccion_k1 <- knn(trainData[,5:10],testData[,5:10], trainData$Estacion , k = 5, prob = TRUE )
# Visualizamos una matriz de confusión
table ( testData$Estacion , KnnTestPrediccion_k1 )