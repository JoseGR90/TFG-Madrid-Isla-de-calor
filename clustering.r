#Clustering
rm(list=ls())

######Idea:::: en vez de cada punto es un dia en una estacion, agruparlos por mes y hacer su media
#por lo que despues cada punto indica mes y estacion, simplificamos el problema.

###############################################KMEANS
meteo21<-read.csv("Meteo/Diario/meteoDiario21_byDay.csv",sep=";", dec=",")
asocNombresNum<-read.csv("Meteo/estacion_distrito.csv",sep=";", dec=",")
#Puesto que hay muchas estaciones de las cuales tenemos bastantes nulos, vamos a realizar distintas
#aproximaciones dado el dataset que conseguimos del Ayuntamiento de Madrid

#1ª aproximacion: hay varias estaciones de las cuales disponemos datos de las 9 magnitudes
# 102, 103, 106, 106, 24, 59
vecEst<-c(102, 103, 106, 107, 24, 54, 56, 59)
sapply(meteo21, function(x) sum(is.na(x)))
dfFirst<-subset(meteo21,meteo21$Estacion %in% vecEst)
sapply(dfFirst, function(x) sum(is.na(x)))
#Quitamos las magnitudes 87 y 88 ya que tienen muchos NA
dfFirst<-dfFirst[,-c(10,11)]
for(i in 6:ncol(dfFirst)) {
  dfFirst[,i][is.na(dfFirst[,i])]<-median(dfFirst[,i],na.rm=TRUE)
}

#Ahora por estacion vamos a sacar la media de sus magnitudes.
dfMeans<-NULL#dataframe con las medias de las magnitudes de cada estacion
for(estacion in unique(dfFirst$Estacion)){
  estMean<-NULL
  a<-dfFirst[dfFirst$Estacion==estacion, ]
  estMean<-colMeans(a[6:10])
  dfMeans<-rbind(dfMeans,c(estacion,estMean))
}
dfMeans<-data.frame(dfMeans)
names(dfMeans)[1] <- 'Estacion'
Nombre<-rep(NA,8)
dfMeans<-cbind(Nombre,dfMeans)
for(myrow in 1:nrow(dfMeans)){#añadimos el nombre a la estacion
  dfMeans[myrow,]$Nombre<-asocNombresNum[asocNombresNum$ESTACION==dfMeans[myrow,]$Estacion,]$NombreEstacion
}

#Una vez que tenemos rellenado el dataframe al completo escalamos los datos
myKmeans<-data.frame(scale(dfMeans[3:7]))
library(cluster)
library(factoextra)
#Ahora vamos a calcular el numero optimo de clusters para el kMeans
#Para ello recurrimos al diagrama del codo.
#fviz_nbclust(myKmeans, kmeans, method = "wss")#No funciona ya que se generan mas centros de clusters que filas.
set.seed(123)
#Establecemos 3 clusters
kmeansResults<-kmeans(myKmeans, centers=4)
#Inercia entre grupos, nos dice la varianza entre los grupos, cuanto mayor mejor es la clasificación. Valor a maximizar.
kmeansResults$betweenss
#Inercia intragrupos, nos indica como de agrupados esta cada punto dentro de cada grupo, cuanto menor, mejor.
kmeansResults$withinss
#Inercia total intragrupos, es un valor a minimizar, cuanto menor es mejor.
kmeansResults$tot.withinss


estacion<-dfMeans$Nombre
grupo<-kmeansResults$cluster
datosRepresentacion<-data.frame(estacion, grupo)

library(ggplot2)
#Se ve basatante mal.
ggplot(datosRepresentacion)+
  geom_point(mapping=aes(x=grupo, y=estacion), color=grupo, size=1)

dfMeansNames <- dfMeans[,-1]
rownames(dfMeansNames) <- dfMeans[,1]
distance <- get_dist(dfMeansNames[,2:6])#Distancia entre estaciones sin normalizar los datos
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

dd <- dist(dfMeansNames[,2:3], method = "euclidean")
hc <- hclust(dd, method = "ward.D2")
plot(hc,hang = -1, cex = 0.4)
grp <- cutree(hc, k = 4)
# Visualize
plot(hc, cex = 0.6) # plot tree
rect.hclust(hc, k = 4, border = 2:5) # add rectangle

#En este dendograma y con la ayuda de la localizacion de las estaciones se ven relaciones
#Rojo --> Madrid sur colindante con la m40
#Verde --> Madrid exterior, fuera de la m40
#Azul oscuro --> Juancarlos I, parque en el exterior, poco efecto isla calor
#Azul claro --> Madrid norte, colindante a la m40


###########################################FIN 1ª Aproximacion

##############2ª Aproximacion: cogemos las estaciones que tienen completos las magnitudes 83 y 86
rm(list=ls())

meteo21<-read.csv("Meteo/Diario/meteoDiario21_byDay.csv",sep=";", dec=",")
asocNombresNum<-read.csv("Meteo/estacion_distrito.csv",sep=";", dec=",")
#Puesto que hay muchas estaciones de las cuales tenemos bastantes nulos, vamos a realizar distintas
#aproximaciones dado el dataset que conseguimos del Ayuntamiento de Madrid

#Estaciones con las magnitudes 83 y 86 completas
vecEst<-c(102, 103, 106, 107, 109, 110, 112, 8, 24, 35, 36, 38, 54, 56, 58, 59)
sapply(meteo21, function(x) sum(is.na(x)))
dfSecond<-subset(meteo21,meteo21$Estacion %in% vecEst)
#Quitamos las magnitudes que no nos interesan
dfSecond<-dfSecond[,-c(6,7,10,11,12)]
sapply(dfSecond, function(x) sum(is.na(x)))#1,7% de NA para la 83 y 2% para la 86 
for(i in 6:ncol(dfSecond)) {
  dfSecond[,i][is.na(dfSecond[,i])]<-median(dfSecond[,i],na.rm=TRUE)
}

#Ahora por estacion vamos a sacar la media de sus magnitudes.
dfMeans<-NULL#dataframe con las medias de las magnitudes de cada estacion
for(estacion in unique(dfSecond$Estacion)){
  estMean<-NULL
  a<-dfSecond[dfSecond$Estacion==estacion, ]
  estMean<-colMeans(a[6:7])
  dfMeans<-rbind(dfMeans,c(estacion,estMean))
}
dfMeans<-data.frame(dfMeans)
names(dfMeans)[1] <- 'Estacion'
Nombre<-rep(NA,16)
dfMeans<-cbind(Nombre,dfMeans)
for(myrow in 1:nrow(dfMeans)){#añadimos el nombre a la estacion
  dfMeans[myrow,]$Nombre<-asocNombresNum[asocNombresNum$ESTACION==dfMeans[myrow,]$Estacion,]$NombreEstacion
}

#Una vez que tenemos rellenado el dataframe al completo escalamos los datos
myKmeans<-data.frame(scale(dfMeans[3:4]))
library(cluster)
library(factoextra)
#Ahora vamos a calcular el numero optimo de clusters para el kMeans
#Para ello recurrimos al diagrama del codo.
fviz_nbclust(myKmeans, kmeans, method = "wss")#Recomendacion de 4 clusters
set.seed(123)
#Establecemos 3 clusters
kmeansResults<-kmeans(myKmeans, centers=4)
#Inercia entre grupos, nos dice la varianza entre los grupos, cuanto mayor mejor es la clasificación. Valor a maximizar.
kmeansResults$betweenss
#Inercia intragrupos, nos indica como de agrupados esta cada punto dentro de cada grupo, cuanto menor, mejor.
kmeansResults$withinss
#Inercia total intragrupos, es un valor a minimizar, cuanto menor es mejor.
kmeansResults$tot.withinss


estacion<-dfMeans$Nombre
grupo<-kmeansResults$cluster
datosRepresentacion<-data.frame(estacion, grupo)

library(ggplot2)
#Se ve basatante mal.
ggplot(datosRepresentacion)+
  geom_point(mapping=aes(x=grupo, y=estacion), color=grupo, size=1)

dfMeansNames <- dfMeans[,-1]
rownames(dfMeansNames) <- dfMeans[,1]
distance <- get_dist(dfMeansNames[,2:3])#Distancia entre estaciones sin normalizar los datos
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

dd <- dist(dfMeansNames[,2:3], method = "euclidean")
hc <- hclust(dd, method = "ward.D2")
plot(hc,hang = -1, cex = 0.4)
grp <- cutree(hc, k = 4)
# Visualize
plot(hc, cex = 0.6) # plot tree
rect.hclust(hc, k = 4, border = 2:5) # add rectangle

#Conclusiones
#Rojo --> son estaciones que se encuentran en zonas verdes, en las que la masa arborea es más elevada que en el resto de zonas.
#verde --> chamberí, madrid ciudad pero en la zona del norte.
#Azul oscuro --> estaciones que se situan en nucleos urbanos entre la m30 y m40
#Azul claro --> se tratan de estaciones que están entre el centro y la m30



###############################################Fin 2ª aproximacion
rm(list=ls())
###3ª aproximacion, ahor avamos a tener en cuenta las zonas verdes de cada una de las estaciones/distrito
###Lo malo------> asigno la misma zona verde a las distintas estaciones del mismo distrito?



###########################################DECISION TREE
#https://www.geeksforgeeks.org/decision-tree-in-r-programming/?ref=lbp
library(dplyr)
library(caTools)
library(party)
library(magrittr)
sample_data = sample.split(dfMeans[,3:9], SplitRatio = 0.8)
train_data <- subset(dfMeans[,3:9], sample_data == TRUE)
test_data <- subset(dfMeans[,3:9], sample_data == FALSE)
model<- ctree(X83 ~ .,dfMeans[,3:9])
plot(model)


predict_model<-predict(model, test_data)
m_at <- table(test_data$X83, predict_model)
m_at
