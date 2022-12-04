#Clustering
rm(list=ls())

######Idea:::: en vez de cada punto es un dia en una estacion, agruparlos por mes y hacer su media
#por lo que despues cada punto indica mes y estacion, simplificamos el problema.

############################################KMEANS
#Cargamos los datos que hemos limpiado y los cuales hemos dado formato.
meteo21<-read.csv("Meteo/Diario/meteoDiario21_byDay.csv",sep=";", dec=",")
asocNombresNum<-read.csv("Meteo/estacion_distrito.csv",sep=";", dec=",")
#Puesto que desde el portal de datos del ayuntamiento hay numerosos valores nulos y este algoritmo
#requiere que sean datos cuantitativos, es decir no puden haber NA, hemos decidido 
#imputar datos la mediana de los datos. 
#https://www.uv.es/webgid/Descriptiva/23_valores_faltantes.html
#Problemas-> como faltan una cantidad increible de datos, creo que es mejor idea hacer el kmeans
#por cada dataframe, es decir, un kmeans por contaminacion y otro por meteorologia.
for(i in 6:ncol(meteo21)) {
  meteo21[,i][is.na(meteo21[,i])]<-median(meteo21[,i],na.rm=TRUE)
}

#Ahora por estacion vamos a sacar la media de sus magnitudes.

dfMeans<-NULL#dataframe con las medias de las magnitudes de cada estacion
for(estacion in unique(meteo21$Estacion)){
  estMean<-NULL
  a<-meteo21[meteo21$Estacion==estacion, ]
  estMean<-colMeans(a[6:12])
  dfMeans<-rbind(dfMeans,c(estacion,estMean))
}
dfMeans<-data.frame(dfMeans)
names(dfMeans)[1] <- 'Estacion'
Nombre<-rep(NA,26)
dfMeans<-cbind(Nombre,dfMeans)
for(myrow in 1:nrow(dfMeans)){#añadimos el nombre a la estacion
  dfMeans[myrow,]$Nombre<-asocNombresNum[asocNombresNum$ESTACION==dfMeans[myrow,]$Estacion,]$X
}

#Una vez que tenemos rellenado el dataframe al completo escalamos los datos
myKmeans<-data.frame(scale(dfMeans[3:9]))
library(cluster)
library(factoextra)
#Ahora vamos a calcular el numero optimo de clusters para el kMeans
#Para ello recurrimos al diagrama del codo.
fviz_nbclust(myKmeans, kmeans, method = "wss")#Tarda en ejecutarse; el "codo" se encuentra en 4 clusters, por lo que lo ejecutamos tal que asi.
set.seed(123)
#Analizar si esta bien con 3 clusters, 2 grupos muy grandes y uno muy pequeño.
kmeansResults<-kmeans(myKmeans, centers=4)
#Inercia entre grupos, nos dice la varianza entre los grupos, cuanto mayor mejor es la clasificación. Valor a maximizar.
kmeansResults$betweenss
#Inercia intragrupos, nos indica como de agrupados esta cada punto dentro de cada grupo, cuanto menor, mejor.
kmeansResults$withinss
#Inercia total intragrupos, es un valor a minimizar, cuanto menor es mejor.
kmeansResults$tot.withinss
#101 la maximizada, 73 la minimizada

estacion<-dfMeans$Nombre
grupo<-kmeansResults$cluster
datosRepresentacion<-data.frame(estacion, grupo)

library(ggplot2)
#Se ve basatante mal.
ggplot(datosRepresentacion)+
  geom_point(mapping=aes(x=grupo, y=estacion), color=grupo, size=1)

dfMeansNames <- dfMeans[,-1]
rownames(dfMeansNames) <- dfMeans[,1]
distance <- get_dist(dfMeansNames[,2:8])#Distancia entre estaciones sin normalizar los datos
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

dd <- dist(dfMeansNames[,2:8], method = "euclidean")
hc <- hclust(dd, method = "ward.D2")
plot(hc,hang = -1, cex = 0.4)
grp <- cutree(hc, k = 4)
# Visualize
plot(hc, cex = 0.6) # plot tree
rect.hclust(hc, k = 4, border = 2:5) # add rectangle

###########################################FIN KMEANS


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
