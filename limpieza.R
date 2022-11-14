rm(list=ls())
#Cargamos el archivo con los datos a limpiar
meteo21 <- read.csv("Meteo/Limpiar/meteo21.csv",sep=";")
meteo21Final<-NULL#Dataframe en el que vamos metiendo los datos limpios
datosInvalidos<-NULL#Dataframe que lleva el contador de datos invalidos por mes
for (myrow in 1:nrow(meteo21)){#Nos quedamos con los datos necesarios para identificar las filas
  estacion<-meteo21[myrow,]$ESTACION
  magnitud<-meteo21[myrow,]$MAGNITUD
  ano<-meteo21[myrow,]$ANO
  mes<-meteo21[myrow,]$MES
  finalrow<-cbind(estacion,magnitud,ano,mes)
  
  #Check los valores nulos
  i<-1#Necesario para saber en que columna vamos, para saber si es valor o validacion
  contInv<-0#Contador de valores nulos por mes
  for(mycol in 7:ncol(meteo21[myrow,])){
    if(i%%2==0){#Si se trata de un valor
      value<-meteo21[myrow,mycol]
    }
    else{#Se trata de su validacion
      if(meteo21[myrow,mycol]=='V'){#Valido
        finalrow<-cbind(finalrow,value)
      }
      else if(meteo21[myrow,mycol]=='N'){#Invalido, aumentamos el contador
        finalrow<-cbind(finalrow,NA)
        contInv<-contInv+1
      }
    }
    i<-i+1    
  }
  #Indexamos las columnas y filas al dataframe
  invRow<-cbind(estacion,magnitud,ano,mes,contInv)
  datosInvalidos<-rbind(datosInvalidos,invRow)
  meteo21Final<-rbind(meteo21Final,finalrow)
}
#Exportamos los datos en un csv
write.csv2(meteo21Final, "Meteo/Diario/meteoDiario21.csv")
write.csv2(datosInvalidos, "Meteo/Diario/datosInvalidosDiarios.csv")
#########################################################
#########################################################
#########################################################
#########################################################
#########################################################
#Para rellenar los meses que se encuentran a medias, hay que añadir un total de 52 filas con 
#cada tupla estacion magnitud mes y sus NA
rm(list=ls())
meteo21 <- read.csv("Meteo/Diario/meteoDiario21.csv",sep=";")
meteo21<-meteo21[,-c(1)]

##Rellenado estacion 108, de las magnitudes 81-89, meses 6,7,8
##Magnitud 81 #########################################################
####PONER COMENTARIOS USO
newRow <- c(108,81,2021,6,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
meteo21 <- rbind(meteo21[1:365, ], newRow, meteo21[- (1:365), ])
newRow <- c(108,81,2021,7,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
meteo21 <- rbind(meteo21[1:366, ], newRow, meteo21[- (1:366), ])
newRow <- c(108,81,2021,8,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
meteo21 <- rbind(meteo21[1:367, ], newRow, meteo21[- (1:367), ])
##Magnitud 82 #########################################################
newRow <- c(108,82,2021,6,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
meteo21 <- rbind(meteo21[1:377, ], newRow, meteo21[- (1:377), ])
newRow <- c(108,82,2021,7,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
meteo21 <- rbind(meteo21[1:378, ], newRow, meteo21[- (1:378), ])
newRow <- c(108,82,2021,8,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
meteo21 <- rbind(meteo21[1:379, ], newRow, meteo21[- (1:379), ])
##Magnitud 83 #########################################################
newRow <- c(108,83,2021,6,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
meteo21 <- rbind(meteo21[1:389, ], newRow, meteo21[- (1:389), ])
newRow <- c(108,83,2021,7,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
meteo21 <- rbind(meteo21[1:390, ], newRow, meteo21[- (1:390), ])
newRow <- c(108,83,2021,8,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
meteo21 <- rbind(meteo21[1:391, ], newRow, meteo21[- (1:391), ])
##Magnitud 86 #########################################################
newRow <- c(108,86,2021,6,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
meteo21 <- rbind(meteo21[1:401, ], newRow, meteo21[- (1:401), ])
newRow <- c(108,86,2021,7,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
meteo21 <- rbind(meteo21[1:402, ], newRow, meteo21[- (1:402), ])
newRow <- c(108,86,2021,8,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
meteo21 <- rbind(meteo21[1:403, ], newRow, meteo21[- (1:403), ])
##Magnitud 87 #########################################################
newRow <- c(108,87,2021,6,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
meteo21 <- rbind(meteo21[1:413, ], newRow, meteo21[- (1:413), ])
newRow <- c(108,87,2021,7,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
meteo21 <- rbind(meteo21[1:414, ], newRow, meteo21[- (1:414), ])
newRow <- c(108,87,2021,8,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
meteo21 <- rbind(meteo21[1:415, ], newRow, meteo21[- (1:415), ])
##Magnitud 88 #########################################################
newRow <- c(108,88,2021,6,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
meteo21 <- rbind(meteo21[1:425, ], newRow, meteo21[- (1:425), ])
newRow <- c(108,88,2021,7,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
meteo21 <- rbind(meteo21[1:426, ], newRow, meteo21[- (1:426), ])
newRow <- c(108,88,2021,8,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
meteo21 <- rbind(meteo21[1:427, ], newRow, meteo21[- (1:427), ])
##Magnitud 89 #########################################################
newRow <- c(108,89,2021,6,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
meteo21 <- rbind(meteo21[1:437, ], newRow, meteo21[- (1:437), ])
newRow <- c(108,89,2021,7,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
meteo21 <- rbind(meteo21[1:438, ], newRow, meteo21[- (1:438), ])
newRow <- c(108,89,2021,8,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
meteo21 <- rbind(meteo21[1:439, ], newRow, meteo21[- (1:439), ])
#########################################################
#########################################################
#########################################################
##Rellenado estacion 111, de las magnitudes 83 y 86, meses 7,8,9,10,11,12
##Magnitud 83 #########################################################
newRow <- c(111,83,2021,7,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
meteo21 <- rbind(meteo21[1:498, ], newRow, meteo21[- (1:498), ])
newRow <- c(111,83,2021,8,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
meteo21 <- rbind(meteo21[1:499, ], newRow, meteo21[- (1:499), ])
newRow <- c(111,83,2021,9,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
meteo21 <- rbind(meteo21[1:500, ], newRow, meteo21[- (1:500), ])
newRow <- c(111,83,2021,10,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
meteo21 <- rbind(meteo21[1:501, ], newRow, meteo21[- (1:501), ])
newRow <- c(111,83,2021,11,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
meteo21 <- rbind(meteo21[1:502, ], newRow, meteo21[- (1:502), ])
newRow <- c(111,83,2021,12,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
meteo21 <- rbind(meteo21[1:503, ], newRow, meteo21[- (1:503), ])
##Magnitud 86 #########################################################
newRow <- c(111,86,2021,7,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
meteo21 <- rbind(meteo21[1:510, ], newRow, meteo21[- (1:510), ])
newRow <- c(111,86,2021,8,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
meteo21 <- rbind(meteo21[1:511, ], newRow, meteo21[- (1:511), ])
newRow <- c(111,86,2021,9,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
meteo21 <- rbind(meteo21[1:512, ], newRow, meteo21[- (1:512), ])
newRow <- c(111,86,2021,10,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
meteo21 <- rbind(meteo21[1:513, ], newRow, meteo21[- (1:513), ])
newRow <- c(111,86,2021,11,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
meteo21 <- rbind(meteo21[1:514, ], newRow, meteo21[- (1:514), ])
newRow <- c(111,86,2021,12,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
meteo21 <- rbind(meteo21[1:515, ], newRow, meteo21[- (1:515), ])
#########################################################
#########################################################
#########################################################
##Rellenado estacion 114, de las magnitudes 83 y 86, meses 4,5,6,7,8,9,10
##Magnitud 83 #########################################################
##Magnitud 83 #########################################################
newRow <- c(114,83,2021,4,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
meteo21 <- rbind(meteo21[1:567, ], newRow, meteo21[- (1:567), ])
newRow <- c(114,83,2021,5,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
meteo21 <- rbind(meteo21[1:568, ], newRow, meteo21[- (1:568), ])
newRow <- c(114,83,2021,6,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
meteo21 <- rbind(meteo21[1:569, ], newRow, meteo21[- (1:569), ])
newRow <- c(114,83,2021,7,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
meteo21 <- rbind(meteo21[1:570, ], newRow, meteo21[- (1:570), ])
newRow <- c(114,83,2021,8,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
meteo21 <- rbind(meteo21[1:571, ], newRow, meteo21[- (1:571), ])
newRow <- c(114,83,2021,9,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
meteo21 <- rbind(meteo21[1:572, ], newRow, meteo21[- (1:572), ])
newRow <- c(114,83,2021,10,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
meteo21 <- rbind(meteo21[1:573, ], newRow, meteo21[- (1:573), ])
##Magnitud 86 #########################################################
newRow <- c(114,86,2021,4,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
meteo21 <- rbind(meteo21[1:579, ], newRow, meteo21[- (1:579), ])
newRow <- c(114,86,2021,5,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
meteo21 <- rbind(meteo21[1:580, ], newRow, meteo21[- (1:580), ])
newRow <- c(114,86,2021,6,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
meteo21 <- rbind(meteo21[1:581, ], newRow, meteo21[- (1:581), ])
newRow <- c(114,86,2021,7,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
meteo21 <- rbind(meteo21[1:582, ], newRow, meteo21[- (1:582), ])
newRow <- c(114,86,2021,8,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
meteo21 <- rbind(meteo21[1:583, ], newRow, meteo21[- (1:583), ])
newRow <- c(114,86,2021,9,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
meteo21 <- rbind(meteo21[1:584, ], newRow, meteo21[- (1:584), ])
newRow <- c(114,86,2021,10,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
meteo21 <- rbind(meteo21[1:585, ], newRow, meteo21[- (1:585), ])
#########################################################
#########################################################
#########################################################
##Rellenado estacion 115, de las magnitudes 83 y 86, mes 10
##Magnitud 83 #########################################################
newRow <- c(115,83,2021,10,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
meteo21 <- rbind(meteo21[1:597, ], newRow, meteo21[- (1:597), ])
##Magnitud 86 #########################################################
newRow <- c(115,86,2021,10,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
meteo21 <- rbind(meteo21[1:609, ], newRow, meteo21[- (1:609), ])
#########################################################
#########################################################
#########################################################
##Rellenado estacion 4, de las magnitudes 83 y 86, meses 11,12
##Magnitud 83 #########################################################
newRow <- c(4,83,2021,11,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
meteo21 <- rbind(meteo21[1:623, ], newRow, meteo21[- (1:623), ])
newRow <- c(4,83,2021,12,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
meteo21 <- rbind(meteo21[1:624, ], newRow, meteo21[- (1:624), ])
#########################################################
#########################################################
#########################################################
##Rellenado estacion 24, de las magnitudes 87, meses 2
##Magnitud 87 #########################################################
newRow <- c(4,87,2021,2,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
meteo21 <- rbind(meteo21[1:721, ], newRow, meteo21[- (1:721), ])
write.csv2(meteo21, "Meteo/Diario/meteoDiario21_rellenadoNA.csv", row.names = FALSE)


############################################################
############################################################
############################################################
############################################################
rm(list=ls())
library(readxl)
zonasVerdes<- read_excel("Zonas Verdes/Limpiar/MasasZonasVerdesDistritosCalles_2021.xlsx")
zonasVerdes<-zonasVerdes[-c(22:30), ]
colnames(zonasVerdes)[1] <- "NumDistr"
colnames(zonasVerdes)[4] <- "NumMasas"
colnames(zonasVerdes)[5] <- "Superficiem2"
colnames(zonasVerdes)[6] <- "Superficieha"
write.csv2(zonasVerdes, "Zonas Verdes/masaArborea21.csv", row.names = FALSE)

zonasVerdes<- read_excel("Zonas Verdes/Limpiar/EstadoZonasVerdesDistritosCalles_2021.xlsx")
zonasVerdes<-zonasVerdes[-c(22:33), ]
write.csv2(zonasVerdes, "Zonas Verdes/estadoZonasVerdes21.csv", row.names = FALSE)

zonasVerdes<- read_excel("Zonas Verdes/Limpiar/Estado_ARBOLADO_ParquesHistoricoSingularesForestales_2021.xlsx")
zonasVerdes<-zonasVerdes[-c(17:23), ]
write.csv2(zonasVerdes, "Zonas Verdes/estadoArbolado21.csv", row.names = FALSE)

