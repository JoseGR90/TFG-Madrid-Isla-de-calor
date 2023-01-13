rm(list=ls())
#Cargamos el archivo con los datos a limpiar
contamina21 <- read.csv("Contaminacion/Limpiar/contaminacion21.csv",sep=";")
contamina21Final<-NULL#Dataframe en el que vamos metiendo los datos limpios
datosInvalidos<-NULL#Dataframe que lleva el contador de datos invalidos por mes
for (myrow in 1:nrow(contamina21)){#Nos quedamos con los datos necesarios para identificar las filas
  estacion<-contamina21[myrow,]$ESTACION
  magnitud<-contamina21[myrow,]$MAGNITUD
  ano<-contamina21[myrow,]$ANO
  mes<-contamina21[myrow,]$MES
  finalrow<-cbind(estacion,magnitud,ano,mes)
  
  #Check los valores nulos
  i<-1#Necesario para saber en que columna vamos, para saber si es valor o validacion
  contInv<-0#Contador de valores nulos por mes
  for(mycol in 1:ncol(contamina21[myrow,])){
    if(i%%2==0){#Si se trata de un valor
      value<-contamina21[myrow,mycol]
    }
    else{#Se trata de su validacion
      if(contamina21[myrow,mycol]=='V'){#Valido
        finalrow<-cbind(finalrow,value)
      }
      else if(contamina21[myrow,mycol]=='N'){#Invalido, aumentamos el contador
        finalrow<-cbind(finalrow,NA)
        contInv<-contInv+1
      }
    }
    i<-i+1    
  }
  #Indexamos las columnas y filas al dataframe
  invRow<-cbind(estacion,magnitud,ano,mes,contInv)
  datosInvalidos<-rbind(datosInvalidos,invRow)
  contamina21Final<-rbind(contamina21Final,finalrow)
}
#Exportamos los datos en un csv
write.csv2(contamina21Final, "contaminacion/Diario/contaminaDiario21.csv")
write.csv2(datosInvalidos, "contaminacion/Diario/datosInvalidosDiarios.csv")
#########################################################
#########################################################
#########################################################
#########################################################
#########################################################
#Para rellenar los meses que se encuentran a medias, hay que añadir un total de 52 filas con 
#cada tupla estacion magnitud mes y sus NA
rm(list=ls())
contamina21 <- read.csv("contaminacion/Diario/contaminaDiario21.csv",sep=";")
contamina21<-contamina21[,-c(1)]

##Rellenado estacion 4, de las magnitudes 9,10,11, meses 1-12
##Magnitud 1 #########################################################
newRow <- c(4,1,2021,11,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
contamina21 <- rbind(contamina21[1:759, ], newRow, contamina21[- (1:759), ])
newRow <- c(4,1,2021,12,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
contamina21 <- rbind(contamina21[1:760, ], newRow, contamina21[- (1:760), ])
##Magnitud 8 ########################################################
newRow <- c(4,8,2021,11,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
contamina21 <- rbind(contamina21[1:761, ], newRow, contamina21[- (1:761), ])
newRow <- c(4,8,2021,12,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
contamina21 <- rbind(contamina21[1:762, ], newRow, contamina21[- (1:762), ])
#########################################################
#########################################################
################################Magnitud 1 ESTACION 17, todos los meses menos el 1
newRow <- c(17,1,2021,2,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
contamina21 <- rbind(contamina21[1:763, ], newRow, contamina21[- (1:763), ])
newRow <- c(17,1,2021,3,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
contamina21 <- rbind(contamina21[1:764, ], newRow, contamina21[- (1:764), ])
newRow <- c(17,1,2021,4,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
contamina21 <- rbind(contamina21[1:765, ], newRow, contamina21[- (1:765), ])
newRow <- c(17,1,2021,5,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
contamina21 <- rbind(contamina21[1:766, ], newRow, contamina21[- (1:766), ])
newRow <- c(17,1,2021,6,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
contamina21 <- rbind(contamina21[1:767, ], newRow, contamina21[- (1:767), ])
newRow <- c(17,1,2021,7,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
contamina21 <- rbind(contamina21[1:768, ], newRow, contamina21[- (1:768), ])
newRow <- c(17,1,2021,8,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
contamina21 <- rbind(contamina21[1:769, ], newRow, contamina21[- (1:769), ])
newRow <- c(17,1,2021,9,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
contamina21 <- rbind(contamina21[1:770, ], newRow, contamina21[- (1:770), ])
newRow <- c(17,1,2021,10,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
contamina21 <- rbind(contamina21[1:771, ], newRow, contamina21[- (1:771), ])
newRow <- c(17,1,2021,11,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
contamina21 <- rbind(contamina21[1:772, ], newRow, contamina21[- (1:772), ])
newRow <- c(17,1,2021,12,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
contamina21 <- rbind(contamina21[1:773, ], newRow, contamina21[- (1:773), ])
########################################################
#########################################################
#########################################################
################################Magnitud 1 ESTACION 24 menos el mes 1
newRow <- c(24,1,2021,2,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
contamina21 <- rbind(contamina21[1:774, ], newRow, contamina21[- (1:774), ])
newRow <- c(24,1,2021,3,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
contamina21 <- rbind(contamina21[1:775, ], newRow, contamina21[- (1:775), ])
newRow <- c(24,1,2021,4,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
contamina21 <- rbind(contamina21[1:776, ], newRow, contamina21[- (1:776), ])
newRow <- c(24,1,2021,5,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
contamina21 <- rbind(contamina21[1:777, ], newRow, contamina21[- (1:777), ])
newRow <- c(24,1,2021,6,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
contamina21 <- rbind(contamina21[1:778, ], newRow, contamina21[- (1:778), ])
newRow <- c(24,1,2021,7,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
contamina21 <- rbind(contamina21[1:779, ], newRow, contamina21[- (1:779), ])
newRow <- c(24,1,2021,8,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
contamina21 <- rbind(contamina21[1:780, ], newRow, contamina21[- (1:780), ])
newRow <- c(24,1,2021,9,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
contamina21 <- rbind(contamina21[1:781, ], newRow, contamina21[- (1:781), ])
newRow <- c(24,1,2021,10,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
contamina21 <- rbind(contamina21[1:782, ], newRow, contamina21[- (1:782), ])
newRow <- c(24,1,2021,11,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
contamina21 <- rbind(contamina21[1:783, ], newRow, contamina21[- (1:783), ])
newRow <- c(24,1,2021,12,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
contamina21 <- rbind(contamina21[1:784, ], newRow, contamina21[- (1:784), ])
#########################################################
######################################################
################################Magnitud 1 ESTACION 38 menos el mes1
newRow <- c(38,1,2021,2,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
contamina21 <- rbind(contamina21[1:785, ], newRow, contamina21[- (1:785), ])
newRow <- c(38,1,2021,3,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
contamina21 <- rbind(contamina21[1:786, ], newRow, contamina21[- (1:786), ])
newRow <- c(38,1,2021,4,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
contamina21 <- rbind(contamina21[1:787, ], newRow, contamina21[- (1:787), ])
newRow <- c(38,1,2021,5,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
contamina21 <- rbind(contamina21[1:788, ], newRow, contamina21[- (1:788), ])
newRow <- c(38,1,2021,6,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
contamina21 <- rbind(contamina21[1:789, ], newRow, contamina21[- (1:789), ])
newRow <- c(38,1,2021,7,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
contamina21 <- rbind(contamina21[1:790, ], newRow, contamina21[- (1:790), ])
newRow <- c(38,1,2021,8,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
contamina21 <- rbind(contamina21[1:791, ], newRow, contamina21[- (1:791), ])
newRow <- c(38,1,2021,9,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
contamina21 <- rbind(contamina21[1:792, ], newRow, contamina21[- (1:792), ])
newRow <- c(38,1,2021,10,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
contamina21 <- rbind(contamina21[1:793, ], newRow, contamina21[- (1:793), ])
newRow <- c(38,1,2021,11,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
contamina21 <- rbind(contamina21[1:794, ], newRow, contamina21[- (1:794), ])
newRow <- c(38,1,2021,12,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
contamina21 <- rbind(contamina21[1:795, ], newRow, contamina21[- (1:795), ])
#########################################################
################################Magniud 1 ESTACION 40 menos el mes 1
newRow <- c(40,1,2021,2,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
contamina21 <- rbind(contamina21[1:796, ], newRow, contamina21[- (1:796), ])
newRow <- c(40,1,2021,3,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
contamina21 <- rbind(contamina21[1:797, ], newRow, contamina21[- (1:797), ])
newRow <- c(40,1,2021,4,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
contamina21 <- rbind(contamina21[1:798, ], newRow, contamina21[- (1:798), ])
newRow <- c(40,1,2021,5,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
contamina21 <- rbind(contamina21[1:799, ], newRow, contamina21[- (1:799), ])
newRow <- c(40,1,2021,6,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
contamina21 <- rbind(contamina21[1:800, ], newRow, contamina21[- (1:800), ])
newRow <- c(40,1,2021,7,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
contamina21 <- rbind(contamina21[1:801, ], newRow, contamina21[- (1:801), ])
newRow <- c(40,1,2021,8,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
contamina21 <- rbind(contamina21[1:802, ], newRow, contamina21[- (1:802), ])
newRow <- c(40,1,2021,9,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
contamina21 <- rbind(contamina21[1:803, ], newRow, contamina21[- (1:803), ])
newRow <- c(40,1,2021,10,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
contamina21 <- rbind(contamina21[1:804, ], newRow, contamina21[- (1:804), ])
newRow <- c(40,1,2021,11,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
contamina21 <- rbind(contamina21[1:805, ], newRow, contamina21[- (1:805), ])
newRow <- c(40,1,2021,12,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
contamina21 <- rbind(contamina21[1:806, ], newRow, contamina21[- (1:806), ])
#########################################################
########################################################

names(contamina21)[5] <- 'Dia1'
names(contamina21)[6] <- 'Dia2'
names(contamina21)[7] <- 'Dia3'
names(contamina21)[8] <- 'Dia4'
names(contamina21)[9] <- 'Dia5'
names(contamina21)[10] <- 'Dia6'
names(contamina21)[11] <- 'Dia7'
names(contamina21)[12] <- 'Dia8'
names(contamina21)[13] <- 'Dia9'
names(contamina21)[14] <- 'Dia10'
names(contamina21)[15] <- 'Dia11'
names(contamina21)[16] <- 'Dia12'
names(contamina21)[17] <- 'Dia13'
names(contamina21)[18] <- 'Dia14'
names(contamina21)[19] <- 'Dia15'
names(contamina21)[20] <- 'Dia16'
names(contamina21)[21] <- 'Dia17'
names(contamina21)[22] <- 'Dia18'
names(contamina21)[23] <- 'Dia19'
names(contamina21)[24] <- 'Dia20'
names(contamina21)[25] <- 'Dia21'
names(contamina21)[26] <- 'Dia22'
names(contamina21)[27] <- 'Dia23'
names(contamina21)[28] <- 'Dia24'
names(contamina21)[29] <- 'Dia25'
names(contamina21)[30] <- 'Dia26'
names(contamina21)[31] <- 'Dia27'
names(contamina21)[32] <- 'Dia28'
names(contamina21)[33] <- 'Dia29'
names(contamina21)[34] <- 'Dia30'
names(contamina21)[35] <- 'Dia31'

write.csv2(contamina21, "contaminacion/Diario/contaminaDiario21_rellenadoNA.csv", row.names = FALSE)

############################################################
############################################################
############################################################
############################################################
rm(list=ls())
library(tidyr)
library(dplyr)
library(lubridate)
contamina21 <- read.csv("contaminacion/Diario/contaminaDiario21_RellenadoNA.csv",sep=";")
#Rellenamos fechas
Fecha<-data.frame(seq(as.Date("2021-1-1"), as.Date("2021-12-31"), by="days"))
dfFin<-cbind(Fecha)
names(dfFin)[1] <- 'Fecha'
#Separamos fechas
splitFech<-do.call("rbind", strsplit(as.character(dfFin$Fecha), "-", fixed = TRUE))
dfFin<-cbind(dfFin, splitFech)
names(dfFin)[2] <- 'Ano'
names(dfFin)[3] <- 'Mes'
names(dfFin)[4] <- 'Dia'
#Pasamos de char a double
dfFin$Ano <- as.numeric(as.character(dfFin$Ano))
dfFin$Mes <- as.numeric(as.character(dfFin$Mes))
dfFin$Dia <- as.numeric(as.character(dfFin$Dia))
#Rellenamos las magnitudes
dfMag<-NULL
for(x in 1:5){
  colu<-rep(NA, 365)
  dfMag<-cbind(dfMag,colu)
}
dfMag<-data.frame(dfMag)
names(dfMag)[1] <- '1'
names(dfMag)[2] <- '8'
names(dfMag)[3] <- '9'
names(dfMag)[4] <- '10'
names(dfMag)[5] <- '14'
#Puesto que hay 26 estacionesvamos a insertar 25 veces mas.
a<-dfFin
b<-dfMag
estaciones<-unique(contamina21$estacion)
rep<-length(estaciones)-1
for(i in 1:rep){
  dfFin<-rbind(dfFin, a)
  dfMag<-rbind(dfMag, b)
}
Estacion<-rep(NA, 365*length(estaciones)) #365*26
#Juntamos
dfFin<-cbind(dfFin,Estacion,dfMag)
#Rellenamos con las estaciones
init<-1
fin<-365
for(i in 1:length(estaciones)){
  est<-rep(estaciones[i], 365) #365*26
  dfFin[c(init:fin),]$Estacion<-est
  init<-init+365
  fin<-fin+365
}


#Rellenamos el dataframe final con los datos
magnitudes<-unique(contamina21$magnitud)
i<-4#En contamina los dias empiezan a partir de la 4 columna
for(x in 1:nrow(dfFin)){#Recorremos el df a rellenar
  myvecofmags<-NULL#Almacenamos en esta lista los 5 valores corresponcientes a las magnitudes
  for(mag in 1: length(magnitudes)){#Vamos a buscar cada una de las magnitudes
    mymag<-magnitudes[mag]
    #Esta sentencia lo que hace es confirmar si es un valor o si no existe en contamina21. 
    #Coge el valor correspondiente a la fila magnitud, estacion y mes, en la columna del dia+4, ya que en el formato de contamina21 los dias empiezan a partir de dicha columna.
    if(identical(contamina21[((contamina21$magnitud==mymag)&(contamina21$estacion==dfFin[x,]$Estacion) &(contamina21$mes==dfFin[x,]$Mes)),dfFin[x,]$Dia+i], character(0))){
      #Si no existe insertamos NA
      myvecofmags<-c(myvecofmags,NA)
    }
    else{
      myvecofmags<-c(myvecofmags,contamina21[((contamina21$magnitud==mymag)&(contamina21$estacion==dfFin[x,]$Estacion) &(contamina21$mes==dfFin[x,]$Mes)),dfFin[x,]$Dia+i])
      
    }
  }
  #Ponemos los valores del vector en su posiciony
  dfFin[x,c(6:10)]<-myvecofmags
}


#Exportamos el nuevo dataframe
write.csv2(dfFin, "contaminacion/Diario/contaminaDiario21_byDay.csv", row.names = FALSE)
