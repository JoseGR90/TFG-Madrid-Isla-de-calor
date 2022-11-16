rm(list=ls())
#Cargamos el archivo con los datos a limpiar
contamina21 <- read.csv("Contaminacion/Limpiar/contaminacion21.csv",sep=";")
mydata<-NULL#Dataframe en el que vamos metiendo los datos limpios
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
    else{
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
  mydata<-rbind(mydata,finalrow)
}
contamina21Final<-mydata
#Exportamos los datos en un csv
write.csv2(contamina21Final, "Contaminacion/Diario/contamina21Final.csv")
write.csv2(datosInvalidos, "Contaminacion/Diario/datosInvalidosDiarios.csv")
