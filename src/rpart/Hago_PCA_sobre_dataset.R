# Hago PCA sobre las variables independientes (todas menos clase_ternaria)

rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

setwd("C:/Users/Bianca/OneDrive/Documentos/Archivos/ITBA/MineriaDeDatos")

#Elijo el dataset
dataset  <- fread("./datasets/paquete_premium_202011.csv")
dataset  <- fread("./datasets/paquete_premium_202101.csv")

a = dim(dataset)[2]-1
data2 <- as.data.frame(dataset[, 1:a]) #Me quedo con todo el dataset menos clase_ternaria #Además transformo en dataframe porque lo necesito para el paso siguiente { no me salio usando data.table ): }
for(i in 1:ncol(data2)) { # Reemplazo los NA en todas las columnas por el valor medio (para hacer PCA no puedo tener NAs, y no quiero tirar los registros que si lo tienen, sobretodo porque en kaggle tengo que predecir para registros con NAs)
  data2[, i][is.na(data2[, i])] <- mean(data2[, i], na.rm=TRUE)
}

#Quiero aplicar PCA con scale=TRUE (porque las variables tienen varianzas muy distintas)
#Pero no puedo hacer esto si alguna variable tiene varianza cero (o sea, todos los registros tienen el mismo valor)
#Detecto y descarto las variables con varianza cero
which(apply(data2, 2, var)==0) #Me dice que columna tiene varianza cero #En este caso es foto_mes
#is.data.table(data2) #No debería ser data.table
setDT(data2) #Vuelvo a transformar en data.table
#is.data.table(data2) #Ahora si debería ser data.table
data2[, foto_mes:=NULL] #Saco la columna con varianza cero

#Ahora si hago PCA
pca = prcomp(data2, scale=TRUE)

#Variabilidad que aporta cada componente principal
vari = pca$sdev^2/ sum(pca$sdev^2)
plot(vari)

#pca$x #Estos son todos los registros en el sistema de referencia de las componentes ppales

#Si me quiero quedar con todas las componentes principales
data_pca = as.data.frame(pca$x)
data_pca = cbind(data_pca,clase_ternaria=dataset$clase_ternaria) #Pongo al final la variable que quiero predecir
#Guardo el nuevo dataset
fwrite(data_pca,file="./datasets/202101DatasetPCA_TodasLasComp_CreadoPorMi.csv") #CHEQUEAR ESTO CUANDO GUARDO !!!

#Repito pero quedandome solo con 50 componentes ppales
data_pca = as.data.frame(pca$x)
data_pca = data_pca[,1:50] #Me quedo con las primeras 50 componentes principales
data_pca = cbind(data_pca,clase_ternaria=dataset$clase_ternaria) #Pongo al final la variable que quiero predecir
#Guardo el nuevo dataset
fwrite(data_pca,file="./datasets/202101DatasetPCA_50comp_CreadoPorMi.csv") #CHEQUEAR ESTO CUANDO GUARDO !!!
