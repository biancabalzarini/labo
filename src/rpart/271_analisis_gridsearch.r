rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

#cargo las librerias que necesito
require("data.table")
require("rpart")
require("rpart.plot")

#Aqui se debe poner la carpeta de SU computadora local
setwd("C:/Users/Bianca/OneDrive/Documentos/Archivos/ITBA/MineriaDeDatos")  #Establezco el Working Directory

#cargo la salida del Grid Seach, verifique que corresponda a la carpeta donde dejó el resultado
dtrain  <- fread("./labo/exp/HT2021/gridsearch_8.632011hours.txt")

#genero el modelo,  aqui se construye el arbol
#este sera un arbol de REGRESION ya que la variable objetivo, ganancia_promedio,  es una variable continua
modelo  <- rpart("ganancia_promedio ~ .",  #quiero predecir la ganancia promedio a partir de el resto de las variables
                 data = dtrain,
                 xval=0,
                 cp=         0,
                 minsplit=  50,     #minima cantidad de registros para que se haga el split
                 minbucket= 10,     #tamaño minimo de una hoja
                 maxdepth=   4 )    #profundidad maxima del arbol


#grafico el arbol

#primero creo la carpeta a donde voy a guardar el dibujo del arbol
dir.create( "./labo/exp/",  showWarnings = FALSE ) 
dir.create( "./labo/exp/ST2030/", showWarnings = FALSE )
archivo_salida  <- "./labo/exp/ST2030/arbol_analisis_gridsearch_8.632011hours.pdf"

#finalmente, genero el grafico guardandolo en un archivo pdf
pdf( archivo_salida, paper="a4r" )
prp(modelo, extra=101, digits=5, branch=1, type=4, varlen=0, faclen=0)
dev.off()

#################################################################
# Agrego yo un par de visualizaciones

#Aca se ve que la mejor max_depth es 5.
plot(dtrain$max_depth,dtrain$ganancia_promedio)
#Aca se ve que minbucket no importa tanto. Sigo prefiriendo max_depth = 5.
plot(dtrain$minbucket,dtrain$ganancia_promedio,col=ifelse(dtrain$max_depth==5,'red',ifelse(dtrain$max_depth==6,'orange','yellow')))
#Aca se ve que para minbucket 320 la ganancia es mejor (es como un zoom del grafico anterior).
plot(dtrain[max_depth==5]$minbucket,dtrain[max_depth==5]$ganancia_promedio,col='red',main='Solo par max_depth=5')
#Aca se ve medio lo mismo que para minbucket.
plot(dtrain$min_split,dtrain$ganancia_promedio,col=ifelse(dtrain$max_depth==5,'red',ifelse(dtrain$max_depth==6,'orange','yellow')))
#Esto es como un zoom del grafico anterior.
plot(dtrain[max_depth==5]$min_split,dtrain[max_depth==5]$ganancia_promedio,col='red',main='Solo par max_depth=5')
#Medio lo mismo que siempre.
plot(dtrain$cp,dtrain$ganancia_promedio,col=ifelse(dtrain$max_depth==5,'red',ifelse(dtrain$max_depth==6,'orange','yellow')))
#Zoom del grafico anterior.
plot(dtrain[max_depth==5]$cp,dtrain[max_depth==5]$ganancia_promedio,col='red',main='Solo par max_depth=5')
#Si la corrida es con peso_error:
plot(dtrain$peso_error,dtrain$ganancia_promedio)

