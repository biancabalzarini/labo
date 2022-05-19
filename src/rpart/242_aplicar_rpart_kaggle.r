rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

#cargo las librerias que necesito
require("data.table")
require("rpart")
require("rpart.plot")

#Aqui debe cambiar los parametros por los que desea probar

param_basicos  <- list( "cp"=          -0.1,  #complejidad minima
                        "minsplit"=   840,     #minima cantidad de registros en un nodo para hacer el split
                        "minbucket"=  350,     #minima cantidad de registros en una hoja
                        "maxdepth"=     6 )    #profundidad máxima del arbol


#Aqui se debe poner la carpeta de SU computadora local
setwd("C:/Users/Bianca/OneDrive/Documentos/Archivos/ITBA/MineriaDeDatos")  #Establezco el Working Directory

#cargo los datos de 202011 que es donde voy a ENTRENAR el modelo
dtrain  <- fread("./datasets/paquete_premium_202011.csv")

###################################
#Si voy a usar los parámetros que encontré usando PCA (ver archivo Hago_PCA_sobre_dataset.R), tengo que cargar el dataset de entrenamiento PCA (o sea las componentes principales del dataset original):
dtrain <- fread("./datasets/202011DatasetPCA_50comp_CreadoPorMi.csv") #50 componentes ppales
dtrain <- fread("./datasets/202011DatasetPCA_TodasLasComp_CreadoPorMi.csv") #Todas las componentes ppales
###################################

#genero el modelo,  aqui se construye el arbol
modelo  <- rpart("clase_ternaria ~ .",  #quiero predecir clase_ternaria a partir de el resto de las variables
                 data = dtrain,
                 xval=0,
                 control=  param_basicos )

#grafico el arbol
prp(modelo, extra=101, digits=5, branch=1, type=4, varlen=0, faclen=0)


#Ahora aplico al modelo  a los datos de 202101  y genero la salida para kaggle

#cargo los datos de 202011, que es donde voy a APLICAR el modelo
dapply  <- fread("./datasets/paquete_premium_202101.csv")

###################################
#Si voy a usar los parámetros que encontré usando PCA (ver archivo Hago_PCA_sobre_dataset.R), tengo que cargar el dataset de PCA (o sea las componentes principales del dataset original):
dapply <- fread("./datasets/202101DatasetPCA_50comp_CreadoPorMi.csv") #50 componentes ppales
dapply <- fread("./datasets/202101DatasetPCA_TodasLasComp_CreadoPorMi.csv") #Todas las componentes ppales
###################################

#aplico el modelo a los datos nuevos
prediccion  <- predict( modelo, dapply , type = "prob")

#prediccion es una matriz con TRES columnas, llamadas "BAJA+1", "BAJA+2"  y "CONTINUA"
#cada columna es el vector de probabilidades 

#agrego a dapply una columna nueva que es la probabilidad de BAJA+2
dapply[ , prob_baja2 := prediccion[, "BAJA+2"] ]

#solo le envio estimulo a los registros con probabilidad de BAJA+2 mayor  a  1/60
dapply[ , Predicted  := as.numeric(prob_baja2 > 1/60) ]

###################################
#Esta sección es solo si estoy usando los datasets PCA
#Tengo que agregar una columna numero_de_cliente (que desapareció cuando hice PCA)
dt <- fread("./datasets/paquete_premium_202101.csv")
dapply[ , numero_de_cliente := dt$numero_de_cliente]
###################################

#genero un dataset con las dos columnas que me interesan
entrega  <- dapply[   , list(numero_de_cliente, Predicted) ] #genero la salida

#genero el archivo para Kaggle
#creo la carpeta donde va el experimento
dir.create( "./labo/exp/", showWarnings = FALSE  )
dir.create( "./labo/exp/KA2022/", showWarnings = FALSE  )

fwrite( entrega, 
        file= "./labo/exp/KA2022/K242_006.csv", #CAMBIAR EL NOMBRE DEL ARCHIVO CUANDO HAGA UNA CORRIDA NUEVA !!! PARA NO SOBREESCRIBIR
        sep= "," )
