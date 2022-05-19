rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

#cargo las librerias que necesito
require("data.table")
require("rpart")
require("rpart.plot")

#Aqui debe cambiar los parametros por los que desea probar

param_basicos  <- list( "cp"=        -0.1,    #complejidad minima
                        "minsplit"=   1200,    #minima cantidad de registros en un nodo para hacer el split
                        "minbucket"=  340,    #minima cantidad de registros en una hoja
                        "maxdepth"=    7 )    #profundidad máxima del arbol


#Aqui se debe poner la carpeta de SU computadora local
setwd("C:/Users/Bianca/OneDrive/Documentos/Archivos/ITBA/MineriaDeDatos")  #Establezco el Working Directory

#cargo los datos de 202011 que es donde voy a ENTRENAR el modelo
dtrain  <- fread("./datasets/paquete_premium_202011.csv")

###
#Esto lo agregue yo (creo que tenia que estar)
peso_error = 400
matriz_perdida = matrix(c( 0,peso_error,1,   1,0,1,   1,peso_error,0), nrow = 3)
###

#genero el modelo,  aqui se construye el arbol
modelo  <- rpart("clase_ternaria ~ .",  #quiero predecir clase_ternaria a partir de el resto de las variables
                 data = dtrain,
                 xval=0,
                 parms= list(loss= matriz_perdida), #Esto lo agregue yo (creo que tenía que estar)
                 cp=        -0.1,   #esto significa no limitar la complejidad de los splits
                 minsplit=  1200,     #minima cantidad de registros para que se haga el split
                 minbucket=  340,     #tamaño minimo de una hoja
                 maxdepth=   7 )    #profundidad maxima del arbol
### CHE, A ESTE MODELO DE ACA ARRIBA NO LE FALTA LA LOSS MATRIX??? SE LA AGREGO YO
### ADEMAS ESTAN DEFINIDOS DOS VECES LOS PARAMETROS (MIRAR ARRIBA params_basicos)

#grafico el arbol
prp(modelo, extra=101, digits=5, branch=1, type=4, varlen=0, faclen=0)


#Ahora aplico al modelo  a los datos de 202101  y genero la salida para kaggle

#cargo los datos de 202011, que es donde voy a APLICAR el modelo
dapply  <- fread("./datasets/paquete_premium_202101.csv")

#aplico el modelo a los datos nuevos
prediccion  <- predict( modelo, dapply , type = "prob")

#prediccion es una matriz con TRES columnas, llamadas "BAJA+1", "BAJA+2"  y "CONTINUA"
#cada columna es el vector de probabilidades 

#agrego a dapply una columna nueva que es la probabilidad de BAJA+2
dapply[ , prob_baja2 := prediccion[, "BAJA+2"] ]

#solo le envio estimulo a los registros con probabilidad de BAJA+2 mayor  a  1/60
dapply[ , Predicted  := as.numeric(prob_baja2 > 1/60) ]

#genero un dataset con las dos columnas que me interesan
entrega  <- dapply[   , list(numero_de_cliente, Predicted) ] #genero la salida

#genero el archivo para Kaggle
#creo la carpeta donde va el experimento
dir.create( "./labo/exp/" ) 
dir.create( "./labo/exp/KA2022/" ) 

fwrite( entrega, 
        file= "./labo/exp/KA2022/K242_014.csv",  #CHEQUEAR SIEMPRE ESTO PARA NO SOBREESCRIBIR NADA !!!!!
        sep= "," )
