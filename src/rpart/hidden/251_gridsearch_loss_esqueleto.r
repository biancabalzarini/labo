#script  para alumnos avanzados

#esqueleto de grid search con la Loss Matrix
#se espera que los alumnos completen lo que falta para recorrer TODOS los cuatro hiperparametros 

rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

require("data.table")
require("rpart")
require("parallel")

ksemillas  <- c(102191, 200177, 410551, 552581, 892237) #reemplazar por las propias semillas

#------------------------------------------------------------------------------
#particionar agrega una columna llamada fold a un dataset que consiste en una particion estratificada segun agrupa
# particionar( data=dataset, division=c(70,30), agrupa=clase_ternaria, seed=semilla)   crea una particion 70, 30 

particionar  <- function( data,  division, agrupa="",  campo="fold", start=1, seed=NA )
{
  if( !is.na(seed) )   set.seed( seed )

  bloque  <- unlist( mapply(  function(x,y) { rep( y, x )} ,   division,  seq( from=start, length.out=length(division) )  ) )  

  data[ , (campo) :=  sample( rep( bloque, ceiling(.N/length(bloque))) )[1:.N],
          by= agrupa ]
}
#------------------------------------------------------------------------------

ArbolEstimarGanancia  <- function( semilla, param_basicos, peso_error )
{
  #particiono estratificadamente el dataset
  particionar( dataset, division=c(70,30), agrupa="clase_ternaria", seed= semilla )  #Cambiar por la primer semilla de cada uno !

  #va la matriz de perdida,  por columnas
  matriz_perdida  <- matrix(c( 0,peso_error,1,   1,0,1,   1,peso_error,0), nrow = 3)

  #genero el modelo
  modelo  <- rpart("clase_ternaria ~ .",     #quiero predecir clase_ternaria a partir del resto
                   data= dataset[fold==1],  #fold==1  es training,  el 70% de los datos
                   xval= 0,
                   parms= list(loss= matriz_perdida),
                   control= param_basicos )  #aqui van los parametros del arbol

  #aplico el modelo a los datos de testing
  prediccion  <- predict( modelo,   #el modelo que genere recien
                          dataset[ fold==2],  #fold==2  es testing, el 30% de los datos
                          type= "prob") #type= "prob"  es que devuelva la probabilidad

  #prediccion es una matriz con TRES columnas, llamadas "BAJA+1", "BAJA+2"  y "CONTINUA"
  #cada columna es el vector de probabilidades 


  #calculo la ganancia en testing  qu es fold==2
  ganancia_test  <- dataset[ fold==2, 
                             sum( ifelse( prediccion[, "BAJA+2"]  >  1/60,
                                         ifelse( clase_ternaria=="BAJA+2", 59000, -1000 ),
                                         0 ) )]

  #escalo la ganancia como si fuera todo el dataset
  ganancia_test_normalizada  <-  ganancia_test / 0.3

  return( ganancia_test_normalizada )
}
#------------------------------------------------------------------------------

ArbolesMontecarlo  <- function( semillas, param_basicos, peso_error )
{
  #la funcion mcmapply  llama a la funcion ArbolEstimarGanancia  tantas veces como valores tenga el vector  ksemillas
  ganancias  <- mcmapply( ArbolEstimarGanancia, 
                          semillas,   #paso el vector de semillas, que debe ser el primer parametro de la funcion ArbolEstimarGanancia
                          MoreArgs= list( param_basicos, peso_error),  #aqui paso el segundo parametro
                          SIMPLIFY= FALSE,
                          mc.cores= 1 )  #se puede subir a 5 si posee Linux o Mac OS

  ganancia_promedio  <- mean( unlist(ganancias) )

  return( ganancia_promedio )
}
#------------------------------------------------------------------------------

#Aqui se debe poner la carpeta de la computadora local
setwd("C:/Users/Bianca/OneDrive/Documentos/Archivos/ITBA/MineriaDeDatos")   #Establezco el Working Directory

#cargo los datos
dataset  <- fread("./datasets/paquete_premium_202011.csv")

#ordeno para que queden en orden BAJA+1, BAJA+2, CONTINUA
#The order of the loss matrix depends on the order of your factor variable in R
setorder( dataset, clase_ternaria )

#genero el archivo para Kaggle
#creo la carpeta donde va el experimento
# HT  representa  Hiperparameter Tuning
dir.create( "./labo/exp/",  showWarnings = FALSE ) 
dir.create( "./labo/exp/HT2021/", showWarnings = FALSE )
archivo_salida  <- "./labo/exp/HT2021/gridsearch.txt"

#Escribo los titulos al archivo donde van a quedar los resultados
#atencion que si ya existe el archivo, esta instruccion LO SOBREESCRIBE, y lo que estaba antes se pierde
#la forma que no suceda lo anterior es con append=TRUE
cat( file=archivo_salida,
     sep= "",
     "max_depth", "\t",
     "min_bucket", "\t",
     "min_split", "\t",
     "peso_error", "\t",
     "cp", "\t",
     "ganancia_promedio", "\n")


#itero por los loops anidados para cada hiperparametro

inicio_loops = Sys.time()

for( vmax_depth  in  c( 4, 5, 6, 7, 8, 9 )  )
{ print(vmax_depth)
for( vmin_bucket in c( 300, 340, 380, 420, 460, 500 ) )
{  print(vmin_bucket)
for( vmin_split  in  c( 1400, 1200, 1000, 800, 600 )  )
{
if(2*vmin_bucket <= vmin_split) #Entiendo que R debería chequear esto solo, pero por las dudas pongo yo la condición. Sobre todo porque podría pasar que si esta condición en algún momento no se cumple, la función rpart me corta la corrida y pierdo el progreso.
{
for( vpeso_error  in  c( 1, 10, 100, 200, 244, 270, 300, 350, 400, 450) )
{ #Para elegir que poner en estos pesos se puede pensar que significan que tanto peor es clasificar mal a un BAJA+2 que a cualquier otro (es grave porque perdemos un cliente que podríamos haber salvado con un 50% de probabilidad)
  #El tema además es que BAJA+2 está super sub-representado en el dataset. La inmesa mayoría de los registros son CONTINUA. Sin la loss matriz, no es tan grave clasificar mal un BAJA+2 simplemente porque son pocos los registros BAJA+2.
  #Quizas se puede elegir este peso usando el cociente entre CONTINUAs y BAJA+2s (que da algo asi como 244) para tratar de equiparar esta sub-representación.
for( cp in c(-0.1) ) #Dejo esto fijo
{

  #notar como se agrega
  param_basicos  <- list( "cp"=           cp,        #complejidad minima
                          "minsplit"=  vmin_split,   #minima cantidad de registros en un nodo para hacer el split
                          "minbucket"=  vmin_bucket, #minima cantidad de registros en una hoja
                          "maxdepth"=  vmax_depth )  #profundidad máxima del arbol

  #Un solo llamado, con la semilla 17
  ganancia_promedio  <- ArbolesMontecarlo( ksemillas,  param_basicos, vpeso_error )

  #escribo los resultados al archivo de salida
  cat(  file=archivo_salida,
        append= TRUE,
        sep= "",
        vmax_depth, "\t",
        vmin_bucket, "\t",
        vmin_split, "\t",
        vpeso_error, "\t",
        cp, "\t",
        ganancia_promedio, "\n"  )
}
}
}
}
}
}

fin_loops = Sys.time()
tiempo_transcurrido = fin_loops - inicio_loops
print(tiempo_transcurrido)
