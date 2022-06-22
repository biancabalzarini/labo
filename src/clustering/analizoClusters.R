#Analizo como son los clusters

#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")

#7 CLUSTERS, NO HISTORICO

setwd( "C:/Users/Bianca/OneDrive/Documentos/Archivos/ITBA/MineriaDeDatos/labo/expCloud/ST7610" )

dataset  <- fread("./exp_ST7610_cluster_de_bajas.txt", stringsAsFactors= TRUE)

print("Cantidad de clusters:")
print(length(unique(dataset$cluster2)))

#ahora a mano veo los centroides de los 7 clusters
#esto hay que hacerlo para cada variable,
#  y ver cuales son las que mas diferencian a los clusters
#esta parte conviene hacerla desde la PC local, sobre  cluster_de_bajas.txt

plot(dataset[  , mean(ctrx_quarter),  cluster2 ])  #media de la variable  ctrx_quarter
plot(dataset[  , mean(mtarjeta_visa_consumo),  cluster2 ])
plot(dataset[  , mean(mcaja_ahorro),  cluster2 ])
plot(dataset[  , mean(mcuentas_saldo),  cluster2 ])
plot(dataset[  , mean(chomebanking_trx),  cluster2 ])
plot(dataset[  , mean(cpayroll_trx),  cluster2 ])
plot(dataset[  , mean(mprestamos_personales),  cluster2 ])
plot(dataset[  , mean(mpayroll),  cluster2 ])

#Arriba grafiqué los centroides para los 7 clusters de varias variables, pero en su mayoria, son variables importantes (salen arriba en la jerarquía de variables que devuelve el HT)
#Los clusters 5, 6 y 7 tienen valores parecidos en los centroides de muchas variables (salvo mcuentas_saldo y mprestamos_personales)
#QUIZAS si bajo a 5 el número de clusters, el 5, 6 y 7 se unan (aproximadamente) en un mismo cluster
#Me parece que puede llegar a ser demasiado tener 7 clusters


#VER LOS DESVIOS ESTANDAR !!!!!!!!!!!


#7 CLUSTERS, HISTORICO (12 MESES)

#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")

setwd( "C:/Users/Bianca/OneDrive/Documentos/Archivos/ITBA/MineriaDeDatos/labo/expCloud/ST7620" )

dataset  <- fread("./exp_ST7620_cluster_de_bajas_12meses.txt", stringsAsFactors= TRUE)

print("Cantidad de clusters:")
print(length(unique(dataset$cluster2)))

