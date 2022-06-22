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


plot(dataset[  , sd(ctrx_quarter),  cluster2 ])  #desvio estandar de la variable  ctrx_quarter
plot(dataset[  , sd(mtarjeta_visa_consumo),  cluster2 ])
plot(dataset[  , sd(mcaja_ahorro),  cluster2 ])
plot(dataset[  , sd(mcuentas_saldo),  cluster2 ])
plot(dataset[  , sd(chomebanking_trx),  cluster2 ])
plot(dataset[  , sd(cpayroll_trx),  cluster2 ])
plot(dataset[  , sd(mprestamos_personales),  cluster2 ])
plot(dataset[  , sd(mpayroll),  cluster2 ])


#7 CLUSTERS, HISTORICO (12 MESES)

#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")

setwd( "C:/Users/Bianca/OneDrive/Documentos/Archivos/ITBA/MineriaDeDatos/labo/expCloud/ST7620" )

dataset  <- fread("./exp_ST7620_cluster_de_bajas_12meses.txt", stringsAsFactors= TRUE)

print("Cantidad de clusters:")
print(length(unique(dataset$cluster2)))

#5 CLUSTERS, NO HISTORICO

#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")

setwd( "C:/Users/Bianca/OneDrive/Documentos/Archivos/ITBA/MineriaDeDatos/labo/expCloud/ST7611" )

dataset  <- fread("./exp_ST7611_cluster_de_bajas.txt", stringsAsFactors= TRUE)

print("Cantidad de clusters:")
print(length(unique(dataset$cluster2)))

#5 CLUSTERS, HISTORICO (12 MESES)

#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")

setwd( "C:/Users/Bianca/OneDrive/Documentos/Archivos/ITBA/MineriaDeDatos/labo/expCloud/ST7621" )

dataset  <- fread("./exp_ST7621_cluster_de_bajas_12meses.txt", stringsAsFactors= TRUE)

print("Cantidad de clusters:")
print(length(unique(dataset$cluster2)))


~~~


# Dataset original completo

setwd("C:/Users/Bianca/OneDrive/Documentos/Archivos/ITBA/MineriaDeDatos/datasets")

dataset1 = fread("./datasets_paquete_premium.csv.gz")

# Clientes del dataset n
n = 1
clustern = dataset[cluster2==n]$numero_de_cliente

d = dataset[, c("numero_de_cliente","cluster2")]

data_all <- merge.data.table(d,      # Merge de los data.tables
                             dataset1,
                             by.x = "numero_de_cliente",
                             by.y = "numero_de_cliente")

length(unique(data_all$numero_de_cliente)) == length(unique(d$numero_de_cliente)) # Esto es TRUE ---> En el dataset mergeado tengo solo los clientes que eventualmente se transforman en BAJA+2

data_all[, rank_foto_mes := frank(foto_mes,ties.method="dense")] # Rankeo los meses

ultimo_mes = data_all[,.SD[which.max(foto_mes)],by=numero_de_cliente] # Me quedo (para cada cliente) con la foto de su último mes antes de morir # Son todos (o casi todos, no se porque podría haber algun colado) BAJA+1

mes_m1 = data_all[,.SD[which.max(foto_mes)-1],by=numero_de_cliente] # Me quedo (para cada cliente) con la foto de su ante-último mes antes de morir (mes -1) # Son todos (o casi todos, no se porque podría haber algun colado) BAJA+2

mes_m2 = data_all[,.SD[which.max(foto_mes)-2],by=numero_de_cliente] # Me quedo (para cada cliente) con la foto de su ante-penúltimo mes antes de morir (mes -2) # Son todos (o casi todos, no se porque podría haber algun colado) CONTINUA

mes_m3 = data_all[,.SD[which.max(foto_mes)-3],by=numero_de_cliente] # Me quedo (para cada cliente) con la foto de su ante-ante-penúltimo mes antes de morir (mes -3) # Son todos (o casi todos, no se porque podría haber algun colado) CONTINUA


# Ejemplo de un grafico que se puede hacer
var = quote(ctrx_quarter)
plot(ultimo_mes[  , mean(eval(var)),  cluster2 ], type="b",ylim=c(0,85)) # El ylim es a mano, tedría que buscar como automatizarlo
lines(mes_m1[  , mean(eval(var)),  cluster2 ], col='red', type = "b")
lines(mes_m2[  , mean(eval(var)),  cluster2 ], col='green', type = "b")
lines(mes_m3[  , mean(eval(var)),  cluster2 ], col='blue', type = "b")
legend("topright", legend=c("Mes 0", "Mes -1", "Mes -2", "Mes -3"),col=c("black", "red", "green", "blue"), lty = 1:2, cex=0.8)

