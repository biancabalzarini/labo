#Analizo como son los clusters

#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")


library("ggplot2")
library("ggforce")

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


#~~~#


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

mes_m4 = data_all[,.SD[which.max(foto_mes)-4],by=numero_de_cliente]

mes_m5 = data_all[,.SD[which.max(foto_mes)-5],by=numero_de_cliente]

mes_m6 = data_all[,.SD[which.max(foto_mes)-6],by=numero_de_cliente]

mes_m7 = data_all[,.SD[which.max(foto_mes)-7],by=numero_de_cliente]

mes_m8 = data_all[,.SD[which.max(foto_mes)-8],by=numero_de_cliente]


# Ejemplo de un grafico que se puede hacer
var = quote(mcuentas_saldo)

plot(ultimo_mes[  , mean(eval(var)),  cluster2 ], type="b")#,ylim=c(-70000,40000)) # El ylim es a mano, tedría que buscar como automatizarlo
lines(mes_m1[  , mean(eval(var)),  cluster2 ], col='red', type = "b")
lines(mes_m2[  , mean(eval(var)),  cluster2 ], col='green', type = "b")
lines(mes_m3[  , mean(eval(var)),  cluster2 ], col='blue', type = "b")
lines(mes_m4[  , mean(eval(var)),  cluster2 ], col='blue', type = "b")
lines(mes_m5[  , mean(eval(var)),  cluster2 ], col='blue', type = "b")
lines(mes_m6[  , mean(eval(var)),  cluster2 ], col='blue', type = "b")
lines(mes_m7[  , mean(eval(var)),  cluster2 ], col='blue', type = "b")
lines(mes_m8[  , mean(eval(var)),  cluster2 ], col='blue', type = "b")
legend("topright", legend=c("Media mes 0", "Media mes -1", "Media mes -2", "Media mes -3", "Media mes -4", "Media mes -5", "Media mes -6", "Media mes -7", "Media mes -8"),col=c("black", "red", "green", "blue", "orange", "violet", "pink", "grey", "yellow"), lty = 1:2, cex=0.8)

plot(ultimo_mes[  , sd(eval(var)),  cluster2 ], type="b")#,ylim=c(50000,600000)) # El ylim es a mano, tedría que buscar como automatizarlo
lines(mes_m1[  , sd(eval(var)),  cluster2 ], col='red', type = "b")
lines(mes_m2[  , sd(eval(var)),  cluster2 ], col='green', type = "b")
lines(mes_m3[  , sd(eval(var)),  cluster2 ], col='blue', type = "b")
lines(mes_m4[  , sd(eval(var)),  cluster2 ], col='blue', type = "b")
lines(mes_m5[  , sd(eval(var)),  cluster2 ], col='blue', type = "b")
lines(mes_m6[  , sd(eval(var)),  cluster2 ], col='blue', type = "b")
lines(mes_m7[  , sd(eval(var)),  cluster2 ], col='blue', type = "b")
lines(mes_m8[  , sd(eval(var)),  cluster2 ], col='blue', type = "b")
legend("topright", legend=c("Desvio mes 0", "Desvio mes -1", "Desvio mes -2", "Desvio mes -3", "Desvio mes -4", "Desvio mes -5", "Desvio mes -6", "Desvio mes -7", "Desvio mes -8"),col=c("black", "red", "green", "blue", "orange", "violet", "pink", "grey", "yellow"), lty = 1:2, cex=0.8)


# Otro grafico
var = quote(cliente_antiguedad)

a = cbind(mes_m8[  , mean(eval(var)),  cluster2 ][,2],
          mes_m7[  , mean(eval(var)),  cluster2 ][,2],
          mes_m6[  , mean(eval(var)),  cluster2 ][,2],
          mes_m5[  , mean(eval(var)),  cluster2 ][,2],
          mes_m4[  , mean(eval(var)),  cluster2 ][,2],
          mes_m3[  , mean(eval(var)),  cluster2 ][,2],
          mes_m2[  , mean(eval(var)),  cluster2 ][,2],
          mes_m1[  , mean(eval(var)),  cluster2 ][,2],
          ultimo_mes[  , mean(eval(var)),  cluster2 ][,2])

b = cbind(mes_m8[  , sd(eval(var)),  cluster2 ][,2],
          mes_m7[  , sd(eval(var)),  cluster2 ][,2],
          mes_m6[  , sd(eval(var)),  cluster2 ][,2],
          mes_m5[  , sd(eval(var)),  cluster2 ][,2],
          mes_m4[  , sd(eval(var)),  cluster2 ][,2],
          mes_m3[  , sd(eval(var)),  cluster2 ][,2],
          mes_m2[  , sd(eval(var)),  cluster2 ][,2],
          mes_m1[  , sd(eval(var)),  cluster2 ][,2],
          ultimo_mes[  , sd(eval(var)),  cluster2 ][,2])


nro = 1 # Numero de cluster

plot(c(-8,-7,-6,-5,-4,-3,-2,-1,0),a[nro])

plot(c(-8,-7,-6,-5,-4,-3,-2,-1,0),b[nro])

data_circle2 <- data.frame(x0 = seq(min(a[nro]),30*max(a[nro]),(30*max(a[nro])-min(a[nro]))/8),    # Create data for multiple circles
                           y0 = unlist(as.list(a[nro])),
                           r = unlist(as.list(b[nro])))

ggplot() +
  geom_circle(data = data_circle2, aes(x0 = x0, y0 = y0, r = r, col = r))


# Otro grafico
var = quote(mcaja_ahorro_dolares) #cpayroll_trx #mpayroll #ctarjeta_visa_trx #chomebanking_trx #mcaja_ahorro_dolares

a = cbind(mes_m8[  , mean(eval(var)),  cluster2 ][,2],
          mes_m7[  , mean(eval(var)),  cluster2 ][,2],
          mes_m6[  , mean(eval(var)),  cluster2 ][,2],
          mes_m5[  , mean(eval(var)),  cluster2 ][,2],
          mes_m4[  , mean(eval(var)),  cluster2 ][,2],
          mes_m3[  , mean(eval(var)),  cluster2 ][,2],
          mes_m2[  , mean(eval(var)),  cluster2 ][,2],
          mes_m1[  , mean(eval(var)),  cluster2 ][,2],
          ultimo_mes[  , mean(eval(var)),  cluster2 ][,2])

b = cbind(mes_m8[  , sd(eval(var)),  cluster2 ][,2],
          mes_m7[  , sd(eval(var)),  cluster2 ][,2],
          mes_m6[  , sd(eval(var)),  cluster2 ][,2],
          mes_m5[  , sd(eval(var)),  cluster2 ][,2],
          mes_m4[  , sd(eval(var)),  cluster2 ][,2],
          mes_m3[  , sd(eval(var)),  cluster2 ][,2],
          mes_m2[  , sd(eval(var)),  cluster2 ][,2],
          mes_m1[  , sd(eval(var)),  cluster2 ][,2],
          ultimo_mes[  , sd(eval(var)),  cluster2 ][,2])

colors = c("#EB1E2C","blue","green","orange","black","violet","pink")
nro = 1 # Numero de cluster
plot(c(-8,-7,-6,-5,-4,-3,-2,-1,0),a[nro],type="l",ylim=c(min(a),max(a)),col=colors[1],xlab='Meses',ylab=var)
for (val in 1: length(unique(dataset$cluster2))-1)
{
  lines(c(-8,-7,-6,-5,-4,-3,-2,-1,0),a[nro+val],ylim=c(min(a),max(a)),col=colors[1+val])
}
