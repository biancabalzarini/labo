#limpio la memoria
rm( list=ls() )
gc()

require("data.table")

ftirar  <- function( prob, qty )
{
  return(  sum( runif(qty) < prob ) )
}

#variables globales que usan las funciones gimnasio_xxxx
GLOBAL_jugadores  <- c()
GLOBAL_tiros_total  <- 0

#Crea el juego
#a cada jugador se le pone un numero de 1 a 100 en la espalda
#debajo de ese numero esta el indice_de_enceste  que NO puede ser visto por el cazatalentos
gimnasio_init  <- function() 
{
  GLOBAL_jugadores  <<-  sample( c( (501:599 )/1000 , 0.7 ) )
  GLOBAL_tiros_total  <<- 0
}

#se le pasa un vector con los IDs de los jugadores y la cantidad de tiros a realizar
#devuelve en un vector cuantos aciertos tuvo cada jugador
gimnasio_tirar  <- function(  pids,  pcantidad )
{
  GLOBAL_tiros_total  <<-  GLOBAL_tiros_total + length( pids )*pcantidad
  res  <- mapply(  ftirar, GLOBAL_jugadores[pids], pcantidad )
  
  return( res )
}

#El cazatalentos decide a que jugador llevarse
#devuelve la cantidad de tiros libres y si le acerto al verdadero_mejor o no
gimnasio_veredicto  <- function( pid )
{
  return( list("tiros_total"= GLOBAL_tiros_total, 
               "acierto"=     as.integer( GLOBAL_jugadores[pid]==0.7) ))
}
#------------------------------------------------------------------------------

set.seed( 102191 )

#inicializo el juego
gimnasio_init()





##############################################################
GLOBAL = c( (501:599)/1000 )

j = 0
for ( i in 1:10000 ){
  
  grupo = 25
  GLOBAL_jugadores = sample( GLOBAL, grupo-1 )
  GLOBAL_jugadores = c( GLOBAL_jugadores, 0.7 )
  jordan = which( GLOBAL_jugadores==0.7 )
  
  resultado1 = gimnasio_tirar( 1:grupo, 40 )
  if ( resultado1[jordan] > median( resultado1 ) ){ #quantile( resultado1 )[4] ){
    j = j+1
  }
}
j = j/10000
##############################################################
GLOBAL_jugadores = sample( c( (501:599 )/1000 , 0.7 ) )
jordan = which( GLOBAL_jugadores==0.7 )

k = 0
for ( i in 1:10000 ){
  resultado1 = gimnasio_tirar( 1:100, 120 )
  if ( resultado1[jordan] > quantile( resultado1 )[4] ){
    k = k+1
  }
}
k = k/10000
##############################################################
GLOBAL = c( (501:599)/1000 )

l = 0
for ( i in 1:10000 ){
  
  grupo = 13
  GLOBAL_jugadores = sample( GLOBAL, grupo-1 )
  GLOBAL_jugadores = c( GLOBAL_jugadores, 0.7 )
  jordan = which( GLOBAL_jugadores==0.7 )
  
  resultado1 = gimnasio_tirar( 1:grupo, 7 )
  if ( resultado1[jordan] == max( resultado1 ) ){
    l = l+1
  }
}
l = l/10000
##############################################################
print(j)
print(k)
print(l)
print(j*k*l)







prob1 = 0.7
qty1 = 100
prob2 = 0.
qty2 = 100

c = 0
for (i in 1:10000){
  a = sum( runif(qty1) < prob1 )
  b = sum( runif(qty2) < prob2 )
  if (a > b){
    c = c+1
    }
}
print(c/10000)

















GLOBAL_jugadores = c(0.6,0.7)
j = 0
for ( i in 1:10000 ){
  
  resultado1 = gimnasio_tirar( 1:grupo, 270 )
  if ( resultado1[1] < resultado1[2] ){
    j = j+1
  }
}
j = j/10000
j



######################################


require("data.table")
set.seed( 102191 )
#calcula cuantos encestes logra un jugador con indice de enceste prob que hace qyt tiros libres
ftirar  <- function( prob, qty )
{
  return( sum( runif(qty) < prob ) )
}

dif = c()
for (k in 1:9){
  jugadores  <- rep( k/10, 100 )  #jugadores identicos
  diferencias  <- c()
  
  for( i in 1:9999 )
  {
    vaciertos  <- mapply( ftirar, jugadores, 90 )  #cada jugador tira 100 tiros libres
    mejor  <- which.max( vaciertos )
    aciertos_torneo  <- vaciertos[ mejor ]
    aciertos_segunda  <- ftirar( jugadores[mejor], 90 )
    diferencias  <- c( diferencias, aciertos_torneo - aciertos_segunda )
  }
  print(k)
  dif = c(dif,mean( diferencias ))
}
plot((1:9)/10,dif)
dif

0.7       | 0.6
#Con 100 tiros
11.080508 | 12.043004
10.968597 | 11.960496
11.016502 | 12.083008
10.988099 | 12.091209
11.037004 | 12.030903
11.077508 | 11.977498
#Con 90 tiros
10.496450 | 11.421542
10.344134 | 11.469547
10.506351 | 11.365537







gimnasio_init()
#Esta el la planilla del cazatalentos
planilla_cazatalentos  <- data.table( "id" = 1:100 )

#Ronda 1  ------------------------------------------------------
#tiran los 100 jugadores es decir 1:100  90 tiros libres cada uno
ids_juegan1  <- 1:100   #los jugadores que participan en la ronda,
planilla_cazatalentos[ ids_juegan1,  tiros1 := 90 ]  #registro en la planilla que tiran 90 tiros
#Hago que tiren
resultado1  <- gimnasio_tirar( ids_juegan1, 90)
planilla_cazatalentos[ ids_juegan1,  aciertos1 := resultado1 ]  #registro en la planilla

mediana  <- planilla_cazatalentos[ ids_juegan1, median(aciertos1) ]
ids_juegan2  <- planilla_cazatalentos[ ids_juegan1 ][aciertos1 >= mediana, id ]

planilla_cazatalentos[ ids_juegan2,  tiros2 := 90 ]
resultado2  <- gimnasio_tirar( ids_juegan2, 90)
planilla_cazatalentos[ ids_juegan2,  aciertos2 := resultado2 ]
planilla_cazatalentos[ ids_juegan2, dif := aciertos1 - aciertos2 ]
vec = planilla_cazatalentos[,dif]
vec = vec[!is.na(vec)]












#~~~~~~~#

set.seed( 102191 )

#inicializo el juego
gimnasio_init()

#Esta el la planilla del cazatalentos
planilla_cazatalentos  <- data.table( "id" = 1:100 )

#Ronda 1  ------------------------------------------------------
#tiran los 100 jugadores es decir 1:100  90 tiros libres cada uno
ids_juegan1  <- 1:100   #los jugadores que participan en la ronda,

planilla_cazatalentos[ ids_juegan1,  tiros1 := 90 ]  #registro en la planilla que tiran 90 tiros

#Hago que tiren
resultado1  <- gimnasio_tirar( ids_juegan1, 90)
planilla_cazatalentos[ ids_juegan1,  aciertos1 := resultado1 ]  #registro en la planilla

#Ronda 2 -------------------------------------------------------
#los mayores a la mediana tiran 90 tiros cada uno
mediana  <- planilla_cazatalentos[ ids_juegan1, median(aciertos1) ]
ids_juegan2  <- planilla_cazatalentos[ ids_juegan1 ][aciertos1 > mediana, id ]

planilla_cazatalentos[ ids_juegan2,  tiros2 := 90 ]  #registro en la planilla que tiran 400 tiros
resultado2  <- gimnasio_tirar( ids_juegan2, 90)
planilla_cazatalentos[ ids_juegan2,  aciertos2 := resultado2 ]  #registro en la planilla

#Ronda 3 -------------------------------------------------------
#los mayores a la mediana tiran
mediana  <- planilla_cazatalentos[ ids_juegan2, median(aciertos2) ]
ids_juegan3  <- planilla_cazatalentos[ ids_juegan2 ][aciertos2 > mediana, id ]











#########
GLOBAL = c( (501:599)/1000 )
GLOBAL_jugadores = c( GLOBAL, 0.7 )

q1 = 120
resultado1 = gimnasio_tirar( 1:100, q1 )/q1
q2 = 100
resultado2 = gimnasio_tirar( 1:100, q2 )/q2
b = which(resultado1>quantile(resultado1)[4])
a = resultado2-resultado1
c = a[b]
c
c[length(c)]>quantile(c)[4]
c[length(c)]>median(c)
#length(b)











##########################################################################################

#t1 = 90 ; t2 = 45 ; t3 = 55 ; t4 = 60 ; t5 = 180 # ; t6 = 100
#t1 = 90 ; t2 = 50 ; t3 = 55 ; t4 = 55 ; t5 = 220 #0.9872
#t1 = 90 ; t2 = 50 ; t3 = 50 ; t4 = 50 ; t5 = 250 #0.987
#t1 = 90 ; t2 = 50 ; t3 = 55 ; t4 = 65 ; t5 = 220 #0.9878
#t1 = 90 ; t2 = 50 ; t3 = 60 ; t4 = 65 ; t5 = 180 #0.987
#t1 = 90 ; t2 = 50 ; t3 = 55 ; t4 = 75 ; t5 = 210 #0.9889 #UPS, ME PASE DE TIROS
#t1 = 90 ; t2 = 50 ; t3 = 60 ; t4 = 75 ; t5 = 170 #0.9865
#t1 = 90 ; t2 = 45 ; t3 = 55 ; t4 = 75 ; t5 = 236 #0.9887

t1 = 90 ; t2 = 46 ; t3 = 57 ; t4 = 80 ; t5 = 225
t1*100 + t2*50 + t3*25 + t3*13 + t4*7 + t5*4 # + t6*2 #Un aproximado de la cantidad total de tiros
t1 + t2 + t3 + t4 + t5 # + t6 #Cantidad de tiros totales que hacen los que llegan hasta el final

Estrategia  <- function()
  {
  #inicializo el juego
  gimnasio_init()
  
  #a = which(GLOBAL_jugadores==0.7)
  #a #Posicion de jordan
  #GLOBAL_jugadores[a] #Este es jordan
  
  #Esta el la planilla del cazatalentos
  planilla_cazatalentos  <- data.table( "id" = 1:100 )
  
  
  #Ronda 1  ------------------------------------------------------
  #tiran los 100 jugadores es decir 1:100  90 tiros libres cada uno
  ids_juegan1  <- 1:100   #los jugadores que participan en la ronda,
  
  planilla_cazatalentos[ ids_juegan1,  tiros1 := t1 ]  #registro en la planilla que tiran 90 tiros
  
  #Hago que tiren
  resultado1  <- gimnasio_tirar( ids_juegan1, t1 )
  planilla_cazatalentos[ ids_juegan1,  aciertos1 := resultado1 ]  #registro en la planilla
  
  #Ronda 2 -------------------------------------------------------
  #la mejor mitad de los jugadores tiran de nuevo
  mediana2  <- planilla_cazatalentos[ ids_juegan1, median(aciertos1) ]
  ids_juegan2  <- planilla_cazatalentos[ ids_juegan1 ][ aciertos1 > mediana2, id ]
  
  planilla_cazatalentos[ ids_juegan2,  tiros2 := t2 ]
  resultado2  <- gimnasio_tirar( ids_juegan2, t2 )
  planilla_cazatalentos[ ids_juegan2,  aciertos2 := resultado2 ]
  
  planilla_cazatalentos[ ids_juegan2, subtotal2 := aciertos1+aciertos2 ]
  
  #Ronda 3 -------------------------------------------------------
  mediana3  <- planilla_cazatalentos[ ids_juegan2, median(subtotal2) ]
  ids_juegan3  <- planilla_cazatalentos[ ids_juegan2 ][subtotal2 > mediana3, id ]
  
  planilla_cazatalentos[ ids_juegan3,  tiros3 := t3 ]
  resultado3  <- gimnasio_tirar( ids_juegan3, t3 )
  planilla_cazatalentos[ ids_juegan3,  aciertos3 := resultado3 ]
  
  planilla_cazatalentos[ ids_juegan3, subtotal3 := subtotal2+aciertos3 ]
  
  #Ronda 4 -------------------------------------------------------
  mediana4  <- planilla_cazatalentos[ ids_juegan3, median(subtotal3) ]
  ids_juegan4  <- planilla_cazatalentos[ ids_juegan3 ][subtotal3 > mediana4, id ]
  
  planilla_cazatalentos[ ids_juegan4,  tiros4 := t4 ]
  resultado4  <- gimnasio_tirar( ids_juegan4, t4 )
  planilla_cazatalentos[ ids_juegan4,  aciertos4 := resultado4 ]
  
  planilla_cazatalentos[ ids_juegan4, subtotal4 := subtotal3+aciertos4 ]
  
  #Ronda 5 -------------------------------------------------------
  mediana5  <- planilla_cazatalentos[ ids_juegan4, median(subtotal4) ]
  ids_juegan5  <- planilla_cazatalentos[ ids_juegan4 ][subtotal4 > mediana5, id ]
  
  planilla_cazatalentos[ ids_juegan5,  tiros5 := t5 ]
  resultado5  <- gimnasio_tirar( ids_juegan5, t5 )
  planilla_cazatalentos[ ids_juegan5,  aciertos5 := resultado5 ]
  
  planilla_cazatalentos[ ids_juegan5, subtotal5 := subtotal4+aciertos5 ]
  
  #Ronda 6 -------------------------------------------------------
  #mediana6  <- planilla_cazatalentos[ ids_juegan5, median(subtotal5) ]
  #ids_juegan6  <- planilla_cazatalentos[ ids_juegan5 ][subtotal5 > mediana6, id ]
  
  #planilla_cazatalentos[ ids_juegan6,  tiros6 := t6 ]
  #resultado6  <- gimnasio_tirar( ids_juegan6, t6 )
  #planilla_cazatalentos[ ids_juegan6,  aciertos6 := resultado6 ]
  
  #planilla_cazatalentos[ ids_juegan6, subtotal6 := subtotal5+aciertos6 ]
  
  #VEREDICTO -----------------------------------------------------
  pos_mejor <-  planilla_cazatalentos[ , which.max(subtotal5) ]
  id_mejor  <-  planilla_cazatalentos[ pos_mejor, id ]
  veredicto  <- gimnasio_veredicto( id_mejor )
  return( veredicto )
}

#Aqui hago la Estimacion Montecarlo del porcentaje de aciertos que tiene la estrategia A

set.seed( 102191 )  #debe ir una sola vez, ANTES de los experimentos

tabla_veredictos  <- data.table(  tiros_total=integer(),  acierto=integer() )

for( experimento  in  1:10000 )
{
  if( experimento %% 1000 == 0 )  cat( experimento, " ")  #desprolijo, pero es para saber por donde voy
  
  veredicto  <- Estrategia()
  
  tabla_veredictos  <- rbind( tabla_veredictos, veredicto )
}

cat("\n")

tiros_total  <-  tabla_veredictos[  , max( tiros_total ) ]
tasa_eleccion_correcta  <-  tabla_veredictos[  , mean( acierto ) ]

tiros_total
tasa_eleccion_correcta

