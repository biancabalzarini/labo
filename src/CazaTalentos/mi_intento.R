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
