#séance 6 
install.packages("bigmemory")

library(bigmemory)

if (!file.exists("big.desc")) {
  # 100 000 000 lignes et 8 colonness =
  #           800 M d'éntrées * 8 bytes ~ 6 GB
  # ceci prends du temps :
  x <- big.matrix(nrow = 1e6,
                  ncol = 8,
                  init = 0, # Optionel mais souvent une bonné idée
                  backingfile    = "big.bin",
                  descriptorfile = "big.desc")
  
} else x <- attach.big.matrix("big.desc")

# Attention : x à le comportement d'une matrice, mais ce n'est pas une matrice

x
dim(x)
x[1,1] <- 0.1234
x[nrow(x), 1] <- 0.5678
head(x)
tail(x)
system.time(x[,1] <- 1.234)

#######################################################################
# Faire attention aux dimensions, adapter en fonction de votre machine 

gc(reset=TRUE) # Utilisation de la mémoire
system.time(x[,3] <- runif(nrow(x)))
gc()           # Encore la mémoire (~.75 GB de mémoire R réelle)
gc(reset=TRUE) # La vision de la RAM selon R

##
## Moving on... column minima?
##

x[1:5,] <- rnorm(40)
x[6,1] <- NA
x[6,2] <- NaN
x[6,3] <- -Inf
x[6,4] <- Inf
x[nrow(x),] <- runif(8)
head(x)
tail(x)

####################################################
# Observer le temps et la conso de mémoire.

system.time({
  ans <- sapply(1:ncol(x), function(i) min(x[,i]))
})
ans

## TP5 Enoncé####
# 1 bigmemory
#   1. Initialiser une matrice de 1e4 lignes et 5 colonnes
#   2. Remplir la matrix avec de valeur aléatoires suivant une loi uniforme standard (∼U[0,1]).
#   3. Centrer et réduire les colonnes de la matrice
#   4. Obtenir une matrice de corrélations (Ce point peut se faire en mémoire)
#   5. Obtenir la décomposition aux valeurs singulières de la matrice de corrélations
# 2 Rcpp
#   Ecrire une version Rcpp de la fonction rejection. Mesurer le temps de calcul et comparer avec la fonction R.
# Défi
#   Ecrire une version de parmutout en Rcpp et lancer le calcul en parallèle de l’algorithme (vous pouvez utiliser foreach ou parallel).

# 1 bigmemory ####
#1.1. remplir une matrice avec une loi uniforme ####
library(bigmemory)
if (!file.exists("big.desc")) {
  x <- big.matrix(nrow = 1e4,
                  ncol = 5,
                  init = 0, # Optionel mais souvent une bonné idée
                  backingfile    = "big.bin",
                  descriptorfile = "big.desc")
  
} else x <- attach.big.matrix("big.desc")
print(x)

#1.2. Remplir la matrix avec de valeur aléatoires suivant une loi uniforme standard (∼U[0,1]). ####
#compromis temps/mémoire souvent
#ici nous allons chercher à ne pas utiliser toute la RAM de la machine !
#cette opération est plus longue que si nous avions fait un runif sur toute la matrice
#mais elle prend bcp moins de mémoire
x[,1] <- runif(1e4, 0, 1)
x[,2] <- runif(1e4, 0, 1)
x[,3] <- runif(1e4, 0, 1)
x[,4] <- runif(1e4, 0, 1)
x[,5] <- runif(1e4, 0, 1)
head(x)

#1.3 centrage et réduction des colonnes ####
#faire une boucle pour ne pas charger toute la matrice en mémoire
#même principe que précédemment, nous voulons utiliser un montant résonnable de mémoire
for(i in 1:ncol(x)){
  x[,i] <- scale(x[,i], center = TRUE, scale =TRUE)#on applique scale sur chaque colonne
  #x[ligne,colonne]
  #scale(vecteur ou matrice sur le/laquelle appliquer la méthode, 
  # center: booleen, donne une moyenne de 0, 
  # scale: booleen, donne un écart-type de 1)
  
  #centrée et réduire une colonne revient à calculer pour chaque valeur 
  # x = (X-mu)/sigma avec mu l'espérance et sigma l'écart-type
  #pour rappel mu = x[,i].reduce(sum())/nrow(x[,i]) -> FAUX
  #sigma = sqrt(variance)
  #variance = (1/nrow[,i])*sum((x(boucle sur chaque valeur de col) - mu)^2)
}
#vérifier quelle est bien entrée et réduite 


#1.4 calcul de la matrice de corrélation ####
matriceCor <- matrix(1, ncol=ncol(x), nrow=ncol(x))
#on initialise la matrice à 1 car une matrice de corrélation a toujours des 1 sur sa diagonale
#La corrélation d'une variable avec soi-même est égale à 1. 
for(i in 1:(ncol(x)-1) ){
  for(j in (i+1):ncol(x)){
    matriceCor[j,i] <- matriceCor[i,j] <- cor(x[,i],x[,j]) 
  }
}

lapply(1:5, function(i) x[,i]<- runif(10))
lapply(1:5, function(i) print(i))#applique la fonction sur chaque élément de X, ici X = c(1,2,3,4,5). 
head(x)

#1.5. Obtenir la décomposition aux valeurs singulières de la matrice de corrélations ####
#1. fonction svd#
eigen(matriceCor)
decamp <- svd(matriceCor)




# 2 Rcpp ####
#2.1. Ecrire une versin Rcpp de la fonction de rejection (exemple fait en cours avec pi) ####
#On peut essayer avec une autre fonction de rejet 
#2.1.a. fonction de rejet en R (+ fonction triangulaire) ####
f_tri <- function(x) {
  SUP01 <- (0 <  x) & (x < 1)
  SUP12 <- (1 <= x) & (x < 2)
  ifelse(SUP01, x, ifelse(SUP12, 2 - x, 0))
}
rejection <- function(f,a,b,M){
  # Nous supposons que f est une densité à support sur le segment [a,b] et qu’elle est
  # majorée uniformément sur son support par M.
  x <- runif(5000, a, b)#entre 0 et M
  y <- runif(5000, 0, M)
  print(length(x))
  print(length(y))
  
  res_x <- vector()
  res_y <- vector()
  
  for(i in 1:length(x)){
    print(i)
    if(y[i] <= f(x[i])){
      #on le garde
      #on ajoute le point dans un vecteur et on display le vecteur à chaque fois ?
      res_y <- c(res_y, y[i])
      res_x <- c(res_x, x[i])
      plot(res_x,res_y)
      }
  }
}
#2.1.b. test ####
rejection(f_tri, 0, 2, 1) 
#2.1.c. version du cours ####
rejection <- function(fx, a, b, M) {
  while (TRUE) { 
    x <- runif(1, a, b)
    y <- runif(1, 0, M)  
    if (y < fx(x)) return(x)
  }
}

#2.1.d. Test rejection ####
nreps <- 1000
Observations <- numeric(nreps) #create a vector of length nreps filed with O.
for (i in seq_along(Observations)) 
  Observations[i] <- rejection(f_tri, 0, 2, 1)

#2.1.e.Rcpp fonction de rejet écrite avec Rcpp ####
#2.1.f.Rcpp fonction de tri en Rcpp ####
Rcpp::cppFunction("float cppTri(float x) {
  bool SUP01 = (0 <  x) && (x < 1);
  bool SUP12 = (1 <= x) && (x < 2);
  if(SUP01){
    return(x);
  } else { 
    if(SUP12){
      return(2 - x);
    }
  }
}")

#2.1.g.Rcpp test cppTri ####
v <- runif(10, 0, 2)
print(v)
for(i in v){
  print(cppTri(i))
}

#2.1.h.Rcpp test sourceCpp("test.cpp")####
Rcpp::sourceCpp("test.cpp")
print(timesTwo(2))
print(timesThree(2))

#2.1.i.Rcpp Test cppRejection ####
cppRejection(f_tri, 0, 2, 1)

nreps <- 1000
Observations <- numeric(nreps) #create a vector of length nreps filed with O.
for (i in seq_along(Observations)) 
  Observations[i] <- cppRejection(f_tri, 0, 2, 1)

sapply(0:11, cppRejection(f_tri, 0, 2, 1))


#2.2. Mesurer le temps de calcul et comparer avec la fonction R ####

#Défi ####