#séance 6 

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

## TP5 ####
# 1 bigmemory
#   1. Initialiser une matrice de 1e4 lignes et 5 colonnes
#   2. Remplir la matrix avec de valeur aléatoires suivant une loi uniforme standard (∼U[0,1]).
#   3. Centrer et réduire les colonnes de la matrice
#   4. Obtenir une matrice de corrélations (Ce point peut se faire en mémoire)
#   5. Obtenir la décomposition aux valeurs singulièrs de la matrice de corrélations
# 2 Rcpp
#   Ecrire une version Rcpp de la fonction rejection. Mesurer le temps de calcul et comparer avec la fonction R.
# Défi
#   Ecrire une version de parmutout en Rcpp et lancer le calcul en parallèle de l’algorithme (vous pouvez utiliser foreach ou parallel).

#1.2 remplir une matrice avec une loi uniforme
#v <- matrix(runif(1))

library(bigmemory)

if (!file.exists("big.desc")) {
  
  # 100 000 000 lignes et 8 colonness =
  #           800 M d'éntrées * 8 bytes ~ 6 GB
  # ceci prends du temps :
  x <- big.matrix(nrow = 1e4,
                  ncol = 5,
                  init = 0, # Optionel mais souvent une bonné idée
                  backingfile    = "big.bin",
                  descriptorfile = "big.desc")
  
} else x <- attach.big.matrix("big.desc")
print(x)

#compromis temps/mémoire souvent
x[,1] <- runif(1e4, 0, 1)
x[,2] <- runif(1e4, 0, 1)
x[,3] <- runif(1e4, 0, 1)
x[,4] <- runif(1e4, 0, 1)
x[,5] <- runif(1e4, 0, 1)
head(x)

#centrage et réduction
#faire une boucle pour ne pas charger toute la matrice en mémoire
for(i in 1:ncol(x)){
  x[,i] <- scale(x[,i], center = TRUE, scale =TRUE)
}
#vérifier quelle est bien entrée et réduite 

matriceCor <- matrix(1, ncol=ncol(x), nrow=ncol(x))
for(i in 1:(ncol(x)-1) ){
  for(j in (i+1):ncol(x)){
    matriceCor[j,i] <- matriceCor[i,j] <- cor(x[,i],x[,j])
  }
}

lapply(1:5, function(i) x[,i]<- runif(10))
#pourquoi lapply commence avec un X ?

head(x)

#décomposition en valeur singulière
#on peut utilser aussi avec la fonction svd#

eigen(matriceCor)
decamp <- svd(matriceCor)

