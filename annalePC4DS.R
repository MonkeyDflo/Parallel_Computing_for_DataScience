#révision Examen

## Fichier: 2017pc4ds-trump.r
## Etudian : D. Trump
## Description : Rendu de l'examen du PC4DS 2017
## Date : 26 janvier 2017

rm(list = ls())

################################################################################
####                                                                        ####
####                       E X E R C I C E   1                              ####
####                                                                        ####
################################################################################
# Description : Fonction permetant d'avoir le nombre d'éléments 
# manquants par ligne.
# Entrée : une matrice de taille n * m
# Sortie : un vecteur de taille n contenant le nombre 
# d' éléments non NA dans chaque colonne
## Un collègue qui travaille sur le test d'indépedance Chi-2 vous transmet le 
## code R ci dessous :

# test
set.seed(2020)
nas <- matrix(ifelse(runif(5e5) > 0.2, NA, 1), 1e4) 
a <- matrix(rnorm(5e5), 1e4) * nas
length(myfun(a))
head(myfun(a))

nas2 <- matrix(ifelse(runif(10) > 0.2, NA, 1), 3) 
nas2
a2 <- matrix(rnorm(2), 2) * nas2
a2

# Il vous demande de l'aide pour améliorer le temps de calcul. 

# __ 1. Rajoutez une description du code ####
# en suivant le canevas ci dessous

# Description : Fonction permetant d'avoir le nombre d'éléments 
# manquants par ligne.
# Entrée : une matrice de taille n * m
# Sortie : un vecteur de taille n contenant le nombre 
# d' éléments non NA dans chaque colonne

# __ 2. Écrivez une version plus facile à lire de myfun #### 
# Corrigez la mise en format du code, le noms des objets intermédiaires 
# et commentez-le si besoin.
myfun<-function(a){
  da <- dim(a) #retrieve dim of a
  n  <- da[1]; #retrieve number of row
  print(n)
  m  <- da[2]; #retrieve number of column
  print(m)
  print(nrow(a))
  res <- c() #initialize res
  for(i in seq(n)){ # first for loop, for i from 1 to n
    res<-c(res,0)  #add a 0 to res list
    for(j in seq(m)){ # second loop, for j from 1 to m 
      if( is.na( a[i,j] ) ) { # if a[i,j] is not ~null so
        res[i]<-res[i]+1 # res i equal res i + 1
      }
    }
  }
  return(res)}

res1 <- myfun(a)

# __ 3. Écrivez une version vectorisée de la fonction dans 2. ####
#une version vectorisée ? 

# __ 4. Écrivez une première version en parallèle de la fonction dans 2. ####
# en utilisant la librarie foreach (parallélisme implicite) et 2 noeuds de 
# calcul.
fun_feach <- function(a, n){
  m <- dim(a)[2]
  res <- 0
  for(j in seq(m)){ 
    if( is.na( a[n,j] ) ) { 
      res<-res+1 
    }
  }
  return(res)
}
  
library(foreach)
library(doParallel)
iterations = seq(dim(a)[1])
res4 <- foreach(n = iterations, .combine='c') %dopar% fun_feach(a, n) 
res4

myfunbis <- function(a){
  iterations = seq(dim(a)[1])
  res4 <- foreach(n = iterations, .combine='c') %dopar% fun_feach(a, n) 
  return(res4)
}

# __ 5. Écrivez une deuxième version en parallèle de myfun2 ####
# en utilisant la librarie parallel (parallélisme explicite) et 2 noeuds de 
# calcul.
library(parallel)
cl <- makeCluster(2)

myfun2 <-function(a){
  varRes <- clusterCall(cl = cl, fun = myfun, a)
  return(varRes)
} 

res5 <- myfun2(a)

# __ 6. Obtenez les temps d'exécution ####
# de toutes les versions de la fonction myfun que vous avez écrit.
# Quelle est la plus performant?
library(microbenchmark)
#df <- microbenchmark(f1, f2, f3, ...)
df <- microbenchmark(myfun(a), myfunbis(a), myfun2(a), times = 10L)
autoplot(df)
#reponse : 

identical(res1, res4, res5)

################################################################################
####                                                                        ####
####                       E X E R C I C E   2                              ####
####                                                                        ####
################################################################################

# Obtenir une version plus performante dela fonction main (cf. fichier 
# simulate_multivariate.r)

# Vous serez notés en fonction du gain obtenu. Ne modifiez pas les valeurs
# du point 1, UNIQUEMENT la fonction main.
# Astuce : n'essayez pas de rentrer dans le détail du code (assez long et )

#library(CDVine)
library(energy)
library(mvtnorm)
source('simulate_exam.r')

## 1. Choix pour les simulations ####
corel   <- seq(0.5, 0.95, by = 0.15)    # Corrélations
nb_data <- seq(20, 200, length.out = 4) # Taille des données réelles
nb_var  <- 4                            # Nombre de variables
n_simu  <- nb_data                      # Taille de données simulées
nb.iter <- 2                            # Nombre d'itérations
nb.test <- 5                            # Nombre de tests
method.vect <- c("indep", "indepPCA")   # Méthodes de simulation

## 2. Simulations ####
system.time(
  multitest <- main(method.vect, corel, nb_data, nb_var, 
                    n_simu, nb.iter, nb.test)
)
