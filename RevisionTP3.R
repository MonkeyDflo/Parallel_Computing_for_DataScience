#Révision TP3
#Florent
#20201021

#Exercice 1 
library(parallel)

cluster <- makeCluster(2)
class(clusterCall(cluster, f<-function(n) runif(n), 2))
typeof(clusterCall(cluster, f<-function(n) runif(n), 2))
l <- clusterCall(cluster, f<-function(n) runif(n), 15)
l
unl <- unlist(clusterCall(cluster, f<-function(n) runif(n), 15))
unl
length(unl)
#ClusterCall : applique la fonction fournie en entrée sur les différents coeur de calcul 
#retourne ensuite le résultat sous la forme d'une liste de 2 dimensions 
#avec pour n éléments correspondant au nombre de coeur pour la 1ère dimension
#et m éléments pour la seconde, m étant le nombre d'applications fournies en entrée. 
#ex clusterCall(nb_cluster, f, m_applications) => retourne un tableau [nb_cluster][m_applications]

rm(cluster)

detectCores()           # handy function to ... detect the number of core !
cl <- makeCluster(2)
S_parlist <- clusterApply(cl, y_list, sum)
sum3   <- Reduce('+', S_parlist)
stopCluster(cl); rm(cl) # Safer

#Quelle est la nature de l’objet obtenu? une liste 
#Vous pouvez le convertir 
#en vecteur à l’aide de la fonction unlist.
#Répétez l’exercice maintenant en utiisant foreach

# version avec foeach
library(foreach)

f <-function(n) runif(1:n)

x <- foreach(i=1:2) %do% runif(2)
x
x <- foreach(i=1:2) %dopar% runif(2)
x
x <- foreach(i=1:2, .combine = sum) %do% runif(i, 0, 1)
x
x <- foreach(i=1:2, .combine = sum) %dopar% runif(i, 0, 1)
x

# Ex2 ####
#Calcul de la somme en parallel
# 1. Simulez un vecteur de données de taille 1e5 et obtenez sa somme.
# 2. Décopez le vecteur en 20 sous vecteurs de même taille à l’aide de la fonction split.
# 3. Utilisez la fonction lapply pour obtenir les sommes partialles de chaque slot de la liste de sous vecteurs,
# puis obtenez la somme.
# 4. Répéter le point 3 maintenant en utilisant du calcul en parallèle.
# 5. Comparez les résultats et chronometrez les temps d’exécution.

# 3 Parallélisme embarrassant
# On souhaite estimer la qualité de prédiction d’un modèle linéaire de la largeur d’une pétale sur sa longueur
# sur le jeu de données iris.
# La technique de leave-one-out consiste estimer l’erreur de généralisation de la manière suivante: on estime
# le modèle avec tous les individus sauf un, on fait une prédiction pour cet individu, et on regarde l’erreur
# quadratique entre la prédiction et la valeur connue; puis on répète l’opération pour chacun des individus
# et on somme les erreurs obtenues.
# 1. Charger le jeu de données iris.
data("iris")
print(head(iris))
print(col(iris$Petal.Width))
print(iris[1,4])
print(iris[1, iris$Petal.Width])
# 2. Écrire la fonction leave.one.out(i) qui renvoie l’erreur de prévision de la i-ème observation à partir
# du modèle entraîné sur les individus restantes.
plot(iris$Petal.Width ~ iris$Petal.Length)
boxplot(iris$Petal.Width ~ iris$Species)

leave.one.out <- function(i){
  fit <- lm(Petal.Width  ~ Petal.Length, data = iris) # Petal.Length = y On essaye de prédire Petal.Length
  pred  <- predict(fit, data.frame(Petal.Length = iris[i, "Petal.Length"]))
  error <- abs(pred - iris[i,"Petal.Length"])^2
  return(error)
}

debug(leave.one.out)
test <- leave.one.out(1)
test
# 3. Estimer l’erreur de généralisation par une boucle for.
sum <- 0
for (i in 1:nrow(iris)) {
  sum <- sum + leave.one.out(i)
}

f1_for <- function(){
  sum <- 0
  for (i in 1:nrow(iris)) {
    sum <- sum + leave.one.out(i)
  }
  return(sum)
}
print(f1_for())
# 4. Idem avec la fonction lapply.
resLA <- lapply(X = 1:nrow(iris), FUN =  leave.one.out)
Reduce("+", resLA)
length(resLA)
f2_lApply <- function(){
  return(Reduce("+", lapply(X = 1:nrow(iris), FUN =  leave.one.out)))
}
print(f2_lApply())

# 5. Remplacer lapply par la version parallèle du paquet parallel.
library(parallel)
detectCores()           # handy function to ... detect the number of core !
cl <- makeCluster(2)
resParLA <- parLapply(cl = cl , X=1:nrow(iris), fun = leave.one.out )
Reduce("+", resParLA)
f3_parLapply <- function(){
  return(Reduce("+", parLapply(cl = cl , X=1:nrow(iris), fun = leave.one.out )))
}
print(f3_parLapply())

# 6. Utiliser le paquet foreach pour estimer l’erreur de généralisation de manière séquentielle.
library(foreach)
foreach(i = 1:150, .combine = "+") %do% leave.one.out(i)
f4_foreach <- function(){
  return(foreach(i = 1:150, .combine = "+") %do% leave.one.out(i))
}
print(f4_foreach())
# 7. Utiliser les paquets foreach et doParallel pour estimer l’erreur de généralisation en parallèle.
library(foreach)
library(doParallel)
registerDoParallel(cores = 2) 
foreach(i = 1:150, .combine = "+") %dopar% leave.one.out(i)
f5_foreach_par <- function(){
  return(foreach(i = 1:150, .combine = "+") %dopar% leave.one.out(i))
}
print(f5_foreach_par())
# Pour les différents approches, chronométrez le temps d’exécutions et comparez.

library(microbenchmark)
df <- microbenchmark(f1_for, f2_lApply, f3_parLapply, f4_foreach, f5_foreach_par)

library(ggplot2)
ggplot2(df)
autoplot(df)