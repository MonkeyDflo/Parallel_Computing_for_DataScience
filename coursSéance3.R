# cours séance 3 

#Codes test cours 3
#1er essai
nbproc<-4
nb<-1e6
y_vec<-rnorm(nbproc*nb)
(sum1<-sum(y_vec))
#2ème essai
y_list<-split(y_vec, rep(1:nbproc, each=nb))
(s_list<-lapply(y_list, sum))
(sum2<-Reduce('+', s_list))
identical(sum1, sum2)
#3ème essai: Parallélism
#install.packages("parallel")
library(parallel)


detectCores()
cl<-makeCluster(2)
s_parlist<-clusterApply(cl,y_list, sum)
(sum3<-Reduce('+', s_parlist))
stopCluster(cl);rm(cl) #Safer


#Exercice 2 Calcul de la somme en parallel ####
#1. Simulez un vecteur de données de taille 1e5 et obtenez sa somme.
n = 1e5

vect <- seq(length.out = n)
sum <- sum(vect)
sum2 <- n*(n+1)/2
print(sum)
print(sum2)
#2. Découpez le vecteur en 20 sous vecteurs de même taille à l’aide de la fonction split.
nb = n/20
vect_split <- split(vect, rep(1:20, each=n/20))
print(vect_split)

#deux manières d'utiliser rep
#rep(c('a','b'),3)
# => ababab
#rep(c('a','b'),each=3)
#=> aaa bbb

print(1:20)
print(n/20)
rep(1:20, each=n/20)

#test pour comprendre le split
#on créé un vecteur de lettres de char aléatoire
long_vect_caract <- sample(letters, 1e5, replace = TRUE)
#On visualise juste la tête 
head(long_vect_caract)
#On reprend le mm deuxième vecteur 
longue_liste <- split(long_vect_caract, rep(1:20, each = 1e5/20))
#lapply applique la même fonction à tous les slots
lapply(longue_liste, head)


#3. Utilisez la fonction lapply pour obtenir les sommes partialles de chaque slot de la liste de sous vecteurs,
#puis obtenez la somme.
#4. Répéter le point 3 maintenant en utilisant du calcul en parallèle.
#5. Comparez les résultats et chronometrez les temps d’exécution.

#pb avec clusterCall()
#cluster export 

#devoir obligatoire 
#1
#je veux simuler un vecteur (U,V) dépendant
#vecteur aléatoire 
#(U,V) unif sur Dk = {(u,v)€R²/ U^(1/k)+V^(1/k) <= 1}
#u,v>=0
#méthode du rejet pour deux dimensions ! 


#29092020

#4th try(foreach)
library(foreach)
library(doParallel)
foreach(i = y_list, combine = sum) %dopar% sum(i)
# alias qui permet de tout faire en séquentiel %do%
# mais avec la répartition du foreach
# généralement on code avec le do puis on passe au do par en suite
# le combine combine tous les résultats. 


## Exo 3 ####
#leave one out
#1. data(iris)
data(iris)
iris
print(unique(iris$Species))
scatter.smooth(x=iris$Sepal.Length, y=iris$Sepal.Width, main="Length ~ Width")  # scatterplot
print(head(iris))
print(head(iris[-1,]))
iris[1,]$Sepal.Length
fit <- lm(iris$Sepal.Length~iris$Sepal.Width, data=iris[-1,])
fit
yhat <- predict(fit, newdata = iris[1,])
yhat

#2. leave.one.out
leave.one.out <- function(i){
  fit <- lm(Sepal.Length~Sepal.Width, data=iris[-i,])
  yhat <- predict(fit, newdata = iris$Petal.Width[i])
  return (yhat-iris$Petal.Length[i])^2
}

#test 
leave.one.out(1)

#k-means avec les données centrées et réduites
#center = 4 - nombre de groupes demandés
#nstart = 5 - nombre d'essais avec différents individus de départ
#parce que les résultats sont dépendants de l’initialisation
#groupes.kmeans <- kmeans(fromage.cr,centers=4,nstart=5)
#affichage des résultats
#print(groupes.kmeans)
#correspondance avec les groupes de la CAH
#print(table(groupes.cah,groupes.kmeans$cluster))

