## nom du fichier 
## qui la créé
## Description 
## Date

## Exercice 1 ####
is.integer(2) #typé par défaut en double
is.integer(2L)

if(sqrt(2) * sqrt(2) != 2) print("what ?!") ## on peut modifier en mettant un seuil 

if(0.1 + 0.2 == 0.3) print("result is ok") ## pas l'arrondis dans le système décimale mais dans le système binaire
#On va tronquer le système binaire 

#en binaire 0.1 est périodique

if(0.1 + 0.2 != 0.3) print("no way !!!!")

## Exercice 2 ####
# 1.
# f(x)=sin(x)²+ sqrt(|x−3|)
f <- function(x) sin(x) ^ 2 + sqrt(abs(x-3))

# 2. 
v = seq(-6, 4, by = 0.1)
print(v)
u <- f(v)
print(u)
#plot
plot(v,u)
plot(f, from = -6, to = 4, main = "2.1")
#on peut utiliser curve aussi
#xlim = from et to 
#lwd = largeur
grid(lty = 1)#rajoute une grille

# 3.
integrate(f, lower=-6, upper=4)
#fonctionne par intégration numérique
#erreur bornée en fonction de l'écartement entre deux points dans la grille

#4. 
mn <- optimise(f, c(-6,4))
mx <- optimise(f, c(-6,4), maximum = TRUE)
#min(f(-6:4)) optimisation sur des valeurs discrètes
#avec optimise vraie opti sur l'intervalle de val continue. 
mx2 <- optimise(-f, c(-6,4))
#si optimise est convexe alors ça fonctionne 
#pour trouver le max de f(x) = min -f(x)

## Exercice 3 ####
#1. 

simuData <- function(x) {
  set.seed(20)
  runif(x)
}
#simuData(3)

#2. 
#perte(s, y, p)
data <- simuData(10)
pertes <- function(s, y=data, p=2) {(sum(abs(s - y)^p))^(1/p)}
pertes(2)
h <- Vectorize(pertes)
curve(h, 0, 10)
#pertes(2, simuData(300), 2)

#3. 
#vect_p = c(1,2,5,1/2)
argmin_p <- optimise(pertes, c(0,10), s=1, p=2)
#optimise(f, interval, parameters à fournir à f, lower, upper, ...)
#https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/optimize
print(argmin_p)

#3. correction
y <- simuData(20)

for(p in c(1,2,5,1/2)) {
  print(paste("Begin computation for p = ", p))
  print(optimise(f=perte, interval = range(y), y=y, p=p))
}


ps <- c(1,2,5,1/2)
lps <- length(ps)
results <- data.frame(p = numerics(lps), 
                      minimum = numerics(lps),
                      objective = numerics(lps))

#correction erreur avec le prof 

# x* pour le minimum 
# f(x*) => fonction objective => focntion qu'on cherche 

#identical permet de comparer deux vecteurs

#chronométrage
system.time(f2simple(xtest)) #pd car les fonctions sont trop rapides
system.time(f2renv(xtest))
#replicate pour répliquer un appel, on va faire une centaine d'appe pour comparer les temsp d'éxécution des fonctons

#Solution : utiliser microbenchmarking (avec répliques)
#library(microbenchmark)
#library(ggplot2)
#affiche un tableau avec les perf
#affiche un res graphique avec ggplot2

#estimateur à noyau
# barystogramme 

# un = est en fait une fonction '=(f,sin(x)^2+sqrt(abs(x-3))')
# tous les opérateurs sont des fonctions en fait

## Exercices additionnels ####

## test GIT ####

