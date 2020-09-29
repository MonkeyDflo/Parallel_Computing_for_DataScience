#révision TP1 à 3
#Florent Jakubowski 
#28092020

## TP1 exo 1 ####
is.integer(2)
is.integer(2L)
if(sqrt(2L) * sqrt(2L) != 2) print("what ?!")
if(0.1 + 0.2 == 0.3) print("result is ok")
if(0.1 + 0.2 != 0.3) print("no way !!!!")

## TP1 exo 2 ####
#1.
vec <- seq(100)
f <- function(x) sin(x) ^2 + sqrt(abs(x-3))
#curve(f, from = -6, to = 4, lwd=3)
#lwd = largeur de la courbe

#2. Integrate
integrate(f,-6,4)
interval = c(-6,4)
print(interval)
min_value <- optimise(f,interval, lower=min(interval), upper = max(interval))
## Notes Optimise
#minimum : point en x 
#objective : f(minimum)
#Tests 
print(min_value)
print(f(0.1503517))

## TP1 exo 3 ####
#1.
n=1000
simuData <- function(x) rnorm(x)
#rnorm = random generation for the normal distribution with mean equal to mean and standard deviation equal to sd. 
ens <- simuData(n)
#Mean :
print(mean(ens))
#Standard deviation : 
#The standard deviation of an observation variable is the square root of its variance. 
sd(ens)
#https://www.dummies.com/education/math/statistics/standard-deviation-r/
#2.
perte <- function(s,y,p) (sum(abs(s - y) ^ p)) ^ (1 / p)
#Comment on choisit s et p ? p arbitraire je pense, mais s je ne sais pas
p = 1
perte(s,ens,p)

#3. Ainsi, la valeur ˆy est la valeur qui rend la plus petite perte de représentation des donnéesypar une statistiques
y <- simuData(20)
#create a list to put all the minimum (arg min of f) into a list
res1 <- list()

#A DEBUGUER !!!
for(p in c(1, 2, 5, 1/2)) {
  print(paste("Begin computation for p = ", p))
  #print(optimise(f = perte, interval = range(y), y = y, p = p))
  print(optimise(f = perte, interval = range(y), y = y, p = p)$minimum)
  res1[p] <- optimise(f = perte, interval = range(y), y = y, p = p)$minimum
}
#argmins perte(s,y,p) = à minimum
print(paste("première version",res1))

#Ctrl+L — Clear the Console

##3.2 2e version 
ps <- c(1, 2, 5, 1/2)
lps <- length(ps)
results <- data.frame(p         = numeric(lps),
                      minimum   = numeric(lps),
                      objective = numeric(lps))

for (p in seq_along(ps)) {
  print(paste("Begin computation for p = ", ps[p]))
  res2 <- optimise(f = perte, interval = range(y), y = y, p = ps[p])
  results[p, ] <- c(ps[p], res2)
}

#print(paste("deuxième version",results))
print(results)

#représentation de manière graphique de la fonction perte
y <- simuData(20)
curve(perte, from = -6, to = 4, lwd=3, y = y, p = 1)

#5. Obtenir la solution du problème de manière analytique pour les valeurs de p=1,2.
res_anal_p1 <- optimise(f = perte, interval = range(y), y = y, p = 1)
print(res_anal_p1)
res_anal_p2 <- optimise(f = perte, interval = range(y), y = y, p = 2)
print(res_anal_p2)

#6. Rajouter au graphiques correspondantes les valeurs obtenues.

#7. Mesurer l’erreur de calcul.

## Exercices Additionnels 
#1. Obtenir le nombre maximal d’entiers qu’on peut représenter avec m bits.
m <- 2L
print(object.size(m))
#https://www.stat.auckland.ac.nz/~paul/ItDT/HTML/node76.html#:~:text=integer().,R%20data%20structure%20in%20memory.

#2. Soit y=1+x avec x un nombre positif. 
# Si y est représenté par virgule flottante (disons ỹ )quelle condition doit vérifier x pour que y˜=1 ?
  



