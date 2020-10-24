#Révision TP1
#20201020

# Exo 1 ####
is.integer(2)
typeof(2)

if(sqrt(2) * sqrt(2) != 2L) print("what ?!")
sqrt(2)*sqrt(2)

if(0.1 + 0.2 == 0.3) print("result is ok")
if(0.10 + 0.20 == 0.30) print("result is ok")
#Explication
#https://techtalkbook.com/why-0-1-0-2-does-not-equal-0-3/
if(0.5 + 0.25 == 0.75 )print("of course")

if(0.1 + 0.2 != 0.3) print("no way !!!!")


# Exo 2 ####
#1.
f <- function(x) sin(x) ^ 2 + sqrt(abs(x-3))
f <- function(x){
  return( sin(x) ^ 2 + sqrt(abs(x-3)) ) 
}
#2.
#a.
curve(f, from = -6, to = 4, lwd = 2)
grid(lty = 1)
#b. ggplot ?
#ggplot()
#3. 
integrate(f, -6, 4)
#4. ...

# Exo 3 ####
## Exercice 3 (Problème) : deuxième version ####

rm(list = ls())
# 1.
simuData <- function(n) rnorm(n)
# 2.
perte <- function(s, y, p) (sum(abs(s - y) ^ p)) ^ (1 / p)
# 3.
y <- simuData(20)

ps <- c(1, 2, 5, 1/2)
lps <- length(ps)
results <- data.frame(p         = numeric(lps),
                      minimum   = numeric(lps),
                      objective = numeric(lps))

for (p in seq_along(ps)) {
  print(paste("Begin computation for p = ", ps[p]))
  res <- optimise(f = perte, interval = range(y), y = y, p = ps[p])
  results[p, ] <- c(ps[p], res)
}
# 4. Représenter de manière graphique la fonction de perte ainsi que la valeur optimale.
vecto <- Vectorize(perte)
curve(vecto, from = 0, to = 1)

#version imene 
## Exercice 3 ####
## Devoir 1
## erreur dans l'énnonce : sum(abs(s-yi))

rm(list = ls())
# 1.
simuData <- function(n) {
  set.seed(2020)
  runif(n)
}

# 2.
perte <- function(s, y = simuData(20), p = 2) (sum(abs(s - y) ^ p)) ^ (1 / p)

# 3.
y <- simuData(20)

ps <- c(1, 2, 5, 1/2)
lps <- length(ps)
lps
results <- data.frame(p = numeric(lps),
                      minimum = numeric(lps),
                      objective = numeric(lps))

for (p in seq_along(ps)) {
  print(paste("Begin computation for p = ", ps[p]))
  res <- optimise(f = perte, interval = range(y), y = y, p = ps[p])
  results[p, ] <- c(ps[p], res)
}
results

#4.
vectorized_perte <- Vectorize(perte)
curve(vectorized_perte, from = 0, to = 2) #Plot for p = 2
abline(h = results$objective[1], col='blue')
abline(h = results$objective[2], col='red')

#5.
print(paste("Median of y : ", median(y))) # should be minimum for p = 1
print(paste("Minimmum of loss function for p = 1 : ", results$minimum[1]))

print(paste("Mean of y : ", mean(y))) # should be minimum for p = 2
print(paste("Minimmum of loss function for p = 2 : ", results$minimum[2]))
# Exos Additionnels