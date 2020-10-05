#TP4
#Florent Jakubowski
#29092020

# Ex1. Que fait ce code? ####
dormir <- function(i) {
  Sys.sleep(i)
  return(paste("le fils", Sys.getpid(), "a dormi", i, "secondes"))
}

temps <- list(5, 30, 5, 10)
temps2 <- list(c(5,5,10), 30)

library(parallel)
# Avec multicore (si disponible)
# mclapply(temps, dormir, mc.cores = 2)
# Avec SNOW
cl <- makeCluster(2)
#system.time(res <- parLapply(cl, temps2, dormir))
stopCluster(cl)


# Ex3. iris dataset is back ! ####


simuData <- function(sizes){
  model <- lm(Petal.Width ~ Petal.Length, data = iris)
  lapply(sizes, function (n) {
    a <- min(iris[, "Petal.Length"])
    b <- max(iris[, "Petal.Length"])
    iris2 <- data.frame(Petal.Width = rep(NA, n),
                        Petal.Length = runif(n, a, b))
    iris2[, "Petal.Width"] <- 
      predict(model, data.frame(Petal.Length = iris2[, "Petal.Length"]))
    return(iris2)
  })
}

## Exercice 2 ####

doichunk <- function(ichunk) {
  tot <- 0
  nr <- nrow(lnks) # lnks global at worker
  for(i in ichunk) {
    tmp <- lnks[(i + 1):nr , ] %*% lnks[i , ]
    tot <- tot + sum(tmp)
  }
  tot
}


mutoutpar <- function(cls, lnks) {
  require(parallel)
  nr <- nrow(lnks) # lnks global at worker
  #print(paste("nr : ",nr))
  deparse(substitute(lnks))
  clusterExport(cls, deparse(substitute(lnks)))
  ichunks <- 1:(nr - 1) # each "chunk" has only 1 value of i, for now
  #print(paste("ichunks : ",ichunks))
  tots <- clusterApply(cls, ichunks, doichunk)
  #print(paste("tots : ",tots))
  Reduce(sum, tots ) / nr
}

cl <- makeCluster(2)
lnks <- matrix(rnorm(500^2), nrow = 500) #rnorm créé un vecteur de 500^2
#matrix créé une matrice à partir d'un vecteur
mutoutpar(cl, lnks)

#deparse(substitute(links))


## Correction ####

## File: pc4ds-lab4.r
## Description: Lab 4 of Parallel Computing for Data Science
## Date:  Nov 2016 by jc

rm(list = ls())

# Ex1. Que fait ce code? ####
dormir <- function(i) {
  Sys.sleep(i)
  return(paste("le fils", Sys.getpid(), "a dormi", i, "secondes"))
}

temps <- list(5, 30, 5, 10)

library(parallel)
# Avec multicore (si disponible)
# mclapply(temps, dormir, mc.cores = 2)
# Avec SNOW
cl <- makeCluster(2)
system.time(res <- parLapply(cl, temps, dormir))
stopCluster(cl)

temps2 <- list(c(5,5,10), 30) 
cl <- makeCluster(2)
system.time(res2 <- parLapply(cl, temps2, dormir))
stop(cl)


## Ex. 2 ####
doichunk <- function(ichunk) {
  tot <- 0
  nr  <- nrow(lnks) # lnks global at worker
  
  for(i in ichunk) {
    tmp <- lnks[(i + 1):nr , ] %*% lnks[i , ]
    tot <- tot + sum(tmp)
  }
  tot
}

mutoutpar <- function(cls, lnks) {
  require(parallel)
  
  nr <- nrow(lnks) # lnks global at worker
  clusterExport(cls, "lnks")
  
  ichunks <- 1:(nr - 1) # each "chunk" has only 1 value of i , for now
  tots <- clusterApply(cls, ichunks, doichunk)
  
  Reduce(sum, tots ) / nr
}

mutoutparB <- function(cls, lnks) {
  require(parallel)
  
  nr <- nrow(lnks) # lnks global at worker
  clusterExport(cls, "lnks")
  
  ichunks <- (nr - 1):1 # each "chunk" has only 1 value of i , for now
  tots <- clusterApply(cls, ichunks, doichunk)
  
  Reduce(sum, tots ) / nr
}

mutoutparC <- function(cls, lnks) {
  require(parallel)
  
  nr <- nrow(lnks) # lnks global at worker
  clusterExport(cls, "lnks")
  
  ichunks <- clusterSplit(cls, 1:(nr - 1)) # each "chunk" has only 1 value of i , for now
  tots <- clusterApply(cls, ichunks, doichunk)
  
  Reduce(sum, tots ) / nr
}

mutoutparD <- function(cls, lnks) {
  require(parallel)
  
  nr <- nrow(lnks) # lnks global at worker
  clusterExport(cls, "lnks")
  
  smp <- sample(nr - 1)
  ichunks <- clusterSplit(cls, smp) 
  tots <- clusterApply(cls, ichunks, doichunk)
  
  Reduce(sum, tots ) / nr
}

mutoutparE <- function(cls, lnks) {
  require(parallel)
  
  nr <- nrow(lnks) # lnks global at worker
  clusterExport(cls, "lnks")
  
  ichunks <- lapply(1:length(cls), function(i) seq(i, nr - 1, by = i))
  tots <- clusterApply(cls, ichunks, doichunk)
  
  Reduce(sum, tots ) / nr
}

## Benchmark and timings with a simulated dataset ####

# Simulate a link matrix of size n (e.g. n = 500)
n   <- 500
lnks <- matrix(sample(0:1 , n^2, replace = TRUE), nrow = n)

#calcul séquentiel
system.time(lapply(1:(nrow(lnks) - 1), function(i) doichunk(i)))

## Paralle computation with snow
library(parallel)
cls <- makeCluster(4) 
system.time(mutoutpar(cls, lnks))
system.time(mutoutparB(cls, lnks))
system.time(mutoutparC(cls, lnks))
system.time(mutoutparD(cls, lnks))
system.time(mutoutparE(cls, lnks))


library(microbenchmark)
library(ggfortify)
compare <- microbenchmark(mutoutpar(cls, lnks), 
                          mutoutparB(cls, lnks), 
                          mutoutparC(cls, lnks),
                          mutoutparD(cls, lnks),
                          mutoutparE(cls, lnks),
                          times = 10)
autoplot(compare)
stopCluster(cls); rm(cls)

# You may use microbenchmark to have more significant timmings

## Exercice 3 ####
leave.one.out <- function(i, dataset) {
  fit   <- lm(Petal.Width ~ Petal.Length, data = dataset[-i, ]) 
  pred  <- predict(fit, data.frame(Petal.Length = dataset[i, "Petal.Length"]))
  error <- (pred - dataset[i, "Petal.Length"]) ^ 2
  return(error)
}

compute.PRESS <- function(dataset) {
  Reduce("+", lapply(1:nrow(dataset), function (i) leave.one.out(i, dataset)))
}

simuData <- function(sizes){
  model <- lm(Petal.Width ~ Petal.Length, data = iris)
  lapply(sizes, function (n) {
    a <- min(iris[, "Petal.Length"])
    b <- max(iris[, "Petal.Length"])
    iris2 <- data.frame(Petal.Width = rep(NA, n),
                        Petal.Length = runif(n, a, b))
    iris2[, "Petal.Width"] <- 
      predict(model, data.frame(Petal.Length = iris2[, "Petal.Length"]))
    return(iris2)
  })
}


library(parallel)


# Situation 1 
multiple.iris <- simuData(rep(200, 8))

cl <- makeCluster(4)
...
stopCluster(cl)

# foreach parallel
library(foreach)    # provides foreach
library(doParallel) # provides %dopar%
registerDoParallel(cores = 4) 
...
stopCluster(cl)

