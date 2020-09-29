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
