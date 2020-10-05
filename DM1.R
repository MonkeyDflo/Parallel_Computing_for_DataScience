#Devoir Maison n°1
#04/10/2020
#Florent Jakubowski

#I.1 1ère variante : méthode du rejet ####

simuRejection <- function(k, n, seed, isPlotted = FALSE){
  # Input
  #k dégrés du polynôme
  #n nombres de point à tester
  #seed graine pour la génération aléatoire
  #isPlotted Booléen pour afficher ou non le plot à chaque étape
  
  #1ère étape : générer n couple (u,v) aléatoire avec u et v compris [0;1]
  set.seed(seed)
  u <- runif(n) # generate samples from uniform distribution (0.0, 1.0)
  v <- runif(n)
  isinDk <- vector(,n)
  df <- data.frame(u, v, isinDk)
  
  #condition
  for(i in 1:n){
    if ( df$u[i]^(1/k) + df$v[i]^(1/k) <= 1 ){
      df$isinDk[i] <- TRUE
    }
  }
  
  if(isPlotted) {plot(df[(df$isinDk == TRUE),])}
  
}

simuRejection(1, 10000, 7)

#I.2 Mesure de la dégradation du temps de calcul ####

estimateTime <- function(k, n, seed, isPlotted = FALSE){
  for(j in k){
    print( paste("k : ",  k, " : ", system.time(simuRejection(j, n, seed, isPlotted))) )
  }
}

k <- c(1,2,3,4,5,10,20, 30, 40)

estimateTime(k, 10000, 7, TRUE)

#II Echantillonage de Gibbs####

gibbs<-function (n) 
{
  mat <- matrix(ncol = 2, nrow = n)
  x <- 0
  y <- 0
  mat[1, ] <- c(x, y)
  for (i in 2:n) {
    x <- runif(n) 
    y <- runif(n)
    mat[i, ] <- c(x, y)
  }
  mat
}

bvn<-gibbs(10000)
par(mfrow=c(3,2))
plot(bvn,col=1:10000)
plot(bvn,type="l")
plot(ts(bvn[,1]))
plot(ts(bvn[,2]))
hist(bvn[,1],40)
hist(bvn[,2],40)
par(mfrow=c(1,1))

