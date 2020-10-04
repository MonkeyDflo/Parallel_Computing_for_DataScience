#Devoir Maison n°1
#04/10/2020
#Florent Jakubowski

#I.1 1ère variante : méthode du rejet ####

simuRejection <- function(k, n, seed, isPlotted = FALSE){
  # Input
  #k dégrés du polynôme
  #n nombres de point à tester
  #seed graine pour la génération aléatoire
  
  #1ère étape : générer n couple (u,v) aléatoire avec u et v compris [0;1]
  set.seed(seed)
  u <- runif(n) # generate samples from uniform distribution (0.0, 1.0)
  v <- runif(n)
  isinDk <- vector(,n)
  df <- data.frame(u, v, isinDk)
  
  #condition
  
  #try to use apply on df
  # test <- function(u,v){
  #   if ( u^(1/k) + v^(1/k) <= 1 ){
  #     return(u,v)
  #   }
  # }
  #apply(df, 1, function(df) test(df['u'],df['v'],df['isinDk']) )
  #df$isinDk <- apply(df[,1:2], 1, test(df$[u],df$[v]))
  
  for(i in 1:n){
    if ( df$u[i]^(1/k) + df$v[i]^(1/k) <= 1 ){
      df$isinDk[i] <- TRUE
    }
  }
  
  # print(df[(df$isinDk == TRUE),])
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
a

#install.packages("microbenchmark")
#install.packages("ggplot2")
# library(microbenchmark)
# library(ggplot2)
# m <- microbenchmark(estimateTime(k, 10000, 7),
#                     times = 1)
# ggplot2::autoplot(m)

#
