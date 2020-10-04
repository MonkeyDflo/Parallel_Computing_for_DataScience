#Devoir Maison n°1
#04/10/2020
#Florent Jakubowski

#1ère variante : méthode du rejet

simuRejection <- function(k, n, seed){
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
  
  print(df)
  #plot(df)
  #plot(df$u,df$v)
  
  #install.packages(plyr)
  library(plyr)
  print(subset(data, isinDk == TRUE))
  #plot(df[df$isinDk == TRUE])
   
  #plot histo ou point
  #return #un couple x,y,z accepté après les conditions ?
}

simuRejection(1,10,7)