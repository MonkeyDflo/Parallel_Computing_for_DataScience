f2 <- function(n) sum(seq(n))
#seq(n) est un alias de 1:n par contre seq(1,n) bcp plus long
f3 <- function(n)
  n * (n + 1) / 2

n <- 1e12
system.time(f2(n))
system.time(f3(n))

microbenchmark::microbenchmark(f2(n)), f3(n))

### exercice 4 ####

f_tri <- function(x) {
  SUP01 <- (0<=x) & (x<1)
  SUP12 <- (1<=x) & (x<2)
  if (SUP01){
    return(x)
  } else {if(SUP12){
    return(2-x)
  } else {
    return(0)
  }
    }
}

# & compare 
# && 
c(T,T,F) & c(F,T,F)
c(T,T,F) && c(F,T,F)
#T alias de TRUE
#F alias de FALSE
# if pas vectoriel, ne prend que le premier élément du vecteur

#Vectorize sert à vectoriser une fonction
#Prend une fonciton non vectorielle et donne une fonction vectorielle 
#f(c(2,1,3)) -> c(f(2),f(1),f(3))

#Vectorize est une routine en c 
#il appelle plusieurs fois la fonction 

#famille de fonction apply. 
#Derrière Vectorise il y a la fonction s.apply

#il faut réfléchir à comment faire du calcul vectorielle plutôt que des boucles for 

#if else est une commande vectorielle 


#Version optimisée 
f_tri2 <- fucntion(x){
  SUP01 <- (0<=x) & (x<1)
  SUP12 <- (1<=x) & (x<2)
  ifelse(SUPO1, x, ifelse(SUP12, 2 -x, 0))
}

library(microbenchmark)
xs <- seq(-3,3, length.out = 1e2)
microbenchmark(f_tri(xs), f_tri2(xs))

# Input 
rejection