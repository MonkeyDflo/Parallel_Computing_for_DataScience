#TD AA Exo 4.3
# V <- c(1,0,0,1/sqrt(3),2/sqrt(3),0)
# X <- matrix(V, nrow = 2, ncol = 3)
# solve((t(X) %*% X)) %*% t(X) %*% y

# Exo 1 
#A
#fonction qui prend x et 

# x <- matrix(c(1,2,3,4))
# x <- c(0,0,0,0,0,0,0,0,1,2,3,4)
# x <- matrix(x, nrow = 4, ncol = 3)

x <- c(1,2,3,4)
x <- matrix(x, nrow=4, ncol=1)
print(x)
print(cbind(x, x[,ncol(x)]*x[,ncol(x)]))

regPolyA <- function(X, dg){
  if(ncol(X)<dg){
    regPolyA( cbind(X, X[,ncol(X)]*X[,ncol(X)]), dg)
  }
  else{
    return(X)
  }
}

print(regPolyA(x, 3))

#B
#C
#D