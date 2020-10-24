#revision TP2
#Florent 
#20201020

#Ex1 ####
## I. Example : findruns ####
## _a. Meaningless naming, bad style : this is difficult to read and debug ! ####
joe=function(x,k){
  n=length(x)
  r=NULL
  for(i in 1:(n-k)) if(all(x[i:i+k-1]==1)) r<-c(r,i)
  r
}
# joe(c(1,0,0,1,1,1,0,1,1),2) # for testing

## _b. Better (buggy) version with corrected style and naming ####
## Find runs of consecutive 1s in 0-1 vectors
## e.g. findruns(c(1,0,0,1,1,1,0,1,1),2) shown return (4,5,8).
## Arguments   
##             x : vector containing 0s and 1s 
##             k : min size of the run
findruns <- function(x, k) {
  n <- length(x)
  runs <- NULL
  for (i in 1:(n - k + 1)) {
    if (all(x[i:(i + k - 1)] == 1)) runs <- c(runs, i)
  }
  return(runs)
}

#Ex2 ####
f1 <- function(n){
  res <- 0
  
  print()
  for (i in 1:n) {
    res <- res + 1
  }
  return(res)
  
}
debug(f1)
f1
undebug(f1)

#Ex3 ####

f2 <- function(n){sum(1:n)}
f2(3)
f3 <- function(n){n*(n+1)/2}
f3(3)

library(microbenchmark)
df <- microbenchmark(f3, f2)

library(ggplot2)
ggplot2(df)

autoplot(microbenchmark(f3(10e8), f2(10e8)))
