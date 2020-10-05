
#cours 5
powers3 <- function(x,dg) return(outer(x, 1:dg, "^"))

x <- runif(200)
dgs <- 1:8
replicate(200,powers3(x,dgs))

#TP5
# 1 Profilage I
# Profiler les 4 fonctions qui calculent la matrice de regressors dans la régression polynomiale.
# Quelles sont les principales sources de ralentissement du code?

powers1 <- function(x, dg) {
  pw <- matrix(x,nrow = length(x))
  prod <- x # current product
  for (i in 2:dg) {
    prod <- prod * x
    pw <- cbind(pw, prod)
  }
  return(pw)
}

powers2 <- function(x, dg) {
  pw <- matrix(nrow = length(x), ncol = dg)
  prod <- x # current product
  pw[,1] <- prod
  for (i in 2:dg) {
    prod <- prod * x
    pw[,i] <- prod
  }
  return(pw)
}

powers3 <- function(x, dg) return(outer(x, 1:dg, "^"))

powers4 <- function(x, dg) {
  repx <- matrix(rep(x, dg), nrow = length(x))
  return(t(apply(repx, 1, cumprod)))
}

# 2 Profilage II
# 1. Profiler le code de la fonction mymds que vous avez écrit dans le cours de Manifold Learning.
# 2. Identifier le principal goulot d’étranglement. Est ce possible de le contourner ?
# 3. Comparer avec le profilage de cmdscale.
# 4. Changer le Blas de base de R au Blas optimisé OpenBlas (ancien GotoBlas). Attention si vous travaillez
# sur une machine Linux ce changement affectera d’autres langages comme python ou octave.
# 5. Refaire les expériences de profilage sur mymds et cmdscale.