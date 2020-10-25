## File: simulate_exam.r
## Description: Functions to conduct multivariate simulation using
##              different approaches (indep, indep on PCA).
## Date: Jan 2017 based on original code from Cugliari, Rolland

summarize <-
  function(multitest,
           method.vect,
           corel,
           nb_data,
           nb_var,
           n_simu,
           nb.iter,
           nb.test) {
    tableau_final <-
      array(dim = c((
        length(corel) * length(nb_data) * 3 * (nb.iter ^ 2)
      ), 4))
    k <- 1
    for (a in 1:length(corel)) {
      for (b in 1:length(nb_data)) {
        for (c in 1:3) {
          for (i in 1:(nb.iter ^ 2)) {
            tableau_final[k, 1] <- corel[a]
            tableau_final[k, 2] <- nb_data[b]
            tableau_final[k, 3] <- method.vect[c]
            tableau_final[k, 4] <- multitest[a, b, c, i]
            k <- k + 1
          }
        }
      }
    }
    
    colnames(tableau_final) <- c("corel", "nb_data", "methode", "pv")
    return(tableau_final)
  }

main2 <- function(method.vect, corel, nb_data, nb_var,
                  n_simu, nb.iter, nb.test) {
  multitest <- array(dim = c(length(corel), length(nb_data), 3, nb.iter ^ 2))
  
  for (i in 0:(nb.iter - 1)) {
    for (a in 1:length(corel)) {
      for (b in 1:length(nb_data)) {
        df <- runif_corel(nb_data[b], nb_var, corel[a] ^ 2)
        df[, 3] <- log(df[, 3]) # la variable 3 devient logarithmique
        df[, 4] <- exp((df[, 4] - min(df[, 4]))) 
        df <- scale(df)
        for (met in 1:2) {
          for (j in 1:nb.iter) {
            res <- simulatemulti(data = df, method = method.vect[met])
            multitest[a, b, met, (i * nb.iter + j)] <-
              eqdist.etest(rbind(df, res), c(nrow(df), nrow(res)))$p.value
          }
        }
      }
    }
  }
  return(multitest)
}


main <-
  function(method.vect,
           corel,
           nb_data,
           nb_var,
           n_simu,
           nb.iter,
           nb.test) {
    multitest <-
      array(dim = c(length(corel), length(nb_data), 3, nb.iter ^ 2))
    
    for (a in 1:length(corel)) {
      for (b in 1:length(nb_data)) {
        print(corel[a])
        print(nb_data[b])
        
        for (i in 0:(nb.iter - 1)) {
          print(i)
          df <- runif_corel(nb_data[b], nb_var, corel[a] ^ 2)
          df[, 3] <- log(df[, 3]) # la variable 3 devient logarithmique
          df[, 4] <- exp((df[, 4] - min(df[, 4]))) 
          df <- scale(df)
          for (met in 1:2) {
            for (j in 1:nb.iter) {
              res <- simulatemulti(data = df, method = method.vect[met])
              multitest[a, b, met, (i * nb.iter + j)] <-
                eqdist.etest(rbind(df, res),
                             c(nrow(df), nrow(res)))$p.value
            }
          }
        }
      }
    }
    return(multitest)
  }

runif_corel <- function(nb_data, nb_var, corel) {
  cholevski <- matrix(rep(corel, nb_var ^ 2), nrow = nb_var)
  diag(cholevski) <- 1
  df <- rmvnorm(nb_data, sigma = cholevski, method = "chol")
  rank_mat <- rank(df)
  unif_mat_sorted <- sort(runif(nb_data * nb_var))
  unif_mat <- unif_mat_sorted[rank_mat]
  unif_mat <- array(unif_mat, dim = c(nb_data, nb_var))
  runif_corel <- unif_mat
}

simulatemulti <- function(data, method, n, ...) {
  if (missing(n))
    n <- nrow(data)
  switch(
    method,
    indep    = simudataindep(data, n),
    indepPCA = simudataindepPCA(data, n, ...)
  )
}

## Simulation from independent data
simudataindep <- function(data, n_simu) {
  if (missing(n_simu))
    n_simu <- nrow(data)
  
  data_sim <- matrix(runif(n_simu * ncol(data)), nrow = n_simu)
  
  for (j in 1:ncol(data)) {
    data_sim[, j] <- icdf(u = data_sim[, j],
                          x = data[, j],
                          n = n_simu)
  }
  
  colnames(data_sim) <- colnames(data)
  return(data_sim)
}

## Simulation from independent data
simudataindepPCA <- function(data, n_simu) {
  if (missing(n_simu))
    n_simu <- nrow(data)
  prfit <- prcomp(data, scale. = TRUE, retx = TRUE)
  
  x_sim <- simudataindep(data = prfit$x, n_simu)
  
  data_sim_sc <- x_sim %*% solve(prfit$rotation)
  data_sim <- sweep(data_sim_sc %*% diag(prfit$scale), 2,
                    FUN = "+", prfit$center)
  
  colnames(data_sim) <- colnames(data)
  return(data_sim)
}

## Function: fpx
## Description : Obtain the ranks of a vector (A factor of
##               0.5 is used to avoid extremes values of ranks).
fpx <- function(x)
  (rank(x, ties.method = "max") - 0.5) / length(x)


## Function: icdf [1st paper version]
## Description : Generalized inverse of the empirical cumulative
##               function. (Should be checked!!!!)
# icdf <- function(u, x, n) {
#   xstar <- numeric(n)
#   for(i in seq_along(xstar)){
#     cand <- ((rank(x) - 0.5) / length(x) - u[i]) <= 0
#     xstar[i] <- ifelse(!any(cand), min(x), max(x[cand]))
#   }
#   return(xstar)
# }

## Function: icdf
## Description : Generalized inverse of the empirical cumulative
##               function.
icdf <- function(u, x, n) {
  freq <- fpx(x)
  Fn   <- splinefun(x, freq, method = "monoH.FC")
  
  xstar <- numeric(n)
  
  for (i in seq_along(xstar)) {
    xstar[i] <- uniroot(
      function(x)
        Fn(x) - u[i],
      range(x),
      extendInt = "upX",
      f.lower = -u[i],
      f.upper = 1 - u[i]
    )$root
  }
  return(xstar)
}