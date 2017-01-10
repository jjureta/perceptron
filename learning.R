source('./transfer_functions.R')

classify <- function(x, weights, transfer_function) {
  return(transfer_function(x %*% weights))
}

perceptron <- function(obs, label, transfer_function) {
  k <- 0 # count updates
  w <- rep(0, ncol(obs)) # init weights
  n <- nrow(obs)
  
  misclassfied <- TRUE
  while (misclassfied) {
    misclassfied <- FALSE
    for (i in 1:n) {
      a <- classify(obs[i,], w, transfer_function)
      if (label[i, ] != a) {
        w <- w + (label[i, ] - a) * obs[i,]
        misclassfied <- TRUE
        k <- k + 1
      }
    }
    
    if (k > 250)
      stop("Did not converge.")
  }
  
  return(list(w = as.matrix(w), updates = k))
}
