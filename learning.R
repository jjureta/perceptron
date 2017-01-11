source('./transfer_functions.R')

classify <- function(x, weights, transfer_function) {
  return(transfer_function(x %*% t(weights)))
}

perceptron <- function(obs, label, transfer_function, add_bias = TRUE) {
  k <- 0 # count updates
  
  if (add_bias) {
    obs <- cbind(obs, rep(1, nrow(obs)))   
  }
  
  w <- matrix(0, nrow = 1, ncol = ncol(obs)) # init weights
  n <- nrow(obs)
  
  misclassfied <- TRUE
  while (misclassfied) {
    misclassfied <- FALSE
    for (i in 1:n) {
      obsi <- obs[i,]
      labeli <- label[i, ]
      a <- classify(obsi, w, transfer_function)
      if (labeli != a) {
        print(obsi)
        w <- w + (labeli - a) * obsi
        misclassfied <- TRUE
        k <- k + 1
      }
    }
    
    if (k > 1000)
      stop("Did not converge.")
  }
  
  return(list(w = t(w), updates = k))
}
