# From: http://www.cs.ukzn.ac.za/~hughm/dm/content/slides07.pdf
euclidean.norm = function(x) {
  sqrt(sum(x * x))
}

distance.from.plane = function(z,w,b) { 
  sum(z*w) + b
}

classify.linear = function(x,w,b) { 
  distances = apply(x, 1, distance.from.plane, w, b) 
  return(ifelse(distances < 0, -1, +1))
}

perceptron = function(x, y, learning.rate=1) { 
  w = vector(length = ncol(x)) # initialize w 
  b = 0 # Initialize b 
  k = 0 # count updates
  R = max(apply(x, 1, euclidean.norm)) 
  made.mistake = TRUE # to enter the while loop 
  while (made.mistake) { 
    made.mistake=FALSE 
    # hopefully 
    yc <- classify.linear(x,w,b) 
    for (i in 1:nrow(x)) { 
      if (y[i] != yc[i]) { 
        w <- w + learning.rate * y[i]*x[i,] 
        b <- b + learning.rate * y[i]*R^2 
        k <- k+1 
        made.mistake=TRUE 
      } 
    }
    
    if (k > 20) 
      break
    
    print(w)
  } 
  s = euclidean.norm(w) 
  return(list(w=w/s,b=b/s,updates=k))
}

perceptron1 = function(data, learning.rate=1) {
  perceptron(data[ , 2:ncol(data)], data[ , 1], learning.rate)
}

# Classify is our simple classification rule for the perceptron.We simply 
# return the sign of the dot-product of our observations and weights
Classify <- function(x, weights) {
  return(sign(x %*% weights))
}

# https://github.com/billderose/perceptron/blob/master/perceptron.R
# Perceptron is a simple implementation of the perceptron learning algorithm.
# It accepts data of the form data[1] = label, data[2] = x_0 = 1, data[3] = x_1,
# etc. w0 is initilized to -threshold and the weights returned are such that
# sign(w_0 * x_0 + w_1 * x_1 + ... + w_n * x_n) == label
Perceptron <- function(data, threshold) {
  w <- c(-threshold, runif(ncol(data) - 2))
  n <- nrow(data)
  label <- data[ , 1]
  obs <- data[ , 2:ncol(data)]
  misclassfied <- TRUE
  while (misclassfied) {
    misclassfied <- FALSE
    for (i in 1:n) {
      if (label[i] * Classify(obs[i , ], w) <= 0) {
        w <- w + label[i] * obs[i , ]
        misclassfied <- TRUE
      }
    }
    
    print(w)
  }
  
  return(w)
}

perceptron_01 <- function(X, Y) {
  converged <- F
  
  # Initialize weight vector to 0, with NO bias neuron/constant/intercept term
  W <- matrix(0, 1, 4)
  
  # Run for 10000 iterations
  for (i in 1:10) {
    
    # Calculate h(x) with the weight vector W and the data input X
    
    h.X <- sign(W %*% t(X))
    
    print(h.X)
    
    # Calculate the misclassified mask
    
    misclassified.mask <- h.X != Y
    
    
    
    # Check if all of the points are classified correctly
    
    if (sum(misclassified.mask) == 0)
      
    {
      
      # Yes! We are done.
      
      converged <- T
      
      break
      
    }
    
    else
      
    {
      
      # No! We have to update the weight vector now using any one of the misclassified input
      
      
      
      # Get the misclassified points out
      
      misclassified.points <- X[misclassified.mask, , drop = F]
      
      misclassified.points.Y <- Y[misclassified.mask]
      
      
      
      # Get one of them
      
      misclassified.point.index <- sample(dim(misclassified.points)[1], 1)
      
      misclassified.point <- misclassified.points[misclassified.point.index, , drop = F]
      
      misclassified.point.Y <- misclassified.points.Y[misclassified.point.index]
      
      
      
      # Now update the weight vector
      
      W <- W + misclassified.point.Y %*% misclassified.point
      
    }
    
    
    
    # repeat
    
  }
  
  
  
  if (converged)
    
  {
    
    cat('Converged! Iteration ', i, ' , with final weight : ', W, '\n')
    
  }
  
  else
    
  {
    
    cat('DID NOT CONVERGE!\n')
    
  }
  
  
  
  return(W)
}

################################################################################
source('./transfer_functions.R')

# Classify is our simple classification rule for the perceptron.We simply 
# return the sign of the dot-product of our observations and weights
classify <- function(x, weights, transfer_function) {
  return(transfer_function(x %*% weights))
}

perceptron_jj <- function(data, transfer_function) {
  k <- 0 # count updates
  w <- rep(0, ncol(data) - 1) # init weights
  
  n <- nrow(data)
  label <- data[ , 1]
  obs <- data[ , 2:ncol(data)]
  misclassfied <- TRUE
  while (misclassfied) {
    misclassfied <- FALSE
    for (i in 1:n) {
      a <- classify(obs[i , ], w, transfer_function)
      if (label[i] != a) {
        w <- w + (label[i] - a) * obs[i , ]
        misclassfied <- TRUE
        k <- k + 1
      }
    }
    
    if (k > 250)
      stop("Did not converge.")
  }
  
  return(list(w=w,updates=k))
}
