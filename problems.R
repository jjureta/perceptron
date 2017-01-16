library(testthat)

source("./learning.R")

# P 4.1
# a.
p <- matrix(c(0,1,1,1,1,0,1,-1,0,-1,-1,-1,-1,0,-1,1), 2, 8)
t <- matrix(c(1, 0, 0, 0, 0, 1, 1, 1), 8, 1)

w <- perceptron( p,  t, hard_limit_transfer)
expected <- list(w = matrix( c(-2, 1, -1), nrow = 3, ncol = 1), updates = 5)

test_that("Test perceptron P 4.1 a",{
  expect_equal( w$w, expected$w)
})

# b.
p <- matrix(c(0,2,2,2,2,0,2,-2,0,-2,-2,-2,-2,0,-2,2), 2, 8)
t <- matrix(c(0, 0, 0, 1, 1, 1, 0, 0), 8, 1)

w <- perceptron( p,  t, hard_limit_transfer)
expected <- list(w = matrix( c(0, -2, -1), nrow = 3, ncol = 1), updates = 1)

test_that("Test perceptron P 4.1 b",{
  expect_equal( w$w, expected$w)
})

# c.
p <- matrix(c(0,1,1,1,1,0,1,-1,0,-1,+1,-1,-1,0,-1,1), 2, 8)
t <- matrix(c(1, 1, 1, 1, 1, 1, 1, 0), 8, 1)

w <- perceptron( p, t, hard_limit_transfer)
expected <- list(w = matrix( c(2, -1, 2), nrow = 3, ncol = 1), updates = 5)

test_that("Test perceptron P 4.1 c",{
  expect_equal( w$w, expected$w)
})
