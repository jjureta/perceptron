library(testthat)

source("./learning.R")

# P 4.1
# a.
p <- data.frame(p0 = c(0, 1, 1, 1, 0, -1,-1,-1),
                p1 = c(1, 1, 0,-1,-1, -1, 0, 1))

t <- data.frame(t = c(1, 0, 0, 0, 0, 1, 1, 1))

w <- perceptron(as.matrix(p),  as.matrix(t), hard_limit_transfer)
expected <- list(w = matrix( c(-2, 1, -1), nrow = 3, ncol = 1), updates = 5)

test_that("Test perceptron P 4.1 a",{
  expect_equal( w$w, expected$w)
})

# b.
p <- data.frame(p0 = c(0, 2, 2, 2, 0, -2,-2,-2),
                p1 = c(2, 2, 0,-2,-2, -2, 0, 2))

t <- data.frame(t = c(0, 0, 0, 1, 1, 1, 0, 0))

w <- perceptron(as.matrix(p),  as.matrix(t), hard_limit_transfer)
expected <- list(w = matrix( c(0, -2, -1), nrow = 3, ncol = 1), updates = 1)

test_that("Test perceptron P 4.1 b",{
  expect_equal( w$w, expected$w)
})

# c.
p <- data.frame(p0 = c(0, 1, 1, 1, 0, -1,-1,-1),
                p1 = c(1, 1, 0,-1,-1, -1, 0, 1))

t <- data.frame(t = c(1, 1, 1, 1, 1, 1, 1, 0))

w <- perceptron(as.matrix(p),  as.matrix(t), hard_limit_transfer)
expected <- list(w = matrix( c(1, -1, 1), nrow = 3, ncol = 1), updates = 5)

test_that("Test perceptron P 4.1 c",{
  expect_equal( w$w, expected$w)
})

# P 4.3