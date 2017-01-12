library(testthat)

source("./learning.R")

p <- data.frame(p0 = c(1, 1, 2, 2,-1, -2,-1,-2),
                p1 = c(1, 2, -1,0, 2,  1,-1,-2))

t <- data.frame(t0 = c(0, 0, 0, 0, 1, 1, 1, 1),
                t1 = c(0, 0, 1, 1, 0, 0, 1, 1))

w <- perceptron(as.matrix(p),  as.matrix(t), hard_limit_transfer)
expected <- list(w = matrix( c(0, -2, -1), nrow = 3, ncol = 1), updates = 1)

test_that("Test perceptron P 4.1 b",{
  expect_equal( w$w, expected$w)
})