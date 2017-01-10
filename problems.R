source("./learning.R")

# P 4.1
# a.
p <- data.frame(p0 = c(0, 1, 1, 1, 0, -1,-1,-1),
                p1 = c(1, 1, 0,-1,-1, -1, 0, 1))

t <- data.frame(t = c(1, 0, 0, 0, 0, 1, 1, 1))

w <- perceptron(as.matrix(p),  as.matrix(t), hard_limit_transfer)
print(w)

# b.
p <- data.frame(p0 = c(0, 2, 2, 2, 0, -2,-2,-2),
                p1 = c(2, 2, 0,-2,-2, -2, 0, 2))

t <- data.frame(t = c(0, 0, 0, 1, 1, 1, 0, 0))

w <- perceptron(as.matrix(p),  as.matrix(t), hard_limit_transfer)
print(w)

# c.
p <- data.frame(p0 = c(0, 1, 1, 1, 0, -1,-1,-1),
                p1 = c(1, 1, 0,-1,-1, -1, 0, 1))

t <- data.frame(t = c(1, 1, 1, 1, 1, 1, 1, 0))

w <- perceptron(as.matrix(p),  as.matrix(t), hard_limit_transfer)
print(w)