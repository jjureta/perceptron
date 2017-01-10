x1 <- runif(30,-1,1) 
x2 <- runif(30,-1,1) 
x <- cbind(x1,x2) 
Y <- ifelse(x2>0.5+x1,+1,-1) 
plot(x,pch=ifelse(Y>0,"+","-"), xlim=c(-1,1),ylim=c(-1,1),cex=2) 
abline(0.5,1) 
points(c(0,0),c(0,0),pch=19) 
lines(c(0,-0.25),c(0,0.25),lty=2) 
arrows(-0.3,0.2,-0.4,0.3)
text(-0.45,0.35,"w",cex=2) 
text(-0.0,0.15,"b",cex=2)

# XOR data
xor_data <- data.frame(t = c(0, 1, 1, 0), 
                       b = c(1, 1, 1, 1),
                       x = c(1, 0, 0, 0),
                       x0 = c(0, 0, 1, 1), 
                       x1 = c(0, 1, 0, 1))

xor_data1 <- data.frame(t = c(0, 1, 1, 0), 
                       x = c(1, 0, 0, 0),
                       x0 = c(0, 0, 1, 1), 
                       x1 = c(0, 1, 0, 1))

and_data <- data.frame(t = c(0, 0, 0, 1), 
                       b = c(1, 1, 1, 1),
                        x0 = c(0, 0, 1, 1), 
                        x1 = c(0, 1, 0, 1))