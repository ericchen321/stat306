rm(list = ls())
x <- c(1, 2, 3, 4, 5)
y <- c(4, 1, 2, 2, 7)
y1 <- 2.6 +0.2*x
y2 <- 0.8 + 0.8*x
y3 <- 5.3 - 0.7*x
plot(x, y, type = "p")
lines(x, y1, type = "l", col = "red")
lines(x, y2, type = "l", col = "blue")
lines(x, y3, type = "l", col = "green")
sumOfSquares_y1 <- sum((y-y1)^2)
sumOfSquares_y2 <- sum((y-y2)^2)
sumOfSquares_y3 <- sum((y-y3)^2)