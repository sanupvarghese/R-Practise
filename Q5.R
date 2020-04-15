# Gradientdescent

install.packages("gradDescent")
library(gradDescent)
plot(Coding$Attendance,Coding$EndTermMarks,col='blue')
x <- Coding$Attendance
y <- Coding$EndTermMarks
w <-runif(1,0,1)
b <-runif(1,0,1)
Y <- w*x+b
gradientDesc <- function(x, y, learn_rate, conv_threshold, n, max_iter){
  plot(x, y, col = "blue", pch = 20)
  w <- runif(1, 0, 1)
  b <- runif(1, 0, 1)
  Y <- w * x + b
  MSE <- sum((y - Y) ^ 2) / n
  converged = F
  iterations = 100
  while(converged == F)
  {
    m_new <- w - learn_rate * ((1 / n) * (sum((Y - y) * x)))
    c_new <- b - learn_rate * ((1 / n) * (sum(Y - y)))
    w <- m_new
    b <- c_new
    Y <- w * x + b
    MSE_new <- sum((y - Y) ^ 2) / n
    if(MSE - MSE_new <= conv_threshold) {
      abline(b, w) 
      converged = T
      return(paste("Optimal intercept:", b, "Optimal slope:", w))
    }
    iterations = iterations + 1
    if(iterations > max_iter) { 
      abline(b, w) 
      converged = T
      return(paste("Optimal intercept:", b, "Optimal slope:", w))
    }
  }
}