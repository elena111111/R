Q_major <- function(M){
  return((1 - M)^2)
}

dQ <- function(M){
  return(2*M - 2)
}

V_major <- function(M){
  if((1 - M) > 0) return(1 - M)
  else return(0)
}

dV <- function(M){
  if(M > 0) return(-1)
  else return(0)
}

S_major <- function(M){
  return(2/(1 + exp(M)))
}

dS <- function(M){
  return(- 2*exp(M) / (1 + exp(M)))
}

L_major <- function(M){
  return(log2(1 + exp(-M)))
}

dL <- function(M){
  return(- exp(-M) / (log(2)*(1 + exp(-M))))
}

E_major <- function(M){
  return(exp(-M))
}

dE <- function(M){
  return(- exp(-M))
}

ScalarProduct <- function(a, b){
  return(sum(a*b))
}

eq <- function(c1, c2){ 
  if(length(c1) != length(c2)) return(FALSE)
  for(i in 1:length(c1)){
    if(c1[i] != c2[i]) return(FALSE)
  }
  return(TRUE)
}

midpoint <- function(v){ #коорд серед, v - вектор
  return(min(v) + (max(v) - min(v))/2)
}

sg <- function(Xl, eta, lambda, classes, major){  #дл€ двух классов c 2 признаками
  rownames(Xl) <- 1:nrow(Xl)
  l <- nrow(Xl)
  n <- ncol(Xl) - 1
  w <- runif(n, -1 / (2*n), 1 / (2*n))
  print(w)
  y <- c()
  y[1:l] <- 1
  
  for(i in 1:l) {
    if(Xl$Species[i] == classes[1]) { y[i] <- -1 }
  }
  
  Q <- 0
  for(i in 1:l)
    Q <- Q + major(y[i]*ScalarProduct(w, Xl[i, 1:2]))
  print(Q)
  
  w_new <- c()
  w_new[1:l] <- 0
  
  while(abs(Q) >= 0.01 && !eq(w, w_new)){
    w_new <- w
    rand <- sample(1:l, 1, replace = T)
    m <- y[rand]*ScalarProduct(w, Xl[rand, 1:2])
    e <- major(m)
    w <- w - eta * dQ(m) * Xl[rand, 1:2] * y[rand]
    print("w:")
    print(w)
    Q <- (1 - lambda)*Q + lambda*e
    print("Q:")
    print(Q)
    #попытки алгоритма построить раздел€ющую пр€мую:
    x <- c(min(Xl[, 2]), max(Xl[, 2]))
    y1 <- c(-w[[2]]*x[2]/w[[1]], -w[[1]]*x[1]/w[[2]])
    fit <- lm(c(y1[[2]], x[2])~c(x[1], y1[[1]]))
    abline(fit, col = "black")
  }
  return(w)
}

classes <- c("setosa", "versicolor")
#classes <- c("setosa", "virginica")
#classes <- c("versicolor", "virginica")
X <- iris[iris$Species == classes[1]  | iris$Species == classes[2], ]
X[ , 3] <- X[ , 3] - midpoint(X[ , 3])
X[ , 4] <- X[ , 4] - midpoint(X[ , 4])
lim_x <- c(min(X[, 3]), max(X[ , 3]))
lim_y <- c(min(X[, 4]), max(X[ , 4]))

colors <- c("setosa" = "red", "versicolor" = "green3", "virginica" = "blue")
plot(X[, 3:4], pch = 21, bg = colors[X$Species], col = colors[X$Species], 
     xlim = lim_x, ylim = lim_y)

eta <- 0.001
lambda <- 0.8 
major <- Q_major
w <- sg(X[, 3:5], eta, lambda, classes, major)
print(w)

#построение раздел€ющей гиперплоскости по 2м точкам
x <- c(min(X[, 4]), max(X[, 4]))
y <- c(-w[[2]]*x[2]/w[[1]], -w[[1]]*x[1]/w[[2]])
fit <- lm(c(y[[2]], x[2])~c(x[1], y[[1]]))
abline(fit, col = "deeppink1", lwd = 2)

legend("bottomright", c("virginica", "versicolor", "setosa"), pch = c(15,15,15), 
       col = c("blue", "green3", "red"))

step <- 0.1
st3 <- seq(from = lim_x[1], to = lim_x[2], by = step)
st4 <- seq(from = lim_y[1], to = lim_y[2], by = step)

for(i in st3){
  for(j in st4){
    z <- c(i, j) 
    if(ScalarProduct(w, z) < 0) 
      points(z[1], z[2], pch = 21, bg = "lightgoldenrodyellow", 
      col = colors[[classes[1]]])
    else if (ScalarProduct(w, z) > 0) 
      points(z[1], z[2], pch = 21, bg = "lightgoldenrodyellow", 
      col = colors[[classes[2]]])
    else points(z[1], z[2], pch = 21, bg = "lightgoldenrodyellow", col = "black")
  }
}

points(X[, 3:4], pch = 21, bg = colors[X$Species], col = colors[X$Species])
legend("bottomright", c("virginica", "versicolor", "setosa"), pch = c(15,15,15), 
       col = c("blue", "green3", "red"))
print(w[[1]]*x[1] + w[[2]]*y1[[1]])
print(w[[1]]*y1[[2]] + w[[2]]*x[[2]])