eDist <- function(u, v) {
  sqrt(sum((u - v)^2))
}
k <- 6

wknn <- function(z, X, k, q){
  xl <- X[ , 3:5]
  l <- dim(xl)[1] 
  n <- dim(xl)[2] - 1
  
  distances <- c()
  for(tmp in 1:l){
    distances[tmp] <- eDist(xl[tmp, 1:n], z)
  }
  orderedxl <- xl[order(distances), ]
  Weights <- q^(1:k) 
  classes <- cbind(orderedxl[1:k, ], Weights)[ , (n + 1):(n + 2)] 
  
  sumSetosa <- sum(classes[classes$Species == "setosa", 2])
  sumVersicolor <- sum(classes[classes$Species == "versicolor", 2])
  sumVirginica <- sum(classes[classes$Species == "virginica", 2])
  
  ans <- matrix(c(sumSetosa, sumVersicolor, sumVirginica), 
                nrow = 1, ncol = 3, byrow = T, list(c(1), c('setosa', 'versicolor', 'virginica'))) 
  return(colnames(ans)[which.max(ans)])
}

loo <- function(X, alg, step, x_max){
  plot(NULL, NULL, type = "l", xlim = c(0, x_max), ylim = c(0, 1), xlab = 'q', ylab = 'loo')
  Ox <- seq(from = step, to = x_max, by = step)
  Oy <- c()
  
  l <- dim(X)[1]
  LooOpt <- 1
  qOpt <- 1
  
  for(q in Ox){
    Q <- 0
    for(i in 1:l){
      X2 <- X[-i, ]
      z <- X[i, 3:4]
      if(wknn(z, X2, k, q) != X[i, 5]) Q <- Q + 1
    }
    Loo <- Q/l
    Oy <- c(Oy, Loo)
    print(q)
    print(Loo)
    if(Loo < LooOpt) {
      LooOpt <- Loo
      qOpt <- q
    }
  }
  lines(Ox, Oy, pch = 8, bg = "black", col = "green3")
  points(qOpt, LooOpt, pch = 8, bg = "black", col = "blue")
  return(qOpt)
}

step <- 0.1
q <- loo(iris, knn, step, 1 - step)
print(q)

colors <- c("setosa" = "red", "versicolor" = "green3", "virginica" = "blue")
plot(iris[, 3:4], pch = 21, bg = colors[iris$Species], col = colors[iris$Species])

st3 <- seq(from = min(iris[, 3]), to = max(iris[, 3]), by = 0.3)
st4 <- seq(from = min(iris[, 4]), to = max(iris[, 4]), by = 0.3)

for(i in st3){
  for(j in st4){
    z <- c(i, j)
    points(z[1], z[2],  pch = 21, bg = "lightgoldenrodyellow", col = colors[wknn(z, iris, k, q)])
  }
}

legend("bottomright", c("virginica", "versicolor", "setosa"), pch = c(15,15,15), col = c("blue", "green3", "red"))

