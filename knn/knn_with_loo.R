eDist <- function(u, v) {
  sqrt(sum((u - v)^2))
}

knn <- function(z, X, k){
  xl <- X[ , 3:5]
  l <- dim(xl)[1] 
  n <- dim(xl)[2] - 1
  
  distances <- c()
  for(tmp in 1:l){
    distances[tmp] <- eDist(xl[tmp, 1:n], z)
  }
  orderedxl <- xl[order(distances), ]
  classes <- orderedxl[1:k, n + 1]  
  counts <- table(classes) 
  return(names(which.max(counts)))
}

loo <- function(X, alg, step, x_max){
  plot(NULL, NULL, type = "l", xlim = c(0, x_max), ylim = c(0, 1), xlab = 'k', ylab = 'loo')
  step <- 5
  Ox <- seq(from = 1, to = x_max, by = step)
  Oy <- c()
  
  l <- dim(X)[1]
  LooOpt <- 1
  kOpt <- 1
  
  for(k in Ox){
    Q <- 0
    for(i in 1:l){
      X2 <- X[-i, ]
      z <- X[i, 3:4]
      if(alg(z, X2, k) != X[i, 5]) Q <- Q + 1
    }
    Loo <- Q/l
    Oy <- c(Oy, Loo)
    print(k)
    print(Loo)
    if(Loo < LooOpt) {
      LooOpt <- Loo
      kOpt <- k
    }
  }
  lines(Ox, Oy, pch = 8, bg = "black", col = "green3")
  points(kOpt, LooOpt, pch = 8, bg = "black", col = "blue")
  return(kOpt)
}

k <- loo(iris, knn, 1, dim(iris)[1])
print(k)

colors <- c("setosa" = "red", "versicolor" = "green3", "virginica" = "blue")
plot(iris[, 3:4], pch = 21, bg = colors[iris$Species], col = colors[iris$Species])

st3 <- seq(from = min(iris[, 3]), to = max(iris[, 3]), by = 0.3)
st4 <- seq(from = min(iris[, 4]), to = max(iris[, 4]), by = 0.3)

for(i in st3){
  for(j in st4){
    z <- c(i, j)
    points(z[1], z[2],  pch = 21, bg = "lightgoldenrodyellow", col = colors[knn(z, iris, k)])
  }
}

legend("bottomright", c("virginica", "versicolor", "setosa"), pch = c(15,15,15), col = c("blue", "green3", "red"))
