eDist <- function(u, v) {
  sqrt(sum((u - v)^2))
}

CoreRect <- function(z){
  if(abs(z) <= 1) return(0.5)
  else return(0)
}

CoreTriang <- function(z){
  if(abs(z) <= 1) return(1 - abs(z))
  else return(0)
}

CoreQuart <- function(z){
  if(abs(z) <= 1) return((15/16)*(1 - z^2)^2)
  else return(0)
}

CoreEpan <- function(z){
  if(abs(z) <= 1) return(0.75*(1 - z^2))
  else return(0)
}

CoreGauss <- function(z){
  (2*pi)^(-0.5)*exp(-0.5*z^2)
}


loo <- function(X, g, K, h){
  LOO <- 0
  l <- dim(X)[1]
  for(i in 1:l){
    X2 <- X[-i, ]
    z <- X[i, 3:4]
    rownames(X2) <- 1:(l-1)
    if(pf(X2, z, g[-i], K, h) != X[i, 5]) LOO <- LOO + 1
  }
  print(LOO)
  print(l)
  LOO <- LOO / l
  print(LOO)
  return(LOO)
}

gamma <- function(X, K, h, eps = 0.15){
  xl <- X[, 3:5]                  
  l <- dim(xl)[1]
  g <- c()
  i <- 1:l
  g[i] <- 0
  while(loo(X, g, K, h) > eps){
    rand <- sample(i, 1, replace = T)
    if(pf(X, c(xl[rand, 1], xl[rand, 2]), g, K, h) != xl[rand, 3]) g[rand] <- g[rand] + 1
    print(g)
  }
  return(g)
}

pf <- function(X, z, g, K, h){
  xl <- X[, 3:5]
  l <- dim(xl)[1]
  n <- dim(xl)[2] - 1
  
  distances_weighed <- matrix(NA, l, 2)
  for(tmp in 1:l){
    distances_weighed[tmp, 1] <- eDist(xl[tmp, 1:n], z)
    distances_weighed[tmp, 2] <- g[tmp]*K(distances_weighed[tmp, 1]/h)
  }
  
  classes <- data.frame(distances_weighed[ , 1], distances_weighed[ , 2], xl[ , 3])
  colnames(classes) <- c("Distances", "Weights", "Species")
  sumSetosa <- sum(classes[classes$Species == "setosa", 2])
  sumVersicolor <- sum(classes[classes$Species == "versicolor", 2])
  sumVirginica <- sum(classes[classes$Species == "virginica", 2])
  ans <- matrix(c(sumSetosa, sumVersicolor, sumVirginica), 
                nrow = 1, ncol = 3, byrow = T, list(c(1), c('setosa', 'versicolor', 'virginica'))) 
  return(colnames(ans)[which.max(ans)])
}

X <- iris
h <- 0.8
K <- CoreGauss
g <- gamma(X, K, h, 0.15)
print("gamma")
print(g)
print(length(g))
step <- 0.2

colors <- c("setosa" = "red", "versicolor" = "green3", "virginica" = "blue")
plot(iris[, 3:4], pch = 21, bg = colors[iris$Species], col = colors[iris$Species])

st3 <- seq(from <- min(iris[, 3]), to <- max(iris[, 3]), by <- step)
st4 <- seq(from <- min(iris[, 4]), to <- max(iris[, 4]), by <-step)

for(i in st3){
  for(j in st4){
    z <- c(i, j)
    points(z[1], z[2],  pch = 21, bg = "lightgoldenrodyellow", col = colors[pf(X, z, g, K, h)])
  }
}

for(i in 1:dim(X)[1]){
  if(g[i] != 0) points(X[i, 3], X[i, 4], pch = 21, bg = "black", col = colors[pf(X, c(X[i, 3], X[i, 4]), g, K, h)])
}

legend("bottomright", c("virginica", "versicolor", "setosa", "center"), pch = c(15, 15, 15, 15), col = c("blue", "green3", "red", "black"))
