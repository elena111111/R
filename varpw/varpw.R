eDist <- function(u, v) {
  sqrt(sum((u - v)^2))
}

KerRect <- function(z){
  if(abs(z) <= 1) return(0.5)
  else return(0)
}

KerTriang <- function(z){
  if(abs(z) <= 1) return(1 - abs(z))
  else return(0)
}

KerQuart <- function(z){
  if(abs(z) <= 1) return((15/16)*(1 - z^2)^2)
  else return(0)
}

KerEpan <- function(z){
  if(abs(z) <= 1) return(0.75*(1 - z^2))
  else return(0)
}

KerGauss <- function(z){
  (2*pi)^(-0.5)*exp(-0.5*z^2)
}


loo <- function(X, K, k, alg){
  LOO <- 0
  l <- nrow(X)
  for(i in 1:l){
    X2 <- X[-i, ]
    z <- X[i, 3:4]
    rownames(X2) <- 1:(l-1)
    if(alg(X2, z, K, k) != X[i, 5]) LOO <- LOO + 1
  }
  print(LOO)
  LOO <- LOO / l
  print("LOO:")
  print(LOO)
  return(LOO)
}

varpw <- function(X, z, K, k){
  xl <- X[, 3:5]
  l <- dim(xl)[1]
  n <- dim(xl)[2] - 1
  
  distances <- c()
  for(tmp in 1:l){
    distances[tmp] <- eDist(xl[tmp, 1:n], z)
  }
  orderedxl <- xl[order(distances), ]
  distances_s <- sort(distances)
  weights <- c()
  for(tmp in 1:k){
    if(distances_s[k+1] != 0)
      weights[tmp] <- K(distances_s[tmp]/distances_s[k+1])
    else weights[tmp] <- K(0)
  }
  #orderedxl_weighed <- cbind(orderedxl[1:k, ], weights)
  #classes <- orderedxl_weighed[ , (n + 1):(n + 2)]
  classes <- cbind(orderedxl[1:k, ], weights)[ , (n + 1):(n + 2)]
  sumSetosa <- sum(classes[classes$Species == "setosa", 2])
  sumVersicolor <- sum(classes[classes$Species == "versicolor", 2])
  sumVirginica <- sum(classes[classes$Species == "virginica", 2])
  ans <- matrix(c(sumSetosa, sumVersicolor, sumVirginica), 
                nrow = 1, ncol = 3, byrow = T, list(c(1), c('setosa', 'versicolor', 'virginica')))
  return(colnames(ans)[which.max(ans)])
}

colors <- c("setosa" = "red", "versicolor" = "green3", "virginica" = "blue")
plot(iris[, 3:4], pch = 21, bg = colors[iris$Species], col = colors[iris$Species])


X <- iris
k <- 6
K <- KerRect
step <- 0.1
loo(X, K, k, varpw)

st3 <- seq(from = min(X[, 3]), to = max(X[, 3]), by = step)
st4 <- seq(from = min(X[, 4]), to = max(X[, 4]), by = step)

for(i in st3){
  for(j in st4){
    z <- c(i, j)
    points(z[1], z[2],  pch = 21, bg = "lightgoldenrodyellow", col = colors[varpw(X, z, K, k)])
  }
}
points(iris[, 3:4], pch = 21, bg = colors[iris$Species], col = colors[iris$Species])
legend("bottomright", c("virginica", "versicolor", "setosa"), pch = c(15,15,15), 
       col = c("blue", "green3", "red"))
