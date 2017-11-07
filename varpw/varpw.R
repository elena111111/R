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
    weights[tmp] <- CoreRect(distances_s[tmp]/distances_s[k+1])
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

k <- 6
K <- CoreRect
step <- 0.3

st3 <- seq(from = min(iris[, 3]), to = max(iris[, 3]), by = step)
st4 <- seq(from = min(iris[, 4]), to = max(iris[, 4]), by = step)

for(i in st3){
  for(j in st4){
    z <- c(i, j)
    points(z[1], z[2],  pch = 21, bg = "lightgoldenrodyellow", col = colors[varpw(iris, z, K, k)])
  }
}

legend("bottomright", c("virginica", "versicolor", "setosa"), pch = c(15,15,15), 
       col = c("blue", "green3", "red"))
