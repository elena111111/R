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

naiveBayes <- function(z, X, lambda, Ker){ 
  n <- 3:4
  #n <- length(z) 
  l <- nrow(X)
  m <- length(levels(X$Species)) 
  P_apr <- c() 
  P_apr[1:m] <- 0
  
  p <- c()
  p[1:m] <- 0
  ans <- data.frame(1:m, levels(X$Species))
  colnames(ans) <- c("rule", "Species")
  for(i in 1:m){ 
    subclass <- X[X$Species == levels(X$Species)[i], ] 
    P_apr[i] <- nrow(subclass)/l
    #print(P_apr[i])
    for(s in 1:nrow(subclass)){
      for(j in n){
        if(Ker((z[j] - subclass[i, j])/h[i]) > 0) 
          p[i] <- p[i] + logb(Ker((z[j] - subclass[i, j])/h[i])/h[i])
      }
    }
    if(p[i] != 0) {
      p[i] <- p[i] - logb(nrow(subclass))
      ans[i, 1] <- (logb(lambda[i]*P_apr[i]) + p[i])
    } else ans[i, 1] <- logb(lambda[i]*P_apr[i])
  }
  print(z)
  print(P_apr)
  print(p)
  print(ans)
  print(max(ans[ , 1]))
  print(ans[which.max(ans[ , 1]), 2])
  return(ans[which.max(ans[ , 1]), 2])
}

X <- iris
step <- 0.1
Ker <- KerGauss
h <- c()
lambda <- c(1, 1, 1) #т к 3 класса
i <- 1:nrow(X)
h[i] <- 0.2

X <- data.frame(X, h, lambda)

colors <- c("setosa" = "red", "versicolor" = "green3", "virginica" = "blue")
plot(iris[, 3:4], pch = 21, bg = colors[iris$Species], col = colors[iris$Species])

st3 <- seq(from = min(iris[, 3]), to = max(iris[, 3]), by = step)
st4 <- seq(from = min(iris[, 4]), to = max(iris[, 4]), by = step)

for(i in st3){
  for(j in st4){
    z <- c(0, 0, i, j)
    points(z[3], z[4],  pch = 21, bg = "lightgoldenrodyellow", col = colors[naiveBayes(z, X, lambda, Ker)])
  }
}
points(iris[, 3:4], pch = 21, bg = colors[iris$Species], col = colors[iris$Species])

legend("bottomright", c("virginica", "versicolor", "setosa"), pch = c(15,15,15), col = c("blue", "green3", "red"))



