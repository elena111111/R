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

pw <- function(X, z, h, K){
  xl <- X[, 3:5]
  l <- dim(xl)[1]
  n <- dim(xl)[2] - 1
  
  distances_weighed <- matrix(NA, l, 2)
  for(tmp in 1:l){
    distances_weighed[tmp, 1] <- eDist(xl[tmp, 1:n], z)
    distances_weighed[tmp, 2] <- K(distances_weighed[tmp, 1]/h)
  }
  classes <- data.frame(distances_weighed[ , 1], distances_weighed[ , 2], xl[ , 3])
  colnames(classes) <- c("Distances", "Weights", "Species")
  sumSetosa <- sum(classes[classes$Species == "setosa", 2])
  sumVersicolor <- sum(classes[classes$Species == "versicolor", 2])
  sumVirginica <- sum(classes[classes$Species == "virginica", 2])
  ans <- matrix(c(sumSetosa, sumVersicolor, sumVirginica), 
                nrow = 1, ncol = 3, byrow = T, list(c(1), c('setosa', 'versicolor', 'virginica'))) #храним название класса и соответствующий ему суммарный вес
  return(colnames(ans)[which.max(ans)])
}

draw_for_loo <- function(Ox, Oy, step, xmax, hOpt, LooOpt, i){
  plot(NULL, NULL, type = "l",  main = i, xlim = c(0, xmax), ylim = c(0, 1), xlab = 'h', ylab = 'loo')
  lines(Ox, Oy, pch = 8, bg = "black", col = "green3")
  points(hOpt, LooOpt, pch = 21, bg = "white", col = "blue")
}

loo <- function(X, alg, step, xmax){
  l <- dim(X)[1]
  Ox <- seq(from = step, to = xmax, by = step)
  Cores <- c(CoreEpan, CoreGauss, CoreQuart, CoreTriang, CoreRect)
  CoresNames <- c("CoreEpan", "CoreGauss", "CoreQuart", "CoreTriang", "CoreRect")
  LooOpt <- 1
  Kopt <- Cores[1]
  ans_Kh <- c() #c(K, h)
  hOpt <- step
  j <- 1
  
  for(K in Cores){
    Oy <- c()
    print(K)
    for(h in Ox){
      print(h)
      Q <- 0
      for(i in 1:l){
        iris2 <- iris[-i, ]
        z <- iris[i, 3:4]
        if(pw(iris2, z, h, K) != iris[i, 5]) Q <- Q + 1
      }
      Loo <- Q/l
      print(Loo)
      if(Loo <= LooOpt) {
        LooOpt <- Loo
        hOpt <- h
        KOpt <- K
      }
      Oy <- c(Oy, Loo)
    }
    draw_for_loo(Ox, Oy, step, xmax, hOpt, LooOpt, CoresNames[j])
    j <- j + 1
    print(KOpt)
    print(hOpt)
    print(LooOpt)
    ans_Kh <- c(KOpt, hOpt)
  }
  print(LooOpt)
  print(ans_Kh)
  return(ans_Kh)
}

step <- 0.2
#Kh <- loo(iris, pw, step, 5)
Kh <- c(CoreTriang, 2.6)

colors <- c("setosa" = "red", "versicolor" = "green3", "virginica" = "blue")
plot(iris[, 3:4], pch = 21, bg = colors[iris$Species], col = colors[iris$Species])

st3 <- seq(from = min(iris[, 3]), to = max(iris[, 3]), by = step)
st4 <- seq(from = min(iris[, 4]), to = max(iris[, 4]), by = step)

for(i in st3){
  for(j in st4){
    z <- c(i, j)
    points(z[1], z[2],  pch = 21, bg = "lightgoldenrodyellow", col = colors[pw(iris, z, Kh[[2]], Kh[[1]])])
  }
}

legend("bottomright", c("virginica", "versicolor", "setosa"), pch = c(15,15,15), col = c("blue", "green3", "red"))

