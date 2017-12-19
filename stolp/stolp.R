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

k_loo <- function(X, alg, step, x_max){
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
      print(alg(z, X2, k))
      print(X[i, 5])
      if(alg(z, X2, k) != X[i, 5]) Q <- Q + 1
    }
    print("Q:")
    print(Q)
    Loo <- Q/l
    Oy <- c(Oy, Loo)
    #print(k)
    #print(Loo)
    if(Loo < LooOpt) {
      LooOpt <- Loo
      kOpt <- k
    }
  }
  print("Loo S:")
  print(LooOpt)
  lines(Ox, Oy, pch = 8, bg = "black", col = "green3")
  points(kOpt, LooOpt, pch = 8, bg = "black", col = "blue")
  return(kOpt)
}

loo <- function(X, S, k){
  LOO <- 0
  l <- nrow(X)
  for(i in 1:l){
    X2 <- X[-i, ]
    z <- X[i, 3:4]
    rownames(X2) <- 1:(l-1)
    
    S2 <- S
    
    j <- 1
    while(j <= nrow(S2)){
      if(eq(z, S2[j , 3:4])){ 
          S2 <- S2[-j, ]
          rownames(S2) <- 1:nrow(S2)
          j <- j - 1
      }
      j <- j + 1
    }
    if(knn(z, S2, k) != X[i, 5]) LOO <- LOO + 1
  }
  print(LOO)
  print(l)
  LOO <- LOO / l
  print("LOO:")
  print(LOO)
  return(LOO)
}


margin <- function(z, class_z, X, k){  
  xl <- X[ , 3:5]
  l <- nrow(xl) 
  n <- ncol(xl) - 1
  
  distances <- c()
  for(tmp in 1:l){
    distances[tmp] <- eDist(xl[tmp, 1:n], z)
  }
  orderedxl <- xl[order(distances), ]
  classes <- orderedxl[1:k, n + 1]  
  counts <- table(classes) 
  
  return(counts[class_z] - max(counts[names(counts) != class_z]))
}

eq <- function(c1, c2){ 
  if(length(c1) != length(c2)) return(FALSE)
  for(i in 1:length(c1)){
    #print(c1[i])
    #print(c2[i])
    if(c1[i] != c2[i]) return(FALSE)
  }
  return(TRUE)
}

stolp <- function(X, k, M, l0 = 50, delta = 3){ # X - обучающая выборка, k - количество соседей, M - вектор отступов, l0 - допустимая доля ошибок, delta - порог фильтрации выбросов
  G <- data.frame() # - здесь будут храниться опорные объекты
  X <- cbind(X, M)
  i <- 1
  while(i <= nrow(X)) {
    if(X[i, 6] < delta) { # если объект является выбросом, удалим его из выборки
      print(i)
      X <- X[-i, ]
      rownames(X) <- 1:nrow(X) # перенумерация строк 
      i <- i - 1
    }
    i <- i + 1
  }
  
  for(i in 1:length(levels(X$Species))){          # для каждого класса
    tmp <- X[X$Species == levels(X$Species)[i], ] # - подмассив (объекты одного класса)
    #print(which.max(tmp[, 6]))
    G <- rbind(G, tmp[which.max(tmp[, 6]), ])     #  добавим по одному эталонному объекту в G
  }
  
  while(nrow(G) != nrow(X)){
    E <- data.frame()
    X2 <- X
    i <- 1
    while(i <= nrow(X2)){     # получим мн-во X\G в X2, т е мн-во неопорных объектов
      for(j in 1:nrow(G)){
        if(eq(X2[i, ], G[j, ])){
          print(i)
          X2 <- X2[-i, ]
          rownames(X2) <- 1:nrow(X2)
          i <- i - 1
          break
        }
      }
      i <- i + 1
    }
    
    m_min <- min(G[ , 6])   # значение минимального отступа в G
    z_min <- G[which.min(G[ , 6]), ]  # объект с наименьшим отступом из G
    for(i in 1:nrow(X2)) { 
      m <- margin(X2[i, 3:4], X2[i, 5], G, k) # вычислить отступ для всех неопорных объектов на мн-ве опорных объектов
      if(m < delta)  {      # добавим в E все неопорные объекты с отступом ниже порогового
        E <- rbind(E, X2[i, ])
          if(m < m_min){    # сразу ищем неопорный объект с минимальным отступом
            m_min <- m      
            z_min <- X2[i, ]
          }
      }
    }
    if(nrow(E) < l0) break   # если количество неопорных объектов с отступом ниже порогового стало преемлемо для нас, алгоритм завершен
    G <- rbind(G, z_min)     # иначе присоединить к G(мн-ву опорных объектов) объект с наименьшим отступом
  }
  print(G)
  G <- G[ , 1:(ncol(G) - 1)] # уберем столбец, который временно присоединили
  return(G)
}

X <- iris
k <- 6
M <- c()

for(j in 1:nrow(X)){
  M[j] <- margin(X[j, 3:4], X[j, 5], X[-j, ], k)
}

print(M)

colors <- c("setosa" = "red", "versicolor" = "green3", "virginica" = "blue")
draw_iris <- function(){
  plot(iris[, 3:4], main = "Опорные объекты", pch = 21, bg = "white", col = colors[iris$Species])
  S <- stolp(X, k, M)
  points(S[, 3:4], pch = 21, bg = colors[S$Species], col = colors[S$Species])
  legend("topleft", c("virginica", "versicolor", "setosa"), pch = c(15, 15, 15), col = c("blue", "green3", "red"))
}

draw_knn <- function(X, k, M, name = "knn"){
  plot(iris[, 3:4], main = name, pch = 21, bg = colors[iris$Species], col = colors[iris$Species])
  
  step <- 0.1
  st3 <- seq(from = min(iris[, 3]), to = max(iris[, 3]), by = step)
  st4 <- seq(from = min(iris[, 4]), to = max(iris[, 4]), by = step)
  
  for(i in st3){
    for(j in st4){
      z <- c(i, j)
      points(z[1], z[2],  pch = 21, bg = "lightgoldenrodyellow", col = colors[knn(z, X, k)])
    }
  }
  points(iris[, 3:4], main = name, pch = 21, bg = colors[iris$Species], col = colors[iris$Species])
  legend("bottomright", c("virginica", "versicolor", "setosa"), pch = c(15,15,15), col = c("blue", "green3", "red"))
}

S <- stolp(X, k, M)
draw_iris()
#draw_knn(iris, k, M, "knn со всей выборкой iris")
k_opt <- k_loo(S, knn, 0.1, nrow(S))
print("k_opt")
print(k_opt)
loo(X, S, k)
draw_knn(S, k_opt, M, "knn после работы stolp") 
points(S[, 3:4], pch = 21, bg = colors[S$Species], col = "black")

