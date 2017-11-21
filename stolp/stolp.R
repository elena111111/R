eDist <- function(u, v) {
  sqrt(sum((u - v)^2))
}

knn <- function(z, X, k){
  if(k > nrow(X)) k <- nrow(X) - 1  #чтобы алгоритм не вырождался в const после stolp
  #print(k)
  xl <- X[ , 3:5]
  l <- dim(xl)[1] 
  n <- dim(xl)[2] - 1
  
  distances <- c()
  for(tmp in 1:l){
    distances[tmp] <- eDist(xl[tmp, 1:n], z)
  }
  orderedxl <- xl[order(distances), ]
  classes <- orderedxl[1:k, n + 1]  #получим названия первых k классов в отсортированном списке
  counts <- table(classes) #посчитали, сколько раз встречается каждый класс в classes
  return(names(which.max(counts)))
}

margin <- function(z, class_z, X, k)
  {  
  xl <- X[ , 3:5]
  l <- dim(xl)[1] 
  n <- dim(xl)[2] - 1
  
  distances <- c()
  for(tmp in 1:l){
    distances[tmp] <- eDist(xl[tmp, 1:n], z)
  }
  orderedxl <- xl[order(distances), ]
  classes <- orderedxl[1:k, n + 1]  #получим названия первых k классов в отсортированном списке
  counts <- table(classes) #посчитали, сколько раз встречается каждый класс в classes
  
  return(counts[class_z] - max(counts[names(counts) != class_z]))
}

eq <- function(c1, c2){ #сравнение векторов
  if(length(c1) != length(c2)) return(FALSE)
  for(i in 1:length(c1)){
    if(c1[i] != c2[i]) return(FALSE)
  }
  return(TRUE)
}

stolp <- function(X, k, M, l0 = 50, delta = 2){
  #удалим строки, где отступ отрицательный
  G <- data.frame()
  X <- cbind(X, M)
  i <- 1
  while(i <= dim(X)[1]) {
    if(X[i, 6] < delta) {
      print(i)
      X <- X[-i, ]
      rownames(X) <- 1:nrow(X) #перенумеровали
      i <- i - 1
    }
    i <- i + 1
  }
  
  #добавим в G по одному элементу с максимальным отступом из каждого класса
  for(i in 1:length(levels(X$Species))){
    tmp <- X[X$Species == levels(X$Species)[i], ]
    #print(which.max(tmp[, 6]))
    G <- rbind(G, tmp[which.max(tmp[, 6]), ])
  }
  
  while(nrow(G) != nrow(X)){
    E <- data.frame()
    X2 <- X
    i <- 1
    while(i <= dim(X2)[1]) {
      for(j in 1:nrow(G)){
        if(eq(X2[i, ], G[j, ])) {
          print(i)
          X2 <- X2[-i, ]
          rownames(X2) <- 1:nrow(X2)
          i <- i - 1
          break
        }
      }
      i <- i + 1
    }
    #теперь в X2 содержатся точки {X}\{G}
    m_min <- min(G[ , 6])
    z_min <- G[which.min(G[ , 6]), ]
    for(i in 1:nrow(X2)) {
      m <- margin(X2[i, 3:4], X2[i, 5], G, k)
      if(m < delta)  {
        E <- rbind(E, X2[i, ])
          if(m < m_min){
            m_min <- m
            z_min <- X2[i, ]
          }
      }
    }
    if(nrow(E) < l0) break
    G <- rbind(G, z_min)
  }
  print(G)
  G <- G[ , 1:(ncol(G) - 1)] #столбец с отступами удалим
  return(G)
}

X <- iris
k <- 6
M <- c()

for(j in 1:nrow(X)){
  M[j] <- margin(X[j, 3:4], X[j, 5], X[-j, ], k)
}

print(M)

#stolp(X, k, M)
colors <- c("setosa" = "red", "versicolor" = "green3", "virginica" = "blue")
draw_iris <- function(){
  plot(iris[, 3:4], main = "Опорные объекты", pch = 21, bg = "white", col = colors[iris$Species])
  S <- stolp(X, k, M)
  points(S[, 3:4], pch = 21, bg = colors[S$Species], col = colors[S$Species])
  legend("topleft", c("virginica", "versicolor", "setosa"), pch = c(15, 15, 15), col = c("blue", "green3", "red"))
}

draw_knn <- function(X, k, M, name = "knn"){
  plot(iris[, 3:4], main = name, pch = 21, bg = colors[iris$Species], col = colors[iris$Species])
  
  step <- 0.2
  st3 <- seq(from = min(iris[, 3]), to = max(iris[, 3]), by = step)
  st4 <- seq(from = min(iris[, 4]), to = max(iris[, 4]), by = step)
  
  for(i in st3){
    for(j in st4){
      z <- c(i, j)
      points(z[1], z[2],  pch = 21, bg = "lightgoldenrodyellow", col = colors[knn(z, X, k)])
    }
  }
  
  legend("bottomright", c("virginica", "versicolor", "setosa"), pch = c(15,15,15), col = c("blue", "green3", "red"))
}

draw_iris()
#draw_knn(iris, k, M, "knn со всей выборкой iris")
#желательно заново пересчитать k оптимальное
draw_knn(stolp(X, k, M), 1, M, "knn после работы stolp") #здесь k оптимальное стало равно 1(определили по картинке опытным путем)
S <- stolp(X, k, M)
points(S[, 3:4], pch = 21, bg = colors[S$Species], col = "black")
