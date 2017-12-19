# ����������� ��������� �������������

��� ����� ���������� ������ ����������� �������� ������������: ������ �������� ������������� ������ ������.
�������� ���� �������� �������� (� ����� ������ ��������� ����������), ������� ����������, ��������� ��� ������� ������. 

���������� ����������� �������������:

<a href="https://www.codecogs.com/eqnedit.php?latex=a(z,&space;X^l)&space;=&space;arg&space;\max_{y&space;\in&space;Y}&space;W_y(z,&space;X^l),&space;W_y(z,&space;X^l)&space;=&space;\sum_{i:&space;y_z^{(i)}&space;=&space;y}w(i,&space;z)," target="_blank"><img src="https://latex.codecogs.com/gif.latex?a(z,&space;X^l)&space;=&space;arg&space;\max_{y&space;\in&space;Y}&space;W_y(z,&space;X^l),&space;W_y(z,&space;X^l)&space;=&space;\sum_{i:&space;y_z^{(i)}&space;=&space;y}w(i,&space;z)," title="a(z, X^l) = arg \max_{y \in Y} W_y(z, X^l), W_y(z, X^l) = \sum_{i: y_z^{(i)} = y}w(i, z)," /></a>

��� *W*- ������ �������� ������� *z* � ����� *y*, *w(i, z)* - ������� �������, ����������� ������� ������� *i*-�� ������ ��� ������� *z*.

## knn - ����� k ��������� �������.
���� ������� ������ ������(150 ���������), � ��� 3 ������(*setosa*, *versicolor*, *virginica*). 
�� ����� ���������������� ��������� ����� *{z}* �� 2� ��������� (Petal.Length, Petal.Width). ��� �� �������� - ���������� ����� �� �������.

����� ������� ����� *z* � ���� ������, ������� ���� ����� ����������� ����� � �������. 

����������:

```R
knn <- function(z, X, k){	# X - ��� ����� ������, k - ����� ������� ����� z
  xl <- X[ , 3:5]		# � xl ���������� ����� Petal.Length, Petal.Width � �������� ������ , � �������� ��������� ��������������� �����
  l <- dim(xl)[1] 
  n <- dim(xl)[2] - 1
  
  distances <- c()
  for(tmp in 1:l){
    distances[tmp] <- eDist(xl[tmp, 1:n], z)	# ���������� �� ����� z �� ������ ����� �� �������
  }
  orderedxl <- xl[order(distances), ]		# ��������� ����� � ������� ���������� ���������� �� z �� ����� �� �������
  classes <- orderedxl[1:k, n + 1]  		# ������� ������ ������ k ����� (������� k ��������� �������), � � ��� ������� ������ ������� �������� �������
  counts <- table(classes) 			# ���������, ������� ��� ���������� ������ �����
  return(names(which.max(counts)))		# ����� z ��������� � ���� ������, ������� ���� ����� ����������� � classes
}
```

����������� ����� ������� *k* ����������� �� *LOO* (���������� ��������), ������� �������� ��������� �������: 

```R
loo <- function(X, alg, step, x_max){
  Ox <- seq(from = 1, to = x_max, by = step)
  Oy <- c()
  l <- dim(X)[1]	
  LooOpt <- 1
  kOpt <- 1
  
  for(k in Ox){		#��� ������� ����� �������
    Q <- 0		# ���������� ������
    for(i in 1:l){
      X2 <- X[-i, ]	# �� ������� X ����� ��������� �� ����� ����� (z)
      z <- X[i, 3:4]	# �������� ��������(knn), ��� ����� �� ����� ���������������� ����� z, � ����� ������� X2.
      if(alg(z, X2, k) != X[i, 5]) Q <- Q + 1	# ���� �������� ������, �������� ���������� ������ Q
    }
    Loo <- Q/l 		# ����� ������ ����� �� ���������� ����� � �������
    Oy <- c(Oy, Loo)
    if(Loo < LooOpt) {
      LooOpt <- Loo
      kOpt <- k		# ����������� ����� k, ��� ������� �������� Loo ����������.
    }
  }
  return(kOpt)
}
```

��������� ������ knn: 

![alt text](https://github.com/elena111111/R/blob/master/knn/knn.png)

����������� *Loo* �� *k*:

![alt text](https://github.com/elena111111/R/blob/master/knn/knn_loo.png)


## �������� k ���������� ��������� ������� (wknn).
������� ������� *X* (����� ������), � 3 ������ (*setosa*, *versicolor*, *virginica*). 
�� ����� ���������������� ��������� ����� *{z}*.
������������� �������� �� ���� ��������� (Petal.Length, Petal.Width), ��� �� �������� ������������ �����.

������� �� �������� knn � ���, ��� ������� ������������� �������� ������� ������� (q^i), ��������� �� ����� ������.
����� ������� ����� *z* � ���� ������, ��������� ��� �������� ���������� ����� � �������. 

����������:

```R
wknn <- function(z, X, k, q){ 		# z - ���������������� �����, X - ��������� �������, k - ����� �������, q - ����������� �������������� ���������� (����� ������� �������)
  xl <- X[ , 3:5]			# � xl ���������� ����� Petal.Length, Petal.Width � �������� ������ , � �������� ��������� ��������������� �����
  l <- dim(xl)[1] 
  n <- dim(xl)[2] - 1
  
  distances <- c()
  for(tmp in 1:l){
    distances[tmp] <- eDist(xl[tmp, 1:n], z)		# ���������� �� ����� z �� ������ ����� �� ������� xl
  }
  orderedxl <- xl[order(distances), ]			# xl, ��������������� �� ����������� ���������� �� ����� z �� ������ ����� �� �������
  Weights <- q^(1:k) 					# ���� ��� k ��������� �������
  classes <- cbind(orderedxl[1:k, ], Weights)[ , (n + 1):(n + 2)]  	#  � classes ����� k �����, � � �������� �������� ������ � ��� �����
  
  sumSetosa <- sum(classes[classes$Species == "setosa", 2])
  sumVersicolor <- sum(classes[classes$Species == "versicolor", 2])
  sumVirginica <- sum(classes[classes$Species == "virginica", 2])
  
  ans <- matrix(c(sumSetosa, sumVersicolor, sumVirginica), 
                nrow = 1, ncol = 3, byrow = T, list(c(1), c('setosa', 'versicolor', 'virginica'))) 	#����� ����� ��� ������� ������
  return(colnames(ans)[which.max(ans)])				# ������� ����� ��� �����, ��� �������� ���������� � ans
}
```

�������� *q* (� ��������� *(0; 1)*) ����������� �� *LOO* (���������� � �������� k � knn).
����������� ����� *q*, ��� ������� �������� *Loo* ����������.

��������� ������ ���������: 

![alt text](https://github.com/elena111111/R/blob/master/wknn/wknn.png)

����������� *Loo* �� *q*:

![alt text](https://github.com/elena111111/R/blob/master/wknn/wknn_loo_q.png)

## �������� ������������� ���� (pw).
������� ������� *X* (����� ������), � 3 ������ (*setosa*, *versicolor*, *virginica*). 
�� ����� ���������������� ��������� ����� *{z}*.
������������� �������� �� ���� ��������� (Petal.Length, Petal.Width), ��� �� �������� ������������ �����.

������� �� ������ wknn � ���, ��� ������� ������� ������ ������� �� �� ����� ������, � �� ���������� (�� ����� *z* �� ����� �� �������), 
� ����� �� ������ ��������� *h* (������ ����) � ���� K, ������� ����������� �� LOO (������� ������ ��� ��� ������ � ��������� ����������).
�.�. �� ������ ������ ����� *z*(����� ����) ����������� ������� *h*, � �������, ��������� ��� ������ ������ � ���� ����������� ������.

����������:
```R
pw <- function(X, z, h, K){	# X - ��������� �������, z - ���������������� �����, h - ������ ����, K - ������� ����
  xl <- X[, 3:5]		# � xl ���������� ����� Petal.Length, Petal.Width � �������� ������ , � �������� ��������� ��������������� �����
  l <- dim(xl)[1]
  n <- dim(xl)[2] - 1
  
  distances_weighed <- matrix(NA, l, 2)
  for(tmp in 1:l){
    distances_weighed[tmp, 1] <- eDist(xl[tmp, 1:n], z)		# distances_weighed[ , 1] - ��� ���������� �� ����� z �� ������ ����� �� ������� xl
    distances_weighed[tmp, 2] <- K(distances_weighed[tmp, 1]/h)	# distances_weighed[ , 2] - �������� ������� ������� ��� ������ ����� �� �������
  }
  classes <- data.frame(distances_weighed[ , 1], distances_weighed[ , 2], xl[ , 3]) #���������� �� z �� ����� �� xl, ���� ����� �� xl � �������� ������ ������ �����
  colnames(classes) <- c("Distances", "Weights", "Species")
  sumSetosa <- sum(classes[classes$Species == "setosa", 2])
  sumVersicolor <- sum(classes[classes$Species == "versicolor", 2])
  sumVirginica <- sum(classes[classes$Species == "virginica", 2])
  ans <- matrix(c(sumSetosa, sumVersicolor, sumVirginica), 
                nrow = 1, ncol = 3, byrow = T, list(c(1), c('setosa', 'versicolor', 'virginica'))) 	# ans - c���� ����� ������� ������
  return(colnames(ans)[which.max(ans)])					# ������� ����� �����, ��������� ��� �������� ���������� � ans
}
```
��� ������� ���� ���������� ���������� �������� ������������ *Loo*. ��� ��� �� ���������, ��� ����� ���� ����� ������ �� �������������.

����� ��������, ��� ������ �������� ����� �������� ��� ������������ ������������� ������� (� ���� ���������� ������ �������� ����� ������ ���������� ��������). 
� ���� ������ ����� ������������ �������� ������������� ���� � ���������� ������� ���� (������ ����).

��������� ������ ��������� / ����������� *loo* �� *h* (��� ������ ����):

1) ������������� ����:

![alt text](https://github.com/elena111111/R/blob/master/pw/pw_ker_rect_and_loo.png)

2) ����������� ����:

![alt text](https://github.com/elena111111/R/blob/master/pw/pw_ker_triang_and_loo.png)

3) ���� ������������:

![alt text](https://github.com/elena111111/R/blob/master/pw/pw_ker_epan_and_loo.png)

4) ������������ ����:

![alt text](https://github.com/elena111111/R/blob/master/pw/pw_ker_quart_and_loo.png)

5) ����������� ����: 

![alt text](https://github.com/elena111111/R/blob/master/pw/pw_ker_gauss_and_loo.png)

## �������� ������������� ���� � ���������� ������� ���� (varpw).
������� ������� *X* (����� ������), � 3 ������ (*setosa*, *versicolor*, *virginica*). 
�� ����� ���������������� ��������� ����� *{z}*.
������������� �������� �� ���� ��������� (Petal.Length, Petal.Width), ��� �� �������� ������������ �����.

������ ���� *h* ������ - ��� ���������� �� �����  *z* �� *k+1*-�� ������ ���� �����.
��� ������� ������� ��� ����� ������� ����(� ������� ������ ��������� ������������ �������������).

����������:
```R
varpw <- function(X, z, K, k){	# X - ��������� �������, z - ���������������� �����, K - ������� ����, k - ����� �������
  xl <- X[, 3:5]		# � xl ���������� ����� Petal.Length, Petal.Width � �������� ������ , � �������� ��������� ��������������� �����
  l <- dim(xl)[1]
  n <- dim(xl)[2] - 1
  
  distances <- c()
  for(tmp in 1:l){
    distances[tmp] <- eDist(xl[tmp, 1:n], z)	# ������ ���������� �� ����� z �� ������ ����� �� �������
  }
  orderedxl <- xl[order(distances), ]		# orderedxl - ��� ������ xl, ��������������� �� ����������� ���������� �� ����� z �� ������ ����� �� �������
  distances_s <- sort(distances)		# distances_s - ��������������� ������ ���������� 
  weights <- c()				# weights - ������ ����� ��� k ��������� �������.
  for(tmp in 1:k){
  if(distances_s[k+1] != 0)
      weights[tmp] <- K(distances_s[tmp]/distances_s[k+1])	# �������� ���� - ���������� �� z �� i-�� ������ ����� z, ������� �� ���������� �� z �� k+1-�� ������.
      else weights[tmp] <- K(0)	
  }
  classes <- cbind(orderedxl[1:k, ], weights)[ , (n + 1):(n + 2)]		# d������ �� orderedxl ������ k �����, ������� � ��� ��������������� ���� � ������� ������ ������� �������� ������� � �����
  sumSetosa <- sum(classes[classes$Species == "setosa", 2])
  sumVersicolor <- sum(classes[classes$Species == "versicolor", 2])
  sumVirginica <- sum(classes[classes$Species == "virginica", 2])
  ans <- matrix(c(sumSetosa, sumVersicolor, sumVirginica), 
                nrow = 1, ncol = 3, byrow = T, list(c(1), c('setosa', 'versicolor', 'virginica')))
  return(colnames(ans)[which.max(ans)])				# ������� ����� ��� �����, ��������� ��� �������� ����������
}
``` 

������ ����� �������� ��� ������������ �������������� �������.
�� ������� �������������� ���� ����� ����� ����� ������� � ����������� ������� � ����������� ���������.

������ ������ ���������:

![alt text](https://github.com/elena111111/R/blob/master/varpw/varpw_core_rect.png)

## ����� ������������� ������� 
������� ������� *X* (����� ������), � 3 ������ (*setosa*, *versicolor*, *virginica*). 
�� ����� ���������������� ��������� ����� *{z}*.
������������� �������� �� ���� ��������� (Petal.Length, Petal.Width), ��� �� �������� ������������ �����.

� ������ ������������� ���� �� �������� ����� ���� � ���������������� ������, � ������ �������� ����������� ������ ��������� ��������. 
������ ������ ����������� ����� ����� ���� ��������� - ��������, ������������, ��������� ������ ��� ���� ������ �� ������ ��������. �� ���� ���� ��� ���������� ������ ���� *h* ��� ������� ����� ���� ������.

������ �� �������� ����� *gamma*, � ���� �������� ���������� ���������: 
`g[dim(iris)[1]]` - ��� ������ �����������, ������� ����������� ��������� �������: `g <- gamma(X, K, h, eps)`:
```R
gamma <- function(X, K, h, eps = 0.15){ 	# X - ��������� �������, K - ������� ����, h - ������ ����, eps - ����������� ���������� ���� ������ (�� 0 �� 1)
  xl <- X[, 3:5]                  
  l <- dim(xl)[1]
  g <- c()
  i <- 1:l
  g[i] <- 0		 
 #���� ����� ������ ������ �������� ��������, ���� �������� ������ g:
  while(loo(X, g, K, h) > eps){						# loo ���������� ���������� ������ ���������(pf) � ������� �����������, ������� �� ����������� ������� (����� �� 0 �� 1)
    rand <- sample(i, 1, replace = T)					# ��������� ������� ������� ����� �������� �� ������� (xi)
    if(pf(X, c(xl[rand, 1], xl[rand, 2]), g, K, h) != xl[rand, 3]) g[rand] <- g[rand] + 1	# ���� �������� pf(X, xi, g, K, h) ���������, ������ � ����� xi ����� ��������� ��������� �� 1
  }
  return(g)
}
```

������ ���� *h* ����� ��������� ������������� ��� ������� �������, �� � ��������� �� �����������, ��� ������ (����������). 
� ������ ������ ��� �������� ����� �� ������ ������������� ����, � ��������� ��� ���� ����.

����� *pf* �������� ���������� � ������� ������������� ����, ������ �������� ������� ���� ��� ����������� �� *g*:

����������:
```R
pf <- function(X, z, g, K, h){	# X - ��������� �������, z - ���������������� �����, g - ������ �����������, K - ������� ����, h - ������ ����
  xl <- X[, 3:5]		# xl - ���������� ����� Petal.Length, Petal.Width � �������� ������ , � �������� ��������� ��������������� �����
  l <- dim(xl)[1]
  n <- dim(xl)[2] - 1
  
  distances_weighed <- matrix(NA, l, 2)
  for(tmp in 1:l){
    distances_weighed[tmp, 1] <- eDist(xl[tmp, 1:n], z)			# distances_weighed[ , 1] - ��� ���������� �� ����� z �� ������ ����� �� �������
    distances_weighed[tmp, 2] <- g[tmp]*K(distances_weighed[tmp, 1]/h)		# distances_weighed[ , 2] - �������� ������� ������� ��� ������ ����� �� �������
  }
  
  classes <- data.frame(distances_weighed[ , 1], distances_weighed[ , 2], xl[ , 3])	# classes - ��� distances_weighed, � �������� �������� ������� ��������������� �������� �������. 
  colnames(classes) <- c("Distances", "Weights", "Species")
  sumSetosa <- sum(classes[classes$Species == "setosa", 2])
  sumVersicolor <- sum(classes[classes$Species == "versicolor", 2])
  sumVirginica <- sum(classes[classes$Species == "virginica", 2])
  ans <- matrix(c(sumSetosa, sumVersicolor, sumVirginica), 
                nrow = 1, ncol = 3, byrow = T, list(c(1), c('setosa', 'versicolor', 'virginica'))) 
  return(colnames(ans)[which.max(ans)])					# ������� ��� z ����� ��� �����, �������� ��� �������� ���������� � classes
}
```

������ ������ ���������(��� ������������ ����):

![alt text](https://github.com/elena111111/R/blob/master/pf/pf_05333.png)

������ �������� ������ ���� � ��������� ����������� (�� ��������� � ��� ����� 1).

## �������� STOLP
 
��������� ������ �������� ������.
��������� �������� �� ������� ��������� ������� ��������, � ��������� �� ���� ���������.

������� ������ ��������� �� ������� knn.

�������� ( *margin* ) ������� *x_i* ������������ ������������ ��������� ������������� ����������: 

<a href="https://www.codecogs.com/eqnedit.php?latex=M(x_i)&space;=&space;W_y_i(x_i,&space;X^l)&space;-&space;\max_{y&space;\in&space;Y&space;\setminus&space;\{y_i\}&space;}&space;W_y(x_i,&space;X^l)" target="_blank"><img src="https://latex.codecogs.com/gif.latex?M(x_i)&space;=&space;W_y_i(x_i,&space;X^l)&space;-&space;\max_{y&space;\in&space;Y&space;\setminus&space;\{y_i\}&space;}&space;W_y(x_i,&space;X^l)" title="M(x_i) = W_y_i(x_i, X^l) - \max_{y \in Y \setminus \{y_i\} } W_y(x_i, X^l)" /></a>

� ����� ������ ��� �������� ����� ����������� ��������(����� *k* �������) ������ ������ ��� *x_i* � ������������ ����������� �������� ������ ������.

���������� ������ margin:
```R
margin <- function(z, class_z, X, k){  # z - �����, ��� ������� ������� ������, class_z - �����, �������� ����������� z, X - ������� (����� ������),  k - ����� ������� ��� knn
  xl <- X[ , 3:5]
  l <- nrow(xl) 
  n <-  ncol(xl) - 1
  
  distances <- c()
  for(tmp in 1:l){
    distances[tmp] <- eDist(xl[tmp, 1:n], z)	# ���������� �� *z*
  }
  orderedxl <- xl[order(distances), ]	# ��������� ����� � ������� ���������� ����������
  classes <- orderedxl[1:k, n + 1]		# �������� ������ ������ k ����� � ������� �������� �������  
  counts <- table(classes) 		# �������, ������� ��� ���������� ������ �����
  
  return(counts[class_z] - max(counts[names(counts) != class_z]))	#�������� ����� ����������� ����� ������ class_z � ������������ ����������� ����� ������� ������
}
```

������� � ������� ������������� �������� ���������� ���������. �� ����� ������� �� �������.
������� � ������� ������������� �������� ���������� ����������. �� ��� ����� ��������������� ��� �������������.

```R
stolp <- function(X, k, M, l0 = 50, delta = 2){  # X - ��������� �������, k - ���������� �������, M - ������ ��������, l0 - ���������� ���� ������, delta - ����� ���������� ��������
  G <- data.frame()		# - ����� ����� ��������� ������� �������
  X <- cbind(X, M)
  i <- 1
  while(i <= nrow(X)) {
    if(X[i, 6] < delta) {	# ���� ������ �������� ��������, ������ ��� �� �������
      X <- X[-i, ]
      rownames(X) <- 1:nrow(X)	# ������������� ����� 
      i <- i - 1
    }
    i <- i + 1
  }
  
  for(i in 1:length(levels(X$Species))){		# ��� ������� ������
    tmp <- X[X$Species == levels(X$Species)[i], ] 	# - ��������� (������� ������ ������)
    G <- rbind(G, tmp[which.max(tmp[, 6]), ])	#  ������� �� ������ ���������� ������� � G
  }
  
  while(nrow(G) != nrow(X)){
    E <- data.frame()
    X2 <- X
    i <- 1
    while(i <= nrow(X2)) {	# ������� ��������� X\G � X2
      for(j in 1:nrow(G)){
        if(eq(X2[i, ], G[j, ])) {
          X2 <- X2[-i, ]
          rownames(X2) <- 1:nrow(X2)
          i <- i - 1
          break
        }
      }
      i <- i + 1
    }
    
    m_min <- min(G[ , 6])		# �������� ������������ ������� � G
    z_min <- G[which.min(G[ , 6]), ]	# ������ � ���������� �������� �� G
    for(i in 1:nrow(X2)) {
      m <- margin(X2[i, 3:4], X2[i, 5], G, k)	# ��������� ������ ��� ���� ��������� �������� �� ��-�� ������� ��������
      if(m < delta)  {			# ������� � E ��� ��������� ������� � �������� ���� ����������
        E <- rbind(E, X2[i, ])
          if(m < m_min){			# ����� ���� ��������� ������ � ����������� ��������
            m_min <- m
            z_min <- X2[i, ]
          }
      }
    }
    if(nrow(E) < l0) break		# ���� ���������� ��������� �������� � �������� ���� ���������� ����� ��������� ��� ���, �������� ��������
    G <- rbind(G, z_min)		# ����� ������������ � G(��-�� ������� ��������) ������ � ���������� ��������
  }
  G <- G[ , 1:(ncol(G) - 1)] 		# ������ �������, ������� �������� ������������
  return(G)
}
```

������ ������ ���������:
1) ������� �������, ���� ����� ���������� �������� ����� 2:

![alt text](https://github.com/elena111111/R/blob/master/stolp/stolp_delta2.png)     

��������� ������ knn ����� stolp � ����� ������ ��������� k:

![alt text](https://github.com/elena111111/R/blob/master/stolp/stolp_knn_delta2_loo03933_and_loo.png)

2) ������� �������, ���� ����� ���������� �������� ����� 3:

![alt text](https://github.com/elena111111/R/blob/master/stolp/stolp_delta3.png)     

��������� ������ knn ����� stolp � ����� ������ ��������� k:

![alt text](https://github.com/elena111111/R/blob/master/stolp/stolp_knn_delta3_loo00933_and_loo.png)

3) �������� ��������� ������ �������� knn:

![alt text](https://github.com/elena111111/R/blob/master/stolp/knn_and_loo.png)

# ��������� �� LOO ����������� ����������

<table><tr>
<th>��������</th><th>LOO</th><th>�������������</th>
</tr><tr><td>knn</td><td>0.03333</td><td>k_opt = 6</td>
</tr><tr><tr><td>wknn</td><td>0.04</td><td>q_opt = 0.95</td>
</tr><tr><tr><td>pw(Epan)</td><td>0.04</td><td>h_opt = 1.6</td>
</tr><tr><tr><td>pw(Gauss)</td><td>0.04</td><td>h_opt = 0.9</td>
</tr><tr><tr><td>pw(Quart)</td><td>0.04</td><td>h_opt = 2</td>
</tr><tr><tr><td>pw(Triang)</td><td>0.04</td><td>h_opt = 2.7</td>
</tr><tr><tr><td>pw(Rect)</td><td>0.04</td><td>h_opt = 1</td>
</tr><tr><tr><td>varpw(Epan)</td><td>0.04</td><td>k = 6</td>
</tr><tr><tr><td>varpw(Gauss)</td><td>0.04</td><td>k = 6</td>
</tr><tr><tr><td>varpw(Quart)</td><td>0.04</td><td>k = 6</td>
</tr><tr><tr><td>varpw(Triang)</td><td>0.04</td><td>k = 6</td>
</tr><tr><tr><td>varpw(Rect)</td><td>0.03333</td><td>k = 6</td>
</tr></table>

# ����������� ������ �������������

����������� ������������ �������� �������:

<a href="https://www.codecogs.com/eqnedit.php?latex=a(x)&space;=&space;arg&space;\max_{y&space;\in&space;Y}&space;\lambda_y&space;P_y&space;p_y(x)" target="_blank"><img src="https://latex.codecogs.com/gif.latex?a(x)&space;=&space;arg&space;\max_{y&space;\in&space;Y}&space;\lambda_y&space;P_y&space;p_y(x)" title="a(x) = arg \max_{y \in Y} \lambda_y P_y p_y(x)" /></a>

���: 
<a href="https://www.codecogs.com/eqnedit.php?latex=\lambda_{yy}&space;=&space;0,&space;\lambda_{ys}&space;=&space;\lambda_{y},&space;\forall&space;{y,&space;s}&space;\in&space;Y" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\lambda_{yy}&space;=&space;0,&space;\lambda_{ys}&space;=&space;\lambda_{y},&space;\forall&space;{y,&space;s}&space;\in&space;Y" title="\lambda_{yy} = 0, \lambda_{ys} = \lambda_{y}, \forall {y, s} \in Y" /></a>
 -- �������� ������ ��� ��������� ������� ������ *y* � ������ *s*,

<a href="https://www.codecogs.com/eqnedit.php?latex=P_y" target="_blank"><img src="https://latex.codecogs.com/gif.latex?P_y" title="P_y" /></a>
-- ��������� ����������� ������,

<a href="https://www.codecogs.com/eqnedit.php?latex=p_y(x)" target="_blank"><img src="https://latex.codecogs.com/gif.latex?p_y(x)" title="p_y(x)" /></a>
 -- ������� ������������� ������.

## ������� ����������� �������������

���������, ����� ��� �������� ����������.
���������� ��, ��� ��������� ��������� ���������� ���������� �����, ��� ���� ����������� ���������.
������������ ������ ��������� ����� �������� ���:

<a href="https://www.codecogs.com/eqnedit.php?latex=\hat{p}_h(x)&space;=&space;\frac{1}{m}&space;\sum_{i&space;=&space;1}^{m}&space;\prod_{j&space;=&space;1}^{n}&space;\frac{1}{h_j}&space;K&space;\left(&space;\frac{f_j(x)&space;-&space;f_j(x_i)))}{h_j}&space;\right&space;)," target="_blank"><img src="https://latex.codecogs.com/gif.latex?\hat{p}_h(x)&space;=&space;\frac{1}{m}&space;\sum_{i&space;=&space;1}^{m}&space;\prod_{j&space;=&space;1}^{n}&space;\frac{1}{h_j}&space;K&space;\left(&space;\frac{f_j(x)&space;-&space;f_j(x_i)))}{h_j}&space;\right&space;)," title="\hat{p}_h(x) = \frac{1}{m} \sum_{i = 1}^{m} \prod_{j = 1}^{n} \frac{1}{h_j} K \left( \frac{f_j(x) - f_j(x_i)))}{h_j} \right )," /></a>

��� *x* - ���������������� �����, *x_i* - ����� �� �������, *h* - ������ ����, *m* - ���������� �����, *n* - ���������� ���������, *K* - ������� ����, *f_j* - �������� ��������.

����������:
```R
naiveBayes <- function(z, X, lambda, Ker){ 	# z - ���������������� �����, X - ��������� �������, lambda - �������� ������ ��� ��������� ������� ������ y � ������� ������, Ker - ������� ���� 
  n <- 3:4					# ������ ���������
  l <- nrow(X)
  m <- length(levels(X$Species)) 		#- ���-�� �������
  P_apr <- c() 
  P_apr[1:m] <- 0
  
  p <- c()					# ����� ����� ��������� �������������
  p[1:m] <- 0
  ans <- data.frame(1:m, levels(X$Species))
  colnames(ans) <- c("rule", "Species")
  # ������� X �� ����������
  for(i in 1:m){ 
    subclass <- X[X$Species == levels(X$Species)[i], ] 	# - ��������
    # ������� ��������� �����������
    P_apr[i] <- nrow(subclass)/l			
    # ������� ��������� �������������
    for(s in 1:nrow(subclass)){
      for(j in n){
        if(Ker((z[j] - subclass[i, j])/h[i]) > 0) 		# ����� �������� �����������
          p[i] <- p[i] + logb(Ker((z[j] - subclass[i, j])/h[i])/h[i])
      }
    }
    if(p[i] != 0) {
      p[i] <- p[i] - logb(nrow(subclass))
      ans[i, 1] <- (logb(lambda[i]*P_apr[i]) + p[i])  # ���������� �� ����������� � ����������� ������������ �������� �������
    } else ans[i, 1] <- logb(lambda[i]*P_apr[i])
  }
  return(ans[which.max(ans[ , 1]), 2])
}
```
 
������ ������ ��������� ��� ������������ ����:

![alt text](https://github.com/elena111111/R/blob/master/nb/nb_gauss.png)

# �������� ��������� �������������

��������������� ������ ������������� � ����� �������� *Y = {-1, +1}*.
������ ����������:

<a href="https://www.codecogs.com/eqnedit.php?latex=a(x,&space;w)&space;=&space;sign&space;f(x,&space;w)" target="_blank"><img src="https://latex.codecogs.com/gif.latex?a(x,&space;w)&space;=&space;sign&space;f(x,&space;w)" title="a(x, w) = sign f(x, w)" /></a>

��� *w* - ������ ����������, *f(x, w)* - ��������������� �������.

���� *f(x, w) > 0*, �� �������� ������� x � ������ +1, ���� *f(x, w) < 0*, �� � ������ -1, � ��������� *f(x, w) = 0* ������ ����������� �����������.

�������� ( *margin* ) ������� x_i ������������ ��������� ��������� ���������� 

<a href="https://www.codecogs.com/eqnedit.php?latex=M_i(w)&space;=&space;y_i&space;f(x_i,&space;w)." target="_blank"><img src="https://latex.codecogs.com/gif.latex?M_i(w)&space;=&space;y_i&space;f(x_i,&space;w)." title="M_i(w) = y_i f(x_i, w)." /></a>

��� ������ ������, ��� �������� ���������������� x_i.

�� ����� �������������� ������������ ����: 

<a href="https://www.codecogs.com/eqnedit.php?latex=Q(w,&space;X^l)&space;\leqslant&space;\sum_{i&space;=&space;1}^{l}\pounds&space;(M_i(w)))&space;\rightarrow&space;\min_w" target="_blank"><img src="https://latex.codecogs.com/gif.latex?Q(w,&space;X^l)&space;\leqslant&space;\sum_{i&space;=&space;1}^{l}\pounds&space;(M_i(w)))&space;\rightarrow&space;\min_w" title="Q(w, X^l) \leqslant \sum_{i = 1}^{l}\pounds (M_i(w))) \rightarrow \min_w" /></a>

��� 
<a href="https://www.codecogs.com/eqnedit.php?latex=\pounds&space;(M)" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\pounds&space;(M)" title="\pounds (M)" /></a>  - ��������� �������������� ������� �������.

������� �������� �������������, ����: 

<a href="https://www.codecogs.com/eqnedit.php?latex=f(x,&space;w)&space;=&space;<&space;w,&space;x&space;>." target="_blank"><img src="https://latex.codecogs.com/gif.latex?f(x,&space;w)&space;=&space;<&space;w,&space;x&space;>." title="f(x, w) = < w, x >." /></a>

## ����� ��������������� ���������

����� ������ ������ ����� *w*, �� ������ ���� ������� ��� � ������� ������������� �������� Q (������������������ ������������ ����):

<a href="https://www.codecogs.com/eqnedit.php?latex=w&space;=&space;w&space;-&space;\eta&space;{Q}'(w)" target="_blank"><img src="https://latex.codecogs.com/gif.latex?w&space;=&space;w&space;-&space;\eta&space;{Q}'(w)" title="w = w - \eta {Q}'(w)" /></a>

���

<a href="https://www.codecogs.com/eqnedit.php?latex=w&space;=&space;w&space;-&space;\eta&space;\sum_{i&space;=&space;1}^{l}&space;{\pounds&space;}'(<&space;w,&space;x_i>&space;y_i)&space;x_i&space;y_i" target="_blank"><img src="https://latex.codecogs.com/gif.latex?w&space;=&space;w&space;-&space;\eta&space;\sum_{i&space;=&space;1}^{l}&space;{\pounds&space;}'(<&space;w,&space;x_i>&space;y_i)&space;x_i&space;y_i" title="w = w - \eta \sum_{i = 1}^{l} {\pounds }'(< w, x_i> y_i) x_i y_i" /></a>

������ ��������� *(x_i, y_i)* ������ ���� ����� � ��������� ������� *w*. ������� �������, ���� (������ �������� ���� *l* ����������� � ����� ����������) �� ������ ���� ����� �������� ��������� ��������� � ����� ������ ����������:

<a href="https://www.codecogs.com/eqnedit.php?latex=w&space;=&space;w&space;-&space;\eta&space;{\pounds&space;}'_a(<&space;w,&space;x_i>&space;y_i)&space;x_i&space;y_i" target="_blank"><img src="https://latex.codecogs.com/gif.latex?w&space;=&space;w&space;-&space;\eta&space;{\pounds&space;}'_a(<&space;w,&space;x_i>&space;y_i)&space;x_i&space;y_i" title="w = w - \eta {\pounds }'_a(< w, x_i> y_i) x_i y_i" /></a>

����� ������ ����������� ������ ��� ���� �������, ���������� ��������� ����� ������� ��������.
����� �����������, ����� �������� *Q* ����� ���������� ����, ��� ����� *w* ���������� ����������.

����������:
```R
sg <- function(Xl, eta, lambda, classes, major){  # Xl - ��������� �������, eta - ���� ��������, lambda - �������� �����������, classes - �������� ���� �������, major - ��������� (�������)
  rownames(Xl) <- 1:nrow(Xl)
  l <- nrow(Xl)
  n <- ncol(Xl) - 1
  w <- runif(n, -1 / (2*n), 1 / (2*n))	# ������� ��������� �������� ������� ����� �� ����� ���������
  print(w)
  y <- c()				# ��� ����� ������ ������� ������� �� ���������� -1, +1
  y[1:l] <- 1
  
  for(i in 1:l) {
    if(Xl$Species[i] == classes[1]) { y[i] <- -1 }
  }
  
  Q <- 0				# ������������ ����, ������� �� ����� ��������������
  for(i in 1:l)
    Q <- Q + major(y[i]*ScalarProduct(w, Xl[i, 1:2]))
  print(Q)
  
  w_new <- c()
  w_new[1:l] <- 0
  
  while(abs(Q) >= 0.01 && !eq(w, w_new)){	# ���� ������������ ���� �� ������ ���������� ���, ��� ������ ����� �� ���������� ����������
    w_new <- w
    rand <- sample(1:l, 1, replace = T)		# ������� �������� ����� ������� 
    m <- y[rand]*ScalarProduct(w, Xl[rand, 1:2])	# ������ ������ ��� ����� Xl[rand, 1:2]
    e <- major(m)				# �������� ������ ���������
    w <- w - eta * dQ(m) * Xl[rand, 1:2] * y[rand]	# ������� ������ w � ����������� ������������� (�������� �������� �������� ����������� Q)
    Q <- (1 - lambda)*Q + lambda*e		# ������ ����� �������� Q
  }
  return(w)
}
```

������ ������ ��������� (������ ����� - ������� ��������� ��������� ����������� ������, ������� - ������������� �������):

1) ��� ������� setosa � versicolor: 

![alt text](https://github.com/elena111111/R/blob/master/sg/sg_set-vers.png)

2) ��� ������� setosa � virginica: 

![alt text](https://github.com/elena111111/R/blob/master/sg/sg_set-virg.png)

3) ��� ������� versicolor � virginica: 

![alt text](https://github.com/elena111111/R/blob/master/sg/sg_vers-virg.png)