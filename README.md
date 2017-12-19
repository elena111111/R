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

```
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

```
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

```
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
```
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
```
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
```
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
```
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

![alt text](https://github.com/elena111111/R/blob/master/pf/pf_gauss_015_center.png)

������ �������� ������ ���� � ��������� ����������� (�� ��������� � ��� ����� 1).

## �������� STOLP
 
��������� ������ �������� ������.
��������� �������� �� ������� ��������� ������� ��������, � ��������� �� ���� ���������.



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
������������ ������� ��������� ����� �������� ���:\

<a href="https://www.codecogs.com/eqnedit.php?latex=\hat{p}_h(x)&space;=&space;\frac{1}{m}&space;\sum_{i&space;=&space;1}^{m}&space;\prod_{j&space;=&space;1}^{n}&space;\frac{1}{h_j}&space;K&space;\left(&space;\frac{f_j(x)&space;-&space;f_j(x_i)))}{h_j}&space;\right&space;)," target="_blank"><img src="https://latex.codecogs.com/gif.latex?\hat{p}_h(x)&space;=&space;\frac{1}{m}&space;\sum_{i&space;=&space;1}^{m}&space;\prod_{j&space;=&space;1}^{n}&space;\frac{1}{h_j}&space;K&space;\left(&space;\frac{f_j(x)&space;-&space;f_j(x_i)))}{h_j}&space;\right&space;)," title="\hat{p}_h(x) = \frac{1}{m} \sum_{i = 1}^{m} \prod_{j = 1}^{n} \frac{1}{h_j} K \left( \frac{f_j(x) - f_j(x_i)))}{h_j} \right )," /></a>

��� *x* - ���������������� �����, *x_i* - ����� �� �������, *h* - ������ ����, *m* - ���������� �����, *n* - ���������� ���������, *K* - ������� ����, *f_j* - �������� ��������.

����������:
```
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
