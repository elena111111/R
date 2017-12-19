# Метрические алгоритмы классификации

Для таких алгоритмов должна выполняться гипотеза компактности: схожим объектам соответствуют схожие ответы.
Вводится мера близости объектов (в нашем случае евклидово расстояние), которая показывает, насколько эти объекты похожи. 

Обобщенный метрический классификатор:

<a href="https://www.codecogs.com/eqnedit.php?latex=a(z,&space;X^l)&space;=&space;arg&space;\max_{y&space;\in&space;Y}&space;W_y(z,&space;X^l),&space;W_y(z,&space;X^l)&space;=&space;\sum_{i:&space;y_z^{(i)}&space;=&space;y}w(i,&space;z)," target="_blank"><img src="https://latex.codecogs.com/gif.latex?a(z,&space;X^l)&space;=&space;arg&space;\max_{y&space;\in&space;Y}&space;W_y(z,&space;X^l),&space;W_y(z,&space;X^l)&space;=&space;\sum_{i:&space;y_z^{(i)}&space;=&space;y}w(i,&space;z)," title="a(z, X^l) = arg \max_{y \in Y} W_y(z, X^l), W_y(z, X^l) = \sum_{i: y_z^{(i)} = y}w(i, z)," /></a>

где *W*- оценка близости объекта *z* к класу *y*, *w(i, z)* - весовая функция, оценивающая степень важноси *i*-го соседа для объекта *z*.

## knn - метод k ближайших соседей.
Дана выборка ирисов Фишера(150 элементов), в ней 3 класса(*setosa*, *versicolor*, *virginica*). 
Мы хотим классифицировать множество точек *{z}* по 2м признакам (Petal.Length, Petal.Width). Эти же признаки - координаты точек на графике.

Метод относит точку *z* к тому классу, который чаще всего встречается среди её соседей. 

Реализация:

```R
knn <- function(z, X, k){	# X - это Ирисы Фишера, k - число соседей точки z
  xl <- X[ , 3:5]		# в xl координаты точек Petal.Length, Petal.Width и название класса , к которому относится соответствующая точка
  l <- dim(xl)[1] 
  n <- dim(xl)[2] - 1
  
  distances <- c()
  for(tmp in 1:l){
    distances[tmp] <- eDist(xl[tmp, 1:n], z)	# расстояния от точки z до каждой точки из выборки
  }
  orderedxl <- xl[order(distances), ]		# сортируем точки в порядке увеличения расстояния от z до точки из выборки
  classes <- orderedxl[1:k, n + 1]  		# заберем только первые k строк (получим k ближайших соседей), и в них оставим только столбец названий классов
  counts <- table(classes) 			# посчитаем, сколько раз встретился каждый класс
  return(names(which.max(counts)))		# точка z относится к тому классу, который чаще всего встречается в classes
}
```

Оптимальное число соседей *k* подбирается по *LOO* (скользящий контроль), который работает следующим образом: 

```R
loo <- function(X, alg, step, x_max){
  Ox <- seq(from = 1, to = x_max, by = step)
  Oy <- c()
  l <- dim(X)[1]	
  LooOpt <- 1
  kOpt <- 1
  
  for(k in Ox){		#для разного числа соседей
    Q <- 0		# количество ошибок
    for(i in 1:l){
      X2 <- X[-i, ]	# из выборки X будем исключать по одной точке (z)
      z <- X[i, 3:4]	# запустим алгоритм(knn), как будто мы хотим классифицировать точку z, и имеем выборку X2.
      if(alg(z, X2, k) != X[i, 5]) Q <- Q + 1	# если алгоритм ошибся, увеличим количество ошибок Q
    }
    Loo <- Q/l 		# сумму ошибок делим на количество точек в выборке
    Oy <- c(Oy, Loo)
    if(Loo < LooOpt) {
      LooOpt <- Loo
      kOpt <- k		# оптимальным будет k, при котором величина Loo минимальна.
    }
  }
  return(kOpt)
}
```

Результат работы knn: 

![alt text](https://github.com/elena111111/R/blob/master/knn/knn.png)

Зависимость *Loo* от *k*:

![alt text](https://github.com/elena111111/R/blob/master/knn/knn_loo.png)


## Алгортим k взвешенных ближайших соседей (wknn).
Имеется выборка *X* (ирисы Фишера), и 3 класса (*setosa*, *versicolor*, *virginica*). 
Мы хотим классифицировать множество точек *{z}*.
Классификацию проводим по двум признакам (Petal.Length, Petal.Width), они же являются координатами точек.

Отличие от обычного knn в том, что соседям присваиваются значения весовой функции (q^i), зависящей от ранга соседа.
Метод относит точку *z* к тому классу, суммарный вес которого максимален среди её соседей. 

Реализация:

```R
wknn <- function(z, X, k, q){ 		# z - классифицируемая точка, X - обучающая выборка, k - число соседей, q - знаменатель геометрической прогрессии (нашей весовой функции)
  xl <- X[ , 3:5]			# в xl координаты точек Petal.Length, Petal.Width и название класса , к которому относится соответствующая точка
  l <- dim(xl)[1] 
  n <- dim(xl)[2] - 1
  
  distances <- c()
  for(tmp in 1:l){
    distances[tmp] <- eDist(xl[tmp, 1:n], z)		# расстояния от точки z до каждой точки из выборки xl
  }
  orderedxl <- xl[order(distances), ]			# xl, отсортированный по возрастанию расстояний от точки z до каждой точки из выборки
  Weights <- q^(1:k) 					# веса для k ближайших соседей
  classes <- cbind(orderedxl[1:k, ], Weights)[ , (n + 1):(n + 2)]  	#  в classes будет k строк, а в столбцах название класса и вес точки
  
  sumSetosa <- sum(classes[classes$Species == "setosa", 2])
  sumVersicolor <- sum(classes[classes$Species == "versicolor", 2])
  sumVirginica <- sum(classes[classes$Species == "virginica", 2])
  
  ans <- matrix(c(sumSetosa, sumVersicolor, sumVirginica), 
                nrow = 1, ncol = 3, byrow = T, list(c(1), c('setosa', 'versicolor', 'virginica'))) 	#сумма весов для каждого класса
  return(colnames(ans)[which.max(ans)])				# ответом будет тот класс, вес которого максимален в ans
}
```

Значение *q* (в интервале *(0; 1)*) подбирается по *LOO* (аналогично с подбором k в knn).
Оптимальным будет *q*, при котором величина *Loo* минимальна.

Результат работы программы: 

![alt text](https://github.com/elena111111/R/blob/master/wknn/wknn.png)

Зависимость *Loo* от *q*:

![alt text](https://github.com/elena111111/R/blob/master/wknn/wknn_loo_q.png)

## Алгоритм парзеновского окна (pw).
Имеется выборка *X* (ирисы Фишера), и 3 класса (*setosa*, *versicolor*, *virginica*). 
Мы хотим классифицировать множество точек *{z}*.
Классификацию проводим по двум признакам (Petal.Length, Petal.Width), они же являются координатами точек.

Отличие от метода wknn в том, что весовая функция теперь зависит не от ранга соседа, а от расстояния (от точки *z* до точки из выборки), 
а также от выбора параметра *h* (ширина окна) и ядра K, которые подбираются по LOO (принцип работы уже был описан в прдыдущих алгоритмах).
Т.е. мы строим вокруг точки *z*(центр окна) окрестность радиуса *h*, и смотрим, суммарный вес какого класса в этой окрестности больше.

Реализация:
```R
pw <- function(X, z, h, K){	# X - обучающая выборка, z - классифицируемая точка, h - ширина окна, K - функция ядра
  xl <- X[, 3:5]		# в xl координаты точек Petal.Length, Petal.Width и название класса , к которому относится соответствующая точка
  l <- dim(xl)[1]
  n <- dim(xl)[2] - 1
  
  distances_weighed <- matrix(NA, l, 2)
  for(tmp in 1:l){
    distances_weighed[tmp, 1] <- eDist(xl[tmp, 1:n], z)		# distances_weighed[ , 1] - это расстояния от точки z до каждой точки из выборки xl
    distances_weighed[tmp, 2] <- K(distances_weighed[tmp, 1]/h)	# distances_weighed[ , 2] - значение весовой функции для каждой точки из выборки
  }
  classes <- data.frame(distances_weighed[ , 1], distances_weighed[ , 2], xl[ , 3]) #расстояния от z до точек из xl, веса точек из xl и название класса каждой точки
  colnames(classes) <- c("Distances", "Weights", "Species")
  sumSetosa <- sum(classes[classes$Species == "setosa", 2])
  sumVersicolor <- sum(classes[classes$Species == "versicolor", 2])
  sumVirginica <- sum(classes[classes$Species == "virginica", 2])
  ans <- matrix(c(sumSetosa, sumVersicolor, sumVirginica), 
                nrow = 1, ncol = 3, byrow = T, list(c(1), c('setosa', 'versicolor', 'virginica'))) 	# ans - cумма весов каждого класса
  return(colnames(ans)[which.max(ans)])					# ответом будет класс, суммарный вес которого максимален в ans
}
```
Для каждого ядра получилось одинаковое значение минимального *Loo*. Так что мы убедились, что выбор ядра слабо влияет на классификацию.

Стоит отметить, что данный алгоритм плохо подходит для неравномерно распределённой выборки (в окно одинаковой ширины попадает очень разное количество объектов). 
В этом случае лучше использовать алгоритм парзеновского окна с переменной шириной окна (описан ниже).

Результат работы программы / Зависимость *loo* от *h* (для разных ядер):

1) Прямоугольное ядро:

![alt text](https://github.com/elena111111/R/blob/master/pw/pw_ker_rect_and_loo.png)

2) Треугольное ядро:

![alt text](https://github.com/elena111111/R/blob/master/pw/pw_ker_triang_and_loo.png)

3) Ядро Епанечникова:

![alt text](https://github.com/elena111111/R/blob/master/pw/pw_ker_epan_and_loo.png)

4) Квартическое ядро:

![alt text](https://github.com/elena111111/R/blob/master/pw/pw_ker_quart_and_loo.png)

5) Гауссовское ядро: 

![alt text](https://github.com/elena111111/R/blob/master/pw/pw_ker_gauss_and_loo.png)

## Алгоритм парзеновского окна с переменной шириной окна (varpw).
Имеется выборка *X* (ирисы Фишера), и 3 класса (*setosa*, *versicolor*, *virginica*). 
Мы хотим классифицировать множество точек *{z}*.
Классификацию проводим по двум признакам (Petal.Length, Petal.Width), они же являются координатами точек.

Ширина окна *h* теперь - это расстояние от точки  *z* до *k+1*-го соседа этой точки.
Для весовой функции нам нужно выбрать ядро(в примере работы программы используется прямоугольное).

Реализация:
```R
varpw <- function(X, z, K, k){	# X - обучающая выборка, z - классифицируемая точка, K - функция ядра, k - число соседей
  xl <- X[, 3:5]		# в xl координаты точек Petal.Length, Petal.Width и название класса , к которому относится соответствующая точка
  l <- dim(xl)[1]
  n <- dim(xl)[2] - 1
  
  distances <- c()
  for(tmp in 1:l){
    distances[tmp] <- eDist(xl[tmp, 1:n], z)	# вектор расстояний от точки z до каждой точки из выборки
  }
  orderedxl <- xl[order(distances), ]		# orderedxl - это массив xl, отсортированный по возрастанию расстояний от точки z до каждой точки из выборки
  distances_s <- sort(distances)		# distances_s - отсортированный вектор расстояний 
  weights <- c()				# weights - вектор весов для k ближайших соседей.
  for(tmp in 1:k){
  if(distances_s[k+1] != 0)
      weights[tmp] <- K(distances_s[tmp]/distances_s[k+1])	# аргумент ядра - расстояние от z до i-го соседа точки z, деленое на расстояние от z до k+1-го соседа.
      else weights[tmp] <- K(0)	
  }
  classes <- cbind(orderedxl[1:k, ], weights)[ , (n + 1):(n + 2)]		# dозьмем из orderedxl первые k строк, допишем к ним соответствующие веса и оставим только столбцы названий классов и весов
  sumSetosa <- sum(classes[classes$Species == "setosa", 2])
  sumVersicolor <- sum(classes[classes$Species == "versicolor", 2])
  sumVirginica <- sum(classes[classes$Species == "virginica", 2])
  ans <- matrix(c(sumSetosa, sumVersicolor, sumVirginica), 
                nrow = 1, ncol = 3, byrow = T, list(c(1), c('setosa', 'versicolor', 'virginica')))
  return(colnames(ans)[which.max(ans)])				# ответом будет тот класс, суммарный вес которого максимален
}
``` 

Данный метод подходит для неравномерно распределенной выборки.
На примере прямоугольного ядра лучше всего видно отличие в результатах данного и предыдущего алгоритма.

Пример работы программы:

![alt text](https://github.com/elena111111/R/blob/master/varpw/varpw_core_rect.png)

## Метод потенциальных функций 
Имеется выборка *X* (ирисы Фишера), и 3 класса (*setosa*, *versicolor*, *virginica*). 
Мы хотим классифицировать множество точек *{z}*.
Классификацию проводим по двум признакам (Petal.Length, Petal.Width), они же являются координатами точек.

В методе парзеновского окна мы помещали центр окна в классифицируемый объект, а теперь построим окрестности вокруг обучающих объектов. 
Причем каджая окрестность будет иметь свой потенциал - величину, показывающую, насколько сильно это окно влияет на другие элементы. То есть даже при одинаковой ширине окна *h* это влияние может быть разное.

Первым мы вызываем метод *gamma*, в теле которого происходит следующее: 
`g[dim(iris)[1]]` - это вектор потенциалов, который вычисляется следующим образом: `g <- gamma(X, K, h, eps)`:
```R
gamma <- function(X, K, h, eps = 0.15){ 	# X - обучающая выборка, K - функция ядра, h - ширина окна, eps - максимально допустимая доля ошибок (от 0 до 1)
  xl <- X[, 3:5]                  
  l <- dim(xl)[1]
  g <- c()
  i <- 1:l
  g[i] <- 0		 
 #пока число ошибок больше заданной точности, надо улучшать вектор g:
  while(loo(X, g, K, h) > eps){						# loo возвращает количество ошибок алгоритма(pf) с данными параметрами, деленое на размерность выборки (число от 0 до 1)
    rand <- sample(i, 1, replace = T)					# случайным образом выберем номер элемента из выборки (xi)
    if(pf(X, c(xl[rand, 1], xl[rand, 2]), g, K, h) != xl[rand, 3]) g[rand] <- g[rand] + 1	# если алгортим pf(X, xi, g, K, h) ошибается, значит в точке xi нужно увеличить потенциал на 1
  }
  return(g)
}
```

Шируну окна *h* можно подбирать индивидуально для каждого объекта, но в алгоритме не указывается, как именно (недостаток). 
В данном случае эта величина взята из метода парзеновского окна, и одинакова для всех окон.

Метод *pf* работает аналогично с методом парзеновского окна, только значение функции ядра ещё домножается на *g*:

Реализация:
```R
pf <- function(X, z, g, K, h){	# X - обучающая выборка, z - классифицируемая точка, g - вектор потенциалов, K - функция ядра, h - ширина окна
  xl <- X[, 3:5]		# xl - координаты точек Petal.Length, Petal.Width и название класса , к которому относится соответствующая точка
  l <- dim(xl)[1]
  n <- dim(xl)[2] - 1
  
  distances_weighed <- matrix(NA, l, 2)
  for(tmp in 1:l){
    distances_weighed[tmp, 1] <- eDist(xl[tmp, 1:n], z)			# distances_weighed[ , 1] - это расстояния от точки z до каждой точки из выборки
    distances_weighed[tmp, 2] <- g[tmp]*K(distances_weighed[tmp, 1]/h)		# distances_weighed[ , 2] - значение весовой функции для каждой точки из выборки
  }
  
  classes <- data.frame(distances_weighed[ , 1], distances_weighed[ , 2], xl[ , 3])	# classes - это distances_weighed, к которому добавили столбец соответствующих названий классов. 
  colnames(classes) <- c("Distances", "Weights", "Species")
  sumSetosa <- sum(classes[classes$Species == "setosa", 2])
  sumVersicolor <- sum(classes[classes$Species == "versicolor", 2])
  sumVirginica <- sum(classes[classes$Species == "virginica", 2])
  ans <- matrix(c(sumSetosa, sumVersicolor, sumVirginica), 
                nrow = 1, ncol = 3, byrow = T, list(c(1), c('setosa', 'versicolor', 'virginica'))) 
  return(colnames(ans)[which.max(ans)])					# ответом для z будет тот класс, сумарный вес которого максимален в classes
}
```

Пример работы программы(для гауссовского ядра):

![alt text](https://github.com/elena111111/R/blob/master/pf/pf_05333.png)

Черным показаны центры окон с ненулевым потенциалом (он получился в них равен 1).

## Алгоритм STOLP
 
Выполняет сжатие исходных данных.
Позволяет отобрать из выборки множество опорных объектов, и обучаться на этом множестве.

Покажем работу алгоритма на примере knn.

Отступом ( *margin* ) объекта *x_i* относительно метрического алгоритма классификации называется: 

<a href="https://www.codecogs.com/eqnedit.php?latex=M(x_i)&space;=&space;W_y_i(x_i,&space;X^l)&space;-&space;\max_{y&space;\in&space;Y&space;\setminus&space;\{y_i\}&space;}&space;W_y(x_i,&space;X^l)" target="_blank"><img src="https://latex.codecogs.com/gif.latex?M(x_i)&space;=&space;W_y_i(x_i,&space;X^l)&space;-&space;\max_{y&space;\in&space;Y&space;\setminus&space;\{y_i\}&space;}&space;W_y(x_i,&space;X^l)" title="M(x_i) = W_y_i(x_i, X^l) - \max_{y \in Y \setminus \{y_i\} } W_y(x_i, X^l)" /></a>

В нашем случае это разность между количеством объектов(среди *k* соседей) своего класса для *x_i* и максимальным количеством объектов чужого класса.

Реализация метода margin:
```R
margin <- function(z, class_z, X, k){  # z - точка, для которой считаем отступ, class_z - класс, которому принадлежит z, X - выборка (ирисы Фишера),  k - число соседей для knn
  xl <- X[ , 3:5]
  l <- nrow(xl) 
  n <-  ncol(xl) - 1
  
  distances <- c()
  for(tmp in 1:l){
    distances[tmp] <- eDist(xl[tmp, 1:n], z)	# расстояния от *z*
  }
  orderedxl <- xl[order(distances), ]	# сортируем точки в порядке увеличения расстояния
  classes <- orderedxl[1:k, n + 1]		# отбираем только первые k строк и столбец названий классов  
  counts <- table(classes) 		# считаем, сколько раз встретился каждый класс
  
  return(counts[class_z] - max(counts[names(counts) != class_z]))	#разность между количеством точек класса class_z и максимальным количеством точек другого класса
}
```

Объекты с большим отрицательным отступом называются выбросами. Их лучше удалить из выборки.
Объекты с большим положительным отступом называются эталонными. На них будем ориентироваться при классификации.

```R
stolp <- function(X, k, M, l0 = 50, delta = 2){  # X - обучающая выборка, k - количество соседей, M - вектор отступов, l0 - допустимая доля ошибок, delta - порог фильтрации выбросов
  G <- data.frame()		# - здесь будут храниться опорные объекты
  X <- cbind(X, M)
  i <- 1
  while(i <= nrow(X)) {
    if(X[i, 6] < delta) {	# если объект является выбросом, удалим его из выборки
      X <- X[-i, ]
      rownames(X) <- 1:nrow(X)	# перенумерация строк 
      i <- i - 1
    }
    i <- i + 1
  }
  
  for(i in 1:length(levels(X$Species))){		# для каждого класса
    tmp <- X[X$Species == levels(X$Species)[i], ] 	# - подмассив (объекты одного класса)
    G <- rbind(G, tmp[which.max(tmp[, 6]), ])	#  добавим по одному эталонному объекту в G
  }
  
  while(nrow(G) != nrow(X)){
    E <- data.frame()
    X2 <- X
    i <- 1
    while(i <= nrow(X2)) {	# получим множество X\G в X2
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
    
    m_min <- min(G[ , 6])		# значение минимального отступа в G
    z_min <- G[which.min(G[ , 6]), ]	# объект с наименьшим отступом из G
    for(i in 1:nrow(X2)) {
      m <- margin(X2[i, 3:4], X2[i, 5], G, k)	# вычислить отступ для всех неопорных объектов на мн-ве опорных объектов
      if(m < delta)  {			# добавим в E все неопорные объекты с отступом ниже порогового
        E <- rbind(E, X2[i, ])
          if(m < m_min){			# сразу ищем неопорный объект с минимальным отступом
            m_min <- m
            z_min <- X2[i, ]
          }
      }
    }
    if(nrow(E) < l0) break		# если количество неопорных объектов с отступом ниже порогового стало преемлемо для нас, алгоритм завершен
    G <- rbind(G, z_min)		# иначе присоединить к G(мн-ву опорных объектов) объект с наименьшим отступом
  }
  G <- G[ , 1:(ncol(G) - 1)] 		# уберем столбец, который временно присоединили
  return(G)
}
```

Пример работы программы:
1) Опорные объекты, если порог фильтрации выбросов равен 2:

![alt text](https://github.com/elena111111/R/blob/master/stolp/stolp_delta2.png)     

Результат работы knn после stolp и новый подбор параметра k:

![alt text](https://github.com/elena111111/R/blob/master/stolp/stolp_knn_delta2_loo03933_and_loo.png)

2) Опорные объекты, если порог фильтрации выбросов равен 3:

![alt text](https://github.com/elena111111/R/blob/master/stolp/stolp_delta3.png)     

Результат работы knn после stolp и новый подбор параметра k:

![alt text](https://github.com/elena111111/R/blob/master/stolp/stolp_knn_delta3_loo00933_and_loo.png)

3) Вспомним результат работы обычного knn:

![alt text](https://github.com/elena111111/R/blob/master/stolp/knn_and_loo.png)

# Сравнение по LOO метрических алгоритмов

<table><tr>
<th>Алгоритм</th><th>LOO</th><th>Дополнительно</th>
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

# Байесовские методы классификации

Оптимальное байесовкское решающее правило:

<a href="https://www.codecogs.com/eqnedit.php?latex=a(x)&space;=&space;arg&space;\max_{y&space;\in&space;Y}&space;\lambda_y&space;P_y&space;p_y(x)" target="_blank"><img src="https://latex.codecogs.com/gif.latex?a(x)&space;=&space;arg&space;\max_{y&space;\in&space;Y}&space;\lambda_y&space;P_y&space;p_y(x)" title="a(x) = arg \max_{y \in Y} \lambda_y P_y p_y(x)" /></a>

где: 
<a href="https://www.codecogs.com/eqnedit.php?latex=\lambda_{yy}&space;=&space;0,&space;\lambda_{ys}&space;=&space;\lambda_{y},&space;\forall&space;{y,&space;s}&space;\in&space;Y" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\lambda_{yy}&space;=&space;0,&space;\lambda_{ys}&space;=&space;\lambda_{y},&space;\forall&space;{y,&space;s}&space;\in&space;Y" title="\lambda_{yy} = 0, \lambda_{ys} = \lambda_{y}, \forall {y, s} \in Y" /></a>
 -- величина потери при отнесении объекта класса *y* к классу *s*,

<a href="https://www.codecogs.com/eqnedit.php?latex=P_y" target="_blank"><img src="https://latex.codecogs.com/gif.latex?P_y" title="P_y" /></a>
-- априорная вероятность класса,

<a href="https://www.codecogs.com/eqnedit.php?latex=p_y(x)" target="_blank"><img src="https://latex.codecogs.com/gif.latex?p_y(x)" title="p_y(x)" /></a>
 -- функция правдоподобия класса.

## Наивный байесовский классификатор

Оптимален, когда все признаки независимы.
Использует то, что оценивать несколько одномерных плотностей проще, чем одну многомерную плотность.
Эмпирическую оценку плотности будем находить так:

<a href="https://www.codecogs.com/eqnedit.php?latex=\hat{p}_h(x)&space;=&space;\frac{1}{m}&space;\sum_{i&space;=&space;1}^{m}&space;\prod_{j&space;=&space;1}^{n}&space;\frac{1}{h_j}&space;K&space;\left(&space;\frac{f_j(x)&space;-&space;f_j(x_i)))}{h_j}&space;\right&space;)," target="_blank"><img src="https://latex.codecogs.com/gif.latex?\hat{p}_h(x)&space;=&space;\frac{1}{m}&space;\sum_{i&space;=&space;1}^{m}&space;\prod_{j&space;=&space;1}^{n}&space;\frac{1}{h_j}&space;K&space;\left(&space;\frac{f_j(x)&space;-&space;f_j(x_i)))}{h_j}&space;\right&space;)," title="\hat{p}_h(x) = \frac{1}{m} \sum_{i = 1}^{m} \prod_{j = 1}^{n} \frac{1}{h_j} K \left( \frac{f_j(x) - f_j(x_i)))}{h_j} \right )," /></a>

где *x* - классифицируемая точка, *x_i* - точка из выборки, *h* - ширина окна, *m* - количество точек, *n* - количество признаков, *K* - функция ядра, *f_j* - значение признака.

Реализация:
```R
naiveBayes <- function(z, X, lambda, Ker){ 	# z - классифицируемая точка, X - обучающая выборка, lambda - величина потери при отнесении объекта класса y к другому классу, Ker - функция ядра 
  n <- 3:4					# номера признаков
  l <- nrow(X)
  m <- length(levels(X$Species)) 		#- кол-во классов
  P_apr <- c() 
  P_apr[1:m] <- 0
  
  p <- c()					# здесь будет плотность распределений
  p[1:m] <- 0
  ans <- data.frame(1:m, levels(X$Species))
  colnames(ans) <- c("rule", "Species")
  # разбить X на подвыборки
  for(i in 1:m){ 
    subclass <- X[X$Species == levels(X$Species)[i], ] 	# - подкласс
    # считаем априорную вероятность
    P_apr[i] <- nrow(subclass)/l			
    # считаем плотность распределений
    for(s in 1:nrow(subclass)){
      for(j in n){
        if(Ker((z[j] - subclass[i, j])/h[i]) > 0) 		# чтобы логарифм существовал
          p[i] <- p[i] + logb(Ker((z[j] - subclass[i, j])/h[i])/h[i])
      }
    }
    if(p[i] != 0) {
      p[i] <- p[i] - logb(nrow(subclass))
      ans[i, 1] <- (logb(lambda[i]*P_apr[i]) + p[i])  # подставили всё необходимое в оптимальное байесовкское решающее правило
    } else ans[i, 1] <- logb(lambda[i]*P_apr[i])
  }
  return(ans[which.max(ans[ , 1]), 2])
}
```
 
Пример работы алгоритма для гауссовского ядра:

![alt text](https://github.com/elena111111/R/blob/master/nb/nb_gauss.png)

# Линейные алгоритмы классификации

Рассматривается задача классификации с двумя классами *Y = {-1, +1}*.
Модель алгоритмов:

<a href="https://www.codecogs.com/eqnedit.php?latex=a(x,&space;w)&space;=&space;sign&space;f(x,&space;w)" target="_blank"><img src="https://latex.codecogs.com/gif.latex?a(x,&space;w)&space;=&space;sign&space;f(x,&space;w)" title="a(x, w) = sign f(x, w)" /></a>

где *w* - вектор параметров, *f(x, w)* - дискриминантная функция.

Если *f(x, w) > 0*, то алгоритм относит x к классу +1, если *f(x, w) < 0*, то к классу -1, а уравнение *f(x, w) = 0* задает разделяющую поверхность.

Отступом ( *margin* ) объекта x_i относительно линейного алгоритма называется 

<a href="https://www.codecogs.com/eqnedit.php?latex=M_i(w)&space;=&space;y_i&space;f(x_i,&space;w)." target="_blank"><img src="https://latex.codecogs.com/gif.latex?M_i(w)&space;=&space;y_i&space;f(x_i,&space;w)." title="M_i(w) = y_i f(x_i, w)." /></a>

Чем больше отступ, тем надежнее классифицируется x_i.

Мы хотим минимизировать эмпирический риск: 

<a href="https://www.codecogs.com/eqnedit.php?latex=Q(w,&space;X^l)&space;\leqslant&space;\sum_{i&space;=&space;1}^{l}\pounds&space;(M_i(w)))&space;\rightarrow&space;\min_w" target="_blank"><img src="https://latex.codecogs.com/gif.latex?Q(w,&space;X^l)&space;\leqslant&space;\sum_{i&space;=&space;1}^{l}\pounds&space;(M_i(w)))&space;\rightarrow&space;\min_w" title="Q(w, X^l) \leqslant \sum_{i = 1}^{l}\pounds (M_i(w))) \rightarrow \min_w" /></a>

где 
<a href="https://www.codecogs.com/eqnedit.php?latex=\pounds&space;(M)" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\pounds&space;(M)" title="\pounds (M)" /></a>  - монотонно невозрастающая функция отступа.

Получим линейный классификатор, если: 

<a href="https://www.codecogs.com/eqnedit.php?latex=f(x,&space;w)&space;=&space;<&space;w,&space;x&space;>." target="_blank"><img src="https://latex.codecogs.com/gif.latex?f(x,&space;w)&space;=&space;<&space;w,&space;x&space;>." title="f(x, w) = < w, x >." /></a>

## Метод стохастического градиента

Будем искать вектор весов *w*, на каждом шаге изменяя его в сторону найскорейшего убывания Q (аппроксимированный эмпирический риск):

<a href="https://www.codecogs.com/eqnedit.php?latex=w&space;=&space;w&space;-&space;\eta&space;{Q}'(w)" target="_blank"><img src="https://latex.codecogs.com/gif.latex?w&space;=&space;w&space;-&space;\eta&space;{Q}'(w)" title="w = w - \eta {Q}'(w)" /></a>

или

<a href="https://www.codecogs.com/eqnedit.php?latex=w&space;=&space;w&space;-&space;\eta&space;\sum_{i&space;=&space;1}^{l}&space;{\pounds&space;}'(<&space;w,&space;x_i>&space;y_i)&space;x_i&space;y_i" target="_blank"><img src="https://latex.codecogs.com/gif.latex?w&space;=&space;w&space;-&space;\eta&space;\sum_{i&space;=&space;1}^{l}&space;{\pounds&space;}'(<&space;w,&space;x_i>&space;y_i)&space;x_i&space;y_i" title="w = w - \eta \sum_{i = 1}^{l} {\pounds }'(< w, x_i> y_i) x_i y_i" /></a>

Каждый прецедент *(x_i, y_i)* вносит свой вклад в изменение вектора *w*. Ускорим процесс, если (вместо перебора всех *l* прецедентов и затем обновления) на каждом шаге будем выбирать случайный прецедент и сразу делать обновление:

<a href="https://www.codecogs.com/eqnedit.php?latex=w&space;=&space;w&space;-&space;\eta&space;{\pounds&space;}'_a(<&space;w,&space;x_i>&space;y_i)&space;x_i&space;y_i" target="_blank"><img src="https://latex.codecogs.com/gif.latex?w&space;=&space;w&space;-&space;\eta&space;{\pounds&space;}'_a(<&space;w,&space;x_i>&space;y_i)&space;x_i&space;y_i" title="w = w - \eta {\pounds }'_a(< w, x_i> y_i) x_i y_i" /></a>

Метод строит разделяющую прямую для двух классов, настраивая параметры весов каждого признака.
Метод остановится, когда значение *Q* будет достаточно мало, или когда *w* перестанет изменяться.

Реализация:
```R
sg <- function(Xl, eta, lambda, classes, major){  # Xl - обучающая выборка, eta - темп обучения, lambda - параметр сглаживания, classes - названия двух классов, major - мажоранта (функция)
  rownames(Xl) <- 1:nrow(Xl)
  l <- nrow(Xl)
  n <- ncol(Xl) - 1
  w <- runif(n, -1 / (2*n), 1 / (2*n))	# зададим начальные значения вектора весов из этого интервала
  print(w)
  y <- c()				# это будет вектор пометок классов со значениями -1, +1
  y[1:l] <- 1
  
  for(i in 1:l) {
    if(Xl$Species[i] == classes[1]) { y[i] <- -1 }
  }
  
  Q <- 0				# эмпирический риск, который мы будем минимизировать
  for(i in 1:l)
    Q <- Q + major(y[i]*ScalarProduct(w, Xl[i, 1:2]))
  print(Q)
  
  w_new <- c()
  w_new[1:l] <- 0
  
  while(abs(Q) >= 0.01 && !eq(w, w_new)){	# пока эмпирический риск не станет достаточно мал, или вектор весов не перестанет изменяться
    w_new <- w
    rand <- sample(1:l, 1, replace = T)		# выберем случайно номер объекта 
    m <- y[rand]*ScalarProduct(w, Xl[rand, 1:2])	# найдем отступ для точки Xl[rand, 1:2]
    e <- major(m)				# вычислим ошибку алгоритма
    w <- w - eta * dQ(m) * Xl[rand, 1:2] * y[rand]	# изменим вектор w в направлении антиградиента (наиболее быстрого убывания функционала Q)
    Q <- (1 - lambda)*Q + lambda*e		# оценим новое значение Q
  }
  return(w)
}
```

Пример работы программы (черные линии - попытки алгоритма построить разделяющую прямую, розовая - окончательный вариант):

1) Для классов setosa и versicolor: 

![alt text](https://github.com/elena111111/R/blob/master/sg/sg_set-vers.png)

2) Для классов setosa и virginica: 

![alt text](https://github.com/elena111111/R/blob/master/sg/sg_set-virg.png)

3) Для классов versicolor и virginica: 

![alt text](https://github.com/elena111111/R/blob/master/sg/sg_vers-virg.png)