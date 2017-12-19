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

```
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

```
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

```
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
```
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
```
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
```
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
```
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

![alt text](https://github.com/elena111111/R/blob/master/pf/pf_gauss_015_center.png)

Черным показаны центры окон с ненулевым потенциалом (он получился в них равен 1).

## Алгоритм STOLP
 
Выполняет сжатие исходных данных.
Позволяет отобрать из выборки множество опорных объектов, и обучаться на этом множестве.



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
Эмпирическую оценкук плотности будем находить так:\

<a href="https://www.codecogs.com/eqnedit.php?latex=\hat{p}_h(x)&space;=&space;\frac{1}{m}&space;\sum_{i&space;=&space;1}^{m}&space;\prod_{j&space;=&space;1}^{n}&space;\frac{1}{h_j}&space;K&space;\left(&space;\frac{f_j(x)&space;-&space;f_j(x_i)))}{h_j}&space;\right&space;)," target="_blank"><img src="https://latex.codecogs.com/gif.latex?\hat{p}_h(x)&space;=&space;\frac{1}{m}&space;\sum_{i&space;=&space;1}^{m}&space;\prod_{j&space;=&space;1}^{n}&space;\frac{1}{h_j}&space;K&space;\left(&space;\frac{f_j(x)&space;-&space;f_j(x_i)))}{h_j}&space;\right&space;)," title="\hat{p}_h(x) = \frac{1}{m} \sum_{i = 1}^{m} \prod_{j = 1}^{n} \frac{1}{h_j} K \left( \frac{f_j(x) - f_j(x_i)))}{h_j} \right )," /></a>

где *x* - классифицируемая точка, *x_i* - точка из выборки, *h* - ширина окна, *m* - количество точек, *n* - количество признаков, *K* - функция ядра, *f_j* - значение признака.

Реализация:
```
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
