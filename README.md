# ����������� ��������� �������������

��� ����� ���������� ������ ����������� �������� ������������: ������ �������� ������������� ������ ������.
�������� ���� �������� �������� (� ����� ������ ��������� ����������), ������� ����������, ��������� ��� ������� ������. 

## knn - ����� k ��������� �������.
���� ������� ������ ������(150 ���������), � ��� 3 ������(*setosa*, *versicolor*, *virginica*). 
�� ����� ���������������� ��������� ����� *{z}* �� 2� ��������� (Petal.Length, Petal.Width). ��� �� �������� - ���������� ����� �� �������.
������ *xl* ������ ���������� ����� �������� ������� � �������� ������, � �������� ��������� ��������������� �����.

��� ������ ����� *z* �������� ��������������� ������ *distances*, � ������� ����� ������� ���������� �� ����� *z* �� ������ ����� �� �������.
������ *orderedxl* - ��� ������ *xl*, ��������������� ���, ����� ����� ��� � ������� ���������� ���������� �� *z* �� ����� �� �������.
������ ������ �� *orderedxl* ������ ������ *k* ����� (������� *k* ��������� �������) � � ���� ������� ������� ������ ������� �������� ������� (������ *classes*).
����� *z* ��������� � ���� ������, ������� ���� ����� ����������� � *classes*.

����������� *k* ����������� �� *LOO* (���������� ��������), ������� �������� ��������� �������: 
����� ���������� *Q = 0*.
�� �������(*X*) ����� ��������� �� ����� ����� (����� ����� ����� *zi*) - ������� ������� *X2*. 
������ �������� ��������(*knn*), ��� ����� �� ����� ���������������� ����� *zi*, � ����� ������� *X2*.
���� �������� ������, �� � �������� *Q* �������� 1.
����� �� ����� ������� �������� ��� ����� �������, �������� *Loo = Q/l*, ��� *l* - ���������� ����� � ������� *X*.

����� ������ ��� ��� ������ *k* �� 1 �� *l*.
����������� ����� *k*, ��� ������� �������� *Loo* ����������.

��������� ������ ���������: 

![alt text](https://github.com/elena111111/R/blob/master/knn/knn.png)

����������� *Loo* �� *k*:

![alt text](https://github.com/elena111111/R/blob/master/knn/knn_loo.png)


## �������� k ���������� ��������� ������� (wknn).
������� ������� *X* (����� ������), � 3 ������ (*setosa*, *versicolor*, *virginica*). 
�� ����� ���������������� ��������� ����� *{z}*.
������������� �������� �� ���� ��������� (Petal.Length, Petal.Width), ��� �� �������� ������������ �����.

*distances* - ��� ������ ���������� �� ����� *z* �� ������ ����� �� ������� *X*.
*orderedxl* - ��� ������ *xl*, ��������������� �� ����������� ���������� �� ����� *z* �� ������ ����� �� ������� *X*.
�� ������� ������� ������� *q^i*.
��� *k* ��������� ������� �������� ��������������� ����.
� ������� *classes* ����� *k* �����, � � �������� �������� ������ � ��� �����. 
�������� ����� ����� ����� ��� ������� ������ �������� � *classes* (��� ������ *ans*).
������� ����� ��� �����, ��� �������� ���������� � *ans*.

�������� *q* ����������� �� *LOO*, ������� �������� ��������� �������:
����� ���������� *Q = 0*.
�� �������(*X*) ����� ��������� �� ����� ����� (����� ����� ����� *zi*) - ������� ������� *X2*. 
������ �������� ��������(*wknn*), ��� ����� �� ����� ���������������� ����� *zi*, � ����� ������� *X2*.
���� �������� ������, �� � �������� *Q* �������� 1.
����� �� ����� ������� �������� ��� ����� �������, �������� *Loo = Q/l*, ��� *l* - ���������� ����� � ������� *X*.

����� ������ ��� ��� ������ *q*, ������� ����� ���������� � ��������� *(0; 1)*.
����������� ����� *q*, ��� ������� �������� *Loo* ����������.

��������� ������ ���������: 

![alt text](https://github.com/elena111111/R/blob/master/wknn/wknn.png)

����������� *Loo* �� *q*:

![alt text](https://github.com/elena111111/R/blob/master/wknn/wknn_loo_q.png)

## �������� ������������� ���� (pw).
������� ������� *X* (����� ������), � 3 ������ (*setosa*, *versicolor*, *virginica*). 
�� ����� ���������������� ��������� ����� *{z}*.
������������� �������� �� ���� ��������� (Petal.Length, Petal.Width), ��� �� �������� ������������ �����.

*distances_weighed[ , 1]* - ��� ���������� �� ����� *z* �� ������ ����� �� ������� *X*.
*distances_weighed[ , 2]* - �������� ������� ������� ��� ������ ����� �� �������.
������� ������� ������� �� ���������� �� ����� *z* �� ����� �� �������, � ����� �� ������ ��������� *h* (������ ����) � ���� K, ������� ����������� �� LOO (������� ������ ��� ��� ������ � ��������� ����������).
��������: �� ������ ������ ����� *z*(����� ����) ����������� ������� *h*, � �������, ��������� ��� ������ ������ � ���� ����������� ������.
��� ������� ���� ���������� ���������� �������� ������������ *Loo*. ��� ��� �� ���������, ��� ����� ���� ����� ������ �� �������������.
� ������� *classes* �������� ���������� �� *z* �� ����� �� *X*, ���� ����� �� *X* � �������� ������ ������ ����� �� *X*.
��������� ���� ������� ������ (������� *ans*).
������� ����� �����, ��� �������� ��������� ��� ���������� � *ans*.

����� ��������, ��� ������ �������� ����� �������� ��� ������������ ������������� ������� (� ���� ���������� ������ �������� ����� ������ ���������� ��������). 
� ���� ������ ����� ������������ �������� ������������� ���� � ���������� ������� ���� (������ ����).

��������� ������ ��������� / ����������� *loo* �� *h* (��� ������ ����):

1) ������������� ����:

![alt text](https://github.com/elena111111/R/blob/master/pw/pw_core_rect_and_loo.png)

2) ����������� ����:

![alt text](https://github.com/elena111111/R/blob/master/pw/pw_core_triang_and_loo.png)

3) ���� ������������:

![alt text](https://github.com/elena111111/R/blob/master/pw/pw_core_epan_and_loo.png)

4) ������������ ����:

![alt text](https://github.com/elena111111/R/blob/master/pw/pw_core_quart_and_loo.png)

5) ����������� ����: 

![alt text](https://github.com/elena111111/R/blob/master/pw/pw_core_gauss_and_loo.png)

## �������� ������������� ���� � ���������� ������� ���� (varpw).
������� ������� *X* (����� ������), � 3 ������ (*setosa*, *versicolor*, *virginica*). 
�� ����� ���������������� ��������� ����� *{z}*.
������������� �������� �� ���� ��������� (Petal.Length, Petal.Width), ��� �� �������� ������������ �����.
*distances* - ��� ������ ���������� �� ����� *z* �� ������ ����� �� ������� *X*.
*orderedxl* - ��� ������ *xl*, ��������������� �� ����������� ���������� �� ����� *z* �� ������ ����� �� ������� *X*.
*distances_s* - ��������������� ������ ����������.
*weights* - ������ ����� ��� *k* ��������� �������.
��� ������� ������� ��� ����� ������� ����(� ������� ������ ��������� ������������ �������������).
�������� ���� - ���������� �� *z* �� *i*-�� ������ ����� *z* (*i* �� *1* �� *k*), ������� �� ���������� �� *z* �� (*k+1*)-�� ������.

������� �� *orderedxl* ������ *k* �����, ������� � ��� ��������������� ���� � ������� ������ ������� �������� ������� � ����� - ������� ������ *classes*.
������� ����� ��� �����, ��������� ��� �������� ���������� � *classes*. 

������ ����� �������� ��� ������������ �������������� �������.

������ ������ ���������:

![alt text](https://github.com/elena111111/R/blob/master/varpw/varpw_core_rect.png)

## ����� ������������� ������� 
������� ������� *X* (����� ������), � 3 ������ (*setosa*, *versicolor*, *virginica*). 
�� ����� ���������������� ��������� ����� *{z}*.
������������� �������� �� ���� ��������� (Petal.Length, Petal.Width), ��� �� �������� ������������ �����.

� ������ ������������� ���� �� �������� ����� ���� � ���������������� ������, � ������ �������� ����������� ������ ��������� ��������. 
������ ������ ����������� ����� ����� ���� ��������� - ������, ������������, ��������� ������ ��� ���� ������ �� ������ ��������. �� ���� ���� ��� ���������� ������ ���� *h* ��� ������� ����� ���� ������.

������ �� �������� ����� *gamma*, � ���� �������� ���������� ���������: 
*g[dim(iris)[1]]* - ��� ������ �����������, ���������� ����������� ������. 

��������:
��������������� ����� *loo* ��������� �������� pf � �������� ����������� *g, h, K*(������� ����) ��� ������� *X*, ��������� �� *X* �� ����� �����, 
��������� �������� *pf* �� ���������� �������� � ���������� ��������� ������ ��������� � ��������� �����������. ���� ������ �� �������, �� �������� ������ ����������� �� 1. 
��������� �������� ������ ����� �� ����������� ������� *X*. ����� ������ ����� �� 0 �� 1.
 
*eps* - ��� ����������� ���������� ����� ������ (�� 0 �� 1).
���� ��������� ������ *loo > eps*, ������ ���� �������� ������ *g*: 
������� ��������� ������� ������� *xi* �� *X*.
���� �������� *pf(X, xi, g, K, h)* ���������, ������ � ����� *xi* ����� ��������� ��������� �� 1.

���� *loo* � �������� ����������� ������ ����� *<= eps*, ������ �� �������� ���������� ���� ������, � *g* ������ �������� �� �����. ������ ������ *g*.

������ ���� *h* ����� ��������� ������������� ��� ������� �������, �� � ��������� �� �����������, ��� ������ (����������). 
� ������ ������ ��� �������� ����� �� ������ ������������� ����, � ��������� ��� ���� ����.

����� *pf* �������� ���������� � ������� ������������� ����, ������ �������� ������� ���� ��� ����������� �� *g*:
*z* - ���������������� �����.
*distances_weighed[ , 1]* - ��� ���������� �� ����� *z* �� ������ ����� �� ������� *X*.
*distances_weighed[ , 2]* - �������� ������� ������� ��� ������ ����� �� ������� *X*.
*classes* - ��� *distances_weighed*, � �������� �������� ������� ��������������� �������� �������. 
������� ��� *z* ����� ��� �����, �������� ��� �������� ���������� � *classes*.

������ ������ ���������(��� ������������ ����):

![alt text](https://github.com/elena111111/R/blob/master/pf/pf_gauss_015_center.png)

������ �������� ������ ���� � ��������� ����������� (�� ��������� � ��� ����� 1).

## �������� STOLP
 



 
