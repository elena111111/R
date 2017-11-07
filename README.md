# R
## knn - ����� k ��������� �������.
���� ������� ������ ������(150 ���������), � ��� 3 ������(setosa, versicolor, virginica). 
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