# Инструкция по установке и получению лицензии на использование академической версии Gurobi:
# https://www.gurobi.com/academia/academic-program-and-licenses/
# Необходимо зарегистрироваться на сайте, использую маёвскую почту:
# https://mail.mai.education/
# Получение пароля от почты на сайте МАИ:
# https://mai.ru/getpass/


# Программа на языке R для решения ЗЛП (c, x) -> max при ограничения Ax <= b с использованием решателя Gurobi (требуется установка пакета)
# x_1 + 3 x_2 -> max
# x_1 + 4 x_2 <= 60.3
# 3 x_1 + x_2 <= 5
# x_1 >= -11
# x_1 произвольного знака
# x_2 >= 0


library(Matrix)
library(gurobi)

model = list()

A = matrix(c(1, 4, 3, 1, 1, 0), nrow=3, byrow=T) # матрица системы ограничений
m = nrow(A) # число строк матрицы A
n = ncol(A) # число столбцов матрицы A


model$A          = A # задаём матрицу системы
model$obj        = c(1,3) # задаём целевую функцию
model$modelsense = 'max' # или 'min'
model$rhs        = c(60.3,5, -11) # вектор правой части ограничений
model$sense      = c('<', '<', '>') # знаки неравенств (по умолчанию '<')
#model$sense = matrix('>', 1, m) # сделать все ограничения типа '>'
model$vtype = 'C' # действительные переменные (по умолчанию), 'I' - целочисленные, 'B' - бинарные
model$lb = c(-Inf, 0) # нижние границы переменных (по умолчанию 0)

params = list()
params$outputflag = 0

result = gurobi(model, params)

print('objective value') 
print(result$objval) # выведем оптимальное значение целевой функции
print('x=')
print(result$x) # выведем решение задачи
print('y=')
print(result$pi) # выведем решение двойственной задачи


print('------------------------------------')

#теперь сделаем первую переменную действительной, а вторую - целочисленной
model$vtype =  c('C', 'I')


result = gurobi(model, params)
print('objective value') 
print(result$objval) # выведем оптимальное значение целевой функции во второй задаче
print('x=')
print(result$x) # выведем решение второй задачи задачи во второй задаче

rm(list=ls()) # освободим память



