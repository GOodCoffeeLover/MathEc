library(Matrix)
library(gurobi)
library(ramify) # для argmax
# =========
#   p two
# =========

l <- 8
k <- 21
n <- 10 + k%/%4
#n <- 2
m <- 30 - k%/%4
#m <- 3

cat("l =", l, '\n')
cat("k =", k, '\n')
cat("n =", n, '\n')
cat("m =", m, '\n')

tmp <- 0

b <- c()

for ( i in 1:m){
  tmp = 70 + l + k + i
  b = append(b, tmp)
}
cat("b :", b, "\n")

c <- c()

for ( i in 1:n){
  tmp = 50 + l + k - i
  c = append(c, tmp)
}
cat("c :", c, "\n")

A <- c()
for (i in 1:m)
  for ( j in 1:n){
    tmp = 1 + ( (j+k)*i + j*j + i*i*i + 3*(i+l) ) %% (30 + k%/%5) 
    #tmp = (i-1)*n + j
    A = append(A, tmp)
  }
A = matrix(A, ncol=n, byrow=T)
cat("A :\n")
print(A)
cat("========================================================\n\n")

# =========
#  p three
# =========

model=list()
model$A = A
model$obj = c
model$modelsense = 'max'
model$rhs = b
model$vtype = 'C'

params = list()
params$outputflag = 0

result = gurobi(model, params)
print('objective value') 
print(result$objval) # выведем оптимальное значение целевой функции
print('x=')
print(result$x) # выведем решение задачи
print('y=')
print(result$pi) # выведем решение двойственной задачи
cat("========================================================\n\n")

value1 = result$objval

# =========
#  p four
# =========

idx = argmax(matrix(result$pi), rows=F)
result$pi[idx]=0
idx2 = argmax(matrix(result$pi), rows=F)

cat("Best resource to increase is", idx, "th resource\n\n")

b[idx] = b[idx] + 1
model$rhs = b
b[idx] = b[idx] - 1


result = gurobi(model, params)
print('objective value') 
print(result$objval) # выведем оптимальное значение целевой функции
print('x=')
print(result$x) # выведем решение задачи
print('y=')
print(result$pi) # выведем решение двойственной задачи
cat("========================================================\n\n")

value2 = result$objval
# =========
#  p five
# =========

b[idx2] = b[idx2] + 1
model$rhs = b
b[idx2] = b[idx2] - 1

cat("change resource", idx2, "\n\n")

result = gurobi(model, params)
print('objective value') 
print(result$objval) # выведем оптимальное значение целевой функции
print('x=')
print(result$x) # выведем решение задачи
print('y=')
print(result$pi) # выведем решение двойственной задачи

value3 = result$objval

cat("previous value", value2, "better then", value3, "\n\n")
cat("========================================================\n\n")

# =========
#  p six
# =========

model$rhs = b
model$vtype = 'I'

result = gurobi(model, params)
print('objective value') 
print(result$objval) # выведем оптимальное значение целевой функции
print('x=')
print(result$x) # выведем решение задачи
cat("========================================================\n\n")

value4 = result$objval

# =========
#  p seven
# =========

vtypes = c()

for ( i in 1:(n%/%2) ){
  vtypes = append(vtypes,'I')
}

for ( i in (n%/%2 + 1):n ){
  vtypes = append(vtypes,'C')
}


model$vtype = vtypes

result = gurobi(model, params)
print('objective value') 
print(result$objval) # выведем оптимальное значение целевой функции
print('x=')
print(result$x) # выведем решение задачи
cat("========================================================\n\n")

value5 = result$objval

# =========
#  p eight
# =========

cat("\n\nvalue \\w all integers", value4, "worse then", value5, "\n\n")

rm(list=ls())