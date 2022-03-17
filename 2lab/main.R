library(Matrix)
library(gurobi)
# =========
#   p two
# =========

l <- 8
k <- 21
n <- 10 + k%/%4
#n <- 3
m <- 30 - k%/%4
#m <- 3

cat("l =", l, '\n')
cat("k =", k, '\n')
cat("n =", n, '\n')
cat("m =", m, '\n')

tmp <- 0


#A <- c(-4, 13, 4, 13, -4, 4, 10, 10, 8)
A <- c()
for (i in 1:m)
  for ( j in 1:n){
    tmp = -k + ( 1451*i + 1571*j + 2081*k + 2543*l  ) %% (30 + k%/%5) 
    A = append(A, tmp)
   }

A = matrix(A, ncol=n, byrow=T)


# sum (xi) -> min
# x*ai^t >=1
#


solve <- function(AA, n, m){
  cat("A :\n")
  print(AA)
  cat("========================================================\n\n")

  b <- matrix(1, 1, m)

  c <- matrix(1, 1, n)

  max_min = min(AA[1,])
  max_i = 1
  for (i in 2:m){
    if (min(AA[i,]) > max_min){
      max_min = min(AA[i,])
      max_i = i
    }
  }

  min_max = max(AA[,1])
  min_j = 1
  for (j in 2:n){
    if (max(AA[,j]) < min_max){
      min_max = max(AA[,j])
      min_j = j
    }
  }


  cat("min_max (win for the first) = ", min_max, "\nmax_min (win for the second) = ", -max_min, '\n\n\n')

  cat('garantiruyuchaia dlya pervogo',max_i,'\n')

  cat('garantiruyuchaia dlya vtorogo', min_j, '\n\n\n')

  cat('solution in clear strategies ')
  if (min_max == max_min){
    cat('exists\n\n\n')
  }else{
    cat('does not exists\n\n\n')
  }

  model=list()
  model$A = t(AA) - min(AA) + 1
  model$obj = b
  model$modelsense = 'min'
  model$rhs = c
  model$sense = matrix('>=', 1, n)
  model$vtype = 'C'

  params = list()
  params$outputflag = 0

  result = gurobi(model, params)
  print('x=')
  print(result$x) # выведем решение задачи
  print('y=')
  print(result$pi) # выведем решение двойственной задачи
  game_price = ((t(result$x)%*%AA)%*%result$pi)
  cat('game pirce is ', game_price, '\n')

  cat("========================================================\n\n")
}

solve(A, n, m)
solve(-A, n, m)
solve(t(A)%*%A, n, n)



rm(list=ls())