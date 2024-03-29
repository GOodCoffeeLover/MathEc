N <- 8
k <- 20

K_ <- 10 + k%/%4
L_ <- 18 - k%/%5

A1 <- 8 + k%/%4
A3 <- 3/2 + (k+1)%/%4
A5 <- 15 - k%/%5
A6 <- 17 - k%/%3 
A7 <- 5 + k - k%/%2
A8 <- 7 + k/2 - k%/%2
A9 <- (log(3*k, exp(1)))%/%1

a <- 4 + k/2 - k%/%2
b <- 7 - k%/%6

cat("N = ", N, '\n')
cat("k = ", k, '\n')

cat("K = ", K_, '\n')
cat("L = ", L_, '\n')

cat("A1 = ", A1, '\n')
cat("A3 = ", A3, '\n')
cat("A5 = ", A5, '\n')
cat("A6 = ", A6, '\n')
cat("A7 = ", A7, '\n')
cat("A8 = ", A8, '\n')
cat("A9 = ", A9, '\n')

cat("a = ", a, '\n')
cat("b = ", b, '\n')

F1 <- function(K, L){
  A1*K^0.3*L^0.6
}

F2 <- function(K, L){
  a*K + b*L
}

F3 <- function(K, L){
  A3*min(a*K, b*L)
}

F4 <- function(K, L){
  34/3*sqrt(K) + 15*sqrt(L)
}

F5 <- function(K, L){
  res<-0
  if(K>0 && L>0){
    res = A5*(1/(3*(K)^3) + 2/(3*(L)^3))^(-1/4)
  }else{
    res = 0
  }
  res
}

F6 <- function(K, L){
  A6*log((K+1)*(2*L+1), exp(1))
}

F7 <- function(K, L){
  A7*K^0.4*L^0.6
}

F8 <- function(K, L){
  A8*K^0.7*L^0.3
}

F9 <- function(K, L){
  A9*min(a*K, b*L)
}




FF <- c(F1, F2, F3, F4, F5, F6, F7, F8, F9)

B <- array(-1, c(K_+1, L_+1, N+1))
U <- array(-1, c(K_+1, L_+1, N+1, 2))

for(k_ in 0:K_){
  for(l_ in 0:L_){
    B[k_+1,l_+1, N+1] = 0
  }
}

for(n_ in N:1){
  for(k_ in 0:K_){
    for(l_ in 0:L_){
      for(kk in 0:(K_-k_)){
        for(ll in 0:(L_-l_)){
          if((FF[[n_]](kk, ll))%/%1 + B[ k_+1+kk , l_+1+ll, n_+1] > B[k_+1,l_+1, n_]){
            B[k_+1,l_+1, n_] =  (FF[[n_]](kk, ll))%/%1 + B[ k_+1+kk , l_+1+ll, n_+1]
            U[k_+1,l_+1, n_,] = c(kk,ll)
          }
        }
      }
    }
  }
}
#print(B[,,1])

cat("max sum (ans) = ", max(B[,,1]), '\n')


cur_sum = max(B[,,1])

cur_K <- 0
cur_L <- 0

state <- c(0,0)

for(n in 1:N){
  #print('==========================')
  #print(B[,,n])
  #print(U[,,n,]) 

  #print(B[,,n] == cur_sum)
  cur_state = which(B[,,n] == cur_sum, arr.ind = TRUE)[1,] 
  # L K
  #print(cur_state)
  cur_K = cur_state[1] - state[1]  #-1
  cur_L = cur_state[2] - state[2]  #-1

  #cat('cur_state = ', cur_state[1], cur_state[2], '\n')
  #cat('B[cur_state, n] = ', B[cur_state[1], cur_state[2], n], '\ncur_K = ',cur_K, '\ncur_L = ', cur_L, '\n')
  u <- U[cur_state[1],cur_state[2], n,]
  
  #cat('state = ', state, '\n')

  cur_sum = cur_sum - (FF[[n]](u[1], u[2]))%/%1
  #cat( 'u = ', u, '\ncur_sum = ', cur_sum,  '\n')
  cat('n=', n, 'K_n =', u[1], 'L_n =', u[2], '\n')
  state[1] = state[1] + u[1] +1
  state[2] = state[2] + u[2] +1
}