n = 5
p = 6
X = matrix(rnorm(n * p), n, p)
# r = rank(X)

# calcul de X
# generer a et b
# on donne ak et bk

XX = X
ndim = 5

A = matrix(0, ncol(XX), ndim)
B = matrix(0, nrow(XX), ndim)
S = rep(0, ndim)

U = matrix(0, ncol(XX), ndim)
V = matrix(0, nrow(XX), ndim)

for (k in 1:ndim) 
{
  a = rnorm(ncol(XX))
  b = rnorm(nrow(XX))
  # calculer omega
  omega = t(b) %*% XX %*% a
  omega = as.numeric(1 / omega)
  A[,k] = a
  B[,k] = b
  S[k] = omega
  # deflation
  XX = XX - omega * XX %*% a %*% t(b) %*% XX
  if (k > 1) {
    dd = diag(t(V[,1:k-1]) %*% X %*% A[,1:k-1])
    Ones = ones(1, k - 1)
    U[,k] = A[,k] - U[,1:k-1] %*% diag(t(V[,1:k-1]) %*% X %*% A[,k] %*% Ones /dd)
    dd = diag(t(B[,1:k-1]) %*% X %*% U[,1:k-1])
    V[,k] = B[,k] - V[,1:k-1] %*% (diag(t(B[,k] %*% ones(1,k-1)) %*% X %*% U[,1:k-1])./dd)
  } else {
    U[,k] = a
    V[,k] = b
  }
}
