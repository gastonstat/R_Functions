
n = 5

X_blocks = list(
  X1 = matrix(rnorm(n * 3), n, 3),
  X2 = matrix(rnorm(n * 4), n, 4),
  X3 = matrix(rnorm(n * 2), n, 2))

X_blocks


RV_matrix <- function(x) 
{
  n = length(x)
  RVS = matrix(1, n, n)
  for (j in 1L:(n-1)) {
    for (i in (j+1):n) {
      RV_ij = RV(x[[i]], x[[j]])
      RVS[i,j] = RV_ij
      RVS[j,i] = RV_ij
    }
  }
  RVS
}

RV_matrix(X_blocks)



lapply(X_blocks, scale, scale = FALSE)


scale_by_first_eigenvalue <- function(y)
{
  eig_vals = eigen_values(y)
  y / eig_vals[1]
}


X_mfa = lapply(X_blocks, scale_by_first_eigenvalue)
A1 = X_mfa[[1]]
sum(A1 %*% t(A1))

A2 = X_mfa[[2]]
sum(A2 %*% t(A2))
