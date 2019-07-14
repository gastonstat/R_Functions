
MaxBet <- function(Data, blocks = NULL, num_dim = 1)
{

  # blockification
  num_blocks = length(blocks)
  X = vector("list", num_blocks)
  for (b in 1L:num_blocks) {
    X[[b]] = Data[,blocks[[b]]]
  }
      
  # list of vectors (loadings)
  u = vector("list", num_blocks)
  for (k in 1L:num_blocks) {
    u[[k]] = normalize(rep(1, length(blocks[[k]])))
  }
  
  # for each dimension
  for (i in 1L:num_dim)
  {
    # iterative procedure
    iters = 1
    repeat 
    {
      # block scores (in a matrix)
      Z = mapply("%*%", X, u)   # Xk %*% uk
      # block weights (in a list)
      v = lapply(X, function(x, z) rowSums(crossprod(x, z)), Z) 
      # normalized block weights (in a list)
      v = lapply(v, function(x) normalize(x))
      # check convergence
      uv_diff = unlist(v) - unlist(u)
      if (sqrt(sum(uv_diff^2)) < 0.0001) break
      iters = iters + 1
      # update vectors
      u = v
    }
  }
  list(u, iters)
}


n = 10000
p = 6
set.seed(5)
Data = matrix(rnorm(n*p), n, p)
blocks = list(1:2, 3:4, 5:6)
num_dim = 1

system.time(MaxBet(Data, blocks))







maxhan <- function(X, blocks = NULL, num_dim = 1)
{
  # blockification
  num_blocks = length(blocks)
  num_vars = ncol(X)
  indices = indexify(blocks)
  # list of vectors (loadings)
  u = rep(1, num_vars)
  for (k in 1L:num_blocks) {
    u[indices == k] = normalize(u[indices == k])
  }
  v = u
  
  XX = t(X) %*% X
  # for each dimension
  for (i in 1L:num_dim)
  {
    # iterative procedure
    iters = 1
    repeat 
    {
      # block scores (in a matrix)
      v = as.numeric(XX %*% u)
      # normalize weights
      for (k in 1L:num_blocks) {
        v[indices == k] = normalize(v[indices == k])
      }
      # check convergence
      uv_diff = v - u
      if (sqrt(sum(uv_diff^2)) < 0.0001) break
      iters = iters + 1
      # update vectors
      u = v
    }
  }
  list(u, iters)
}


n = 10
p = 6
set.seed(5)
Data = matrix(rnorm(n*p), n, p)
blocks = list(1:2, 3:4, 5:6)
num_dim = 1

system.time(maxhan(Data, blocks))
