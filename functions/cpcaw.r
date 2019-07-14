
n = 10
p = 6

Y = matrix(rnorm(n*p), n, p)
X = list(Y[,1:2], Y[,3:4], Y[,5:6])
a = list(c(1, 2), c(3, 4), c(5, 6))
tb = mapply("%*%", X, a)
X[[1]] %*% a[[1]]


global = runif(n)

b = lapply(Y, crossprod, global)
b[[1]]
t(Y[[1]]) %*% global



#' @param Data matrix or data frame containing the data
#' @param blocks list of vectors indicating the columns forming the blocks
#' @return a list of blocks
blockification <- function(Data, blocks)
{
  num_blocks = length(blocks)
  Blocks = vector("list", num_blocks)
  for (b in 1L:num_blocks) {
    Blocks[[b]] = Data[,blocks[[b]]]
  }
  # output
  Blocks
}


#' @param x vector to be normalized
vnorm <- function(x) {
  sqrt(sum(x * x))
}

#' @param x vector to be normalized
normalize <- function(x) {
  x / vnorm(x)
}


#' @param Scores matrix of block scores
#' @param super_score vector of super (global) score
norm_super_score <- function(Scores, super_score)
{
  super_weights = t(Scores) %*% super_score
  super_score = Scores %*% super_weights
  super_weights = super_weights / vnorm(super_score)
  super_score = normalize(super_score)
  # output
  list(
    super_score = as.vector(super_score),
    weights = super_weights
  )
}


convergence <- function(old_one, new_one, tolerance) {
  difference = new_one - old_one
  norm_diff = vnorm(difference)
  if (norm_diff < tolerance) TRUE else FALSE
}


cpcaw <- function(data, blocks, super_score = NULL, tol = 0.0001)
{    
  # blockification
  Blocks = blockification(data, blocks)

  # starting super score
  if (is.null(super_score)) {
    arbitrary_score = runif(nrow(data))
    super_score = normalize(arbitrary_score)
  } else {
    super_score = normalize(super_score)
  }
  old_super_score = super_score

  # iterative loop
  iters = 1
  repeat 
  {
    weights = lapply(Blocks, crossprod, old_super_score)
    weights = lapply(weights, normalize)
    scores = mapply("%*%", Blocks, weights)
    # apply common operations to get super score
    common_ops = norm_super_score(scores, old_super_score)
    new_super_score = common_ops$super_score
    # check convergence
    reached = convergence(old_super_score, new_super_score, tol)
    if (reached) break
    # otherwise
    old_super_score = new_super_score
    iters = iters + 1
  }
  
  # output
  list(
    iterations = iters,
    super_score = new_super_score,
    super_weights = common_ops$weights,
    block_scores = scores,
    block_weights = weights
  )
}

# ---------------------------------------
n = 10
p = 6
set.seed(5)
datos = matrix(rnorm(n*p), n, p)
bloques = list(c(1, 2), c(3, 4), c(5, 6))
# ---------------------------------------

res = cpcaw(datos, bloques)
res




# adapted version of CPCA-W using the data superblock
# instead of the table of block-scores
xpcaw <- function(data, blocks, super_score = NULL, tol = 0.0001)
{    
  # blockification
  Blocks = blockification(data, blocks)
  
  # starting super score
  #  if (is.null(super_score)) {
  #    arbitrary_score = runif(nrow(data))
  #    super_score = normalize(arbitrary_score)
  #  } else {
  #    super_score = normalize(super_score)
  #  }
  #  old_super_score = super_score
  
  # arbitrary initial weights
  weights = lapply(blocks, function(x) rep(1, length(x)))
  weights = lapply(weights, normalize)
  # initial block-scores and superscore
  scores = mapply("%*%", Blocks, weights)
  super_score = data %*% rep(1, ncol(data))
  old_super_score = normalize(super_score)
  # inner estimation
  inner_superscore = rowSums(scores)
  weights = lapply(Blocks, crossprod, old_super_score)
  weights = lapply(weights, normalize)
  #super_weights = t(data) %*% inner_superscore
  super_weights = t(data) %*% old_super_score
  
  # outer estimation
  scores = mapply("%*%", Blocks, weights)
  super_score = data %*% super_weights
  old_super_score = normalize(super_score)
  
  # iterative loop
  iters = 2
  repeat 
  {
    weights = lapply(Blocks, crossprod, old_super_score)
    weights = lapply(weights, normalize)
    scores = mapply("%*%", Blocks, weights)
    # apply common operations to get super score
    # common_ops = norm_super_score(scores, old_super_score)
    inner_superscore = rowSums(scores)
    # super_weights = t(data) %*% inner_superscore
    super_weights = t(data) %*% old_super_score
    super_score = data %*% super_weights
    new_super_score = normalize(super_score)
    # check convergence
    reached = convergence(old_super_score, new_super_score, tol)
    if (reached) break
    # otherwise
    old_super_score = new_super_score
    iters = iters + 1
  }
  
  # output
  list(
    iterations = iters,
    super_score = new_super_score,
    super_weights = common_ops$weights,
    block_scores = scores,
    block_weights = weights
  )
}


