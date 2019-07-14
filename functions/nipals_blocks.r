#' @title Blockification
#' @description Separates a data set into a list of blocks
#' @param dataset matrix or data frame containing the data
#' @param block_list list of vectors indicating which columns of the 
#' dataset form the blocks
#' @return a list of blocks
#' @export
#' @keywords internal
blockification <- function(dataset, block_list)
{
  num_blocks = length(block_list)
  Blocks = vector("list", num_blocks)
  for (b in 1L:num_blocks) {
    Blocks[[b]] = dataset[,block_list[[b]]]
  }
  # output
  Blocks
}


#' @title Convergence
#' @description Determined whether two vectors are similar enough
#' @param v_old old vector
#' @param v_new new vector
#' @param threshold positive decimal indicating the tolerance threshold
#' @return whether the given vectors have converged
#' @export
#' @keywords internal
convergence <- function(v_old, v_new, threshold)
{
  v_diff = v_new - v_old
  norm_diff = vnorm(v_diff)
  if (norm_diff < threshold) TRUE else FALSE
}


#' @title Initial Super Score
#' @description Computes a starting value for a super score
#' @param initial optional value for the initial super score 
#' If \code{NULL} then a vector of ones is used. If \code{NA} then a 
#' random normal vector is used
#' @param num_obs number of observations
initial_super_score <- function(value, num_obs)
{
  if (is.null(value)) {
    # vector of ones
    unit_score = rep(1, num_obs)
    super_score = normalize(unit_score)
  } else {
    if (is.na(value)) {
      # random normal vector
      random_score = rnorm(num_obs)
      super_score = normalize(random_score) 
    } else {
      # user provided vector
      if (length(value) != num_obs)
        stop("\n'initial' values don't match number of rows in Data")
      super_score = normalize(value)      
    }
  }
  # output
  super_score
}


#' @title Deflation
#' @description Deflation of scores
#' @param Blocks list of blocks
#' @param super_score vector of super score
#' @export
#' @keywords internal
deflation <- function(Blocks, super_score)
{
  # deflate on weights
  weights = lapply(Blocks, crossprod, super_score)
  TP = lapply(weights, function(wg, supsco) supsco %*% t(wg), super_score)
  residual = mapply("-", Blocks, TP, SIMPLIFY = FALSE)
  # output
  residual
}


#' @param Data a numeric matrix or data frame
#' @param blocks a list of vectors indicating which columns of the 
#' \code{Data} form the blocks
#' @param method string indicating the desired method. Options are: 
#' \code{"cpca", "cpcaw", "hpca", "hpcaw"}
#' @param initial optional value for the initial super score. 
#' If \code{NULL} then a vector of ones is used. If \code{NA} then a 
#' random normal vector is used. If a vector is provided, then its length
#' must be equal to the number of rows in \code{Data}
#' @param tol threshold tolerance for convergence
#' @export
master_block <- 
function(Data, blocks, method = "cpca", initial = NULL, tol = 0.0001)
{
  # blockification
  Blocks = blockification(Data, blocks)
  
  # starting super score
  old_super_score = initial_super_score(initial, nrow(Data))
  
  # iterative loop
  iters = 1
  repeat
  {
    # block weights (in a list)
    weights = lapply(Blocks, crossprod, old_super_score)
    if (method == "cpcaw") {
      weights = lapply(weights, normalize)
    }
    
    # block scores (in a matrix)
    Scores = mapply("%*%", Blocks, weights)
    if (method == "cpca") {
      aux = unlist(lapply(weights, function(x) sum(x * x)))
      Scores = sweep(Scores, 2, aux, FUN = "/")
    }
    if (method == "hpcaw") {
      Scores = apply(Scores, 2, normalize)
    }
    
    # super weights and new super score
    super_weights = t(Scores) %*% old_super_score
    new_super_score = as.vector(Scores %*% super_weights)
    super_weights = super_weights / vnorm(new_super_score)
    new_super_score = normalize(new_super_score)
    
    # check convergence
    reached = convergence(old_super_score, new_super_score, tol)
    if (reached) break
    # otherwise update super score
    old_super_score = new_super_score
    iters = iters + 1
  } # end repeat
  
  # output
  list(iterations = iters,
       super_score = new_super_score,
       super_weights = super_weights,
       block_scores = Scores,
       block_weights = weights)
}


#library(matrixkit)
#n = 10
#p = 6
#set.seed(5)
#datos = matrix(rnorm(n*p), n, p)
#bloques = list(1:2, 3:4, 5:6)
#hey = master_block(datos, bloques, "cpcaw", NA)
#hey




cpca <- 
function(Data, blocks, tol = 0.0001)
{
  # blockification
  Blocks = blockification(Data, blocks)
  
  # starting super score
  old_super_score = rep(1, nrow(Data))
  
  # iterative loop
  iters = 1
  repeat
  {
    # block weights (in a list)
    weights = lapply(Blocks, crossprod, old_super_score)
    weights = lapply(weights, function(x, u) x / sum(u*u), old_super_score)
    weights = lapply(weights, normalize)
    
    # block scores (in a matrix)
    Scores = mapply("%*%", Blocks, weights)
    
    # super weights and new super score
    tmp = sum(old_super_score * old_super_score)
    super_weights = (t(Scores) %*% old_super_score) / tmp
    super_weights = normalize(super_weights)
    new_super_score = as.vector(Scores %*% super_weights)
    
    # check convergence
    reached = convergence(old_super_score, new_super_score, tol)
    if (reached) break
    # otherwise update super score
    old_super_score = new_super_score
    iters = iters + 1
  } # end repeat
  
  # output
  list(iterations = iters,
       super_score = new_super_score,
       super_weights = super_weights,
       block_scores = Scores,
       block_weights = weights)
}

n = 10
p = 6
set.seed(5)
datos = matrix(rnorm(n*p), n, p)
bloques = list(1:2, 3:4, 5:6)
mod1 = master_block(datos, bloques, "cpcaw", NA)
mod2 = cpca(datos, bloques)

mod1$super_score
mod2$super_score

vnorm(mod1$super_score)
vnorm(mod2$super_score)

mod1$super_weights
mod2$super_weights

vnorm(mod1$super_weights)
vnorm(mod2$super_weights)

mod1$iterations
mod2$iterations

mod1$block_weights
mod2$block_weights

cbind(mod1$block_scores, mod2$block_scores)
