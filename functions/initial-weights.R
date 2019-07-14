# weights = "ones" (i.e. all ones)
# weights = "random" (randomly generated)
# weights = "column1" (first column of each block)
# weights = list(1:3, 4:5, 6:10) (i.e. provided by user)
initial_weights <- function(ws, blocks)
{
  if (is.list(ws)) {
    return(ws)
  } else {
    start_weights <- switch(
      ws,
      "ones" = ones_weights(blocks),
      "random" = random_weights(blocks),
      "column1" = column1_weights(blocks)
    )    
  }
  # output
  start_weights
}

#' initial weights randomly generated
ones_weights <- function(blocks) {
  lapply(blocks, function(x) rep(1L, length(x)))
}

#' initial weights randomly generated
random_weights <- function(blocks) {
  lapply(blocks, function(x) runif(length(x)))
}

#' initial weights of first column in each block
column1_weights <- function(blocks) {
  lapply(blocks, function(x) c(1L, rep(0, length(x) - 1)))
}

