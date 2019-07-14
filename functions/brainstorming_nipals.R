
library(igraph)

Ad <- rbind(
  c(0, 0, 0),
  c(1, 0, 0),
  c(1, 1, 0))
dimnames(Ad) <- list(
    c("A", "B", "C"),
    c("A", "B", "C"))

Ad

g <- graph.adjacency(t(Ad))
edge_list <- get.edgelist(g)
plot(g)


A <- matrix(rnorm(12), 4, 3)
colnames(A) <- c("a1", "a2", "a3")
B <- matrix(rnorm(8), 4, 2)
colnames(B) <- c("b1", "b2")
C <- matrix(rnorm(16), 4, 4)
colnames(C) <- c("c1", "c2", "c3", "c4")

X <- list(A, B, C)

lapply(ncols_per_block)


graph_diagram <- function(blocs, adjacency) {
  g <- graph.adjacency(t(adjacency))
  edge_list <- get.edgelist(g)
  ncols_per_block <- unlist(lapply(blocs, NCOL))
  num_mvs <- sum(ncols_per_block)
  tmp <- matrix("", num_mvs, 2)
  comp_names <- rownames(adjacency)
  tmp[ ,1] <- rep(comp_names, times = ncols_per_block)
  tmp[ ,2] <- unlist(lapply(blocs, colnames))
  rbind(edge_list, tmp)
}
edge <- graph_diagram(X, Ad)
gg <- graph.edgelist(edge, directed = FALSE)
V(gg)$color <- rep(c("tomato", "gray80"), times = c(3, 9))
V(gg)$shape <- rep(c("circle", "square"), times = c(3, 9))
V(gg)$size <- rep(c(32, 20), times = c(3, 9))
plot(gg)


unlist(lapply(X, NCOL))
rep(colnames(Ad), times = ncols_per_block)


same_num_obs <- function(x) {
  obs_per_block <- unlist(lapply(x, NROW))
  if (length(unique(obs_per_block)) == 1) TRUE else FALSE
}

blocks <- function(...) {
  blocs <- list(...)
  if (!same_num_obs(blocs)) {
    stop("\nobjects with different number of observations")
  }
  class(blocs) <- "blocks"
  blocs
}


X <- blocks(
  A = scale(matrix(rnorm(12), 4, 3), center = TRUE),
  B = scale(matrix(rnorm(8), 4, 2), center = TRUE)
)

blocks(
  A = scale(matrix(rnorm(12), 3, 4), center = TRUE),
  B = scale(matrix(rnorm(8), 4, 2), center = TRUE)
)





# blocks = list(1:3, 4:5, 6:10)
# blocks = list(c("uno", "dos", "tres"),
#               c("cuatro", "cinco"), 
#               c("seis", "siete", "ocho", "nueve", "diez"))





weights <- list(
  w1 = c(1, 2, 3),
  w2 = c(1, 1)
)



#' Merge blocks into one matrix
#' @param blocks list of block data
merge_datablocks <- function(dblocks) {
  do.call("cbind", dblocks)
}


X <- merge_datablocks(dblocks)
w <- list_to_matrix(weights)
Y <- X %*% w
tmp <- rowSums(w / apply(Y, 2, vnorm))

blocks <- list(1:3, 4:5)
indexify(blocks)

# normalization of weights
# stdcomp (comps of unit variance)
# normcomp (comps of unit norm)
# normweight (weights of unit norm)
# recweight (reciprocal weight unit norm)
# none

#' @param weights list of weights
#' @param blocks list of data blocks
#' @param option string indicating type of normalization
normalize_weights <- function(weights, blocks, option)
{
  if (option == "normcomp") {
    return(unit_norm_comp(weights, dblocks))
  } 
  if (option == "stdcomp") {
    unit_var_comp(weights, dblocks)
  }
  if (option == "normweight") {
    lapply(weights, unit_norm_weight)
  }
  if (option == "recweight") {
    lapply(weights, reciprocal_norm_weight)
  }
}


#' weights making components of unit norm
#' @param wgs list of weights
#' @param Xbs list of data blocks
unit_norm_comp <- function(wgs, Xbs) {
  new_weights <- wgs
  for (b in 1:length(wgs)) {
    aux_norm <- vnorm(Xbs[[b]] %*% wgs[[b]])
    new_weights[[b]] <- wgs[[b]] / aux_norm
  }
  new_weights
}


#' weights making components of unit variance
#' @param wgs list of weights
#' @param Xbs list of data blocks
unit_var_comp <- function(wgs, Xbs) {
  new_weights <- unit_norm_comp(wgs, Xbs)
  sqrtN <- sqrt(nrow(Xbs[[1L]]))
  lapply(new_weights, 
         function(w, sqrtN) w * sqrtN,
         sqrtN)
}


#' weights of unit norm
#' @param w vector of weights (for one block)
unit_norm_weight <- function(w) {
  w / vnorm(w)
}


#' weights of unit norm
#' @param w vector of weights (for one block)
reciprocal_norm_weight <- function(w) {
  w / sum(w * w)
}

normalize_weights <- function(weights, blocks, option)
{
  switch(option,
         normcomp = unit_norm_comp(weights, dblocks),
         stdcomp = unit_var_comp(weights, dblocks),
         normweight = lapply(weights, unit_norm_weight),
         recweight = lapply(weights, reciprocal_norm_weight)
  )
}

normalize_weights(weights, dblocks, "normweight")
normalize_weights(weights, dblocks, "recweight")
normalize_weights(weights, dblocks, "normcomp")
normalize_weights(weights, dblocks, "stdcomp")


# inner phase
# phase = "retrofit" (wold)
# phase = "fixed" (lohmoller)

# inner scheme
# scheme = "horst"
# scheme = "centroid"
# scheme = "factorial"
# scheme = "abscor"
# scheme = "sigregression"
# scheme = "regression"
# scheme = "path"

# modes A or B
