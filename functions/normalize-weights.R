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


