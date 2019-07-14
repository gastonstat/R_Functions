#' @title Blockification
#' 
#' @description Separates a data set into a list of blocks
#' 
#' @param dataset matrix or data frame containing the data
#' @param block_list list of vectors indicating which columns of the 
#' dataset form the blocks
#' @return a list of blocks
#' @export
#' @keywords internal
#' @examples
#' num_obs = 10
#' num_var = 6
#' set.seed(5)
#' dataset = matrix(rnorm(num_obs * num_var), num_obs, num_var)
#' block_list = list(1:2, 3:4, 5:6)
#' 
#' blockification(dataset, block_list)
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
