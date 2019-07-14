#' @title Chi Square test
#' 
#' @description Chi Square test
#' @param x a vector or factor
#' @param y a vector or factor
#' @param na.rm whether to remove missing values
#' @note x and y must have the same length
#' @export
#' @examples
#' chi_square(iris$Species, iris$Species)
chi_square <- function(x, y, na.rm = TRUE) 
{
  if (!is_one_dim(x))
    stop("\n'x' must be a one-dimensional object")
  if (!is_one_dim(y))
    stop("\n'y' must be a one-dimensional object")
  if (length(x) != length(y)) 
    stop("\n'x' and 'y' must have the same length")

  if (na.rm) {
    OK <- complete.cases(x, y)
    x <- factor(x[OK])
    y <- factor(y[OK])
    if ((nlevels(x) < 2L) || (nlevels(y) < 2L)) 
      stop("'x' and 'y' must have at least 2 levels")
  }

  # contingency table
  xy_table <- table(x, y)
  
  if (any(xy_table < 0) || any(is.na(xy_table))) 
    stop("\nthere are nonnegative and/or finite entries")
  if ((n <- sum(xy_table)) == 0) 
    stop("at least one entry is zero")

  METHOD <- "Pearson's Chi-squared test"
  nr <- as.integer(nrow(xy_table))
  nc <- as.integer(ncol(xy_table))
  if (is.na(nr) || is.na(nc) || is.na(nr * nc)) 
    stop("\nInvalid data")
  sr <- rowSums(xy_table)
  sc <- colSums(xy_table)
  E <- outer(sr, sc, "*") / n
  v <- function(r, c, n) c * r * (n - r) * (n - c)/n^3
  V <- outer(sr, sc, v, n)
  
  STATISTIC <- sum((xy_table - E)^2/E)
#  PARAMETER <- length(xy_table) - 1
  PARAMETER <- (nr - 1L) * (nc - 1L)
  PVAL <- pchisq(STATISTIC, PARAMETER, lower.tail = FALSE)
  
  structure(list(statistic = STATISTIC, 
                 parameter = PARAMETER,
                 p.value = PVAL, 
                 method = METHOD, 
                 observed = xy_table, 
                 expected = E, 
                 residuals = (xy_table - E) / sqrt(E), 
                 stdres = (xy_table - E) / sqrt(V)), 
            class = "htest")
}


