
myfun <- function(a, b, ...)
{
  arglist <- match.call(expand.dots = FALSE)$...
  arglist
}

  
myfun <- function(a, b, ...)
{
  arglist = match.call()[-1]
  if (arglist$what == "suma")
    res = a + b
  if (arglist$what == "resta")
    res = a - b
  print(paste("the result is", res))
}

myfun(1, 2, what="resta", na.rm=TRUE)




myfun <- function(a, b, ...)
{
  arglist = match.call()[-1]
  if ('what' %in% names(arglist)) {
    tmp = match("what", names(arglist))
    names(arglist)[tmp] = "operacion"
  }
  if (arglist$operacion == "suma")
    res = a + b
  if (arglist$operacion == "resta")
    res = a - b
  print(paste("the result is", res))
}

myfun(1, 2, what="resta", na.rm=TRUE)
