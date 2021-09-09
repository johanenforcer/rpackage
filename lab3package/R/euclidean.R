
#' Euclidean algorithm implementation ...
#' @export euclidean
#' @param a A number
#' @param b A number
#' @return The gcd between both numbers
#' @references https://en.wikipedia.org/wiki/Euclidean_algorithm

euclidean <- function(a, b){
  while(b != 0){
    r <- a %% b
    a <- b
    b <- r
  }
  return(a)
}
