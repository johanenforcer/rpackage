name <- "Johan Marbinah"
liuid <-  "nnnnn000"

euclidean <- function(a, b){
  while(b != 0){
    r <- a %% b
    a <- b
    b <- r
  }
  return(a)
}

euclidean(123612, 13892347912)
euclidean(100, 1000)
