#'spcol
#'@export
spcol <- function(x, Species) {
  nrx <- nrow(x)
  x$Species = rep(Species, nrx)
  return(x)
}
