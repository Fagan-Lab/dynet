# get dimensions of vertices helper function
DIM <- function(...) {
  args <- list(...)
  lapply(args, function(x) {
    if (is.null(dim(x))) {
      return(length(x))
    }
    dim(x)
  })
}
