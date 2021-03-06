#' Type Check
#'
#' Checks if an object inherits class 'outForest'.
#'
#' @param x Any object.
#' @return A logical vector of length one.
#' @export
#' @examples
#' a <- outForest(iris)
#' is.outForest(a)
#' is.outForest("a")
is.outForest <- function(x) {
  inherits(x, "outForest")
}
