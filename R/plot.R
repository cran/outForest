#' Plot for outForest
#'
#' This function can plot aspects of an 'outForest' object. For \code{what = "counts"}, the number of outliers per variable is visualized as a barplot. For \code{what = "scores"}, outlier scores (i.e. the scaled difference between predicted and observed value) are shown as scatter plot per variable.
#'
#' @importFrom graphics text barplot stripchart abline
#' @method plot outForest
#' @param x An object of class \code{outForest}.
#' @param what What should be plotted? One of "counts" (the default) or "scores".
#' @param ... Further arguments passed to \code{graphics::barplot} or \code{graphics::stripchart}.
#' @return An object of class \code{ggplot2}.
#' @export
#' @examples
#' irisWithOutliers <- generateOutliers(iris, seed = 345)
#' x <- outForest(irisWithOutliers, verbose = 0)
#' plot(x)
#' plot(x, what = "scores")
plot.outForest <- function(x, what = c("counts", "scores"), ...) {
  what <- match.arg(what)

  if (what == "counts") {
    yy <- barplot(x$n_outliers, horiz = TRUE, yaxt = "n",
                  main = "Number of outliers per variable", xlab = "Count", ...)
    text(0.1, yy, names(x$n_outliers), adj = 0)
  } else {
    if (nrow(outliers(x)) == 0L) {
      stop("No outlier to plot")
    }
    stripchart(score ~ col, data = outliers(x), vertical = TRUE,
               pch = 4, las = 2, cex.axis = 0.7, ...)
    abline(h = c(-1, 1) * outliers(x)$threshold[1], lty = 2)
  }
}
