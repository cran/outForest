#' Out-of-Sample Application
#'
#' Identifies outliers in new data based on previously fitted "outForest" object.
#' The result of `predict()` is again an object of class "outForest".
#' All its methods can be applied to it.
#'
#' @param object An object of class "outForest".
#' @param newdata A new `data.frame` to be assessed for numeric outliers.
#' @inheritParams outForest
#' @param ... Further arguments passed from other methods.
#' @returns An object of class "outForest".
#' @export
#' @examples
#' (out <- outForest(iris, allow_predictions = TRUE))
#' iris1 <- iris[1, ]
#' iris1$Sepal.Length <- -1
#' pred <- predict(out, newdata = iris1)
#' outliers(pred)
#' Data(pred)
#' plot(pred)
#' plot(pred, what = "scores")
#' @seealso [outForest()], [outliers()], [Data()]
predict.outForest <- function(object, newdata,
                              replace = c("pmm", "predictions", "NA", "no"),
                              pmm.k = 3L, threshold = object$threshold,
                              max_n_outliers = Inf,
                              max_prop_outliers = 1, seed = NULL, ...) {
  replace <- match.arg(replace)
  newdata <- as.data.frame(newdata)

  stopifnot(
    is.data.frame(newdata),
    (n <- nrow(newdata)) >= 1L
  )
  if (!object$allow_predictions) {
    stop("Use 'allow_predictions = TRUE' when creating 'outForest' object.")
  }

  # Initialization
  if (!is.null(seed)) {
    set.seed(seed)
  }
  v <- object$v
  predData <- newdata[, v, drop = FALSE]

  # Currently, can deal only with NA in one single v
  all_vars <- union(v, object$used_to_check)
  any_NA <- names(which(0 < colSums(is.na(newdata[, all_vars, drop = FALSE]))))
  if (length(any_NA) <= 1L && all(any_NA %in% v)) {
    if (length(any_NA) == 1L) {
      .s <- is.na(newdata[[any_NA]])
      newdata[.s, any_NA] <- stats::predict(
        object$forests[[any_NA]], data = newdata[.s, ]
      )$predictions
    }
  } else {
    stop("Missing values only in one v variable allowed.")
  }

  # Check each variable
  for (vv in v) {
    if (length(setdiff(object$used_to_check, vv))) {
      predData[[vv]] <- stats::predict(object$forests[[vv]], data = newdata)$predictions
    } else {
      predData[[vv]] <- object$mu[vv]
    }
  }
  # Calculate outlier scores and status
  scores <- scale(
    newdata[, v, drop = FALSE] - predData, center = FALSE, scale = object$rmse
  )
  if (length(any_NA)) {
    scores[.s, any_NA] <- 0
    newdata[.s, any_NA] <- NA
  }
  out <- process_scores(
    data = newdata,
    scores = scores,
    predData = predData,
    v = v,
    rmse = object$rmse,
    replace = replace,
    pmm.k = pmm.k,
    threshold = threshold,
    max_n_outliers = max_n_outliers,
    max_prop_outliers = max_prop_outliers,
    allow_predictions = FALSE, obj = object
  )
  out <- c(out, list(forests = NULL), object[c("used_to_check", "mu")])
  class(out) <- c("outForest", "list")
  out
}
