#' @export
"predict.rpart" <-
  function(object, newdata = list(), type = c(
             "vector", "prob",
             "class", "matrix", "where"
           ), ...) {
    if (!inherits(object, "rpart")) {
      stop("Not legitimate tree")
    }
    mtype <- missing(type)
    type <- match.arg(type)
    if (missing(newdata)) {
      where <- object$where
    } else {
      if (is.null(attr(newdata, "terms"))) {
        Terms <- stats::delete.response(object$terms)
        act <- (object$call)$na.action
        if (is.null(act)) {
          act <- na.rpart
        }
        newdata <- stats::model.frame(Terms, newdata,
          na.action = act,
          xlev = attr(object, "xlevels")
        )
      }
      where <- pred.rpart(object, rpart.matrix(newdata))
    }
    if (type == "where") {
      return(where)
    } else {
      frame <- object$frame
      method <- object$method
      ylevels <- attr(object, "ylevels")
      nclass <- length(ylevels)
      if (mtype && nclass > 0) {
        type <- "prob"
      }
      if (mtype && method == "mrt") {
        type <- "matrix"
      }
      if (type == "vector" || (type == "matrix" && is.null(frame$yval2))) {
        pred <- frame$yval[where]
        names(pred) <- names(where)
      } else if (type == "matrix") {
        pred <- frame$yval2[where, ]
        dimnames(pred) <- list(names(where), NULL)
      } else if (type == "class" && nclass > 0) {
        pred <- factor(ylevels[frame$yval[where]], levels = ylevels)
        names(pred) <- names(where)
      } else if (type == "prob" && nclass > 0) {
        pred <- frame$yval2[where, 1 + nclass + 1:nclass]
        dimnames(pred) <- list(names(where), ylevels)
      } else {
        stop("Invalid prediction for rpart object")
      }
      if (missing(newdata) && !is.null(object$na.action)) {
        pred <- stats::naresid(object$na.action, pred)
      }
      pred
    }
  }
