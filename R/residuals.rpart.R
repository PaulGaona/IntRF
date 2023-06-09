#' @export
"residuals.rpart" <-
  function(object, type = c("usual", "pearson", "deviance"), ...) {
    if (!inherits(object, "rpart")) {
      stop("Not legitimate rpart object")
    }

    y <- object$y
    if (is.null(y)) y <- stats::model.extract(stats::model.frame(object), "response")
    frame <- object$frame
    type <- match.arg(type)
    if (is.na(match(type, c("usual", "pearson", "deviance")))) {
      stop("Don't know about this type of residual")
    }

    if (object$method == "class") {
      ylevels <- attr(object, "ylevels")
      nclass <- length(ylevels)

      if (type == "usual") {
        yhat <- frame$yval[object$where]
        loss <- object$parms$loss
      } else {
        yprob <- frame$yval2[object$where, 1 + nclass + 1:nclass]
        yhat <- yprob[cbind(seq(y), unclass(y))]
      }
      resid <- switch(type,
        usual = loss[cbind(y, yhat)],
        pearson = (1 - yhat) / yhat,
        deviance = -2 * log(yhat)
      )
    } else if (object$method == "poisson" || object$method == "exp") {
      lambda <- (object$frame$yval)[object$where]
      time <- y[, 1] # observation time in new data
      events <- y[, 2] # number of events, in new data
      expect <- lambda * time # expected number of events
      temp <- ifelse(expect == 0, .0001, 0) # failsafe for log(0)

      resid <- switch(type,
        usual = events - expect,
        pearson = (events - expect) / sqrt(temp),
        deviance = sign(events - expect) *
          sqrt(2 * (events * log(events / temp) - (events - expect)))
      )
    } else {
      resid <- y - frame$yval[object$where]
    }


    names(resid) <- names(y)
    # Expand out the missing values in the result
    if (!is.null(object$na.action)) {
      resid <- stats::naresid(object$na.action, resid)
    }

    resid
  }
