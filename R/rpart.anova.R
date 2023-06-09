#' @export
"rpart.anova" <-
  function(y, offset, parms, wt) {
    if (!is.null(offset)) y <- y - offset
    list(
      y = y, parms = 0, numresp = 1, numy = 1,
      summary = function(yval, dev, wt, ylevel, digits) {
        paste("  mean=", formatg(yval, digits),
          ", MSE=", formatg(dev / wt, digits),
          sep = ""
        )
      },
      text = function(yval, dev, wt, ylevel, digits, n, use.n) {
        if (use.n) {
          paste(formatg(yval, digits), "\nn=", n, sep = "")
        } else {
          paste(formatg(yval, digits))
        }
      }
    )
  }
