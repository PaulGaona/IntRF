#' @export
"post.rpart" <-
  function(tree, title.,
           filename = paste(deparse(substitute(tree)), ".ps", sep = ""),
           digits = base::getOption("digits") - 3, pretty = TRUE,
           use.n = TRUE, horizontal = TRUE, ...) {
    if (filename != "") {
      grDevices::postscript(file = filename, horizontal = horizontal, ...)
      graphics::par(mar = c(2, 2, 4, 2) + .1)
      on.exit(grDevices::dev.off())
    } else {
      oldpar <- graphics::par(mar = c(2, 2, 4, 2) + .1)
      on.exit(invisible(graphics::par(oldpar)))
    }

    plot(tree, uniform = TRUE, branch = .2, compress = TRUE, margin = .1)
    graphics::text(tree, all = TRUE, use.n = use.n, digits = digits, pretty = pretty)
    method <- tree$method

    if (missing(title.)) {
      temp <- attr(tree$terms, "variables")[2]
      graphics::title(paste("Endpoint =", temp), cex = .8)
    } else if (title. != "") graphics::title(title., cex = .8)
  }
