#' @export
"path.rpart" <-
  function(tree, nodes, pretty = 0, print.it = TRUE) {
    if (!inherits(tree, "rpart")) {
      stop("Not legitimate tree")
    }
    splits <- labels.rpart(tree, pretty = pretty)
    frame <- tree$frame
    n <- row.names(frame)
    node <- as.numeric(n)
    which <- descendants(node) # ancestors are columns
    path <- list()
    if (missing(nodes)) {
      xy <- rpartco(tree)
      while (length(i <- graphics::identify(xy, n = 1, plot = FALSE)) > 0) {
        path[[n[i]]] <- path.i <- splits[which[, i]]
        if (print.it) {
          cat("\n", "node number:", n[i], "\n")
          cat(paste("  ", path.i), sep = "\n")
        }
      }
    } else {
      if (length(nodes <- node.match(nodes, node)) == 0) {
        return(invisible())
      }
      for (i in nodes)
      {
        path[[n[i]]] <- path.i <- splits[which[, i]]
        if (print.it) {
          cat("\n", "node number:", n[i], "\n")
          cat(paste("  ", path.i), sep = "\n")
        }
      }
    }
    invisible(path)
  }
