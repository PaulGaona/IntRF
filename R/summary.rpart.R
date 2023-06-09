#' @export
"summary.rpart" <-
  function(object, cp = 0, digits = base::getOption("digits"), file, ...) {
    if (!inherits(object, "rpart")) stop("Not legitimate rpart object")

    # If this is an older-style rpart object, convert it
    #  either way, rename it to "x" to save typing
    if (!is.null(object$frame$splits)) {
      x <- rpconvert(object)
    } else {
      x <- object
    }

    if (!missing(file)) {
      sink(file)
      on.exit(sink())
    }

    if (!is.null(x$call)) {
      cat("Call:\n")
      dput(x$call)
    }

    omit <- x$na.action
    n <- x$frame$n
    if (length(omit)) {
      cat("  n=", n[1], " (", stats::naprint(omit), ")\n\n", sep = "")
    } else {
      cat("  n=", n[1], "\n\n")
    }

    print(x$cptable, digits = digits)
    ff <- x$frame
    ylevel <- attr(x, "ylevels")
    id <- as.integer(row.names(ff))
    parent.id <- ifelse(id == 1, 1, floor(id / 2))
    parent.cp <- ff$complexity[match(parent.id, id)]
    rows <- (1:length(id))[parent.cp > cp]
    if (length(rows) > 0) {
      rows <- rows[order(id[rows])]
    } else {
      rows <- 1
    }
    is.leaf <- (ff$var == "<leaf>")
    index <- cumsum(c(1, ff$ncompete + ff$nsurrogate + 1 * (!is.leaf)))

    if (!all(is.leaf)) { # skip these lines for a "no splits" tree
      sname <- dimnames(x$splits)[[1]]
      cuts <- vector(mode = "character", length = nrow(x$splits))
      temp <- x$splits[, 2]
      for (i in 1:length(cuts)) {
        if (temp[i] == -1) {
          cuts[i] <- paste("<", format(signif(x$splits[i, 4], digits = digits)))
        } else if (temp[i] == 1) {
          cuts[i] <- paste("<", format(signif(x$splits[i, 4], digits = digits)))
        } else {
          cuts[i] <- paste("splits as ",
            paste(c("L", "-", "R")[x$csplit[x$splits[i, 4], 1:temp[i]]],
              collapse = "", sep = ""
            ),
            collapse = ""
          )
        }
      }
      # S-PLUS 4.0 can't handle null vectors here
      if (any(temp < 2)) cuts[temp < 2] <- format(cuts[temp < 2], justify = "left")
      cuts <- paste(cuts, ifelse(temp >= 2, ",",
        ifelse(temp == 1, " to the right,", " to the left, ")
      ),
      sep = ""
      )
    }

    if (is.null(ff$yval2)) {
      tprint <- x$functions$summary(
        ff$yval[rows], ff$dev[rows],
        ff$wt[rows], ylevel, digits
      )
    } else {
      tprint <- x$functions$summary(
        ff$yval2[rows, ], ff$dev[rows],
        ff$wt[rows], ylevel, digits
      )
    }

    for (ii in 1:length(rows)) {
      i <- rows[ii]
      nn <- ff$n[i]
      twt <- ff$wt[i]
      cat("\nNode number ", id[i], ": ", nn, " observations", sep = "")
      if (ff$complexity[i] < cp || is.leaf[i]) {
        cat("\n")
      } else {
        cat(",    complexity param=",
          format(signif(ff$complexity[i], digits)), "\n",
          sep = ""
        )
      }

      cat(tprint[ii], "\n")
      if (ff$complexity[i] > cp && !is.leaf[i]) {
        sons <- 2 * id[i] + c(0, 1)
        sons.n <- ff$n[match(sons, id)]
        cat("  left son=", sons[1], " (", sons.n[1], " obs)",
          " right son=", sons[2], " (", sons.n[2], " obs)",
          sep = ""
        )
        j <- nn - (sons.n[1] + sons.n[2])
        if (j > 1) {
          cat(", ", j, " observations remain\n", sep = "")
        } else if (j == 1) {
          cat(", 1 observation remains\n")
        } else {
          cat("\n")
        }
        cat("  Primary splits:\n")
        j <- seq(index[i], length = 1 + ff$ncompete[i])
        if (all(nchar(cuts[j]) < 25)) {
          temp <- format(cuts[j], justify = "left")
        } else {
          temp <- cuts[j]
        }
        cat(
          paste("      ", format(sname[j], justify = "left"), " ", temp,
            " improve=", format(signif(x$splits[j, 3], digits)),
            ", (", nn - x$splits[j, 1], " missing)",
            sep = ""
          ),
          sep = "\n"
        )
        if (ff$nsurrogate[i] > 0) {
          cat("  Surrogate splits:\n")
          j <- seq(1 + index[i] + ff$ncompete[i], length = ff$nsurrogate[i])
          agree <- x$splits[j, 3]
          if (all(nchar(cuts[j]) < 25)) {
            temp <- format(cuts[j], justify = "left")
          } else {
            temp <- cuts[j]
          }
          if (ncol(x$splits) == 5) {
            adj <- x$splits[j, 5]
            cat(
              paste("      ", format(sname[j], justify = "left"), " ",
                temp,
                " agree=", format(round(agree, 3)),
                ", adj=", format(round(adj, 3)),
                ", (", x$splits[j, 1], " split)",
                sep = ""
              ),
              sep = "\n"
            )
          } else { # an older style rpart object -- no adj value present
            cat(
              paste("      ", format(sname[j], justify = "left"), " ",
                temp,
                " agree=", format(round(agree, 3)),
                ", (", x$splits[j, 1], " split)",
                sep = ""
              ),
              sep = "\n"
            )
          }
        }
      }
    }
    cat("\n")
    invisible(x)
  }
