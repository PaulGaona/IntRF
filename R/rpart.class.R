#' @export
"rpart.class" <-
  function(y, offset, parms, wt) {
    if (!is.null(offset)) {
      stop("No offset allowed in classification models")
    }
    fy <- as.factor(y)
    y <- as.integer(fy)
    numclass <- max(y[!is.na(y)])
    counts <- tapply(wt, factor(y, levels = 1:numclass), sum)
    counts <- ifelse(is.na(counts), 0, counts)
    numresp <- 1 + numclass
    if (missing(parms) || is.null(parms)) {
      parms <- list(prior = counts / sum(counts), loss = matrix(rep(
        1,
        numclass^2
      ) - diag(numclass), numclass), split = 1)
    } else if (is.list(parms)) {
      if (is.null(names(parms))) {
        stop("The parms list must have names")
      }
      temp <- pmatch(names(parms), c("prior", "loss", "split"),
        nomatch = 0
      )
      if (any(temp == 0)) {
        stop(paste("parms component not matched:", (names(parms))[temp ==
          0]))
      }
      names(parms) <- c("prior", "loss", "split")[temp]
      if (is.null(parms$prior)) {
        temp <- c(counts / sum(counts))
      } else {
        temp <- parms$prior
        if (sum(temp) != 1) {
          stop("Priors must sum to 1")
        }
        if (any(temp < 0)) {
          stop("Priors must be >= 0")
        }
        if (length(temp) != numclass) {
          stop("Wrong length for priors")
        }
      }
      if (is.null(parms$loss)) {
        temp2 <- 1 - diag(numclass)
      } else {
        temp2 <- parms$loss
        if (length(temp2) != numclass^2) {
          stop("Wrong length for loss matrix")
        }
        temp2 <- matrix(temp2, ncol = numclass)
        if (any(diag(temp2) != 0)) {
          stop("Loss matrix must have zero on diagonals")
        }
        if (any(temp2 < 0)) {
          stop("Loss matrix cannot have negative elements")
        }
        if (any(rowSums(temp2) == 0)) {
          stop("Loss matrix has a row of zeros")
        }
      }
      if (is.null(parms$split)) {
        temp3 <- 1
      } else {
        temp3 <- pmatch(parms$split, c("gini", "information"))
        if (is.null(temp3)) {
          stop("Invalid splitting rule")
        }
      }
      parms <- list(
        prior = temp, loss = matrix(temp2, numclass),
        split = temp3
      )
    } else {
      stop("Parameter argument must be a list")
    }
    list(
      y = y, parms = parms, numresp = numclass + 1, counts = counts,
      ylevels = levels(fy), numy = 1, print = function(yval,
                                                       ylevel, digits) {
        if (is.null(ylevel)) {
          temp <- as.character(yval[
            ,
            1
          ])
        } else {
          temp <- ylevel[yval[, 1]]
        }
        nclass <- (ncol(yval) - 1) / 2
        if (nclass < 5) {
          yprob <- format(yval[, 1 + nclass + 1:nclass],
            digits = digits, nsmall = digits
          )
        } else {
          yprob <- formatg(yval[, 1 + nclass + 1:nclass],
            digits = 2
          )
        }
        if (is.null(dim(yprob))) yprob <- matrix(yprob, ncol = length(yprob))
        temp <- paste(temp, " (", yprob[, 1], sep = "")
        for (i in 2:ncol(yprob)) {
          temp <- paste(temp, yprob[
            ,
            i
          ], sep = " ")
        }
        temp <- paste(temp, ")", sep = "")
        temp
      }, summary = function(yval, dev, wt, ylevel, digits) {
        nclass <- (ncol(yval) - 1) / 2
        group <- yval[, 1]
        counts <- yval[, 1 + (1:nclass)]
        yprob <- yval[, 1 + nclass + 1:nclass]
        if (!is.null(ylevel)) group <- ylevel[group]
        temp1 <- formatg(counts, format = "%5g")
        temp2 <- formatg(yprob, format = "%5.3f")
        if (nclass > 1) {
          temp1 <- apply(matrix(temp1, ncol = nclass),
            1, paste,
            collapse = " "
          )
          temp2 <- apply(matrix(temp2, ncol = nclass),
            1, paste,
            collapse = " "
          )
        }
        paste("  predicted class=", format(group, justify = "left"),
          "  expected loss=", formatg(dev / wt, digits),
          "\n", "    class counts: ", temp1, "\n", "   probabilities: ",
          temp2,
          sep = ""
        )
      }, text = function(yval, dev, wt, ylevel, digits, n,
                         use.n) {
        nclass <- (ncol(yval) - 1) / 2
        group <- yval[, 1]
        counts <- yval[, 1 + (1:nclass)]
        if (!is.null(ylevel)) group <- ylevel[group]
        temp1 <- formatg(counts, digits)
        if (nclass > 1) {
          temp1 <- apply(matrix(temp1, ncol = nclass),
            1, paste,
            collapse = "/"
          )
        }
        if (use.n) {
          out <- paste(format(group, justify = "left"),
            "\n", temp1,
            sep = ""
          )
        } else {
          out <- format(group, justify = "left")
        }
        return(out)
      }, bar = function(yval2) {
        yval2[, 2:((ncol(yval2) + 1) / 2)]
      }
    )
  }
