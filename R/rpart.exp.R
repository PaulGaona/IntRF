#' @export
"rpart.exp" <-
  function(y, offset, parms, wt) {
    if (!inherits(y, "Surv")) {
      stop("Response must be a survival object - use the Surv() function")
    }

    ny <- ncol(y)
    n <- nrow(y)

    status <- y[, ny]
    if (any(y[, 1] <= 0)) stop("Observation time must be >0")
    if (all(status == 0)) stop("No deaths in data set")
    time <- y[, ny - 1]

    # Make my table of time intervals.  The first goes from 0 to the first
    #   death, the next from death 2 to death 3, ..., and the last from
    #   "next to last death" to "max time in dataset".
    # We also need to avoid a pathological condition in some data sets, where
    #   two death times differ by a trivial amount, e.g., 10^-16, perhaps due
    #   to roundoff error in creating the input data.  Ammalgamate such
    #   intervals.  This turns out to be hard to do in S, but easy in C
    dtimes <- sort(unique(time[status == 1])) # unique death times
    temp <- .C("rpartexp2",
      as.integer(length(dtimes)),
      as.double(dtimes),
      as.double(.Machine$double.eps),
      keep = integer(length(dtimes)), PACKAGE = "IntRF"
    )$keep
    dtimes <- dtimes[temp == 1]

    # For the sake of speed, restrict the number of intervals to be <1000.
    #   (Actually, anything >100 is probably overkill for the
    #   actual task at hand, which is to approximately scale to exponential).
    if (length(dtimes) > 1000) dtimes <- stats::quantile(dtimes, 0:1000 / 1000)

    # The last interval goes to the max time in the data set
    itable <- c(0, dtimes[-length(dtimes)], max(time)) # set of intervals

    # ~     drate1 <- function(n, ny, y, wt, itable) {
    # ~     # Compute the death rate within each of the intervals
    # ~     #  The pyears2 routine is part of the survival library
    # ~     ngrp <- length(itable) -1
    # ~     temp <- .C('pyears2',
    # ~            as.integer(n),
    # ~            as.integer(ny),
    # ~            as.integer(1),
    # ~            as.double (y),
    # ~            as.double(wt),
    # ~            as.integer(1),
    # ~            as.integer(0),
    # ~            as.integer(ngrp),
    # ~            as.double(itable),
    # ~            as.double(rep(0., n)),
    # ~            pyears = double(ngrp),
    # ~            pn     = double(ngrp),
    # ~            pcount = double(ngrp),
    # ~            offtable= double(1), PACKAGE="IntRF")[11:14]
    # ~     rates <- temp$pcount / temp$pyears
    # ~     rates
    # ~     }

    drate2 <- function(n, ny, y, wt, itable) {
      # An alternative to the drate1 function
      # Why?  The pyears2 routine changed in 6/2001, with the inclusion
      #  of case weights.  We need the newer version.  If you have the
      #  older version of the survival library, the above will crash S.
      # How to tell -- list the pyears function, and see whether it's
      #  call to pyears2 has weights in the argument list.
      #
      time <- y[, ny - 1]
      status <- y[, ny]
      ilength <- diff(itable) # lengths of intervals
      ngrp <- length(ilength) # number of intervals

      # The code below is as opaque as any I've written, all in the
      #  service of "no for loops".
      # First, 'index' gives the time interval (as defined by itable)
      #  in which the end of each observation's follow-up (time) lies.
      #  Then 'itime' will be the amount of time spent in that last
      #  interval, which is of course somewhat less than ilength.
      index <- unclass(cut(time, itable, include.lowest = TRUE))
      itime <- time - itable[index]
      if (ny == 3) {
        # there is both a start time and a stop time
        #  compute the amount of time NOT spent in the interval that
        #  the start time lies in.
        # stime <- y[,1]   #start time for each interval
        index2 <- unclass(cut(y[, 1], itable, include.lowest = TRUE))
        itime2 <- y[, 1] - itable[index2]
      }

      # Compute the amount of person-years in each of the intervals
      #   This is:  (width of interval) * (number of "time" elements that
      #                                     end in an interval farther right)
      #            + (ending times in this interval)
      # By construction, I know that there is at least 1 obs in each of the
      #  intervals, so tab1 is of a determined length
      tab1 <- table(index)
      temp <- rev(cumsum(rev(tab1))) # cumsum, counting from the right
      pyears <- ilength * c(temp[-1], 0) + tapply(itime, index, sum)
      if (ny == 3) {
        # subtract off the time before "start"
        tab2 <- table(index2, levels = 1:ngrp) # force the length of tab2
        temp <- rev(cumsum(rev(tab2)))
        py2 <- ilength * c(0, temp[-ngrp]) + tapply(itime2, index2, sum)
        pyears <- pyears - py2
      }

      deaths <- tapply(status, index, sum)
      rate <- deaths / pyears # hazard rate in each interval
      rate
    }

    #
    # Now, compute the "new y" for each observation.
    #  This is a stretching of the time axis
    # The cumulative hazard over each interval is rate*length(interval),
    #  and is the basis of the rescaling.
    rate <- drate2(n, ny, y, wt, itable)
    cumhaz <- cumsum(c(0, rate * diff(itable)))
    newy <- stats::approx(itable, cumhaz, time)$y
    if (ny == 3) {
      newy <- newy - stats::approx(itable, cumhaz, y[, 1])$y
    }

    if (length(offset) == n) newy <- newy * exp(offset)

    if (missing(parms)) {
      parms <- c(shrink = 1, method = 1)
    } else {
      parms <- as.list(parms)
      if (is.null(names(parms))) stop("You must input a named list for parms")
      parmsNames <- c("method", "shrink")
      indx <- pmatch(names(parms), parmsNames, nomatch = 0)
      if (any(indx == 0)) {
        stop(paste(
          "parms component not matched: ",
          names(parms)[indx == 0]
        ))
      } else {
        names(parms) <- parmsNames[indx]
      }

      if (is.null(parms$method)) {
        method <- 1
      } else {
        method <- pmatch(parms$method, c("deviance", "sqrt"))
      }
      if (is.na(method)) stop("Invalid error method for Poisson")

      if (is.null(parms$shrink)) {
        shrink <- 2 - method
      } else {
        shrink <- parms$shrink
      }
      if (!is.numeric(shrink) || shrink < 0) {
        stop("Invalid shrinkage value")
      }
      parms <- c(shrink = shrink, method = method)
    }
    list(
      y = cbind(newy, y[, 2]), parms = parms, numresp = 2, numy = 2,
      summary = function(yval, dev, wt, ylevel, digits) {
        paste("  events=", formatg(yval[, 2]),
          ",  estimated rate=", formatg(yval[, 1], digits),
          " , mean deviance=", formatg(dev / wt, digits),
          sep = ""
        )
      },
      text = function(yval, dev, wt, ylevel, digits, n, use.n) {
        if (use.n) {
          paste(formatg(yval[, 1], digits), "\n",
            formatg(yval[, 2]), "/", n,
            sep = ""
          )
        } else {
          paste(formatg(yval[, 1], digits))
        }
      }
    )
  }
