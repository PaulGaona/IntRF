################################################################################
#                           HELPER FUNCTIONS                                   #
################################################################################
#' Pairwise Sampling (Helper Function)
#'
#' @param int_resp A data frame of interval-valued response variable
#' (copy of int_resp in intrf).
#' @param cent_pred A data frame of the center values of the predictor
#' variables (copy of cent_pred in intrf).
#' @param ran_pred A data frame of the range values of the predictor
#' variables (copy of ran_pred in intrf).
#' @param train  A data frame of the training set of the
#' interval-valued data (copy of train in intrf).
#' @param mtry Number of interval predictor variables sampled (default =
#' (p^c + p^r )/6) (copy of mtry_int in intrf).
#'
#' @return a list containing the original data set and a subset of the predictors.
#' @examples
#'
#' # Simulate data
#' set.seed(123)
#' int_resp <- rnorm(100)
#' cent_pred <- matrix(rnorm(500), nrow = 100)
#' ran_pred <- matrix(runif(500), nrow = 100)
#' train <- data.frame(int_resp, cent_pred, ran_pred)
#'
#' # Randomly sample subset of predictors
#' mtry <- 3
#' pairsamp(int_resp, cent_pred, ran_pred, train, mtry)
#'
#' @export
pairsamp <- function(int_resp,
                     cent_pred,
                     ran_pred,
                     train,
                     mtry) {
  # warning about low number of variables?
  #### need more check statements

  # random sample of centers and ranges
  int_len <- ncol(cent_pred)
  psamp_len <- sort(sample(int_len, mtry))
  psamp_cent <- cent_pred[psamp_len]
  psamp_ran <- ran_pred[psamp_len]
  #
  psampdf <- train[c(names(int_resp), names(psamp_cent), names(psamp_ran))]
  # redefine new equation
  resp_name <- names(int_resp) # extract "y" / "response" interval
  # output as formula
  # extract "x" / "predictor" interval
  pred_name <- paste(c(names(psamp_cent), names(psamp_ran)), collapse = " + ")
  # list of sampled data, response names, and predictor names
  obj <- list(
    origSample = psampdf,
    response = resp_name,
    predictor = pred_name
  )
  obj
}
################################################################################
#' Calculate accuracy metrics for interval data
#'
#' The function \code{acc_met} takes in predicted and actual center and range values
#' of interval data, along with the outcome training data, and calculates
#' the mean squared error (MSE), mean absolute error (MAE), and R-squared (R2) values
#' for the intervals.
#'
#' @param cent_pred A data frame of the center values of the predictions
#' from the testing data.
#' @param cent_act A data frame of the center values of the actual
#' from the testing data.
#' @param ran_pred A data frame of the range values of the predictions
#' from the testing data.
#' @param ran_act A data frame of the range values of the actual
#' from the testing data.
#' @param train_resp A data frame of the outcome training values, used
#' for standardizing the accuracy metrics.
#'
#' @return A data frame with the calculated accuracy metrics including
#' mean squared error (MSE), mean absolute error (MAE), and R-squared (R2)
#' values for the intervals.
#'
#' @examples
#' cent_pred <- data.frame(1, 2, 3)
#' cent_act <- data.frame(1.1, 1.8, 2.9)
#' ran_pred <- data.frame(0.2, 0.3, 0.1)
#' ran_act <- data.frame(0.3, 0.2, 0.15)
#' train_resp <- data.frame(1.1, 1.2, 1.3)
#' acc_met(cent_pred, cent_act, ran_pred, ran_act, train_resp)
#'
#'@export

acc_met <- function(cent_pred,
                    cent_act,
                    ran_pred,
                    ran_act,
                    train_resp) {
  # mse of intervals
  mse_cent <- mean((cent_pred - cent_act)^2)
  mse_ran <- mean((ran_pred - ran_act)^2)
  mse_int <- mse_cent + mse_ran
  # mae of intervals
  mae_cent <- mean(abs(cent_pred - cent_act))
  mae_ran <- mean(abs(ran_pred - ran_act))
  mae_int <- mae_cent + mae_ran
  # R^2 of intervals
  num <- mse_int
  den <- sum(apply(train_resp, 2, stats::sd, na.rm = TRUE))
  r2_int <- 1 - (num / den)
  acc_met <- data.frame(
    MSE = mse_int,
    MAE = mae_int,
    R2 = r2_int
  )
  acc_met
}
################################################################################
#' Boxes for interval data
#'
#' @param int_data A data frame of center and range interval-valued data
#' (copy of int_plot).
#' @export
make_lu_int <- function(int_data) {
  xc <- xr <- yc <- yr <- NULL

  yc <- int_data[1]
  yr <- int_data[2]
  xc <- int_data[3]
  xr <- int_data[4]

  xl <- xc - xr
  xu <- xc + xr
  yl <- yc - yr
  yu <- yc + yr

  int_set <- data.frame(xl, xu, yl, yu) # interval for plotting
  int_set
}
