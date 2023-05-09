#' Interval-valued Random Forests
#'
#' @param int_resp A data frame of interval-valued response variable.
#' @param cent_pred A data frame of the center values of the predictor
#' variables.
#' @param ran_pred A data frame of the range values of the predictor
#' variables.
#'
#' @param train  !IDK if I want this! A data frame of the training set of the
#' interval-valued data
#' @param test !IDK if I want this! A data frame of the testing set of the
#' interval-valued data (default = NULL)
#'
#' @param num_trees Number of trees used for ensembling (default = 500).
#' @param boot_perc Percent of observations used in the bootstrapping process
#' (default = 0.8).
#' @param mtry_int Number of interval predictor variables sampled (default =
#' (p^c + p^r )/6)
#' @return A random forest model with the bootstrapped information, individual
#'  trees and their predictions, and the interval predictions.
#' @export
#' @examples
#' # set seed for reproducibility
#'set.seed(123)
#'
# create interval-valued response variable
#'int_resp <- data.frame(low = runif(100), high = runif(100))
#'
#'# create center and range values of predictor variables
#'cent_pred <- data.frame(x1 = runif(100), x2 = runif(100), x3 = runif(100))
#'ran_pred <- data.frame(x1 = runif(100), x2 = runif(100), x3 = runif(100))
#'ran_pred <- ran_pred - cent_pred
#'
#'# create training set
#'train <- cbind(int_resp, cent_pred, ran_pred)
#'
# fit interval-valued random forest
#'intrf(int_resp = int_resp, cent_pred = cent_pred,
#'ran_pred = ran_pred, train = train, num_trees = 100)
#'

intrf <- function(int_resp,
                  cent_pred,
                  ran_pred,
                  train,
                  test = NULL,
                  num_trees = 500,
                  boot_perc = 0.8,
                  mtry_int = NULL) {
  # Error check
  # data types numeric
  if (all(!(sapply(train, is.numeric))) == TRUE) {
    rlang::abort("Atleast one column in 'train' is not a numeric type.")
  }
  if (all(!(sapply(int_resp, is.numeric))) == TRUE) {
    rlang::abort("Atleast one column in 'int_resp' is not a numeric type.")
  }
  if (all(!(sapply(cent_pred, is.numeric))) == TRUE) {
    rlang::abort("Atleast one column in 'cent_pred' is not a numeric type.")
  }
  if (all(!(sapply(ran_pred, is.numeric))) == TRUE) {
    rlang::abort("Atleast one column in 'ran_pred' is not a numeric type.")
  }
  # data types dataframe
  if (!is.data.frame(int_resp)) { # stop if center_predictors ! = data.frame
    rlang::abort("'int_resp' argument must be of type data.frame.")
  }
  if (!is.data.frame(cent_pred)) { # stop if center_predictors ! = data.frame
    rlang::abort("'cent_pred' argument must be of type data.frame.")
  }
  if (!is.data.frame(ran_pred)) { # stop if center_predictors ! = data.frame
    rlang::abort("'ran_pred' argument must be of type data.frame.")
  }
  if (!is.data.frame(train)) {
    # stop if train_data ! = data.frame
    rlang::abort("'train' argument must be of type data.frame.")
  }
  if (!is.numeric(boot_perc)) {
    # stop if bootstrap_percent ! = numeric
    rlang::abort("'boot_perc' argument is not numeric.")
  }
  if (!is.numeric(num_trees)) {
    # stop if num_trees ! = numeric
    rlang::abort("'num_trees' argument is not numeric.")
  }
  # data lengths
  if (!(nrow(int_resp) == nrow(cent_pred))) {
    # stop if number of rows of interval_response ! = center_predictors
    rlang::abort("Number of observations of `int_resp` and `cent_pred` must be
                 equal.")
  }
  if (!(nrow(cent_pred) == nrow(ran_pred))) {
    # stop if number of rows of center_predictors ! = range_predictors
    rlang::abort("Number of observations of `cent_pred` and `ran_pred` must be
                 of equal.")
  }
  if (!(length(int_resp) == 2)) {
    # stop if interval_response ! = 2
    rlang::abort("`int_resp` must be of length 2.")
  }
  if (!(length(cent_pred) == length(ran_pred))) {
    # stop if lengths of center_predictors and  range_predictors !=
    rlang::abort("`cent_pred` and `ran_pred` must be of equal length.")
  }
  if (length(boot_perc) != 1) {
    # stop if length of bootstrop ! = 1
    rlang::abort("'boot_perc' must be of length 1.")
  }
  if (length(num_trees) != 1) {
    # stop if length of num_trees ! = 1
    rlang::abort("'num_trees' must be of length 1.")
  }
  # columns in training set
  if (all(!(names(int_resp) %in% names(train))) == TRUE) {
    rlang::abort("Names of 'int_resp' are not in 'train'.")
  }
  if (all(!(names(cent_pred) %in% names(train))) == TRUE) {
    rlang::abort("Names of 'centResp' are not in 'train'.")
  }
  if (all(!(names(ran_pred) %in% names(train))) == TRUE) {
    rlang::abort("Names of 'ran_pred' are not in 'train'.")
  }
  # warnings
  if ((length(cent_pred) + length(ran_pred)) < 6) {
    # stop if length of centers is < 5
    rlang::warn("Length of predictor variables is < 6, the default mtry (p/3)
                will only produce a single interval for prediction.")
  }

  # Updates
  num_trees <- as.integer(num_trees)
  # number of predictor intervals
  num_int_pred <- ncol(cent_pred)
  # default mtry
  mtry <- ceiling(num_int_pred / 3)

  if (!missing(mtry_int)) {
    # User designated mtry error check
    mtry <- mtry_int
  }
  if (mtry < 1 || mtry > num_int_pred) {
    rlang::abort("'mtry_int' is not a valid number. Ensure it is at least one
                 and no less than the number of interval predictors.")
  }
  if (boot_perc > 1 || boot_perc < 0) {
    # Default and user designated boot_perc error check
    rlang::abort("'boot_perc' is not a valid number. Ensure it is less than
                 one and no less than 0.")
  }
  boots <- vector("list", num_trees)
  # pairwise sampling followed by bootstrap sampling
  for (i in seq(num_trees)) {
    # pairwise sampling
    boot_eq <- IntRF::pairsamp(
      int_resp = int_resp,
      cent_pred = cent_pred,
      ran_pred = ran_pred,
      train = train,
      mtry = mtry
    )
    # bootstrap sampling
    boot_samp <- sort(sample(nrow(int_resp * boot_perc), replace = TRUE))
    # Save the individual pairwise sample's bootstrap sample
    boots[[i]] <- c(
      boot_eq,
      list(bootstrapSamples = boot_eq[[1]][boot_samp, ])
      # , Potentially can use OOB samples for model testing
      # 'list(OOB = boot_eq[[1]][-boot_samp, ])
    ) # bs list
  }
  ind_trees <- vector("list", num_trees)
  # construction of tree
  for (i in seq(num_trees)) {
    # only for mvpart structure, potential better way if rewritten ###
    df_list <- boots[[i]]
    df <- df_list[[4]]
    df_resp_names <- df_list[[2]]
    eq_form <- data.matrix(df[df_resp_names]) ~ .
    ind_trees[[i]] <- IntRF::mvpart(eq_form,
      data = df,
      plot.add = FALSE,
      xv = "none"
    )
  }
  # Null test returns bootstrap data and individual trees
  if (is.null(test) == TRUE) {
    res <- list(boots, ind_trees)
  }
  # User provided test error checks
  if (!(is.null(test) == TRUE)) {
    # data types numeric
    if (all(!(sapply(test, is.numeric))) == TRUE) {
      rlang::abort("Atleast one column in 'test' is not a numeric type.")
    }
    # data types data frame
    if (!is.data.frame(test)) { # stop if train_data ! = data.frame
      rlang::abort("'test' argument must be of type data.frame.")
    }
    # length
    if (!(length(test) == length(train))) {
      rlang::abort("'test' data and 'train' data sets are not equal in length.")
    }
    # columns in training
    if (all(!(names(test) %in% names(train))) == TRUE) {
      rlang::abort("Names of 'test' are not in 'train'.")
    }
    # obtain individual tree predictions from user provided test
    # Predictions
    cent_pred <- vector("list", num_trees)
    ran_pred <- vector("list", num_trees)
    for (i in seq(cent_pred)) {
      # center predictions
      cent_pred[[i]] <- stats::predict(ind_trees[[i]],
        newdata = test,
        type = "matrix"
      )[, 1]
      # range predictions
      ran_pred[[i]] <- stats::predict(ind_trees[[i]],
        newdata = test,
        type = "matrix"
      )[, 2]
    }
    # save predictions
    pred_res <- list(
      centerPredictions = cent_pred,
      rangePredictions = ran_pred
    )
    # return only test data response variables
    dftest_resp <- test[df_resp_names]
    # averaging of predictions, and saving test data predictions
    int_pred <- list(
      center_pred = colMeans(do.call(rbind, cent_pred)),
      center_actual = as.vector(t(dftest_resp[1])),
      range_pred = colMeans(do.call(rbind, ran_pred)),
      range_actual = as.vector(t(dftest_resp[2]))
    )
    # save all information
    # bootstrap sampled data
    # individual trees (mvpart info)
    # individual tree predictions
    # results
    res <- list(
      bootstrapInfo = boots,
      Trees = ind_trees,
      allTreePredictions = pred_res,
      Results = int_pred
    )
  }
  res
}
