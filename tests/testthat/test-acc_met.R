
# Define test cases
test_that("acc_met returns expected output", {
  # Define inputs
  cent_pred <- c(1, 2, 3)
  cent_act <- c(1.5, 2.5, 3.5)
  ran_pred <- c(0.5, 1.5, 2.5)
  ran_act <- c(1, 2, 3)
  train_resp <- matrix(c(1, 2, 3, 4, 5, 6), ncol = 2)

  # Define expected output
  expected_output <- data.frame(
    MSE = 0.5,
    MAE = 1,
    R2 = 0.75
  )

  # Ensure that the output of the function matches the expected output
  expect_equal(acc_met(cent_pred, cent_act, ran_pred, ran_act, train_resp), expected_output)
})




