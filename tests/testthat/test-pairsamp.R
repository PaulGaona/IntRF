
# Define test data
set.seed(123)
y1 = rnorm(10)
y2 = rnorm(10)
x1 = rnorm(10)
x2 = rnorm(10)
x3 = rnorm(10)
x4 = rnorm(10)
int_resp <- data.frame(y1, y2)
cent_pred <- data.frame(x1, x2)
ran_pred <- data.frame(x3, x4)
train <- data.frame(y1, y2, x1, x2, x3, x4)
mtry <- 2

# Define test cases
test_that("pairsamp function returns correct output", {
  # Call the function
  output <- pairsamp(int_resp, cent_pred, ran_pred, train, mtry)

  # Test that output is a list with three elements
  expect_type(output, "list")
  expect_length(output, 3)

  # Test that first element of output is a data frame
  expect_type(output$origSample, "list")

  # Test that number of rows and columns of origSample are correct
  expect_equal(nrow(output$origSample), nrow(train))
  expect_equal(ncol(output$origSample), ncol(int_resp) +
                 ncol(cent_pred)+ ncol(ran_pred))

  # Test that second and third elements of output are character vectors
  expect_type(output$response, "character")
  expect_type(output$predictor, "character")

  # Test that second and third elements of output have correct values
  expect_equal(output$response, c("y1", "y2"))
  expect_equal(output$predictor, "x1 + x2 + x3 + x4")
})

