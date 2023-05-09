
# Define test cases
test_that("make_lu_int returns expected output with positive input values", {
  # Define inputs
  int_data <- c(1, 2, 3, 4)

  # Define expected output
  expected_output <- data.frame(
    xl = -1,
    xu = 7,
    yl = -1,
    yu = 3
  )

  # Ensure that the output of the function matches the expected output
  expect_equal(make_lu_int(int_data), expected_output)
})

test_that("make_lu_int returns expected output with negative input values", {
  # Define inputs
  int_data <- c(-2, 4, -3, 5)

  # Define expected output
  expected_output <- data.frame(
    xl = -8,
    xu = 2,
    yl = -6,
    yu = 2
  )

  # Ensure that the output of the function matches the expected output
  expect_equal(make_lu_int(int_data), expected_output)
})

test_that("make_lu_int returns expected output with zero input values", {
  # Define inputs
  int_data <- c(0, 0, 0, 0)

  # Define expected output
  expected_output <- data.frame(
    xl = 0,
    xu = 0,
    yl = 0,
    yu = 0
  )

  # Ensure that the output of the function matches the expected output
  expect_equal(make_lu_int(int_data), expected_output)
})

