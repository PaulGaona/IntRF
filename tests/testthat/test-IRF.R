

# Example data
int_resp <- data.frame(response1 = c(1,2,3,4), response2 = c(5,6,7,8))
cent_pred <- data.frame(predictor1 = c(1,2,3,4), predictor2 = c(5,6,7,8))
ran_pred <- data.frame(predictor3 = c(1,2,3,4), predictor4 = c(5,6,7,8))
train <- data.frame(response1 = c(1,2,3,4), response2 = c(5,6,7,8),
                    predictor1 = c(1,2,3,4), predictor2 = c(5,6,7,8),
                    predictor3 = c(1,2,3,4), predictor4 = c(5,6,7,8))
test <- data.frame(response1 = c(1,2,3,4), response2 = c(5,6,7,8),
                   predictor1 = c(1,2,3,4), predictor2 = c(5,6,7,8),
                   predictor3 = c(1,2,3,4), predictor4 = c(5,6,7,8))

# Test for numeric type errors
#expect_error(intrf(int_resp = data.frame(response1 = c(1,2,3,4), response2 = c(5,"a",7,8)),
#                   cent_pred, ran_pred, train),
#             "Atleast one column in 'int_resp' is not a numeric type.")
#expect_error(intrf(int_resp, cent_pred = data.frame(predictor1 = c(1,2,3,4), predictor2 = c("a",6,7,8)),
#                   ran_pred, train),
#             "Atleast one column in 'cent_pred' is not a numeric type.")
#expect_error(intrf(int_resp, cent_pred, ran_pred = data.frame(predictor3 = c(1,2,3,4), predictor4 = c(5,6,"b",8)),
#                   train),
#             "Atleast one column in 'ran_pred' is not a numeric type.")
#expect_error(intrf(int_resp, cent_pred, ran_pred, train = data.frame(response1 = c(1,2,3,"d"), response2 = c(5,6,7,8), predictor1 = c(1,2,3,4), predictor2 = c(5,6,7,8), predictor3 = c(1,2,3,4), predictor4 = c(5,6,7,8))),
#             "Atleast one column in 'train' is not a numeric type.")

# Test for data.frame type errors
expect_error(intrf(int_resp = c(1,2,3,4), cent_pred, ran_pred, train),
             "'int_resp' argument must be of type data.frame.")
expect_error(intrf(int_resp, cent_pred = c(1,2,3,4), ran_pred, train),
             "'cent_pred' argument must be of type data.frame.")
expect_error(intrf(int_resp, cent_pred, ran_pred = c(1,2,3,4), train),
             "'ran_pred' argument must be of type data.frame.")
expect_error(intrf(int_resp, cent_pred, ran_pred, train = c(1,2,3,4),
                   "'train' argument must be of type data.frame."))
# Test that function returns expected error message for non-numeric boot_perc
expect_error(intrf(int_resp, cent_pred, ran_pred, train, boot_perc = "abc"),
             "'boot_perc' argument is not numeric.")
# Test that function returns expected error message for non-numeric num_trees
expect_error(intrf(int_resp, cent_pred, ran_pred, train, num_trees = "abc"),
             "'num_trees' argument is not numeric.")

