## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
#devtools::install_github("PaulGaona/IntRF")

library(IntRF)

## ----a------------------------------------------------------------------------
head(prices)
str(prices)
prices_ge <- prices[c(3,4,1,2)]
prices_stand_ge <- prices_stand[c(3,4,1,2)]

## ----b------------------------------------------------------------------------
IntRF::int_plot(int_data = prices_ge,
                       title = "DJIA vs GE", 
                       xlabel = "[DJIA]", 
                       ylabel = "[GE]"
                       )

## ----c------------------------------------------------------------------------
set.seed(1)

samp <- sort(sample(nrow(prices_ge), nrow(prices_ge) * .8))

price_train <- prices_ge[samp, ]
price_test <- prices_ge[-samp, ]
price_sd <- apply(price_train, 2, sd, na.rm = TRUE)

train_stand_ge <- sweep(price_train,2,price_sd,FUN="/")
test_stand_ge <- sweep(price_test,2,price_sd,FUN="/")
# train data
colnames(prices_ge)
yprice_train <- train_stand_ge[1:2]
xcprice_train <- train_stand_ge[3]
xrprice_train <- train_stand_ge[4]
yprice_test <- test_stand_ge[1:2]

## ----d------------------------------------------------------------------------
model <- IntRF::intrf(int_resp = yprice_train,
                             cent_pred = xcprice_train,
                             ran_pred = xrprice_train,
                             train = train_stand_ge,
                             test = test_stand_ge
)

## ----e------------------------------------------------------------------------
names(model)
length(model[[1]])
head(model$bootstrapInfo[[1]]$bootstrapSamples)
length(model[[2]])
length(model[[3]])
names(model$allTreePredictions)
head(model$allTreePredictions[[1]][[1]])
length(model[[4]])
names(model$Results)
head(model$Results[[1]])

## ----f------------------------------------------------------------------------
# Extract the results from the model
res_ge <- as.data.frame(model$Results)

IntRF::acc_met(cent_pred = res_ge$center_pred,
                    cent_act = res_ge$center_actual,
                    ran_pred = res_ge$range_pred,
                    ran_act = res_ge$range_actual,
                    train_resp = yprice_train)

