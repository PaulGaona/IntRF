#' A data frame of stock prices for GE, where each variable is standardized
#' by its respective variance.
#'
#' @format A data frame with 1509 rows and 4 variables:
#' \describe{
#' \item{c.DJI}{Center values for The Dow Jones Industrial Average}
#'
#' \item{r.DJI}{Range values for The Dow Jones Industrial Average}
#'
#' \item{c.GE}{Center values for General Electric}
#'
#' \item{r.GE}{Range values for General Electric}
#' }
#' @name prices_stand
NULL
#' A data frame of stock prices for GE.
#'
#' @format A data frame with 1509 rows and 4 variables:
#' \describe{
#' \item{c.DJI}{Center values for The Dow Jones Industrial Average}
#'
#' \item{r.DJI}{Range values for The Dow Jones Industrial Average}
#'
#' \item{c.GE}{Center values for General Electric}
#'
#' \item{r.GE}{Range values for General Electric}
#' }
#' @name prices
NULL
#' A data frame of stock prices for GE.
#'
#' @format A data frame with 1509 rows and 4 variables:
#' \describe{
#' \item{c.DJI}{Center values for The Dow Jones Industrial Average}
#'
#' \item{r.DJI}{Range values for The Dow Jones Industrial Average}
#'
#' \item{c.GE}{Center values for General Electric}
#'
#' \item{r.GE}{Range values for General Electric}
#' }
#' @name sub_prices_stand
NULL
#' A data frame of stock prices for GE.
#'
#' @format A data frame with 1509 rows and 4 variables:
#' \describe{
#' \item{c.DJI}{Center values for The Dow Jones Industrial Average}
#'
#' \item{r.DJI}{Range values for The Dow Jones Industrial Average}
#'
#' \item{c.GE}{Center values for General Electric}
#'
#' \item{r.GE}{Range values for General Electric}
#' }
#' @name sub_prices
NULL
# Data generation and saving
# packages needed
# library(tidyquant)
# library(tidyverse)
# vector of stock indexes
# stock_ind <- c("^DJI", "GE")
# obtain stock price data
# prices <- tq_get(stock_ind,
#                 from = "2012-01-01",
#                 to = "2017-12-30",
#                 get = "stock.prices"
# )
#  Select variables, convert to centers and ranges, pivot df
# prices <- prices %>%
#  select(symbol, high, low) %>%
#  mutate(
#    c = (high + low) / 2, # center
#    r = (high - low) / 2 # range
#  ) %>%
#  select(symbol, c, r) %>%
#  pivot_wider(
#    names_from = symbol, names_sep = ".", values_from = c(c, r),
#    values_fn = list
#  ) %>%
#  unchop(everything()) %>%
#  rename(c.DJI = `c.^DJI`) %>%
#  rename(r.DJI = `r.^DJI`)
# scale data by individual variable variance
# ge_prices <- as.data.frame(scale(prices,
#                                center = FALSE,
#                                scale = apply(prices, 2, sd, na.rm = TRUE)
# ))
# prices_stand <- ge_prices %>% relocate(r.DJI, .before = c.GE)
# prices <- prices %>% relocate(r.DJI, .before = c.GE)
# usethis::use_data(prices,prices_stand,internal = TRUE,overwrite = TRUE)
# usethis::use_r('ge_prices')
# usethis::use_r
# ??ge_prices
# usethis::use_vignette(name = 'demo', title = 'IRF

# usethis::use_test(prices)

# split to training and testing
# set.seed(1)
# samp <- sort(sample(nrow(prices_stand), nrow(prices_stand) * .8))
# price_train <- prices_stand[samp, ]
# price_test <- prices_stand[-samp, ]
#
# train data
# colnames(price_train)
# yprice_train <- price_train[1:2]
# head(yprice_train)
# xcprice_train <- price_train[3]
# xrprice_train <- price_train[4]
# yprice_test <- price_test[1:2]

# devtools::load_all()

# model <- IntRF::intrf(int_resp = yprice_train,
#                             cent_pred = xcprice_train,
#                             ran_pred = xrprice_train,
#                             train = price_train,
#                             test = price_test
# )
