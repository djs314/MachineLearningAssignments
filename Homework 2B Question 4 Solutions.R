## Machine Learning Homework 2, Part B, Question 3

library(ggplot2)
library(tidyverse)
library(splines)

# Load training and test data
load("oilimportstrain.rdata")
load("oilimportstestCPI.rdata")

# Take a look at the data
ggplot(train, aes(x=CPI, y=TotVal1000)) + geom_point()

# Because the data is changing so rapidly, using a step function
# would not be a good idea. We proceed with polynomial regression,
# regression splines, natural splines, and smoothing splines.
# Although the data changes rapidly in some places, overall it
# appears cubic, so we will start with polynomial regression
# using polynomials of at most degree 3.

# Divide the training data into training and test subsets
n_rows <- nrow(train)
oil_test_sample <- sample(n_rows, size=floor(n_rows*0.1))
oil_test <- train[oil_test_sample,]
oil_train <- train[-oil_test_sample,]


## Fit using polynomial regression
poly_1 <- lm(TotVal1000 ~ CPI, data=oil_train)
poly_2 <- lm(TotVal1000 ~ poly(CPI, 2), data=oil_train)
poly_3 <- lm(TotVal1000 ~ poly(CPI, 3), data=oil_train)

poly1_predict <- predict(poly_1, newdata=select(oil_test, -TotVal1000))
poly2_predict <- predict(poly_2, newdata=select(oil_test, -TotVal1000))
poly3_predict <- predict(poly_3, newdata=select(oil_test, -TotVal1000))

# Get the RMSE for polynomial regression
poly1_rmse <- sqrt(mean((poly1_predict - oil_test$TotVal1000)^2))
poly2_rmse <- sqrt(mean((poly2_predict - oil_test$TotVal1000)^2))
poly3_rmse <- sqrt(mean((poly3_predict - oil_test$TotVal1000)^2))
c(poly1_rmse, poly2_rmse, poly3_rmse)

# The RMSE for linear regression is 3,218,491, the RMSE for quadratic regression
# is 2,305,696, and the RMSE for cubic regression is the best, at 1,000,971.


## Fit using cubic regression splines with three knots
reg_spline_fit <- lm(TotVal1000 ~ bs(CPI, df=6), data=oil_train)
reg_spline_predict <- predict(reg_spline_fit, newdata=select(oil_test, -TotVal1000))

# Get the RMSE for fitting using regression splines
reg_spline_rmse <- sqrt(mean((reg_spline_predict - oil_test$TotVal1000)^2))
reg_spline_rmse

# The RMSE for fitting using regression splines is 934,360.1.


## Fit using a natural cubic spline with three knots
nat_spline_fit <- lm(TotVal1000 ~ ns(CPI, df=4), data=oil_train)
nat_spline_predict <- predict(nat_spline_fit, newdata=select(oil_test, -TotVal1000))

# Get the RMSE for fitting using regression splines
nat_spline_rmse <- sqrt(mean((nat_spline_predict - oil_test$TotVal1000)^2))
nat_spline_rmse

# The RMSE for fitting using a natural spline is 1,051,192.


## Fit using a smoothing spline
smooth_spline_fit <- smooth.spline(oil_train$CPI, oil_train$TotVal1000, cv=TRUE)
smooth_spline_predict <- predict(smooth_spline_fit, x=select(oil_test, -TotVal1000))$y

# Get the RMSE for fitting using a smoothing spline
smooth_spline_rmse <- sqrt(mean((smooth_spline_predict - oil_test$TotVal1000)^2))
smooth_spline_rmse

# The RMSE for fitting using a smoothing spline is 723,677.8.

# Fitting using a smoothing spline is the best, as it results in the lowest RMSE.


## Predict for the actual test data using a smoothing spline
ypred <- predict(smooth_spline_fit, x=testCPI)$y