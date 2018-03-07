## Machine Learning Homework 2, Part B, Question 3

library(tidyverse)
library(glmnet)


# Load training and test data
load("hw2simdatatrain.rdata")
load("hw2simdatatest.rdata")

# Add the y values to the data for OLS
traindata <- cbind(traindata, y)

# Divide the training data into training and test subsets
n_rows <- nrow(traindata)
data_test_sample <- sample(n_rows, size=floor(n_rows*0.1))
data_test <- data.frame(traindata[data_test_sample,])
data_train <- data.frame(traindata[-data_test_sample,])


## Run ordinary least squares
data_ols <- lm(y~., data=data_train)
data_ols_predict <- predict(data_ols, newdata=select(data_test, -y))

# Get the RMSE for OLS
data_ols_rmse <- sqrt(mean((data_ols_predict - data_test$y)^2))
data_ols_rmse

# The RMSE for ordinary least squares is 2.573475


## Separate the y values for the training set for ridge and lasso
data_train_x <- select(data_train, -y)
data_train_y <- data_train$y

# Run ridge regression
data_ridge <- glmnet(as.matrix(data_train_x), data_train_y, alpha=0)

# Run cross validation for ridge regression
data_ridge_cv <- cv.glmnet(as.matrix(data_train_x), data_train_y, alpha=0)
data_ridge_cv$lambda.min

# The best value of lambda is 1.353319

# Use the best value of lambda to predict with ridge regression
data_ridge_predict <- predict(data_ridge, s=data_ridge_cv$lambda.min, newx=as.matrix(select(data_test, -y)))

# Get the RMSE for ridge regression
data_ridge_rmse <- sqrt(mean((data_ridge_predict - data_test$y)^2))
data_ridge_rmse

# The RMSE for ridge regression is 2.121225


## Run lasso regression
data_lasso <- glmnet(as.matrix(data_train_x), data_train_y, alpha=1)

# Run cross validation for lasso regression
data_lasso_cv <- cv.glmnet(as.matrix(data_train_x), data_train_y, alpha=1)
data_lasso_cv$lambda.min

# The best value of lambda is 0.04642521

# Use the best value of lambda to predict with lasso regression
data_lasso_predict <- predict(data_lasso, s=data_lasso_cv$lambda.min, newx=as.matrix(select(data_test, -y)))

# Get the RMSE for lasso regression
data_lasso_rmse <- sqrt(mean((data_lasso_predict - data_test$y)^2))
data_lasso_rmse

# The RMSE for lasso regression is 1.005977

# Lasso regression is the best model to choose, as it has the smallest RMSE


## Predict for the actual test data using lasso regression
ypred <- predict(data_lasso, s=data_lasso_cv$lambda.min, newx=as.matrix(testdata))