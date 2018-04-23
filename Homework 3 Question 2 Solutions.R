library(ISLR)
library(gbm)
library(glmnet)
library(randomForest)


## Part a


# Remove rows with missing values

hit <- Hitters[complete.cases(Hitters),]

# Log-transform the salaries

hit$Salary <- log(hit$Salary)


## Part b


# Get the training and test subsets

samp <- 1:200

hit_train <- hit[samp,]
hit_test <- hit[-samp,]


## Part c


# Get the lambda values

l <- seq(0.005, 0.4, by=0.005)

# Create a vector to hold the training MSE's

train_mse <- numeric()

# For each value of lambda, perform boosting with 1000 trees, and calculate the
# training MSE.

for(i in 1:length(l)) {
  hit_gbm <- gbm(Salary ~ ., data=hit_train, distribution="gaussian", n.trees=1000, interaction.depth=4, shrinkage=l[i])
  train_pred <- predict(hit_gbm, newdata=hit_train, n.trees=1000)
  train_mse[i] <- mean((train_pred - hit_train$Salary)^2)
}

plot(l, train_mse)

# We see that the training MSE is high for small values of lambda, and quickly
# decreases for larger values of lambda, leveling out at close to 0 around lambda = 0.1.


## Part d


# Create a vector to hold the test MSE's

test_mse <- numeric()

# For each value of lambda, predict for the test subset, and calculate the test MSE.

for(i in 1:length(l)) {
  hit_gbm <- gbm(Salary ~ ., data=hit_train, distribution="gaussian", n.trees=1000, interaction.depth=4, shrinkage=l[i])
  test_pred <- predict(hit_gbm, newdata=hit_test, n.trees=1000)
  test_mse[i] <- mean((test_pred - hit_test$Salary)^2)
}

plot(l, test_mse)

# The test MSE's are quite scattered, but are generally smaller for values of lambda around
# 0.05 to 0.1, and tend to grow larger at higher values of lambda. The lowest test MSE
# appears to be for a value of lambda around 0.04.


## Part e


# The lowest test MSE for boosting appears to be about 0.255. We proceed to apply linear
# and ridge regression to the data, and compare the MSE's obtained to that of boosting.

# Perform linear regression

hit_lm <- lm(Salary ~ ., data=hit_train)

# Get predictions for the test subset

hit_lm_pred <- predict.lm(hit_lm, newdata=hit_test)

# Calculate the test MSE for linear regression

mean((hit_lm_pred - hit_test$Salary)^2)

# The test MSE for linear regression is 0.4918.

# Now, we apply ridge regression to the data

# Separate the predictors and response variable, and only consider numeric columns

hit_vars <- hit_train[, -c(14,15,19,20)]
hit_resp <- hit_train$Salary

# Perform ridge regression

hit_ridge <- glmnet(as.matrix(hit_vars), hit_resp, alpha=0)

# Use cross validation to determine the optimal value of lambda

cv_ridge <- cv.glmnet(as.matrix(hit_vars), hit_resp, alpha=0)
ridge_lambda <- cv_ridge$lambda.min
ridge_lambda

# The best value of lambda chosen by cross validation is 0.3717.

# Predict sale price for the test subset

hit_ridge_pred <- predict(hit_ridge, s=ridge_lambda, newx=as.matrix(hit_test[-c(14,15,19,20)]))

# Get the test MSE for ridge regression

mean((hit_ridge_pred - hit_test$Salary)^2)

# The test MSE for ridge regression is 0.4639, slightly better than linear regression.

# We see that boosting performed significantly better than linear and ridge
# regression, with a test MSE of about 0.255 at the best value of lambda compared
# to test MSE's of 0.4918 and 0.4639 for linear and ridge regression, respectively.


## Part f


# Run boosting with a lambda of 0.4

hit_gbm <- gbm(Salary ~ ., data=hit_train, distribution="gaussian", n.trees=1000, interaction.depth=4, shrinkage=0.4)

# View the most important predictors

summary(hit_gbm)

# We see that CAtBat is the most important predictor by a wide margin, followed
# by CWalks.


## Part g


# Apply bagging

hit_bag <- randomForest(Salary ~ ., data=hit_train, mtry=ncol(hit_train)-1)

# Predict for the test subset

hit_bag_pred <- predict(hit_bag, newdata=hit_test)

# Get the test MSE for bagging

mean((hit_bag_pred - hit_test$Salary)^2)

# The test MSE for bagging is 0.2289, which is slightly better than the test
# MSE of about 0.255 for boosting.