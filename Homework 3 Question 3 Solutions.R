library(e1071)


## Part a


x1 <- runif(500) - 0.5
x2 <- runif(500) - 0.5
y <- 1 * (x1^2 - x2^2 > 0)


## Part b


# Plot the data

plot(x1, x2, col=as.factor(y))

# We see that the boundary between the two classes is nonlinear.


## Part c


# Get the data in a dataframe

data <- data.frame(x1, x2, y)

# Run logistic regression

data_log <- glm(y ~ ., data=data, family="binomial")


## Part d


# Predict for the training data

data_log_pred <- predict(data_log, newdata=data, type="response")

# Set the decision boundary

data_log_pred_class <- rep(0, 500)
data_log_pred_class[data_log_pred >= 0.5] <- 1

# Plot the predictions

plot(x1, x2, col=as.factor(data_log_pred_class))

# We see that the decision boundary is linear, and classifies poorly.


## Part e


# Fit a logistic regression model to nonlinear functions of x1 and x2

data_log_nl <- glm(y ~ I(x1^2) + x1*x2 + I(x2^2), data=data, family="binomial")


## Part f


# Predict for the training data

data_log_nl_pred <- predict(data_log_nl, newdata=data, type="response")

# Set the decision boundary

data_log_nl_pred_class <- rep(0, 500)
data_log_nl_pred_class[data_log_nl_pred >= 0.5] <- 1

# Plot the predictions

plot(x1, x2, col=as.factor(data_log_nl_pred_class))

# We see that the decision boundary is nonlinear, and is very close to the
# true boundary.


## Part g


# Convert the response to a factor variable

data$y <- as.factor(data$y)

# Perform cross validation to find the best value of cost

data_tune <- tune(svm, y ~ ., data=data, kernel="linear", ranges=list(cost=c(0.001,
                  0.01, 0.1, 1, 5, 10, 100)))

# Predict using the best model

data_tune_pred <- predict(data_tune$best.model, data)

# Plot the results

plot(x1, x2, col=data_tune_pred)

# We see that the support vector classifier assigned all data points to one class.


## Part h


# Perform cross validation to find the best value of cost

data_svm_tune <- tune(svm, y ~ ., data=data, kernel="radial", ranges=list(cost=c(0.1,
                      1, 10, 100, 1000), gamma=c(0.5, 1, 2, 3, 4)))

# Predict using the best model

data_svm_pred <- predict(data_svm_tune$best.model, data)

# Plot the results

plot(x1, x2, col=data_svm_pred)

# We see that the SVM aligned the decision boundary very close to the true
# boundary.


## Part i


# We can see from these results that support vector machines and logistic
# regression using nonlinear functions of the variables are both very useful
# for classifying data with a nonlinear decision boundary, while normal
# logistic regression and supoort vector classifiers are poor at classifying
# such data. Using logistic regression rather than a support vector machine
# might require more guesswork to find a good nonlinear function of the
# variables, while with a support vector machine, cross validation can be
# used to find the optimal values of the paramters that can be tuned.