## Machine Learning Homework 1, Problem 2

## a.

# Function that simulates y = 0.2x + sin(x) + noise

gety <- function(x) {
  size <- length(x)
  0.2*x + sin(x) + rnorm(size)
}

## b.

y.new <- numeric()
f.hat.7 <- numeric()
f.hat.7.mse <- numeric()

for(i in 1:1000) {
  x <- seq(-10, 10, by=0.1)
  y <- gety(x) # Simulate data
  data <- data.frame(x=x, y=y) # Put data into a data frame
  f.hat <- lm(y ~ x, data) # Calculate the line of best fit
  f.hat.b0 <- f.hat[[1]][1] # Get the coefficients of the line
  f.hat.b1 <- f.hat[[1]][2]
  y.new[i] <- gety(7) # Get a y value for x = 7
  f.hat.7[i] <- f.hat.b0 + f.hat.b1*7 # Calculate f-hat for x = 7
  f.hat.7.mse[i] <- (y.new[i] - f.hat.7[i])^2 # Get the MSE for x = 7
}

## c.

# Get the average prediction at x = 7

f.bar <- mean(f.hat.7)

## d.

# Calculate the variance of y-hat at x = 7

f.hat.var <- mean((f.hat.7 - f.bar)^2)

## e.

# Calculate the bias at x = 7

bias <- 0.2*7 + sin(7) - f.bar

## f.

# Calculate the left-hand side and right-hand side of the theoretical
# formula using the quantities from our simulation, and compare

LHS <- mean(f.hat.7.mse)
RHS <- mean((y.new - 0.2*7 - sin(7))^2) + bias^2 + f.hat.var
abs(LHS - RHS)

## The difference between the two sides of the theoretical equation is 0.00325,
## which is small, so our simulation helps verify the equation.

## g.

## By only using half of the x values to find the line of best fit, we would
## decrease variability of the function estimate, since we would avoid overfitting
## the data with less points available, but we would increase the bias, since
## using less data would restrict our model by not being as good of an estimate
## of the population.