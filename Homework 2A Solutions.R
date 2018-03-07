## Machine Learning Homework 2, Part A

library(ggplot2)


## 1.


# Function that calculates the statistic for a data set using the bootstrap
# resampling method
myboot <- function(data, statistic=mean, R) {
  
  stat <- numeric(length=R)
  n <- length(data)
  
  for(i in 1:R) {
    sample.boot <- sample(data, size=n, replace=TRUE)
    stat[i] <- statistic(sample.boot)
  }
  
  sd(stat)
}


## 2a.


n <- 30

normal_data <- rnorm(n, mean=0, sd=sqrt(10))
normal_mean <- mean(normal_data)
normal_mean

# The mean of the single sample is 0.1002.

normal_se <- myboot(normal_data, R=10000)
normal_se

# The standard error of the single sample is estimated as 0.5758.

# I am running 10,000 bootstrap samples. As the number of samples is increased, the bootstrap
# estimate of the standard error will approach the actual standard error of the population mean.

means <- numeric(length=100)
std_errors <- numeric(length=100)

for (i in 1:100) {
  n_data <- rnorm(n, mean=0, sd=sqrt(10))
  means[i] <- mean(n_data)
  std_errors[i] <- myboot(n_data, R=10000)
}

# Plot the standard errors with a histogram
ggplot() + aes(x=std_errors) + geom_histogram(bins=20, colour="black", fill="white")

# The distribution of standard errors vaguely resembles a normal distribution. It is
# mostly symmetric and slightly bell-shaped, but it is wide and does not shrink too
# quickly near the tails. WIth a higher number of standard errors, it would more closely
# resemble a normal distribution. The theoretical standard error is sqrt(10)/sqrt(30) =
# 0.5774, which is approximately the center of the graphed data.

t <- abs(qt(0.025, n-1))
lower <- means - t*std_errors
upper <- means + t*std_errors
not_contains0 <- length(lower[lower > 0]) + length(upper[upper < 0])
100 - not_contains0

# 97 of the 100 confidence intervals contain 0, which is reasonable.


## 2b.


# I changed n above to be 100, re-ran the code from part 2a, and obtained the following results.

# The mean of the single sample is -0.3397.

# The standard error of the single sample is estimated as 0.2763.

# Looking at the histogram, the distribution is much less wide, becoming much thinner at
# the tails. It more closely resembles a normal distribution, with the bell shape much more
# evident. There are still some fluctuations in the data, but the approximate normality
# is much clearer than before. These standard errors also span a much shorter interval,
# between about 0.2 to 0.4, compared to the distribution having values from about 0.4 to 0.8
# when n was equal to 30. The theoretical standard error is now sqrt(10)/sqrt(100) =
# 0.3162, which is right around the peak of the curve.

# 96 of the 100 confidence intervals contain 0, which is reasonable.


# I changed n above to be 200, re-ran the code from part 2a, and obtained the following results.

# The mean of the single sample is 0.06194.

# The standard error of the single sample is estimated as 0.2471.

# This histogram looks about the same as the histogram when n was 100. The main differences
# are that the data spans an even shorter interval, about 0.2 to 0.25, and there seems to be
# less fluctuation between adjacent bars, due to the higher number of observations. The
# standard errors appear quite normal at this point. The theoretical standard error is
# sqrt(10)/sqrt(200) = 0.2236, which again matches the center of the distribution quite well.

# 93 of the 100 confidence intervals contain 0, which is reasonable.


## 2c.


# As I observed above, the histograms of the standard errors were shrinking horizontally
# quite rapidly as n increased. This indicates that the variance of the distribution of
# standard errors gets smaller as the sample size increases, leading to higher accuracy
# for any individual bootstrapped standard error.


## 2d.

# The bootstrap is an effective method of estimating the uncertainty of an estimator or
# method. It works by drawing large samples of the data with replacement, as opposed to
# cross validation, which looks at subsets of the data. That way, simulations can be run
# on subsets of the data without reducing the sample size, leading to more relevant
# estimates. As with any statistical method, more accurate results are obtained with a
# larger number of iterations and larger sample sizes. The bootstrap could feasibly be
# applied to any method without difficulty, including those for which variability cannot be
# easily calculated, making it versatile and powerful.