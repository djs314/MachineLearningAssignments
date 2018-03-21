library(boot)

## Part b

# Get our data

data1 <- rnorm(50, mean=100, sd=10)
data2 <- rnorm(5, mean=120, sd=50)
data <- c(data1, data2)

# Use the bootstrap to estimate the standard error of the trimmed mean

boot.fn <- function(data, index) {
  d <- data[index]
  mean(d, trim=0.1)
}

boot(data, boot.fn, 10000)

# The standard error of the trimmed mean is 1.6220.

## Part ci

# Get 100,000 data sets

data <- replicate(100000, c(rnorm(50, mean=100, sd=10), rnorm(5, mean=120, sd=50)))

# Calculate the trimmed mean of each data set and find the standard error

t_means <- apply(data, MARGIN=2, FUN=mean, trim=0.1)
sd(t_means)

# The standard error of the trimmed means is 1.5551.

## Part cii

# Get 100 data sets

data2 <- replicate(100, c(rnorm(50, mean=100, sd=10), rnorm(5, mean=120, sd=50)))

# Estimate the standard error of the trimmed mean of each data set
# using the bootstrap

boot_means <- apply(data2, MARGIN=2, FUN=boot, statistic=boot.fn, R=10000)

# Get the standard errors

boot_errors <- numeric()
for (i in 1:100) {
  boot_errors[i] <- sd(boot_means[i][[1]]$t)
}

## Part ciii

# Calculate the bias

mean(boot_errors - sd(t_means))

# The bias is 0.1088.

# Calculate the variance

var(boot_errors)

# The variance is 0.0604.