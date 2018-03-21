# Read in data

x <- read.csv("Xvals.csv", header=FALSE)
y <- read.csv("Yvals.csv", header=FALSE)

# Display the beginning of the predictor values, and get the number of rows and columns

x
nrow(x)
ncol(x)

# Add the response variable to the predictor data frame

x$V9 <- y$V1

# Plot relationships between variables

pairs(x)

plot(x$V1, x$V9)
plot(x$V2, x$V9)
plot(x$V3, x$V9)
plot(x$V4, x$V9)
plot(x$V5, x$V9)
plot(x$V6, x$V9)
plot(x$V7, x$V9)
plot(x$V8, x$V9)

# Check correlations

cor(x[,1:8])

# Convert the eighth predictor to a categorical variable

x$V8 <- factor(x$V8)

# Run regression with all variables

fit <- lm(V9 ~ ., data=x)
summary(fit)
plot(fit)

# Run regression with fifth predictor removed

fit2 <- lm(V9 ~ . -V5, data=x)
summary(fit2)
plot(fit2)

# Try a transformation on the first predictor

plot(x$V1, x$V9)
plot(log(x$V1), x$V9)
plot(sqrt(x$V1), x$V9)

x$logV1 <- log(x$V1)
x[!is.finite(x$logV1), 10] <- 0

fit3 <- lm(V9 ~ logV1 + V2 + V3 + V4 + V5 + V6 + V7 + V8, data=x)
summary(fit3)

# Remove insignificant variables

fit4 <- lm(V9 ~ logV1 + V2 + V3 + V4 + V5 + V6 + V8, data=x)
summary(fit4)

fit5 <- lm(V9 ~ logV1 + V2 + V3 + V4 + V6 + V8, data=x)
summary(fit5)

fit6 <- lm(V9 ~ logV1 + V2 + V3 + V4 + V8, data=x)
summary(fit6)

# Check the summary for the previous fit again

summary(fit2)

# Check the residual plots

plot(fit6)

# Check if any further transformations would help

plot(x$V2, x$V9)
plot(x$V3, x$V9)
plot(x$V4, x$V9)

# Predict using our final model and calculate the training MSE

pred <- predict.lm(fit6)

mean((pred - x$V9)^2)

# The MSE is 46.982