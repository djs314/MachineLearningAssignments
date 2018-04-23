library(ISLR)
library(tree)
library(randomForest)

## Part a


# Take a look at the data

Carseats

# Get the training and test subsets

samp <- sample(nrow(Carseats), nrow(Carseats)/2)

cs_train <- Carseats[samp,]
cs_test <- Carseats[-samp,]


## Part b


# Fit the regression tree

cs_tree <- tree(Sales ~ ., data=cs_train)

# Plot the tree

plot(cs_tree)
text(cs_tree, pretty=0)

# The most important indicator of carseat sales appears to be shelving location, with
# the first branch of the tree distinguishing bad and medium locations from good
# locations. The good locations generally have better sales, of course. The next best
# indicator of sales seems to be price, with that variable being the next decision
# for each branch of the inital split on shelf location. Lower prices result in the
# best sales.

# Predict for the test subset

cs_pred <- predict(cs_tree, newdata=cs_test)

# Get the test MSE

mean((cs_pred - cs_test$Sales)^2)

# The test error rate is about 5.399.


## Part c


# Apply cross validation

cs_cv <- cv.tree(cs_tree)

# Plot the results to see which size tree is being selected by cross validation

plot(cs_cv$size, cs_cv$dev, type="b")

cs_cv$size
cs_cv$dev

# In this case, it appears that cross validation has selected the optimal level
# of tree complexity to be size 11, which is close to the size of the full tree.

# Prune the tree

cs_prune <- prune.tree(cs_tree, best=11)

# Predict for the test subset

cs_prune_pred <- predict(cs_prune, newdata=cs_test)

# Get the test MSE

mean((cs_prune_pred - cs_test$Sales)^2)

# The test error rate is now 5.366, so pruning the tree has slightly improved the
# test error rate.


## Part d


# Apply bagging

cs_bag <- randomForest(Sales ~ ., data=cs_train, mtry=ncol(Carseats)-1, importance=TRUE)

# Predict for the test subset

cs_bag_pred <- predict(cs_bag, newdata=cs_test)

# Get the test MSE

mean((cs_bag_pred - cs_test$Sales)^2)

# The test error rate has improved significantly using bagging, and is now about
# 2.986.

# See which variables are the most important

importance(cs_bag)

# As before, we see that shelving location and price are the most important variables.


## Part e


# Let's try using m = sqrt(p)

m <- ceiling(sqrt(ncol(Carseats) - 1))

# Apply a random forest with m = 4

cs_for <- randomForest(Sales ~ ., data=cs_train, mtry=m, importance=TRUE)

# Predict for the test subset

cs_for_pred <- predict(cs_for, newdata=cs_test)

# Get the test MSE

mean((cs_for_pred - cs_test$Sales)^2)

# The test error rate is now 2.928, which provides a slight improvement over bagging.

# See which variables are the most important

importance(cs_for)

# Again, shelving location and price are the most important variables, which
# makes sense.

# By choosing a value of m = 4 smaller than the number of predictors, we are
# building trees in our forest by limiting the number of options at each split
# to a random sample of size 4. This limits the prevalence of strong predictors
# (such as shelving location and price) taking over in each tree, which in turn
# makes the trees look different from each other, reducing their correlation.
# This reduction in correlation between the trees results in the lower variance
# we see here when we average the uncorrelated trees.