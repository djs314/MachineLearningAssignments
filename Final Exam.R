## Problem 1


## Part a

simulate_dataset <- function(dim, num_data_points)
{
  
  
  #first function draws observations from first population
  sim_pos <- function(num_pos)
  {
    number_ones <- floor(dim*.1) # figure number of 1s by dimension of data
    mu <- c(rep(1, number_ones), rep(0, dim-number_ones)) #create basic mean
    data <- matrix(rnorm(num_pos * dim), nrow=num_pos) + matrix(rep(mu, num_pos), nrow=num_pos, byrow=T) #simulate data
    num_flip <- rbinom(n=1, size=num_pos, prob=.5) #how many to flip
    data[1:num_flip,] <- -1*(data[1:num_flip,]) #so flip them
    data
  }
  
  #second function draws observations from second population
  sim_neg <- function(num_neg)
  {
    number_cauchy <- floor(dim*.1) # figure number of cauchy's by dimension of data
    mat1 <- matrix(rcauchy(num_neg*number_cauchy), nrow=num_neg) #simulate cauchy part
    mat2 <- matrix(rnorm(num_neg*(dim-number_cauchy)), nrow=num_neg) #simulate normal part
    data <- cbind(mat1, mat2) #data
    data
  }
  
  
  y <- rnorm(num_data_points) #flip a coin to determine which population each point will come from
  numpos <- sum(y>0)
  numneg <- num_data_points-numpos
  
  data <- rbind(sim_pos(numpos), sim_neg(numneg))
  data
}

# Get the data
X <- simulate_dataset(100, 500)

# Vector of values for p
p <- c(5, 20, 40, 60, 80, 95)

out <- numeric()
result <- numeric()

for (i in 1:6) {
  for (j in 1:1000) {
    # Generate the random matrix W
    W <- (1/sqrt(p[i])) * matrix(rnorm(100*p[i]), nrow=100)
    
    # Calculate the reduced-dimensional observation matrix
    Y <- X%*%W
    
    # Return the distance statistic
    out[j] <- mean(dist(Y))/mean(dist(X))
  }
  
  result[i] <- mean(out)
}

result

# We have 0.9476 for p = 5, 0.9830 for p = 20, 0.9924 for p = 40,
# 0.9973 for p = 60, 0.9975 for p = 80, and 0.9981 for p = 95.


## Part b

# Simulate the data set
X <- simulate_dataset(250, 500)

# Vector of values for d
d <- c(250, 150, 100, 80, 60, 55)

out <- numeric()
result <- numeric()

for (i in 1:6) {
  # Adjust the original data
  X <- X[,1:d[i]]
  
  for (j in 1:1000) {
    # Generate the random matrix W
    W <- (1/sqrt(50)) * matrix(rnorm(d[i]*50), nrow=d[i])
    
    # Calculate the reduced-dimensional observation matrix
    Y <- X%*%W
    
    # Return the distance statistic
    out[j] <- mean(dist(Y))/mean(dist(X))
  }
  
  result[i] <- mean(out)
}

result

# We have 0.9952 for d = 250, 0.9950 for d = 150, 0.9971 for d = 100,
# 0.9950 for d = 80, 0.9947 for d = 60, and 0.9965 for d = 55.


## Problem 2

library(tidyverse)

load("acting.Rdata")
load("genres.Rdata")

# Merge the data by movie title
data <- acting %>% rename(actor=name, act_id=id, movie_title=title, id=movie_id) %>% inner_join(genres_readable)

# Get the number of actors
n <- length(unique(data$act_id))

# Get the number of movies of each genre that each actor acted in, with the genres forming the columns
# and the actors the rows
data_counts <- group_by(data, act_id, id1) %>% summarise(count=n()) %>% spread(id1, count)
data_counts[is.na(data_counts)] <- 0
data_counts <- data_counts[,2:21]

# Divide each element by the sum of its row
data_counts <- t(apply(t(data_counts), 2, function(x) x/sum(x)))

# Run PCA
data_counts_pca <- prcomp(data_counts)

# Run k-means with 100 clusters on the first two principal components of the data
data_counts_kmeans <- kmeans(data_counts_pca$x[,1:2], 100, nstart=20)

# Plot the clusters on the first two principal components
library(fpc)
plotcluster(data_counts, data_counts_kmeans$cluster)

# Get the total within-cluster sum of squares
data_counts_kmeans$tot.withinss

# 24.280

# Run k-means with 50 clusters on the first two principal components of the data
data_counts_kmeans <- kmeans(data_counts_pca$x[,1:2], 50, nstart=20)

# Plot the clusters on the first two principal components
plotcluster(data_counts, data_counts_kmeans$cluster)

# Get the total within-cluster sum of squares
data_counts_kmeans$tot.withinss

# 49.068

# Run k-means with 200 clusters on the first two principal components of the data
data_counts_kmeans <- kmeans(data_counts_pca$x[,1:2], 200, nstart=20)

# Plot the clusters on the first two principal components
plotcluster(data_counts, data_counts_kmeans$cluster)

# Get the total within-cluster sum of squares
data_counts_kmeans$tot.withinss

# 10.877

# Look at actor 1
data_counts_kmeans$cluster
head(group_by(data, act_id, id1) %>% summarise(count=n()) %>% spread(id1, count))
length(data_counts_kmeans$cluster[data_counts_kmeans$cluster == 101])

# Actor 1 is in cluster 101, which contains 239 actors.


## Problem 3


load("directors.Rdata")

# Trim extraneous data
acting <- acting[,c(1,3)]
genres <- genres_readable[,c(1,3)]
directors <- directors[c(1,3)]

# Rename the variables for convenience
acting <- acting %>% rename(actor_id=id)
genres <- genres %>% rename(movie_id=id, genre_id=id1)
directors <- directors %>% rename(director_id=id)

# Join the three data sets
data <- inner_join(acting, genres)
data <- inner_join(data, directors)

# Convert the response to a categorical variable
data$genre_id <- as.factor(data$genre_id)

# Reassign values
j <- 1
for (i in unique(data$movie_id)) {
  data$movie_id[data$movie_id == i] <- j
  j <- j + 1
}

k <- 1
for (i in unique(data$actor_id)) {
  data$actor_id[data$actor_id == i] <- k
  k <- k + 1
}

l <- 1
for (i in unique(data$director_id)) {
  data$director_id[data$director_id == i] <- l
  l <- l + 1
}

# Split into training and test subsets
samp <- sample(nrow(data), floor(nrow(data)*0.9))

data_train <- data[samp,]
data_test <- data[-samp,]

# Try LDA
library(MASS)

lda_fit <- lda(genre_id ~ ., data=data_train)
lda_pred <- predict(lda_fit, newdata=dplyr::select(data_test, -genre_id))

lda_pred$class
length(unique(lda_pred$class))

# LDA classified everything as genre 18, 28, or 35

accuracy <- table(lda_pred$class, data_test$genre_id)
1 - (sum(diag(accuracy))/sum(accuracy))

# The misclassification rate is 0.8094

# Try QDA
qda_fit <- qda(genre_id ~ ., data=data_train)
qda_pred <- predict(qda_fit, newdata=dplyr::select(data_test, -genre_id))

qda_pred$class
length(unique(qda_pred$class))

# QDA classified in five different genres

accuracy <- table(qda_pred$class, data_test$genre_id)
1 - (sum(diag(accuracy))/sum(accuracy))

# The misclassification rate is 0.8072

# Try pruned classification tree
library(tree)

tree_fit <- tree(genre_id ~ ., data=data_train)

# Use cross-validation to determine the optimal size of the tree
tree_cv <- cv.tree(tree_fit)

tree_cv$size
tree_cv$dev

# A size of 2 is best

# Prune the tree
tree_prune <- prune.misclass(tree_fit, best=2)

tree_pred <- predict(tree_prune, newdata=data_test, type="class")
tree_pred

length(unique(tree_pred))

# Using a pruned tree results in everything being classified as genre
# 18 or 28

accuracy <- table(tree_pred, data_test$genre_id)
1 - (sum(diag(accuracy))/sum(accuracy))

# The misclassification rate is 0.8148

# Try bagging and random forests
library(randomForest)

bag_fit <- randomForest(genre_id ~ ., data=data_train, mtry=3, ntree=100)
bag_pred <- predict(bag_fit, newdata=data_test)

accuracy <- table(bag_pred, data_test$genre_id)
1 - (sum(diag(accuracy))/sum(accuracy))

# More genres were predicted, but the misclassification rate is worse, at 0.9204

forest_fit <- randomForest(genre_id ~ ., data=data_train, mtry=2, ntree=100)
forest_pred <- predict(forest_fit, newdata=data_test)

accuracy <- table(forest_pred, data_test$genre_id)
1 - (sum(diag(accuracy))/sum(accuracy))

# The misclassification rate is slightly better, at 0.8916, but still worse
# than other methods