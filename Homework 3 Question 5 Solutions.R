## Part a
library(ISLR)

# Generate the data and add a mean shift to separate it into three classes

x <- matrix(rnorm(60*50), ncol=50)

x[1:20, 1] <- x[1:20, 1] - 10
x[21:40, 1] <- x[21:40, 1] + 10
x[21:40, 2] <- x[21:40, 2] - 20
x[41:60, 2] <- x[41:60, 2] + 20


## Part b


# Run PCA

x_pca <- prcomp(x)

# Plot the first two principal component score vectors

plot(x_pca$x[,1:2], col=rep(1:3, each=20), pch=19)

# The three classes appear well-separated.


## Part c


# Assign the true labels

labels <- rep(1:3, each=20)

# Run k-means and view the cluster assignments

x_kmeans <- kmeans(x, 3, nstart=20)

table(labels, x_kmeans$cluster)

# The algorithm perfectly separated the values into the true clusters.


## Part d


# Run k-means and view the cluster assignments

x_kmeans <- kmeans(x, 2, nstart=20)

table(labels, x_kmeans$cluster)

# We see that all of the observations in two of the true clusters
# have been absorbed into one cluster, with the remaining cluster
# staying the same.


## Part e


# Run k-means and view the cluster assignments

x_kmeans <- kmeans(x, 4, nstart=20)

table(labels, x_kmeans$cluster)

# We see that all of the observations in one of the true clusters
# have been separated into two clusters, with the remaining two clusters
# staying the same.


## Part f


# Run k-means on the first two principal component vectors, and view
# the results

x_kmeans <- kmeans(x_pca$x[,1:2], 3, nstart=20)

table(labels, x_kmeans$cluster)

# We see that once again, all of the observations have been perfectly
# assigned to clusters. This makes sense since representing data by its
# principal components actually makes it less noisy, since the overall signal
# in a data set is often concentrated in its first few principal components.


## Part g


# Run k means on scaled data, and view the results

x_kmeans <- kmeans(scale(x), 3, nstart=20)

table(labels, x_kmeans$cluster)

# We see that are clusters have become inaccurate, with 4-9 observations in
# each cluster being assigned to the srong cluster. This is because the scaling
# has reduced the distance between the observations, negating the mean shift
# present in the original data.