# Read in the data
load("movie_genreID_genre.Rdata")

library(tidyverse)
ratings <- read_csv("movie_rating_data.csv")

# Merge the data by movie ID
data <- ratings %>% rename(movie_id=movieId) %>% inner_join(movie_genre)

# Get the number of users
n <- length(unique(data$userId))

# There are 265,847 users.

# Get the number of genres
n_genre <- length(unique(data$genre_id))

# There are 20 genres.


# Get the number of movies of each genre that each user watched, with the genres forming the columns
# and the users the rows
data_counts <- group_by(data, userId, genre_id) %>% summarise(count=n()) %>% spread(genre_id, count)
data_counts[is.na(data_counts)] <- 0
data_counts <- data_counts[,2:21]

# Standardize the data
data_counts <- scale(data_counts)

# Run PCA
data_counts_pca <- prcomp(data_counts)

# Run k-means with 100 clusters on the first two principal components of the data
data_counts_kmeans <- kmeans(data_counts_pca$x[,1:2], 100, nstart=20)

# Plot the clusters on the first two principal components
library(fpc)
plotcluster(data_counts, data_counts_kmeans$cluster)

# Get the total within-cluster sum of squares
data_counts_kmeans$tot.withinss

# The error is 89,735.53.

# Look at user 1
data_counts_kmeans$cluster
head(group_by(data, userId, genre_id) %>% summarise(count=n()) %>% spread(genre_id, count))
length(data_counts_kmeans$cluster[data_counts_kmeans$cluster == 22])


# Get the average rating movies of each genre that each user watched, with the genres forming the columns
# and the users the rows
data_avgrate <- group_by(data, userId, genre_id) %>% summarise(rate=mean(rating)) %>% spread(genre_id, rate)
data_avgrate[is.na(data_avgrate)] <- 0
data_avgrate <- data_avgrate[,2:21]

# Standardize the data
data_avgrate <- scale(data_avgrate)

# Run PCA
data_avgrate_pca <- prcomp(data_avgrate)

# Run k-means with 100 clusters on the first two principal components of the data
data_avgrate_kmeans <- kmeans(data_avgrate_pca$x[,1:2], 100, nstart=20)

# Plot the clusters on the first two principal components
plotcluster(data_avgrate, data_avgrate_kmeans$cluster)

# Get the total within-cluster sum of squares
data_avgrate_kmeans$tot.withinss

# The error is 21,785.88.

# Look at user 1
data_avgrate_kmeans$cluster
head(group_by(data, userId, genre_id) %>% summarise(rate=mean(rating)) %>% spread(genre_id, rate))
length(data_avgrate_kmeans$cluster[data_avgrate_kmeans$cluster == 13])


# Get the average rating of movies of each genre times the number of movies in each genre that each user
# watched, with the genres forming the columns and the users the rows
data_comb <- group_by(data, userId, genre_id) %>% summarise(comb=mean(rating)*n()) %>% spread(genre_id, comb)
data_comb[is.na(data_comb)] <- 0
data_comb <- data_comb[,2:21]

# Standardize the data
data_comb <- scale(data_comb)

# Run PCA
data_comb_pca <- prcomp(data_comb)

# Run k-means with 100 clusters on the first two principal components of the data
data_comb_kmeans <- kmeans(data_comb_pca$x[,1:2], 100, nstart=20)

# Plot the clusters on the first two principal components
plotcluster(data_comb, data_comb_kmeans$cluster)

# Get the total within-cluster sum of squares
data_comb_kmeans$tot.withinss

# The error is 89,575.69.

# Look at user 1
data_comb_kmeans$cluster
head(group_by(data, userId, genre_id) %>% summarise(comb=mean(rating)*n()) %>% spread(genre_id, comb))
length(data_avgrate_kmeans$cluster[data_avgrate_kmeans$cluster == 53])