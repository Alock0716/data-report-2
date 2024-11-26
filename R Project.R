
library(readr)
OA <- read_csv("C:/Users/Alock/Downloads/OA 11.7 - yelp_academic_dataset_business.json.csv")
library(ggplot2)
ggplot(OA, (aes(x=state))) + geom_bar(stat="count")

head(OA)
ggplot(OA) + geom_histogram(aes(x=state), stat = 'count')

ContingenceyTable <- table(OA$stars)
print(ContingenceyTable)
pie(ContingenceyTable, main='pie chart', col=rainbow(9))

OA1 <- subset(OA, stars == 1)
OA2 <- subset(OA, stars == 2)
OA3 <- subset(OA, stars == 3)
OA4 <- subset(OA, stars == 4)
OA5 <- subset(OA, stars == 5)


ggplot(OA1) + geom_boxplot(aes(y=review_count))
ggplot(OA2) + geom_boxplot(aes(y=review_count))
ggplot(OA3) + geom_boxplot(aes(y=review_count))
ggplot(OA4) + geom_boxplot(aes(y=review_count))
ggplot(OA5) + geom_boxplot(aes(y=review_count))

chisq.test(OA1$stars,OA5$stars) 


# second half user thingies
library(ggplot2)  
library(cluster)  
library(class)
library(readr)
library(caret)
library(corrplot)





# Load the data and print column names
data <- read.csv("C:/Users/Alock/Downloads/OA 11.6 - yelp_academic_dataset_user.json.csv")
print(colnames(data))

# Making correlation Table.
cor_matrix <- cor(data[, c("cool_votes", "funny_votes", "useful_votes")])
print(cor_matrix)

# Plotting the correlation table
corrplot(cor_matrix, method = "color", tl.col = "black")

# Making linear Model
linear_model <- lm(data$funny_votes ~ data$cool_votes)
summary(linear_model)

# Scatter plot
ggplot(data) + geom_point(aes(funny_votes,cool_votes)) + geom_smooth(aes(funny_votes,cool_votes),method="lm") + labs(x="Funny Votes",y="Cool Votes")

# Linear Regression
review_model <- lm(data$fans ~ data$review_count)
summary(review_model)

# Plotting the relationship
ggplot(data, aes(x = review_count, y = fans)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "Linear Regression vs Review Count",
       x = "Review Count",
       y = "Fans")

#review count doesnt correlate with Fans.
#lack of presence around regression line indicates a weak relationship.


# Repeating step 4, but with useful_votes
fan_useful_model <- lm(data$fans ~ data$useful_votes)
summary(fan_useful_model)

# Plotting the relationship between 'fans' and 'useful_votes'
ggplot(data, aes(x = useful_votes, y = fans)) +
  geom_point() +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Linear Regression of Fans vs Useful Votes",
       x = "Useful Votes",
       y = "Fans")

# The graph indicates there is a relationship between Fans and Useful votes due to the more consistent upward trend compared to the other graph.


data_scaled <- scale(data[, c("review_count", "fans")])

# K-means clustering on review_count and fans
# Select 'review_count' and 'fans' columns, use 3 clusters as a starting point
userCluster <- kmeans(data[, c("review_count", "fans")],4)

# Display the number of items in each cluster
table(userCluster$cluster)

# Add the clusters to the original data frame
data$cluster <- userCluster$cluster

# Plot the clusters using base R plot functions2
plot(data$review_count, data$fans, col = data$cluster,
     main = "K-means Clustering for Review Count vs Fans",
     xlab = "Review Count", ylab = "Fans", pch = 19)

# Re-run k-means on useful_votes and fans
user_data <- data[, c("useful_votes", "fans")]
userCluster2 <- kmeans(user_data, 4)

# Display cluster information
table(userCluster2$cluster)

# Add the new clusters to the original data frame
data$cluster2 <- userCluster2$cluster

# Plot the new clusters
plot(data$useful_votes, data$fans, col = data$cluster2,
     main = "K-means Clustering for Useful Votes vs Fans",
     xlab = "Useful Votes", ylab = "Fans", pch = 19)

# Elbow method to find the optimal number of clusters
# Define a function to calculate total within-cluster sum of squares (WCSS)
wcss <- function(k) {
  kmeans(user_data, centers = k, nstart = 10)$tot.withinss
}

# Create a vector for the number of clusters from 1 to 10
k_values <- 1:10

# Calculate WCSS for each value of k
wcss_values <- sapply(k_values, wcss)


plot(k_values, wcss_values, type = "b", col = "blue", pch = 19,
     main = "Elbow Plot for Optimal Clusters",
     xlab = "Number of Clusters (k)", ylab = "Within-Cluster Sum of Squares")