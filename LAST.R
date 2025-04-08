setwd('C:/Users/rakhymzhan.seiitkhan/rstudio/kaspianalysis')

library(psych)
library(tidyverse)
library(GPArotation)
library(corrplot)
library(lavaan)
library(semPlot)
library(factoextra)
library(corrr)
library(ggcorrplot)
library(rpart)
library(scales)
library(gridExtra)
library(rpart.plot)
library(caret)
#kaspi2
data <-  read.csv('kaspi15_02_2024.csv')

datatree <- data %>%  rename(Sales = Total) %>% select(-c('X','Link','Name','Item','Model','LowestPrice')) %>% replace(is.na(.), 0)%>% mutate(
  LGPriceDifferencefromavgtop24 = LGElectronicKazakhstanPrice - AvgPriceTop24) %>% select(-c('ProductName','AveragePrice','Kaspi','OBS','purchased','addedtocart','AvgPriceInCategory','AvgPriceTop24'))

dataclean <- data %>% rename(Sales = Total) %>% select(-c('X','Link','Name','Item','Model','LowestPrice')) %>% replace(is.na(.), 0)%>% mutate(
  LGPriceDifferencefromavgtop24 = LGElectronicKazakhstanPrice - AvgPriceTop24) %>% select(-c('ProductName','Category','AveragePrice','Kaspi','OBS','purchased','addedtocart','AvgPriceInCategory','AvgPriceTop24'))


# ----------------------------------------------------------------------------------- CLUSTERS
dataclean <- as.data.frame(lapply(dataclean,scale))

fviz_nbclust(dataclean, kmeans, method = "wss")

k2 <- kmeans(dataclean, centers = 3, nstart = 25)
k3 <- kmeans(dataclean, centers = 4, nstart = 25)
k4 <- kmeans(dataclean, centers = 5, nstart = 25)
k5 <- kmeans(dataclean, centers = 6, nstart = 25)

# plots to compare
p1 <- fviz_cluster(k2, geom = "point", data = dataclean) + ggtitle("k = 3")
p2 <- fviz_cluster(k3, geom = "point",  data = dataclean) + ggtitle("k = 4")
p3 <- fviz_cluster(k4, geom = "point",  data = dataclean) + ggtitle("k = 5")
p4 <- fviz_cluster(k5, geom = "point",  data = dataclean) + ggtitle("k = 6")
grid.arrange(p1, p2, p3, p4, nrow = 2)

set.seed(123)
# ----------------------------------------------------------------------------------- PCA AND VIS and KLUSTER
# Perform PCA
pca_result <- prcomp(dataclean, scale. = TRUE)
summary(pca_result)
pca_data <- data.frame(pca_result$x[, 1:7])
set.seed(123) # Set seed for reproducibility
fviz_nbclust(pca_data, kmeans, method = "wss") # Elbow method
k_means_result <- kmeans(pca_data, centers =4, nstart = 25)



# You can now analyze the clusters to understand how products are categorized


fviz_pca_biplot(pca_result,              # Visualize clusters in biplot
                        col.var = "black",
                        alpha.var = 0.6,
                        label = "var",
                        habillage = k_means_result$cluster,
                        repel = TRUE,
                        addEllipses = TRUE,
                        ellipse.type = "convex",
                        labelsize = 5) +
  guides(color = guide_legend(override.aes = list(label = "")))


# Add cluster assignments to your data
dataclean$cluster <- k_means_result$cluster


# ----------------------------------------------------------------------------------- DECISION TREE FOR SALES


dt <-rpart(Sales ~ . , # set the task as a classification problem
           data = datatree)
# Extract the rules
rpart.rules(dt, roundint=FALSE, clip.facs=TRUE) 
rpart.plot(dt, box.palette="RdBu", shadow.col="gray", nn=TRUE)

# ----------------------------------------------------------------------------------- DECISION TREE FOR CLUSTERS


datatree$cluster <- k_means_result$cluster
dt <-rpart(as.factor(cluster) ~ . , # set the task as a classification problem
           data = datatree)
# Extract the rules
rpart.rules(dt, roundint=FALSE, clip.facs=TRUE) 
rpart.plot(dt, box.palette="RdBu", shadow.col="gray", nn=TRUE)


# ----------------------------------------------------------------------------------- CLUSTER INTERPRET


# 
# ggplot(datatree, aes(LGElectronicKazakhstanPrice,Full.PLP.Ranking, color = as.factor(Category))) +
#   geom_point(alpha = 1, size =2 ) +
#   xlab("LGElectronicKazakhstanPrice") +
#   ylab("Full.PLP.Ranking")




#_________________________________________________________________ Plots




data <- data %>% rename(Sales = Total) %>% select(-c('X','Link','Name','Item','Model')) %>% replace(is.na(.), 0)%>% mutate(
  LGPriceDifferencefromavgtop24 = LGElectronicKazakhstanPrice - AvgPriceTop24)


data$cluster <- k_means_result$cluster


write.csv(data,'productslist2.csv',fileEncoding='UTF-8')


means = data %>% 
  group_by(cluster) %>%
  summarise_all("mean")



# ----------------------------- Prepare Original Dataset -----------------------------
# Assuming 'dataclean' is already loaded and contains cluster labels
# Ensure cluster labels are factors
dataclean$cluster <- as.factor(dataclean$cluster)

# Split data into training and test sets
set.seed(123)  # For reproducibility
trainingIndex <- createDataPartition(dataclean$cluster, p = .8, list = FALSE, times = 1)
trainData <- dataclean[trainingIndex, ]
testData <- dataclean[-trainingIndex, ]

# ----------------------------- Train Classification Model -----------------------------
# Train a decision tree model
model <- rpart(cluster ~ ., data = trainData, method = "class")

# Predict on test data to evaluate the model
testPredictions <- predict(model, testData, type = "class")
confusionMatrix(testPredictions, testData$cluster)

# ----------------------------- Prepare New Data -----------------------------
# Load new data - Ensure this matches the format of your original data before PCA/clustering
new_data <- read.csv('kaspi14_02_2024.csv')

# Preprocess new data as per your original preprocessing steps
# Here, you'd include steps like scaling, PCA transformation, etc., similar to your original dataset preparation
# For demonstration, let's assume it's ready for prediction and named 'new_data_ready'

new_data_ready <- new_data %>% rename(Sales = Total) %>% select(-c('X','Link','Index_x','Page_x','Page_y','Index_y','Name','Date','Item','Model','LowestPrice')) %>% replace(is.na(.), 0)%>% mutate(
  LGPriceDifferencefromavgtop24 = LGElectronicKazakhstanPrice - AvgPriceTop24) %>% select(-c('ProductName','Category','AveragePrice','Price','Kaspi','OBS','purchased','addedtocart','AvgPriceInCategory','AvgPriceTop24'))
new_data_ready <- prcomp(new_data_ready, scale. = TRUE)
summary(new_data_ready)
new_data_ready <- data.frame(new_data_ready$x[, 1:7])

# Placeholder for actual preprocessing steps

# ----------------------------- Classify New Products -----------------------------
# Predict the cluster assignments for new products
newProductClusters <- predict(model, new_data_ready, type = "class")



new_data_unload <- new_data %>% rename(Sales = Total) %>% select(-c('X','Link','Index_x','Page_x','Page_y','Index_y','Name','Date','Item','Model','LowestPrice')) %>% replace(is.na(.), 0)%>% mutate(
  LGPriceDifferencefromavgtop24 = LGElectronicKazakhstanPrice - AvgPriceTop24)

# Add the predicted cluster labels to the new products data
new_data_unload$PredictedCluster <- newProductClusters

# Optionally, save the new data with predicted clusters
write.csv(new_data, "new_products_with_predicted_clusters.csv", row.names = FALSE)
