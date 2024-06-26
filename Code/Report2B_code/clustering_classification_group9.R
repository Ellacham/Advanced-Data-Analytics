##CLUSTERING
################################################################################################
#rm(list = ls())

install.packages('cluster')
install.packages("dendextend")
install.packages('pheatmap')





# Load necessary libraries
library(fastDummies)
library(cluster)     # For silhouette plots
library(ggplot2)
library(gridExtra)   # For arranging multiple plots
library(dendextend)  # For enhancing dendrogram plots
library(GGally)      # For parallel coordinate plots


# Load and preprocess data
data.df <- read.csv("updated_cleaned_dataset_balanced.csv")
str(data.df)
#data.df <- subset(data.df, select = -X)
#str(data.df)

data.df <- dummy_cols(data.df, remove_first_dummy = TRUE, remove_selected_columns = TRUE)
str(data.df)

selected_columns <- c("Age", "Income","Kidhome","Teenhome","LastPurchase","Wines",
                      "Fruits","Meats","Fish","Sweets","Gold","total_purchases",
                      "DiscountPurchases","WebPurchases","CatalogPurchases",
                      "StorePurchases","WebVisitsMonth","AcceptedCmp3","AcceptedCmp4",
                      "AcceptedCmp5","AcceptedCmp1","AcceptedCmp2", "Complain",
                      "Age_Intervals_other","Education_non_graduate","Marital_Status_Married",
                      "Marital_Status_Single","Marital_Status_Together","Marital_Status_Widow",
                      "Income_Intervals_other_income","Recency_Intervals_31-60 days",
                      "Recency_Intervals_61-90 days","Recency_Intervals_More than 90 days","Response") 

data.df <- data.df[, selected_columns]
str(data.df)

# Normalize input variables
data.df.norm <- as.data.frame(scale(data.df))

# Compute Euclidean distance for hierarchical clustering
d.norm <- dist(data.df.norm, method = "euclidean")

#hierarchical clustering
methods <- c("single", "complete", "median", "average", "centroid", "ward.D2")
# Initialize 'plots' as an empty list
plots <- list()
for (method in methods) {
  hc <- hclust(d.norm, method = method)
  # Cutting tree to get desired number of clusters, here using 4 as an example
  clusters <- cutree(hc, k = 4)
  # Generate dendrogram
  dend <- as.dendrogram(hc)
  # Color branches by cluster
  colored_dend <- color_branches(dend, k = 4)
  # Prepare and plot dendrogram directly
  plot(colored_dend, main = paste("Method:", method))
  # Store plot for later (optional, if you need it)
  plots[[method]] <- recordPlot()  # This stores the current plot for later use
  # Store cluster assignments for each method
  data.df.norm[[paste0("HClust_Cluster_", method)]] <- as.factor(clusters)
}

# 'ward.D2' was the chosen method from the previous steps
# Print out Hierarchical cluster labels for 'ward.D2'
cat("\nHierarchical Cluster Labels (ward.D2):\n")
cat(data.df.norm$HClust_Cluster_ward.D2)

####################################################################################
# K-Means Clustering
k.max <- 10  # Max number of clusters to consider
wss <- numeric(k.max)  # Within-cluster sum of squares

# Compute and plot within-cluster sum of squares for different k values
for (k in 1:k.max) {
  km <- kmeans(data.df.norm, centers = k, nstart = 25)
  wss[k] <- km$tot.withinss
}

# Elbow method plot
plot(1:k.max, wss, type = "b", xlab = "Number of clusters (k)", ylab = "Within-cluster sum of squares")

# Choose a k based on the plot, here we choose k=3 for example
set.seed(123)  # For reproducibility
km.final <- kmeans(data.df.norm, centers = 4, nstart = 25)

# Add K-means cluster labels to the data
data.df.norm$KMeans_Cluster <- as.factor(km.final$cluster)

# Print out K-means cluster labels
cat("K-Means Cluster Labels:\n")
cat(km.final$cluster)


# Plotting parallel coordinates for K-Means clusters
ggparcoord(data.df.norm, columns = 2:4, groupColumn = 'KMeans_Cluster', scale = "globalminmax", showPoints = TRUE) +
  theme_bw() +
  labs(title = "K-Means Clustering: Parallel Coordinates Plot")



# Plotting parallel coordinates for Hierarchical clusters (assuming ward.D2 is selected)
ggparcoord(data.df.norm, columns = 2:3, groupColumn = 'HClust_Cluster_ward.D2', scale = "globalminmax", showPoints = TRUE) +
  theme_bw() +
  labs(title = "Hierarchical Clustering (ward.D2): Parallel Coordinates Plot")


# Convert cluster labels to factors 
data.df.norm$KMeans_Cluster <- as.factor(data.df.norm$KMeans_Cluster)
data.df.norm$HClust_Cluster_ward.D2 <- as.factor(data.df.norm$HClust_Cluster_ward.D2)

# Plot
ggplot(data.df.norm, aes(x = KMeans_Cluster, y = HClust_Cluster_ward.D2)) +
  geom_count(aes(color = KMeans_Cluster), size = 3) +  # geom_count to show number of points
  theme_minimal() +
  labs(title = "Comparison of K-Means and Hierarchical Clustering Assignments",
       x = "K-Means Cluster", 
       y = "Hierarchical Cluster (Ward.D2)",
       color = "K-Means Cluster") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis text for readability


# Double-check the structure of your heatmap data
str(data_for_heatmap)
# Keep only numeric columns
data_for_heatmap <- data_for_heatmap[, sapply(data_for_heatmap, is.numeric)]
# Test basic heatmap functionality
pheatmap(data_for_heatmap)


########################################################################################
library(ggplot2)
# Load and preprocess data
data.df <- read.csv("updated_cleaned_dataset_balanced.csv")
str(data.df)

data.df <- dummy_cols(data.df, remove_first_dummy = TRUE, remove_selected_columns = TRUE)
str(data.df)

# Scatter plot of Age vs Income colored by Cluster
ggplot(data.df, aes(x = total_purchases, y = Response, color = as.factor(hcWardD4clusters))) +
  geom_point(alpha = 0.5) +  # alpha for transparency
  labs(title = "Scatter Plot of Age vs Income by Cluster",
       x = "total_purchases",
       y = "Response",
       color = "Cluster") +
  theme_minimal() +
  scale_color_manual(values = c("1" = "red", "2" = "blue", "3" = "green", "4" = "yellow"))  

###################################################################################33
# Scatter plot of Age vs Income colored by Cluster
ggplot(data.df, aes(x = total_purchases, y = Response, color = as.factor(hcWardD7clusters))) +
  geom_point(alpha = 0.5) +  # alpha for transparency
  labs(title = "Scatter Plot of Age vs Income by Cluster",
       x = "total_purchases",
       y = "Income",
       color = "Response") +
  theme_minimal() +
  scale_color_manual(values = c("1" = "red", "2" = "blue", "3" = "green", "4" = "yellow",
                                "5" = "purple", "6" = "orange", "7" = "pink"))  

