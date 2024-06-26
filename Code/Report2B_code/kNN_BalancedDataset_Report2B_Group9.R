
##kNN

# Load necessary packages
if (!require(caret)) install.packages("caret")
library(caret)
if (!require(class)) install.packages("class")
library(class)

# Load your data
data <- read.csv('updated_cleaned_dataset_balanced.csv')
str(data)



data <- dummy_cols(data, remove_first_dummy = TRUE, remove_selected_columns = TRUE)
str(data)

# Convert the target variable into a factor 
data$Response <- as.factor(data$Response)

# Define variables without new variables (model1 - original model)
#data <- data[, c("Teenhome", "Gold", "Meats", "LastPurchase", "DiscountPurchases" , "CatalogPurchases", "WebVisitsMonth" ,"WebPurchases" ,
#                 "StorePurchases", "AcceptedCmp3" , "AcceptedCmp5" ,"AcceptedCmp1", "Education_non_graduate",
#                 "Marital_Status_Married", "Marital_Status_Together","Response")]
#str(data)


# Define variables with the 2 new variables (model2) - best model
data <- data[, c("Teenhome", "Gold", "Meats", "LastPurchase", "DiscountPurchases" , "CatalogPurchases", "WebVisitsMonth" ,"WebPurchases" ,
                 "StorePurchases", "AcceptedCmp3" , "AcceptedCmp5" ,"AcceptedCmp1", "Education_non_graduate",
                 "Marital_Status_Married", "Marital_Status_Together", "hcWardD4clusters",	"Kmeans3clusters","Response")]

#str(data)

# Define variables with the 4 new variables (model3)
#data <- data[, c("Teenhome", "Gold", "Meats", "LastPurchase", "DiscountPurchases" , "CatalogPurchases", "WebVisitsMonth" ,"WebPurchases" ,
#                 "StorePurchases", "AcceptedCmp3" , "AcceptedCmp5" ,"AcceptedCmp1", "Education_non_graduate",
#                 "Marital_Status_Married", "Marital_Status_Together", "hcWardD4clusters",	"Kmeans3clusters", "hcWardD7clusters", "Kmeans4clusters","Response")]

#str(data)



# Split the dataset into training and test sets
set.seed(123)  # For reproducibility
indexes <- createDataPartition(data$Response, p = 0.8, list = FALSE)
trainSet <- data[indexes, ]
testSet <- data[-indexes, ]

# Normalize data: scale and center
preProcRange <- preProcess(trainSet[, -ncol(trainSet)], method = c("center", "scale"))
trainNorm <- predict(preProcRange, trainSet[, -ncol(trainSet)])
testNorm <- predict(preProcRange, testSet[, -ncol(testSet)])

# Convert normalized data back to data frames and add the 'Response' variable back
trainNorm <- as.data.frame(trainNorm)
testNorm <- as.data.frame(testNorm)
trainNorm$Response <- trainSet$Response
testNorm$Response <- testSet$Response

# Set up cross-validation and grid of k values
control <- trainControl(method = "cv", number = 10)
k_values <- expand.grid(k = seq(1, 20, by = 2))

# Train the k-NN model using cross-validation
set.seed(123)
knnFit <- train(Response ~ ., data = trainNorm, method = "knn", tuneGrid = k_values, trControl = control)

# View best tuning parameters and model accuracy
print(knnFit)
bestK <- knnFit$bestTune$k
bestK

predictions <- predict(knnFit, newdata = testNorm[, -ncol(testNorm)])

# Evaluate model performance
confusionMatrix <- table(Predicted = predictions, Actual = testNorm$Response)

print(confusionMatrix)
accuracy <- sum(diag(confusionMatrix)) / sum(confusionMatrix)
print(paste("Accuracy:", accuracy))

# Calculate Precision, Recall, and F1 Score from the confusion matrix
true_positives <- confusionMatrix[2, 2]
false_positives <- confusionMatrix[1, 2]
false_negatives <- confusionMatrix[2, 1]
true_negatives <- confusionMatrix[1, 1]

precision <- true_positives / (true_positives + false_positives)
recall <- true_positives / (true_positives + false_negatives)
F1 <- 2 * (precision * recall) / (precision + recall)

# Print Precision, Recall, and F1 Score
print(paste("Precision:", precision))
print(paste("Recall:", recall))
print(paste("F1 Score:", F1))

