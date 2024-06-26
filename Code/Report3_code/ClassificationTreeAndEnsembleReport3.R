##CLASSIFICATION
############PART1: ClassificationTree
##CODE FOR TREE MODEL (PART1)
install.packages('rpart.plot')
install.packages('randomForest')
install.packages('adabag')
install.packages('gbm')

library(fastDummies)
library(rpart)
library(rpart.plot)
library(caret)
library(pROC)
library(randomForest)
library(adabag)
library(gbm) # For Boosted Trees

# Loading and preparing the classification dataset
dataClass.df <- read.csv('updated_cleaned_dataset_balanced.csv')  
str(dataClass.df)

dataClass.df <- dummy_cols(dataClass.df, remove_first_dummy = TRUE, remove_selected_columns = TRUE)
head(dataClass.df)
#dataClass.df = dataClass.df[ ,-1]  # Drop ID column
#str(dataClass.df)

dataClass.df <- dataClass.df[, c('Income', "Teenhome", "Gold", "Meats", "LastPurchase", "DiscountPurchases" , "CatalogPurchases", "WebVisitsMonth" ,"WebPurchases" ,
                 "StorePurchases", "AcceptedCmp3" , "AcceptedCmp5" ,"AcceptedCmp1", "Education_non_graduate",
                 "Marital_Status_Married", "Marital_Status_Together", "hcWardD4clusters",	"Kmeans3clusters","Response")]

#str(data)


dataClass.df$Response <- as.factor(dataClass.df$Response)
str(dataClass.df$Response)
#str(dataClass.df)



# Splitting the dataset into training and validation sets
set.seed(123)
index <- createDataPartition(dataClass.df$Response, p = 0.8, list = FALSE)
train.df <- dataClass.df[index, ]
valid.df <- dataClass.df[-index, ]

dim(train.df)
dim(valid.df)
sum(is.na(train.df))

# Building the initial classification tree model
initClassTree <- rpart(Response ~ ., data = train.df, method = "class")
print(initClassTree)

#levels(dataClass.df$Response)

# Plotting the initial tree
prp(initClassTree, type = 1, extra = 1, split.font = 1, varlen = -10)

valid.pred <- predict(initClassTree, newdata = valid.df, type = "class")

confMatrix <- confusionMatrix(valid.pred, valid.df$Response)
print(confMatrix)

accuracy <- confMatrix$overall['Accuracy']
sensitivity <- confMatrix$byClass['Sensitivity']
specificity <- confMatrix$byClass['Specificity']
F1 <- 2 * (sensitivity * specificity) / (sensitivity + specificity)

print(paste("Accuracy:", accuracy))
print(paste("Sensitivity (Recall):", sensitivity))
print(paste("Specificity:", specificity))
print(paste("F1 Score:", F1))




# Grid Search for hyperparameters tuning
set.seed(123)
trControl <- trainControl(method = "cv", number = 10)
cpGrid <- expand.grid(cp = seq(0.001, 0.05, by = 0.001))


bestClassTree <- train(Response ~ ., data = train.df, method = "rpart", trControl = trControl, tuneGrid = cpGrid)
print(bestClassTree)

# Extract the underlying 'rpart' model
finalModel <- bestClassTree$finalModel
print(finalModel)

# Now plot with prp()
prp(finalModel, type = 0, extra = 104, main = "Best Classification Tree")



#####################################################################
#plotting the important variable
# Calculate variable importance
varImpModel <- varImp(finalModel, scale = FALSE)

# Check the structure of the importance to understand how it's stored
print(varImpModel)
str(varImpModel)

# Create a data frame from the importance scores
importanceDf <- as.data.frame(varImpModel$Overall, stringsAsFactors = FALSE)
rownames(importanceDf) <- rownames(varImpModel)
names(importanceDf)[1] <- "Importance"

# Add the variable names as a new column
importanceDf$Variable <- rownames(importanceDf)

# Now we can plot using ggplot2
ggplot(importanceDf, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  xlab("Variables") +
  ylab("Importance") +
  ggtitle("Variable Importance Plot")

########################################################################


# Predicting on validation data and generating confusion matrix
valid.pred <- predict(bestClassTree, newdata = valid.df, type = "raw")


# Generate the confusion matrix
confMatrix <- confusionMatrix(valid.pred, valid.df$Response)

print(confMatrix)

# Extracting metrics: Accuracy, Sensitivity, Specificity, F1 Score
accuracy <- confMatrix$overall['Accuracy']
sensitivity <- confMatrix$byClass['Sensitivity']
specificity <- confMatrix$byClass['Specificity']
F1 <- 2 * (sensitivity * specificity) / (sensitivity + specificity)

# Printing the metrics
print(paste("Accuracy:", accuracy))
print(paste("Sensitivity (Recall):", sensitivity))
print(paste("Specificity:", specificity))
print(paste("F1 Score:", F1))



#########ensemble PART2

# Bagged Trees Model with Grid Search (Using caret's "treebag" method for simplicity)
trControl <- trainControl(method = "cv", number = 5)
bagGridSearch <- train(Response~., data=train.df, method="treebag",
                       trControl=trControl)
bagPredictions <- predict(bagGridSearch, newdata=valid.df)
bagCm <- confusionMatrix(bagPredictions, valid.df$Response)
bagCm
cat("\nBagged Trees Model\n")
cat("Accuracy: ", bagCm$overall['Accuracy'], "\n")


# Random Forest Model with Grid Search
trControl <- trainControl(method = "cv", number = 5)
tuneGridRF <- expand.grid(.mtry=c(2, 4, 6, 8))
rfGridSearch <- train(Response~., data=train.df, method="rf",
                      trControl=trControl, tuneGrid=tuneGridRF,
                      metric="Accuracy")
rfPredictions <- predict(rfGridSearch, newdata=valid.df)
rfCm <- confusionMatrix(rfPredictions, valid.df$Response)
rfCm
cat("\nRandom Forest Model\n")
cat("Accuracy: ", rfCm$overall['Accuracy'], "\n")
print(rfGridSearch$bestTune)

# Variable importance from the best RF model
rfBestModel <- rfGridSearch$finalModel
print(rfBestModel$importance)
# Extracting importance scores
importanceScores <- rfBestModel$importance
# Sorting the importance scores in descending order
sortedScores <- sort(importanceScores[, "MeanDecreaseGini"], decreasing = FALSE)
print(sortedScores)
# Creating a bar plot for the sorted importance scores
barplot(sortedScores, main = "Variable Importance", las = 2, cex.names = 0.5, horiz = TRUE)




# Boosted Trees Model with Grid Search 
# Set up control for cross-validation
trControl <- trainControl(method = "cv", number = 5)

# Set up the tuning grid. Including the missing 'n.minobsinnode'
tuneGridGBM <- expand.grid(.interaction.depth = c(1, 3, 5), 
                           .n.trees = c(100, 200), 
                           .shrinkage = c(0.01, 0.1),
                           .n.minobsinnode = c(10, 20))  

# Train the model using the grid search
gbmGridSearch <- train(Response~., data=train.df, method="gbm",
                       trControl=trControl, 
                       tuneGrid=tuneGridGBM,
                       verbose=FALSE)

gbmPredictions <- predict(gbmGridSearch, newdata=valid.df)
gbmCm <- confusionMatrix(gbmPredictions, valid.df$Response)
cat("\nBoosted Trees Model\n")
cat("Accuracy: ", gbmCm$overall['Accuracy'], "\n")


# Output the best tuning parameters
print(gbmGridSearch$bestTune)

