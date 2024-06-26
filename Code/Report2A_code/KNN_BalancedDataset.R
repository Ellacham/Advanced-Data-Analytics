# K-Nearest Neighbors ###Balanced dataset 
## The K-NN Classifier (Categorical Outcome - RESPONSE)


# First, we ensure necessary libraries are installed and loaded
if (!require(caret)) install.packages("caret")
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(ggrepel)) install.packages("ggrepel")
library(caret)
library(ggplot2)
library(ggrepel)


data <- read.csv('updated_cleaned_dataset_balanced.csv')


head(data)
str(data)



data <- dummy_cols(data, remove_first_dummy = TRUE, remove_selected_columns = TRUE)
data$Response <- as.factor(data$Response)
str(data$Response)
head(data)
str(data)
############################################################################
#Initially starting with all for exhaustive search with the exception of Income, total_purchases(since Wines was transformed to arrive at total_purchases) and Wines(target variable)
outcome <- 'Response'  # Target variable
identifier <- "ID"  # Identifier variable 
predictors <- c('Kidhome','Wines', 'Teenhome', 'LastPurchase', 'Fruits', 'Sweets', 'Gold', 'Meats', 'Fish', 'DiscountPurchases', 
                'WebPurchases', 'CatalogPurchases', 'StorePurchases', 
                'WebVisitsMonth', 'AcceptedCmp3', 
                'AcceptedCmp4', 'AcceptedCmp5', 'AcceptedCmp1', 'AcceptedCmp2', 'Complain', 
                'Education_non_graduate', 'Age_Intervals_other',
                'Marital_Status_Married' , 'Marital_Status_Single' , 'Marital_Status_Together',
                'Marital_Status_Widow','Income_Intervals_other_income', 'Recency_Intervals_31-60 days', 'Recency_Intervals_61-90 days', 
                'Recency_Intervals_More than 90 days')


# Reduce the data set to selected variables
data_pred <- data[, c(identifier, outcome, predictors)]

dim(data_pred)
head(data_pred)

# Partition the data
set.seed(1)  # Set seed for reproducibility
idx <- createDataPartition(data_pred$Response, p = 0.8, list = FALSE)
train_pred <- data_pred[idx, ]
holdout_pred <- data_pred[-idx, ]

# Run exhaustive search for best subset selection
search <- regsubsets(Response ~ ., data = train_pred, nbest = 1, nvmax = ncol(train_pred), method = "exhaustive")

# Summary of the best subsets
summary(search)

# Determine the optimal subset based on Cp statistic or other criteria
optimal <- which.min(summary(search)$cp)

# Get the variable names for the optimal model
X <- summary(search)$which[, -1]
xvars <- dimnames(X)[[2]]
xvars <- xvars[X[optimal,]]
xvars


xvars <- data[, c("Teenhome", "Gold", "Meats", "LastPurchase", "DiscountPurchases" , "CatalogPurchases", "WebVisitsMonth" ,"WebPurchases" ,
                  "StorePurchases", "AcceptedCmp3" , "AcceptedCmp5" ,"AcceptedCmp1", "Education_non_graduate",
                  "Marital_Status_Married", "Marital_Status_Together", "Response")]
str(xvars)
data <- xvars
##adding some interaction terms
#We realized they didn't have any effect on the model's performance hence were droped
#data$DiscStore = data$DiscountPurchases * data$StorePurchases  
#data$DiscCatalog = data$DiscountPurchases * data$CatalogPurchases  # Another interaction term
#data$MeatsGold = sqrt(data$Meats * data$Meats)  #  polynomial term (square root of Wines)
str(data)
########################################################################

# Set the random seed for reproducibility
set.seed(35)

# Isolate one record for new_idx
new_idx <- sample(nrow(data), size = 1)  # Select one random index from data

# Create new.df using new_idx
new.df <- data[new_idx, ]

# Remove that one record from the dataset to create data_remaining
data_remaining <- data[-new_idx, ]

# Define the proportion for the training set (80% for training)
training_proportion = 0.8  

# Sample indices for the training data
idx <- sample(nrow(data_remaining), size = floor(training_proportion * nrow(data_remaining)))

# Split the remaining data into training and holdout datasets
train.df <- data_remaining[idx, ]
holdout.df <- data_remaining[-idx, ]




# Visualizing training data and new data point (if new.df is defined)
g <- ggplot(mapping = aes(x = CatalogPurchases, y = StorePurchases, shape = Response, color = Response)) +
  geom_point(data = train.df, size = 3) +
  geom_text_repel(aes(label = rownames(train.df)), data = train.df, show.legend = FALSE,  size = 3) +
  geom_point(data = new.df, size = 5, aes(fill = 'New'), shape = 21) +  
  scale_shape_manual(values = c(18, 15)) +
  scale_color_manual(values = c('black', 'darkorange')) +
  scale_fill_manual(values = c('lightblue')) +
  labs(title = "Training Data and New Data Point")
print(g)


# Preprocess the training data: center and scale
preProcValues <- preProcess(train.df[, -ncol(train.df)], method = c("center", "scale"))

# Apply the preprocessing steps to new.df and holdout.df
new.df_norm <- predict(preProcValues, new.df)
holdout.df_norm <- predict(preProcValues, holdout.df)




# Training the k-NN model
train.control <- trainControl(method = "none")  # No resampling
model <- train(Response ~ ., data = train.df,
               method = "knn",
               preProcess = c("center", "scale"),
               tuneGrid = expand.grid(k = 3),
               trControl = train.control)

# Predicting the new data point using k-NN model
predictions <- predict(model, new.df_norm)
print(predictions)



# Optimize K using LOOCV
trControlparameters <- trainControl(method = "loocv")
model.optimized <- train(Response ~ ., data = train.df,
                         method = "knn",
                         preProcess = c("center", "scale"),
                         tuneGrid = expand.grid(k = seq(1, 15, 2)),
                         trControl = trControlparameters)
print(model.optimized)

# Predicting with the optimized model
holdout.predictions <- predict(model.optimized, holdout.df_norm)
holdout.df$PredictedResponse <- holdout.predictions  # Add predictions to the original (non-normalized) holdout data for visualization

# Visualization
g <- ggplot() +
  geom_point(data = train.df, aes(x = CatalogPurchases, y = StorePurchases, shape = Response, color = Response), size = 3) +
  geom_text_repel(data = train.df, aes(x = CatalogPurchases, y = StorePurchases, label = rownames(train.df)), size = 3, show.legend = FALSE) +
  geom_point(data = new.df, aes(x = CatalogPurchases, y = StorePurchases, fill = 'New'), size = 5, shape = 21) +
  scale_shape_manual(values = c(18, 15)) +
  scale_color_manual(values = c('black', 'darkorange')) +
  scale_fill_manual(values = c('lightblue')) +
  labs(title = "Training Data and New Data Point")
print(g)
