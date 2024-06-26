#CODE FOR REGRESSION TREE MODEL - REPORT 3

install.packages("rpart")
install.packages("rpart.plot")

library(rpart)
library(rpart.plot)

# Set the working directory to the desktop
setwd("~/Desktop")
data <- read.csv('updated_cleaned_dataset2.csv')

# use rpart() to run a regression tree.
# Build a regression tree with target variable "Wines"
## Includes all predictors: regression_tree <- rpart(Wines ~ ., data = data, control = rpart.control(maxdepth = 2), method = "anova")

##Just "CatalogPurchases', 'StorePurchases', and 'Kidhome' as predictors
regression_tree <- rpart(Wines ~ CatalogPurchases + StorePurchases + Kidhome, 
                         data = data, 
                         control = rpart.control(maxdepth = 3, minsplit = 10), 
                         method = "anova")

## plot tree
# use prp() to plot the tree. You can control plotting parameters such as color, shape, 
# and information displayed (which and where).
prp(regression_tree, type = 1, extra = 1, split.font = 1, varlen = -10)

rpart.rules(regression_tree)

#### PARTITION DATA

library(rpart)
library(rpart.plot)

# Define the predictors I want to include in the model
predictors <- c("CatalogPurchases", "StorePurchases", "Kidhome")

# partition
set.seed(1)  
train.index = sample(c(1:dim(data)[1]), dim(data)[1]*0.6)  
train.df = data[train.index, ]
valid.df = data[-train.index, ]

# Define maxdepth and minsplit values (ADDED)
maxdepth <- 3
minsplit <- 10  # Adjust minsplit value as needed

# Fit the regression tree model with reduced complexity (ADDED)
default_ct <- rpart(
  Wines ~ CatalogPurchases + StorePurchases + Kidhome, 
  data = train.df, 
  method = "anova",
  control = rpart.control(maxdepth = maxdepth, minsplit = minsplit)
)

# Generate predictions using the regression tree model(ADDED)
default_ct_point_pred_train <- predict(default_ct, train.df)
default_ct_point_pred_valid <- predict(default_ct, valid.df)

# Calculate MSE for training and validation sets (ADDED)
mse_train <- mean((default_ct_point_pred_train - train.df$Wines)^2)
mse_valid <- mean((default_ct_point_pred_valid - valid.df$Wines)^2)

# Print MSE values (ADDED)
print(paste("Training MSE:", mse_train))
print(paste("Validation MSE:", mse_valid))

# plot tree (ADDED)
prp(default_ct, type = 1, extra = 1, under = TRUE, split.font = 1, varlen = -10)
# or with color
rpart.plot(default_ct, extra=1, fallen.leaves=FALSE)

############################TRYING SOMETHING NEW ###############################

# Define maxdepth and minsplit values to explore
maxdepth_values <- c(2, 3, 4)  # Try increasing the maximum depth
minsplit_values <- c(5, 10, 15) # Try decreasing the minimum split size

# Initialize variables to store best hyperparameters and lowest MSE
best_maxdepth <- NULL
best_minsplit <- NULL
lowest_mse <- Inf

# Perform grid search
for (maxdepth in maxdepth_values) {
  for (minsplit in minsplit_values) {
    # Fit the regression tree model with current hyperparameters
    ct <- rpart(
      Wines ~ CatalogPurchases + StorePurchases + Kidhome, 
      data = train.df, 
      method = "anova",
      control = rpart.control(maxdepth = maxdepth, minsplit = minsplit)
    )
    
    # Generate predictions using the regression tree model
    pred_valid <- predict(ct, valid.df)
    
    # Calculate MSE for validation set
    mse_valid <- mean((pred_valid - valid.df$Wines)^2)
    
    # Check if current MSE is lower than lowest MSE
    if (mse_valid < lowest_mse) {
      lowest_mse <- mse_valid
      best_maxdepth <- maxdepth
      best_minsplit <- minsplit
    }
  }
}

# Print best hyperparameters and lowest MSE
print(paste("Best maxdepth:", best_maxdepth)) #Best = 3
print(paste("Best minsplit:", best_minsplit)) #Best = 5
print(paste("Lowest validation MSE:", lowest_mse)) #Lowest validation MSE: 44427.8022452807
 
#################################################################################
##Now, refit the model with best hyperparameters
final_ct <- rpart(
  Wines ~ CatalogPurchases + StorePurchases + Kidhome, 
  data = train.df, 
  method = "anova",
  control = rpart.control(maxdepth = best_maxdepth, minsplit = best_minsplit)
)

# Generate predictions using the final model
final_pred_train <- predict(final_ct, train.df)
final_pred_valid <- predict(final_ct, valid.df)

# Calculate MSE for training and validation sets
final_mse_train <- mean((final_pred_train - train.df$Wines)^2)
final_mse_valid <- mean((final_pred_valid - valid.df$Wines)^2)

# Print MSE values
print(paste("Final Training MSE:", final_mse_train)) 
print(paste("Final Validation MSE:", final_mse_valid)) 


#####################TRY PRUNING TO REDUCE OVERFIT#####################################

# Prune the best tree
pruned_tree <- prune(final_ct, cp = final_ct$cptable[which.min(final_ct$cptable[, "xerror"]), "CP"])

# Generate predictions using the pruned tree model
pruned_pred_train <- predict(pruned_tree, train.df)
pruned_pred_valid <- predict(pruned_tree, valid.df)

# Calculate MSE for training and validation sets
pruned_mse_train <- mean((pruned_pred_train - train.df$Wines)^2)
pruned_mse_valid <- mean((pruned_pred_valid - valid.df$Wines)^2)

# Print MSE values
print(paste("Pruned Training MSE:", pruned_mse_train))
print(paste("Pruned Validation MSE:", pruned_mse_valid))

# Visualize the pruned tree
prp(pruned_tree, type = 1, extra = 1, under = TRUE, split.font = 1, varlen = -10)

####################TRY CP######################################################

# Define a grid of cp values to search over
cp_grid <- c(0.001, 0.005, 0.01, 0.05, 0.1)

# Set up cross-validation
ctrl <- trainControl(method = "cv", number = 5)

# Perform grid search
tuning <- train(
  Wines ~ CatalogPurchases + StorePurchases + Kidhome, 
  data = train.df,
  method = "rpart",
  trControl = ctrl,
  tuneGrid = expand.grid(cp = cp_grid)
)

# Get the best cp value
best_cp <- tuning$bestTune$cp
print(best_cp)

# Refit the model with the selected best cp value
final_ct <- rpart(
  Wines ~ CatalogPurchases + StorePurchases + Kidhome, 
  data = train.df, 
  method = "anova",
  control = rpart.control(cp = best_cp)
)

# Generate predictions using the final model
final_pred_train <- predict(final_ct, train.df)
final_pred_valid <- predict(final_ct, valid.df)

# Calculate MSE for training and validation sets
final_mse_train <- mean((final_pred_train - train.df$Wines)^2)
final_mse_valid <- mean((final_pred_valid - valid.df$Wines)^2)

# Print MSE values
print(paste("Final Training MSE:", final_mse_train))
print(paste("Final Validation MSE:", final_mse_valid))

#########################APPLY REGULARIZATION TECHNIQUE###########################

# Load required libraries
install.packages("glmnet")
library(glmnet)

# Define predictors and response variable
predictors <- c("CatalogPurchases", "StorePurchases", "Kidhome")
response <- "Wines"

# Partition the data
set.seed(1)  
train.index <- sample(c(1:dim(data)[1]), dim(data)[1] * 0.6)  
train.df <- data[train.index, ]
valid.df <- data[-train.index, ]

# Prepare the predictor matrix and response vector
X_train <- as.matrix(train.df[, predictors])
y_train <- train.df[[response]]
X_valid <- as.matrix(valid.df[, predictors])
y_valid <- valid.df[[response]]

# Fit Lasso regression model (L1 regularization)
lasso_model <- glmnet(X_train, y_train, alpha = 1)

# Fit Ridge regression model (L2 regularization)
ridge_model <- glmnet(X_train, y_train, alpha = 0)

# Predict on training and validation sets
lasso_pred_train <- predict(lasso_model, s = 0.01, newx = X_train)
lasso_pred_valid <- predict(lasso_model, s = 0.01, newx = X_valid)

ridge_pred_train <- predict(ridge_model, s = 0.01, newx = X_train)
ridge_pred_valid <- predict(ridge_model, s = 0.01, newx = X_valid)

# Calculate MSE for Lasso and Ridge on training and validation sets
lasso_mse_train <- mean((lasso_pred_train - y_train)^2)
lasso_mse_valid <- mean((lasso_pred_valid - y_valid)^2)

ridge_mse_train <- mean((ridge_pred_train - y_train)^2)
ridge_mse_valid <- mean((ridge_pred_valid - y_valid)^2)

# Print MSE values
print("Lasso Regression:")
print(paste("Training MSE:", lasso_mse_train))
print(paste("Validation MSE:", lasso_mse_valid))

print("Ridge Regression:")
print(paste("Training MSE:", ridge_mse_train))
print(paste("Validation MSE:", ridge_mse_valid))


################################################################################
#Now, try controlling for  cp (cost of complexity)
library(caret)

# Define a grid of cp values to search over
cp_grid <- c(0.001, 0.005, 0.01, 0.05, 0.1)

# Set up cross-validation
ctrl <- trainControl(method = "cv", number = 10)

# Perform grid search
tuning <- train(
  Wines ~ CatalogPurchases + StorePurchases + Kidhome, 
  data = train.df,
  method = "rpart",
  trControl = ctrl,
  tuneGrid = expand.grid(cp = cp_grid)
)

# Get the best hyperparameters
best_cp <- tuning$bestTune$cp

print(best_cp) #best = 0.005

####

# Now try to refit the model with adjusted complexity parameter (cp)
final_ct <- rpart(
  Wines ~ CatalogPurchases + StorePurchases + Kidhome, 
  data = train.df, 
  method = "anova",
  control = rpart.control(maxdepth = best_maxdepth, minsplit = best_minsplit, cp = best_cp)
)

# Generate predictions using the final model
final_pred_train <- predict(final_ct, train.df)
final_pred_valid <- predict(final_ct, valid.df)

# Calculate MSE for training and validation sets
final_mse_train <- mean((final_pred_train - train.df$Wines)^2)
final_mse_valid <- mean((final_pred_valid - valid.df$Wines)^2)

# Print MSE values
print(paste("Final Training MSE:", final_mse_train)) 
print(paste("Final Validation MSE:", final_mse_valid))


#################################################################################
##ADJUST TO AVOID OVERFIT
# Fit the regression tree model with reduced complexity
regression_tree <- rpart(
  formula = Wines ~ CatalogPurchases + StorePurchases + Kidhome, 
  data = data,
  method = "anova",
  control = rpart.control(maxdepth = 3, minsplit = 5)
)

# Generate predictions using the reduced complexity model
reduced_complexity_pred_train <- predict(reduced_complexity_ct, train.df)
reduced_complexity_pred_valid <- predict(reduced_complexity_ct, valid.df)

# Evaluate the model using regression metrics
reduced_complexity_mse_train <- mean((reduced_complexity_pred_train - train.df$Wines)^2)
reduced_complexity_mse_valid <- mean((reduced_complexity_pred_valid - valid.df$Wines)^2)

# Print the MSE for the reduced complexity model
print("Reduced Complexity Model:")
print(paste("Training MSE:", reduced_complexity_mse_train))
print(paste("Validation MSE:", reduced_complexity_mse_valid))

#################################################################################
#Keep trying to improve MSE

library(rpart.plot)

prp( default_ct, type = 1, extra = 1, under = TRUE, split.font = 1, varlen = -10, 
     box.col=ifelse( deeper_ct$frame$var == "<leaf>", 'gray', 'white'))  
# or with color
rpart.plot(default_ct, extra=1, fallen.leaves=FALSE)

### let's repeat this for the deeper tree (both training and validation)

# Deeper tree - Train the regression tree model
deeper_ct <- rpart(Wines ~ CatalogPurchases + StorePurchases + Kidhome, data = train.df, method = "anova", cp = 0.05, minsplit = 5)

# Generate predictions using the regression tree model
deeper_ct_point_pred_train <- predict(deeper_ct, train.df)

# Evaluate the model using appropriate regression metrics
mse <- mean((deeper_ct_point_pred_train - train.df$Wines)^2)
print(paste("Mean Squared Error:", mse))

prp(deeper_ct, type = 1, extra = 1, under = TRUE, split.font = 1, varlen = -10, 
    box.col=ifelse(deeper_ct$frame$var == "<leaf>", 'gray', 'white'))
# or with color
rpart.plot(deeper_ct, extra=1, fallen.leaves=FALSE)

# deeper tree - validation to compare predicted value against actual value in the validation data
deeper_ct_point_pred_valid <- predict(deeper_ct, valid.df)

# Evaluate the model using appropriate regression metrics
mse <- mean((deeper_ct_point_pred_valid - valid.df$Wines)^2)
print(paste("Mean Squared Error:", mse))

prp(deeper_ct, type = 1, extra = 1, under = TRUE, split.font = 1, varlen = -10, 
    box.col=ifelse(deeper_ct$frame$var == "<leaf>", 'gray', 'white'))  
# or with color
rpart.plot(deeper_ct, extra=1, fallen.leaves=FALSE)


############ADDITIONAL HYPERPARAMETER TUNING STEP################################

set.seed(1)
trControl <- trainControl(method="cv", number=5, allowParallel=TRUE)

set.seed(1)
trControl <- trainControl(method = "cv", number = 5, allowParallel = TRUE)
model1 <- train(Wines ~ CatalogPurchases + StorePurchases + Kidhome, data = train.df,
                method = "rpart", trControl = trControl,
                tuneGrid = data.frame(cp = c(1, 0.1, 0.01, 0.001, 0.0001)))


model2 <- train(Wines ~ CatalogPurchases + StorePurchases + Kidhome, data=train.df,
                method="rpart", trControl=trControl,
                tuneGrid=data.frame(cp=c(0.005, 0.002, 0.001, 0.0005, 0.0002)))
model2$results

#######################################################################################################################
# Now try to refit the model with adjusted complexity parameter (cp) with 0.002 - the best defined by the results above
final_ct <- rpart(
  Wines ~ CatalogPurchases + StorePurchases + Kidhome, 
  data = train.df, 
  method = "anova",
  control = rpart.control(maxdepth = best_maxdepth, minsplit = best_minsplit, cp = 0.002)
)

# Generate predictions using the final model
final_pred_train <- predict(final_ct, train.df)
final_pred_valid <- predict(final_ct, valid.df)

# Calculate MSE for training and validation sets
final_mse_train <- mean((final_pred_train - train.df$Wines)^2)
final_mse_valid <- mean((final_pred_valid - valid.df$Wines)^2)

# Print MSE values
print(paste("Final Training MSE:", final_mse_train)) #Final Training MSE: 41333.4630562819
print(paste("Final Validation MSE:", final_mse_valid)) #Final Validation MSE: 44427.8022452807

#######################################################################################################################


# or let's make a Grid Search to try many combinations of cp and minsize, the two parameters.

curr_MSE <- Inf  
best_cost_penalty <- 0
best_min_leaf_to_split <- 2

for (cost_penalty in seq(from=0, to=0.1, by=0.01)) {
  for (min_leaf_to_split in seq(from=1, to=10, by=1)) {
    
    # Train the tree
    trained_tree <- rpart(Wines ~ CatalogPurchases + StorePurchases + Kidhome, data = train.df, method = "anova", 
                          cp = cost_penalty, minsplit = min_leaf_to_split)
    
    # Predict with the trained tree
    train_results <- predict(trained_tree, train.df)
    valid_results <- predict(trained_tree, valid.df)
    
    # Calculate MSE from results
    train_MSE <- mean((train_results - train.df$Wines)^2)
    valid_MSE <- mean((valid_results - valid.df$Wines)^2)
    
    # Is this MSE the best we have so far? If so, store the current values:
    if (valid_MSE < curr_MSE) {
      curr_MSE <- valid_MSE
      best_cost_penalty <- cost_penalty
      best_min_leaf_to_split <- min_leaf_to_split
    }
  }
}
cat("Best MSE =", curr_MSE, "; Best cost_penalty =", best_cost_penalty, "; Best min_leaf_to_split =", best_min_leaf_to_split)

# retrain the tree to match the best parameters we found  
trained_tree = rpart(Wines ~ CatalogPurchases + StorePurchases + Kidhome, data = train.df, method = "class", 
                     cp = best_cost_penalty , minsplit = best_min_leaf_to_split )  # change the original parameters

# print that best tree 
prp(trained_tree, type = 1, extra = 1, under = TRUE, split.font = 1, varlen = -10, 
    box.col=ifelse(trained_tree$frame$var == "<leaf>", 'gray', 'white'))  

###### TRY BEST PRUNED TREE #######

# Instead of using those two parameters, cp and minsplit, let's find the best pruned tree statistically.

# Fit a regression tree with cross-validation
cv_ct <- rpart(Wines ~ CatalogPurchases + StorePurchases + Kidhome, data = train.df, method = "anova", 
               cp = 0, minsplit = 3, xval = 5)

# Plot the cross-validated regression tree
prp(cv_ct)

# Or with color
rpart.plot(cv_ct, extra = 1, fallen.leaves = FALSE, varlen = -10)

# Print the complexity parameter table
printcp(cv_ct)


##Finding Best Pruned Tree
#Fit a Regression Tree with Cross-Validation
cv_ct <- rpart(Wines ~ CatalogPurchases + StorePurchases + Kidhome, 
               data = train.df, 
               method = "anova", 
               cp = 0.005, 
               minsplit = 3, 
               xval = 5)

printcp(cv_ct)

pruned_ct <- prune(cv_ct, cp = cv_ct$cptable[which.min(cv_ct$cptable[,"xerror"]),"CP"])

prp(pruned_ct, type = 1, extra = 1, split.font = 1, varlen = -10)  

set.seed(1)

#Cross Validation
cv_ct <- rpart(Wines ~ CatalogPurchases + StorePurchases + Kidhome, 
               data = train.df, 
               method = "anova", 
               cp = 0.00001, 
               minsplit = 1, 
               xval = 5)

printcp(cv_ct)

pruned_ct <- prune(cv_ct, cp = 0.0154639)

#Generate Predictions for Training and Validation 
pruned_ct_pred_train <- predict(pruned_ct, train.df)
pruned_ct_pred_valid <- predict(pruned_ct, valid.df)

mse_train <- mean((pruned_ct_pred_train - train.df$Wines)^2)
mse_valid <- mean((pruned_ct_pred_valid - valid.df$Wines)^2)

print(paste("Training MSE:", mse_train)) #Training MSE: 110636.214859896
print(paste("Validation MSE:", mse_valid)) #Validation MSE: 107941.989505501

#MSE gets smaller, but still incredibly high numbers 



#################################ENSEMBLE METHODS################################

# Load necessary libraries
library(caret)
library(randomForest)
library(gbm)
library(ipred) # For Bagging


# Setup for cross-validation
trControl <- trainControl(method = "cv", number = 5)

### BAGGED TREES MODEL ###
set.seed(123) # For reproducibility
bagged_model <- bagging(Wines ~ CatalogPurchases + StorePurchases + Kidhome, data = train.df)

# Predict and calculate RMSE for the Bagged Trees model
bagged_valid_pred <- predict(bagged_model, newdata = valid.df)
bagged_valid_rmse <- sqrt(mean((bagged_valid_pred - valid.df$Wines)^2))
print(paste("Bagged Trees Validation RMSE:", bagged_valid_rmse))

### RANDOM FOREST MODEL WITH DETAILED GRID SEARCH ###
set.seed(123)
tuneGridRF <- expand.grid(mtry = c(2, sqrt(ncol(train.df)), ncol(train.df)/2))
rf_model <- train(Wines ~ CatalogPurchases + StorePurchases + Kidhome, data = train.df, 
                  method = "rf", trControl = trControl, tuneGrid = tuneGridRF)

# Predict and calculate RMSE for the Random Forest model
rf_valid_pred <- predict(rf_model, newdata = valid.df)
rf_valid_rmse <- sqrt(mean((rf_valid_pred - valid.df$Wines)^2))
print(paste("Random Forest Validation RMSE:", rf_valid_rmse))

# Print the best mtry value and variable importance
print(rf_model$bestTune)
varImpPlot(rf_model$finalModel)

### BOOSTED TREES MODEL WITH DETAILED GRID SEARCH ###
set.seed(123)
tuneGridGBM <- expand.grid(interaction.depth = c(1, 3, 5),
                           n.trees = c(50, 100, 150),
                           shrinkage = c(0.01, 0.05, 0.1),
                           n.minobsinnode = c(10, 20))
gbm_model <- train(Wines ~ CatalogPurchases + StorePurchases + Kidhome, data = train.df, 
                   method = "gbm", trControl = trControl, tuneGrid = tuneGridGBM, 
                   verbose = FALSE)

# Predict and calculate RMSE for the Boosted Trees model
gbm_valid_pred <- predict(gbm_model, newdata = valid.df, n.trees = 100)
gbm_valid_rmse <- sqrt(mean((gbm_valid_pred - valid.df$Wines)^2))
print(paste("Boosted Trees Validation RMSE:", gbm_valid_rmse))

# Print the best tuning parameters
print(gbm_model$bestTune)


