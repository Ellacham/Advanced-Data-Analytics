install.packages('tidyverse')
install.packages('car')


library(tidyverse)
library(MASS) # for Box-Cox transformation to get a normal distribution for Wines since our target variable(Wines)  is skewed

# Load the dataset
data <- read.csv('updated_cleaned_dataset.csv')

# View the structure of the dataset
str(data)

# Summary statistics
summary(data)

# Visualize the distribution of the target variable
ggplot(data, aes(x = Wines)) +
  geom_histogram(binwidth = 50, fill = "lightblue", color = "black") +
  labs(title = "Distribution of Wines",
       x = "Wines Bought",
       y = "Frequency") +
  theme_minimal()

# Apply Box-Cox transformation to the target variable to transform it
lambda <- 0.5

transformed_Wines <- if (lambda == 0) {
  log(data$Wines)
} else {
  (data$Wines^lambda - 1) / lambda
}

# Visualize the transformed target variable
hist(transformed_Wines, main = "Transformed Wines Bought")

data$Wines = transformed_Wines #replace the transformed values in the dataset

#Visualize again to see the effect of the transformation
ggplot(data, aes(x = transformed_Wines)) +
  geom_histogram( fill = "lightblue", color = "black") +
  labs(title = "Distribution of Wines Bought",
       x = "Wines Bought",
       y = "Frequency") +
  theme_minimal()

############################################################################
#Apply Exhaustive search to find best subset of predictors
#First convert categorical values to dummies
data <- dummy_cols(data, remove_first_dummy = TRUE, remove_selected_columns = TRUE)

head(data)
str(data)

#Initially starting with all for exhaustive search with the exception of Income, total_purchases(since Wines was transformed to arrive at total_purchases) and Wines(target variable)
outcome <- "Wines"  # Target variable
identifier <- "ID"  # Identifier variable 
predictors <- c('Kidhome', 'Teenhome', 'LastPurchase', 'Fruits', 'Sweets', 'Gold', 'Meats', 'Fish', 'DiscountPurchases', 
                'WebPurchases', 'CatalogPurchases', 'StorePurchases', 
                'WebVisitsMonth', 'AcceptedCmp3', 
                'AcceptedCmp4', 'AcceptedCmp5', 'AcceptedCmp1', 'AcceptedCmp2', 'Complain', 
                'Response', 'Education_non_graduate', 'Age_Intervals_other',
                'Marital_Status_Married' , 'Marital_Status_Single' , 'Marital_Status_Together',
                'Marital_Status_Widow','Income_Intervals_other_income', 'Recency_Intervals_31-60 days', 'Recency_Intervals_61-90 days', 
                'Recency_Intervals_More than 90 days')


# Reduce the data set to selected variables
data <- data[, c(identifier, outcome, predictors)]

dim(data)
head(data)

# Partition the data
set.seed(1)  # Set seed for reproducibility
idx <- createDataPartition(data$Wines, p = 0.8, list = FALSE)
train <- data[idx, ]
holdout <- data[-idx, ]

# Run exhaustive search for best subset selection
search <- regsubsets(Wines ~ ., data = train, nbest = 1, nvmax = ncol(train), method = "exhaustive")

# Summary of the best subsets
summary(search)

# Determine the optimal subset based on Cp statistic or other criteria
optimal <- which.min(summary(search)$cp)

# Get the variable names for the optimal model
X <- summary(search)$which[, -1]
xvars <- dimnames(X)[[2]]
xvars <- xvars[X[optimal,]]
xvars


xvars <- data[, c("Kidhome",  "Teenhome", "LastPurchase", "Sweets", "Meats", "Fish","WebPurchases" ,
                  "CatalogPurchases", "WebVisitsMonth", "StorePurchases" , "AcceptedCmp3" ,"AcceptedCmp4", "AcceptedCmp5", "AcceptedCmp1",
                  "Education_non_graduate", "AcceptedCmp2" , "Age_Intervals_other", "Income_Intervals_other_income","Recency_Intervals_More than 90 days")]
              
# Visualize the relationship between selected predictors and the target variable
pairs(~ Wines +	StorePurchases + Complain + Kidhome, data = data)
pairs(~ Wines + Kidhome + Teenhome + LastPurchase + Sweets + Meats + Fish , data = data	)
pairs(~ Wines+ WebPurchases + CatalogPurchases + WebVisitsMonth + StorePurchases + AcceptedCmp3 + AcceptedCmp4 + AcceptedCmp5 + AcceptedCmp1+ Education_non_graduate, data = data)
pairs(~ Wines+ AcceptedCmp2+  Age_Intervals_other+ Income_Intervals_other_income , data = data)


#The pair plots don't reveal any meaningful correlation between the target variable and the predictors. There are also several multicollinear relationship among the predictors. 
#Hence we resorted to these few subset that had strong correlation with Wines.

#DiscountPurchases
#CatalogPurchases
#StorePurchases
#Complain
#Income
#Kidhome


#################################################################################33

# Visualize the relationship between final predictors and the target variable
pairs(~ Wines + CatalogPurchases + StorePurchases + Complain + Kidhome, data = data)
#Discount purchases and Income will be excluded because they are correlated with other predictor variables) 
#Final Predictors
#CatalogPurchases
#StorePurchases
#Complain
#Kidhome

#We will now consider introducing new variables through basis expansion
data$Catalog_purchase_sqrd = data$CatalogPurchases^2
data$StorePurchases_sqrd = data$StorePurchases^2
data$CatalogXStorePurchases = data$CatalogPurchases * data$StorePurchases

# Visualize the relationship between predictors and the target variable
pairs(~ Wines + CatalogPurchases + StorePurchases + Complain + Kidhome + StorePurchases_sqrd + Catalog_purchase_sqrd +CatalogXStorePurchases, data = data)

#There were some form of multicollinearity introduced because of the inclusion of the new variables.
#Hence we will resort to the variables without any engineering

# Select variables for regression
outcome <- "Wines"  # Target variable
identifier <- "ID"  # Identifier variable 
predictors <- c("CatalogPurchases", "StorePurchases", "Complain", "Kidhome")  # Predictor variables 
#predictors <- c("CatalogPurchases", "StorePurchases", "Complain", "Kidhome","Catalog_purchase_sqrd" , "StorePurchases_sqrd")  # Predictor variables 
#predictors <- c("CatalogPurchases", "StorePurchases", "Complain", "Kidhome","CatalogXStorePurchases")  # Predictor variables 

#Now we're ready to model
# Reduce the data set to selected variables
data <- data[, c(identifier, outcome, predictors)]

# Partition the data
set.seed(1)  # Set seed for reproducibility
idx <- createDataPartition(data$Wines, p = 0.7, list = FALSE)
train <- data[idx, ]
holdout <- data[-idx, ]



# Omit outliers from the training data
# Update 'omission_vector' with the indices of outliers if needed
omission_vector <- which(idx %in% c(756, 755, 623, 612, 590, 245, 9, 15, 133, 185, 18, 943, 343, 316,
                                    10, 11, 25, 40, 52, 54, 64, 66, 67, 69, 72, 74, 78, 80, 81, 82, 86,
                                    87, 89, 90, 95, 131, 135, 136, 138, 144, 148, 160, 161, 170, 178, 192,
                                    195, 212, 215, 216, 224, 234, 239, 269, 279, 285, 292, 321, 336, 360, 
                                    365, 381, 382, 383, 387, 394, 399, 413, 430, 441, 447, 469, 478, 483, 
                                    484, 523, 554, 564, 572, 596, 602, 608, 646, 658, 664, 666, 667, 668,
                                    671, 674, 675, 686, 702, 727, 731, 748, 772, 774, 776, 777, 791, 800, 
                                    849, 861, 869, 880, 894, 904, 922, 923, 929, 942, 968, 1041, 1073, 1091,
                                    
                                    3, 4, 16, 17, 22, 29, 34, 42, 44, 53, 57, 71, 103, 104, 109, 112, 114, 115,
                                    129, 139, 147, 223, 226, 227, 237, 240, 244, 249, 262, 270, 293, 294, 297, 
                                    329, 351, 355, 426, 427, 428, 439, 450, 453, 459, 465, 466, 475, 485, 486, 
                                    489, 496, 503, 509, 524, 531, 535, 548, 568, 585, 616, 624, 629, 649, 650,
                                    657, 672, 681, 691, 695, 736, 740, 746, 778, 785, 806, 815, 822, 825, 828, 
                                    866, 878, 879, 883, 900, 1012, 1083, 1436
                                    ))  


# Add indices of outliers here
idx <- idx[-omission_vector]
train <- data[idx, ]
holdout <- data[-idx, ]


# We created this function to help identify outliers without looking out for them manually
# Function to remove outliers based on Cook's distance
remove_outliers <- function(model, threshold = 4/(nrow(model$model) - length(model$coefficients) - 1)) {
  cooksd <- cooks.distance(model)
  # Find the influential points
  influential <- as.numeric(names(cooksd)[(cooksd > threshold)])
  # Remove influential points
  data_clean <- model$model[-influential,]
  return(list(clean_data = data_clean, removed_points = influential))
}

# Apply the function to the model
model <- lm_model  
result <- remove_outliers(model)

# Inspect removed points
print(result$removed_points)



# Fit linear regression model
lm_model <- lm(Wines ~ .-Complain, data = train[, c(outcome, predictors)])


# use options() to ensure numbers are not displayed in scientific notation.
#options(scipen = 999)
#summary(lm_model)
#options(scipen = 0) # back to scientific notation


vif(lm_model)

## additional diagnostics to check for outliers/leverage points
par(mfrow = c(2,2))
plot(lm_model)



# Summary of the model
summary(lm_model)

# Check for multicollinearity
vif(lm_model)

# Additional diagnostics
par(mfrow = c(2,2))
plot(lm_model)

# Predict on holdout data
pred <- predict(lm_model, newdata = holdout)

# Calculate performance metrics
training_predictions <- predict(lm_model, newdata = train)
holdout_predictions <- predict(lm_model, newdata = holdout)

training_summary <- summary(lm_model)
training_summary

training_metrics <- c(RMSE = sqrt(mean((train$Wines - training_predictions)^2)),
                      MAE = mean(abs(train$Wines - training_predictions)))


holdout_metrics <- c(RMSE = sqrt(mean((holdout$Wines - holdout_predictions)^2)),
                     MAE = mean(abs(holdout$Wines - holdout_predictions)))


performance_metrics <- data.frame(Training = training_metrics, Holdout = holdout_metrics)
performance_metrics


# Histogram of residuals
ggplot() +
  geom_histogram(aes(x = residuals(lm_model)), fill = "lightgray", color = "grey") +
  labs(x = "Residuals", y = "Frequency") +
  theme_bw()

# Cross-validation approach
set.seed(1)
trControl <- caret::trainControl(method = "cv", number = 5, allowParallel = TRUE)
model <- caret::train(Wines ~ .-Complain, data = train,
                      method = "lm", trControl = trControl)
model
coef(model$finalModel)
summary(model)
options(scipen = 999, digits = 3) 

# take a look at the first 20 Wines, predicted price vs. actual price = residual
data.frame(
  'Predicted' = pred[1:20],
  'Actual' = holdout$Wines[1:20],
  'Residual' = holdout$Wines[1:20] - pred[1:20]
)
options(scipen = 999, digits = 3)
