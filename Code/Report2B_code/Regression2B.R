install.packages('tidyverse')
install.packages('car')

library(car)
library(caret)
library(tidyverse)
library(MASS) # for Box-Cox transformation to get a normal distribution for Wines since our target variable(Wines)  is skewed

# Load the dataset
data <- read.csv('updated_cleaned_dataset2.csv')

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

#First convert categorical values to dummies
data <- dummy_cols(data, remove_first_dummy = TRUE, remove_selected_columns = TRUE)

head(data)
str(data)

# Select variables for regression
outcome <- "Wines"  # Target variable
identifier <- "ID"  # Identifier variable 
predictors <- c("CatalogPurchases", "StorePurchases", "Kidhome", 'Hclusters', 'Kmcluster')  # Predictor variables 

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
                                    866, 878, 879, 883, 900, 1012, 1083, 1436,
                                    #new outliers removed
                                    97, 98, 100, 107, 118, 121, 124, 158, 171, 173, 187, 196, 197, 203, 205, 220, 
                                    230, 253, 271, 284, 286, 301, 302, 303, 320, 326, 327, 347, 502, 588, 634, 680,
                                    696, 725, 726, 730, 810, 847, 856, 889, 890, 906, 914, 932, 944, 948, 981, 1038,
                                    1042, 1043, 1053, 1072, 1085, 1109, 1115, 1116, 1138, 1139, 1151, 1152, 1173,
                                    1178, 1190, 1230, 1237, 1240, 1241, 1276, 1376, 1445, 1720, 1726, 1733, 1884,
                                    1906, 1983, 1993, 1997, 2011, 2013, 2018, 2028, 2118, 2132, 2134, 2156, 2187, 
                                    2192, 2193, 2206, 2219, 2233,
                                    
                                    7, 8, 13, 14, 19, 20, 23, 24, 26, 33, 35, 36, 37, 39, 41, 43, 46, 47, 49, 56,
                                    58, 59, 68, 88, 96, 130, 172, 189, 190, 213, 233, 256, 261, 281, 283, 300, 306, 
                                    330, 342, 356, 358, 361, 371, 395, 398, 417, 423, 468, 471, 504, 512, 601, 620, 
                                    652, 716, 795, 813, 834, 844, 846, 860, 863, 877, 885, 895, 935, 953, 976, 1019,
                                    1062, 1106, 1127, 1193, 1247, 1250, 1368, 1550, 1670, 1685, 1686, 1786, 1791, 1802, 
                                    1921, 1951
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
lm_model <- lm(Wines ~ ., data = train[, c(outcome, predictors)])

vif(lm_model)

## additional diagnostics to check for outliers/leverage points
par(mfrow = c(2,2))
plot(lm_model)

# Summary of the model
summary(lm_model)



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


#################################################################################################

# take a look at the first 20 Wines, predicted price vs. actual price = residual
data.frame(
  'Predicted' = pred[1:20],
  'Actual' = holdout$Wines[1:20],
  'Residual' = holdout$Wines[1:20] - pred[1:20]
)
options(scipen = 999, digits = 3)
