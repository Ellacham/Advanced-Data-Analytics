# LOGISTIC REGRESSION 
## Balanced Dataset (Categorical Outcome - RESPONSE)

library(ggrepel)
library(fastDummies)
library(caret)
library(leaps)
data <- read.csv("C:\\Users\\amuku\\Downloads\\updated_cleaned_dataset_balanced.csv")

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
predictors <- c('Kidhome', 'Teenhome', 'LastPurchase', 'Fruits', 'Sweets', 'Gold', 'Meats', 'Fish', 'DiscountPurchases', 
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


xvars <- data[, c("Teenhome",  "Meats", "LastPurchase", "DiscountPurchases" , "CatalogPurchases", "WebVisitsMonth" ,"WebPurchases" ,
                  "StorePurchases", "AcceptedCmp4", "AcceptedCmp3" , "AcceptedCmp5" ,"AcceptedCmp1", "AcceptedCmp2", "Education_non_graduate",
                  "Marital_Status_Married", "Marital_Status_Together","Recency_Intervals_More than 90 days", "Response", "Income")]
str(xvars)
data <- xvars
##adding some interaction terms
data$DiscStore = data$DiscountPurchases * data$StorePurchases  
data$WebCatalog = data$WebPurchases * data$CatalogPurchases  # Another interaction term
data$IncomeSqrd = data$Income^2  # A polynomial term (square of Income)
data$sqtMeats = sqrt(data$Meats)  #  polynomial term (square root of Wines)
str(data)
########################################################################
#We first set aside 5% to test our model's performance before splitting as train and hold out
## new to the neighborhood, for example
set.seed(35)  # Ensure reproducibility
 # Set seed for reproducibility
idx <- createDataPartition(data$Response, p = 0.8, list = FALSE)

train <- data[idx, ]
str(train)

holdout <- data[-idx, ]
str(holdout)


# build model
trControl = caret::trainControl(method="cv", number=5, allowParallel=TRUE)
logit.reg = caret::train(Response ~ . , data=train, trControl=trControl,
                         # fit logistic regression with a generalized linear model
                         method="glm", family="binomial")
summary(logit.reg$finalModel)
par(mfrow = c(2,2))
plot(logit.reg$finalModel)
par(mfrow = c(1,1))



hist(as.numeric(train$Response))

# Let's check the confusion matrix
cm = confusionMatrix(as.factor( ifelse(logit.reg$finalModel$fitted.values > 0.5, 1, 0) ), 
                     as.factor(train[,'Response']))
sensitivity = cm$byClass[1] # also known as recall
specificity = cm$byClass[2] # also known as precision
sensitivity #82.46
specificity #81.34

# Let's calculate F1, which combines sensitivity and specificity
F1 = (2 * sensitivity * specificity) / (sensitivity + specificity)
cat( "F1 statistic = ", round(F1,3)) 
#F1 statistic =  0.819

# remove any unnecessary predictors?
logit.reg = caret::train(Response ~ ., data=train, trControl=trControl,
                         # fit logistic regression with a generalized linear model
                         method="glm", family="binomial")

summary(logit.reg$finalModel)
par(mfrow = c(2,2))
plot(logit.reg$finalModel)
par(mfrow = c(1,1))

round(data.frame(summary(logit.reg$finalModel)$coefficients,
                 odds = exp(coef(logit.reg$finalModel))), 5)

# Let's check the confusion matrix
cm = confusionMatrix(as.factor( ifelse(logit.reg$finalModel$fitted.values > 0.5, 1, 0) ), 
                     as.factor(train[,'Response']))
 str(cm)
 cm
sensitivity = cm$byClass[1] # also known as recall
specificity = cm$byClass[2] # also known as precision

# Let's calculate F1, which combines sensitivity and specificity
F1 = (2 * sensitivity * specificity) / (sensitivity + specificity)
cat( "F1 statistic = ", round(F1,3))

## Evaluating Classification Performance
### Interpreting Results in Terms of Odds (for a Profiling Goal)

# use predict() with type = "response" to compute predicted probabilities.
#logit.reg.pred = predict(logit.reg, holdout[, -8], type = "prob")
# Use predict() correctly without arbitrarily excluding columns by index
logit.reg.pred = predict(logit.reg, newdata = holdout, type = "prob")

str(holdout)
str(train)


# display four different cases
interestingCases = c(1, 12, 32, 1333)
data.frame(
  actual = holdout$Response[interestingCases],
  p0 = logit.reg.pred[interestingCases, 1],
  p1 = logit.reg.pred[interestingCases, 2],
  predicted = ifelse(logit.reg.pred[interestingCases, 2] > 0.5, 1, 0)
)


# Generating predicted classes from logistic regression model's fitted values
predicted_classes <- as.factor(ifelse(logit.reg$finalModel$fitted.values > 0.5, 1, 0))

# Actual classes from the training dataset
actual_classes <- as.factor(train$Response)
# Creating the confusion matrix
cm <- confusionMatrix(predicted_classes, actual_classes)
cm

sensitivity = cm$byClass[1] # also known as recall
sensitivity
specificity = cm$byClass[2] # also known as precision
specificity



# Let's calculate F1, which combines sensitivity and specificity
F1 = (2 * sensitivity * specificity) / (sensitivity + specificity)
cat( "F1 statistic = ", round(F1,3))

# Assuming 'logit.reg.pred[,2]' contains the predicted probabilities for the positive class
# and 'actual' contains the actual binary outcomes (1s and 0s).

library(ROCR)
library(ggplot2)
library(gridExtra)

# ROC Curve and Lift Curve with ROCR
pred <- prediction(logit.reg.pred[,2], actual)

# ROC Curve
perf_roc <- performance(pred, "tpr", "fpr")
plot(perf_roc, colorize = TRUE, main="ROC Curve")

# Lift Curve
perf_lift <- performance(pred, measure = "lift", x.measure = "rpp")
plot(perf_lift, main = "Lift Curve")

# Manual Gains Calculation and Lift Chart
# Sort the indices based on predicted probabilities
sorted_indices <- order(logit.reg.pred[,2], decreasing = TRUE)

# Arrange the actual values based on these sorted indices
sorted_actual <- actual[sorted_indices]

# Calculate cumulative gains
cumulative_gains <- cumsum(sorted_actual) / sum(sorted_actual)

# Calculate deciles
deciles <- seq(1, length(cumulative_gains), by = length(cumulative_gains)/10)

# Plot Cumulative Gains
g1 <- ggplot(data.frame(x = 1:length(cumulative_gains), y = cumulative_gains), aes(x = x, y = y)) +
  geom_line() +
  xlab("Number of Cases") +
  ylab("Cumulative Gains") +
  ggtitle("Cumulative Gains Chart")

# Calculate and Plot Lift at Deciles
lift_values <- sapply(deciles, function(d) cumulative_gains[d] / (d / length(sorted_actual)))
decile_indices <- deciles / length(sorted_actual) * 100

g2 <- ggplot(data.frame(Decile = decile_indices, Lift = lift_values), aes(x = Decile, y = Lift)) +
  geom_col(fill = "steelblue") +
  xlab("Decile") +
  ylab("Lift") +
  ggtitle("Lift Chart")

# Combine and display both plots side by side
grid.arrange(g1, g2, ncol = 2)

