#removed age and dob outliers, imputed with mean, merged marital status. absurd = single, alon = single. EDU - Graduation to graduate
#ages were calculated out of the year_bith variable.unnecessary columns were removed
install.packages("fmsb")
install.packages("psych")
library(dplyr)
library(fmsb)
library(psych)

#1 Read the CSV file
data <- read.csv('Personality Analytics.csv')

#assess the data structure
str(data)

# Check the first few rows
head(data)

#inspect column names
colnames(data)
#rename some columns
data <- data %>%
  rename(date_enrolled = Dt_Customer, 
         last_purchase = Recency,
         Wines = MntWines, 
         Fruits = MntFruits,
         Meat = MntMeatProducts,
         Fish = MntFishProducts,
         Sweet = MntSweetProducts, 
         Gold = MntGoldProds,
         discount_purchase = NumDealsPurchases,
         web_purchases = NumWebPurchases,
         Catalog_Purchases = NumCatalogPurchases,
         Store_Purchases = NumStorePurchases,
         Web_Visits = NumWebVisitsMonth
         )
head(data)


#Engi. #calculate age and add to dataset
age_fnc <- function(data){
  data$Age <- 2024 - data$Year_Birth
  return(data)
}

data <- age_fnc(data)
head(data)

#transform values in marital status
data <- data %>%
  mutate(Marital_Status=ifelse(Marital_Status == 'Alone', 'Single', Marital_Status))
head(data)

#remove unnecessary columns and explain why
data <- data %>%
  select(-Year_Birth)
head(data)

#transform values in education
data<- data %>%
  mutate(Education = ifelse(Education == 'Graduation', 'Graduate', Education))
head(data)
data$Education

#transform similar values in marital status - Ian
data <- data %>%
  mutate(Marital_Status = ifelse(Marital_Status=='Alone', 'Single', Marital_Status))
head(data)

str(data)

#change data types where necessary
data$Age <- as.integer(data$Age)


glimpse(data)
summary(data)



#identifying rows with missing values
rows_with_missing <- which(rowSums(is.na(data)) > 0)
# Display the rows with missing values
if (length(rows_with_missing) > 0) {
  print("Rows with missing values:")
  print(data[rows_with_missing, ])
} else {
  print("No missing values found.")
}

#initial visualization of income distribution to decide on mode of imputation
inc_vis <-ggplot(data = data, aes(x=Income))+
  geom_histogram(binwidth = 9000, fill = "blue")+
  labs(title = 'Income Distribution', x = "Income", y = "Frequency")+
  theme_minimal()
inc_vis

#to investigate if mean and median are same and to decide which one to use
summary(data$Income) 

  
# correcting NAs in Income using median imputation
data <- data %>%
  mutate(Income = ifelse(is.na(Income), median(Income, na.rm = TRUE), Income))

#assessing the impact on summary statistics
summary(data$Income)


#we now examine again to see if NAs are resolved
#identifying rows with missing values
rows_with_missing <- which(rowSums(is.na(data)) > 0)
# Display the rows with missing values
if (length(rows_with_missing) > 0) {
  print("Rows with missing values:")
  print(data[rows_with_missing, ])
} else {
  print("No missing values found.")
}

#Engi./segmentation. Create intervals for Income
income_intervals <- cut(data$Income, breaks = c(0, 30000, 60000, 90000, Inf), labels = c("Low", "Medium", "High", "Very High"))
income_intervals
# Add the income intervals feature to the dataset
data$Income_Intervals <- income_intervals

#Engi./segmentation. Create intervals for Age
data$Age_Intervals  <- cut(data$Age, breaks = c(0, 30, 50, 70, Inf), labels = c("Young", "Adult", "Middle-Aged", "Senior"))
data$Age_Intervals

#VIZUALIZING VARIABLES FOR OUTLIER DETECTION AND RESOLUTION
#for the numerical variables we aimed to first plot to have a look at the distribution. 
#Then once outliers were detected, we treated and visualized again to see the effect.
#1.FINANCIAL FEATURE
#a. Income: Numerical. 
income_distribution <- ggplot(data, aes(x = Income)) +
  geom_boxplot(fill = "green", color = "black") +
  labs(title = "Income Distribution") +
  theme_minimal()
income_distribution
#income has outliers

# Investigating Income outliers causes
Q3_Income <- quantile(data$Income, 0.75)
IQR_Income <- IQR(data$Income)
upper_limit_Income <- Q3_Income + 1.5 * IQR_Income

# Create a subset of outliers
outliers_Income <- data[data$Income > upper_limit_Income, c("ID", "Education", "Income", "Marital_Status")]
kable(outliers_Income)
summary(outliers_Income$Income)

count(outliers_Income)
#8




#As a general approach in handling outliers in this project, we aim to utilize 
# Winsorization(pulling outliers towards the lower and upper bounds of the distribution)
#the reason being that we deem it to be simple as it doesn't affect the distribution's shape and makes interpretability easy unlike log transformation.

#handling outliers in Income
Q1_Income <- quantile(data$Income, 0.25)
lower_limit_Income <- Q1_Income - 1.5 * IQR_Income

# Winsorize the data
data$Income <- ifelse(data$Income < lower_limit_Income, lower_limit_Income, 
                                 ifelse(data$Income > upper_limit_Income, upper_limit_Income, data$Income))

summary(data$Income)


# distribution after handling NAs and Outliers - histogram
corrected_income <- ggplot(data, aes(x = Income)) +
  geom_histogram(binwidth = 5000, fill = "yellow", color = "black") +
  labs(title = "Income Distribution without Outliers", x = "Income", y = "Frequency") +
  theme_minimal()
corrected_income


#good income distribution - distribution by age (multiple variables)
ridgeline_plot_income <- ggplot(data, aes(x = Income, y = Age_Intervals, fill = Age_Intervals)) +
  geom_density_ridges() +
  labs(title = "Ridgeline Plot of Income by Age Group", x = "Income", y = "Age Intervals") +
  theme_minimal()

# Display the plot
ridgeline_plot_income

#income distribution - distribution by education
ridgeline_plot_income_edu <- ggplot(data, aes(x = Income, y = Education, fill = Education)) +
  geom_density_ridges() +
  labs(title = "Ridgeline Plot Income by Education", x = "Income", y = "Education") +
  theme_minimal()

# Display the plot
ridgeline_plot_income_edu



##2. DEMOGRAPHIC FEATURES. Majority are categorical so we chose to use bar plots for categorical and histogram and boxplot for numeric variables
#a. Education: Categorical
# Get the frequency of each Education level
edu_freq <- table(data$Education)

# Sort Education levels by frequency in descending order
sorted_levels <- names(sort(edu_freq, decreasing = TRUE))

# Arrange Education levels in the sorted order
data$Education <- factor(data$Education, levels = sorted_levels)

# Create the plot
edu_distribution <- ggplot(data, aes(x = Education, fill = Marital_Status)) +
  geom_bar() +
  labs(title = "Education Distribution by Marital Status") +
  theme_minimal() 
 # scale_fill_manual(values = scales::hue_pal()(length(sorted_levels)))

# Show the plot
edu_distribution



##education distribution by marital status ##do this after merging (multiple variable)
edu_distribution_marital <- ggplot(data, aes(x = income_intervals, fill = Marital_Status)) +
  geom_bar(position = "stack") +
  labs(title = "Income Distribution by Marital Status") +
  theme_minimal() 
 # scale_fill_manual(values = scales::hue_pal()(nlevels(data$Marital_Status)))

# Show the plot
edu_distribution_marital
###




#b. marital status: Categorical
marital_distribution <- ggplot(data, aes(x = Marital_Status, fill = Marital_Status)) +
  geom_bar(color = "white", alpha = 0.7, width = 0.8) +
  labs(title = "Marital Status Distribution", x = "Marital Status", y = "Count")+
  theme_minimal()
##consider adding YOLO and absurd to single since it's small.
marital_distribution
##was singles initially ranked 2nd?

# Engi. adding YOLO and absurd to single
data <- data%>%
  mutate(Marital_Status = ifelse(Marital_Status == 'Absurd', 'Single',
                                 ifelse(Marital_Status == 'YOLO', 'Single', Marital_Status)))

#distribution after merging
marital_distribution <- ggplot(data, aes(x = Marital_Status, fill = Marital_Status)) +
  geom_bar(color = "white", alpha = 0.7, width = 0.8) +
  labs(title = "Marital Status Distribution After Merging", x = "Marital Status", y = "Count")+
  theme_minimal()
##consider adding YOLO and absurd to single since it's small.
marital_distribution
##was singles initially ranked 2nd?                               

###
# Pie chart for Marital Status - ignore
marital_pie_chart <- ggplot(data, aes(x = "", fill = Marital_Status)) +
  geom_bar(width = 1, stat = "count") +
  coord_polar("y") +
  labs(title = "Marital Status Distribution") +
  theme_minimal()

# Show the pie chart
marital_pie_chart
###
 
#c. Age1_all: Categorical and Numeric
age_box_plot_discrete <- ggplot(data, aes(x = "", y = Age)) +
  geom_boxplot(fill = "skyblue", color = "black", alpha = 0.7, outlier.shape = 25, width = 4, outlier.color = 'red', outlier.fill = 'red') +
  labs(title = "Box Plot of Age", x = NULL, y = "Age") +
  theme_minimal()
age_box_plot_discrete
#age has some outliers


# Box Plot for Age accross categories to investigate cause of outlier
age_box_plot_intervals <- ggplot(data, aes(x = Age_Intervals, y = Age, fill = Age_Intervals)) +
  geom_boxplot(color = "black") +
  labs(title = "Box Plot of Age by Group", x = "Age Intervals", y = "Age") +
  theme_minimal()
age_box_plot_intervals
#we confirmed this was coming from the seniors group where people are over 100 years. We want to consider removing them from our dataset because they should be dead by now


#handling outliers in age
Q1_age <- quantile(data$Age, 0.25)
Q3_age <- quantile(data$Age, 0.75)
IQR_age <- IQR(data$Age)
lower_limit_age <- Q1_age - 1.5 * IQR_age
upper_limit_age <- Q3_age + 1.5 * IQR_age

summary(data$Age)
# Winsorize the data
data$age <- ifelse(data$age < lower_limit_age, lower_limit_age, 
                      ifelse(data$Age > upper_limit_age, upper_limit_age, data$Age))
str(data)
summary(data$age) - ignore
###age distribution without outliers
# Violin Plot for Age
age_violin_plot <- ggplot(data, aes(x = income_intervals, y = Education)) +
  geom_violin(fill = "skyblue", color = "black") +
  labs(title = "Violin Plot of Age", x = "Age Intervals", y = "Age") +
  theme_minimal()

age_violin_plot
# Density Plot for Age - ignore
age_density_plot <- ggplot(data, aes(x = age, fill = Age_Intervals)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Plot of Age", x = "Age", y = "Density") +
  theme_minimal()
age_density_plot

# Ridgeline Plot for Age ##love this
age_ridgeline_plot <- ggplot(data, aes(x = Age, y = Age_Intervals, fill = Age_Intervals)) +
  geom_density_ridges() +
  labs(title = "Ridgeline Plot of Age", x = "Age", y = "Age Intervals") +
  theme_minimal()
age_ridgeline_plot




#d. kidhome
bar_plot_kidhome <- ggplot(data, aes(x = factor(Kidhome)), fill = Kidhome) +
  geom_bar( color = "skyblue") +
  labs(title = "Distribution of Kidhome", x = "Kidhome", y = "Count") +
  theme_minimal()
bar_plot_kidhome



#e.teenhome
bar_plot_teenhome <- ggplot(data, aes(x = factor(Teenhome))) +
  geom_bar(fill = "lightcoral", color = "black") +
  labs(title = "Distribution of Teenhome", x = "Teenhome", y = "Count") +
  theme_minimal()
# Display the plot
bar_plot_teenhome
##consider adding both kidhome and teenhome if they show correlation

#f. numeric and categorical
box_plot_recency <- ggplot(data, aes(y = LastPurchase)) +
  geom_boxplot(fill = "lightgreen", color = "black") +
  labs(title = "Recency of purchase", y = "Days Since Last Purchase") +
  theme_minimal()

# Display the plot
box_plot_recency
#no outlier


#engi. To better understand the data, we introduced age buckets defined as 1,2,3 or more months since last purchase,
# Define the intervals for Recency
intervals <- c(0, 30, 60, 90, Inf)  

# Create the Recency_Intervals variable
data$Recency_Intervals <- cut(data$last_purchase, breaks = intervals, labels = c("0-30 days", "31-60 days", "61-90 days", "More than 90 days"), include.lowest = TRUE)

# Create a histogram for Recency_Intervals
histogram_recency_intervals <- ggplot(data, aes(x = factor(Recency_Intervals), fill = "lightblue")) +
  geom_bar(width = 0.9, color = "black", position = "identity") +
  labs(title = "Histogram of Recency Intervals", x = "Recency Intervals", y = "Frequency") +
  theme_minimal()

# Display the plot
histogram_recency_intervals


#now we sort to understand the trend overtime
cumulative_density_recency <- ggplot(data, aes(x = LastPurchase, fill = "lightblue")) +
  geom_density(aes(y = after_stat(cumsum(..density..))), color = "black", alpha = 0.7) +
  labs(title = "Cumulative Density of Purchase Recency", x = "Recency", y = "Cumulative Density") +
  theme_minimal()

# Display the plot
print(cumulative_density_recency)

#######
#more numerical variables: Wine
box_plot_Wines <- ggplot(data, aes(y = Wines)) +
  geom_boxplot(fill = "blue", color = "black") +
  labs(title = "Wines Bought after Treating Outliers", y = "Total") +
  theme_minimal()
box_plot_Wines
#wines have outliers

# Investigating wine outliers causes
Q3_wines <- quantile(data$Wines, 0.75)
IQR_Wines <- IQR(data$Wines)
upper_limit_Wines <- Q3_wines + 1.5 * IQR_Wines

# Create a subset of outliers
outliers_subset_Wines <- data[data$Wines > upper_limit_Wines, ]

#we may focus on them to sell more wines
outliers_subset_Wines %>%
  select(ID, Education, Marital_Status, Income_Intervals, last_purchase, Wines, Fruits, Meat, Fish, Sweet, Gold, discount_purchase, Age_Intervals, Recency_Intervals)

count(outliers_subset_Wines)
#35 is there anything unique to them?

####
#chart to show top 10
top_wine_buyers <- outliers_subset_Wines %>%
  arrange(desc(Wines)) %>%
  slice_head(n = 10)

top_wine_buyers_plot <- ggplot(top_wine_buyers, aes(x = reorder(`ID`, -Wines), y = Wines)) +
  geom_point(size = 3, color = "blue") +
  labs(title = "Top 10 Wine Buyers", x = "Customer ID", y = "Wines Purchased") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
  #theme_minimal()

# Show the plot
top_wine_buyers_plot
###


#handling outliers in Wines
Q1_Wines <- quantile(data$Wines, 0.25)
IQR_Wines <- IQR(data$Wines)
lower_limit_Wines <- Q1_Wines - 1.5 * IQR_Wines

# Winsorize the data
data$Wines <- ifelse(data$Wines < lower_limit_Wines, lower_limit_Wines, 
                      ifelse(data$Wines > upper_limit_Wines, upper_limit_Wines, data$Wines))

summary(data$Wines)
#show the effect now with chart (change the colour of the box plot) and talk about distribution
#in the end, compare top 10 for all to see which product is more profitable
#also count the total number of people for each and the total revenue, now what's the proportion of that to the top 10 profitability.
#we may create a nitch for the top 10 if they control more purchases.
#######

#######
density_plot_Fruits <- ggplot(data, aes( Fish)) +
  geom_density(fill = "peachpuff", color = "black", alpha = 0.9,linewidth = 0.7,size = 10) +
  labs(title = "Fishes Bought After Treating Outliers") +
  theme_minimal()
density_plot_Fruits
#Fruits have outliers

# Investigating fruits outliers causes
Q3_Fruits <- quantile(data$Fruits, 0.75)
IQR_Fruits <- IQR(data$Fruits)
upper_limit_Fruits <- Q3_Fruits + 1.5 * IQR_Fruits


# Create a subset of outliers
outliers_subset_Fruits <- data[data$Fruits > upper_limit_Fruits, ]


count(outliers_subset_Fruits)
#227


#we may focus on them to sell more fruits
outliers_subset_Fruits %>%
  select(Education, Marital_Status, Income_Intervals, last_purchase, Wines, Fruits, Meat, Fish, Sweet, Gold, discount_purchase, Age_Intervals, Recency_Intervals)

####
top_fruit_buyers <- outliers_subset_Fruits %>%
  arrange(desc(Fruits)) %>%
  slice(1:10)

top_fruit_buyer_plot <- ggplot(top_fruit_buyers, aes(x = reorder(ID, Fruits), y = Fruits, fill = Fruits)) +
  geom_col() +
  labs(title = "Top 10 Fruit Buyers by Amount", x = "Customer ID", y = "Amount Spent on Fruits") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
top_fruit_buyer_plot
###

#handling outliers in Fruits
Q1_Fruits <- quantile(data$Fruits, 0.25)
IQR_Fruits <- IQR(data$Fruits)
Q3_Fruits <- quantile(data$Fruits, 0.75)
lower_limit_Fruits <- Q1_Fruits - 1.5 * IQR_Fruits
upper_limit_Fruits <- Q3_Fruits + 1.5 * IQR_Fruits



# Winsorize the data
data$Fruits <- ifelse(data$Fruits <= lower_limit_Fruits, lower_limit_Fruits, 
                     ifelse(data$Fruits > upper_limit_Fruits, upper_limit_Fruits, data$Fruits))

summary(data$Fruits)

#####


######
box_plot_Fish <- ggplot(data, aes(y = Fish)) +
  geom_boxplot(fill = "peachpuff", color = "black") +
  labs(title = "Box Plot of Fishes Bought") +
  theme_minimal()
box_plot_Fish
#Fishes have outliers

# Investigating fish outliers causes
Q3_Fish <- quantile(data$Fish, 0.75)
IQR_Fish <- IQR(data$Fish)
upper_limit_Fish <- Q3_Fish + 1.5 * IQR_Fish

# Create a subset of outliers
outliers_subset_Fish <- data[data$Fish > upper_limit_Fish, ]


count(outliers_subset_Fish)
#223

#we may focus on them to sell more fruits
outliers_subset_Fish %>%
  select(Education, Marital_Status, Income_Intervals, last_purchase, Wines, Fruits, Meat, Fish, Sweet, Gold, discount_purchase, Age_Intervals, Recency_Intervals)

#chart for top 10 fish buyers
top_fish_buyers <- data %>% 
  arrange(desc(Fish)) %>% 
  slice(1:10)

dot_plot_Fish <- ggplot(top_fish_buyers, aes(x = reorder(ID, Fish), y = Fish)) +
  geom_segment(aes(xend = reorder(ID, Fish), yend = 0), color = "lightcoral", size = 2) +
  geom_point(color = "darkred", size = 4) +
  labs(title = "Top 10 Fish Buyers by Amount", x = "Customer ID", y = "Amount Spent on Fish") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
dot_plot_Fish

#handling outliers in fishes bought
Q1_Fish <- quantile(data$Fish, 0.25)
IQR_Fish <- IQR(data$Fish)
Q3_Fish <- quantile(data$Fish, 0.75)
lower_limit_Fish <- Q1_Fish - 1.5 * IQR_Fish
upper_limit_Fish <- Q3_Fish + 1.5 * IQR_Fish

# Winsorize the data
data$Fish <- ifelse(data$Fish <= lower_limit_Fish, lower_limit_Fish, 
                      ifelse(data$Fish > upper_limit_Fish, upper_limit_Fish, data$Fish))

summary(data$Fish)

#show box plot again
density_plot_Fish <-ggplot(data, aes(x = Fish)) +
  geom_density(fill = "skyblue", color = "darkblue") +
  labs(title = "Distribution of Fishes Bought", x = "Fish") +
  theme_minimal()

density_plot_Fish
######

#######



density_plot_Meat <- ggplot(data, aes( Meats)) +
  geom_density(fill = "grey", color = "black", alpha = 0.9,linewidth = 0.7,size = 10) +
  labs(title = "Density Plot of Meat Bought") +
  theme_minimal()
density_plot_Meat
# meat have outliers

box_plot_Meat <- ggplot(data, aes(x = Meat)) + ## use this
  geom_boxplot(fill = "red", color = "black") +
  labs(title = "Meat Bought after Treating Outliers") +
  theme_minimal()
box_plot_Meat
#Meats have outliers

# Investigating Meat outliers causes
Q3_Meat <- quantile(data$Meats, 0.75)
IQR_Meat <- IQR(data$Meats)
upper_limit_Meat <- Q3_Meat + 1.5 * IQR_Meat

# Create a subset of outliers
outliers_subset_Meat <- data[data$Meats > upper_limit_Meat, ]


count(outliers_subset_Meat)
#174

#we may focus on them to sell more fruits
outliers_subset_Meat %>%
  select(Education, Marital_Status, Income_Intervals, last_purchase, Wines, Fruits, Meat, Fish, Sweet, Gold, discount_purchase, Age_Intervals, Recency_Intervals)

#top 10 meat buyers
top_meat_buyers <- outliers_subset_Meat %>%
  arrange(desc(Meats)) %>%
  slice(1:10)

top_meat_buyers_plot <- ggplot(top_meat_buyers, aes(x = reorder(ID, Meats), y = Meats, fill = Meats)) +
  geom_col() +
  labs(title = "Top 10 Meat Buyers by Amount", x = "Customer ID", y = "Amount Spent on Meat") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
top_meat_buyers_plot

#handling meat outliers
Q1_Meat <- quantile(data$Meats, 0.25)
IQR_Meat <- IQR(data$Meats)
Q3_Meat <- quantile(data$Meats, 0.75)
lower_limit_Meat <- Q1_Meat - 1.5 * IQR_Meat
upper_limit_Meat <- Q3_Meat + 1.5 * IQR_Meat

# Winsorize the data
data$Meat <- ifelse(data$Meats <= lower_limit_Meat, lower_limit_Meat, 
                    ifelse(data$Meats > upper_limit_Meat, upper_limit_Meat, data$Meat))

summary(data$Meat)
#distribution of meat
violin_plot_Meat <-ggplot(data, aes(x = " ", y = Meats)) +  ##use this first
  geom_violin(fill = "skyblue", color = "blue") +
  labs(title = "Distribution of Meat Bought",x = "Density", y = "Meat Amount") +
  theme_minimal()
violin_plot_Meat
#####

#######


box_plot_Sweet <- ggplot(data, aes(x = Sweets)) +
  geom_boxplot(fill = "yellow", color = "black") +
  labs(title = "Sweet Bought after Treating Outliers") +
  theme_minimal()
box_plot_Sweet
#Sweet have outliers

# Investigating Sweet outliers causes
Q3_Sweet <- quantile(data$Sweets, 0.75)
IQR_Sweet <- IQR(data$Sweets)
upper_limit_Sweet <- Q3_Sweet + 1.5 * IQR_Sweet

# Create a subset of outliers
outliers_subset_Sweet <- data[data$Sweet > upper_limit_Sweet, ]


count(outliers_subset_Sweet)
#248

#we may focus on them to sell more fruits
outliers_subset_Sweet %>%
  select(Education, Marital_Status, Income_Intervals, last_purchase, Wines, Fruits, Meat, Fish, Sweet, Gold, discount_purchase, Age_Intervals, Recency_Intervals)

#top 10 sweet buyers
#chart for top 10 fish buyers
top_Sweet_buyers <- data %>% 
  arrange(desc(Sweets)) %>% 
  slice(1:10)

dot_plot_Sweet <- ggplot(top_Sweet_buyers, aes(x = reorder(ID, Sweets), y = Sweets)) +
  geom_segment(aes(xend = reorder(ID, Sweets), yend = 0), color = "blue", size = 2) +
  geom_point(color = "grey", size = 4) +
  labs(title = "Top 10 sweet Buyers by Amount", x = "Customer ID", y = "Amount Spent on sweet") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
dot_plot_Sweet



#handling outliers
Q1_Sweet <- quantile(data$Sweets, 0.25)
IQR_Sweet <- IQR(data$Sweets)
Q3_Sweet <- quantile(data$Sweets, 0.75)
lower_limit_Sweet <- Q1_Sweet - 1.5 * IQR_Sweet
upper_limit_Sweet <- Q3_Sweet + 1.5 * IQR_Sweet

data$Sweet <- ifelse(data$Sweets < lower_limit_Sweet, lower_limit_Sweet, 
                    ifelse(data$Sweets > upper_limit_Sweet, upper_limit_Sweet, data$Sweet))
summary(data$Sweet)

#sweet distribution
density_plot_Sweet <- ggplot(data, aes( Sweets)) +
  geom_density(fill = "blue", color = "black", alpha = 0.9,linewidth = 0.7,size = 10) +
  labs(title = "Density Plot of Sweet Bought") +
  theme_minimal()
density_plot_Sweet
#Sweet have outliers
#####

#######


box_plot_Gold <- ggplot(data, aes(x = Gold)) +
  geom_boxplot(fill = "green", color = "black") +
  labs(title = "Box Plot of Gold Bought") +
  theme_minimal()
box_plot_Gold
#Gold have outliers

# Investigating Gold outliers causes
Q3_Gold <- quantile(data$Gold, 0.75)
IQR_Gold <- IQR(data$Gold)
upper_limit_Gold <- Q3_Gold + 1.5 * IQR_Gold
Q1_Gold <- quantile(data$Gold, 0.25)
lower_limit_Gold <- Q1_Gold - 1.5 * IQR_Gold

# Create a subset of outliers
outliers_subset_Gold <- data[data$Gold > upper_limit_Gold, ]


count(outliers_subset_Gold)
#207

#we may focus on them to sell more gold
outliers_subset_Gold %>%
  select(Education, Marital_Status, Income_Intervals, last_purchase, Wines, Fruits, Meat, Fish, Sweet, Gold, discount_purchase, Age_Intervals, Recency_Intervals)

#top 10 gold buyers
top_gold_buyers <- outliers_subset_Gold %>%
  arrange(desc(Gold)) %>%
  slice(1:10)

top_gold_buyers_plot <- ggplot(top_gold_buyers, aes(x = reorder(ID, Gold), y = Gold, fill = Gold)) +
  geom_col() +
  labs(title = "Top 10 Gold Buyers by Amount", x = "Customer ID", y = "Amount Spent on Gold") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
top_meat_buyers_plot


#handling outliers
# Winsorize the data
data$Gold <- ifelse(data$Gold < lower_limit_Gold, lower_limit_Gold, 
                    ifelse(data$Gold > upper_limit_Gold, upper_limit_Gold, data$Gold))

summary(data$Gold)

#gold distribution
density_plot_Gold <- ggplot(data, aes( Gold)) +
  geom_density(fill = "blue", color = "black", alpha = 0.9,linewidth = 0.7,size = 10) +
  labs(title = "Gold Bought after Treating Outliers") +
  theme_minimal()
density_plot_Gold

#####

####
# engi. Calculate the total purchases for each individual
data$total_purchases  <- rowSums(data[, c("Wines", "Fruits", "Meats", "Fish", "Sweets", "Gold")])

# Select the top 10 individuals based on total purchases
top_total_buyers <- head(data[order(data$total_purchases, decreasing = TRUE), ], 10)


# Transform data for spider plot (excluding 'total_purchases' column)
spider_data_total <- as.data.frame(top_total_buyers[, c("Wines", "Fruits", "Meats", "Fish", "Sweets", "Gold")])


# Create the spider plot
radarchart_total <- radarchart(spider_data_total, title = "Top 10 Total Buyers", cglcol = "gray", cglty = 1, axislabcol = "black")
radarchart_total

####



str(data)

###
box_plot_discount_purchase <- ggplot(data, aes(x = DiscountPurchases)) +
  geom_boxplot(fill = "red", color = "black") +
  labs(title = "Discount Purchases after Treating Outliers") +
  theme_minimal()
box_plot_discount_purchase
#discount_purchase have outliers

# Investigating discount_purchase outliers causes
Q3_discount_purchase <- quantile(data$DiscountPurchases, 0.75)
IQR_discount_purchase <- IQR(data$DiscountPurchases)
upper_limit_discount_purchase <- Q3_discount_purchase + 1.5 * IQR_discount_purchase
lower_limit_discount_purchase <- Q1_discount_purchase - 1.5 * IQR_discount_purchase
Q1_discount_purchase <- quantile(data$DiscountPurchases, 0.25)


# Create a subset of outliers
outliers_discount_purchase <- data[data$DiscountPurchases > upper_limit_discount_purchase, ]

count(outliers_discount_purchase)
#86. quite a few? why? not attractive discount or targeted at some people. we will investigate

#we may focus on them to give more discount
outliers_discount_purchase %>%
  select(Education, Marital_Status, Income_Intervals, last_purchase, Wines, Fruits, Meat, Fish, Sweet, Gold, discount_purchase, Age_Intervals, Recency_Intervals)

data$DiscountPurchases <- ifelse(data$DiscountPurchases < lower_limit_discount_purchase, lower_limit_discount_purchase, 
                     ifelse(data$DiscountPurchases > upper_limit_discount_purchase, upper_limit_discount_purchase, data$DiscountPurchases))
summary(data$DiscountPurchases)

###

###
box_plot_web_purchases <- ggplot(data, aes(y = WebPurchases)) +
  geom_boxplot(fill = "yellow", color = "black") +
  labs(title = "Web Purchases after Treating Outliers") +
  theme_minimal()
box_plot_web_purchases
#web_purchases have outliers

# Investigating web_purchases outliers causes
Q3_web_purchases <- quantile(data$WebPurchases, 0.75)
IQR_web_purchases <- IQR(data$WebPurchases)
upper_limit_web_purchases <- Q3_web_purchases + 1.5 * IQR_web_purchases

# Create a subset of outliers
outliers_subset_web_purchases <- data[data$WebPurchases > upper_limit_web_purchases, ]


count(outliers_subset_web_purchases)
#4


outliers_subset_web_purchases %>%
  select(Education, Marital_Status, Income_Intervals, last_purchase, Wines, Fruits, Meat, Fish, Sweet, Gold, discount_purchase, Age_Intervals, Recency_Intervals, web_purchases)

Q1_web_purchases <- quantile(data$WebPurchases, 0.25)
lower_limit_web_purchases <- Q1_web_purchases - 1.5 * IQR_web_purchases

data$WebPurchases <- ifelse(data$WebPurchases <= lower_limit_web_purchases, lower_limit_web_purchases, 
                                 ifelse(data$WebPurchases > upper_limit_web_purchases, upper_limit_web_purchases, data$WebPurchases))
summary(data$web_purchases)

#####


###
box_plot_Catalog_Purchases <- ggplot(data, aes(y = CatalogPurchases)) +
  geom_boxplot(fill = "yellow", color = "black") +
  labs(title = "Catalog Purchases after Treating Outliers") +
  theme_minimal()
box_plot_Catalog_Purchases
#Catalog_Purchases have outliers

# Investigating Catalog_Purchases outliers causes
Q3_Catalog_Purchases <- quantile(data$CatalogPurchases, 0.75)
IQR_Catalog_Purchases <- IQR(data$CatalogPurchases)
upper_limit_Catalog_Purchases <- Q3_Catalog_Purchases + 1.5 * IQR_Catalog_Purchases

# Create a subset of outliers
outliers_subset_Catalog_Purchases <- data[data$CatalogPurchases > upper_limit_Catalog_Purchases, ]


count(outliers_subset_Catalog_Purchases)
#23

#we may focus on them to sell more fruits
outliers_subset_Catalog_Purchases %>%
  select(Education, Marital_Status, Income_Intervals, last_purchase, Wines, Fruits, Meat, Fish, Sweet, Gold, discount_purchase, Age_Intervals, Recency_Intervals, web_purchases, Catalog_Purchases)

Q1_Catalog_Purchases <- quantile(data$CatalogPurchases, 0.25)
lower_limit_Catalog_Purchases <- Q1_Catalog_Purchases - 1.5 * IQR_Catalog_Purchases

data$CatalogPurchases <- ifelse(data$CatalogPurchases < lower_limit_Catalog_Purchases, lower_limit_Catalog_Purchases, 
                             ifelse(data$CatalogPurchases > upper_limit_Catalog_Purchases, upper_limit_Catalog_Purchases, data$CatalogPurchases))
summary(data$CatalogPurchases)

#####

###
box_plot_Store_Purchases <- ggplot(data, aes(y = StorePurchases)) +
  geom_boxplot( color = "black") +
  labs(title = "Box Plot of Store Purchases") +
  theme_minimal()
box_plot_Store_Purchases
#Catalog_Purchases have no outliers
#####

###
box_plot_Web_Visits <- ggplot(data, aes(y = WebVisitsMonth)) +
  geom_boxplot(fill = "grey", color = "black") +
  labs(title = "Web Visits after Treating Outliers") +
  theme_minimal()
box_plot_Web_Visits
#Web_Visits have outliers

# Investigating Web_Visits outliers causes
Q3_Web_Visits <- quantile(data$WebVisitsMonth, 0.75)
IQR_Web_Visits <- IQR(data$WebVisitsMonth)
upper_limit_Web_Visits <- Q3_Web_Visits + 1.5 * IQR_Web_Visits

# Create a subset of outliers
outliers_subset_Web_Visits <- data[data$WebVisitsMonth > upper_limit_Web_Visits, ]


count(outliers_subset_Web_Visits)
#8

#we may focus on them to sell more fruits
outliers_subset_Web_Visits %>%
  select(Education, Marital_Status, Income_Intervals, last_purchase, Wines, Fruits, Meat, Fish, Sweet, Gold, discount_purchase, Age_Intervals, Recency_Intervals, web_purchases, Catalog_Purchases, Web_Visits)

Q1_Web_Visits <- quantile(data$WebVisitsMonth, 0.25)
lower_limit_Web_Visits <- Q1_Web_Visits - 1.5 * IQR_Web_Visits

data$WebVisitsMonth <- ifelse(data$WebVisitsMonth < lower_limit_Web_Visits, lower_limit_Web_Visits, 
                                 ifelse(data$WebVisitsMonth > upper_limit_Web_Visits, upper_limit_Web_Visits, data$WebVisitsMonth))
summary(data$WebVisitsMonth)
#####



campaign_acceptance <- data %>%
  select(starts_with("AcceptedCmp"))

campaign_acceptance_plot <- ggplot(melt(campaign_acceptance), aes(x = variable, fill = factor(Education))) +
  geom_bar(position = "stack", color = "white") +
  labs(title = "Campaign Acceptance Distribution", x = "Campaign", y = "Count") +
  theme_minimal()

print(campaign_acceptance_plot)




#pair plots
numeric_data <- subset(data, select = c(#"Age" , "Income"
                                        "Kidhome","Teenhome"
                                         #"Wines", "Fruits", "Meats", "Fish", "Sweets", "Gold"
                                        ))

# Create the pairs plot
pairs(numeric_data)

facet <- ggplot(data, aes(x = Age, y = Income)) +
  geom_point() +
  facet_grid(Education ~ Marital_Status)+
  labs(title = "Facet Grid across Categorical Levels") +
  theme_minimal()
facet

str(data)
# Perform standardization
scaled_data <- scale(numeric_data)


#scale_x_continuous(breaks = breaks, labels = scales::comma_format()) +
  
###Age Distribution by Education
age_education_distribution <- ggplot(data, aes(x = Education, y = age, fill = Education)) +
  geom_boxplot() +
  labs(title = "Age Distribution by Education", x = "Education Level", y = "Age") +
  theme_minimal()

###Campaign Response by Education:
campaign_response_education <- ggplot(data, aes(x = Education, fill = factor(Response))) +
  geom_bar(position = "stack", color = "white") +
  labs(title = "Campaign Response by Education", x = "Education Level", y = "Count", fill = "Response") +
  theme_minimal()
campaign_response_education
##it was surprising to know that people don't respond to campaign based on their income

### total purchases and compaints ###show CDF Instead
income_age_scatter <- ggplot(data, aes(x = total_purchases, y = Complain, color = Marital_Status)) +
  geom_point() +
  labs(title = "Total Purchases vs. Complains", x = "total purchases", y = "total_purchases", color = "Complain") +
  theme_minimal()

print(income_age_scatter)


income_age_scatter <- ggplot(data, aes(x = WebPurchases, y = WebVisitsMonth, color = Age_Intervals)) +
  geom_point() +
  labs(title = "Income vs. Age", x = "Age", y = "Income", color = "Marital Status") +
  theme_minimal()

print(income_age_scatter)



print(age_education_distribution)

###feature engineer for total campaign count and relationship with response

### Correlation Heatmap:
correlation_matrix <- cor(data[, sapply(data, is.numeric)], use = "complete.obs")

library(corrplot)
corrplot(correlation_matrix, method = "color", title = "Correlation Heatmap", tl.col = "black", tl.srt = 45)


###Box and Whiskers Plot: Age Distribution by Marital Status
age_marital_status_boxplot <- ggplot(data, aes(x = Marital_Status, y = Age, fill = Marital_Status)) +
geom_boxplot() +
  labs(title = "Age Distribution by Marital Status", x = "Marital Status", y = "Age") +
  theme_minimal()

print(age_marital_status_boxplot)

# Calculate total campaign count
data$total_campaign_count <- rowSums(data[, c("AcceptedCmp1", "AcceptedCmp2", "AcceptedCmp3", "AcceptedCmp4", "AcceptedCmp5")])

# Plot PDF for total campaign count and response variable
ggplot(data, aes(x = total_campaign_count)) +
  geom_density(aes(fill = factor(Response)), alpha = 0.5) +
  labs(title = "PDF for Total Campaign Count and Response", x = "Total Campaign Count", y = "Density") +
  theme_minimal()



###Density Plot: Distribution of Recency
recency_density <- ggplot(data, aes(x = LastPurchase , fill = factor(Response))) +
geom_density(alpha = 0.7) +
  labs(title = "Density Plot of Recency by Response", x = "Days Since Last Purchase", fill = "Response to Last Campaign") +
  theme_minimal()
recency_density

###Bubble Chart: Income vs. Age vs. Web Visits
income_age_web_bubble <- ggplot(data, aes(x = Complain, y = scaled(total_purchases) , size = Response, color = factor(Response))) +
  geom_point(alpha = 0.2) +
  labs(title = "Bubble Chart: Income vs. Age vs. Web Visits", x = "Age", y = "Income", size = "Web Visits", color = "Response") +
  theme_minimal()

income_age_web_bubble


# Selecting numerical variables
numerical_vars <- c("Income", "Kidhome", "Teenhome", "Recency", 
                    "MntWines", "MntFruits", "MntMeat", "MntFish", 
                    "MntSweet", "MntGold", "NumDealsPurchases", 
                    "NumWebPurchases", "NumCatalogPurchases", 
                    "NumStorePurchases", "NumWebVisitsMonth")
#boxen plot
ggplot(data, aes(x = variable, y = category)) +
  geom_boxen()

str(data)
print(recency_density)



### 
# Calculate ECDF for income
ecdf_income <- ecdf(data$Income)

# Calculate cumulative distribution for response categories
cdf_response_0 <- ecdf(data$Income[data$Response == 0])
cdf_response_1 <- ecdf(data$Income[data$Response == 1])

# Create a data frame for plotting
plot_data <- data.frame(
  income = seq(min(data$Income), max(data$Income), length.out = 1000),
  response_0 = cdf_response_0(seq(min(data$Income), max(data$Income), length.out = 1000)),
  response_1 = cdf_response_1(seq(min(data$Income), max(data$Income), length.out = 1000))
)

# Plot the CDFs
inc_resp <- ggplot(plot_data, aes(x = income)) +
  geom_line(aes(y = response_0), color = "blue", linetype = "solid") +
  geom_line(aes(y = response_1), color = "red", linetype = "dashed") +
  labs(title = "CDF Plot of Income by Response") +
  xlab("Income") +
  ylab("Cumulative Probability") +
  scale_color_manual(values = c("blue", "red"), labels = c("Response 0", "Response 1")) +
  theme_minimal()
inc_resp


# Example for CDF of buying a particular product given income - WIERD IMAGE
# Assuming 'data' contains the necessary columns: 'Income', 'Wines', 'Fruits', 'Meats', 'Fish', etc.
# Example for 'Wines' purchases
wines_cdf <- data %>%
  group_by(Income) %>%
  summarise(cumulative_wines = cumsum(Wines) / sum(Wines), .groups = "drop")


# Plot CDF for Wines purchases
ggplot(wines_cdf, aes(x = Income, y = cumulative_wines)) +
  geom_line() +
  labs(title = "CDF of Wines Purchases by Income", x = "Income", y = "Cumulative Probability") +
  theme_minimal()

# Example for probability distribution of recency given income
# Assuming 'data' contains the necessary columns: 'Income', 'Recency'
# Plot KDE of recency for each income group
last <- ggplot(data, aes (x = LastPurchase)) +
  geom_density(aes(fill = Income), alpha = 0.5) +
  labs(title = "Probability Distribution of Recency by Income", x = "Recency", y = "Density") +
  theme_minimal()
last

#Step 1: Subset data for married couples
married_couples_data <- data %>%
  filter(Marital_Status == "Married")

# Step 2: Create a heatmap to visualize correlation
# Subset the data to include only relevant columns
correlation_data <- married_couples_data %>%
  select(income_intervals, Wines, Fruits, Meats, Fish, Sweets, Gold)

# Calculate the correlation matrix
correlation_matrix <- cor(correlation_data)

# Create a heatmap
heatmap_data <- melt(correlation_matrix)

show <- ggplot(heatmap_data, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  labs(title = "Correlation Heatmap between Income and Products for Married Couples",
       x = "Products", y = "Products", fill = "Correlation") +
  scale_fill_gradient(low = "blue", high = "red") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
show



# Subset data for married couples
married_data <- data[data$Marital_Status == "Married", ]

# Select the relevant columns for correlation analysis
selected_columns <- c("Wines", "Fruits", "Meats", "Fish", "Sweets", "Gold", "income_intervals")
str(data)
# Calculate the correlation matrix
correlation_matrix <- cor(married_data[selected_columns])

# Create a heatmap


heatmap <- ggplot(data = melt(correlation_matrix), aes(Var2, Var1, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(title = "Correlation Heatmap of Income Intervals and Products for Married Couples",
       x = "Products", y = "Income Intervals") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
heatmap


# Subset data for married couples
married_data <- data[data$Marital_Status == "Married", ]
selected_income_intervals <- c("Medium", "High", "Very High")
married_data_filtered <- married_data[married_data$Income_Intervals %in% selected_income_intervals, ]

# Select the relevant columns for correlation analysis (excluding "Income_Intervals")
selected_columns <- c("Wines", "Fruits", "Meat", "Fish", "Sweet", "Gold")

# Calculate the correlation matrix
correlation_matrix <- cor(married_data_filtered[selected_columns])

# Create a heatmap


h2 <- ggplot(data = melt(correlation_matrix), aes(Var2, Var1, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(title = "Correlation Heatmap of Married Couples and Products",
       x = "Products", y = "Income Intervals") +  # Corrected y-axis label
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

h2




selected_columns <- c("Wines", "Fruits", "Meat", "Fish", "Sweet", "Gold")
income_intervals <- unique(married_data$income_intervals)

correlation_matrix <- matrix(NA, ncol = length(selected_columns), nrow = length(income_intervals))

# Calculate the correlation for each combination of income interval and product
for (i in 1:length(income_intervals)) {
  for (j in 1:length(selected_columns)) {
    x <- married_data_filtered[married_data_filtered$income_intervals == income_intervals[i], selected_columns[j]]
    y <- married_data_filtered[married_data_filtered$income_intervals == income_intervals[i], "Income"]
    correlation_matrix[i, j] <- cor(x, y)
  }
}

# Create a heatmap
h2 <- ggplot(data = melt(correlation_matrix), aes(Var2, Var1, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(title = "Correlation Heatmap of Married Couples' Income Intervals and Products",
       x = "Products", y = "Income Intervals") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

h2


agg_data <- data %>%
  group_by(Marital_Status) %>%
  summarise_all(sum)

# Create the scatter plot
scatter_plot <- ggplot(agg_data, aes(x = Income, y = Wines)) +
  geom_point(aes(color = Marital_Status, size = Fruits)) +
  labs(title = "Income vs. Wines Purchased by Marital Status",
       x = "Income", y = "Wines Purchased") +
  scale_color_manual(values = c("Single" = "blue", "Married" = "red")) +
  theme_minimal()

scatter_plot

married_data <- data[data$Marital_Status == "Married", ]

# Select relevant columns
selected_columns <- c("Income", "Wines", "Fruits", "Meat", "Fish", "Sweet", "Gold")

# Create a scatter plot matrix
scatter_matrix <- ggplot(married_data, aes(x = Income)) +
  geom_point(aes(y = .data[[col]], color = Marital_Status)) +
  labs(title = "Scatter Plot of Income vs. Products for Married Individuals",
       x = "Income") +
  facet_wrap(~col, scales = "free_y", ncol = 2) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

scatter_matrix
