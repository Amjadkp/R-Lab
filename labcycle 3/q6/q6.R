library(MASS)    #  Boston dataset
library(ggplot2) 
library(caret)  
library(dplyr)  
library(corrplot)
library(Metrics)

data("Boston")
head(Boston)

str(Boston)
summary(Boston)

# check missing values
any(is.na(Boston))

# calc and print the correlation matrix
cor_matrix <- cor(Boston)
print(cor_matrix)

# Plot the correlation heatmap
corrplot(cor_matrix, method = "color", type = "upper", tl.col = "black", tl.cex = 0.8)

# Histogram of medv
ggplot(Boston, aes(x = medv)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Median Value of Owner-Occupied Homes", x = "Median Value (medv)", y = "Frequency")

# Scatter plot of 'medv' vs. 'lstat' 
ggplot(Boston, aes(x = lstat, y = medv)) +
  geom_point(color = "blue") +
  labs(title = "Median Value vs. Lower Status Population", x = "Lower Status (lstat)", y = "Median Value (medv)")

# Scatter plot of 'medv' vs. 'rm' (average number of rooms)
ggplot(Boston, aes(x = rm, y = medv)) +
  geom_point(color = "red") +
  labs(title = "Median Value vs. Average Number of Rooms", x = "Average Number of Rooms (rm)", y = "Median Value (medv)")

# Split the data training (70%) and test (30%) sets
set.seed(123)
trainIndex <- createDataPartition(Boston$medv, p = 0.7, list = FALSE)
trainData <- Boston[trainIndex, ]
testData <- Boston[-trainIndex, ]

# linear regression model and view the summary
lm_model <- lm(medv ~ ., data = trainData)
summary(lm_model)

# Predict on the test set
predictions <- predict(lm_model, newdata = testData)

#  performance metrics: RMSE and R-squared
rmse_val <- rmse(testData$medv, predictions)
cat("RMSE:", rmse_val, "\n")

r_squared <- summary(lm_model)$r.squared
cat("R-squared:", r_squared, "\n")