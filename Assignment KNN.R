install.packages("dplyr")
install.packages("ggplot2")
install.packages("class")
install.packages("glmnet")


library(dplyr)
library(ggplot2)

ziptrain <- read.csv("zip.train.csv", header = FALSE)
ziptrain27 <- subset(ziptrain, ziptrain[,1]==2 | ziptrain[,1]==7);

# Rename columns for clarity
colnames(ziptrain27)[1] <- "Label"
colnames(ziptrain27)[-1] <- paste0("Feature", 1:(ncol(ziptrain27)-1))


# Summary information
summary_info <- summary(ziptrain27)


# Visualize class distribution
ggplot(ziptrain27, aes(x = factor(Label))) +
  geom_bar() +
  labs(x = "Digit", y = "Count", title = "Class Distribution")

# Visualize feature distributions
feature_plots <- lapply(2:ncol(ziptrain27), function(i) {
  ggplot(ziptrain27, aes(x = ziptrain27[[i]], fill = factor(Label))) +
    geom_density(alpha = 0.5) +
    labs(x = colnames(ziptrain27)[i], y = "Density") +
    theme_minimal()
})

# Display digit images for rows with digits 2 and 7
rowindices_2 <- which(ziptrain27[, 1] == 2)[1:3]
rowindices_7 <- which(ziptrain27[, 1] == 7)[1:3]
rowindices <- c(rowindices_2, rowindices_7)

par(mfrow = c(2, length(rowindices) / 2))

for (i in seq_along(rowindices)) {
  rowindex <- rowindices[i]
  Xval <- t(matrix(data.matrix(ziptrain27[rowindex, -1]), byrow = TRUE, 16, 16)[16:1,])
  image(Xval, col = gray(0:1), axes = FALSE, main = paste("Digit:", ziptrain27[rowindex, 1]))
}

par(mfrow = c(1, 1))  # Reset the layout

# Load necessary libraries
library(glmnet)
library(class)

# Read the training data
ziptrain <- read.csv("zip.train.csv", header = FALSE)

# Subset the data for only 2's and 7's
ziptrain27 <- subset(ziptrain, ziptrain[, 1] == 2 | ziptrain[, 1] == 7)

# Fit linear regression model
mod1 <- lm(V1 ~ ., data = ziptrain27)
pred1.train <- predict.lm(mod1, ziptrain27[, -1])
y1pred.train <- ifelse(pred1.train >= 4.5, 7, 2)

# Calculate training error for linear regression
linear_training_error <- mean(y1pred.train != ziptrain27[, 1])
print(linear_training_error)

library(class)

k_values <- c(1, 3, 5, 7, 9, 11, 13, 15)
training_errors_knn <- numeric(length(k_values))

for (i in seq_along(k_values)) {
  k <- k_values[i]
  ypred2.train <- knn(ziptrain27[, -1], xnew, ziptrain27[, 1], k = k)
  training_errors_knn[i] <- mean(ypred2.train != ziptrain27[, 1])
  cat("KNN Training Error (k =", k, "):", training_errors_knn[i], "\n")
}


# Load necessary libraries
library(glmnet)
library(class)

# Read the testing dataset
ziptest <- read.csv("zip.test.csv", header = FALSE)
ziptest27 <- subset(ziptest, ziptest[, 1] == 2 | ziptest[, 1] == 7)

# Convert testing data to matrix
test_data_matrix <- as.matrix(ziptest27[, -1])

# Convert labels to binary (2 and 7) for testing data
binary_labels_test <- ifelse(ziptest27$V1 == 2, 0, 1)

# Load the trained logistic regression model
load("logistic_regression_model.RData")

# Predict using the trained logistic regression model
linear_predictions_test <- predict(fit, newx = test_data_matrix, s = 0.01, type = "response")

# Convert predicted probabilities to predicted classes
linear_predicted_classes_test <- ifelse(linear_predictions_test > 0.5, 1, 0)

# Calculate testing error for logistic regression
linear_testing_error <- mean(linear_predicted_classes_test != binary_labels_test)

# Print the testing error
print(paste("Logistic Regression Testing Error:", linear_testing_error))



library(class)

# Initialize k values
k_values <- c(1, 3, 5, 7, 9, 11, 13, 15)

# Initialize vector to store testing errors
knn_testing_errors <- numeric(length(k_values))

# Subset labels and data
train_labels <- ziptrain27[, 1]  # Assuming Label is the first column in ziptrain27
test_labels <- ziptest27[, 1]    # Assuming V1 is the first column in ziptest27
train_data <- ziptrain27[, -1]    # Remove the first column from ziptrain27
test_data <- ziptest27[, -1]      # Remove the first column from ziptest27

# Loop through different k values
for (i in seq_along(k_values)) {
  k <- k_values[i]
  
  # Predict using KNN on testing data
  knn_predicted_classes_test <- knn(train_data, test_data, train_labels, k = k)
  
  # Calculate testing error for KNN
  knn_testing_errors[i] <- mean(knn_predicted_classes_test != test_labels)
  
  # Print the KNN testing error
  print(paste("KNN Testing Error (k =", k, "):", knn_testing_errors[i]))
}



# Load necessary libraries
library(class)

# Combine training and testing data
zip27full <- rbind(ziptrain27, ziptest27)

# Initialize parameters
n1 <- 1376
n2 <- 345
n <- dim(zip27full)[1]
B <- 100
k_values <- c(1, 3, 5, 7, 9, 11, 13, 15)

# Initialize matrix to store testing error values
TEALL <- matrix(0, nrow = B, ncol = length(k_values) + 1)

# Set random seed
set.seed(7406)

# Loop through each iteration
for (b in 1:B) {
  # Randomly select n1 observations as training subset
  flag <- sort(sample(1:n, n1))
  zip27traintemp <- zip27full[flag,]
  zip27testtemp <- zip27full[-flag,]
  
  # Initialize vector to store testing error values for this iteration
  TE <- numeric(length(k_values) + 1)
  
  # Linear Regression
  mod1 <- lm(V1 ~ ., data = zip27traintemp)
  pred1.test <- predict.lm(mod1, newdata = zip27testtemp[, -1])
  y1pred.test <- ifelse(pred1.test >= 4.5, 2, 7)
  TE[1] <- mean(y1pred.test != zip27testtemp[, 1])
  
  # KNN
  for (i in seq_along(k_values)) {
    k <- k_values[i]
    ypred2.test <- knn(zip27traintemp[, -1], zip27testtemp[, -1], zip27traintemp$V1, k = k)
    TE[i + 1] <- mean(ypred2.test != zip27testtemp$V1)
  }
  
  # Store testing error values for this iteration
  TEALL[b, ] <- TE
}

# Compute mean and variance of testing errors
mean_testing_errors <- apply(TEALL, 2, mean)
var_testing_errors <- apply(TEALL, 2, var)

# Print mean and variance of testing errors
for (i in seq_along(mean_testing_errors)) {
  print(paste("Mean Testing Error for Model", i, ":", mean_testing_errors[i]))
  print(paste("Variance of Testing Error for Model", i, ":", var_testing_errors[i]))
}







