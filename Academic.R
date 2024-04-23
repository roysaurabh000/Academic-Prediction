# Load the necessary library
library(readr)
library(dplyr)

# Read the CSV file
data <- read_csv("Expanded_data_with_more_features.csv")  # Replace "your_file_path.csv" with the actual path to your CSV file

# Check the structure of the dataset
str(data)

# Check the first few rows of the dataset
head(data)

# Check for missing values
summary(data)

data <- data %>%
  mutate(
    Gender = as.factor(Gender),
    EthnicGroup = as.factor(EthnicGroup),
    ParentEduc = as.factor(ParentEduc),
    LunchType = as.factor(LunchType),
    TestPrep = as.factor(TestPrep),
    ParentMaritalStatus = as.factor(ParentMaritalStatus),
    PracticeSport = as.factor(PracticeSport),
    IsFirstChild = as.factor(IsFirstChild),
    NrSiblings = as.numeric(NrSiblings),  # Change to numeric if appropriate
    TransportMeans = as.factor(TransportMeans),
   # WklyStudyHours = as.numeric(WklyStudyHours),  # Change to numeric if appropriate
    MathScore = as.numeric(MathScore),  # Change to numeric if appropriate
    ReadingScore = as.numeric(ReadingScore),  # Change to numeric if appropriate
    WritingScore = as.numeric(WritingScore)  # Change to numeric if appropriate
  )

# Check the structure of the dataset after conversion
str(data)



# Convert "< 5" to 2.5 (assuming the midpoint of the range)
data$WklyStudyHours[data$WklyStudyHours == "< 5"] <- 2.5

# Convert "10-May" to 7.5 (assuming the midpoint of the range)
data$WklyStudyHours[data$WklyStudyHours == "10-May"] <- 7.5

# Convert "> 10" to 12.5 (assuming the midpoint of the range)
data$WklyStudyHours[data$WklyStudyHours == "> 10"] <- 12.5

# Convert the column to numeric
data$WklyStudyHours <- as.numeric(data$WklyStudyHours)

summary(data)



# Deal with missing values (if any)
# For example, you can impute missing values with the mean or median for numeric variables
# data$Your_Variable[is.na(data$Your_Variable)] <- mean(data$Your_Variable, na.rm = TRUE)

# Deal with missing values (if any)
# For example, you can impute missing values with the mean or median for numeric variables
numeric_vars <- sapply(data, is.numeric)
numeric_missing <- colnames(data)[numeric_vars][apply(data[, numeric_vars], 2, anyNA)]

for (var in numeric_missing) {
  if (any(is.na(data[[var]]))) {
    data[[var]][is.na(data[[var]])] <- mean(data[[var]], na.rm = TRUE)  # You can replace mean with median if you prefer
  }
}

# Deal with missing values (if any)
# For example, you can impute missing values with the mean or median for numeric variables
# And with the mode (most frequent value) for categorical variables

# Identify categorical variables
categorical_vars <- sapply(data, is.factor)
categorical_missing <- colnames(data)[categorical_vars][apply(data[, categorical_vars], 2, anyNA)]

for (var in categorical_missing) {
  if (any(is.na(data[[var]]))) {
    mode_val <- names(sort(table(data[[var]], useNA = "ifany"), decreasing = TRUE))[1]  # Calculate mode
    data[[var]][is.na(data[[var]])] <- mode_val
  }
}

# Check if all missing values are handled
summary(data)



# Load required libraries
library(ggplot2)
library(corrplot)
################

# Univariate Analysis
for (col in names(data)) {
  if (is.numeric(data[[col]])) {
    # Numerical Summaries
    cat("\nNumerical Summary for", col, "\n")
    print(summary(data[[col]]))
    
    # Histogram
    hist_plot <- ggplot(data, aes_string(x = col)) +
      geom_histogram(bins = 30, fill = "steelblue", color = "black") +
      labs(title = paste("Histogram for", col),
           x = col, y = "Count")
    print(hist_plot)
    
    # Box Plot
    box_plot <- ggplot(data, aes_string(y = col)) +
      geom_boxplot(fill = "steelblue", color = "black") +
      labs(title = paste("Box Plot for", col),
           x = "", y = col)
    print(box_plot)
  } else {
    # Bar Plot for Categorical Variables
    cat("\nBar Plot for", col, "\n")
    bar_plot <- ggplot(data, aes_string(x = col)) +
      geom_bar(fill = "steelblue", color = "black") +
      labs(title = paste("Bar Plot for", col),
           x = col, y = "Count")
    print(bar_plot)
  }
  cat("\n---\n")
}

# Bivariate Analysis
numeric_cols <- names(data)[sapply(data, is.numeric)]

# Correlation Matrix
cor_matrix <- cor(data[, numeric_cols], use = "pairwise.complete.obs")
corrplot(cor_matrix, method = "circle", type = "lower", tl.col = "black")

# Scatter Plot Matrix
pairs(data[, numeric_cols], pch = 19, cex = 0.5)

# Visualize relationships between categorical and numeric variables
for (cat_col in names(data)[sapply(data, is.factor)]) {
  for (num_col in numeric_cols) {
    cat_plot <- ggplot(data, aes_string(x = cat_col, y = num_col)) +
      geom_boxplot(fill = "steelblue", color = "black") +
      labs(title = paste("Relationship between", cat_col, "and", num_col),
           x = cat_col, y = num_col)
    print(cat_plot)
  }
}


# Bivariate Analysis
numeric_cols <- names(data)[sapply(data, is.numeric)]

# Correlation Matrix
par(mfrow = c(1, 1))  # Reset to a single plot layout
cor_matrix <- cor(data[, numeric_cols], use = "pairwise.complete.obs")
corrplot(cor_matrix, method = "circle", type = "lower", tl.col = "black")

# Scatter Plot Matrix
pairs(data[, numeric_cols], pch = 19, cex = 0.5)

# Visualize relationships between categorical and numeric variables
par(mfrow = c(2, 3))  # Set a 2x3 grid layout

count <- 1
for (cat_col in names(data)[sapply(data, is.factor)]) {
  for (num_col in numeric_cols) {
    cat_plot <- ggplot(data, aes_string(x = cat_col, y = num_col)) +
      geom_boxplot(fill = "steelblue", color = "black") +
      labs(title = paste("Relationship between", cat_col, "and", num_col),
           x = cat_col, y = num_col)
    print(cat_plot, split = count)
    count <- count + 1
  }
}


# Arrange and print numeric plots
numeric_grid <- do.call(grid.arrange, c(numeric_plots, ncol = 2))
print(numeric_grid)

# Arrange and print categorical plots
categorical_grid <- do.call(grid.arrange, c(categorical_plots, ncol = 3))
print(categorical_grid)


ggplot(data, aes(x = MathScore, y = ReadingScore)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Relationship between Math and Reading Scores",
       x = "Math Score", y = "Reading Score")
ggplot(data, aes(x = Gender, y = MathScore, fill = Gender)) +
  geom_boxplot() +
  labs(title = "Math Scores by Gender",
       x = "Gender", y = "Math Score")


# Load necessary libraries
library(dplyr)
library(tidyr)

###################

# Define categorical variables
categorical_vars <- c("Gender", "EthnicGroup", "ParentEduc", "LunchType", "TestPrep", "ParentMaritalStatus", "PracticeSport", "IsFirstChild", "TransportMeans")

# Perform one-hot encoding for each categorical variable
for (var in categorical_vars) {
  if (var %in% names(data)) {
    if (is.factor(data[[var]]) || is.character(data[[var]])) {
      # Create dummy variables using one-hot encoding
      dummy_vars <- model.matrix(~ . - 1, data = data.frame(x = data[[var]]))
      # Give meaningful column names
      colnames(dummy_vars) <- paste(var, colnames(dummy_vars), sep = "_")
      # Add dummy variables to the original dataset
      data <- cbind(data, dummy_vars)
    } else {
      warning(paste("Variable", var, "is not categorical. Skipping one-hot encoding."))
    }
  } else {
    warning(paste("Variable", var, "not found in the dataset. Skipping."))
  }
}

# Remove the original categorical variables from the dataset
data <- data[, !names(data) %in% categorical_vars]

# Print the updated dataset
print(data)

numeric_vars <- c("NrSiblings", "WklyStudyHours", "MathScore", "ReadingScore", "WritingScore")
# Normalize numerical variables
for (var in numeric_vars) {
  data[[var]] <- (data[[var]] - min(data[[var]])) / (max(data[[var]]) - min(data[[var]]))
}

# Print the updated dataset
print(data)


#Regression Code (MATHSCORE as Target)

library(caret)
set.seed(123)  # Set seed for reproducibility
train_index <- createDataPartition(data$MathScore, p = 0.7, list = FALSE)
train_data <- data[train_index, ]
test_data <- data[-train_index, ]
results_regression <- list()

ctrl <- trainControl(method = "cv",  # Cross-validation method
                     number = 5,      # Number of folds
                     verboseIter = TRUE)  # Print progress during training


# Train and evaluate Linear Regression model
model_fit_lm <- train(MathScore ~ ., 
                      data = train_data, 
                      method = "lm", 
                      trControl = ctrl)

# Make predictions on the test set
predictions_lm <- predict(model_fit_lm, newdata = test_data)

# Evaluate the model
rmse_lm <- sqrt(mean((test_data$MathScore - predictions_lm)^2))

# Store the results
results_regression$MathScore_Linear_Regression <- list(rmse = rmse_lm, model = model_fit_lm)


# Train and evaluate Random Forest model
model_fit_rf <- train(MathScore ~ ., 
                      data = train_data, 
                      method = "rf", 
                      trControl = ctrl)

# Make predictions on the test set
predictions_rf <- predict(model_fit_rf, newdata = test_data)

# Evaluate the model
rmse_rf <- sqrt(mean((test_data$MathScore - predictions_rf)^2))

# Store the results
results_regression$MathScore_Random_Forest <- list(rmse = rmse_rf, model = model_fit_rf)


# Train and evaluate Gradient Boosting Machine model
model_fit_gbm <- train(MathScore ~ ., 
                       data = train_data, 
                       method = "gbm", 
                       trControl = ctrl)

# Make predictions on the test set
predictions_gbm <- predict(model_fit_gbm, newdata = test_data)

# Evaluate the model
rmse_gbm <- sqrt(mean((test_data$MathScore - predictions_gbm)^2))

# Store the results
results_regression$MathScore_Gradient_Boosting_Machine <- list(rmse = rmse_gbm, model = model_fit_gbm)



results_regression


#CLASSIFICATION ( we are creating new Feature as GradeStatus, which is our new Target)

print(data)
# Assuming your dataset is named 'data'
data$GradeStatus <- ifelse(data$MathScore > 0.42 & data$ReadingScore > 0.42 & data$WritingScore > 0.42, "Pass", "Fail")
head(data)


library(glmnet)
library(caret)

set.seed(123)

# Create an index for splitting the data into train and test
index <- createDataPartition(data$GradeStatus, p = 0.8, list = FALSE)

# Create training and test sets
train_data <- data[index, ]
test_data <- data[-index, ]

# Define control parameters for cross-validation
ctrl <- trainControl(method = "cv",    # 10-fold cross-validation
                     number = 10,      # Number of folds
                     classProbs = TRUE,  # For class probability estimation
                     summaryFunction = twoClassSummary,  # For binary classification
                     search = "grid")  # Use grid search for hyperparameter tuning

# Define tuning grid
alpha_values <- seq(0, 1, by = 0.1)  # Range of alpha values (0 to 1)
lambda_values <- 10^seq(-3, 3, by = 0.5)  # Range of lambda values (10^-3 to 10^3)
tuneGrid <- expand.grid(.alpha = alpha_values, .lambda = lambda_values)

# Train logistic regression model using glmnet
model_fit_glmnet <- train(GradeStatus ~ .,
                          data = data,
                          method = "glmnet",
                          trControl = ctrl,
                          tuneGrid = tuneGrid)

# Print the model
print(model_fit_glmnet)

# Define control parameters for cross-validation
ctrl <- trainControl(method = "cv",    # 10-fold cross-validation
                     number = 10,      # Number of folds
                     classProbs = TRUE,  # For class probability estimation
                     summaryFunction = twoClassSummary)  # For binary classification

# Define tuning grid
k_vals <- c(3, 5, 7, 9)
tuneGrid <- expand.grid(.k = k_vals)

# Train KNN model
model_fit_knn <- train(GradeStatus ~ .,
                       data = data,
                       method = "knn",
                       trControl = ctrl,
                       tuneGrid = tuneGrid)

# Print the model
print(model_fit_knn)


# Define control parameters for cross-validation
ctrl <- trainControl(method = "cv",    # 10-fold cross-validation
                     number = 10,      # Number of folds
                     classProbs = TRUE,  # For class probability estimation
                     summaryFunction = twoClassSummary,  # For binary classification
                     search = "grid")  # Use grid search for hyperparameter tuning

# Train LDA model
model_fit_lda <- train(GradeStatus ~ .,
                       data = data,
                       method = "lda",
                       trControl = ctrl)

# Print the model
print(model_fit_lda)



logistic_pred <- predict(model_fit_glmnet, newdata = test_data)
logistic_accuracy <- mean(logistic_pred == test_data$GradeStatus)
cat("Logistic Regression Accuracy:", logistic_accuracy, "\n")


# Evaluate KNN model
knn_pred <- predict(model_fit_knn, newdata = test_data)
knn_accuracy <- mean(knn_pred == test_data$GradeStatus)
cat("KNN Accuracy:", knn_accuracy, "\n")

# Evaluate LDA model
lda_pred <- predict(model_fit_lda, newdata = test_data)
lda_accuracy <- mean(lda_pred == test_data$GradeStatus)
cat("LDA Accuracy:", lda_accuracy, "\n")


library(caret)

# Get confusion matrix for logistic regression model
logistic_confusion <- confusionMatrix(logistic_pred, test_data$GradeStatus)
print("Confusion Matrix for Logistic Regression:")
print(logistic_confusion)

# Calculate precision and recall for logistic regression
logistic_precision <- logistic_confusion$byClass["Precision"]
logistic_recall <- logistic_confusion$byClass["Recall"]
cat("Precision for Logistic Regression:", logistic_precision, "\n")
cat("Recall for Logistic Regression:", logistic_recall, "\n")

# Get confusion matrix for KNN model
knn_confusion <- confusionMatrix(knn_pred, test_data$GradeStatus)
print("Confusion Matrix for KNN:")
print(knn_confusion)

# Calculate precision and recall for KNN
knn_precision <- knn_confusion$byClass["Precision"]
knn_recall <- knn_confusion$byClass["Recall"]
cat("Precision for KNN:", knn_precision, "\n")
cat("Recall for KNN:", knn_recall, "\n")

# Get confusion matrix for LDA model
lda_confusion <- confusionMatrix(lda_pred, test_data$GradeStatus)
print("Confusion Matrix for LDA:")
print(lda_confusion)

# Calculate precision and recall for LDA
lda_precision <- lda_confusion$byClass["Precision"]
lda_recall <- lda_confusion$byClass["Recall"]
cat("Precision for LDA:", lda_precision, "\n")
cat("Recall for LDA:", lda_recall, "\n")

test_data$GradeStatus <- factor(test_data$GradeStatus, levels = c("Fail", "Pass"))  


# Check levels of predicted and actual class labels
levels(logistic_pred)
levels(test_data$GradeStatus)

# Make sure levels are consistent
levels(logistic_pred) <- levels(test_data$GradeStatus)

# Now calculate confusion matrix, precision, and recall
logistic_confusion <- confusionMatrix(logistic_pred, test_data$GradeStatus)
logistic_precision <- logistic_confusion$byClass["Precision"]
logistic_recall <- logistic_confusion$byClass["Recall"]

# Print confusion matrix, precision, and recall
print("Confusion Matrix for Logistic Regression:")
print(logistic_confusion)
cat("Precision for Logistic Regression:", logistic_precision, "\n")
cat("Recall for Logistic Regression:", logistic_recall, "\n")


