library(dplyr)
library(caret)
library(glmnet)
library(ggplot2)


# Selection
data <- fifa_ordered3 %>%
  dplyr::select(Tackle, Passing, Speed, Technique, Shooting, Equilibrium, BallRecovery, Reflexes, Position)

# Position as a factor
data$Position <- as.factor(data$Position)


# Create the predictor matrix and response vector
# -1 to not include the intercept
x <- model.matrix(Position ~ . - 1, data = data)  
y <- data$Position

# Data was split using a ratio of 80% for the training set and 20% for the test set, maintaining reproducibility with set.seed(123)
set.seed(123)  
trainIndex <- createDataPartition(y, p = .8, list = FALSE)
x_train <- x[trainIndex, ]
y_train <- y[trainIndex]
x_test <- x[-trainIndex, ]
y_test <- y[-trainIndex]


# Finding the best lambda and alpha parameters with cross-validation
cv_model <- cv.glmnet(x_train, y_train, family = "multinomial", type.measure = "class", alpha = 0.5)

# Train the Elastic Net model
final_model <- glmnet(x_train, y_train, family = "multinomial", lambda = cv_model$lambda.min, alpha = 0.5)


# Predictions on the test set
predicted_roles <- predict(final_model, s = cv_model$lambda.min, newx = x_test, type = "class")

# Accuracy calculation
accuracy <- sum(predicted_roles == y_test) / length(y_test)
print(paste("Accuracy:", accuracy))

# Confusion matrix
confusionMatrix <- confusionMatrix(as.factor(predicted_roles), as.factor(y_test))
print(confusionMatrix)












