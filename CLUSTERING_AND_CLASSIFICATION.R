
# Load the necessary packages for the analysis ----------------------------

library(tidyverse)
library(corrplot)
library(FactoMineR)
library(factoextra)
library(MASS)
library(tidymodels)
library(nnet)
library(parsnip)
library(recipes)
library(workflows)
library(yardstick)
library(rpart)
library(randomForest)
library(caTools) 
library(class)
library(caret)
library(glmnet)

# 1) Load and clean the data ----------------------------------------------
# p.s. This path only works if the dataset is in the folder of the R project

fifa <- read.csv("FIFA.csv")

# This dataset is very dense in variables. We wish to remove some that we
# know we are not interested in using:

fifa <- fifa %>% dplyr::select(!c(Wage, Special, International.Reputation,
                           Contract.Valid.Until, Height, Weight,
                           GKDiving, GKHandling, GKKicking, GKPositioning,
                           GKReflexes, Release.Clause))
# We put dplyr::select() because there was a conflict with the MASS package

# We removed GK variables because we will not consider goalkeepers in our analysis

# We can check for NA's

fifa %>% summarise(count = sum(is.na(fifa)))
### There are many NA's, let's drop them

fifa <- drop_na(fifa) 
# We can safely drop them, they won't influence the analysis


# 2) Visualise the data ---------------------------------------------------

# I would like to check the distribution of overalls depending on the positions
# First, let's order the positions from defensive to offensive

# We have to create a vector with the desired order

order <- c("GK", "CB", "RCB", "LCB", "RB", "LB", "LWB",
           "RWB", "CDM", "RDM", "LDM", "RCM", "LCM",
           "CM", "RM", "LM", "CAM", "RAM", "LAM", "RW", 
           "LW", "RF", "LF", "CF", "ST", "RS", "LS")

# Then we match it with our Position column in the data frame

fifa_ordered <- fifa %>% mutate(
  Position =  factor(Position, levels = order)) %>%
  arrange(Position) 

fifa_ordered <- fifa_ordered[-c(1:1989),] 
# We dropped all rows dedicated to goalkeepers

# Now that they are ordered, let's check if offensive and defensive positions
# have different average overalls

fifa_ordered %>% group_by(Position) %>% 
  summarise(Overall = mean(Overall)) %>% 
  ggplot(mapping = aes(Overall, Position)) +
  geom_point()

# It does indeed seem like there are higher average overalls for
# offensive positions rather than defensive

# Let's add a new column where we group all of the positions in:
# "Defender", "Midfielder", "Striker"

fifa_ordered2 <- fifa_ordered %>% mutate(Position = case_when(
  Position %in% c("CB", "RCB", "LCB", "RB", "LB", "LWB", "RWB") ~ "Defender",
  Position %in% c("CDM", "RDM", "LDM", "RCM", "LCM", "CM", "RM", "LM", "CAM", "RAM", "LAM") ~ "Midfielder",
  Position %in% c("RW", "LW", "RF", "LF", "CF", "ST", "RS", "LS") ~ "Striker"
))

# Now we can check the distribution of overalls for each role

ggplot(fifa_ordered2, aes(x = Position, y = Overall, fill = Position)) +
  geom_boxplot() +
  labs(title = "Overall distribution per position",
       x = "Position",
       y = "Overall") +
  scale_fill_brewer(palette = "Set1") +  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

# All roles have similar average overalls, there are more differences in
# outliers

# 3) Correlations and model building --------------------------------------

cor_matrix <- cor(fifa_ordered2[, c("Overall", "Crossing", "Finishing", "HeadingAccuracy", "ShortPassing", "Volleys", 
                                    "Dribbling", "Curve", "FKAccuracy", "LongPassing", "BallControl", "Acceleration", 
                                    "SprintSpeed", "Agility", "Reactions", "Balance", "ShotPower", "Jumping", "Stamina", 
                                    "Strength", "LongShots", "Aggression", "Interceptions", "Positioning", "Vision", 
                                    "Penalties", "Composure", "Marking", "StandingTackle", "SlidingTackle")], 
                  use = "complete.obs")

corrplot(cor_matrix, method = "circle")

# Some variables have extremely high correlations, and/or are basically the same.
# Since there are many variables, we can reduce the number of them by combining
# two or more variables together (for instance: Sliding and standing tackle).
# We will do that by averaging their values

fifa_ordered3 <- fifa_ordered2 %>% 
  mutate(Tackle = (SlidingTackle + StandingTackle)/2) %>%
  mutate(Passing = (LongPassing + ShortPassing + Crossing + Vision + Curve +
                      FKAccuracy)/6) %>%
  mutate(Speed = (Acceleration + SprintSpeed)/2) %>%
  mutate(Technique = (BallControl + Dribbling)/2) %>%
  mutate(Shooting = (Finishing + Volleys + ShotPower + LongShots + Positioning)/5) %>%
  mutate(Equilibrium = (Agility + Balance)/2) %>%
  mutate(BallRecovery = (Aggression + Interceptions + Marking)/3) %>%
  mutate(Reflexes = (Reactions + Composure)/2) %>%
  dplyr::select(!c(SlidingTackle, StandingTackle, LongPassing, ShortPassing, 
            Crossing, Vision, Curve, Acceleration , SprintSpeed,
            BallControl, FKAccuracy, Positioning, Reactions, Composure,
            Dribbling, Finishing, Volleys, ShotPower, LongShots, Agility,
            Balance, Aggression, Interceptions, Marking))
# In the end we also erased the variables we combined from the dataset. 


# 4) Clustering -----------------------------------------------------------

# 4.1) Perform a PCA ------------------------------------------------------

pca_results <- PCA(fifa_ordered3[, c("Tackle", "Passing", "Speed", 
                                     "Technique", "Shooting", "Equilibrium",
                                     "BallRecovery", "Reflexes",
                                     "HeadingAccuracy", "Jumping", "Stamina",
                                     "Strength", "Penalties")],
                   graph = FALSE)

# Let's visualise the variables graph

fviz_pca_var(pca_results, col.var = "contrib", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)  


fviz_pca_ind(pca_results,
             geom.ind = "point", 
             col.ind = fifa_ordered3$Position, 
             palette = c("blue", "red", "green"), 
             addEllipses = TRUE, 
             ellipse.level = 0.95, 
             pointshape = 21, 
             pointsize = 2, 
             fill.ind = fifa_ordered3$Position, 
             legend.title = "Role",
             axes.labs = c("PC1", "PC2")) +
  theme_minimal() +
  theme(legend.position = "right", 
        axis.text = element_text(size = 12), 
        axis.title = element_text(size = 14), 
        legend.text = element_text(size = 12)) 

# We can tell two clusters mainly: Defenders are mostly separate
# from the rest, midfielders and strikers are more mixed.
# However, strikers and defenders are clearly separate, so we consider
# 3 clusters

# 4.2) LDA Analysis -------------------------------------------------------

# Prepare the formula for LDA

formula_lda <- Position ~ Tackle + Passing + Speed + Technique + 
                          Shooting + Equilibrium + BallRecovery + 
                          Reflexes + HeadingAccuracy + Jumping + 
                          Stamina + Strength + Penalties

# LDA

lda_model <- lda(formula_lda, data = fifa_ordered3)
print(summary(lda_model))

# Show which variables have the highest contributions

print(lda_model$scaling)

# Let's visualise the results

plot(lda_model, dimen = 1, col = as.numeric(fifa_ordered3$Position))


# Put the Position variable as factor
fifa_ordered3$Position <- as.factor(fifa_ordered3$Position)

### 4.3) K-mean clustering --------------------------------------------------

# Variables to analise

variables <- c("Tackle", "Passing", "Speed", 
               "Technique", "Shooting", "Equilibrium",
               "BallRecovery", "Reflexes",
               "HeadingAccuracy", "Jumping", "Stamina",
               "Strength", "Penalties")

# Dataframe to collect the results

results <- data.frame()

# For loop on each possible combination of variables

for (i in 1:(length(variables)-1)) {
  for (j in (i+1):length(variables)) {
    
    # Select the couple of variables
    data_for_clustering <- fifa_ordered3 %>% 
      dplyr::select(all_of(c(variables[i], variables[j])))
    
    # Standardise data
    data_scaled <- scale(data_for_clustering)
    
    # perform k-means clustering
    set.seed(123)
    kmeans_result <- kmeans(data_scaled, centers = 3, nstart = 25)
    
    # Order clusters based on centroids 
    centroids <- kmeans_result$centers %>% as.data.frame()
    centroids$cluster <- row.names(centroids)
    centroids <- centroids %>% 
      mutate(label = cut(rowMeans(centroids[,1:ncol(centroids)-1]),
                         breaks = 3, 
                         labels = c("Low Values", "Mid Values", "High Values"),
                         include.lowest = TRUE))
    
    # Map the original clusters
    cluster_labels <- centroids %>% 
      dplyr::select(cluster, label) %>%
      deframe()
    fifa_ordered3$Cluster <- factor(cluster_labels[as.character(kmeans_result$cluster)])
    
    # Calculate the number of players in each cluster for each role
    temp_results <- fifa_ordered3 %>%
      group_by(Position, Cluster) %>%
      summarise(Count = n(), .groups = 'drop') %>%
      mutate(VariablePair = paste(variables[i], variables[j], sep = " & "))
    
    # Accumulate the results
    results <- rbind(results, temp_results)
  }
}

print(results)


# Create a plot for couples of variables (you can freely change the couple)

ggplot(results %>%
         filter(VariablePair == "Tackle & Passing"), # We arbitrarily chose this couple
       aes(x = Position, y = Count, fill = Cluster)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Cluster distribution for tackling and passing",
       x = "Position", y = "Count", fill = "Cluster Level")


# Filter the data to only consider mid and high values (most important features for each role)

filtered_results <- results %>%
  filter(Cluster %in% c("Mid Values", "High Values"))

# Aggregate the results for positions and variable couples

aggregated_results <- filtered_results %>%
  group_by(Position, VariablePair) %>%
  summarise(TotalCount = sum(Count), .groups = 'drop') %>%
  arrange(Position, desc(TotalCount))

# Identify the best 3 couple of features for each Position

best_pairs <- aggregated_results %>%
  group_by(Position) %>%
  slice_max(order_by = TotalCount, n = 3) %>%
  ungroup()

print(best_pairs)


# We can create a barplot to visualise this

ggplot(best_pairs, aes(x = VariablePair, y = TotalCount, fill = Position)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  facet_wrap(~ Position, scales = "free_x") +
  theme_minimal() +
  labs(title = "Top 3 Variable Pairs by Role with High and Mid Values",
       x = "Variable Pairs",
       y = "Total Count",
       fill = "Position") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  


# PCA ON 1000 BEST PLAYERS PER ROLE ---------------------------------------

# We can also perform the PCA by selecting the top 1000 players per role
# to see if there are significantly different results or not

# 1) Let's perform a PCA on a subset of the fifa dataset ------------------

# This subset will contain only the best 1000 players for each role

# Let's first create subsets for each position
# Defenders:

top_defenders <- fifa_ordered3 %>%
  filter(Position == "Defender") %>%
  arrange(desc(Overall)) %>%
  slice(1:1000) 

# Midfielders

top_midfielders <- fifa_ordered3 %>%
  filter(Position == "Midfielder") %>%
  arrange(desc(Overall)) %>%
  slice(1:1000)

# Strikers

top_strikers <- fifa_ordered3 %>%
  filter(Position == "Striker") %>%
  arrange(desc(Overall)) %>%
  slice(1:1000) 

# Now let's combine them

best_players <- bind_rows(top_defenders, top_midfielders, top_strikers)

view(best_players)

# Now we can perform the same analysis as before, but on this new subset

pca_results <- PCA(best_players[, c("Tackle", "Passing", "Speed", 
                                    "Technique", "Shooting", "Equilibrium",
                                    "BallRecovery", "Reflexes",
                                    "HeadingAccuracy", "Jumping", "Stamina",
                                    "Strength", "Penalties")],
                   graph = FALSE)

# Visualise variables graph

fviz_pca_var(pca_results, col.var = "contrib", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)  


fviz_pca_ind(pca_results,
             geom.ind = "point", 
             col.ind = best_players$Position, 
             palette = c("blue", "red", "green"), 
             addEllipses = TRUE, 
             ellipse.level = 0.95, 
             pointshape = 21, 
             pointsize = 2, 
             fill.ind = best_players$Position, 
             legend.title = "Role",
             axes.labs = c("PC1", "PC2")) +
  theme_minimal() +
  theme(legend.position = "right", 
        axis.text = element_text(size = 12), 
        axis.title = element_text(size = 14), 
        legend.text = element_text(size = 12))

# There is still a good amount of overlap between midfielders and the other
# two roles, but now the positions are very clearly distinct

# 5) Classification -------------------------------------------------------

# 5.1) With multinomial regression ------------------------------------------

# e need to perform some data cleaning operations first.
# We don't want our dataset to be ordered by Position anymore.

fifa_ordered3$Position = sample(fifa_ordered3$Position)

# Then, let's reduce the dataset size by only choosing the variables of interest
# for the analysis (the player stats and the response (position)).
# We also want to reduce the number of observations because otherwise it will
# require too much computational power

fifa_cleaned <- fifa_ordered3 %>%
  dplyr::select(c(12, 40:52)) %>%
  slice(1:(round(15918 * 0.70)))

# Split the data into 70% per training and 30% for testing

set.seed(2056)

fifa_split <- fifa_cleaned %>%
  initial_split(prop = 0.70)

# Extract the data in each split

fifa_train <- training(fifa_split)
fifa_test <- testing(fifa_split)

# Let's see exactly how many observations we have in training and testing

cat("Training cases: ", nrow(fifa_train), "\n",
    "Test cases: ", nrow(fifa_test), sep = "")
### We have 7800 training cases and 3343 testing cases

### Let's specifiy a multinomial regression via nnet

mr_spec <- multinom_reg(
  penalty = 0.5,
  engine = "nnet",
  mode = "classification")

# To be sure, let's create a recipe that specifies that predictors
# should be on the same scale

fifa_recipe <- recipe(Position ~ ., data = fifa_train) %>%
  step_normalize(all_numeric_predictors())

# Bundle recipe and model specification into a workflow

mr_wflow <- workflow(preprocessor = fifa_recipe, spec = mr_spec)

print(mr_wflow)

# Now let's fit the workflow object

mr_wflow_fit <- mr_wflow %>%
  fit(data = fifa_train)

mr_wflow_fit

# Let's obtain predictions on the test set

results <- augment(mr_wflow_fit, fifa_test)

results %>% slice_head(n = 10)

# It's easier to evaluate using a confusion matrix

results %>%
  conf_mat(truth = Position, estimate = .pred_class)

# In this case only the midfielders got predicted accurately
# However, this model performed really badly in predicting defenders and strikers. 

# It could be easier to visualise this through a heat map:

update_geom_defaults(geom = "tile", new = list(color = "black",
                                               alpha = 0.7))

results %>% 
  conf_mat(Position, .pred_class) %>%
  autoplot(type = "heatmap")

# We can notice there are very bad predictions. 

### Let's try other models now

# We create training and testing data we'll use for all our models --------

set.seed(123)

inTrain <- createDataPartition(y = fifa_cleaned$Position, p = .70,
                               list = FALSE)

training <- fifa_cleaned[inTrain,]
testing <- fifa_cleaned[-inTrain,]

# CART --------------------------------------------------------------------

# It stands for classification and regression trees

train_control <- trainControl(method = "cv", number = 10,
                              savePredictions = TRUE)

# We used 10fold cross-validation

M_CART <- train(Position ~ ., data = training,
                trControl = train_control, tuneLength = 20,
                method = "rpart")
plot(M_CART)
M_CART$bestTune ### Returns the best complexity parameter

confusionMatrix(predict(M_CART, testing), testing$Position)
# Again, it struggles to predict defenders and strikers. 


# RANDOM FOREST -----------------------------------------------------------

# Caret package implementation with 20-fold cross validation

train_control <- trainControl(method = "cv",
                              number = 20,
                              savePredictions = TRUE)

RF1 <- train(Position ~ ., method = "rf", 
             trControl = train_control,
             preProcess = c("center", "scale"),
             tuneLength = 2,
             data = training)

print(RF1)
# Again, accuracy is not great

confusionMatrix(predict(RF1, testing), testing$Position)
# The random forest predicts a bit better than before, but it's still very bad
# There is also still an issue with the strikers.

# Let's do a random forest again but with the randomForest package

RF2 <- randomForest(Position ~ ., training)

print(RF2)
# This package does a better job than other techniques tried so far.
# Still, there is low accuracy
confusionMatrix(predict(RF2, testing), testing$Position)

# K-NEAREST NEIGHBOURS ----------------------------------------------------

### Let's turn the position variable into numbers

fifa_cleaned2 <- fifa_cleaned %>%
  mutate(Position = case_when(
    Position == "Defender" ~ 1,
    Position == "Midfielder" ~ 2,
    Position == "Striker" ~ 3
  ))

fifa_cleaned2$Position = factor(fifa_cleaned2$Position)

# Split the data

split <- sample.split(fifa_cleaned2, SplitRatio = 0.7) 
train_cl <- subset(fifa_cleaned2, split == "TRUE") 
test_cl <- subset(fifa_cleaned2, split == "FALSE") 

# Feature Scaling 

train_scale <- scale(train_cl[, 2:14]) 
test_scale <- scale(test_cl[, 2:14])

### Use the knn function

classifier_knn <- knn(train = train_scale, 
                      test = test_scale, 
                      cl = train_cl$Position, 
                      k = 1) 

# Confusion Matrix 
cm <- table(test_cl$Position, classifier_knn) 
cm

# Still not great, we need to choose different values of K

# Model Evaluation - Choosing K 

# Calculate out of Sample error 
misClassError <- mean(classifier_knn != test_cl$Position) 
print(paste('Accuracy =', 1-misClassError)) 

# K = 3 
classifier_knn <- knn(train = train_scale, 
                      test = test_scale, 
                      cl = train_cl$Position, 
                      k = 3) 
misClassError <- mean(classifier_knn != test_cl$Position) 
print(paste('Accuracy =', 1-misClassError)) 

# K = 5 
classifier_knn <- knn(train = train_scale, 
                      test = test_scale, 
                      cl = train_cl$Position, 
                      k = 5) 
misClassError <- mean(classifier_knn != test_cl$Position) 
print(paste('Accuracy =', 1-misClassError)) 

# K = 7 
classifier_knn <- knn(train = train_scale, 
                      test = test_scale, 
                      cl = train_cl$Position, 
                      k = 7) 
misClassError <- mean(classifier_knn != test_cl$Position) 
print(paste('Accuracy =', 1-misClassError)) 

# K = 15 
classifier_knn <- knn(train = train_scale, 
                      test = test_scale, 
                      cl = train_cl$Position, 
                      k = 15) 
misClassError <- mean(classifier_knn != test_cl$Position) 
print(paste('Accuracy =', 1-misClassError)) 

# K = 19 
classifier_knn <- knn(train = train_scale, 
                      test = test_scale, 
                      cl = train_cl$Position, 
                      k = 19) 
misClassError <- mean(classifier_knn != test_cl$Position) 
print(paste('Accuracy =', 1-misClassError))

# Let's plot the accuracy for different values of K.

# Data preparation
k_values <- c(1, 3, 5, 7, 15, 19)

# Calculate accuracy for each k value
accuracy_values <- sapply(k_values, function(k) {
  classifier_knn <- knn(train = train_scale, 
                        test = test_scale, 
                        cl = train_cl$Position, 
                        k = k)
  1 - mean(classifier_knn != test_cl$Position)
})


# Create a data frame for plotting
accuracy_data <- data.frame(K = k_values, Accuracy = accuracy_values)

# Plotting
ggplot(accuracy_data, aes(x = K, y = Accuracy)) +
  geom_line(color = "lightblue", size = 1) +
  geom_point(color = "lightgreen", size = 3) +
  labs(title = "Model Accuracy for Different K Values",
       x = "Number of Neighbors (K)",
       y = "Accuracy") +
  theme_minimal()

# It seems like higher values of k increase accuracy. Let's try with k = 15

classifier_knn <- knn(train = train_scale, 
                      test = test_scale, 
                      cl = train_cl$Position, 
                      k = 15) 

cm <- table(test_cl$Position, classifier_knn) 
cm

# It also doesn't perform well. Let's try with elastic net

# ELASTIC NET -------------------------------------------------------------

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

cv_model <- cv.glmnet(x_train, y_train, family = "multinomial",
                      type.measure = "class", alpha = 0.5)

# Train the Elastic Net model

final_model <- glmnet(x_train, y_train, family = "multinomial",
                      lambda = cv_model$lambda.min, alpha = 0.5)


# Predictions on the test set

predicted_roles <- predict(final_model, s = cv_model$lambda.min,
                           newx = x_test, type = "class")

# Accuracy calculation

accuracy <- sum(predicted_roles == y_test) / length(y_test)
print(paste("Accuracy:", accuracy))

# Confusion matrix

confusionMatrix <- confusionMatrix(as.factor(predicted_roles), as.factor(y_test))
print(confusionMatrix)










