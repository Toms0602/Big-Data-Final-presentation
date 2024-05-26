library(caret)
library(rpart)
library(e1071)
library(randomForest)
library(tidymodels)
library(nnet)
library(parsnip)
library(recipes)
library(workflows)
library(yardstick)
library(tidyverse)
library(corrplot)
library(FactoMineR)
library(factoextra)
library(MASS)

# Let's perform the same classification but on 2 different subsets --------

# Clean the data first ----------------------------------------------------

# 1) Load and clean the data ----------------------------------------------

fifa <- read.csv("FIFA.csv")

fifa <- fifa %>% dplyr::select(!c(Wage, Special, International.Reputation,
                                  Contract.Valid.Until, Height, Weight,
                                  GKDiving, GKHandling, GKKicking, GKPositioning,
                                  GKReflexes, Release.Clause))


fifa <- drop_na(fifa) 

order <- c("GK", "CB", "RCB", "LCB", "RB", "LB", "LWB",
           "RWB", "CDM", "RDM", "LDM", "RCM", "LCM",
           "CM", "RM", "LM", "CAM", "RAM", "LAM", "RW", 
           "LW", "RF", "LF", "CF", "ST", "RS", "LS")

fifa_ordered <- fifa %>% mutate(
  Position =  factor(Position, levels = order)) %>%
  arrange(Position) 

fifa_ordered <- fifa_ordered[-c(1:1989),] 

fifa_ordered2 <- fifa_ordered %>% mutate(Position = case_when(
  Position %in% c("CB", "RCB", "LCB", "RB", "LB", "LWB", "RWB") ~ "Defender",
  Position %in% c("CDM", "RDM", "LDM", "RCM", "LCM", "CM", "RM", "LM", "CAM", "RAM", "LAM") ~ "Midfielder",
  Position %in% c("RW", "LW", "RF", "LF", "CF", "ST", "RS", "LS") ~ "Striker"
))

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


# k-means clustering for best variable couple -----------------------------

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

### We identify the best variables we want to keep in the dataset:
### BallRecovery, Stamina, Tackle, Passing, Equilibrium, Technique,
### Shooting, Penalties, Speed


# CREATE SUBSET 1 - BEST VARIABLES ----------------------------------------

fifa_best <- fifa_ordered3 %>%
  dplyr::select(c(Position, BallRecovery, Stamina, Tackle, Passing, 
           Equilibrium, Technique,
           Shooting, Penalties, Speed))

## In order to do the classification, let's un-order the position variable and
## select only 70% of the dataset to reduce computational power required

fifa_best$Position = sample(fifa_best$Position)

fifa_best <- fifa_best %>%
  slice(1:(round(15918 * 0.5)))

fifa_best$Position = factor(fifa_best$Position)

### Let's perform classification on this subset

### Split the data into 70% per training and 30% for testing

set.seed(2056)

fifa_split <- fifa_best %>%
  initial_split(prop = 0.70)

### Extract the data in each split

fifa_train <- training(fifa_split)
fifa_test <- testing(fifa_split)

### Let's see exactly how many observations we have in training and testing

cat("Training cases: ", nrow(fifa_train), "\n",
    "Test cases: ", nrow(fifa_test), sep = "")
### We have 5571 training cases and 2388 testing cases

### Let's specifiy a multinomial regression via nnet

mr_spec <- multinom_reg(
  penalty = 1,
  engine = "nnet",
  mode = "classification")

### To be sure, let's create a recipe that specifies that predictors
### should be on the same scale

fifa_recipe <- recipe(Position ~ ., data = fifa_train) %>%
  step_normalize(all_numeric_predictors())

### Bundle recipe and model specification into a workflow

mr_wflow <- workflow(preprocessor = fifa_recipe, spec = mr_spec)

print(mr_wflow)

### Now let's fit the workflow object

mr_wflow_fit <- mr_wflow %>%
  fit(data = fifa_train)

mr_wflow_fit

### Let's obtain predictions on the test set

results <- augment(mr_wflow_fit, fifa_test)

results %>% slice_head(n = 10)

### It's easier to evaluate using a confusion matrix

results %>%
  conf_mat(truth = Position, estimate = .pred_class)

### In this case only the midfielders got predicted accurately (93% correctly predicted)
### However, this model performed really badly in predicting defenders and strikers. 

### It could be easier to visualise this through a heat map:

update_geom_defaults(geom = "tile", new = list(color = "black",
                                               alpha = 0.7))

results %>% 
  conf_mat(Position, .pred_class) %>%
  autoplot(type = "heatmap")

### We can notice there are very bad predictions. 
### We can show it even better by using the yarstick package to 
### calulcate Accuracy, Precision and Recall

accuracy = accuracy(data = results, truth = Position,
                    estimate = .pred_class)
precision = precision(data = results, truth = Position,
                      estimate = .pred_class)
recall = recall(data = results, truth = Position,
                estimate = .pred_class)

### Print the metrics

accuracy %>% 
  bind_rows(precision) %>%
  bind_rows(recall)
### Very low values

### Let's try other models now

# We create training and testing data we'll use for all our models --------

set.seed(123)

inTrain <- createDataPartition(y = fifa_best$Position, p = .70,
                               list = FALSE)

training <- fifa_best[inTrain,]
testing <- fifa_best[-inTrain,]

# CART --------------------------------------------------------------------

### It stands for classification and regression trees

train_control <- trainControl(method = "cv", number = 10,
                              savePredictions = TRUE)
### We used 10fold cross-validation

M_CART <- train(Position ~ ., data = training,
                trControl = train_control, tuneLength = 10,
                method = "rpart")
plot(M_CART)
M_CART$bestTune ### Returns the best complexity parameter

confusionMatrix(predict(M_CART, testing), testing$Position)
### Again, it struggles to predict defenders and strikers. 


# RANDOM FOREST -----------------------------------------------------------

### Caret package implementation with 20-fold cross validation

train_control <- trainControl(method = "cv",
                              number = 10,
                              savePredictions = TRUE)

RF1 <- train(Position ~ ., method = "rf", 
             trControl = train_control,
             preProcess = c("center", "scale"),
             tuneLength = 2,
             data = training)

print(RF1)
### Again, accuracy is not great

confusionMatrix(predict(RF1, testing), testing$Position)
### The random forest predicts defenders a bit better than other methods.
### There is still an issue with the strikers.

### Let's do a random forest again but with the randomForest package

RF2 <- randomForest(Position ~ ., training)

print(RF2)
### This package does a better job than other techniques tried so far.
### Still, there is low accuracy
confusionMatrix(predict(RF2, testing), testing$Position)


# SUPPORT VECTOR MACHINE --------------------------------------------------

### e1071 package implementation

SVM1 <- svm(Position ~ ., data = training)

confusionMatrix(predict(SVM1, testing), testing$Position)
### This also didn't perform well

### We can tune this 

svm_tune <- e1071::tune(svm, train.x = training[,-1], train.y = training[,1],
                 ranges = list(cost = 10^(-1:2), gamma = c(.5, 1, 2)))
### We have to tune the cost and gamma parameters. We are checking for 
### 3 different values of cost parameter and gamma parameter

print(svm_tune) ### Allows you to select best cost and gamma parameter

### Doesn't run, too much computational power required

### This is the code after selection the optimal parameters:

SVM_RETUNE <- svm(Position ~ ., data = training, cost = ..., gamma = ...)
confusionMatrix(preditc(SVM_RETUNE, testing), testing$Position)


# CLASSIFICATION ON SUBSET 2 ----------------------------------------------

### Let's first create subsets for each position
### Defenders:

top_defenders <- fifa_ordered3 %>%
  filter(Position == "Defender") %>%
  arrange(desc(Overall)) %>%
  slice(1:1000) 

### Midfielders

top_midfielders <- fifa_ordered3 %>%
  filter(Position == "Midfielder") %>%
  arrange(desc(Overall)) %>%
  slice(1:1000)

### Strikers

top_strikers <- fifa_ordered3 %>%
  filter(Position == "Striker") %>%
  arrange(desc(Overall)) %>%
  slice(1:1000) 

### Now let's combine them

best_players <- bind_rows(top_defenders, top_midfielders, top_strikers)

### let's only keep the variables of interest and un-order the dataset

best_players <- best_players %>%
  dplyr::select(c(12, 40:52))

best_players$Position = sample(best_players$Position)

### Let's perform classification 

best_players$Position = factor(best_players$Position)

### Let's perform classification on this subset

### Split the data into 80% per training and 20% for testing

set.seed(2056)

fifa_split <- best_players %>%
  initial_split(prop = 0.80)

### Extract the data in each split

fifa_train <- training(fifa_split)
fifa_test <- testing(fifa_split)

### Let's see exactly how many observations we have in training and testing

cat("Training cases: ", nrow(fifa_train), "\n",
    "Test cases: ", nrow(fifa_test), sep = "")
### We have 2400 training cases and 600 testing cases

### Let's specify a multinomial regression via nnet

mr_spec <- multinom_reg(
  penalty = 1,
  engine = "nnet",
  mode = "classification")

### To be sure, let's create a recipe that specifies that predictors
### should be on the same scale

fifa_recipe <- recipe(Position ~ ., data = fifa_train) %>%
  step_normalize(all_numeric_predictors())

### Bundle recipe and model specification into a workflow

mr_wflow <- workflow(preprocessor = fifa_recipe, spec = mr_spec)

print(mr_wflow)

### Now let's fit the workflow object

mr_wflow_fit <- mr_wflow %>%
  fit(data = fifa_train)

mr_wflow_fit

### Let's obtain predictions on the test set

results <- augment(mr_wflow_fit, fifa_test)

results %>% slice_head(n = 10)

### It's easier to evaluate using a confusion matrix

results %>%
  conf_mat(truth = Position, estimate = .pred_class)

### This algorithm is predicting almost equally every class, which is not good

### It could be easier to visualise this through a heat map:

update_geom_defaults(geom = "tile", new = list(color = "black",
                                               alpha = 0.7))

results %>% 
  conf_mat(Position, .pred_class) %>%
  autoplot(type = "heatmap")

### We can notice there are very bad predictions. 
### We can show it even better by using the yarstick package to 
### calulcate Accuracy, Precision and Recall

accuracy = accuracy(data = results, truth = Position,
                    estimate = .pred_class)
precision = precision(data = results, truth = Position,
                      estimate = .pred_class)
recall = recall(data = results, truth = Position,
                estimate = .pred_class)

### Print the metrics

accuracy %>% 
  bind_rows(precision) %>%
  bind_rows(recall)
### Very low values

### Let's try other models now

# We create training and testing data we'll use for all our models --------

set.seed(123)

inTrain <- createDataPartition(y = best_players$Position, p = .70,
                               list = FALSE)

training <- best_players[inTrain,]
testing <- best_players[-inTrain,]

# CART --------------------------------------------------------------------

### It stands for classification and regression trees

train_control <- trainControl(method = "cv", number = 10,
                              savePredictions = TRUE)
### We used 10fold cross-validation

M_CART <- train(Position ~ ., data = training,
                trControl = train_control, tuneLength = 10,
                method = "rpart")
plot(M_CART)
M_CART$bestTune ### Returns the best complexity parameter

confusionMatrix(predict(M_CART, testing), testing$Position)
### Again, it struggles to predict defenders and strikers. 


# RANDOM FOREST -----------------------------------------------------------

### Caret package implementation with 20-fold cross validation

train_control <- trainControl(method = "cv",
                              number = 10,
                              savePredictions = TRUE)

RF1 <- train(Position ~ ., method = "rf", 
             trControl = train_control,
             preProcess = c("center", "scale"),
             tuneLength = 2,
             data = training)

print(RF1)
### Again, accuracy is not great

confusionMatrix(predict(RF1, testing), testing$Position)
### The random forest predicts defenders a bit better than other methods.
### There is still an issue with the strikers.

### Let's do a random forest again but with the randomForest package

RF2 <- randomForest(Position ~ ., training)

print(RF2)
### This package does a better job than other techniques tried so far.
### Still, there is low accuracy
confusionMatrix(predict(RF2, testing), testing$Position)


# SUPPORT VECTOR MACHINE --------------------------------------------------

### e1071 package implementation

SVM1 <- svm(Position ~ ., data = training)

confusionMatrix(predict(SVM1, testing), testing$Position)
### This also didn't perform well

### We can tune this 

svm_tune <- e1071::tune(svm, train.x = training[,-1], train.y = training[,1],
                        ranges = list(cost = 10^(-1:2), gamma = c(.5, 1, 2)))
### We have to tune the cost and gamma parameters. We are checking for 
### 3 different values of cost parameter and gamma parameter

print(svm_tune) ### Allows you to select best cost and gamma parameter
### According to this the best parameters are cost = 10, gamma = 0.5

### This is the code after selection the optimal parameters:

SVM_RETUNE <- svm(Position ~ ., data = training, cost = 10, gamma = 0.5)
confusionMatrix(predict(SVM_RETUNE, testing), testing$Position)

### Overall, we can't get good results.




























