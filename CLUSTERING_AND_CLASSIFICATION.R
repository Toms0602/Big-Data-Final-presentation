library(tidyverse)
library(corrplot)
library(FactoMineR)
library(factoextra)
library(MASS)

# 1) Load and clean the data ----------------------------------------------

fifa <- read.csv("FIFA.csv")

### This dataset is very dense in variables. We wish to remove some that we
### know we are not interested in using:

fifa <- fifa %>% dplyr::select(!c(Wage, Special, International.Reputation,
                           Contract.Valid.Until, Height, Weight,
                           GKDiving, GKHandling, GKKicking, GKPositioning,
                           GKReflexes, Release.Clause))

### We removed GK variables because we will not consider goalkeepers in our analysis

### We can check for NA's

fifa %>% summarise(count = sum(is.na(fifa)))
### There are many NA's, let's drop them

fifa <- drop_na(fifa) 
### We can safely drop them, they won't influence the analysis


# 2) Visualise the data ---------------------------------------------------

### I would like to check the distribution of overalls depending on the positions
### First, let's order the positions from defensive to offensive

### We have to create a vector with the desired order

order <- c("GK", "CB", "RCB", "LCB", "RB", "LB", "LWB",
           "RWB", "CDM", "RDM", "LDM", "RCM", "LCM",
           "CM", "RM", "LM", "CAM", "RAM", "LAM", "RW", 
           "LW", "RF", "LF", "CF", "ST", "RS", "LS")

### Then we match it with our Position column in the data frame

fifa_ordered <- fifa %>% mutate(
  Position =  factor(Position, levels = order)) %>%
  arrange(Position) 

fifa_ordered <- fifa_ordered[-c(1:1989),] 
### We dropped all rows dedicated to goalkeepers

### Now that they are ordered, let's check if offensive and defensive positions
### have different average overalls

fifa_ordered %>% group_by(Position) %>% 
  summarise(Overall = mean(Overall)) %>% 
  ggplot(mapping = aes(Overall, Position)) +
  geom_point()

### It does indeed seem like there is are higher average overalls
### for offensive positions rather than defensive

### Let's add a new column where we group all of the positions in:
### "Defender", "Midfielder", "Striker"

fifa_ordered2 <- fifa_ordered %>% mutate(Position = case_when(
  Position %in% c("CB", "RCB", "LCB", "RB", "LB", "LWB", "RWB") ~ "Defender",
  Position %in% c("CDM", "RDM", "LDM", "RCM", "LCM", "CM", "RM", "LM", "CAM", "RAM", "LAM") ~ "Midfielder",
  Position %in% c("RW", "LW", "RF", "LF", "CF", "ST", "RS", "LS") ~ "Striker"
))

### Now we can check the distribution of overalls for each role

ggplot(fifa_ordered2, aes(x = Position, y = Overall, fill = Position)) +
  geom_boxplot() +
  labs(title = "Overall distribution per position",
       x = "Position",
       y = "Overall") +
  scale_fill_brewer(palette = "Set1") +  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 


# 3) Correlations and model building --------------------------------------

cor_matrix <- cor(fifa_ordered2[, c("Overall", "Crossing", "Finishing", "HeadingAccuracy", "ShortPassing", "Volleys", 
                                    "Dribbling", "Curve", "FKAccuracy", "LongPassing", "BallControl", "Acceleration", 
                                    "SprintSpeed", "Agility", "Reactions", "Balance", "ShotPower", "Jumping", "Stamina", 
                                    "Strength", "LongShots", "Aggression", "Interceptions", "Positioning", "Vision", 
                                    "Penalties", "Composure", "Marking", "StandingTackle", "SlidingTackle")], 
                  use = "complete.obs")

corrplot(cor_matrix, method = "circle")

### Some variables have extremely high correlations, and/or are basically the same.
### Since there are many variables, we can reduce the number of them by combining
### two or more variables together (for instance: Sliding and standing tackle).
### We will do that by averaging their values

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
### In the end we also erased the variables we combined from the dataset. 


# 4) Clustering -----------------------------------------------------------


### 4.1) Perform a PCA ------------------------------------------------------

### We will utilise every variables: The combines + the uncombined ones

pca_results <- PCA(fifa_ordered3[, c("Tackle", "Passing", "Speed", 
                                     "Technique", "Shooting", "Equilibrium",
                                     "BallRecovery", "Reflexes",
                                     "HeadingAccuracy", "Jumping", "Stamina",
                                     "Strength", "Penalties")],
                   graph = FALSE)

# Visualizza il grafico delle variabili (loading plot)
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

### We can tell two clusters mainly: Defenders are mostly separate
### from the rest, midfielders and strikers are more mixed.
### However, strikers and defenders are clearly separate.

### 4.2) LDA Analysis -------------------------------------------------------

### Prepare the formula for LDA
formula_lda <- Position ~ Tackle + Passing + Speed + Technique + 
                          Shooting + Equilibrium + BallRecovery + 
                          Reflexes + HeadingAccuracy + Jumping + 
                          Stamina + Strength + Penalties

### LDA
lda_model <- lda(formula_lda, data = fifa_ordered3)
print(summary(lda_model))

### Show which variables have the highest contributions
print(lda_model$scaling)

# Let's visualise the results
plot(lda_model, dimen = 1, col = as.numeric(fifa_ordered3$Position))


# Put the Position variable as factor
fifa_ordered3$Position <- as.factor(fifa_ordered3$Position)

# STESSO GRAFICO MA CON GGPLOT
ggplot(fifa_ordered3, aes(x = LD1, fill = Position)) +
  geom_histogram(bins = 30, alpha = 0.6, position = 'identity') +
  facet_wrap(~ Position, scales = 'free') +
  labs(title = "Distribution of LDA Scores by Player Role",
       x = "First Linear Discriminant (LD1)",
       y = "Frequency") +
  scale_fill_brewer(palette = "Set1") +  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  


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


# Creare a plot for couples of variables (you can freely change the couple)
ggplot(results %>%
         filter(VariablePair == "Tackle & Passing"),
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


























































































