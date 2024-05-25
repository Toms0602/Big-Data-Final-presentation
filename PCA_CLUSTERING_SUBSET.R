library(tidyverse)
library(corrplot)
library(FactoMineR)
library(factoextra)
library(MASS)

# 1) Let's perform a PCA on a subset of the fifa dataset ------------------

### This subset will contain only the best 1000 players for each role

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

view(best_players)

### Now we can perform the same analysis as before, but on this new subset

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



### Let's also perform the LDA analysis again


### Prepare the formula for LDA
formula_lda <- Position ~ Tackle + Passing + Speed + Technique + 
  Shooting + Equilibrium + BallRecovery + 
  Reflexes + HeadingAccuracy + Jumping + 
  Stamina + Strength + Penalties

### LDA
lda_model <- lda(formula_lda, data = best_players)
print(summary(lda_model))

### Show which variables have the highest contributions
print(lda_model$scaling)

# Let's visualise the results
plot(lda_model, dimen = 1, col = as.numeric(best_players$Position))



### Let's also do the k-means clustering


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
    data_for_clustering <- best_players %>% 
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
    best_players$Cluster <- factor(cluster_labels[as.character(kmeans_result$cluster)])
    
    # Calculate the number of players in each cluster for each role
    temp_results <- best_players %>%
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
         filter(VariablePair == "Speed & Shooting"),
       aes(x = Position, y = Count, fill = Cluster)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Cluster distribution for speed & shooting",
       x = "Position", y = "Count", fill = "Cluster Level")


# Filter the data to only consider mid and high values (most important features for each role)
filtered_results <- results %>%
  filter(Cluster %in% c("High Values")) ### We removed low and mid values since the best players don't have many

# Aggregate the results for positions and variable couples
aggregated_results <- filtered_results %>%
  group_by(Position, VariablePair) %>%
  summarise(TotalCount = sum(Count), .groups = 'drop') %>%
  arrange(Position, desc(TotalCount))

view(aggregated_results)

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
  labs(title = "Top 3 Variable Pairs by Role with High Values",
       x = "Variable Pairs",
       y = "Total Count",
       fill = "Position") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 



























































