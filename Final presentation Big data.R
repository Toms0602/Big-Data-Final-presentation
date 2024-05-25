
# Final presentation Bid Data ---------------------------------------------

library(tidyverse)
library(dplyr)
library(ggplot2)
library(corrplot)
library(car)
library(FactoMineR)
library(factoextra)
library(MASS)
library(tidyr)


# 1) Open and clean the file -------------------------------------------------

fifa <- read.csv("FIFA.csv")

### Let's remove unnecessary variables
### We can remove Wage; Special; International.Reputation; Contract.Valid.Until;
### Height, Weight; 

fifa <- fifa %>% select(!c(Wage, Special, International.Reputation,
                           Contract.Valid.Until, Height, Weight))
### With the "!! operator we are basically keeping all the variables except the
### ones we wrote in the code

### Now let's check for NA's

fifa %>% summarise(count = sum(is.na(fifa)))
### There are many NA's, let's drop them

fifa <- drop_na(fifa) 
### We lost some observations which were players that were without some stats.
### We can't fill the up with average values, or other kind of values, because
### each player has its own unique stats


# 2) Visualise the data ---------------------------------------------------

### I would like to check the distribution of overalls depending on the positions
### First, let's order the positions from defensive to offensive

### We have to create a vector with the desired order

order <- c("GK", "CB", "RCB", "LCB", "RB", "LB", "CDM", 
           "RDM", "LDM", "LWB", "RWB", "RCM", "LCM",
           "CM", "RM", "LM", "CAM", "RAM", "LAM", "RW", 
           "LW", "RF", "LF", "CF", "ST", "RS", "LS")

### Then we match it with our Position column in the data frame

fifa_ordered <- fifa %>% mutate(
  Position =  factor(Position, levels = order)) %>%
  arrange(Position)

### Now that they are ordered, let's check if offensive and defensive positions
### have different average overalls

fifa_ordered %>% group_by(Position) %>% 
  summarise(Overall = mean(Overall)) %>% 
  ggplot(mapping = aes(Overall, Position)) +
  geom_point()

### It does indeed seem like there is are higher average overalls
### for offensive positions rather than defensive


# Aggiungere una nuova colonna "Role" basata sulla posizione
fifa <- fifa %>% mutate(Role = case_when(
  Position %in% c("GK") ~ "Goalkeeper",
  Position %in% c("CB", "RCB", "LCB", "RB", "LB", "LWB", "RWB") ~ "Defender",
  Position %in% c("CDM", "RDM", "LDM", "RCM", "LCM", "CM", "RM", "LM", "CAM", "RAM", "LAM") ~ "Midfielder",
  Position %in% c("RW", "LW", "RF", "LF", "CF", "ST", "RS", "LS") ~ "Attacker",
  TRUE ~ "Other"  # Questo gestisce eventuali posizioni non elencate
))

head(fifa)

######## VISUALIZZAZIONE PLOT PER CAPIRE DOVE SI DISTRIBUISCONO GLI OVERALL PIU' ELEVATI

# Creare un boxplot per vedere la distribuzione degli Overall per ogni ruolo
ggplot(fifa, aes(x = Role, y = Overall, fill = Role)) +
  geom_boxplot() +
  labs(title = "Distribuzione degli Overall per Ruolo",
       x = "Ruolo",
       y = "Overall") +
  scale_fill_brewer(palette = "Set1") +  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  



# Calcola la media degli Overall per ogni ruolo a intervalli di 5 punti di Overall
overall_groups <- fifa %>% 
  mutate(Overall_group = cut(Overall, breaks = seq(40, 100, by = 5), include.lowest = TRUE)) %>%
  group_by(Role, Overall_group) %>%
  summarise(Average_Overall = mean(Overall, na.rm = TRUE), .groups = 'drop')

# Grafico a linee delle medie di Overall per ruolo
ggplot(overall_groups, aes(x = Overall_group, y = Average_Overall, group = Role, color = Role)) +
  geom_line() +
  geom_point() +  
  labs(title = "Media di Overall per Ruolo a Intervalli Specifici",
       x = "Intervallo di Overall",
       y = "Media di Overall") +
  scale_color_brewer(palette = "Set1") +  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  



#### PICCOLO CUT ####

# Filtra fuori i goalkeepers e crea un nuovo dataset fifa2
fifa2 <- fifa %>%
  filter(Position != "GK")

head(fifa2)

# escludiamo tutte le colonne che contengono "GK" nel nome
fifa2 <- fifa2 %>%
  select(-contains("GK"))  

glimpse(fifa2)



########## CALCOLO DELLE MEDIE PRECISE #########

# Calcolare la media degli Overall per ogni ruolo
average_overalls <- fifa %>%
  group_by(Role) %>%
  summarise(Average_Overall = mean(Overall, na.rm = TRUE))

print(average_overalls)




######### MATRICE DI CORRELAZIONE #########

# Calcola la matrice di correlazione tra Overall e le altre variabili
cor_matrix <- cor(fifa2[, c("Overall", "Crossing", "Finishing", "HeadingAccuracy", "ShortPassing", "Volleys", 
                            "Dribbling", "Curve", "FKAccuracy", "LongPassing", "BallControl", "Acceleration", 
                            "SprintSpeed", "Agility", "Reactions", "Balance", "ShotPower", "Jumping", "Stamina", 
                            "Strength", "LongShots", "Aggression", "Interceptions", "Positioning", "Vision", 
                            "Penalties", "Composure", "Marking", "StandingTackle", "SlidingTackle")], 
                  use = "complete.obs")

corrplot(cor_matrix, method = "circle")


####### COMBINAZIONE DELLE VARIABILI ####

#combinare le variabili e rimuovere quelle vecchie non necessarie
fifa3 <- fifa2 %>% 
  mutate(Aggressiveness = (SlidingTackle + StandingTackle + 
                             Aggression + Interceptions + Marking)/5) %>%
  mutate(Speed = (Acceleration + SprintSpeed + Agility + Balance)/4) %>%
  mutate(Technique = (BallControl + Dribbling + LongPassing + 
                        ShortPassing + Crossing + Vision)/6) %>%
  mutate(Shooting = (Finishing + Volleys + ShotPower + LongShots)/4) %>%
  select(!c(SlidingTackle, StandingTackle, LongPassing, ShortPassing, 
            Crossing, Vision, Acceleration , SprintSpeed, BallControl, 
            Dribbling, Finishing, Volleys, ShotPower, LongShots, Agility,
            Balance, Aggression, Interceptions, Marking))


# Converti le colonne specificate in integers
fifa3 <- fifa3 %>%
  mutate(
        Aggressiveness = as.integer(Aggressiveness),
    Speed = as.integer(Speed),
    Technique = as.integer(Technique),  
    Shooting = as.integer(Shooting)
    )

head(fifa3)

view(fifa3)



######## PCA ########

# Supponendo che 'fifa3' contenga le variabili numeriche di interesse
# Esegui la PCA sulle variabili selezionate
pca_results <- PCA(fifa3[, c("Aggressiveness", "Speed", "Technique", "Shooting", "HeadingAccuracy",
                             "Curve", "Jumping", "Stamina", "Strength", "Positioning",
                             "Penalties", "Composure")], graph = FALSE)

# Visualizza il grafico delle variabili (loading plot)
fviz_pca_var(pca_results, col.var = "contrib", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)  # Usa repel per evitare sovrapposizione di etichette


fviz_pca_ind(pca_results,
             geom.ind = "point", # Usa solo punti, senza testo
             col.ind = fifa3$Role, # Colora in base al ruolo
             palette = c("blue", "red", "green"), # Imposta i colori per i ruoli
             addEllipses = TRUE, # Aggiungi ellissi di confidenza per ciascun ruolo
             ellipse.level = 0.95, # Livello di confidenza per le ellissi
             pointshape = 21, # Forma dei punti (21 è un cerchio pieno)
             pointsize = 2, # Dimensione dei punti
             fill.ind = fifa3$Role, # Riempimento dei punti basato sui ruoli
             legend.title = "Role",
             axes.labs = c("PC1", "PC2")) +
  theme_minimal() +
  theme(legend.position = "right", # Posiziona la legenda a destra
        axis.text = element_text(size = 12), # Dimensione testo degli assi
        axis.title = element_text(size = 14), # Dimensione titoli degli assi
        legend.text = element_text(size = 12)) # Dimensione testo della legenda




###### L'Analisi Discriminante Lineare (LDA) ####


# Assumendo che 'fifa3' sia il tuo dataframe e che include le colonne per reparto e per le variabili di interesse
# Prepara la formula per LDA
formula_lda <- Role ~ Aggressiveness + Speed + Technique + Shooting + HeadingAccuracy +
  Curve + Jumping + Stamina + Strength + Positioning + Penalties + Composure

# Analisi Discriminante Lineare
lda_model <- lda(formula_lda, data = fifa3)
print(summary(lda_model))

# Mostra i coefficienti dei discriminanti lineari per vedere quali variabili contribuiscono maggiormente
print(lda_model$scaling)

# Visualizzazione dei risultati
plot(lda_model, dimen = 1, col = as.numeric(fifa3$Role))


# 'Role' fattorizzato 
fifa3$Role <- as.factor(fifa3$Role)

# STESSO GRAFICO MA CON GGPLOT
ggplot(fifa3, aes(x = LD1, fill = Role)) +
  geom_histogram(bins = 30, alpha = 0.6, position = 'identity') +
  facet_wrap(~ Role, scales = 'free') +
  labs(title = "Distribution of LDA Scores by Player Role",
       x = "First Linear Discriminant (LD1)",
       y = "Frequency") +
  scale_fill_brewer(palette = "Set1") +  # Usa una palette di colori per distinguere i ruoli
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Migliora la leggibilità





#### CLUSTERING K-MEANS ####


# Lista delle variabili da analizzare
variables <- c("Aggressiveness", "Speed", "Technique", "Shooting", "HeadingAccuracy", "Curve", "Jumping", "Stamina", "Strength", "Positioning", "Penalties", "Composure")

# Dataframe per raccogliere i risultati
results <- data.frame()

# Ciclo su tutte le combinazioni possibili delle variabili
for (i in 1:(length(variables)-1)) {
  for (j in (i+1):length(variables)) {
    # Selezionare la coppia di variabili
    data_for_clustering <- fifa3 %>% 
      select(all_of(c(variables[i], variables[j])))
    
    # Standardizzare i dati
    data_scaled <- scale(data_for_clustering)
    
    # Eseguire il clustering K-means
    set.seed(123)
    kmeans_result <- kmeans(data_scaled, centers = 3, nstart = 25)
    
    # Ordinare i cluster in base ai centroidi e assegna le etichette
    centroids <- kmeans_result$centers %>% as.data.frame()
    centroids$cluster <- row.names(centroids)
    centroids <- centroids %>% mutate(label = cut(rowMeans(centroids[,1:ncol(centroids)-1]), breaks = 3, labels = c("Low Values", "Mid Values", "High Values"), include.lowest = TRUE))
    
    # Mappare i cluster originali alle nuove etichette
    cluster_labels <- centroids %>% select(cluster, label) %>% deframe()
    fifa3$Cluster <- factor(cluster_labels[as.character(kmeans_result$cluster)])
    
    # Calcolare il numero di giocatori in ogni cluster per ogni reparto
    temp_results <- fifa3 %>%
      group_by(Role, Cluster) %>%
      summarise(Count = n(), .groups = 'drop') %>%
      mutate(VariablePair = paste(variables[i], variables[j], sep = " & "))
    
    # Accumulare i risultati
    results <- rbind(results, temp_results)
  }
}

print(results)


# Creare un plot per coppie di variabili (BASTA CAMBIARE LA COPPIA)
ggplot(results %>% filter(VariablePair == "Speed & Technique"), aes(x = Role, y = Count, fill = Cluster)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Distribuzione dei Cluster per Speed & Technique", x = "Role", y = "Count", fill = "Cluster Level")


# Filtrare i dati per considerare solo i clusters "Mid Values" e "High Values"
filtered_results <- results %>%
  filter(Cluster %in% c("Mid Values", "High Values"))

# Aggregare i risultati per reparto e coppia di variabili
aggregated_results <- filtered_results %>%
  group_by(Role, VariablePair) %>%
  summarise(TotalCount = sum(Count), .groups = 'drop') %>%
  arrange(Role, desc(TotalCount))

# Identificare le migliori tre coppie per ogni reparto
best_pairs <- aggregated_results %>%
  group_by(Role) %>%
  slice_max(order_by = TotalCount, n = 3) %>%
  ungroup()

print(best_pairs)


# Creare un grafico a barre per visualizzare le migliori tre coppie di variabili per ogni ruolo
ggplot(best_pairs, aes(x = VariablePair, y = TotalCount, fill = Role)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  facet_wrap(~ Role, scales = "free_x") +
  theme_minimal() +
  labs(title = "Top 3 Variable Pairs by Role with High and Mid Values",
       x = "Variable Pairs",
       y = "Total Count",
       fill = "Role") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Ruota i nomi delle variabili sull'asse X per migliorare la leggibilità



