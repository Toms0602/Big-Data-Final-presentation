
# Final presentation Bid Data ---------------------------------------------

library(tidyverse)

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

################ DA QUI NUOVO CODICE #################

library(dplyr)
library(ggplot2)


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


########## CALCOLO DELLE MEDIE PRECISE #########

# Calcolare la media degli Overall per ogni ruolo
average_overalls <- fifa %>%
  group_by(Role) %>%
  summarise(Average_Overall = mean(Overall, na.rm = TRUE))

print(average_overalls)



######### ANALISI DI CORRELAZIONE E REGRESSIONE ##########


#### PICCOLO CUT ####

# Filtra fuori i goalkeepers e crea un nuovo dataset fifa2
fifa2 <- fifa %>%
  filter(Position != "GK")

head(fifa2)

# escludiamo tutte le colonne che contengono "GK" nel nome
fifa2 <- fifa2 %>%
  select(-contains("GK"))  

glimpse(fifa2)



###### CORRELAZIONE #####

library(corrplot)
library(car)

# Calcola la matrice di correlazione tra Overall e le altre variabili
cor_matrix <- cor(fifa2[, c("Overall", "Crossing", "Finishing", "HeadingAccuracy", "ShortPassing", "Volleys", 
                           "Dribbling", "Curve", "FKAccuracy", "LongPassing", "BallControl", "Acceleration", 
                           "SprintSpeed", "Agility", "Reactions", "Balance", "ShotPower", "Jumping", "Stamina", 
                           "Strength", "LongShots", "Aggression", "Interceptions", "Positioning", "Vision", 
                           "Penalties", "Composure", "Marking", "StandingTackle", "SlidingTackle")], 
                  use = "complete.obs")

corrplot(cor_matrix, method = "circle")



###### ANALISI MULTICOLLINEARITA' ########

vif_model <- lm(Overall ~ Crossing + Finishing + HeadingAccuracy + ShortPassing + Volleys + 
                  Dribbling + Curve + FKAccuracy + LongPassing + BallControl + Acceleration + 
                  SprintSpeed + Agility + Reactions + Balance + ShotPower + Jumping + Stamina + 
                  Strength + LongShots + Aggression + Interceptions + Positioning + Vision + 
                  Penalties + Composure + Marking + StandingTackle + SlidingTackle, data = fifa2)

# Calcolo del VIF per ogni variabile predittiva nel modello
vif_values <- vif(vif_model)

print(vif_values)

#From the VIF results we can see that some variables have significantly high VIF values, indicating potential multicollinearity problems
#Generally, a VIF greater than 5 suggests that the variable may be strongly correlated with other variables in the model, and even higher values such as 10 or more are particularly concerning.



########## RIDGE E LASSO ##########

library(gamlss)

# Definizione del modello Lasso 
lasso_model_gamlss <- gamlss(Overall ~ pb(Crossing) + pb(Finishing) + pb(HeadingAccuracy) + pb(ShortPassing) + 
                               pb(Volleys) + pb(Dribbling) + pb(Curve) + pb(FKAccuracy) + pb(LongPassing) + 
                               pb(BallControl) + pb(Acceleration) + pb(SprintSpeed) + pb(Agility) + pb(Reactions) + 
                               pb(Balance) + pb(ShotPower) + pb(Jumping) + pb(Stamina) + pb(Strength) + 
                               pb(LongShots) + pb(Aggression) + pb(Interceptions) + pb(Positioning) + 
                               pb(Vision) + pb(Penalties) + pb(Composure) + pb(Marking) + pb(StandingTackle) + 
                               pb(SlidingTackle), data = fifa2, family = GA, method = RS())

# Definizione del modello Ridge 
ridge_model_gamlss <- gamlss(Overall ~ pb(Crossing) + pb(Finishing) + pb(HeadingAccuracy) + pb(ShortPassing) + 
                               pb(Volleys) + pb(Dribbling) + pb(Curve) + pb(FKAccuracy) + pb(LongPassing) + 
                               pb(BallControl) + pb(Acceleration) + pb(SprintSpeed) + pb(Agility) + pb(Reactions) + 
                               pb(Balance) + pb(ShotPower) + pb(Jumping) + pb(Stamina) + pb(Strength) + 
                               pb(LongShots) + pb(Aggression) + pb(Interceptions) + pb(Positioning) + 
                               pb(Vision) + pb(Penalties) + pb(Composure) + pb(Marking) + pb(StandingTackle) + 
                               pb(SlidingTackle), data = fifa2, family = GA, method = RS(), control = gamlss.control(cycle.limit = 200))



# Summaries
summary(lasso_model_gamlss)
summary(ridge_model_gamlss)

# Plot dei residui
plot(lasso_model_gamlss)
plot(ridge_model_gamlss)


#### CONSIDERAZIONI #####

#Il modello LASSO mostra una buona capacità di centrare i residui attorno allo zero, come evidenziato dalla media dei residui che è quasi nulla. La varianza è vicina a 1, il che suggerisce che i residui sono ben standardizzati. 
#Tuttavia, il modello presenta alcune criticità: i residui hanno un'asimmetria leggermente negativa e una curtosi maggiore rispetto a una distribuzione normale ( > 3), indicando code più pesanti. Il coefficiente di correlazione di Filliben molto alto mostra invece un buon allineamento nel Q-Q plot, suggerendo che la distribuzione dei residui segue abbastanza bene quella teorica normale.
#Il plot dei residui contro i valori adattati, indica la presenza di eteroschedasticità, cioè che la varianza dei residui non è costante, e questo potrebbe compromettere la validità delle inferenze statistiche del modello
